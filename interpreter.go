package main

import (
	"fmt"
	"io"

	"glox/ast"
)

// Interpreter holds the globals and current execution
// environment for a program to be executed
type Interpreter struct {
	// TODO: Why are these pointers?
	// current execution environment
	environment *environment
	// global variables
	globals *environment
	// standard output
	stdOut io.Writer
	// pointer, else compares same pointer
	locals map[ast.Expr]int
}

// InterpreterConfig holds the configuration for an interpreter
type InterpreterConfig struct {
	// Standard output, defaults to io.Stdout
	StdOut io.Writer
}

// NewInterpreter sets up a new interpreter with its environment and config
func NewInterpreter(config InterpreterConfig) *Interpreter {
	globals := &environment{}
	globals.define("clock", clock{})

	return &Interpreter{
		globals:     globals,
		environment: globals,
		stdOut:      config.StdOut,
		locals:      make(map[ast.Expr]int),
	}
}

// Interpret interprets a list of statements within the interpreter's environment
func (in *Interpreter) Interpret(stmts []ast.Stmt) interface{} {
	defer func() {
		if err := recover(); err != nil {
			if e, ok := err.(runtimeError); ok {
				reportRuntimeErr(e)
			} else {
				fmt.Printf("Error: %s\n", err)
			}
		}
	}()

	var result interface{}
	for _, statement := range stmts {
		result = in.execute(statement)
	}

	return result
}

func (in *Interpreter) VisitCallExpr(expr ast.CallExpr) interface{} {
	callee := in.evaluate(expr.Callee)

	args := make([]interface{}, 0)
	for _, arg := range expr.Arguments {
		args = append(args, in.evaluate(arg))
	}

	fn, ok := (callee).(callable)
	if !ok {
		panic(runtimeError{token: expr.Paren, msg: "Can only call functions and classes."})
	}

	if len(args) != fn.arity() {
		panic(runtimeError{token: expr.Paren,
			msg: fmt.Sprintf("Expected %d arguments but got %d.", fn.arity(), len(args))})
	}

	return fn.call(in, args)
}

func (in *Interpreter) execute(stmt ast.Stmt) interface{} {
	return stmt.Accept(in)
}

// resolve sets the depth of a local variable access
func (in *Interpreter) resolve(expr ast.Expr, depth int) {
	in.locals[expr] = depth
}

func (in *Interpreter) evaluate(expr ast.Expr) interface{} {
	return expr.Accept(in)
}

func (in *Interpreter) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	// TODO: Why do I need to pass in a pointer here
	in.executeBlock(stmt.Statements, &environment{enclosing: in.environment})
	return nil
}

func (in *Interpreter) VisitVarStmt(stmt ast.VarStmt) interface{} {
	var val interface{}
	if stmt.Initializer != nil {
		val = in.evaluate(stmt.Initializer)
	}
	in.environment.define(stmt.Name.Lexeme, val)
	return nil
}

func (in *Interpreter) VisitIfStmt(stmt ast.IfStmt) interface{} {
	if in.isTruthy(in.evaluate(stmt.Condition)) {
		in.execute(stmt.ThenBranch)
	} else if stmt.ElseBranch != nil {
		in.execute(stmt.ElseBranch)
	}
	return nil
}

type Break struct{}

func (in *Interpreter) VisitWhileStmt(stmt ast.WhileStmt) interface{} {
	// Exit while stmt if a break is called
	defer func() {
		if err := recover(); err != nil {
			if _, ok := err.(Break); !ok {
				panic(err)
			}
		}
	}()

	for in.isTruthy(in.evaluate(stmt.Condition)) {
		in.executeLoopBody(stmt.Body)
	}
	return nil
}

type Continue struct{}

func (in *Interpreter) executeLoopBody(body ast.Stmt) interface{} {
	// Exit current body if continue panic is found
	defer func() {
		if err := recover(); err != nil {
			if _, ok := err.(Continue); !ok {
				panic(err)
			}
		}
	}()

	in.execute(body)
	return nil
}

func (in *Interpreter) VisitContinueStmt(_ ast.ContinueStmt) interface{} {
	panic(Continue{})
}

func (in *Interpreter) VisitBreakStmt(_ ast.BreakStmt) interface{} {
	panic(Break{})
}

func (in *Interpreter) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	left := in.evaluate(expr.Left)
	if expr.Operator.TokenType == ast.TokenOr {
		if in.isTruthy(left) {
			return left
		}
	} else { // and
		if !in.isTruthy(left) {
			return left
		}
	}
	return in.evaluate(expr.Right)
}

func (in *Interpreter) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	return in.evaluate(stmt.Expr)
}

// VisitFunctionStmt creates a new function from a function statement and
// the current environment and defines the function in the current environment
func (in *Interpreter) VisitFunctionStmt(stmt ast.FunctionStmt) interface{} {
	fn := function{declaration: stmt, closure: in.environment}
	in.environment.define(stmt.Name.Lexeme, fn)
	return nil
}

// VisitPrintStmt evaluates the statement's expression and prints
// the result to the interpreter's standard output
func (in *Interpreter) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	value := in.evaluate(stmt.Expr)
	_, _ = in.stdOut.Write([]byte(in.stringify(value) + "\n"))
	return nil
}

type Return struct {
	Value interface{}
}

func (in *Interpreter) VisitReturnStmt(stmt ast.ReturnStmt) interface{} {
	var value interface{}
	if stmt.Value != nil {
		value = in.evaluate(stmt.Value)
	}
	panic(Return{Value: value})
}

func (in *Interpreter) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	value := in.evaluate(expr.Value)

	distance, ok := in.locals[expr]
	if ok {
		in.environment.assignAt(distance, expr.Name, value)
	} else {
		in.globals.assign(expr.Name, value)
	}

	return value
}

func (in *Interpreter) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	return in.lookupVariable(expr.Name, expr)
}

// lookupVariable returns the value of a variable
func (in *Interpreter) lookupVariable(name ast.Token, expr ast.VariableExpr) interface{} {
	// If the variable is a local variable, find it in the resolved enclosing scope
	if distance, ok := in.locals[expr]; ok {
		return in.environment.getAt(distance, name.Lexeme)
	}

	return in.globals.get(name)
}

func (in *Interpreter) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	left := in.evaluate(expr.Left)
	right := in.evaluate(expr.Right)

	switch expr.Operator.TokenType {
	case ast.TokenPlus:
		_, leftIsFloat := left.(float64)
		_, rightIsFloat := right.(float64)
		if leftIsFloat && rightIsFloat {
			return left.(float64) + right.(float64)
		}
		_, leftIsString := left.(string)
		_, rightIsString := right.(string)
		if leftIsString && rightIsString {
			return left.(string) + right.(string)
		}
		panic(runtimeError{expr.Operator, "Operands must be two numbers or two strings"})
	case ast.TokenMinus:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) - right.(float64)
	case ast.TokenSlash:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) / right.(float64)
	case ast.TokenStar:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) * right.(float64)
	// comparison
	case ast.TokenGreater:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) > right.(float64)
	case ast.TokenGreaterEqual:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) >= right.(float64)
	case ast.TokenLess:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) < right.(float64)
	case ast.TokenLessEqual:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) <= right.(float64)
	case ast.TokenEqualEqual:
		return left == right
	case ast.TokenBangEqual:
		return left != right
	case ast.TokenComma:
		return right
	}
	return nil
}

// VisitFunctionExpr creates a new function from the function expression and the
// current environment. The name of the function expression is defined within its block.
func (in *Interpreter) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	fn := functionExpr{declaration: expr, closure: &environment{enclosing: in.environment}}
	if expr.Name != nil {
		fn.closure.define(expr.Name.Lexeme, fn)
	}

	return fn
}

func (in *Interpreter) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return in.evaluate(expr.Expression)
}

func (in *Interpreter) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	return expr.Value
}

func (in *Interpreter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	right := in.evaluate(expr.Right)
	switch expr.Operator.TokenType {
	case ast.TokenBang:
		return !in.isTruthy(right)
	case ast.TokenMinus:
		in.checkNumberOperand(expr.Operator, right)
		return -right.(float64)
	}
	return nil
}

func (in *Interpreter) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	cond := in.evaluate(expr.Cond)
	if in.isTruthy(cond) {
		return in.evaluate(expr.Left)
	}
	return in.evaluate(expr.Right)
}

func (in *Interpreter) executeBlock(statements []ast.Stmt, env *environment) {
	previous := in.environment
	defer func() {
		in.environment = previous
	}()

	in.environment = env
	for _, statement := range statements {
		in.execute(statement)
	}
}

func (in *Interpreter) isTruthy(val interface{}) bool {
	if val == nil {
		return false
	}
	if v, ok := val.(bool); ok {
		return v
	}
	return true
}

func (in *Interpreter) checkNumberOperand(operator ast.Token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in *Interpreter) checkNumberOperands(operator ast.Token, left interface{}, right interface{}) {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return
		}
	}
	panic(runtimeError{operator, "Operands must be number"})
}

func (in *Interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}
