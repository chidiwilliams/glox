package main

import (
	"fmt"
	"io"

	"glox/ast"
)

type interpreter struct {
	// TODO: Why are these pointers?
	environment *environment
	globals     *environment
	stdOut      io.Writer
}

type interpreterConfig struct {
	stdOut io.Writer
}

func newInterpreter(config interpreterConfig) interpreter {
	globals := &environment{}
	globals.define("clock", clock{})

	return interpreter{globals: globals, environment: globals, stdOut: config.stdOut}
}

func (in *interpreter) VisitCallExpr(expr ast.CallExpr) interface{} {
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

func (in *interpreter) interpret(stmts []ast.Stmt) interface{} {
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

func (in *interpreter) execute(stmt ast.Stmt) interface{} {
	return stmt.Accept(in)
}

func (in *interpreter) evaluate(expr ast.Expr) interface{} {
	return expr.Accept(in)
}

func (in *interpreter) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	// TODO: Why do I need to pass in a pointer here
	in.executeBlock(stmt.Statements, &environment{enclosing: in.environment})
	return nil
}

func (in *interpreter) VisitVarStmt(stmt ast.VarStmt) interface{} {
	var val interface{}
	if stmt.Initializer != nil {
		val = in.evaluate(stmt.Initializer)
	}
	in.environment.define(stmt.Name.Lexeme, val)
	return nil
}

func (in *interpreter) VisitIfStmt(stmt ast.IfStmt) interface{} {
	if in.isTruthy(in.evaluate(stmt.Condition)) {
		in.execute(stmt.ThenBranch)
	} else if stmt.ElseBranch != nil {
		in.execute(stmt.ElseBranch)
	}
	return nil
}

type Break struct{}

func (in *interpreter) VisitWhileStmt(stmt ast.WhileStmt) interface{} {
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

func (in *interpreter) executeLoopBody(body ast.Stmt) interface{} {
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

func (in *interpreter) VisitContinueStmt(_ ast.ContinueStmt) interface{} {
	panic(Continue{})
}

func (in *interpreter) VisitBreakStmt(_ ast.BreakStmt) interface{} {
	panic(Break{})
}

func (in *interpreter) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
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

func (in *interpreter) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	return in.evaluate(stmt.Expr)
}

func (in *interpreter) VisitFunctionStmt(stmt ast.FunctionStmt) interface{} {
	fn := function{declaration: stmt}
	in.environment.define(stmt.Name.Lexeme, fn)
	return nil
}

func (in *interpreter) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	value := in.evaluate(stmt.Expr)
	_, _ = in.stdOut.Write([]byte(in.stringify(value) + "\n"))
	return nil
}

type Return struct {
	Value interface{}
}

func (in *interpreter) VisitReturnStmt(stmt ast.ReturnStmt) interface{} {
	var value interface{}
	if stmt.Value != nil {
		value = in.evaluate(stmt.Value)
	}
	panic(Return{Value: value})
}

func (in *interpreter) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	val := in.evaluate(expr.Value)
	in.environment.assign(expr.Name, val)
	return val
}

func (in *interpreter) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	return in.environment.get(expr.Name)
}

func (in *interpreter) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
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

func (in *interpreter) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return in.evaluate(expr.Expression)
}

func (in *interpreter) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	return expr.Value
}

func (in *interpreter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
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

func (in *interpreter) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	cond := in.evaluate(expr.Cond)
	if in.isTruthy(cond) {
		return in.evaluate(expr.Left)
	}
	return in.evaluate(expr.Right)
}

func (in *interpreter) executeBlock(statements []ast.Stmt, env *environment) {
	previous := in.environment
	defer func() { in.environment = previous }()

	in.environment = env
	for _, statement := range statements {
		in.execute(statement)
	}
}

func (in *interpreter) isTruthy(val interface{}) bool {
	if val == nil {
		return false
	}
	if v, ok := val.(bool); ok {
		return v
	}
	return true
}

func (in *interpreter) checkNumberOperand(operator ast.Token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in *interpreter) checkNumberOperands(operator ast.Token, left interface{}, right interface{}) {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return
		}
	}
	panic(runtimeError{operator, "Operands must be number"})
}

func (in *interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}
