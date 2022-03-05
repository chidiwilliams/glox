package interpret

import (
	"fmt"
	"io"

	"github.com/chidiwilliams/glox/ast"
)

type runtimeError struct {
	token ast.Token
	msg   string
}

func (r runtimeError) Error() string {
	return fmt.Sprintf("%s\n[line %d]", r.msg, r.token.Line)
}

// Interpreter holds the globals and current execution
// environment for a program to be executed
type Interpreter struct {
	// current execution environment
	environment *environment
	// global variables
	globals environment
	// standard output
	stdOut io.Writer
	// standard error
	stdErr io.Writer
	// pointer, else compares same pointer
	locals map[ast.Expr]int
}

// NewInterpreter sets up a new interpreter with its environment and config
func NewInterpreter(stdOut io.Writer, stdErr io.Writer) *Interpreter {
	globals := environment{}
	globals.define("clock", clock{})

	return &Interpreter{
		globals:     globals,
		environment: &globals,
		stdOut:      stdOut,
		stdErr:      stdErr,
		locals:      make(map[ast.Expr]int),
	}
}

// Interpret interprets a list of statements within the interpreter's environment
func (in *Interpreter) Interpret(stmts []ast.Stmt) (result interface{}, hadRuntimeError bool) {
	defer func() {
		if err := recover(); err != nil {
			if e, ok := err.(runtimeError); ok {
				_, _ = in.stdErr.Write([]byte(e.Error()))
				hadRuntimeError = true
			} else {
				fmt.Printf("Error: %s\n", err)
			}
		}
	}()

	for _, statement := range stmts {
		result = in.execute(statement)
	}

	return
}

func (in Interpreter) error(token ast.Token, message string) {
	panic(runtimeError{token: token, msg: message})
}

func (in *Interpreter) execute(stmt ast.Stmt) interface{} {
	return stmt.Accept(in)
}

// Resolve sets the depth of a local variable access
func (in *Interpreter) Resolve(expr ast.Expr, depth int) {
	in.locals[expr] = depth
}

func (in *Interpreter) evaluate(expr ast.Expr) interface{} {
	return expr.Accept(in)
}

func (in *Interpreter) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	in.executeBlock(stmt.Statements, environment{enclosing: in.environment})
	return nil
}

func (in *Interpreter) VisitClassStmt(stmt ast.ClassStmt) interface{} {
	var superclass *Class
	if stmt.Superclass != nil {
		superclassValue, ok := in.evaluate(stmt.Superclass).(Class)
		if !ok {
			in.error(stmt.Superclass.Name, "Superclass must be a class.")
		}
		superclass = &superclassValue
	}

	in.environment.define(stmt.Name.Lexeme, nil)

	if superclass != nil {
		in.environment = &environment{enclosing: in.environment}
		in.environment.define("super", superclass)
	}

	methods := make(map[string]function)
	for _, method := range stmt.Methods {
		fn := function{
			declaration:   method,
			closure:       in.environment,
			isInitializer: method.Name.Lexeme == "init",
			isGetter:      method.Params == nil, // is this the best way to know it's a getter?
		}
		methods[method.Name.Lexeme] = fn
	}

	class := Class{name: stmt.Name.Lexeme, methods: methods, superclass: superclass}

	if superclass != nil {
		in.environment = in.environment.enclosing
	}

	err := in.environment.assign(stmt.Name, class)
	if err != nil {
		panic(err)
	}
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
		if err := in.globals.assign(expr.Name, value); err != nil {
			panic(err)
		}
	}

	return value
}

func (in *Interpreter) VisitCallExpr(expr ast.CallExpr) interface{} {
	callee := in.evaluate(expr.Callee)

	args := make([]interface{}, 0)
	for _, arg := range expr.Arguments {
		args = append(args, in.evaluate(arg))
	}

	fn, ok := (callee).(callable)
	if !ok {
		in.error(expr.Paren, "Can only call functions and classes.")
	}

	if len(args) != fn.arity() {
		in.error(expr.Paren,
			fmt.Sprintf("Expected %d arguments but got %d.", fn.arity(), len(args)))
	}

	return fn.call(in, args)
}

func (in *Interpreter) VisitGetExpr(expr ast.GetExpr) interface{} {
	object := in.evaluate(expr.Object)
	if instance, ok := object.(Instance); ok {
		val, err := instance.Get(in, expr.Name)
		if err != nil {
			panic(err)
		}
		return val
	}
	in.error(expr.Name, "Only instances have properties.")
	return nil
}

func (in *Interpreter) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	val, err := in.lookupVariable(expr.Name, expr)
	if err != nil {
		panic(err)
	}
	return val
}

// lookupVariable returns the value of a variable
func (in *Interpreter) lookupVariable(name ast.Token, expr ast.Expr) (interface{}, error) {
	// If the variable is a local variable, find it in the resolved enclosing scope
	if distance, ok := in.locals[expr]; ok {
		return in.environment.getAt(distance, name.Lexeme), nil
	}
	return in.globals.Get(nil, name)
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
		in.error(expr.Operator, "Operands must be two numbers or two strings")
	case ast.TokenMinus:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) - right.(float64)
	case ast.TokenSlash:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) / right.(float64)
	case ast.TokenStar:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) * right.(float64)
	// comparison
	case ast.TokenGreater:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) > right.(float64)
	case ast.TokenGreaterEqual:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) >= right.(float64)
	case ast.TokenLess:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
		return left.(float64) < right.(float64)
	case ast.TokenLessEqual:
		if err := in.checkNumberOperands(expr.Operator, left, right); err != nil {
			panic(err)
		}
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

func (in *Interpreter) VisitSetExpr(expr ast.SetExpr) interface{} {
	object := in.evaluate(expr.Object)

	inst, ok := object.(*instance)
	if !ok {
		in.error(expr.Name, "Only instances have fields.")
	}

	value := in.evaluate(expr.Value)
	inst.set(expr.Name, value)
	return nil
}

func (in *Interpreter) VisitSuperExpr(expr ast.SuperExpr) interface{} {
	distance := in.locals[expr]
	superclass := in.environment.getAt(distance, "super").(*Class)
	object := in.environment.getAt(distance-1, "this").(*instance)
	method, ok := superclass.findMethod(expr.Method.Lexeme)
	if !ok {
		in.error(expr.Method, fmt.Sprintf("Undefined property '%s'.", expr.Method.Lexeme))
	}
	return method.bind(object)
}

func (in *Interpreter) VisitThisExpr(expr ast.ThisExpr) interface{} {
	val, err := in.lookupVariable(expr.Keyword, expr)
	if err != nil {
		panic(err)
	}
	return val
}

func (in *Interpreter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	right := in.evaluate(expr.Right)
	switch expr.Operator.TokenType {
	case ast.TokenBang:
		return !in.isTruthy(right)
	case ast.TokenMinus:
		if err := in.checkNumberOperand(expr.Operator, right); err != nil {
			panic(err)
		}
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

func (in *Interpreter) executeBlock(statements []ast.Stmt, env environment) {
	// Restore the current environment after executing the block
	previous := in.environment
	defer func() {
		in.environment = previous
	}()

	in.environment = &env
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

func (in *Interpreter) checkNumberOperand(operator ast.Token, operand interface{}) error {
	if _, ok := operand.(float64); ok {
		return nil
	}
	return runtimeError{operator, "Operand must be a number"}
}

func (in *Interpreter) checkNumberOperands(operator ast.Token, left interface{}, right interface{}) error {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return nil
		}
	}
	return runtimeError{operator, "Operands must be number"}
}

func (in *Interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}