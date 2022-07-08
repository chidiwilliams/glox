package resolve

import (
	"fmt"
	"io"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/interpret"
)

type functionType int

const (
	functionTypeNone functionType = iota
	functionTypeFunction
	functionTypeMethod
	functionTypeInitializer
)

type classType int

const (
	classTypeNone classType = iota
	classTypeClass
	classTypeSubClass
)

type scopeVar struct {
	token   ast.Token
	defined bool
	used    bool
}

// scope describes all the local variables
// declared and defined in the current scope
type scope map[string]*scopeVar

// declare a variable with the given name in this scope
func (s scope) declare(name string, token ast.Token) {
	s[name] = &scopeVar{token: token}
}

// define a variable with the given name in this scope
func (s scope) define(name string) {
	s[name].defined = true
}

// has returns whether a variable with the given
// name is declared and defined in this scope
func (s scope) has(name string) (declared, defined bool) {
	v, ok := s[name]
	if !ok {
		return false, false
	}
	return true, v.defined
}

// use sets a variable as used in this scope
func (s scope) use(name string) {
	s[name].used = true
}

func (s scope) set(name string) {
	s[name] = &scopeVar{defined: true, used: true}
}

type scopes []scope

func (s *scopes) peek() scope {
	return (*s)[len(*s)-1]
}

func (s *scopes) push(scope scope) {
	*s = append(*s, scope)
}

func (s *scopes) pop() {
	*s = (*s)[:len(*s)-1]
}

// Resolver resolves local variables in a program. It reports
// to the interpreter the variable to use each time a local
// variable is accessed in the program.
type Resolver struct {
	// the program Interpreter
	interpreter *interpret.Interpreter
	// scopes is a stack of scope-s
	scopes scopes
	// currentFunction is the functionType of the
	// current enclosing function. The Resolver uses
	// the field to report an error when a return
	// statement appears outside a function
	currentFunction functionType
	// the classType of the current enclosing class, used
	// to report an error when "this" appears outside a class
	currentClass classType
	stdErr       io.Writer
	hadError     bool
}

// NewResolver returns a new Resolver
func NewResolver(interpreter *interpret.Interpreter, stdErr io.Writer) *Resolver {
	return &Resolver{interpreter: interpreter, stdErr: stdErr}
}

// ResolveStmts resolves all the local variables in a list of statements
func (r *Resolver) ResolveStmts(statements []ast.Stmt) (hadError bool) {
	for _, statement := range statements {
		r.resolveStmt(statement)
	}
	return r.hadError
}

// resolveExpr resolves an expression
func (r *Resolver) resolveExpr(expr ast.Expr) {
	expr.Accept(r)
}

// resolveStmt resolves a statement
func (r *Resolver) resolveStmt(stmt ast.Stmt) {
	stmt.Accept(r)
}

// resolveFunction resolves a function statement. It begins a
// new scope and resolves the function body within the scope.
func (r *Resolver) resolveFunction(function ast.FunctionStmt, fnType functionType) {
	// change the current function type and save it back
	enclosingFunction := r.currentFunction
	r.currentFunction = fnType
	defer func() { r.currentFunction = enclosingFunction }()

	r.beginScope()
	for _, param := range function.Params {
		r.declare(param.Token)
		r.define(param.Token)
	}
	r.ResolveStmts(function.Body)
	r.endScope()
}

// beginScope pushes a new scope to the stack
func (r *Resolver) beginScope() {
	r.scopes.push(make(scope))
}

// endScope pops the current scope. Before removing the scope,
// it reports an error if any local variable in the scope was unused.
func (r *Resolver) endScope() {
	for name, v := range r.scopes.peek() {
		if !v.used {
			r.error(v.token, fmt.Sprintf("Variable '%s' declared but not used.", name))
		}
	}

	r.scopes.pop()
}

func (r *Resolver) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	r.resolveExpr(expr.Value)
	r.resolveLocal(expr, expr.Name)
	return nil
}

func (r *Resolver) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	r.resolveExpr(expr.Left)
	r.resolveExpr(expr.Right)
	return nil
}

func (r *Resolver) VisitCallExpr(expr ast.CallExpr) interface{} {
	r.resolveExpr(expr.Callee)
	for _, argument := range expr.Arguments {
		r.resolveExpr(argument)
	}
	return nil
}

// VisitFunctionExpr resolves a function expression.
func (r *Resolver) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	// change the current function type
	// and save it back after the resolution
	enclosingFunction := r.currentFunction
	r.currentFunction = functionTypeFunction
	defer func() { r.currentFunction = enclosingFunction }()

	// function expression scope
	r.beginScope()
	if expr.Name != nil {
		r.declare(*expr.Name)
		r.define(*expr.Name)
	}

	// function call scope
	r.beginScope()
	for _, param := range expr.Params {
		r.declare(param.Token)
		r.define(param.Token)
	}
	r.ResolveStmts(expr.Body)
	r.endScope()

	r.endScope()
	return nil
}

func (r *Resolver) VisitGetExpr(expr ast.GetExpr) interface{} {
	r.resolveExpr(expr.Object)
	return nil
}

func (r *Resolver) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	r.resolveExpr(expr.Expression)
	return nil
}

func (r *Resolver) VisitLiteralExpr(_ ast.LiteralExpr) interface{} {
	return nil
}

func (r *Resolver) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	r.resolveExpr(expr.Left)
	r.resolveExpr(expr.Right)
	return nil
}

func (r *Resolver) VisitSetExpr(expr ast.SetExpr) interface{} {
	r.resolveExpr(expr.Value)
	r.resolveExpr(expr.Object)
	return nil
}

func (r *Resolver) VisitSuperExpr(expr ast.SuperExpr) interface{} {
	if r.currentClass == classTypeNone {
		r.error(expr.Keyword, "Can't use 'super' outside of a class.")
	} else if r.currentClass != classTypeSubClass {
		r.error(expr.Keyword, "Can't use 'super' in a class with no superclass.")
	}

	r.resolveLocal(expr, expr.Keyword)
	return nil
}

func (r *Resolver) VisitThisExpr(expr ast.ThisExpr) interface{} {
	if r.currentClass == classTypeNone {
		r.error(expr.Keyword, "Can't use 'this' outside of a class.")
	}

	r.resolveLocal(expr, expr.Keyword)
	return nil
}

func (r *Resolver) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	r.resolveExpr(expr.Cond)
	r.resolveExpr(expr.Consequent)
	r.resolveExpr(expr.Alternate)
	return nil
}

func (r *Resolver) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	r.resolveExpr(expr.Right)
	return nil
}

func (r *Resolver) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	if len(r.scopes) > 0 {
		if declared, defined := r.scopes.peek().has(expr.Name.Lexeme); declared && !defined { // if the variable name is declared but not defined, report error
			r.error(expr.Name, "Can't read local variable in its own initializer.")
		}
	}

	r.resolveLocal(expr, expr.Name)
	return nil
}

func (r *Resolver) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	r.beginScope()
	r.ResolveStmts(stmt.Statements)
	r.endScope()
	return nil
}

func (r *Resolver) VisitClassStmt(stmt ast.ClassStmt) interface{} {
	enclosingClass := r.currentClass
	defer func() { r.currentClass = enclosingClass }()

	r.currentClass = classTypeClass

	r.declare(stmt.Name)
	r.define(stmt.Name)

	if stmt.Superclass != nil && stmt.Name.Lexeme == stmt.Superclass.Name.Lexeme {
		r.error(stmt.Superclass.Name, "A class can't inherit from itself.")
	}

	if stmt.Superclass != nil {
		r.currentClass = classTypeSubClass
		r.resolveExpr(stmt.Superclass)
	}

	if stmt.Superclass != nil {
		r.beginScope()
		defer func() { r.endScope() }()

		r.scopes.peek().set("super")
	}

	r.beginScope()

	r.scopes.peek().set("this")

	for _, method := range stmt.Methods {
		declaration := functionTypeMethod
		if method.Name.Lexeme == "init" {
			declaration = functionTypeInitializer
		}

		r.resolveFunction(method, declaration)
	}

	r.endScope()

	return nil
}

func (r *Resolver) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	r.resolveExpr(stmt.Expr)
	return nil
}

func (r *Resolver) VisitFunctionStmt(stmt ast.FunctionStmt) interface{} {
	r.declare(stmt.Name)
	r.define(stmt.Name)
	r.resolveFunction(stmt, functionTypeFunction)
	return nil
}

func (r *Resolver) VisitIfStmt(stmt ast.IfStmt) interface{} {
	r.resolveExpr(stmt.Condition)
	r.resolveStmt(stmt.ThenBranch)
	if stmt.ElseBranch != nil {
		r.resolveStmt(stmt.ElseBranch)
	}
	return nil
}

func (r *Resolver) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	r.resolveExpr(stmt.Expr)
	return nil
}

func (r *Resolver) VisitReturnStmt(stmt ast.ReturnStmt) interface{} {
	if r.currentFunction == functionTypeNone {
		r.error(stmt.Keyword, "Can't return from top-level code.")
	}

	if stmt.Value != nil {
		if r.currentFunction == functionTypeInitializer {
			r.error(stmt.Keyword, "Can't return a value from an initializer;")
		}
		r.resolveExpr(stmt.Value)
	}

	return nil
}

func (r *Resolver) VisitWhileStmt(stmt ast.WhileStmt) interface{} {
	r.resolveExpr(stmt.Condition)
	r.resolveStmt(stmt.Body)
	return nil
}

func (r *Resolver) VisitContinueStmt(stmt ast.ContinueStmt) interface{} {
	return nil
}

func (r *Resolver) VisitBreakStmt(stmt ast.BreakStmt) interface{} {
	return nil
}

func (r *Resolver) VisitVarStmt(stmt ast.VarStmt) interface{} {
	r.declare(stmt.Name)
	if stmt.Initializer != nil {
		r.resolveExpr(stmt.Initializer)
	}
	r.define(stmt.Name)
	return nil
}

// declare a variable name within the current scope.
// If a variable with the same name is already declared
// in the current scope, it reports an error.
func (r *Resolver) declare(name ast.Token) {
	// if at the global scope, return
	if len(r.scopes) == 0 {
		return
	}

	sc := r.scopes.peek()
	if _, defined := sc.has(name.Lexeme); defined {
		r.error(name, "Already a variable with this name in this scope")
	}

	sc.declare(name.Lexeme, name)
}

// define a variable name within the current scope
func (r *Resolver) define(name ast.Token) {
	// at global scope, no need to do anything
	if len(r.scopes) == 0 {
		return
	}

	r.scopes.peek().define(name.Lexeme)
}

// resolveLocal resolves a local variable or assignment expression. It
// looks through the scope stack and reports the "depth" of the variable
// to the interpreter. The depth is the number of scopes between the scope
// where the variable is accessed and the scope where the variable was defined.
func (r *Resolver) resolveLocal(expr ast.Expr, name ast.Token) {
	for i := len(r.scopes) - 1; i >= 0; i-- {
		s := r.scopes[i]
		if _, defined := s.has(name.Lexeme); defined {
			depth := len(r.scopes) - 1 - i
			r.interpreter.Resolve(expr, depth)
			s.use(name.Lexeme)
			return
		}
	}
}

func (r *Resolver) error(token ast.Token, message string) {
	var where string
	if token.TokenType == ast.TokenEof {
		where = " at end"
	} else {
		where = " at '" + token.Lexeme + "'"
	}

	_, _ = r.stdErr.Write([]byte(fmt.Sprintf("[line %d] Error%s: %s\n", token.Line, where, message)))
	r.hadError = true
}
