package main

import "glox/ast"

type functionType int

const (
	functionTypeNone functionType = iota
	functionTypeFunction
)

// scope is a map of all the local variables defined
// in the current scope. A key corresponding to the
// variable name exists in the map when the variable
// is declared. The value of the key (true/false)
// says whether the variable has been defined or not.
type scope map[string]bool

func (s scope) declare(name string) {
	s[name] = false
}

func (s scope) define(name string) {
	s[name] = true
}

func (s scope) get(name string) (declared, defined bool) {
	v, ok := s[name]
	return v, ok
}

// Resolver resolves local variables in a program. It reports
// to the interpreter the variable to use each time a local
// variable is accessed in the program.
type Resolver struct {
	// the program Interpreter
	interpreter *Interpreter
	// scopes is a stack of scope-s
	scopes []scope
	// currentFunction is the functionType of the
	// current enclosing function. The Resolver uses
	// the field to report an error when a return
	// statement appears outside a function
	currentFunction functionType
}

// NewResolver returns a new Resolver
func NewResolver(interpreter *Interpreter) *Resolver {
	return &Resolver{interpreter: interpreter}
}

// resolveExpr resolves an expression
func (r *Resolver) resolveExpr(expr ast.Expr) {
	expr.Accept(r)
}

// resolveStmt resolves a statement
func (r *Resolver) resolveStmt(stmt ast.Stmt) {
	stmt.Accept(r)
}

// resolveStmts resolves all the local variables in a list of statements
func (r *Resolver) resolveStmts(statements []ast.Stmt) {
	for _, statement := range statements {
		r.resolveStmt(statement)
	}
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
		r.declare(param)
		r.define(param)
	}
	r.resolveStmts(function.Body)
	r.endScope()
}

// beginScope pushes a new scope to the stack
func (r *Resolver) beginScope() {
	r.scopes = append(r.scopes, make(map[string]bool))
}

// endScope pops the current scope
func (r *Resolver) endScope() {
	r.scopes = r.scopes[:len(r.scopes)-1]
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
		r.declare(param)
		r.define(param)
	}
	r.resolveStmts(expr.Body)
	r.endScope()

	r.endScope()
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

func (r *Resolver) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	r.resolveExpr(expr.Cond)
	r.resolveExpr(expr.Left)
	r.resolveExpr(expr.Right)
	return nil
}

func (r *Resolver) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	r.resolveExpr(expr.Right)
	return nil
}

func (r *Resolver) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	if len(r.scopes) > 0 {
		if defined, declared := r.scopes[len(r.scopes)-1].get(expr.Name.Lexeme); declared && !defined { // if the variable name is declared but not defined, report error
			reportTokenErr(expr.Name, "Can't read local variable in its own initializer.")
		}
	}

	r.resolveLocal(expr, expr.Name)
	return nil
}

func (r *Resolver) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	r.beginScope()
	r.resolveStmts(stmt.Statements)
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
		reportTokenErr(stmt.Keyword, "Can't return from top-level code.")
	}

	if stmt.Value != nil {
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

	sc := r.scopes[len(r.scopes)-1]
	if _, defined := sc.get(name.Lexeme); defined {
		reportTokenErr(name, "Already a variable with this name in this scope")
	}

	sc.declare(name.Lexeme)
}

// define a variable name within the current scope
func (r *Resolver) define(name ast.Token) {
	// at global scope, no need to do anything
	if len(r.scopes) == 0 {
		return
	}

	r.scopes[len(r.scopes)-1].define(name.Lexeme)
}

// resolveLocal resolves a local variable or assignment expression. It
// looks through the scope stack and reports the "depth" of the variable
// to the interpreter. The depth is the number of scopes between the scope
// where the variable is accessed and the scope where the variable was defined.
func (r *Resolver) resolveLocal(expr ast.Expr, name ast.Token) {
	for i := len(r.scopes) - 1; i >= 0; i-- {
		if _, defined := r.scopes[i].get(name.Lexeme); defined {
			depth := len(r.scopes) - 1 - i
			r.interpreter.resolve(expr, depth)
			return
		}
	}
}
