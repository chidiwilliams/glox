package ast

type Stmt interface {
	Accept(visitor StmtVisitor) interface{}
}

type BlockStmt struct {
	Statements []Stmt
}

func (b BlockStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitBlockStmt(b)
}

type ClassStmt struct {
	Name       Token
	Superclass *VariableExpr
	Methods    []FunctionStmt
}

func (b ClassStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitClassStmt(b)
}

type ExpressionStmt struct {
	Expr Expr
}

func (b ExpressionStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitExpressionStmt(b)
}

type FunctionStmt struct {
	Name   Token
	Params []Token
	Body   []Stmt
}

func (b FunctionStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitFunctionStmt(b)
}

type IfStmt struct {
	Condition  Expr
	ThenBranch Stmt
	ElseBranch Stmt
}

func (b IfStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitIfStmt(b)
}

type PrintStmt struct {
	Expr Expr
}

func (b PrintStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitPrintStmt(b)
}

type ReturnStmt struct {
	Keyword Token
	Value   Expr
}

func (b ReturnStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitReturnStmt(b)
}

type WhileStmt struct {
	Condition Expr
	Body      Stmt
}

func (b WhileStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitWhileStmt(b)
}

type ContinueStmt struct {
}

func (b ContinueStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitContinueStmt(b)
}

type BreakStmt struct {
}

func (b BreakStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitBreakStmt(b)
}

type VarStmt struct {
	Name        Token
	Initializer Expr
}

func (b VarStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitVarStmt(b)
}

type StmtVisitor interface {
	VisitBlockStmt(stmt BlockStmt) interface{}
	VisitClassStmt(stmt ClassStmt) interface{}
	VisitExpressionStmt(stmt ExpressionStmt) interface{}
	VisitFunctionStmt(stmt FunctionStmt) interface{}
	VisitIfStmt(stmt IfStmt) interface{}
	VisitPrintStmt(stmt PrintStmt) interface{}
	VisitReturnStmt(stmt ReturnStmt) interface{}
	VisitWhileStmt(stmt WhileStmt) interface{}
	VisitContinueStmt(stmt ContinueStmt) interface{}
	VisitBreakStmt(stmt BreakStmt) interface{}
	VisitVarStmt(stmt VarStmt) interface{}
}
