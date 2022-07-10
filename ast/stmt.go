package ast

type Stmt interface {
	Accept(visitor StmtVisitor) interface{}
	StartLine() int
	EndLine() int
}

type BlockStmt struct {
	Statements []Stmt
}

func (b BlockStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b BlockStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b BlockStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitBlockStmt(b)
}

type ClassStmt struct {
	Name       Token
	Superclass *VariableExpr
	Methods    []FunctionStmt
	LineStart  int
	LineEnd    int
}

func (b ClassStmt) StartLine() int {
	return b.LineStart
}

func (b ClassStmt) EndLine() int {
	return b.LineEnd
}

func (b ClassStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitClassStmt(b)
}

type ExpressionStmt struct {
	Expr Expr
}

func (b ExpressionStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ExpressionStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ExpressionStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitExpressionStmt(b)
}

type FunctionStmt struct {
	Name       Token
	Params     []Param
	Body       []Stmt
	ReturnType Type
}

func (b FunctionStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b FunctionStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b FunctionStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitFunctionStmt(b)
}

type IfStmt struct {
	Condition  Expr
	ThenBranch Stmt
	ElseBranch Stmt
}

func (b IfStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b IfStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b IfStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitIfStmt(b)
}

type PrintStmt struct {
	Expr Expr
}

func (b PrintStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b PrintStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b PrintStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitPrintStmt(b)
}

type ReturnStmt struct {
	Keyword Token
	Value   Expr
}

func (b ReturnStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ReturnStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ReturnStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitReturnStmt(b)
}

type WhileStmt struct {
	Condition Expr
	Body      Stmt
}

func (b WhileStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b WhileStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b WhileStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitWhileStmt(b)
}

type ContinueStmt struct {
}

func (b ContinueStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ContinueStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ContinueStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitContinueStmt(b)
}

type BreakStmt struct {
}

func (b BreakStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b BreakStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b BreakStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitBreakStmt(b)
}

type VarStmt struct {
	Name        Token
	Initializer Expr
	TypeDecl    Type
}

func (b VarStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b VarStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b VarStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitVarStmt(b)
}

type TypeDeclStmt struct {
	Name Token
	Base Type
}

func (b TypeDeclStmt) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b TypeDeclStmt) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b TypeDeclStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitTypeDeclStmt(b)
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
	VisitTypeDeclStmt(stmt TypeDeclStmt) interface{}
}
