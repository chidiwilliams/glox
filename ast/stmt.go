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

type ExpressionStmt struct {
	Expr Expr
}

func (b ExpressionStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitExpressionStmt(b)
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

type WhileStmt struct {
	Condition Expr
	Body      Stmt
}

func (b WhileStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitWhileStmt(b)
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
	VisitExpressionStmt(stmt ExpressionStmt) interface{}
	VisitIfStmt(stmt IfStmt) interface{}
	VisitPrintStmt(stmt PrintStmt) interface{}
	VisitWhileStmt(stmt WhileStmt) interface{}
	VisitVarStmt(stmt VarStmt) interface{}
}
