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

type PrintStmt struct {
	Expr Expr
}

func (b PrintStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitPrintStmt(b)
}

type VarStmt struct {
	Name        Token
	Initializer Expr
}

func (b VarStmt) Accept(visitor StmtVisitor) interface{} {
	return visitor.VisitVarStmt(b)
}

type StmtVisitor interface {
	VisitBlockStmt(Expr BlockStmt) interface{}
	VisitExpressionStmt(Expr ExpressionStmt) interface{}
	VisitPrintStmt(Expr PrintStmt) interface{}
	VisitVarStmt(Expr VarStmt) interface{}
}
