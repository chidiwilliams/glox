package main

type Stmt interface {
	accept(visitor StmtVisitor) interface{}
}

type ExpressionStmt struct {
	expr Expr
}

func (b ExpressionStmt) accept(visitor StmtVisitor) interface{} {
	return visitor.visitExpressionStmt(b)
}

type PrintStmt struct {
	expr Expr
}

func (b PrintStmt) accept(visitor StmtVisitor) interface{} {
	return visitor.visitPrintStmt(b)
}

type StmtVisitor interface {
	visitExpressionStmt(expr ExpressionStmt) interface{}
	visitPrintStmt(expr PrintStmt) interface{}
}
