package main

type stmt interface {
	accept(visitor stmtVisitor) interface{}
}

type expressionStmt struct {
	expr expr
}

func (b expressionStmt) accept(visitor stmtVisitor) interface{} {
	return visitor.visitExpressionStmt(b)
}

type printStmt struct {
	expr expr
}

func (b printStmt) accept(visitor stmtVisitor) interface{} {
	return visitor.visitPrintStmt(b)
}

type varStmt struct {
	name        token
	initializer expr
}

func (b varStmt) accept(visitor stmtVisitor) interface{} {
	return visitor.visitVarStmt(b)
}

type stmtVisitor interface {
	visitExpressionStmt(expr expressionStmt) interface{}
	visitPrintStmt(expr printStmt) interface{}
	visitVarStmt(expr varStmt) interface{}
}
