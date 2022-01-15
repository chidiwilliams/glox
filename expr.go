package main

type Expr interface {
	accept(visitor ExprVisitor) interface{}
}

type UnaryExpr struct {
	operator Token
	right    Expr
}

func (b UnaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitUnaryExpr(b)
}

type BinaryExpr struct {
	left     Expr
	operator Token
	right    Expr
}

func (b BinaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitBinaryExpr(b)
}

type TernaryExpr struct {
	cond  Expr
	left  Expr
	right Expr
}

func (b TernaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitTernaryExpr(b)
}

type GroupingExpr struct {
	expression Expr
}

func (b GroupingExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitGroupingExpr(b)
}

type LiteralExpr struct {
	value interface{}
}

func (b LiteralExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitLiteralExpr(b)
}

type ExprVisitor interface {
	visitUnaryExpr(expr UnaryExpr) interface{}
	visitBinaryExpr(expr BinaryExpr) interface{}
	visitTernaryExpr(expr TernaryExpr) interface{}
	visitGroupingExpr(expr GroupingExpr) interface{}
	visitLiteralExpr(expr LiteralExpr) interface{}
}
