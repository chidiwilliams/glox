package main

type expr interface {
	accept(visitor exprVisitor) interface{}
}

type assignExpr struct {
	name  token
	value expr
}

func (b assignExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitAssignExpr(b)
}

type unaryExpr struct {
	operator token
	right    expr
}

func (b unaryExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitUnaryExpr(b)
}

type binaryExpr struct {
	left     expr
	operator token
	right    expr
}

func (b binaryExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitBinaryExpr(b)
}

type ternaryExpr struct {
	cond  expr
	left  expr
	right expr
}

func (b ternaryExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitTernaryExpr(b)
}

type groupingExpr struct {
	expression expr
}

func (b groupingExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitGroupingExpr(b)
}

type literalExpr struct {
	value interface{}
}

func (b literalExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitLiteralExpr(b)
}

type variableExpr struct {
	name token
}

func (b variableExpr) accept(visitor exprVisitor) interface{} {
	return visitor.visitVariableExpr(b)
}

type exprVisitor interface {
	visitAssignExpr(expr assignExpr) interface{}
	visitUnaryExpr(expr unaryExpr) interface{}
	visitBinaryExpr(expr binaryExpr) interface{}
	visitTernaryExpr(expr ternaryExpr) interface{}
	visitGroupingExpr(expr groupingExpr) interface{}
	visitLiteralExpr(expr literalExpr) interface{}
	visitVariableExpr(expr variableExpr) interface{}
}
