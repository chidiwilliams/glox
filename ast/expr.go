package ast

type Expr interface {
	Accept(visitor ExprVisitor) interface{}
}

type AssignExpr struct {
	Name  Token
	Value Expr
}

func (b AssignExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitAssignExpr(b)
}

type BinaryExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

func (b BinaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(b)
}

type CallExpr struct {
	Callee    Expr
	Paren     Token
	Arguments []Expr
}

func (b CallExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitCallExpr(b)
}

type FunctionExpr struct {
	Name   *Token
	Params []Token
	Body   []Stmt
}

func (b FunctionExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitFunctionExpr(b)
}

type GetExpr struct {
	Object Expr
	Name   Token
}

func (b GetExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitGetExpr(b)
}

type GroupingExpr struct {
	Expression Expr
}

func (b GroupingExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitGroupingExpr(b)
}

type LiteralExpr struct {
	Value interface{}
}

func (b LiteralExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitLiteralExpr(b)
}

type LogicalExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

func (b LogicalExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitLogicalExpr(b)
}

type SetExpr struct {
	Object Expr
	Name   Token
	Value  Expr
}

func (b SetExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitSetExpr(b)
}

type SuperExpr struct {
	Keyword Token
	Method  Token
}

func (b SuperExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitSuperExpr(b)
}

type ThisExpr struct {
	Keyword Token
}

func (b ThisExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitThisExpr(b)
}

type TernaryExpr struct {
	Cond  Expr
	Left  Expr
	Right Expr
}

func (b TernaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitTernaryExpr(b)
}

type UnaryExpr struct {
	Operator Token
	Right    Expr
}

func (b UnaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(b)
}

type VariableExpr struct {
	Name Token
}

func (b VariableExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitVariableExpr(b)
}

type ExprVisitor interface {
	VisitAssignExpr(expr AssignExpr) interface{}
	VisitBinaryExpr(expr BinaryExpr) interface{}
	VisitCallExpr(expr CallExpr) interface{}
	VisitFunctionExpr(expr FunctionExpr) interface{}
	VisitGetExpr(expr GetExpr) interface{}
	VisitGroupingExpr(expr GroupingExpr) interface{}
	VisitLiteralExpr(expr LiteralExpr) interface{}
	VisitLogicalExpr(expr LogicalExpr) interface{}
	VisitSetExpr(expr SetExpr) interface{}
	VisitSuperExpr(expr SuperExpr) interface{}
	VisitThisExpr(expr ThisExpr) interface{}
	VisitTernaryExpr(expr TernaryExpr) interface{}
	VisitUnaryExpr(expr UnaryExpr) interface{}
	VisitVariableExpr(expr VariableExpr) interface{}
}
