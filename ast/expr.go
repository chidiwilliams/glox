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

type UnaryExpr struct {
	Operator Token
	Right    Expr
}

func (b UnaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(b)
}

type BinaryExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

func (b BinaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(b)
}

type TernaryExpr struct {
	Cond  Expr
	Left  Expr
	Right Expr
}

func (b TernaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitTernaryExpr(b)
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

type VariableExpr struct {
	Name Token
}

func (b VariableExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitVariableExpr(b)
}

type ExprVisitor interface {
	VisitAssignExpr(Expr AssignExpr) interface{}
	VisitUnaryExpr(Expr UnaryExpr) interface{}
	VisitBinaryExpr(Expr BinaryExpr) interface{}
	VisitTernaryExpr(Expr TernaryExpr) interface{}
	VisitGroupingExpr(Expr GroupingExpr) interface{}
	VisitLiteralExpr(Expr LiteralExpr) interface{}
	VisitVariableExpr(Expr VariableExpr) interface{}
}
