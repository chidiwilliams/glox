package ast

type Expr interface {
	Accept(visitor ExprVisitor) interface{}
	StartLine() int
	EndLine() int
}

type AssignExpr struct {
	Name  Token
	Value Expr
}

func (b AssignExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b AssignExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b AssignExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitAssignExpr(b)
}

type BinaryExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

func (b BinaryExpr) StartLine() int {
	return b.Left.StartLine()
}

func (b BinaryExpr) EndLine() int {
	return b.Right.EndLine()
}

func (b BinaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(b)
}

type CallExpr struct {
	Callee    Expr
	Paren     Token
	Arguments []Expr
}

func (b CallExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b CallExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b CallExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitCallExpr(b)
}

type FunctionExpr struct {
	Name       *Token
	Params     []Param
	Body       []Stmt
	ReturnType Type
}

func (b FunctionExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b FunctionExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b FunctionExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitFunctionExpr(b)
}

type GetExpr struct {
	Object Expr
	Name   Token
}

func (b GetExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b GetExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b GetExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitGetExpr(b)
}

type GroupingExpr struct {
	Expression Expr
}

func (b GroupingExpr) StartLine() int {
	return b.Expression.StartLine()
}

func (b GroupingExpr) EndLine() int {
	return b.Expression.EndLine()
}

func (b GroupingExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitGroupingExpr(b)
}

type LiteralExpr struct {
	Value     interface{}
	LineStart int
	LineEnd   int
}

func (b LiteralExpr) StartLine() int {
	return b.LineStart
}

func (b LiteralExpr) EndLine() int {
	return b.LineEnd
}

func (b LiteralExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitLiteralExpr(b)
}

type LogicalExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

func (b LogicalExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b LogicalExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b LogicalExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitLogicalExpr(b)
}

type SetExpr struct {
	Object Expr
	Name   Token
	Value  Expr
}

func (b SetExpr) StartLine() int {
	return b.Object.StartLine()
}

func (b SetExpr) EndLine() int {
	return b.Value.EndLine()
}

func (b SetExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitSetExpr(b)
}

type SuperExpr struct {
	Keyword Token
	Method  Token
}

func (b SuperExpr) StartLine() int {
	return b.Keyword.Line
}

func (b SuperExpr) EndLine() int {
	return b.Method.Line
}

func (b SuperExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitSuperExpr(b)
}

type ThisExpr struct {
	Keyword Token
}

func (b ThisExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ThisExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b ThisExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitThisExpr(b)
}

type TernaryExpr struct {
	Cond       Expr
	Consequent Expr
	Alternate  Expr
}

func (b TernaryExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b TernaryExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b TernaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitTernaryExpr(b)
}

type UnaryExpr struct {
	Operator Token
	Right    Expr
}

func (b UnaryExpr) StartLine() int {
	// TODO implement me
	panic("implement me")
}

func (b UnaryExpr) EndLine() int {
	// TODO implement me
	panic("implement me")
}

func (b UnaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(b)
}

type VariableExpr struct {
	Name Token
}

func (b VariableExpr) StartLine() int {
	return b.Name.Line
}

func (b VariableExpr) EndLine() int {
	return b.Name.Line
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
