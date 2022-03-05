package ast

import (
	"fmt"
)

type astPrinter struct{}

func (a astPrinter) VisitSuperExpr(expr SuperExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (a astPrinter) VisitThisExpr(expr ThisExpr) interface{} {
	return expr.Keyword.Lexeme
}

func (a astPrinter) VisitGetExpr(expr GetExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (a astPrinter) VisitSetExpr(expr SetExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (a astPrinter) VisitFunctionExpr(expr FunctionExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (a astPrinter) VisitCallExpr(expr CallExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

// print returns a string representation of an Expr node
func (a astPrinter) print(expr Expr) string {
	return expr.Accept(a).(string)
}

func (a astPrinter) VisitAssignExpr(expr AssignExpr) interface{} {
	return a.parenthesize("= "+expr.Name.Lexeme, expr.Value)
}

func (a astPrinter) VisitVariableExpr(expr VariableExpr) interface{} {
	return expr.Name.Lexeme
}

func (a astPrinter) VisitTernaryExpr(expr TernaryExpr) interface{} {
	return a.parenthesize("?:", expr.Cond, expr.Left, expr.Right)
}

func (a astPrinter) VisitBinaryExpr(expr BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a astPrinter) VisitGroupingExpr(expr GroupingExpr) interface{} {
	return a.parenthesize("group", expr.Expression)
}

func (a astPrinter) VisitLiteralExpr(expr LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}

	return fmt.Sprint(expr.Value)
}

func (a astPrinter) VisitUnaryExpr(expr UnaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Right)
}

func (a astPrinter) VisitLogicalExpr(expr LogicalExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a astPrinter) parenthesize(name string, exprs ...Expr) string {
	var str string

	str += "(" + name
	for _, expr := range exprs {
		str += " " + a.print(expr)
	}
	str += ")"

	return str
}
