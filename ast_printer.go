package main

import (
	"fmt"

	"glox/ast"
)

type astPrinter struct{}

func (a astPrinter) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (a astPrinter) VisitCallExpr(expr ast.CallExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

// print returns a string representation of an ast.Expr node
func (a astPrinter) print(expr ast.Expr) string {
	return expr.Accept(a).(string)
}

func (a astPrinter) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	return a.parenthesize("= "+expr.Name.Lexeme, expr.Value)
}

func (a astPrinter) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	return expr.Name.Lexeme
}

func (a astPrinter) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	return a.parenthesize("?:", expr.Cond, expr.Left, expr.Right)
}

func (a astPrinter) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a astPrinter) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return a.parenthesize("group", expr.Expression)
}

func (a astPrinter) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}

	return fmt.Sprint(expr.Value)
}

func (a astPrinter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Right)
}

func (a astPrinter) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a astPrinter) parenthesize(name string, exprs ...ast.Expr) string {
	var str string

	str += "(" + name
	for _, expr := range exprs {
		str += " " + a.print(expr)
	}
	str += ")"

	return str
}
