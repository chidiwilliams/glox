package main

import "fmt"

type astPrinter struct{}

func (a astPrinter) visitAssignExpr(expr assignExpr) interface{} {
	return a.parenthesize("= "+expr.name.lexeme, expr.value)
}

func (a astPrinter) print(expr expr) string {
	return expr.accept(a).(string)
}

func (a astPrinter) visitVariableExpr(expr variableExpr) interface{} {
	return expr.name.lexeme
}

func (a astPrinter) visitTernaryExpr(expr ternaryExpr) interface{} {
	return a.parenthesize("?:", expr.cond, expr.left, expr.right)
}

func (a astPrinter) visitBinaryExpr(expr binaryExpr) interface{} {
	return a.parenthesize(expr.operator.lexeme, expr.left, expr.right)
}

func (a astPrinter) visitGroupingExpr(expr groupingExpr) interface{} {
	return a.parenthesize("group", expr.expression)
}

func (a astPrinter) visitLiteralExpr(expr literalExpr) interface{} {
	if expr.value == nil {
		return "nil"
	}

	return fmt.Sprint(expr.value)
}

func (a astPrinter) visitUnaryExpr(expr unaryExpr) interface{} {
	return a.parenthesize(expr.operator.lexeme, expr.right)
}

func (a astPrinter) parenthesize(name string, exprs ...expr) string {
	var str string

	str += "(" + name
	for _, expr := range exprs {
		str += " " + a.print(expr)
	}
	str += ")"

	return str
}
