package main

import (
	"fmt"

	"glox/ast"
)

type interpreter struct {
	environment *environment
}

func (in *interpreter) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	in.executeBlock(stmt.Statements, &environment{enclosing: in.environment})
	return nil
}

func (in *interpreter) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	val := in.evaluate(expr.Value)
	in.environment.assign(expr.Name, val)
	return val
}

func (in *interpreter) VisitVarStmt(stmt ast.VarStmt) interface{} {
	var val interface{}
	if stmt.Initializer != nil {
		val = in.evaluate(stmt.Initializer)
	}
	in.environment.define(stmt.Name.Lexeme, val)
	return nil
}

func (in *interpreter) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	return in.evaluate(stmt.Expr)
}

func (in *interpreter) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	value := in.evaluate(stmt.Expr)
	fmt.Println(in.stringify(value))
	return nil
}

func (in *interpreter) interpret(stmts []ast.Stmt) interface{} {
	defer func() {
		if err := recover(); err != nil {
			if e, ok := err.(runtimeError); ok {
				reportRuntimeErr(e)
			}
			fmt.Printf("Error: %s\n", err)
		}
	}()

	var result interface{}
	for _, statement := range stmts {
		result = in.execute(statement)
	}

	return result
}

func (in *interpreter) execute(stmt ast.Stmt) interface{} {
	return stmt.Accept(in)
}

func (in *interpreter) evaluate(expr ast.Expr) interface{} {
	return expr.Accept(in)
}

func (in *interpreter) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	return in.environment.get(expr.Name)
}

func (in *interpreter) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	left := in.evaluate(expr.Left)
	right := in.evaluate(expr.Right)

	switch expr.Operator.TokenType {
	case ast.TokenPlus:
		_, leftIsFloat := left.(float64)
		_, rightIsFloat := right.(float64)
		if leftIsFloat && rightIsFloat {
			return left.(float64) + right.(float64)
		}
		_, leftIsString := left.(string)
		_, rightIsString := right.(string)
		if leftIsString && rightIsString {
			return left.(string) + right.(string)
		}
		panic(runtimeError{expr.Operator, "Operands must be two numbers or two strings"})
	case ast.TokenMinus:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) - right.(float64)
	case ast.TokenSlash:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) / right.(float64)
	case ast.TokenStar:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) * right.(float64)
	// comparison
	case ast.TokenGreater:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) > right.(float64)
	case ast.TokenGreaterEqual:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) >= right.(float64)
	case ast.TokenLess:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) < right.(float64)
	case ast.TokenLessEqual:
		in.checkNumberOperands(expr.Operator, left, right)
		return left.(float64) <= right.(float64)
	case ast.TokenEqual:
		return left == right
	case ast.TokenBangEqual:
		return left != right
	case ast.TokenComma:
		return right
	}
	return nil
}

func (in *interpreter) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return in.evaluate(expr.Expression)
}

func (in *interpreter) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	return expr.Value
}

func (in *interpreter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	right := in.evaluate(expr.Right)
	switch expr.Operator.TokenType {
	case ast.TokenBang:
		return !in.isTruthy(right)
	case ast.TokenMinus:
		in.checkNumberOperand(expr.Operator, right)
		return -right.(float64)
	}
	return nil
}

func (in *interpreter) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	cond := in.evaluate(expr.Cond)
	if in.isTruthy(cond) {
		return in.evaluate(expr.Left)
	}
	return in.evaluate(expr.Right)
}

func (in *interpreter) isTruthy(val interface{}) bool {
	if val == nil {
		return false
	}
	if v, ok := val.(bool); ok {
		return v
	}
	return true
}

func (in *interpreter) checkNumberOperand(operator ast.Token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in *interpreter) checkNumberOperands(operator ast.Token, left interface{}, right interface{}) {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return
		}
	}
	panic(runtimeError{operator, "Operands must be number"})
}

func (in *interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}

func (in *interpreter) executeBlock(statements []ast.Stmt, env *environment) {
	previous := in.environment
	defer func() { in.environment = previous }()

	in.environment = env
	for _, statement := range statements {
		in.execute(statement)
	}
}
