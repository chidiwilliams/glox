package main

import "fmt"

type interpreter struct {
	environment *environment
}

func (in *interpreter) visitBlockStmt(stmt blockStmt) interface{} {
	in.executeBlock(stmt.statements, &environment{enclosing: in.environment})
	return nil
}

func (in *interpreter) visitAssignExpr(expr assignExpr) interface{} {
	val := in.evaluate(expr.value)
	in.environment.assign(expr.name, val)
	return val
}

func (in *interpreter) visitVarStmt(stmt varStmt) interface{} {
	var val interface{}
	if stmt.initializer != nil {
		val = in.evaluate(stmt.initializer)
	}
	in.environment.define(stmt.name.lexeme, val)
	return nil
}

func (in *interpreter) visitExpressionStmt(stmt expressionStmt) interface{} {
	return in.evaluate(stmt.expr)
}

func (in *interpreter) visitPrintStmt(stmt printStmt) interface{} {
	value := in.evaluate(stmt.expr)
	fmt.Println(in.stringify(value))
	return nil
}

func (in *interpreter) interpret(stmts []stmt) interface{} {
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

func (in *interpreter) execute(stmt stmt) interface{} {
	return stmt.accept(in)
}

func (in *interpreter) evaluate(expr expr) interface{} {
	return expr.accept(in)
}

func (in *interpreter) visitVariableExpr(expr variableExpr) interface{} {
	return in.environment.get(expr.name)
}

func (in *interpreter) visitBinaryExpr(expr binaryExpr) interface{} {
	left := in.evaluate(expr.left)
	right := in.evaluate(expr.right)

	switch expr.operator.tknType {
	case tokenPlus:
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
		panic(runtimeError{expr.operator, "Operands must be two numbers or two strings"})
	case tokenMinus:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) - right.(float64)
	case tokenSlash:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) / right.(float64)
	case tokenStar:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) * right.(float64)
	// comparison
	case tokenGreater:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) > right.(float64)
	case tokenGreaterEqual:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) >= right.(float64)
	case tokenLess:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) < right.(float64)
	case tokenLessEqual:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) <= right.(float64)
	case tokenEqual:
		return left == right
	case tokenBangEqual:
		return left != right
	case tokenComma:
		return right
	}
	return nil
}

func (in *interpreter) visitGroupingExpr(expr groupingExpr) interface{} {
	return in.evaluate(expr.expression)
}

func (in *interpreter) visitLiteralExpr(expr literalExpr) interface{} {
	return expr.value
}

func (in *interpreter) visitUnaryExpr(expr unaryExpr) interface{} {
	right := in.evaluate(expr.right)
	switch expr.operator.tknType {
	case tokenBang:
		return !in.isTruthy(right)
	case tokenMinus:
		in.checkNumberOperand(expr.operator, right)
		return -right.(float64)
	}
	return nil
}

func (in *interpreter) visitTernaryExpr(expr ternaryExpr) interface{} {
	cond := in.evaluate(expr.cond)
	if in.isTruthy(cond) {
		return in.evaluate(expr.left)
	}
	return in.evaluate(expr.right)
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

func (in *interpreter) checkNumberOperand(operator token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in *interpreter) checkNumberOperands(operator token, left interface{}, right interface{}) {
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

func (in *interpreter) executeBlock(statements []stmt, env *environment) {
	previous := in.environment
	defer func() { in.environment = previous }()

	in.environment = env
	for _, statement := range statements {
		in.execute(statement)
	}
}
