package typechecker

import (
	"fmt"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/interpret"
)

type TypeError struct {
	message string
}

func (e TypeError) Error() string {
	return e.message
}

type Type uint8

const (
	TypeNumber Type = iota
	TypeString
)

type TypeChecker struct {
	globals     interpret.Environment
	env         *interpret.Environment
	interpreter *interpret.Interpreter
}

func (c *TypeChecker) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	c.executeBlock(stmt.Statements, interpret.Environment{Enclosing: c.env})
	return nil
}

func (c *TypeChecker) executeBlock(stmts []ast.Stmt, env interpret.Environment) {
	// Restore the current environment after executing the block
	previous := c.env
	defer func() {
		c.env = previous
	}()

	c.env = &env
	for _, stmt := range stmts {
		c.checkStmt(stmt)
	}
}

func (c *TypeChecker) VisitClassStmt(stmt ast.ClassStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	c.check(stmt.Expr)
	return nil
}

func (c *TypeChecker) VisitFunctionStmt(stmt ast.FunctionStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitIfStmt(stmt ast.IfStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitReturnStmt(stmt ast.ReturnStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitWhileStmt(stmt ast.WhileStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitContinueStmt(stmt ast.ContinueStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitBreakStmt(stmt ast.BreakStmt) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitVarStmt(stmt ast.VarStmt) interface{} {
	if stmt.Initializer != nil {
		// Infer actual tag
		valueType := c.check(stmt.Initializer)

		// With type check
		if stmt.TypeDecl != "" {
			expectedType := c.typeFromString(stmt.TypeDecl)
			c.expect(valueType, expectedType, stmt.Initializer, stmt.Initializer)
		}

		c.env.Define(stmt.Name.Lexeme, valueType)
	}
	return nil
}

func NewTypeChecker(interpreter *interpret.Interpreter) *TypeChecker {
	globals := interpret.Environment{}
	return &TypeChecker{env: &globals, globals: globals, interpreter: interpreter}
}

func (c *TypeChecker) Check(expr ast.Expr) (exprType Type, err error) {
	defer func() {
		if recovered := recover(); recovered != nil {
			if typeErr, ok := recovered.(TypeError); ok {
				err = typeErr
			} else {
				panic(recovered)
			}
		}
	}()

	exprType = c.check(expr)
	return exprType, nil
}

func (c *TypeChecker) check(expr ast.Expr) Type {
	return expr.Accept(c).(Type)
}

func (c *TypeChecker) CheckStmts(stmts []ast.Stmt) (err error) {
	defer func() {
		if recovered := recover(); recovered != nil {
			if typeErr, ok := recovered.(TypeError); ok {
				err = typeErr
			} else {
				panic(recovered)
			}
		}
	}()

	for _, stmt := range stmts {
		c.checkStmt(stmt)
	}

	return nil
}

func (c *TypeChecker) CheckStmt(stmt ast.Stmt) (err error) {
	defer func() {
		if recovered := recover(); recovered != nil {
			if typeErr, ok := recovered.(TypeError); ok {
				err = typeErr
			} else {
				panic(recovered)
			}
		}
	}()

	c.checkStmt(stmt)
	return nil
}

func (c *TypeChecker) checkStmt(stmt ast.Stmt) {
	stmt.Accept(c)
}

func (c *TypeChecker) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	leftType := c.check(expr.Left)
	rightType := c.check(expr.Right)

	allowedTypes := c.getOperandTypesForOperator(expr.Operator)

	c.expectOperatorType(leftType, allowedTypes, expr)
	c.expectOperatorType(rightType, allowedTypes, expr)

	return c.expect(leftType, rightType, expr.Right, expr)
}

func (c *TypeChecker) VisitCallExpr(expr ast.CallExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitGetExpr(expr ast.GetExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return c.check(expr.Expression)
}

func (c *TypeChecker) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	switch expr.Value.(type) {
	case string:
		return TypeString
	case float64:
		return TypeNumber
	}
	panic(TypeError{message: "unable to determine expression type"})
}

func (c *TypeChecker) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitSetExpr(expr ast.SetExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitSuperExpr(expr ast.SuperExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitThisExpr(expr ast.ThisExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	exprType, err := c.lookupType(expr.Name, expr)
	if err != nil {
		panic(TypeError{message: err.Error()})
	}
	return exprType
}

func (c *TypeChecker) lookupType(name ast.Token, expr ast.Expr) (interface{}, error) {
	// If the variable is a local variable, find it in the resolved enclosing scope
	if distance, ok := c.interpreter.GetLocalDistance(expr); ok {
		return c.env.GetAt(distance, name.Lexeme), nil
	}
	return c.globals.Get(name)
}

func (c *TypeChecker) typeFromString(str string) Type {
	switch str {
	case "string":
		return TypeString
	case "number":
		return TypeNumber
	default:
		panic(TypeError{message: "Unknown type for expression."})
	}
}

func (c *TypeChecker) expect(actual Type, expected Type, value ast.Expr, expr ast.Expr) Type {
	if actual != expected {
		c.error(fmt.Sprintf("expected '%v' type for %s in %s, but got %v", expected, value, expr, actual))
	}
	return actual
}

func (c *TypeChecker) getOperandTypesForOperator(operator ast.Token) []Type {
	switch operator.Lexeme {
	case "+":
		return []Type{TypeNumber, TypeString}
	default:
		return []Type{TypeNumber}
	}
}

func (c *TypeChecker) expectOperatorType(inputType Type, allowedTypes []Type, expr ast.Expr) {
	for _, allowedType := range allowedTypes {
		if allowedType == inputType {
			return
		}
	}
	c.error(fmt.Sprintf("unexpected type: %v in %v, allowed: %v", inputType, expr, allowedTypes))
}

func (c *TypeChecker) error(message string) {
	panic(TypeError{message: message})
}
