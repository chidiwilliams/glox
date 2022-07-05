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
	env *interpret.Environment
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{env: &interpret.Environment{}}
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

func (c *TypeChecker) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	// Infer actual tag
	valueType := c.check(expr.Value)

	// With type check
	if expr.TypeDecl != "" {
		expectedType := c.typeFromString(expr.TypeDecl)
		c.expect(valueType, expectedType, expr.Value, expr)
	}

	c.env.Define(expr.Name.Lexeme, valueType)
	return valueType
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
	// TODO implement me
	panic("implement me")
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
	exprType, err := c.env.Get(expr.Name)
	if err != nil {
		panic(TypeError{message: err.Error()})
	}
	return exprType
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
