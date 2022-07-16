package typechecker

import (
	"fmt"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/env"
	"github.com/chidiwilliams/glox/interpret"
)

func NewTypeChecker(interpreter *interpret.Interpreter) *TypeChecker {
	globals := env.New(nil)
	globals.Define("clock", newFunctionType("", []loxType{}, typeNumber))

	types := map[string]loxType{
		"number": typeNumber,
		"string": typeString,
		"bool":   typeBoolean,
		"nil":    typeNil,
	}

	return &TypeChecker{env: globals, globals: globals, interpreter: interpreter, types: types}
}

type TypeChecker struct {
	env                  *env.Environment
	globals              *env.Environment
	interpreter          *interpret.Interpreter
	declaredFnReturnType loxType
	inferredFnReturnType loxType
	types                map[string]loxType
}

func (c *TypeChecker) VisitTypeDeclStmt(stmt ast.TypeDeclStmt) interface{} {
	if c.env.Has(stmt.Name.Lexeme) {
		c.errorNoLine(fmt.Sprintf("Type with name %s is already defined.", stmt.Name.Lexeme))
	}

	baseType := c.typeFromParsed(stmt.Base)
	if baseType == nil {
		c.errorNoLine(fmt.Sprintf("Type %v is not defined", stmt.Base))
	}

	alias := newAliasType(stmt.Name.Lexeme, baseType)
	c.types[stmt.Name.Lexeme] = alias
	return alias
}

func (c *TypeChecker) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	c.checkBlock(stmt.Statements, env.New(c.env))
	return nil
}

func (c *TypeChecker) checkBlock(stmts []ast.Stmt, env *env.Environment) {
	// Restore the current environment after executing the block
	previous := c.env
	defer func() {
		c.env = previous
	}()

	c.env = env
	for _, stmt := range stmts {
		c.checkStmt(stmt)
	}
}

func (c *TypeChecker) VisitClassStmt(stmt ast.ClassStmt) interface{} {
	var superclassType *classType
	if stmt.Superclass != nil {
		var ok bool
		superclass, ok := c.types[stmt.Superclass.Name.Lexeme]
		if !ok {
			c.error(stmt.Superclass.StartLine(), "superclass is undefined")
		}

		superclassType, ok = superclass.(*classType)
		if !ok {
			c.error(stmt.Superclass.StartLine(), "superclass must be a class")
		}
	}

	classType := newClassType(stmt.Name.Lexeme, superclassType)
	c.types[stmt.Name.Lexeme] = classType
	classEnv := c.env

	if stmt.Superclass != nil {
		previous := c.env
		defer func() { c.env = previous }()

		c.env = env.New(previous)
		c.env.Define("super", superclassType)
	}

	// TODO: is there a way to make this a function, beginScope and endScope
	previous := c.env
	defer func() { c.env = previous }()

	c.env = env.New(previous)

	c.env.Define("this", classType)

	for _, field := range stmt.Fields {
		fieldType := c.check(field.Value)

		if field.Type != nil {
			expectedType := c.typeFromParsed(field.Type)
			c.expect(fieldType, expectedType, field.Value, field.Value)
		}

		classType.properties.Define(field.Name.Lexeme, fieldType)
	}

	// Get signature of the init method. We do this before checking the rest of the
	// methods, just in case the methods also reference the class in their body
	var initMethodParams []loxType
	if stmt.Init != nil {
		initMethodParams = make([]loxType, len(stmt.Init.Params))
		for i, param := range stmt.Init.Params {
			initMethodParams[i] = c.typeFromParsed(param.Type)
		}
	}

	classEnv.Define(stmt.Name.Lexeme, newFunctionType("", initMethodParams, classType))

	for _, method := range stmt.Methods {
		c.checkMethod(classType, method)
	}

	return nil
}

func (c *TypeChecker) checkMethod(class *classType, stmt ast.FunctionStmt) functionType {
	// Check the method within the current environment
	// Save the name and return type in the class's properties

	params := stmt.Params
	parsedReturnType := stmt.ReturnType
	name := &stmt.Name
	enclosingEnv := c.env
	body := stmt.Body

	// Predefine function type from signature for recursive calls
	paramTypes := make([]loxType, len(params))
	for i, param := range params {
		paramTypes[i] = c.typeFromParsed(param.Type)
	}

	var returnType loxType
	if name != nil && (*name).Lexeme == "init" {
		returnType = class
	} else {
		returnType = c.typeFromParsed(parsedReturnType)
	}

	if name != nil {
		class.properties.Define(name.Lexeme, newFunctionType("", paramTypes, returnType))
	}

	fnEnv := env.New(enclosingEnv)
	for i, param := range params {
		fnEnv.Define(param.Token.Lexeme, paramTypes[i])
	}

	inferredReturnType := c.checkFunctionBody(body, fnEnv, returnType)

	if parsedReturnType != nil && !returnType.contains(inferredReturnType) {
		panic(typeError{message: fmt.Sprintf("expected function %s to return %s, but got %s", body, parsedReturnType, inferredReturnType)})
	}

	return functionType{paramTypes: paramTypes, returnType: inferredReturnType}
}

func (c *TypeChecker) VisitExpressionStmt(stmt ast.ExpressionStmt) interface{} {
	c.check(stmt.Expr)
	return nil
}

func (c *TypeChecker) VisitFunctionStmt(stmt ast.FunctionStmt) interface{} {
	return c.checkFunction(&stmt.Name, stmt.Params, stmt.Body, stmt.ReturnType, c.env)
}

func (c *TypeChecker) VisitIfStmt(stmt ast.IfStmt) interface{} {
	conditionType := c.check(stmt.Condition)
	c.expect(conditionType, typeBoolean, stmt.Condition, stmt.Condition)

	c.checkStmt(stmt.ThenBranch)
	if stmt.ElseBranch != nil {
		c.checkStmt(stmt.ElseBranch)
	}
	return nil
}

func (c *TypeChecker) VisitPrintStmt(stmt ast.PrintStmt) interface{} {
	c.check(stmt.Expr)
	return nil
}

func (c *TypeChecker) VisitReturnStmt(stmt ast.ReturnStmt) interface{} {
	returnType := typeNil
	if stmt.Value != nil {
		returnType = c.check(stmt.Value)
	}

	if c.declaredFnReturnType != nil && !c.declaredFnReturnType.contains(returnType) {
		panic(typeError{message: fmt.Sprintf("expected enclosing function to return '%s', but got '%s'", c.declaredFnReturnType, returnType)})
	}

	// For now there's no union type, so without the declared annotation, the function's return type
	// is just the type of the value returned by the last traversed return statement. Ideally, the
	// inferred function type should be some kind of union of all its return values. Or at least,
	// reduced to "any" if there's no union.
	c.inferredFnReturnType = returnType

	return nil
}

func (c *TypeChecker) VisitWhileStmt(stmt ast.WhileStmt) interface{} {
	conditionType := c.check(stmt.Condition)
	c.expect(conditionType, typeBoolean, stmt.Condition, stmt.Condition)
	c.checkStmt(stmt.Body)
	return nil
}

func (c *TypeChecker) VisitContinueStmt(stmt ast.ContinueStmt) interface{} {
	return nil
}

func (c *TypeChecker) VisitBreakStmt(stmt ast.BreakStmt) interface{} {
	return nil
}

func (c *TypeChecker) VisitVarStmt(stmt ast.VarStmt) interface{} {
	if stmt.Initializer != nil {
		// Infer actual type
		valueType := c.check(stmt.Initializer)

		if stmt.TypeDecl == nil {
			c.env.Define(stmt.Name.Lexeme, valueType)
		} else { // With type check
			expectedType := c.typeFromParsed(stmt.TypeDecl)
			c.expect(valueType, expectedType, stmt.Initializer, stmt.Initializer)
			c.env.Define(stmt.Name.Lexeme, expectedType)
		}
	}
	return nil
}

func (c *TypeChecker) CheckStmts(stmts []ast.Stmt) (err error) {
	defer func() {
		if recovered := recover(); recovered != nil {
			if typeErr, ok := recovered.(typeError); ok {
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

func (c *TypeChecker) check(expr ast.Expr) loxType {
	return expr.Accept(c).(loxType)
}

func (c *TypeChecker) checkStmt(stmt ast.Stmt) {
	stmt.Accept(c)
}

func (c *TypeChecker) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	valueType := c.check(expr.Value)
	varType, err := c.lookupType(expr.Name, expr)
	if err != nil {
		panic(typeError{message: err.Error()})
	}

	c.expect(valueType, varType, expr.Value, expr)
	return valueType
}

func (c *TypeChecker) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	if c.isBooleanBinary(expr.Operator) {
		return c.checkBooleanBinaryExpr(expr)
	}

	leftType := c.check(expr.Left)
	rightType := c.check(expr.Right)

	allowedTypes := c.getOperandTypesForOperator(expr.Operator)

	c.expectOperatorType(leftType, allowedTypes, expr)
	c.expectOperatorType(rightType, allowedTypes, expr)

	return c.expect(leftType, rightType, expr.Right, expr)
}

func (c *TypeChecker) VisitCallExpr(expr ast.CallExpr) interface{} {
	calleeType, ok := c.check(expr.Callee).(functionType)
	if !ok {
		c.error(expr.Callee.StartLine(), "can only call functions or classes")
	}
	return c.checkFunctionCall(calleeType, expr)
}

func (c *TypeChecker) checkFunctionCall(calleeType functionType, expr ast.CallExpr) loxType {
	if len(calleeType.paramTypes) != len(expr.Arguments) {
		panic(typeError{message: fmt.Sprintf("function of type %s expects %d arguments, got %d", calleeType, len(calleeType.paramTypes), len(expr.Arguments))})
	}

	for i, arg := range expr.Arguments {
		argType := c.check(arg)
		c.expect(argType, calleeType.paramTypes[i], arg, expr)
	}

	return calleeType.returnType
}

func (c *TypeChecker) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	return c.checkFunction(expr.Name, expr.Params, expr.Body, expr.ReturnType, env.New(c.env))
}

func (c *TypeChecker) checkFunction(name *ast.Token, params []ast.Param, body []ast.Stmt, parsedReturnType ast.Type, enclosingEnv *env.Environment) functionType {
	// Predefine function type from signature for recursive calls
	paramTypes := make([]loxType, len(params))
	for i, param := range params {
		paramTypes[i] = c.typeFromParsed(param.Type)
	}

	returnType := c.typeFromParsed(parsedReturnType)
	if name != nil {
		c.env.Define(name.Lexeme, newFunctionType("", paramTypes, returnType))
	}

	fnEnv := env.New(enclosingEnv)
	for i, param := range params {
		fnEnv.Define(param.Token.Lexeme, paramTypes[i])
	}

	inferredReturnType := c.checkFunctionBody(body, fnEnv, returnType)

	if parsedReturnType != nil && !returnType.contains(inferredReturnType) {
		panic(typeError{message: fmt.Sprintf("expected function %s to return %s, but got %s", body, parsedReturnType, inferredReturnType)})
	}

	return functionType{paramTypes: paramTypes, returnType: inferredReturnType}
}

func (c *TypeChecker) checkFunctionBody(fnBody []ast.Stmt, fnEnv *env.Environment, declaredReturnType loxType) loxType {
	previousEnclosingFnReturnType := c.declaredFnReturnType
	defer func() { c.declaredFnReturnType = previousEnclosingFnReturnType }()

	c.declaredFnReturnType = declaredReturnType

	c.checkBlock(fnBody, fnEnv)

	returnType := c.inferredFnReturnType
	c.inferredFnReturnType = nil
	return returnType
}

func (c *TypeChecker) VisitGetExpr(expr ast.GetExpr) interface{} {
	object := c.check(expr.Object)

	objectClassType, ok := object.(*classType)
	if !ok {
		c.errorNoLine("object must be an instance of a class")
	}

	field, err := objectClassType.getField(expr.Name.Lexeme)
	if err != nil {
		c.errorNoLine(err.Error())
	}

	return field
}

func (c *TypeChecker) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return c.check(expr.Expression)
}

func (c *TypeChecker) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	switch expr.Value.(type) {
	case string:
		return typeString
	case float64:
		return typeNumber
	case bool:
		return typeBoolean
	}
	panic(typeError{message: "unable to determine expression type"})
}

func (c *TypeChecker) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitSetExpr(expr ast.SetExpr) interface{} {
	object := c.check(expr.Object)

	objectAsClassType, ok := object.(*classType)
	if !ok {
		c.error(expr.StartLine(), "only instances have fields")
	}

	property, err := objectAsClassType.properties.Get(expr.Name.Lexeme)
	if err != nil {
		c.errorNoLine("property does not exist on class")
	}

	valueType := c.check(expr.Value)
	c.expect(valueType, property.(loxType), expr.Value, expr)
	return valueType
}

func (c *TypeChecker) VisitSuperExpr(expr ast.SuperExpr) interface{} {
	distance, _ := c.interpreter.GetLocalDistance(expr)
	superclass := c.env.GetAt(distance, "super").(*classType)

	method, err := superclass.properties.Get(expr.Method.Lexeme)
	if err != nil {
		c.error(expr.EndLine(), err.Error())
	}

	return method
}

func (c *TypeChecker) VisitThisExpr(expr ast.ThisExpr) interface{} {
	distance, _ := c.interpreter.GetLocalDistance(expr)
	return c.env.GetAt(distance, "this")
}

func (c *TypeChecker) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	conditionType := c.check(expr.Cond)
	c.expect(conditionType, typeBoolean, expr.Cond, expr)

	consequentType := c.check(expr.Consequent)
	alternateType := c.check(expr.Alternate)

	c.expect(alternateType, consequentType, expr, expr)
	return alternateType
}

func (c *TypeChecker) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitVariableExpr(expr ast.VariableExpr) interface{} {
	exprType, err := c.lookupType(expr.Name, expr)
	if err != nil {
		panic(typeError{message: err.Error()})
	}
	return exprType
}

func (c *TypeChecker) lookupType(name ast.Token, expr ast.Expr) (loxType, error) {
	// If the variable is a local variable, find it in the resolved enclosing scope
	if distance, ok := c.interpreter.GetLocalDistance(expr); ok {
		return c.env.GetAt(distance, name.Lexeme).(loxType), nil
	}
	nameType, err := c.globals.Get(name.Lexeme)
	if err != nil {
		return nil, err
	}
	return nameType.(loxType), nil
}

func (c *TypeChecker) typeFromParsed(parsedType ast.Type) loxType {
	switch parsed := parsedType.(type) {
	case ast.SingleType:
		switch parsed.Name {
		case "Fn":
			// TODO: So many possible bugs here. Should assert on length of generic arguments.
			// Should there actually be an ast.ArrayType?
			parsedParamTypes := parsed.GenericArgs[0].(ast.ArrayType).Types

			paramTypes := make([]loxType, len(parsedParamTypes))
			for i, parsedParamType := range parsedParamTypes {
				paramTypes[i] = c.typeFromParsed(parsedParamType)
			}

			parsedReturnType := parsed.GenericArgs[1].(ast.SingleType)
			returnType := c.typeFromParsed(parsedReturnType)

			return newFunctionType("", paramTypes, returnType)
		default:
			t, ok := c.types[parsed.Name]
			if ok {
				return t
			}
		}
	case ast.UnionType:
		// TODO: check for nils
		left := c.typeFromParsed(parsed.Left)
		right := c.typeFromParsed(parsed.Right)
		return unionType{left: left, right: right}
	}
	return nil
}

func (c *TypeChecker) expect(actual loxType, expected loxType, value ast.Expr, expr ast.Expr) loxType {
	if !expected.contains(actual) {
		c.error(value.StartLine(), fmt.Sprintf("expected '%s' type, but got '%s'", expected.String(), actual.String()))
	}
	return actual
}

func (c *TypeChecker) getOperandTypesForOperator(operator ast.Token) loxType {
	switch operator.Lexeme {
	case "+":
		return unionType{typeNumber, typeString}
	default:
		return typeNumber
	}
}

func (c *TypeChecker) expectOperatorType(inputType loxType, allowedTypes loxType, expr ast.Expr) {
	if allowedTypes.contains(inputType) {
		return
	}

	c.error(expr.StartLine(), fmt.Sprintf("unexpected type: %v, allowed: %v", inputType, allowedTypes))
}

func (c *TypeChecker) errorNoLine(message string) {
	panic(typeError{message: message})
}

func (c *TypeChecker) error(line int, message string) {
	panic(typeError{line: line, message: message})
}

func (c *TypeChecker) isBooleanBinary(operator ast.Token) bool {
	switch operator.TokenType {
	case ast.TokenBangEqual, ast.TokenEqualEqual,
		ast.TokenGreater, ast.TokenGreaterEqual,
		ast.TokenLess, ast.TokenLessEqual:
		return true
	default:
		return false
	}
}

func (c *TypeChecker) checkBooleanBinaryExpr(expr ast.BinaryExpr) interface{} {
	leftType := c.check(expr.Left)
	rightType := c.check(expr.Right)

	c.expect(rightType, leftType, expr.Right, expr)
	return typeBoolean
}
