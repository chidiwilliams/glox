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

type Type interface {
	String() string
	Equals(t Type) bool
}

type primitiveType struct {
	name string
}

func newPrimitiveType(name string) Type {
	return primitiveType{name: name}
}

func (s primitiveType) String() string {
	return s.name
}

func (s primitiveType) Equals(t Type) bool {
	if _, ok := t.(aliasType); ok {
		return t.Equals(s)
	}
	return s == t
}

var (
	TypeNumber  = newPrimitiveType("number")
	TypeString  = newPrimitiveType("string")
	TypeBoolean = newPrimitiveType("boolean")
	TypeNil     = newPrimitiveType("nil")
)

func newFunctionType(name string, paramTypes []Type, returnType Type) functionType {
	return functionType{name: name, paramTypes: paramTypes, returnType: returnType}
}

type functionType struct {
	name       string
	paramTypes []Type
	returnType Type
}

func (t functionType) Equals(t2 Type) bool {
	if _, ok := t2.(aliasType); ok {
		return t2.Equals(t)
	}

	fnType, ok := t2.(functionType)
	if !ok {
		return false
	}

	if !fnType.returnType.Equals(t.returnType) {
		return false
	}

	if len(fnType.paramTypes) != len(t.paramTypes) {
		return false
	}

	for i, paramType := range fnType.paramTypes {
		if !paramType.Equals(t.paramTypes[i]) {
			return false
		}
	}

	return true
}

func (t functionType) String() string {
	if t.name != "" {
		return t.name
	}

	name := "Fn<"

	name += "["
	for i, paramType := range t.paramTypes {
		if i > 0 {
			name += ","
		}
		name += paramType.String()
	}
	name += "], "
	name += t.returnType.String()
	name += ">"
	return name
}

func newAliasType(name string, parent Type) Type {
	return aliasType{name: name, parent: parent}
}

type aliasType struct {
	name   string
	parent Type
}

func (t aliasType) String() string {
	return t.name
}

func (t aliasType) Equals(t2 Type) bool {
	if t.String() == t2.String() {
		return true
	}
	return t.parent.Equals(t2)
}

type classType struct {
	name       string
	superClass Type
	env        *interpret.Environment
}

func (c classType) String() string {
	return c.name
}

func (c classType) Equals(t Type) bool {
	if c == t {
		return true
	}

	if alias, ok := t.(aliasType); ok {
		return alias.Equals(c)
	}

	if c.superClass != nil {
		return c.superClass.Equals(t)
	}

	return false
}

func (c classType) getField(name string) (Type, error) {
	fieldType, err := c.env.Get(name)
	if err != nil {
		return nil, err
	}
	return fieldType.(Type), nil
}

func (c classType) getConstructor() (functionType, error) {
	constructor, err := c.getField("init")
	if err == interpret.ErrUndefined {
		return newFunctionType("", []Type{}, c), nil
	} else if err != nil {
		return functionType{}, err
	}
	return constructor.(functionType), nil
}

func newClassType(name string, superClass Type) classType {
	var enclosingEnv *interpret.Environment
	if superClassAsClassType, ok := superClass.(classType); ok {
		enclosingEnv = superClassAsClassType.env
	}
	return classType{
		name:       name,
		superClass: superClass,
		env:        &interpret.Environment{Enclosing: enclosingEnv},
	}
}

func NewTypeChecker(interpreter *interpret.Interpreter) *TypeChecker {
	globals := interpret.Environment{}
	globals.Define("clock", newFunctionType("", []Type{}, TypeNumber))

	types := map[string]Type{
		"number": TypeNumber,
		"string": TypeString,
		"bool":   TypeBoolean,
		"nil":    TypeNil,
	}
	return &TypeChecker{env: &globals, globals: &globals, interpreter: interpreter, types: types}
}

type TypeChecker struct {
	env                  *interpret.Environment
	globals              *interpret.Environment
	interpreter          *interpret.Interpreter
	declaredFnReturnType Type
	inferredFnReturnType Type
	types                map[string]Type
	enclosingClass       classType
}

func (c *TypeChecker) VisitTypeDeclStmt(stmt ast.TypeDeclStmt) interface{} {
	if c.env.Has(stmt.Name.Lexeme) {
		c.error(fmt.Sprintf("Type with name %s is already defined.", stmt.Name.Lexeme))
	}

	baseType := c.typeFromParsed(stmt.Base)
	if baseType == nil {
		c.error(fmt.Sprintf("Type %v is not defined", stmt.Base))
	}

	alias := newAliasType(stmt.Name.Lexeme, baseType)
	c.types[stmt.Name.Lexeme] = alias
	return alias
}

func (c *TypeChecker) VisitBlockStmt(stmt ast.BlockStmt) interface{} {
	c.checkBlock(stmt.Statements, interpret.Environment{Enclosing: c.env})
	return nil
}

func (c *TypeChecker) checkBlock(stmts []ast.Stmt, env interpret.Environment) {
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
	var superclassType Type
	if stmt.Superclass != nil {
		superclassType = c.check(stmt.Superclass)
	}

	classType := newClassType(stmt.Name.Lexeme, superclassType)

	c.types[stmt.Name.Lexeme] = classType
	c.env.Define(stmt.Name.Lexeme, classType)

	previous := c.env
	previousClass := c.enclosingClass
	defer func() {
		c.env = previous
		c.enclosingClass = previousClass
	}()

	c.env = classType.env
	c.enclosingClass = classType

	// What should the actual type of classType be? Shouldn't it be a function
	// callable by its initializer's args to return the class type?

	for _, method := range stmt.Methods {
		c.checkStmt(method)
	}

	return nil
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
	c.expect(conditionType, TypeBoolean, stmt.Condition, stmt.Condition)

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
	returnType := TypeNil
	if stmt.Value != nil {
		returnType = c.check(stmt.Value)
	}

	if c.declaredFnReturnType != nil && !c.declaredFnReturnType.Equals(returnType) {
		panic(TypeError{message: fmt.Sprintf("expected enclosing function to return '%s', but got '%s'", c.declaredFnReturnType, returnType)})
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
	c.expect(conditionType, TypeBoolean, stmt.Condition, stmt.Condition)
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
		// Infer actual tag
		valueType := c.check(stmt.Initializer)

		// With type check
		if stmt.TypeDecl != nil {
			expectedType := c.typeFromParsed(stmt.TypeDecl)
			c.expect(valueType, expectedType, stmt.Initializer, stmt.Initializer)
		}

		c.env.Define(stmt.Name.Lexeme, valueType)
	}
	return nil
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

func (c *TypeChecker) check(expr ast.Expr) Type {
	return expr.Accept(c).(Type)
}

func (c *TypeChecker) checkStmt(stmt ast.Stmt) {
	stmt.Accept(c)
}

func (c *TypeChecker) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	valueType := c.check(expr.Value)
	varType, err := c.lookupType(expr.Name, expr)
	if err != nil {
		panic(TypeError{message: err.Error()})
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
	switch calleeType := c.check(expr.Callee).(type) {
	case functionType:
		return c.checkFunctionCall(calleeType, expr)
	case classType:
		constructor, err := calleeType.getConstructor()
		if err != nil {
			panic(TypeError{message: err.Error()})
		}
		return c.checkFunctionCall(constructor, expr)
	default:
		panic(TypeError{message: "Cannot call a value that is not a function or method"})
	}
}

func (c *TypeChecker) checkFunctionCall(calleeType functionType, expr ast.CallExpr) Type {
	if len(calleeType.paramTypes) != len(expr.Arguments) {
		panic(TypeError{message: fmt.Sprintf("function of type %s expects %d arguments, got %d", calleeType, len(calleeType.paramTypes), len(expr.Arguments))})
	}

	for i, arg := range expr.Arguments {
		argType := c.check(arg)
		c.expect(argType, calleeType.paramTypes[i], arg, expr)
	}

	return calleeType.returnType
}

func (c *TypeChecker) VisitFunctionExpr(expr ast.FunctionExpr) interface{} {
	fnDeclEnv := &interpret.Environment{Enclosing: c.env}
	return c.checkFunction(expr.Name, expr.Params, expr.Body, expr.ReturnType, fnDeclEnv)
}

func (c *TypeChecker) checkFunction(name *ast.Token, params []ast.Param, body []ast.Stmt, parsedReturnType ast.Type, env *interpret.Environment) functionType {
	// Predefine function type from signature for recursive calls
	paramTypes := make([]Type, len(params))
	for i, param := range params {
		paramTypes[i] = c.typeFromParsed(param.Type)
	}

	returnType := c.typeFromParsed(parsedReturnType)
	if name != nil {
		c.env.Define(name.Lexeme, newFunctionType("", paramTypes, returnType))
	}

	fnEnv := interpret.Environment{Enclosing: env}
	for i, param := range params {
		fnEnv.Define(param.Token.Lexeme, paramTypes[i])
	}

	inferredReturnType := c.checkFunctionBody(body, fnEnv, returnType)

	if parsedReturnType != nil && !inferredReturnType.Equals(returnType) {
		panic(TypeError{message: fmt.Sprintf("expected function %s to return %s, but got %s", body, parsedReturnType, inferredReturnType)})
	}

	return functionType{paramTypes: paramTypes, returnType: inferredReturnType}
}

func (c *TypeChecker) checkFunctionBody(fnBody []ast.Stmt, fnEnv interpret.Environment, declaredReturnType Type) Type {
	previousEnclosingFnReturnType := c.declaredFnReturnType
	defer func() { c.declaredFnReturnType = previousEnclosingFnReturnType }()

	c.declaredFnReturnType = declaredReturnType

	c.checkBlock(fnBody, fnEnv)

	returnType := c.inferredFnReturnType
	c.inferredFnReturnType = nil
	return returnType
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
	case bool:
		return TypeBoolean
	}
	panic(TypeError{message: "unable to determine expression type"})
}

func (c *TypeChecker) VisitLogicalExpr(expr ast.LogicalExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitSetExpr(expr ast.SetExpr) interface{} {
	// TODO: check that the object is an instance of a class
	// and that object[name] has the same type as value
	c.check(expr.Object)

	return c.check(expr.Value)
}

func (c *TypeChecker) VisitSuperExpr(expr ast.SuperExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (c *TypeChecker) VisitThisExpr(expr ast.ThisExpr) interface{} {
	return c.enclosingClass
}

func (c *TypeChecker) VisitTernaryExpr(expr ast.TernaryExpr) interface{} {
	conditionType := c.check(expr.Cond)
	c.expect(conditionType, TypeBoolean, expr.Cond, expr)

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
		panic(TypeError{message: err.Error()})
	}
	return exprType
}

func (c *TypeChecker) lookupType(name ast.Token, expr ast.Expr) (Type, error) {
	// If the variable is a local variable, find it in the resolved enclosing scope
	if distance, ok := c.interpreter.GetLocalDistance(expr); ok {
		return c.env.GetAt(distance, name.Lexeme).(Type), nil
	}
	nameType, err := c.globals.Get(name.Lexeme)
	if err != nil {
		return nil, err
	}
	return nameType.(Type), nil
}

func (c *TypeChecker) typeFromParsed(parsedType ast.Type) Type {
	switch parsed := parsedType.(type) {
	case ast.SingleType:
		switch parsed.Name {
		case "Fn":
			// TODO: So many possible bugs here. Should assert on length of generic arguments.
			// Should there actually be an ast.ArrayType?
			parsedParamTypes := parsed.GenericArgs[0].(ast.ArrayType).Types

			paramTypes := make([]Type, len(parsedParamTypes))
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
	}
	return nil
}

func (c *TypeChecker) expect(actual Type, expected Type, value ast.Expr, expr ast.Expr) Type {
	if !actual.Equals(expected) {
		c.error(fmt.Sprintf("error on line %d: expected '%s' type, but got '%s'", value.StartLine()+1, expected.String(), actual.String()))
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
		if allowedType.Equals(inputType) {
			return
		}
	}
	c.error(fmt.Sprintf("unexpected type: %v in %v, allowed: %v", inputType, expr, allowedTypes))
}

func (c *TypeChecker) error(message string) {
	panic(TypeError{message: message})
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
	return TypeBoolean
}
