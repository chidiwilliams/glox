package typechecker

import (
	"fmt"

	"github.com/chidiwilliams/glox/env"
)

type typeError struct {
	message string
	line    int
}

func (e typeError) Error() string {
	return fmt.Sprintf("type error on line %d: %s", e.line+1, e.message)
}

type loxType interface {
	String() string
	equals(t loxType) bool
}

type primitiveType struct {
	name string
}

func newPrimitiveType(name string) loxType {
	return primitiveType{name: name}
}

func (t primitiveType) String() string {
	return t.name
}

func (t primitiveType) equals(t2 loxType) bool {
	if _, ok := t2.(aliasType); ok {
		return t2.equals(t)
	}
	return t == t2
}

var (
	typeNumber  = newPrimitiveType("number")
	typeString  = newPrimitiveType("string")
	typeBoolean = newPrimitiveType("boolean")
	typeNil     = newPrimitiveType("nil")
)

func newFunctionType(name string, paramTypes []loxType, returnType loxType) functionType {
	return functionType{name: name, paramTypes: paramTypes, returnType: returnType}
}

type functionType struct {
	name       string
	paramTypes []loxType
	returnType loxType
}

func (t functionType) equals(t2 loxType) bool {
	if _, ok := t2.(aliasType); ok {
		return t2.equals(t)
	}

	fnType, ok := t2.(functionType)
	if !ok {
		return false
	}

	if !fnType.returnType.equals(t.returnType) {
		return false
	}

	if len(fnType.paramTypes) != len(t.paramTypes) {
		return false
	}

	for i, paramType := range fnType.paramTypes {
		if !paramType.equals(t.paramTypes[i]) {
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

func newAliasType(name string, parent loxType) loxType {
	return aliasType{name: name, parent: parent}
}

type aliasType struct {
	name   string
	parent loxType
}

func (t aliasType) String() string {
	return t.name
}

func (t aliasType) equals(t2 loxType) bool {
	if t.String() == t2.String() {
		return true
	}
	return t.parent.equals(t2)
}

type classType struct {
	name       string
	superClass loxType
	properties *env.Environment
}

func (t *classType) String() string {
	return t.name
}

func (t *classType) equals(t2 loxType) bool {
	if t == t2 {
		return true
	}

	if alias, ok := t2.(aliasType); ok {
		return alias.equals(t)
	}

	if t.superClass != nil {
		return t.superClass.equals(t2)
	}

	return false
}

func (t *classType) getField(name string) (loxType, error) {
	fieldType, err := t.properties.Get(name)
	if err != nil {
		return nil, err
	}
	return fieldType.(loxType), nil
}

func (t *classType) getConstructor() (functionType, error) {
	constructor, err := t.getField("init")
	if err == env.ErrUndefined {
		return newFunctionType("", []loxType{}, t), nil
	} else if err != nil {
		return functionType{}, err
	}
	return constructor.(functionType), nil
}

func newClassType(name string, superClass *classType) *classType {
	var enclosingEnv *env.Environment
	if superClass != nil {
		enclosingEnv = superClass.properties
	}

	properties := env.New(enclosingEnv)
	return &classType{
		name:       name,
		superClass: superClass,
		properties: properties,
	}
}
