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
	contains(other loxType) bool
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

func (t primitiveType) contains(other loxType) bool {
	if other, ok := other.(aliasType); ok {
		return t.contains(other.parent)
	}

	return t == other
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

func (t functionType) contains(other loxType) bool {
	if other, ok := other.(aliasType); ok {
		return t.contains(other.parent)
	}

	fnType, ok := other.(functionType)
	if !ok {
		return false
	}

	if !t.returnType.contains(fnType.returnType) {
		return false
	}

	if len(fnType.paramTypes) != len(t.paramTypes) {
		return false
	}

	for i, paramType := range fnType.paramTypes {
		if !t.paramTypes[i].contains(paramType) {
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

func (t aliasType) contains(other loxType) bool {
	if other, ok := other.(aliasType); ok && t.name == other.name {
		return true
	}

	return t.parent.contains(other)
}

type classType struct {
	name       string
	superClass loxType
	properties *env.Environment
}

func (t *classType) String() string {
	return t.name
}

func (t *classType) contains(other loxType) bool {
	if t == other {
		return true
	}

	if alias, ok := other.(aliasType); ok {
		return alias.contains(t)
	}

	if t.superClass != nil {
		return t.superClass.contains(other)
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

type unionType struct {
	left  loxType
	right loxType
}

func (t unionType) String() string {
	return fmt.Sprintf("%s | %s", t.left.String(), t.right.String())
}

func (t unionType) contains(other loxType) bool {
	if other == t {
		return true
	}

	if other, ok := other.(unionType); ok {
		return t.contains(other.left) && t.contains(other.right)
	}

	return t.left.contains(other) || t.right.contains(other)
}
