package interpret

import (
	"fmt"

	"glox/ast"
)

type Class struct {
	name       string
	methods    map[string]function
	superclass *Class
}

// arity returns the arity of the class's constructor
func (c Class) arity() int {
	if initializer, ok := c.findMethod("init"); ok {
		return initializer.arity()
	}
	return 0
}

// call-s the class's constructor and returns the new instance
func (c Class) call(interpreter *Interpreter, arguments []interface{}) interface{} {
	in := &instance{class: c}

	// initialize
	if initializer, ok := c.findMethod("init"); ok {
		initializer.bind(in).call(interpreter, arguments)
	}

	return in
}

// Get returns value of the static method with the given name
func (c Class) Get(in *Interpreter, name ast.Token) (interface{}, error) {
	if method, ok := c.findMethod(name.Lexeme); ok {
		return method, nil
	}
	return nil, runtimeError{token: name, msg: fmt.Sprintf("Undefined property '%s'.", name.Lexeme)}
}

func (c Class) findMethod(name string) (function, bool) {
	method, ok := c.methods[name]
	if ok {
		return method, true
	}
	if c.superclass != nil {
		return c.superclass.findMethod(name)
	}
	return function{}, false
}

type Instance interface {
	Get(in *Interpreter, name ast.Token) (interface{}, error)
}

// instance is an instance of a class
type instance struct {
	class  Class
	fields map[string]interface{}
}

// Get returns value of the field or method with the given name. If
// the field is a getter, it runs the method body and returns the result.
func (i *instance) Get(in *Interpreter, name ast.Token) (interface{}, error) {
	if val, ok := i.fields[name.Lexeme]; ok {
		return val, nil
	}

	if method, ok := i.class.findMethod(name.Lexeme); ok {
		// if the method is a getter, call and return its value
		if method.isGetter {
			return method.bind(i).call(in, nil), nil
		}
		return method.bind(i), nil
	}

	return nil, runtimeError{token: name, msg: fmt.Sprintf("Undefined property '%s'.", name.Lexeme)}
}

// set-s the value of a field
func (i *instance) set(name ast.Token, value interface{}) {
	if i.fields == nil {
		i.fields = make(map[string]interface{})
	}
	i.fields[name.Lexeme] = value
}
