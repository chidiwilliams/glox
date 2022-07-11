package interpret

import (
	"fmt"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/env"
)

type class struct {
	name        string
	initializer *function
	methods     map[string]function
	superclass  *class
	fields      []ast.Field
	env         *env.Environment
}

// arity returns the arity of the class's constructor
func (c class) arity() int {
	if c.initializer == nil {
		return 0
	}
	return c.initializer.arity()
}

// call-s the class's constructor and returns the new instance
func (c class) call(interpreter *Interpreter, arguments []interface{}) interface{} {
	in := &instance{class: c}

	// interpret fields
	previous := interpreter.environment
	interpreter.environment = c.env
	for _, field := range c.fields {
		value := interpreter.evaluate(field.Value)
		in.set(field.Name, value)
	}
	interpreter.environment = previous

	// initialize
	if c.initializer != nil {
		c.initializer.bind(in).call(interpreter, arguments)
	}

	return in
}

// Get returns value of the static method with the given name
func (c class) Get(in *Interpreter, name ast.Token) (interface{}, error) {
	method := c.findMethod(name.Lexeme)
	if method == nil {
		return nil, runtimeError{token: name, msg: fmt.Sprintf("Undefined property '%s'.", name.Lexeme)}
	}
	return method, nil
}

// todo: should probably return a pointer
func (c class) findMethod(name string) *function {
	if name == "init" {
		if c.initializer == nil {
			return nil
		}
		return c.initializer
	}

	method, ok := c.methods[name]
	if ok {
		return &method
	}
	if c.superclass != nil {
		return c.superclass.findMethod(name)
	}
	return nil
}

func (c class) String() string {
	return c.name
}

type Instance interface {
	Get(in *Interpreter, name ast.Token) (interface{}, error)
}

// instance is an instance of a class
type instance struct {
	class  class
	fields map[string]interface{}
}

// Get returns value of the field or method with the given name. If
// the field is a getter, it runs the method body and returns the result.
func (i *instance) Get(in *Interpreter, name ast.Token) (interface{}, error) {
	if val, ok := i.fields[name.Lexeme]; ok {
		return val, nil
	}

	method := i.class.findMethod(name.Lexeme)
	if method != nil {
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
