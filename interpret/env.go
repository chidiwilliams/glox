package interpret

import (
	"fmt"

	"glox/ast"
)

type environment struct {
	enclosing *environment
	values    map[string]interface{}
}

func (e *environment) define(name string, value interface{}) {
	if e.values == nil {
		e.values = make(map[string]interface{})
	}
	e.values[name] = value
}

func (e *environment) Get(in *Interpreter, name ast.Token) (interface{}, error) {
	if val, ok := e.values[name.Lexeme]; ok {
		return val, nil
	}
	if e.enclosing != nil {
		return e.enclosing.Get(nil, name)
	}
	return nil, runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)}
}

func (e *environment) assign(name ast.Token, value interface{}) error {
	if _, ok := e.values[name.Lexeme]; ok {
		e.define(name.Lexeme, value)
		return nil
	}
	if e.enclosing != nil {
		return e.enclosing.assign(name, value)
	}
	return runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)}
}

func (e *environment) getAt(distance int, name string) interface{} {
	return e.ancestor(distance).values[name]
}

func (e *environment) assignAt(distance int, name ast.Token, value interface{}) {
	e.ancestor(distance).values[name.Lexeme] = value
}

func (e *environment) ancestor(distance int) *environment {
	env := e
	for i := 0; i < distance; i++ {
		env = env.enclosing
	}
	return env
}
