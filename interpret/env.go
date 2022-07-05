package interpret

import (
	"fmt"

	"github.com/chidiwilliams/glox/ast"
)

type Environment struct {
	enclosing *Environment
	values    map[string]interface{}
}

func (e *Environment) Define(name string, value interface{}) {
	if e.values == nil {
		e.values = make(map[string]interface{})
	}
	e.values[name] = value
}

func (e *Environment) Get(name ast.Token) (interface{}, error) {
	if val, ok := e.values[name.Lexeme]; ok {
		return val, nil
	}
	if e.enclosing != nil {
		return e.enclosing.Get(name)
	}
	return nil, runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)}
}

func (e *Environment) assign(name ast.Token, value interface{}) error {
	if _, ok := e.values[name.Lexeme]; ok {
		e.Define(name.Lexeme, value)
		return nil
	}
	if e.enclosing != nil {
		return e.enclosing.assign(name, value)
	}
	return runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)}
}

func (e *Environment) getAt(distance int, name string) interface{} {
	return e.ancestor(distance).values[name]
}

func (e *Environment) assignAt(distance int, name ast.Token, value interface{}) {
	e.ancestor(distance).values[name.Lexeme] = value
}

func (e *Environment) ancestor(distance int) *Environment {
	env := e
	for i := 0; i < distance; i++ {
		env = env.enclosing
	}
	return env
}
