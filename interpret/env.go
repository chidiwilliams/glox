package interpret

import (
	"errors"

	"github.com/chidiwilliams/glox/ast"
)

// TODO: move this to an env package

// ErrUndefined is returned when retrieving or assigning to an undefined variable
var ErrUndefined = errors.New("undefined variable")

type Environment struct {
	Enclosing *Environment
	values    map[string]interface{}
}

func (e *Environment) Define(name string, value interface{}) {
	if e.values == nil {
		e.values = make(map[string]interface{})
	}
	e.values[name] = value
}

func (e *Environment) Has(name string) bool {
	_, ok := e.values[name]
	if ok {
		return true
	}
	if e.Enclosing != nil {
		return e.Enclosing.Has(name)
	}
	return false
}

func (e *Environment) Get(name string) (interface{}, error) {
	if val, ok := e.values[name]; ok {
		return val, nil
	}
	if e.Enclosing != nil {
		return e.Enclosing.Get(name)
	}
	return nil, ErrUndefined
}

func (e *Environment) assign(name ast.Token, value interface{}) error {
	if _, ok := e.values[name.Lexeme]; ok {
		e.Define(name.Lexeme, value)
		return nil
	}
	if e.Enclosing != nil {
		return e.Enclosing.assign(name, value)
	}
	return ErrUndefined
}

func (e *Environment) GetAt(distance int, name string) interface{} {
	return e.ancestor(distance).values[name]
}

func (e *Environment) assignAt(distance int, name ast.Token, value interface{}) {
	e.ancestor(distance).values[name.Lexeme] = value
}

func (e *Environment) ancestor(distance int) *Environment {
	env := e
	for i := 0; i < distance; i++ {
		env = env.Enclosing
	}
	return env
}
