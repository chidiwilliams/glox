package main

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

func (e environment) get(name ast.Token) interface{} {
	if v, ok := e.values[name.Lexeme]; ok {
		return v
	}
	if e.enclosing != nil {
		return e.enclosing.get(name)
	}

	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)})
}

func (e *environment) assign(name ast.Token, value interface{}) {
	if _, ok := e.values[name.Lexeme]; ok {
		e.define(name.Lexeme, value)
		return
	}
	if e.enclosing != nil {
		e.enclosing.assign(name, value)
		return
	}
	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.Lexeme)})
}
