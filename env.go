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
