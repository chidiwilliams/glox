package main

import "fmt"

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

func (e environment) get(name token) interface{} {
	if v, ok := e.values[name.lexeme]; ok {
		return v
	}
	if e.enclosing != nil {
		return e.enclosing.get(name)
	}

	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.lexeme)})
}

func (e *environment) assign(name token, value interface{}) {
	if _, ok := e.values[name.lexeme]; ok {
		e.define(name.lexeme, value)
		return
	}
	if e.enclosing != nil {
		e.enclosing.assign(name, value)
		return
	}
	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.lexeme)})
}
