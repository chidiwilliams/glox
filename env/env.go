package env

import (
	"errors"
)

// ErrUndefined is returned when retrieving or assigning to an undefined variable
var ErrUndefined = errors.New("undefined variable")

// Environment holds a map of key-value pairs as
// well as a reference to an enclosing environment
type Environment struct {
	Enclosing *Environment
	values    map[string]interface{}
}

// New returns a new environment enclosed by the given environment
func New(enclosing *Environment) Environment {
	return Environment{Enclosing: enclosing, values: make(map[string]interface{})}
}

// Define stores a new key-value pair
func (e *Environment) Define(name string, value interface{}) {
	e.values[name] = value
}

// Assign defines a key-value pair in the environment if the key already exists.
// If it doesn't exist in this environment, it checks the enclosing environment
// and tries to assign the value there. If there are no other enclosing
// environments to check and the key-value pair has not been assigned, it returns
// an ErrUndefined.
func (e *Environment) Assign(name string, value interface{}) error {
	if _, ok := e.values[name]; ok {
		e.Define(name, value)
		return nil
	}
	if e.Enclosing != nil {
		return e.Enclosing.Assign(name, value)
	}
	return ErrUndefined
}

// AssignAt sets the value of the key-value pair at a given distance from this environment
func (e *Environment) AssignAt(distance int, name string, value interface{}) {
	e.ancestor(distance).values[name] = value
}

// Has returns true if a key-value pair with the given name exists
// in this environment or any of its enclosing environments
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

// Get returns the value of the pair with the given name
// in this environment or its enclosing environments. If
// there are no other enclosing environments and the value
// has not been found, it returns an ErrUndefined.
func (e *Environment) Get(name string) (interface{}, error) {
	if val, ok := e.values[name]; ok {
		return val, nil
	}
	if e.Enclosing != nil {
		return e.Enclosing.Get(name)
	}
	return nil, ErrUndefined
}

// GetAt returns the value of the key-value pair at a given distance from this environment
func (e *Environment) GetAt(distance int, name string) interface{} {
	return e.ancestor(distance).values[name]
}

// ancestor returns the environment at a given enclosing distance from this environment
func (e *Environment) ancestor(distance int) *Environment {
	env := e
	for i := 0; i < distance; i++ {
		env = env.Enclosing
	}
	return env
}
