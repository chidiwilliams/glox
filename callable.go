package main

import "glox/ast"

type callable interface {
	arity() int
	call(in *Interpreter, args []interface{}) interface{}
}

type function struct {
	declaration ast.FunctionStmt
	closure     *environment
}

func (f function) arity() int {
	return len(f.declaration.Params)
}

func (f function) call(interpreter *Interpreter, args []interface{}) (returnVal interface{}) {
	defer func() {
		if err := recover(); err != nil {
			if v, ok := err.(Return); ok {
				returnVal = v.Value
				return
			}
			panic(err)
		}
	}()

	env := environment{enclosing: f.closure}
	for i, v := range f.declaration.Params {
		env.define(v.Lexeme, args[i])
	}
	interpreter.executeBlock(f.declaration.Body, env)
	return nil
}

func (f function) String() string {
	return "<fn " + f.declaration.Name.Lexeme + ">"
}

type functionExpr struct {
	declaration ast.FunctionExpr
	closure     *environment
}

func (f functionExpr) arity() int {
	return len(f.declaration.Params)
}

func (f functionExpr) call(in *Interpreter, args []interface{}) (returnVal interface{}) {
	defer func() {
		if err := recover(); err != nil {
			if v, ok := err.(Return); ok {
				returnVal = v.Value
				return
			}
			panic(err)
		}
	}()

	env := environment{enclosing: f.closure}
	for i, v := range f.declaration.Params {
		env.define(v.Lexeme, args[i])
	}
	in.executeBlock(f.declaration.Body, env)
	return nil
}
