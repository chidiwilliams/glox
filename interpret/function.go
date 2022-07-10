package interpret

import (
	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/env"
)

type callable interface {
	arity() int
	call(in *Interpreter, args []interface{}) interface{}
}

type function struct {
	declaration   ast.FunctionStmt
	closure       *env.Environment
	isInitializer bool
	isGetter      bool
}

func (f function) arity() int {
	return len(f.declaration.Params)
}

func (f function) call(interpreter *Interpreter, args []interface{}) (returnVal interface{}) {
	defer func() {
		if err := recover(); err != nil {
			if v, ok := err.(Return); ok {
				if f.isInitializer {
					returnVal = f.closure.GetAt(0, "this")
					return
				}
				returnVal = v.Value
				return
			}
			panic(err)
		}
	}()

	callEnv := env.New(f.closure)
	for i, v := range f.declaration.Params {
		callEnv.Define(v.Token.Lexeme, args[i])
	}
	interpreter.executeBlock(f.declaration.Body, callEnv)

	if f.isInitializer {
		return f.closure.GetAt(0, "this")
	}

	return nil
}

func (f function) bind(i *instance) function {
	closureEnv := env.New(f.closure)
	closureEnv.Define("this", i)
	return function{
		declaration:   f.declaration,
		closure:       &closureEnv,
		isInitializer: f.isInitializer,
	}
}

type functionExpr struct {
	declaration ast.FunctionExpr
	closure     *env.Environment
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

	callEnv := env.New(f.closure)
	for i, v := range f.declaration.Params {
		callEnv.Define(v.Token.Lexeme, args[i])
	}
	in.executeBlock(f.declaration.Body, callEnv)
	return nil
}
