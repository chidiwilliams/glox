package interpret

import "github.com/chidiwilliams/glox/ast"

type callable interface {
	arity() int
	call(in *Interpreter, args []interface{}) interface{}
}

type function struct {
	declaration   ast.FunctionStmt
	closure       *Environment
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

	env := Environment{Enclosing: f.closure}
	for i, v := range f.declaration.Params {
		env.Define(v.Lexeme, args[i])
	}
	interpreter.executeBlock(f.declaration.Body, env)

	if f.isInitializer {
		return f.closure.GetAt(0, "this")
	}

	return nil
}

func (f function) bind(i *instance) function {
	env := Environment{Enclosing: f.closure}
	env.Define("this", i)
	return function{
		declaration:   f.declaration,
		closure:       &env,
		isInitializer: f.isInitializer,
	}
}

type functionExpr struct {
	declaration ast.FunctionExpr
	closure     *Environment
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

	env := Environment{Enclosing: f.closure}
	for i, v := range f.declaration.Params {
		env.Define(v.Lexeme, args[i])
	}
	in.executeBlock(f.declaration.Body, env)
	return nil
}
