// Generates AST nodes
package main

import (
	"fmt"
	"go/format"
	"io/ioutil"
	"strings"
)

func main() {
	writeAst("Expr", []string{
		"Assign   : Name Token, Value Expr, TypeDecl string",
		"Binary   : Left Expr, Operator Token, Right Expr",
		"Call     : Callee Expr, Paren Token, Arguments []Expr",
		"Function : Name *Token, Params []Token, Body []Stmt",
		"Get      : Object Expr, Name Token",
		"Grouping : Expression Expr",
		"Literal  : Value interface{}",
		"Logical  : Left Expr, Operator Token, Right Expr",
		"Set      : Object Expr, Name Token, Value Expr",
		"Super    : Keyword Token, Method Token",
		"This     : Keyword Token",
		"Ternary  : Cond Expr, Left Expr, Right Expr",
		"Unary    : Operator Token, Right Expr",
		"Variable : Name Token",
	})

	writeAst("Stmt", []string{
		"Block      : Statements []Stmt",
		"Class      : Name Token, Superclass *VariableExpr, Methods []FunctionStmt",
		"Expression : Expr Expr",
		"Function   : Name Token, Params []Token, Body []Stmt",
		"If         : Condition Expr, ThenBranch Stmt, ElseBranch Stmt",
		"Print      : Expr Expr",
		"Return     : Keyword Token, Value Expr",
		"While      : Condition Expr, Body Stmt",
		"Continue   : ",
		"Break      : ",
		"Var        : Name Token, Initializer Expr",
	})
}

func writeAst(name string, types []string) {
	ast, err := defineAst(name, types)
	if err != nil {
		panic(err)
	}

	filename := "ast/" + strings.ToLower(name) + ".go"
	err = ioutil.WriteFile(filename, ast, 0655)
	if err != nil {
		panic(err)
	}
}

func defineAst(name string, types []string) ([]byte, error) {
	var str string

	str += "package ast\n"
	str += defineInterface(name)
	str += defineTypes(name, types)
	str += defineVisitor(name, types)

	// Format code with go fmt
	return format.Source([]byte(str))
}

func defineInterface(name string) string {
	return fmt.Sprintf(`
type %s interface {
	Accept(visitor %sVisitor) interface{}
}
`, name, name)
}

func defineTypes(name string, types []string) (str string) {
	for _, t := range types {
		splitType := strings.Split(t, ":")
		fullTypeName := strings.Trim(splitType[0], " ") + strings.ToUpper(name[:1]) + name[1:]
		str += fmt.Sprintf("\ntype %s struct {\n", fullTypeName)

		fields := strings.Split(splitType[1], ", ")
		for _, field := range fields {
			str += fmt.Sprintf("\t%s\n", strings.Trim(field, " "))
		}

		str += "}\n"

		str += fmt.Sprintf(`
func (b %s) Accept(visitor %sVisitor) interface{} {
	return visitor.Visit%s(b)
}
`, fullTypeName, name, fullTypeName)
	}
	return str
}

func defineVisitor(name string, types []string) (str string) {
	str += fmt.Sprintf("\ntype %sVisitor interface {\n", name)
	for _, t := range types {
		splitType := strings.Split(t, ":")
		fullTypeName := strings.Trim(splitType[0], " ") + strings.ToUpper(name[:1]) + name[1:]
		str += fmt.Sprintf("\tVisit%s(%s %s) interface{}\n", fullTypeName, strings.ToLower(name), fullTypeName)
	}
	str += "}\n"
	return str
}
