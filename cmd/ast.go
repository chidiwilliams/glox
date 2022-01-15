// Generates AST nodes
package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	defineAst("Expr", []string{
		"Unary    : operator Token, right Expr",
		"Binary   : left Expr, operator Token, right Expr",
		"Ternary  : cond Expr, left Expr, right Expr",
		"Grouping : expression Expr",
		"Literal  : value interface{}",
	})

	defineAst("Stmt", []string{
		"Expression : expr Expr",
		"Print      : expr Expr",
	})
}

func defineAst(name string, types []string) {
	var str string

	str += "package main\n"
	str += defineInterface(name)
	str += defineTypes(name, types)
	str += defineVisitor(name, types)

	filename := strings.ToLower(name) + ".go"
	err := ioutil.WriteFile(filename, []byte(str), 0655)
	if err != nil {
		fmt.Println(err)
	}
}

func defineInterface(name string) string {
	return fmt.Sprintf(`
type %s interface {
	accept(visitor %sVisitor) interface{}
}
`, name, name)
}

func defineTypes(name string, types []string) (str string) {
	for _, t := range types {
		splitType := strings.Split(t, ":")
		fullTypeName := strings.Trim(splitType[0], " ") + name
		str += fmt.Sprintf("\ntype %s struct {\n", fullTypeName)

		fields := strings.Split(splitType[1], ", ")
		for _, field := range fields {
			str += fmt.Sprintf("\t%s\n", strings.Trim(field, " "))
		}

		str += "}\n"

		str += fmt.Sprintf(`
func (b %s) accept(visitor %sVisitor) interface{} {
	return visitor.visit%s(b)
}
`, fullTypeName, name, fullTypeName)
	}
	return str
}

func defineVisitor(name string, types []string) (str string) {
	str += fmt.Sprintf("\ntype %sVisitor interface {\n", name)
	for _, t := range types {
		splitType := strings.Split(t, ":")
		fullTypeName := strings.Trim(splitType[0], " ") + name
		str += fmt.Sprintf("\tvisit%s(expr %s) interface{}\n", fullTypeName, fullTypeName)
	}
	str += "}\n"
	return str
}
