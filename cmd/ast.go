// Generates AST nodes
package main

import (
	"fmt"
	"go/format"
	"io/ioutil"
	"strings"
)

func main() {
	writeAst("expr", []string{
		"assign   : name token, value expr",
		"unary    : operator token, right expr",
		"binary   : left expr, operator token, right expr",
		"ternary  : cond expr, left expr, right expr",
		"grouping : expression expr",
		"literal  : value interface{}",
		"variable : name token",
	})

	writeAst("stmt", []string{
		"block      : statements []stmt",
		"expression : expr expr",
		"print      : expr expr",
		"var        : name token, initializer expr",
	})
}

func writeAst(name string, types []string) {
	ast, err := defineAst(name, types)
	if err != nil {
		panic(err)
	}

	err = ioutil.WriteFile(strings.ToLower(name)+".go", ast, 0655)
	if err != nil {
		panic(err)
	}
}

func defineAst(name string, types []string) ([]byte, error) {
	var str string

	str += "package main\n"
	str += defineInterface(name)
	str += defineTypes(name, types)
	str += defineVisitor(name, types)

	// Format code with go fmt
	return format.Source([]byte(str))
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
		fullTypeName := strings.Trim(splitType[0], " ") + strings.ToUpper(name[:1]) + name[1:]
		str += fmt.Sprintf("\ntype %s struct {\n", fullTypeName)

		fields := strings.Split(splitType[1], ", ")
		for _, field := range fields {
			str += fmt.Sprintf("\t%s\n", strings.Trim(field, " "))
		}

		str += "}\n"

		str += fmt.Sprintf(`
func (b %s) accept(visitor %sVisitor) interface{} {
	return visitor.visit%s%s(b)
}
`, fullTypeName, name, strings.ToUpper(fullTypeName[:1]), fullTypeName[1:])
	}
	return str
}

func defineVisitor(name string, types []string) (str string) {
	str += fmt.Sprintf("\ntype %sVisitor interface {\n", name)
	for _, t := range types {
		splitType := strings.Split(t, ":")
		fullTypeName := strings.Trim(splitType[0], " ") + strings.ToUpper(name[:1]) + name[1:]
		str += fmt.Sprintf("\tvisit%s%s(expr %s) interface{}\n", strings.ToUpper(fullTypeName[:1]), fullTypeName[1:], fullTypeName)
	}
	str += "}\n"
	return str
}
