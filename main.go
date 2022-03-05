//go:generate go run cmd/ast.go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/interpret"
	"github.com/chidiwilliams/glox/parse"
	"github.com/chidiwilliams/glox/resolve"
	"github.com/chidiwilliams/glox/scan"
)

var (
	hadError        bool
	hadRuntimeError bool

	r = newRunner(os.Stdout, os.Stderr)
)

func main() {
	var filePath string

	flag.StringVar(&filePath, "filePath", "", "File path")
	flag.Parse()

	if filePath == "" {
		runPrompt()
	} else {
		runFile(filePath)
	}
}

func runPrompt() {
	inputScanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !inputScanner.Scan() {
			break
		}

		line := inputScanner.Text()
		run(line)
		hadError = false
	}
}

func runFile(path string) {
	file, err := ioutil.ReadFile(path)
	if err != nil {
		panic(err)
	}

	run(string(file))
	if hadError {
		os.Exit(65)
	}
	if hadRuntimeError {
		os.Exit(70)
	}
}

func newRunner(stdOut io.Writer, stdErr io.Writer) runner {
	return runner{interpreter: interpret.NewInterpreter(stdOut, stdErr), stdErr: stdErr}
}

type runner struct {
	interpreter *interpret.Interpreter
	stdErr      io.Writer
}

func (r *runner) run(source string) {
	scanner := scan.NewScanner(source, r.stdErr)
	tokens := scanner.ScanTokens()

	parser := parse.NewParser(tokens, r.stdErr)
	var statements []ast.Stmt
	statements, hadError = parser.Parse()

	if hadError {
		return
	}

	resolver := resolve.NewResolver(r.interpreter, r.stdErr)
	hadError = resolver.ResolveStmts(statements)

	var result interface{}
	result, hadRuntimeError = r.interpreter.Interpret(statements)
	fmt.Println(result)
}

func run(source string) {
	r.run(source)
}
