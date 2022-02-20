//go:generate go run cmd/ast.go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
)

var (
	hadError        bool
	hadRuntimeError bool
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
	return runner{interpreter: NewInterpreter(stdOut, stdErr), stdErr: stdErr}
}

type runner struct {
	interpreter *Interpreter
	stdErr      io.Writer
}

func (r runner) run(source string) {
	scanner := NewScanner(source)
	tokens := scanner.ScanTokens()

	parser := NewParser(tokens)
	statements := parser.Parse()

	if hadError {
		return
	}

	resolver := NewResolver(r.interpreter, r.stdErr)
	resolver.resolveStmts(statements)

	if hadError {
		return
	}

	result := r.interpreter.Interpret(statements)
	fmt.Println(result)
}

func run(source string) {
	r := newRunner(os.Stdout, os.Stderr)
	r.run(source)
}
