//go:generate go run cmd/ast.go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
)

var (
	hadError        bool
	hadRuntimeError bool
)

var interpreter = NewInterpreter(InterpreterConfig{StdOut: os.Stdout})

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

func run(source string) {
	scanner := Scanner{source: source}
	tokens := scanner.ScanTokens()

	parser := Parser{tokens: tokens}
	statements := parser.Parse()

	if hadError {
		return
	}

	result := interpreter.Interpret(statements)
	fmt.Println(result)
}
