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

var interp = interpreter{&environment{}}

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
	scanner := scanner{source: source}
	tokens := scanner.scanTokens()

	parser := parser{tokens: tokens}
	statements, err := parser.parse()
	if err != nil {
		return
	}

	if hadError {
		return
	}

	result := interp.interpret(statements)
	fmt.Println(result)
}
