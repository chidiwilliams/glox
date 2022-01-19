package main

import (
	"fmt"

	"glox/ast"
)

func reportTokenErr(token ast.Token, message string) {
	if token.TokenType == ast.TokenEof {
		report(token.Line, " at end", message)
	} else {
		report(token.Line, " at '"+token.Lexeme+"'", message)
	}
}

func reportErr(line int, message string) {
	report(line, "", message)
}

func report(line int, where string, message string) {
	fmt.Printf("[line %d] Error%s: %s\n", line, where, message)
	hadError = true
}

type runtimeError struct {
	token ast.Token
	msg   string
}

func (r runtimeError) Error() string {
	return fmt.Sprintf("%s\n[line %d]", r.msg, r.token.Line)
}

func reportRuntimeErr(err error) {
	fmt.Println(err.Error())
	hadRuntimeError = true
}
