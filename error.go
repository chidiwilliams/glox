package main

import (
	"fmt"
	"io"
	"os"

	"glox/ast"
)

func reportTokenErr(w io.Writer, token ast.Token, message string) {
	if token.TokenType == ast.TokenEof {
		report(w, token.Line, " at end", message)
	} else {
		report(w, token.Line, " at '"+token.Lexeme+"'", message)
	}
}

func reportErr(w io.Writer, line int, message string) {
	report(w, line, "", message)
}

func report(w io.Writer, line int, where string, message string) {
	if w == nil {
		w = os.Stderr
	}

	_, _ = w.Write([]byte(fmt.Sprintf("[line %d] Error%s: %s\n", line, where, message)))
	hadError = true
}

type runtimeError struct {
	token ast.Token
	msg   string
}

func (r runtimeError) Error() string {
	return fmt.Sprintf("%s\n[line %d]", r.msg, r.token.Line)
}

func reportRuntimeErr(w io.Writer, err error) {
	_, _ = w.Write([]byte(err.Error()))
	hadRuntimeError = true
}
