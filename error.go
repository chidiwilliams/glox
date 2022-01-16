package main

import "fmt"

func reportTokenErr(token token, message string) {
	if token.tknType == tokenEof {
		report(token.line, " at end", message)
	} else {
		report(token.line, " at '"+token.lexeme+"'", message)
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
	token token
	msg   string
}

func (r runtimeError) Error() string {
	return fmt.Sprintf("%s\n[line %d]", r.msg, r.token.line)
}

func reportRuntimeErr(err error) {
	fmt.Println(err.Error())
	hadRuntimeError = true
}
