package main

import "fmt"

type tokenType uint8

const (
	// single-character tokens
	tokenLeftParen tokenType = iota
	tokenRightParen
	tokenLeftBrace
	tokenRightBrace
	tokenComma
	tokenDot
	tokenMinus
	tokenPlus
	tokenSemicolon
	tokenSlash
	tokenStar
	tokenColon
	tokenQuestionMark

	// one or two character tokens
	tokenBang
	tokenBangEqual
	tokenEqual
	tokenEqualEqual
	tokenGreater
	tokenGreaterEqual
	tokenLess
	tokenLessEqual

	// literals
	tokenIdentifier
	tokenString
	tokenNumber

	// keywords
	tokenAnd
	tokenClass
	tokenElse
	tokenFalse
	tokenFun
	tokenFor
	tokenIf
	tokenNil
	tokenOr
	tokenPrint
	tokenReturn
	tokenSuper
	tokenThis
	tokenTrue
	tokenVar
	tokenWhile

	tokenEof
)

type token struct {
	tknType tokenType
	lexeme  string
	literal interface{}
	line    int
}

func (t token) String() string {
	return fmt.Sprintf("%d %s %s", t.tknType, t.lexeme, t.literal)
}
