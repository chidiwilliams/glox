package ast

import "fmt"

type TokenType uint8

const (
	// single-character tokens
	TokenLeftParen TokenType = iota
	TokenRightParen
	TokenLeftBrace
	TokenRightBrace
	TokenComma
	TokenDot
	TokenMinus
	TokenPlus
	TokenSemicolon
	TokenSlash
	TokenStar
	TokenColon
	TokenQuestionMark

	// one or two character tokens
	TokenBang
	TokenBangEqual
	TokenEqual
	TokenEqualEqual
	TokenGreater
	TokenGreaterEqual
	TokenLess
	TokenLessEqual

	// literals
	TokenIdentifier
	TokenString
	TokenNumber

	// keywords
	TokenAnd
	TokenClass
	TokenElse
	TokenFalse
	TokenFun
	TokenFor
	TokenIf
	TokenNil
	TokenOr
	TokenPrint
	TokenReturn
	TokenSuper
	TokenThis
	TokenTrue
	TokenVar
	TokenWhile
	TokenBreak
	TokenContinue

	TokenEof
)

type Token struct {
	TokenType TokenType
	Lexeme    string
	Literal   interface{}
	Line      int
}

func (t Token) String() string {
	return fmt.Sprintf("%d %s %s", t.TokenType, t.Lexeme, t.Literal)
}
