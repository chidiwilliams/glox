package main

import (
	"strconv"

	"glox/ast"
)

type scanner struct {
	start   int
	current int
	line    int
	source  string
	tokens  []ast.Token
}

func (s *scanner) scanTokens() []ast.Token {
	for !s.isAtEnd() {
		// we're at the beginning of the next lexeme
		s.start = s.current
		s.scanToken()
	}

	s.tokens = append(s.tokens, ast.Token{TokenType: ast.TokenEof, Line: s.line})
	return s.tokens
}

func (s *scanner) scanToken() {
	char := s.advance()
	switch char {
	case '(':
		s.addToken(ast.TokenLeftParen)
	case ')':
		s.addToken(ast.TokenRightParen)
	case '{':
		s.addToken(ast.TokenLeftBrace)
	case '}':
		s.addToken(ast.TokenRightBrace)
	case ',':
		s.addToken(ast.TokenComma)
	case '.':
		s.addToken(ast.TokenDot)
	case '-':
		s.addToken(ast.TokenMinus)
	case '+':
		s.addToken(ast.TokenPlus)
	case ';':
		s.addToken(ast.TokenSemicolon)
	case ':':
		s.addToken(ast.TokenColon)
	case '*':
		s.addToken(ast.TokenStar)
	case '?':
		s.addToken(ast.TokenQuestionMark)

	// with look-ahead
	case '!':
		var nextToken ast.TokenType
		if s.match('=') {
			nextToken = ast.TokenBangEqual
		} else {
			nextToken = ast.TokenBang
		}
		s.addToken(nextToken)
	case '=':
		var nextToken ast.TokenType
		if s.match('=') {
			nextToken = ast.TokenEqualEqual
		} else {
			nextToken = ast.TokenEqual
		}
		s.addToken(nextToken)
	case '<':
		var nextToken ast.TokenType
		if s.match('=') {
			nextToken = ast.TokenLessEqual
		} else {
			nextToken = ast.TokenLess
		}
		s.addToken(nextToken)
	case '>':
		var nextToken ast.TokenType
		if s.match('=') {
			nextToken = ast.TokenGreaterEqual
		} else {
			nextToken = ast.TokenGreater
		}
		s.addToken(nextToken)
	case '/':
		if s.match('/') {
			for s.peek() != '\n' && !s.isAtEnd() {
				s.advance()
			}
		} else {
			s.addToken(ast.TokenSlash)
		}

	// whitespace
	case ' ':
	case '\r':
	case '\t':
	case '\n':
		s.line++

	// string
	case '"':
		s.string()

	default:
		if s.isDigit(char) {
			s.number()
		} else if s.isAlpha(char) {
			s.identifier()
		} else {
			reportErr(s.line, "Unexpected character.")
		}
	}
}

func (s *scanner) isAtEnd() bool {
	return s.current >= len(s.source)
}

func (s *scanner) advance() rune {
	curr := rune(s.source[s.current])
	s.current++
	return curr
}

func (s *scanner) addToken(tokenType ast.TokenType) {
	s.addTokenWithLiteral(tokenType, nil)
}

func (s *scanner) addTokenWithLiteral(tokenType ast.TokenType, literal interface{}) {
	text := s.source[s.start:s.current]
	token := ast.Token{TokenType: tokenType, Lexeme: text, Literal: literal, Line: s.line}
	s.tokens = append(s.tokens, token)
}

func (s *scanner) match(expected rune) bool {
	if s.isAtEnd() {
		return false
	}

	if rune(s.source[s.current]) != expected {
		return false
	}

	s.current++
	return true
}

func (s *scanner) string() {
	for s.peek() != '"' && !s.isAtEnd() {
		if s.peek() == '\n' {
			s.line++
		}
		s.advance()
	}

	if s.isAtEnd() {
		reportErr(s.line, "Unterminated string.")
		return
	}

	s.advance() // the closing "

	value := s.source[s.start:s.current]
	s.addTokenWithLiteral(ast.TokenString, value)
}

func (s *scanner) isDigit(r rune) bool {
	return r >= '0' && r <= '9'
}

func (s *scanner) number() {
	for s.isDigit(s.peek()) {
		s.advance()
	}

	// look for a fractional part
	if s.peek() == '.' && s.isDigit(s.peekNext()) {
		s.advance()
		for s.isDigit(s.peek()) {
			s.advance()
		}
	}

	val, _ := strconv.ParseFloat(s.source[s.start:s.current], 64)
	s.addTokenWithLiteral(ast.TokenNumber, val)
}

func (s *scanner) peek() rune {
	if s.isAtEnd() {
		return '\000'
	}
	return rune(s.source[s.current])
}

func (s *scanner) peekNext() rune {
	if s.current+1 >= len(s.source) {
		return '\000'
	}
	return rune(s.source[s.current+1])
}

func (s *scanner) isAlpha(char rune) bool {
	return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char == '_')
}

var keywords = map[string]ast.TokenType{
	"and":    ast.TokenAnd,
	"class":  ast.TokenClass,
	"else":   ast.TokenElse,
	"false":  ast.TokenFalse,
	"for":    ast.TokenFor,
	"fun":    ast.TokenFun,
	"if":     ast.TokenIf,
	"nil":    ast.TokenNil,
	"or":     ast.TokenOr,
	"print":  ast.TokenPrint,
	"return": ast.TokenReturn,
	"super":  ast.TokenSuper,
	"this":   ast.TokenThis,
	"true":   ast.TokenTrue,
	"var":    ast.TokenVar,
	"while":  ast.TokenWhile,
}

func (s *scanner) identifier() {
	for s.isAlphaNumeric(s.peek()) {
		s.advance()
	}

	text := s.source[s.start:s.current]
	tokenType, found := keywords[text]
	if !found {
		tokenType = ast.TokenIdentifier
	}
	s.addToken(tokenType)
}

func (s *scanner) isAlphaNumeric(char rune) bool {
	return s.isAlpha(char) || s.isDigit(char)
}
