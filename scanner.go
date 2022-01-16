package main

import (
	"strconv"
)

type scanner struct {
	start   int
	current int
	line    int
	source  string
	tokens  []token
}

func (s *scanner) scanTokens() []token {
	for !s.isAtEnd() {
		// we're at the beginning of the next lexeme
		s.start = s.current
		s.scanToken()
	}

	s.tokens = append(s.tokens, token{tokenEof, "", nil, s.line})
	return s.tokens
}

func (s *scanner) scanToken() {
	char := s.advance()
	switch char {
	case '(':
		s.addToken(tokenLeftParen)
	case ')':
		s.addToken(tokenRightParen)
	case '{':
		s.addToken(tokenLeftBrace)
	case '}':
		s.addToken(tokenRightBrace)
	case ',':
		s.addToken(tokenComma)
	case '.':
		s.addToken(tokenDot)
	case '-':
		s.addToken(tokenMinus)
	case '+':
		s.addToken(tokenPlus)
	case ';':
		s.addToken(tokenSemicolon)
	case ':':
		s.addToken(tokenColon)
	case '*':
		s.addToken(tokenStar)
	case '?':
		s.addToken(tokenQuestionMark)

	// with look-ahead
	case '!':
		var nextToken tokenType
		if s.match('=') {
			nextToken = tokenBangEqual
		} else {
			nextToken = tokenBang
		}
		s.addToken(nextToken)
	case '=':
		var nextToken tokenType
		if s.match('=') {
			nextToken = tokenEqualEqual
		} else {
			nextToken = tokenEqual
		}
		s.addToken(nextToken)
	case '<':
		var nextToken tokenType
		if s.match('=') {
			nextToken = tokenLessEqual
		} else {
			nextToken = tokenLess
		}
		s.addToken(nextToken)
	case '>':
		var nextToken tokenType
		if s.match('=') {
			nextToken = tokenGreaterEqual
		} else {
			nextToken = tokenGreater
		}
		s.addToken(nextToken)
	case '/':
		if s.match('/') {
			for s.peek() != '\n' && !s.isAtEnd() {
				s.advance()
			}
		} else {
			s.addToken(tokenSlash)
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

func (s *scanner) addToken(tknType tokenType) {
	s.addTokenWithLiteral(tknType, nil)
}

func (s *scanner) addTokenWithLiteral(tknType tokenType, literal interface{}) {
	text := s.source[s.start:s.current]
	s.tokens = append(s.tokens, token{tknType, text, literal, s.line})
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
	s.addTokenWithLiteral(tokenString, value)
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
	s.addTokenWithLiteral(tokenNumber, val)
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

var keywords = map[string]tokenType{
	"and":    tokenAnd,
	"class":  tokenClass,
	"else":   tokenElse,
	"false":  tokenFalse,
	"for":    tokenFor,
	"fun":    tokenFun,
	"if":     tokenIf,
	"nil":    tokenNil,
	"or":     tokenOr,
	"print":  tokenPrint,
	"return": tokenReturn,
	"super":  tokenSuper,
	"this":   tokenThis,
	"true":   tokenTrue,
	"var":    tokenVar,
	"while":  tokenWhile,
}

func (s *scanner) identifier() {
	for s.isAlphaNumeric(s.peek()) {
		s.advance()
	}

	text := s.source[s.start:s.current]
	tknType, found := keywords[text]
	if !found {
		tknType = tokenIdentifier
	}
	s.addToken(tknType)
}

func (s *scanner) isAlphaNumeric(char rune) bool {
	return s.isAlpha(char) || s.isDigit(char)
}
