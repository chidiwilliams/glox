package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

var hadError bool

type token uint8

const (
	// single-character tokens
	tokenLeftParen token = iota
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
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}

		line := scanner.Text()
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
}

func run(source string) {
	scanner := NewScanner(source)
	tokens := scanner.scanTokens()
	for _, tkn := range tokens {
		s, _ := json.MarshalIndent(tkn, "", "\t")
		fmt.Printf("%s\n", s)
	}
}

func reportErr(line int, message string) {
	report(line, "", message)
}

func report(line int, where string, message string) {
	fmt.Printf("[line %d] Error%s: %s", line, where, message)
	hadError = true
}

type Token struct {
	TokenType token
	Lexeme    string
	Literal   interface{}
	Line      int
}

func NewToken(tokenType token, lexeme string, literal interface{}, line int) Token {
	return Token{TokenType: tokenType, Lexeme: lexeme, Literal: literal, Line: line}
}

func (t Token) String() string {
	return fmt.Sprintf("%d %s %s", t.TokenType, t.Lexeme, t.Literal)
}

type Scanner struct {
	start   int
	current int
	line    int
	source  string
	tokens  []Token
}

func NewScanner(source string) *Scanner {
	return &Scanner{source: source, line: 1}
}

func (s *Scanner) scanTokens() []Token {
	for !s.isAtEnd() {
		// we're at the beginning of the next lexeme
		s.start = s.current
		s.scanToken()
	}

	s.tokens = append(s.tokens, NewToken(tokenEof, "", nil, s.line))
	return s.tokens
}

func (s *Scanner) scanToken() {
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
	case '*':
		s.addToken(tokenStar)

	// with look-ahead
	case '!':
		var nextToken token
		if s.match('=') {
			nextToken = tokenBangEqual
		} else {
			nextToken = tokenBang
		}
		s.addToken(nextToken)
	case '=':
		var nextToken token
		if s.match('=') {
			nextToken = tokenEqualEqual
		} else {
			nextToken = tokenEqual
		}
		s.addToken(nextToken)
	case '<':
		var nextToken token
		if s.match('=') {
			nextToken = tokenLessEqual
		} else {
			nextToken = tokenLess
		}
		s.addToken(nextToken)
	case '>':
		var nextToken token
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

func (s *Scanner) isAtEnd() bool {
	return s.current >= len(s.source)
}

func (s *Scanner) advance() rune {
	curr := rune(s.source[s.current])
	s.current++
	return curr
}

func (s *Scanner) addToken(tokenType token) {
	s.addTokenWithLiteral(tokenType, nil)
}

func (s *Scanner) addTokenWithLiteral(tokenType token, literal interface{}) {
	text := s.source[s.start:s.current]
	s.tokens = append(s.tokens, NewToken(tokenType, text, literal, s.line))
}

func (s *Scanner) match(char rune) bool {
	if s.isAtEnd() {
		return false
	}

	if rune(s.source[s.current]) == char {
		return false
	}

	s.current++
	return true
}

func (s *Scanner) string() {
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

	value := s.source[s.start+1 : s.current+1]
	s.addTokenWithLiteral(tokenString, value)
}

func (s *Scanner) isDigit(r rune) bool {
	return r >= '0' && r <= '9'
}

func (s *Scanner) number() {
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

func (s *Scanner) peek() rune {
	if s.isAtEnd() {
		return '\000'
	}
	return rune(s.source[s.current])
}

func (s *Scanner) peekNext() rune {
	if s.current+1 >= len(s.source) {
		return '\000'
	}
	return rune(s.source[s.current+1])
}

func (s *Scanner) isAlpha(char rune) bool {
	return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char == '_')
}

func (s *Scanner) identifier() {
	for s.isAlphaNumeric(s.peek()) {
		s.advance()
	}

	text := s.source[s.start:s.current]
	tokenType, found := keywords[text]
	if !found {
		tokenType = tokenIdentifier
	}
	s.addToken(tokenType)
}

func (s *Scanner) isAlphaNumeric(char rune) bool {
	return s.isAlpha(char) || s.isDigit(char)
}

var keywords = map[string]token{
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
