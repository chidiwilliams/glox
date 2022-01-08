package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

var hadError bool

type TokenType uint8

const (
	// single-character tokens
	tokenLeftParen TokenType = iota
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

	parser := Parser{tokens: tokens}
	expr := parser.parse()

	if hadError {
		return
	}

	fmt.Println(AstPrinter{}.Print(expr))
}

// TODO: Fix these errors. See Lox.error, p91

func reportTokenErr(token Token, message string) {
	if token.tokenType == tokenEof {
		report(token.Line, " at end", message)
	} else {
		report(token.Line, " at '"+token.Lexeme+"'", message)
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
	tokenType TokenType
	Lexeme    string
	Literal   interface{}
	Line      int
}

func NewToken(tknType TokenType, lexeme string, literal interface{}, line int) Token {
	return Token{tokenType: tknType, Lexeme: lexeme, Literal: literal, Line: line}
}

func (t Token) String() string {
	return fmt.Sprintf("%d %s %s", t.tokenType, t.Lexeme, t.Literal)
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
		var nextToken TokenType
		if s.match('=') {
			nextToken = tokenBangEqual
		} else {
			nextToken = tokenBang
		}
		s.addToken(nextToken)
	case '=':
		var nextToken TokenType
		if s.match('=') {
			nextToken = tokenEqualEqual
		} else {
			nextToken = tokenEqual
		}
		s.addToken(nextToken)
	case '<':
		var nextToken TokenType
		if s.match('=') {
			nextToken = tokenLessEqual
		} else {
			nextToken = tokenLess
		}
		s.addToken(nextToken)
	case '>':
		var nextToken TokenType
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

func (s *Scanner) addToken(tokenType TokenType) {
	s.addTokenWithLiteral(tokenType, nil)
}

func (s *Scanner) addTokenWithLiteral(tokenType TokenType, literal interface{}) {
	text := s.source[s.start:s.current]
	s.tokens = append(s.tokens, NewToken(tokenType, text, literal, s.line))
}

func (s *Scanner) match(expected rune) bool {
	if s.isAtEnd() {
		return false
	}

	if rune(s.source[s.current]) != expected {
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

var keywords = map[string]TokenType{
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

func (s *Scanner) identifier() {
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

func (s *Scanner) isAlphaNumeric(char rune) bool {
	return s.isAlpha(char) || s.isDigit(char)
}

type Expr interface {
	accept(visitor ExprVisitor) interface{}
}

type BinaryExpr struct {
	left     Expr
	operator Token
	right    Expr
}

func (b BinaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitBinaryExpr(b)
}

type GroupingExpr struct {
	expression Expr
}

func (b GroupingExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitGroupingExpr(b)
}

type LiteralExpr struct {
	value interface{}
}

func (b LiteralExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitObjectExpr(b)
}

type UnaryExpr struct {
	operator Token
	right    Expr
}

func (b UnaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitUnaryExpr(b)
}

type ExprVisitor interface {
	visitBinaryExpr(expr BinaryExpr) interface{}
	visitGroupingExpr(expr GroupingExpr) interface{}
	visitObjectExpr(expr LiteralExpr) interface{}
	visitUnaryExpr(expr UnaryExpr) interface{}
}

type AstPrinter struct{}

func (a AstPrinter) Print(expr Expr) string {
	return expr.accept(a).(string)
}

func (a AstPrinter) visitBinaryExpr(expr BinaryExpr) interface{} {
	return a.Parenthesize(expr.operator.Lexeme, expr.left, expr.right)
}

func (a AstPrinter) visitGroupingExpr(expr GroupingExpr) interface{} {
	return a.Parenthesize("group", expr.expression)
}

func (a AstPrinter) visitObjectExpr(expr LiteralExpr) interface{} {
	if expr.value == nil {
		return "nil"
	}

	return fmt.Sprint(expr.value)
}

func (a AstPrinter) visitUnaryExpr(expr UnaryExpr) interface{} {
	return a.Parenthesize(expr.operator.Lexeme, expr.right)
}

func (a AstPrinter) Parenthesize(name string, exprs ...Expr) string {
	var str string

	str += "(" + name
	for _, expr := range exprs {
		str += " " + a.Print(expr)
	}
	str += ")"

	return str
}

/**
Parser grammar:

	expression => equality
	equality   => comparison ( ( "!=" | "==" ) comparison )*
	comparison => term ( ( ">" | ">=" | "<" | "<=" ) term )*
	term       => factor ( ( "+" | "-" ) factor )*
	factor     => unary ( ( "/" | "*" ) unary )*
	unary      => ( "!" | "-" ) unary | primary
	primary    => NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"

*/

type Parser struct {
	tokens  []Token
	current int
}

func (p Parser) parse() (expr Expr) {
	defer func() {
		recover()
	}()

	return p.expression()
}

func (p *Parser) expression() Expr {
	return p.equality()
}

func (p *Parser) equality() Expr {
	expr := p.comparison()

	for p.match(tokenBangEqual, tokenEqualEqual) {
		operator := p.previous()
		right := p.comparison()
		expr = BinaryExpr{expr, operator, right}
	}

	return expr
}

func (p *Parser) comparison() Expr {
	expr := p.term()

	for p.match(tokenGreater, tokenGreaterEqual, tokenLess, tokenLessEqual) {
		operator := p.previous()
		right := p.term()
		expr = BinaryExpr{expr, operator, right}
	}

	return expr
}

func (p *Parser) term() Expr {
	expr := p.factor()

	for p.match(tokenMinus, tokenPlus) {
		operator := p.previous()
		right := p.factor()
		expr = BinaryExpr{expr, operator, right}
	}

	return expr
}

func (p *Parser) factor() Expr {
	expr := p.unary()

	for p.match(tokenSlash, tokenStar) {
		operator := p.previous()
		right := p.unary()
		expr = BinaryExpr{expr, operator, right}
	}

	return expr
}

func (p *Parser) unary() Expr {
	if p.match(tokenBang, tokenMinus) {
		operator := p.previous()
		right := p.unary()
		return UnaryExpr{operator, right}
	}

	return p.primary()
}

func (p *Parser) primary() Expr {
	switch {
	case p.match(tokenFalse):
		return LiteralExpr{false}
	case p.match(tokenTrue):
		return LiteralExpr{true}
	case p.match(tokenNil):
		return LiteralExpr{nil}
	case p.match(tokenNumber, tokenString):
		return LiteralExpr{p.previous().Literal}
	case p.match(tokenLeftParen):
		expr := p.expression()
		p.consume(tokenRightParen, "Expect ')' after expression.")
		return GroupingExpr{expr}
	}

	panic(p.error(p.peek(), "Expect expression."))
}

func (p *Parser) consume(tokenType TokenType, message string) Token {
	if p.check(tokenType) {
		return p.advance()
	}

	panic(p.error(p.peek(), message))
}

func (p *Parser) error(token Token, message string) error {
	reportTokenErr(token, message)
	return errors.New("parse error")
}

func (p Parser) synchronize() {
	p.advance()
	for !p.isAtEnd() {
		if p.previous().tokenType == tokenSemicolon {
			return
		}

		switch p.peek().tokenType {
		case tokenClass, tokenFor, tokenFun, tokenIf, tokenPrint, tokenReturn, tokenVar, tokenWhile:
			return
		}

		p.advance()
	}
}

func (p *Parser) match(types ...TokenType) bool {
	for _, tokenType := range types {
		if p.check(tokenType) {
			p.advance()
			return true
		}
	}

	return false
}

func (p Parser) check(tokenType TokenType) bool {
	if p.isAtEnd() {
		return false
	}

	return p.peek().tokenType == tokenType
}

func (p *Parser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) isAtEnd() bool {
	return p.peek().tokenType == tokenEof
}

func (p *Parser) peek() Token {
	return p.tokens[p.current]
}

func (p *Parser) previous() Token {
	return p.tokens[p.current-1]
}

// TODO: Implement comma operator
