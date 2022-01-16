//go:generate go run cmd/ast.go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

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

var (
	hadError        bool
	hadRuntimeError bool
)

var interp = interpreter{}

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
	scannr := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !scannr.Scan() {
			break
		}

		line := scannr.Text()
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
	scannr := scanner{source: source}
	tokens := scannr.scanTokens()

	parsr := parser{tokens: tokens}
	statements := parsr.parse()

	if hadError {
		return
	}

	interp.interpret(statements)
}

// TODO: Fix these errors. See Lox.error, p91

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

type token struct {
	tknType tokenType
	lexeme  string
	literal interface{}
	line    int
}

func (t token) String() string {
	return fmt.Sprintf("%d %s %s", t.tknType, t.lexeme, t.literal)
}

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

type astPrinter struct{}

func (a astPrinter) visitAssignExpr(expr assignExpr) interface{} {
	return a.parenthesize("= "+expr.name.lexeme, expr.value)
}

func (a astPrinter) print(expr expr) string {
	return expr.accept(a).(string)
}

func (a astPrinter) visitVariableExpr(expr variableExpr) interface{} {
	return expr.name.lexeme
}

func (a astPrinter) visitTernaryExpr(expr ternaryExpr) interface{} {
	return a.parenthesize("?:", expr.cond, expr.left, expr.right)
}

func (a astPrinter) visitBinaryExpr(expr binaryExpr) interface{} {
	return a.parenthesize(expr.operator.lexeme, expr.left, expr.right)
}

func (a astPrinter) visitGroupingExpr(expr groupingExpr) interface{} {
	return a.parenthesize("group", expr.expression)
}

func (a astPrinter) visitLiteralExpr(expr literalExpr) interface{} {
	if expr.value == nil {
		return "nil"
	}

	return fmt.Sprint(expr.value)
}

func (a astPrinter) visitUnaryExpr(expr unaryExpr) interface{} {
	return a.parenthesize(expr.operator.lexeme, expr.right)
}

func (a astPrinter) parenthesize(name string, exprs ...expr) string {
	var str string

	str += "(" + name
	for _, expr := range exprs {
		str += " " + a.print(expr)
	}
	str += ")"

	return str
}

/**
Parser grammar:

	program     => declaration* EOF
	declaration => varDecl | statement
	varDecl     => "var" IDENTIFIER ( "=" expression )? ";"
	statement   => exprStmt | printStmt
	exprStmt    => expression ";"
	printStmt   => "print" expression ";"
	expression  => assignment
	assignment  => IDENTIFIER "=" assignment | series
	series      => ternary ( "," ternary )*
	ternary     => expression ( "?" ternary ":" ternary )*
	equality    => comparison ( ( "!=" | "==" ) comparison )*
	comparison  => term ( ( ">" | ">=" | "<" | "<=" ) term )*
	term        => factor ( ( "+" | "-" ) factor )*
	factor      => unary ( ( "/" | "*" ) unary )*
	unary       => ( "!" | "-" ) unary | primary
	primary     => NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER

*/

type parser struct {
	tokens  []token
	current int
}

func (p *parser) parse() []stmt {
	var statements []stmt
	for !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	return statements
}

func (p *parser) declaration() stmt {
	if p.match(tokenVar) {
		return p.varDeclaration()
	}
	return p.statement()
}

func (p *parser) varDeclaration() stmt {
	name := p.consume(tokenIdentifier, "Expect variable name")
	var initializer expr
	if p.match(tokenEqual) {
		initializer = p.expression()
	}
	p.consume(tokenSemicolon, "Expect ';' after variable declaration")
	return varStmt{name, initializer}
}

func (p *parser) statement() stmt {
	if p.match(tokenPrint) {
		return p.printStatement()
	}
	return p.expressionStatement()
}

func (p *parser) printStatement() stmt {
	exp := p.expression()
	p.consume(tokenSemicolon, "Expect ';' after value")
	return printStmt{exp}
}

func (p *parser) expressionStatement() stmt {
	exp := p.expression()
	p.consume(tokenSemicolon, "Expect ';' after value")
	return expressionStmt{exp}
}

func (p *parser) expression() expr {
	return p.assignment()
}

func (p *parser) assignment() expr {
	exp := p.series()
	if p.match(tokenEqual) {
		equals := p.previous()
		if varExpr, ok := exp.(variableExpr); ok {
			value := p.assignment()
			return assignExpr{varExpr.name, value}
		}
		_ = p.error(equals, "Invalid assignment target.")
	}

	return exp
}

func (p *parser) series() expr {
	expr := p.ternary()
	for p.match(tokenComma) {
		operator := p.previous()
		right := p.ternary()
		expr = binaryExpr{expr, operator, right}
	}

	return expr
}

func (p *parser) ternary() expr {
	expr := p.equality()

	if p.match(tokenQuestionMark) {
		cond1 := p.ternary()
		p.consume(tokenColon, "Expect ':' after conditional.")
		cond2 := p.ternary()
		expr = ternaryExpr{expr, cond1, cond2}
	}

	return expr
}

func (p *parser) equality() expr {
	expr := p.comparison()

	for p.match(tokenBangEqual, tokenEqualEqual) {
		operator := p.previous()
		right := p.comparison()
		expr = binaryExpr{expr, operator, right}
	}

	return expr
}

func (p *parser) comparison() expr {
	expr := p.term()

	for p.match(tokenGreater, tokenGreaterEqual, tokenLess, tokenLessEqual) {
		operator := p.previous()
		right := p.term()
		expr = binaryExpr{expr, operator, right}
	}

	return expr
}

func (p *parser) term() expr {
	expr := p.factor()

	for p.match(tokenMinus, tokenPlus) {
		operator := p.previous()
		right := p.factor()
		expr = binaryExpr{expr, operator, right}
	}

	return expr
}

func (p *parser) factor() expr {
	expr := p.unary()

	for p.match(tokenSlash, tokenStar) {
		operator := p.previous()
		right := p.unary()
		expr = binaryExpr{expr, operator, right}
	}

	return expr
}

func (p *parser) unary() expr {
	if p.match(tokenBang, tokenMinus) {
		operator := p.previous()
		right := p.unary()
		return unaryExpr{operator, right}
	}

	return p.primary()
}

type parseError struct {
	msg string
}

func (p parseError) Error() string {
	return p.msg
}

func (p *parser) primary() expr {
	switch {
	case p.match(tokenFalse):
		return literalExpr{false}
	case p.match(tokenTrue):
		return literalExpr{true}
	case p.match(tokenNil):
		return literalExpr{nil}
	case p.match(tokenNumber, tokenString):
		return literalExpr{p.previous().literal}
	case p.match(tokenLeftParen):
		exp := p.expression()
		p.consume(tokenRightParen, "Expect ')' after expression.")
		return groupingExpr{exp}
	case p.match(tokenIdentifier):
		return variableExpr{p.previous()}
	}

	panic(p.error(p.peek(), "Expect expression."))
}

func (p *parser) consume(tknType tokenType, message string) token {
	if p.check(tknType) {
		return p.advance()
	}

	panic(p.error(p.peek(), message))
}

func (p *parser) error(token token, message string) error {
	reportTokenErr(token, message)
	return parseError{}
}

func (p *parser) synchronize() {
	p.advance()
	for !p.isAtEnd() {
		if p.previous().tknType == tokenSemicolon {
			return
		}

		switch p.peek().tknType {
		case tokenClass, tokenFor, tokenFun, tokenIf, tokenPrint, tokenReturn, tokenVar, tokenWhile:
			return
		}

		p.advance()
	}
}

func (p *parser) match(types ...tokenType) bool {
	for _, tknType := range types {
		if p.check(tknType) {
			p.advance()
			return true
		}
	}

	return false
}

func (p *parser) check(tknType tokenType) bool {
	if p.isAtEnd() {
		return false
	}

	return p.peek().tknType == tknType
}

func (p *parser) advance() token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *parser) isAtEnd() bool {
	return p.peek().tknType == tokenEof
}

func (p *parser) peek() token {
	return p.tokens[p.current]
}

func (p *parser) previous() token {
	return p.tokens[p.current-1]
}

type interpreter struct {
	environment environment
}

func (in *interpreter) visitAssignExpr(expr assignExpr) interface{} {
	val := in.evaluate(expr.value)
	in.environment.assign(expr.name, val)
	return val
}

func (in *interpreter) visitVarStmt(stmt varStmt) interface{} {
	var val interface{}
	if stmt.initializer != nil {
		val = in.evaluate(stmt.initializer)
	}
	in.environment.define(stmt.name.lexeme, val)
	return nil
}

func (in *interpreter) visitExpressionStmt(stmt expressionStmt) interface{} {
	in.evaluate(stmt.expr)
	return nil
}

func (in *interpreter) visitPrintStmt(stmt printStmt) interface{} {
	value := in.evaluate(stmt.expr)
	fmt.Println(in.stringify(value))
	return nil
}

func (in *interpreter) interpret(stmts []stmt) {
	defer func() {
		if err := recover(); err != nil {
			if e, ok := err.(runtimeError); ok {
				reportRuntimeErr(e)
			}
			fmt.Printf("Error: %s\n", err)
		}
	}()

	for _, statement := range stmts {
		in.execute(statement)
	}
}

func (in *interpreter) execute(stmt stmt) {
	stmt.accept(in)
}

func (in *interpreter) evaluate(expr expr) interface{} {
	return expr.accept(in)
}

func (in *interpreter) visitVariableExpr(expr variableExpr) interface{} {
	return in.environment.get(expr.name)
}

func (in *interpreter) visitBinaryExpr(expr binaryExpr) interface{} {
	left := in.evaluate(expr.left)
	right := in.evaluate(expr.right)

	switch expr.operator.tknType {
	case tokenPlus:
		_, leftIsFloat := left.(float64)
		_, rightIsFloat := right.(float64)
		if leftIsFloat && rightIsFloat {
			return left.(float64) + right.(float64)
		}
		_, leftIsString := left.(string)
		_, rightIsString := right.(string)
		if leftIsString && rightIsString {
			return left.(string) + right.(string)
		}
		panic(runtimeError{expr.operator, "Operands must be two numbers or two strings"})
	case tokenMinus:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) - right.(float64)
	case tokenSlash:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) / right.(float64)
	case tokenStar:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) * right.(float64)
	// comparison
	case tokenGreater:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) > right.(float64)
	case tokenGreaterEqual:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) >= right.(float64)
	case tokenLess:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) < right.(float64)
	case tokenLessEqual:
		in.checkNumberOperands(expr.operator, left, right)
		return left.(float64) <= right.(float64)
	case tokenEqual:
		return left == right
	case tokenBangEqual:
		return left != right
	case tokenComma:
		return right
	}
	return nil
}

func (in *interpreter) visitGroupingExpr(expr groupingExpr) interface{} {
	return in.evaluate(expr.expression)
}

func (in *interpreter) visitLiteralExpr(expr literalExpr) interface{} {
	return expr.value
}

func (in *interpreter) visitUnaryExpr(expr unaryExpr) interface{} {
	right := in.evaluate(expr.right)
	switch expr.operator.tknType {
	case tokenBang:
		return !in.isTruthy(right)
	case tokenMinus:
		in.checkNumberOperand(expr.operator, right)
		return -right.(float64)
	}
	return nil
}

func (in *interpreter) visitTernaryExpr(expr ternaryExpr) interface{} {
	cond := in.evaluate(expr.cond)
	if in.isTruthy(cond) {
		return in.evaluate(expr.left)
	}
	return in.evaluate(expr.right)
}

func (in *interpreter) isTruthy(val interface{}) bool {
	if val == nil {
		return false
	}
	if v, ok := val.(bool); ok {
		return v
	}
	return true
}

func (in *interpreter) checkNumberOperand(operator token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in *interpreter) checkNumberOperands(operator token, left interface{}, right interface{}) {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return
		}
	}
	panic(runtimeError{operator, "Operands must be number"})
}

func (in *interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}

type environment struct {
	values map[string]interface{}
}

func (e *environment) define(name string, value interface{}) {
	if e.values == nil {
		e.values = make(map[string]interface{})
	}
	e.values[name] = value
}

func (e environment) get(name token) interface{} {
	if v, ok := e.values[name.lexeme]; ok {
		return v
	}
	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.lexeme)})
}

func (e *environment) assign(name token, value interface{}) {
	if _, ok := e.values[name.lexeme]; ok {
		e.define(name.lexeme, value)
		return
	}
	panic(runtimeError{name, fmt.Sprintf("Undefined variable '%s'", name.lexeme)})
}
