package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

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

var interpreter = Interpreter{}

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
	if hadRuntimeError {
		os.Exit(70)
	}
}

func run(source string) {
	scanner := Scanner{source: source}
	tokens := scanner.scanTokens()

	parser := Parser{tokens: tokens}
	statements := parser.parse()

	if hadError {
		return
	}

	interpreter.interpret(statements)
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
	fmt.Printf("[line %d] Error%s: %s\n", line, where, message)
	hadError = true
}

type runtimeError struct {
	token Token
	msg   string
}

func (r runtimeError) Error() string {
	return fmt.Sprintf("%s\n[line %d]", r.msg, r.token.Line)
}

func reportRuntimeErr(err error) {
	fmt.Println(err.Error())
	hadRuntimeError = true
}

type Token struct {
	tokenType TokenType
	Lexeme    string
	Literal   interface{}
	Line      int
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

func (s *Scanner) scanTokens() []Token {
	for !s.isAtEnd() {
		// we're at the beginning of the next lexeme
		s.start = s.current
		s.scanToken()
	}

	s.tokens = append(s.tokens, Token{tokenEof, "", nil, s.line})
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
	case ':':
		s.addToken(tokenColon)
	case '*':
		s.addToken(tokenStar)
	case '?':
		s.addToken(tokenQuestionMark)

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
	s.tokens = append(s.tokens, Token{tokenType, text, literal, s.line})
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

	value := s.source[s.start:s.current]
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

type TernaryExpr struct {
	cond  Expr
	left  Expr
	right Expr
}

func (t TernaryExpr) accept(visitor ExprVisitor) interface{} {
	return visitor.visitTernaryExpr(t)
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
	return visitor.visitLiteralExpr(b)
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
	visitLiteralExpr(expr LiteralExpr) interface{}
	visitUnaryExpr(expr UnaryExpr) interface{}
	visitTernaryExpr(expr TernaryExpr) interface{}
}

type AstPrinter struct{}

func (a AstPrinter) visitTernaryExpr(expr TernaryExpr) interface{} {
	return a.Parenthesize("?:", expr.cond, expr.left, expr.right)
}

func (a AstPrinter) Print(expr Expr) string {
	return expr.accept(a).(string)
}

func (a AstPrinter) visitBinaryExpr(expr BinaryExpr) interface{} {
	return a.Parenthesize(expr.operator.Lexeme, expr.left, expr.right)
}

func (a AstPrinter) visitGroupingExpr(expr GroupingExpr) interface{} {
	return a.Parenthesize("group", expr.expression)
}

func (a AstPrinter) visitLiteralExpr(expr LiteralExpr) interface{} {
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

type Stmt interface {
	accept(v StmtVisitor)
}

type ExpressionStmt struct {
	expr Expr
}

func (e ExpressionStmt) accept(v StmtVisitor) {
	v.visitExpressionStmt(e)
}

type PrintStmt struct {
	expr Expr
}

func (p PrintStmt) accept(v StmtVisitor) {
	v.visitPrintStmt(p)
}

type StmtVisitor interface {
	visitExpressionStmt(expr ExpressionStmt)
	visitPrintStmt(expr PrintStmt)
}

/**
Parser grammar:

	program    => statement* EOF
  statement  => exprStmt | printStmt
	exprStmt   => series ";"
	printStmt  => "print" series ";"
	series     => ternary ( "," ternary )*
	ternary    => expression ( "?" ternary ":" ternary )*
	expression => equality
	equality   => comparison ( ( "!=" | "==" ) comparison )*
	comparison => term ( ( ">" | ">=" | "<" | "<=" ) term )*
	term       => factor ( ( "+" | "-" ) factor )*
	factor     => unary ( ( "/" | "*" ) unary )*
	unary      => ( "!" | "-" ) unary | primary
	primary    => NUMBER | STRING | "true" | "false" | "nil" | "(" series ")"

*/

type Parser struct {
	tokens  []Token
	current int
}

func (p *Parser) parse() []Stmt {
	var statements []Stmt
	for !p.isAtEnd() {
		statements = append(statements, p.statement())
	}
	return statements
}

func (p *Parser) statement() Stmt {
	if p.match(tokenPrint) {
		return p.printStatement()
	}
	return p.expressionStatement()
}

func (p *Parser) printStatement() Stmt {
	expr := p.series()
	p.consume(tokenSemicolon, "Expect ';' after value")
	return PrintStmt{expr}
}

func (p *Parser) expressionStatement() Stmt {
	expr := p.series()
	p.consume(tokenSemicolon, "Expect ';' after value")
	return ExpressionStmt{expr}
}

func (p *Parser) series() Expr {
	expr := p.ternary()
	for p.match(tokenComma) {
		operator := p.previous()
		right := p.ternary()
		expr = BinaryExpr{expr, operator, right}
	}

	return expr
}

func (p *Parser) ternary() Expr {
	expr := p.expression()

	if p.match(tokenQuestionMark) {
		cond1 := p.ternary()
		p.consume(tokenColon, "Expect ':' after conditional.")
		cond2 := p.ternary()
		expr = TernaryExpr{expr, cond1, cond2}
	}

	return expr
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

type parseError struct {
	msg string
}

func (p parseError) Error() string {
	return p.msg
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
		expr := p.series()
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
	return parseError{}
}

func (p *Parser) synchronize() {
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

func (p *Parser) check(tokenType TokenType) bool {
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

type Interpreter struct{}

func (in Interpreter) visitExpressionStmt(statement ExpressionStmt) {
	in.evaluate(statement.expr)
}

func (in Interpreter) visitPrintStmt(statement PrintStmt) {
	value := in.evaluate(statement.expr)
	fmt.Println(in.stringify(value))
}

func (in Interpreter) interpret(statements []Stmt) {
	defer func() {
		if err := recover(); err != nil {
			if e, ok := err.(runtimeError); ok {
				reportRuntimeErr(e)
			}
		}
	}()

	for _, statement := range statements {
		in.execute(statement)
	}
}

func (in Interpreter) execute(statement Stmt) {
	statement.accept(in)
}

func (in Interpreter) evaluate(expr Expr) interface{} {
	return expr.accept(in)
}

func (in Interpreter) visitBinaryExpr(expr BinaryExpr) interface{} {
	left := in.evaluate(expr.left)
	right := in.evaluate(expr.right)

	switch expr.operator.tokenType {
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

func (in Interpreter) visitGroupingExpr(expr GroupingExpr) interface{} {
	return in.evaluate(expr.expression)
}

func (in Interpreter) visitLiteralExpr(expr LiteralExpr) interface{} {
	return expr.value
}

func (in Interpreter) visitUnaryExpr(expr UnaryExpr) interface{} {
	right := in.evaluate(expr.right)
	switch expr.operator.tokenType {
	case tokenBang:
		return !in.isTruthy(right)
	case tokenMinus:
		in.checkNumberOperand(expr.operator, right)
		return -right.(float64)
	}
	return nil
}

func (in Interpreter) visitTernaryExpr(expr TernaryExpr) interface{} {
	// TODO implement me
	panic("implement me")
}

func (in Interpreter) isTruthy(val interface{}) bool {
	if val == nil {
		return false
	}
	if v, ok := val.(bool); ok {
		return v
	}
	return true
}

func (in Interpreter) checkNumberOperand(operator Token, operand interface{}) {
	if _, ok := operand.(float64); ok {
		return
	}
	panic(runtimeError{operator, "Operand must be a number"})
}

func (in Interpreter) checkNumberOperands(operator Token, left interface{}, right interface{}) {
	if _, ok := left.(float64); ok {
		if _, ok = right.(float64); ok {
			return
		}
	}
	panic(runtimeError{operator, "Operands must be number"})
}

func (in Interpreter) stringify(value interface{}) string {
	if value == nil {
		return "nil"
	}
	return fmt.Sprint(value)
}
