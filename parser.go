package main

import "glox/ast"

type parseError struct {
	msg string
}

func (p parseError) Error() string {
	return p.msg
}

/**
Parser grammar:

	program     => declaration* EOF
	declaration => varDecl | statement
	varDecl     => "var" IDENTIFIER ( "=" expression )? ";"
	statement   => exprStmt | printStmt | block
	exprStmt    => expression ";"
	printStmt   => "print" expression ";"
	block       => "{" declaration* "}" ;
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
	tokens  []ast.Token
	current int
}

func (p *parser) parse() []ast.Stmt {
	var statements []ast.Stmt
	for !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	return statements
}

func (p *parser) declaration() ast.Stmt {
	defer func() {
		if err := recover(); err != nil {
			if _, ok := err.(parseError); ok {
				p.synchronize()
			} else {
				panic(err)
			}
		}
	}()

	if p.match(ast.TokenVar) {
		return p.varDeclaration()
	}
	return p.statement()
}

func (p *parser) varDeclaration() ast.Stmt {
	name := p.consume(ast.TokenIdentifier, "Expect variable name")
	var initializer ast.Expr
	if p.match(ast.TokenEqual) {
		initializer = p.expression()
	}
	p.consume(ast.TokenSemicolon, "Expect ';' after variable declaration")
	return ast.VarStmt{Name: name, Initializer: initializer}
}

func (p *parser) statement() ast.Stmt {
	if p.match(ast.TokenPrint) {
		return p.printStatement()
	}
	if p.match(ast.TokenLeftBrace) {
		return ast.BlockStmt{Statements: p.block()}
	}
	return p.expressionStatement()
}

func (p *parser) printStatement() ast.Stmt {
	exp := p.expression()
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.PrintStmt{Expr: exp}
}

func (p *parser) block() []ast.Stmt {
	var statements []ast.Stmt
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	p.consume(ast.TokenRightBrace, "Expect '}' after block.")
	return statements
}

func (p *parser) expressionStatement() ast.Stmt {
	exp := p.expression()
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.ExpressionStmt{Expr: exp}
}

func (p *parser) expression() ast.Expr {
	return p.assignment()
}

func (p *parser) assignment() ast.Expr {
	exp := p.series()
	if p.match(ast.TokenEqual) {
		equals := p.previous()
		if varExpr, ok := exp.(ast.VariableExpr); ok {
			value := p.assignment()
			return ast.AssignExpr{Name: varExpr.Name, Value: value}
		}
		_ = p.error(equals, "Invalid assignment target.")
	}

	return exp
}

func (p *parser) series() ast.Expr {
	expr := p.ternary()
	for p.match(ast.TokenComma) {
		operator := p.previous()
		right := p.ternary()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) ternary() ast.Expr {
	expr := p.equality()

	if p.match(ast.TokenQuestionMark) {
		cond1 := p.ternary()
		p.consume(ast.TokenColon, "Expect ':' after conditional.")
		cond2 := p.ternary()
		expr = ast.TernaryExpr{Cond: expr, Left: cond1, Right: cond2}
	}

	return expr
}

func (p *parser) equality() ast.Expr {
	expr := p.comparison()

	for p.match(ast.TokenBangEqual, ast.TokenEqualEqual) {
		operator := p.previous()
		right := p.comparison()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) comparison() ast.Expr {
	expr := p.term()

	for p.match(ast.TokenGreater, ast.TokenGreaterEqual, ast.TokenLess, ast.TokenLessEqual) {
		operator := p.previous()
		right := p.term()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) term() ast.Expr {
	expr := p.factor()

	for p.match(ast.TokenMinus, ast.TokenPlus) {
		operator := p.previous()
		right := p.factor()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) factor() ast.Expr {
	expr := p.unary()

	for p.match(ast.TokenSlash, ast.TokenStar) {
		operator := p.previous()
		right := p.unary()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) unary() ast.Expr {
	if p.match(ast.TokenBang, ast.TokenMinus) {
		operator := p.previous()
		right := p.unary()
		return ast.UnaryExpr{Operator: operator, Right: right}
	}

	return p.primary()
}

func (p *parser) primary() ast.Expr {
	switch {
	case p.match(ast.TokenFalse):
		return ast.LiteralExpr{Value: false}
	case p.match(ast.TokenTrue):
		return ast.LiteralExpr{Value: true}
	case p.match(ast.TokenNil):
		return ast.LiteralExpr{}
	case p.match(ast.TokenNumber, ast.TokenString):
		return ast.LiteralExpr{Value: p.previous().Literal}
	case p.match(ast.TokenLeftParen):
		exp := p.expression()
		p.consume(ast.TokenRightParen, "Expect ')' after expression.")
		return ast.GroupingExpr{Expression: exp}
	case p.match(ast.TokenIdentifier):
		return ast.VariableExpr{Name: p.previous()}
	}

	panic(p.error(p.peek(), "Expect expression."))
}

func (p *parser) consume(tokenType ast.TokenType, message string) ast.Token {
	if p.check(tokenType) {
		return p.advance()
	}

	panic(p.error(p.peek(), message))
}

func (p *parser) error(Token ast.Token, message string) error {
	reportTokenErr(Token, message)
	return parseError{}
}

func (p *parser) synchronize() {
	p.advance()
	for !p.isAtEnd() {
		if p.previous().TokenType == ast.TokenSemicolon {
			return
		}

		switch p.peek().TokenType {
		case ast.TokenClass, ast.TokenFor, ast.TokenFun, ast.TokenIf, ast.TokenPrint, ast.TokenReturn, ast.TokenVar, ast.TokenWhile:
			return
		}

		p.advance()
	}
}

func (p *parser) match(types ...ast.TokenType) bool {
	for _, tokenType := range types {
		if p.check(tokenType) {
			p.advance()
			return true
		}
	}

	return false
}

func (p *parser) check(tokenType ast.TokenType) bool {
	if p.isAtEnd() {
		return false
	}

	return p.peek().TokenType == tokenType
}

func (p *parser) advance() ast.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *parser) isAtEnd() bool {
	return p.peek().TokenType == ast.TokenEof
}

func (p *parser) peek() ast.Token {
	return p.tokens[p.current]
}

func (p *parser) previous() ast.Token {
	return p.tokens[p.current-1]
}
