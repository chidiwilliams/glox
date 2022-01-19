package main

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
	defer func() {
		if err := recover(); err != nil {
			if _, ok := err.(parseError); ok {
				p.synchronize()
			} else {
				panic(err)
			}
		}
	}()

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
	if p.match(tokenLeftBrace) {
		return blockStmt{p.block()}
	}
	return p.expressionStatement()
}

func (p *parser) printStatement() stmt {
	exp := p.expression()
	p.consume(tokenSemicolon, "Expect ';' after value")
	return printStmt{exp}
}

func (p *parser) block() []stmt {
	var statements []stmt
	for !p.check(tokenRightBrace) && !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	p.consume(tokenRightBrace, "Expect '}' after block.")
	return statements
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
