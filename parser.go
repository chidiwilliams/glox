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

func (p *parser) parse() ([]stmt, error) {
	var statements []stmt
	for !p.isAtEnd() {
		decl, err := p.declaration()
		if err != nil {
			return nil, err
		}
		statements = append(statements, decl)
	}
	return statements, nil
}

func (p *parser) declaration() (stmt, error) {
	var statement stmt
	var err error

	if p.match(tokenVar) {
		statement, err = p.varDeclaration()
	} else {
		statement, err = p.statement()
	}

	if err != nil {
		if _, ok := err.(parseError); ok {
			p.synchronize()
			return nil, nil
		} else {
			return nil, err
		}
	}

	return statement, nil
}

func (p *parser) varDeclaration() (stmt, error) {
	name, err := p.consume(tokenIdentifier, "Expect variable name")
	if err != nil {
		return nil, err
	}
	var initializer expr
	if p.match(tokenEqual) {
		initializer, err = p.expression()
		if err != nil {
			return nil, err
		}
	}
	_, err = p.consume(tokenSemicolon, "Expect ';' after variable declaration")
	if err != nil {
		return nil, err
	}
	return varStmt{name, initializer}, nil
}

func (p *parser) statement() (stmt, error) {
	if p.match(tokenPrint) {
		return p.printStatement()
	}
	if p.match(tokenLeftBrace) {
		block, err := p.block()
		if err != nil {
			return nil, err
		}
		return blockStmt{block}, nil
	}
	return p.expressionStatement()
}

func (p *parser) printStatement() (stmt, error) {
	exp, err := p.expression()
	if err != nil {
		return nil, err
	}
	_, err = p.consume(tokenSemicolon, "Expect ';' after value")
	if err != nil {
		return nil, err
	}
	return printStmt{exp}, nil
}

func (p *parser) block() ([]stmt, error) {
	var statements []stmt
	for !p.check(tokenRightBrace) && !p.isAtEnd() {
		decl, err := p.declaration()
		if err != nil {
			return nil, err
		}
		statements = append(statements, decl)
	}
	_, err := p.consume(tokenRightBrace, "Expect '}' after block.")
	if err != nil {
		return nil, err
	}
	return statements, nil
}

func (p *parser) expressionStatement() (stmt, error) {
	exp, err := p.expression()
	if err != nil {
		return nil, err
	}
	_, err = p.consume(tokenSemicolon, "Expect ';' after value")
	if err != nil {
		return nil, err
	}
	return expressionStmt{exp}, nil
}

func (p *parser) expression() (expr, error) {
	return p.assignment()
}

func (p *parser) assignment() (expr, error) {
	exp, err := p.series()
	if err != nil {
		return nil, err
	}
	if p.match(tokenEqual) {
		equals := p.previous()
		if varExpr, ok := exp.(variableExpr); ok {
			value, err := p.assignment()
			if err != nil {
				return nil, err
			}
			return assignExpr{varExpr.name, value}, nil
		}
		_ = p.error(equals, "Invalid assignment target.")
	}

	return exp, nil
}

func (p *parser) series() (expr, error) {
	expr, err := p.ternary()
	if err != nil {
		return nil, err
	}
	for p.match(tokenComma) {
		operator := p.previous()
		right, err := p.ternary()
		if err != nil {
			return nil, err
		}
		expr = binaryExpr{expr, operator, right}
	}

	return expr, nil
}

func (p *parser) ternary() (expr, error) {
	expr, err := p.equality()
	if err != nil {
		return nil, err
	}

	if p.match(tokenQuestionMark) {
		cond1, err := p.ternary()
		if err != nil {
			return nil, err
		}
		_, err = p.consume(tokenColon, "Expect ':' after conditional.")
		if err != nil {
			return nil, err
		}
		cond2, err := p.ternary()
		if err != nil {
			return nil, err
		}
		expr = ternaryExpr{expr, cond1, cond2}
	}

	return expr, nil
}

func (p *parser) equality() (expr, error) {
	expr, err := p.comparison()
	if err != nil {
		return nil, err
	}

	for p.match(tokenBangEqual, tokenEqualEqual) {
		operator := p.previous()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = binaryExpr{expr, operator, right}
	}

	return expr, nil
}

func (p *parser) comparison() (expr, error) {
	expr, err := p.term()
	if err != nil {
		return nil, err
	}

	for p.match(tokenGreater, tokenGreaterEqual, tokenLess, tokenLessEqual) {
		operator := p.previous()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		expr = binaryExpr{expr, operator, right}
	}

	return expr, nil
}

func (p *parser) term() (expr, error) {
	expr, err := p.factor()
	if err != nil {
		return nil, err
	}

	for p.match(tokenMinus, tokenPlus) {
		operator := p.previous()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		expr = binaryExpr{expr, operator, right}
	}

	return expr, nil
}

func (p *parser) factor() (expr, error) {
	expr, err := p.unary()
	if err != nil {
		return nil, err
	}

	for p.match(tokenSlash, tokenStar) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = binaryExpr{expr, operator, right}
	}

	return expr, nil
}

func (p *parser) unary() (expr, error) {
	if p.match(tokenBang, tokenMinus) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		return unaryExpr{operator, right}, nil
	}

	return p.primary()
}

func (p *parser) primary() (expr, error) {
	switch {
	case p.match(tokenFalse):
		return literalExpr{false}, nil
	case p.match(tokenTrue):
		return literalExpr{true}, nil
	case p.match(tokenNil):
		return literalExpr{nil}, nil
	case p.match(tokenNumber, tokenString):
		return literalExpr{p.previous().literal}, nil
	case p.match(tokenLeftParen):
		exp, err := p.expression()
		if err != nil {
			return nil, err
		}
		_, err = p.consume(tokenRightParen, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}
		return groupingExpr{exp}, nil
	case p.match(tokenIdentifier):
		return variableExpr{p.previous()}, nil
	}

	return nil, p.error(p.peek(), "Expect expression.")
}

func (p *parser) consume(tknType tokenType, message string) (token, error) {
	if p.check(tknType) {
		return p.advance(), nil
	}

	return token{}, p.error(p.peek(), message)
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
