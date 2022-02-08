package main

import (
	"glox/ast"
)

type parseError struct {
	msg string
}

func (p parseError) Error() string {
	return p.msg
}

/**
Parser grammar:

	program     => declaration* EOF
	declaration => funcDecl | varDecl | statement
	funDecl     => "fun" function
	function    => IDENTIFIER "(" parameters? ")" block
	parameters  => IDENTIFIER ( "," IDENTIFIER )*
	varDecl     => "var" IDENTIFIER ( "=" expression )? ";"
	statement   => exprStmt | ifStmt | forStmt | printStmt | returnStmt | whileStmt
									| breakStmt | continueStmt | block
	exprStmt    => expression ";"
	ifStmt      => "if" "(" expression ")" statement ( "else" statement )?
	forStmt     => "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
	printStmt   => "print" expression ";"
	returnStmt  => "return" expression? ";"
	whileStmt   => "while" "(" expression ")" statement
	block       => "{" declaration* "}" ;
	expression  => series
	series      => assignment ( "," assignment )*
	assignment  => IDENTIFIER "=" assignment | ternary
	ternary     => logic_or ( "?" ternary ":" ternary )*
	logic_or    => logic_and ( "or" logic_and )*
	logic_and   => equality ( "and" equality )*
	equality    => comparison ( ( "!=" | "==" ) comparison )
	comparison  => term ( ( ">" | ">=" | "<" | "<=" ) term )*
	term        => factor ( ( "+" | "-" ) factor )*
	factor      => unary ( ( "/" | "*" ) unary )*
	unary       => ( "!" | "-" ) unary | call
	call        => primary ( "(" arguments? ")" )*
	arguments   => expression ( "," expression )*
	primary     => NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER

	Reference: C Operator Precedence https://en.cppreference.com/w/c/language/operator_precedence

*/

type parser struct {
	tokens  []ast.Token
	current int
	loop    int
}

func (p *parser) parse() []ast.Stmt {
	var statements []ast.Stmt
	for !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	return statements
}

// declaration parses declaration statements. A declaration statement is
// a variable declaration or a regular statement. If the statement contains
// a parse error, it skips to the start of the next statement and returns nil.
func (p *parser) declaration() ast.Stmt {
	defer func() {
		if err := recover(); err != nil {
			// If the error is a parseError, synchronize to
			// the next statement. If not, propagate the panic.
			if _, ok := err.(parseError); ok {
				p.synchronize()
			} else {
				panic(err)
			}
		}
	}()

	if p.match(ast.TokenFun) {
		return p.function("function")
	}
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

// statement parses statements. A statement can be a print,
// if, while, block or expression statement.
func (p *parser) statement() ast.Stmt {
	if p.match(ast.TokenPrint) {
		return p.printStatement()
	}
	if p.match(ast.TokenLeftBrace) {
		return ast.BlockStmt{Statements: p.block()}
	}
	if p.match(ast.TokenIf) {
		return p.ifStatement()
	}
	if p.match(ast.TokenWhile) {
		p.loop++
		defer func() { p.loop-- }()
		return p.whileStatement()
	}
	if p.match(ast.TokenFor) {
		p.loop++
		defer func() { p.loop-- }()
		return p.forStatement()
	}
	if p.match(ast.TokenBreak) {
		if p.loop == 0 {
			panic(p.error(p.previous(), "Break outside loop"))
		}
		p.consume(ast.TokenSemicolon, "Expect ';' after break")
		return ast.BreakStmt{}
	}
	if p.match(ast.TokenContinue) {
		if p.loop == 0 {
			panic(p.error(p.previous(), "Continue outside loop"))
		}
		p.consume(ast.TokenSemicolon, "Expect ';' after continue")
		return ast.ContinueStmt{}
	}
	if p.match(ast.TokenReturn) {
		return p.returnStatement()
	}
	return p.expressionStatement()
}

func (p *parser) forStatement() ast.Stmt {
	p.consume(ast.TokenLeftParen, "Expect '(' after 'for'.")

	var initializer ast.Stmt
	if p.match(ast.TokenSemicolon) {
		initializer = nil
	} else if p.match(ast.TokenVar) {
		initializer = p.varDeclaration()
	} else {
		initializer = p.expressionStatement()
	}

	var condition ast.Expr
	if !p.check(ast.TokenSemicolon) {
		condition = p.expression()
	}
	p.consume(ast.TokenSemicolon, "Expect ';' after look condition.")

	var increment ast.Expr
	if !p.check(ast.TokenRightParen) {
		increment = p.expression()
	}
	p.consume(ast.TokenRightParen, "Expect ')' after for clauses.")
	body := p.statement()

	if increment != nil {
		body = ast.BlockStmt{Statements: []ast.Stmt{body, ast.ExpressionStmt{Expr: increment}}}
	}

	if condition == nil {
		condition = ast.LiteralExpr{Value: true}
	}
	body = ast.WhileStmt{Body: body, Condition: condition}

	if initializer != nil {
		body = ast.BlockStmt{Statements: []ast.Stmt{initializer, body}}
	}

	return body
}

func (p *parser) printStatement() ast.Stmt {
	expr := p.expression()
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.PrintStmt{Expr: expr}
}

func (p *parser) returnStatement() ast.Stmt {
	keyword := p.previous()
	var value ast.Expr
	if !p.check(ast.TokenSemicolon) {
		value = p.expression()
	}
	p.consume(ast.TokenSemicolon, "Expect ';' after return value.")
	return ast.ReturnStmt{Keyword: keyword, Value: value}
}

func (p *parser) block() []ast.Stmt {
	var statements []ast.Stmt
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	p.consume(ast.TokenRightBrace, "Expect '}' after block.")
	return statements
}

func (p *parser) ifStatement() ast.Stmt {
	p.consume(ast.TokenLeftParen, "Expect '(' after 'if'.")
	condition := p.expression()
	p.consume(ast.TokenRightParen, "Expect ')' after if condition.")

	thenBranch := p.statement()
	var elseBranch ast.Stmt
	if p.match(ast.TokenElse) {
		elseBranch = p.statement()
	}

	return ast.IfStmt{Condition: condition, ThenBranch: thenBranch, ElseBranch: elseBranch}
}

func (p *parser) whileStatement() ast.Stmt {
	p.consume(ast.TokenLeftParen, "Expect '(' after 'while'.")
	condition := p.expression()
	p.consume(ast.TokenRightParen, "Expect ')' after while condition.")
	body := p.statement()
	return ast.WhileStmt{Condition: condition, Body: body}
}

// expressionStatement parses expression statements
func (p *parser) expressionStatement() ast.Stmt {
	// parse the next expression
	expr := p.expression()
	// panic if the next token is not a semicolon
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.ExpressionStmt{Expr: expr}
}

func (p *parser) function(kind string) ast.Stmt {
	name := p.consume(ast.TokenIdentifier, "Expect "+kind+" name.")
	p.consume(ast.TokenLeftParen, "Expect '(' after "+kind+" name.")

	parameters := make([]ast.Token, 0)
	if !p.check(ast.TokenRightParen) {
		for {
			if len(parameters) >= 255 {
				p.error(p.peek(), "Can't have more than 255 parameters.")
			}
			parameters = append(parameters, p.consume(ast.TokenIdentifier, "Expect parameter name."))
			if !p.match(ast.TokenComma) {
				break
			}
		}
	}

	p.consume(ast.TokenRightParen, "Expect ')' after parameters")
	p.consume(ast.TokenLeftBrace, "Expect '{' before "+kind+" body.")

	body := p.block()
	return ast.FunctionStmt{Name: name, Params: parameters, Body: body}
}

func (p *parser) expression() ast.Expr {
	return p.series()
}

func (p *parser) series() ast.Expr {
	expr := p.assignment()
	for p.match(ast.TokenComma) {
		operator := p.previous()
		right := p.assignment()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *parser) assignment() ast.Expr {
	expr := p.ternary()
	if p.match(ast.TokenEqual) {
		equals := p.previous()
		if varExpr, ok := expr.(ast.VariableExpr); ok {
			value := p.assignment()
			return ast.AssignExpr{Name: varExpr.Name, Value: value}
		}
		_ = p.error(equals, "Invalid assignment target.")
	}

	return expr
}

func (p *parser) ternary() ast.Expr {
	expr := p.or()

	if p.match(ast.TokenQuestionMark) {
		cond1 := p.ternary()
		p.consume(ast.TokenColon, "Expect ':' after conditional.")
		cond2 := p.ternary()
		expr = ast.TernaryExpr{Cond: expr, Left: cond1, Right: cond2}
	}

	return expr
}

func (p *parser) or() ast.Expr {
	expr := p.and()
	for p.match(ast.TokenOr) {
		operator := p.previous()
		right := p.and()
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
	}
	return expr
}

func (p *parser) and() ast.Expr {
	expr := p.equality()
	for p.match(ast.TokenAnd) {
		operator := p.previous()
		right := p.equality()
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
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

	return p.call()
}

func (p *parser) call() ast.Expr {
	expr := p.primary()
	for {
		if p.match(ast.TokenLeftParen) {
			expr = p.finishCall(expr)
		} else {
			break
		}
	}
	return expr
}

func (p *parser) finishCall(callee ast.Expr) ast.Expr {
	args := make([]ast.Expr, 0)
	if !p.check(ast.TokenRightParen) {
		for {
			if len(args) >= 255 {
				p.error(p.peek(), "Can't have more than 255 arguments.")
			}
			args = append(args, p.assignment()) // Didn't use p.expression() because an expression can be a series!
			if !p.match(ast.TokenComma) {
				break
			}
		}
	}
	paren := p.consume(ast.TokenRightParen, "Expect ')' after arguments.")
	return ast.CallExpr{Callee: callee, Paren: paren, Arguments: args}
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
		expr := p.expression()
		p.consume(ast.TokenRightParen, "Expect ')' after expression.")
		return ast.GroupingExpr{Expression: expr}
	case p.match(ast.TokenIdentifier):
		return ast.VariableExpr{Name: p.previous()}
	}

	panic(p.error(p.peek(), "Expect expression."))
}

// consume checks that the next ast.Token is of the given ast.TokenType and then
// advances to the next token. If the check fails, it panics with the given message.
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
		case ast.TokenClass, ast.TokenFor, ast.TokenFun, ast.TokenIf,
			ast.TokenPrint, ast.TokenReturn, ast.TokenVar, ast.TokenWhile,
			ast.TokenBreak, ast.TokenContinue:
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
