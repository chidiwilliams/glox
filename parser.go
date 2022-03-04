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

// Parser parses a flat list of tokens into
// an AST representation of the source program
type Parser struct {
	tokens  []ast.Token
	current int
	loop    int
}

// NewParser returns a new Parser that reads a list of tokens
func NewParser(tokens []ast.Token) *Parser {
	return &Parser{tokens: tokens}
}

/**
Parser grammar:

	program      => declaration* EOF
	declaration  => classDecl | funcDecl | varDecl | statement
	classDecl    => "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}"
	funDecl      => "fun" function
	function     => IDENTIFIER "(" parameters? ")" block
	parameters   => IDENTIFIER ( "," IDENTIFIER )*
	varDecl      => "var" IDENTIFIER ( "=" expression )? ";"
	statement    => exprStmt | ifStmt | forStmt | printStmt | returnStmt | whileStmt
									| breakStmt | continueStmt | block
	exprStmt     => expression ";"
	ifStmt       => "if" "(" expression ")" statement ( "else" statement )?
	forStmt      => "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
	printStmt    => "print" expression ";"
	returnStmt   => "return" expression? ";"
	whileStmt    => "while" "(" expression ")" statement
	block        => "{" declaration* "}" ;
	expression   => series
	series       => assignment ( "," assignment )*
	assignment   => ( call "." )? IDENTIFIER "=" assignment | ternary
	ternary      => logic_or ( "?" ternary ":" ternary )*
	logic_or     => logic_and ( "or" logic_and )*
	logic_and    => equality ( "and" equality )*
	equality     => comparison ( ( "!=" | "==" ) comparison )
	comparison   => term ( ( ">" | ">=" | "<" | "<=" ) term )*
	term         => factor ( ( "+" | "-" ) factor )*
	factor       => unary ( ( "/" | "*" ) unary )*
	unary        => ( "!" | "-" ) unary | call
	call         => primary ( "(" arguments? ")" | "." IDENTIFIER )*
	arguments    => expression ( "," expression )*
	primary      => NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
									| IDENTIFIER | functionExpr | "super" . IDENTIFIER
	functionExpr => "fun" IDENTIFIER? "(" parameters? ")" block

	Reference: C Operator Precedence https://en.cppreference.com/w/c/language/operator_precedence

*/

// Parse reads the list of tokens and returns a
// list of statements representing the source program
func (p *Parser) Parse() []ast.Stmt {
	var statements []ast.Stmt
	for !p.isAtEnd() {
		stmt, err := p.declaration()
		if err == nil {
			statements = append(statements, stmt)
		} else {
			if _, ok := err.(parseError); ok {
				p.synchronize()
			} else {
				panic(err)
			}
		}
	}
	return statements
}

// declaration parses declaration statements. A declaration statement is
// a variable declaration or a regular statement. If the statement contains
// a parse error, it skips to the start of the next statement and returns nil.
func (p *Parser) declaration() (ast.Stmt, error) {
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

	if p.match(ast.TokenClass) {
		return p.classDeclaration()
	}
	if p.match(ast.TokenFun) {
		return p.function("function")
	}
	if p.match(ast.TokenVar) {
		return p.varDeclaration()
	}
	return p.statement()
}

func (p *Parser) classDeclaration() (ast.Stmt, error) {
	name, err := p.consume(ast.TokenIdentifier, "Expect class name.")
	if err != nil {
		return nil, err
	}

	var superclass *ast.VariableExpr
	if p.match(ast.TokenLess) {
		if _, err = p.consume(ast.TokenIdentifier, "Expect superclass name."); err != nil {
			return nil, err
		}
		superclass = &ast.VariableExpr{Name: p.previous()}
	}

	if _, err = p.consume(ast.TokenLeftBrace, "Expect '{' before class body."); err != nil {
		return nil, err
	}

	methods := make([]ast.FunctionStmt, 0)
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		fn, err := p.function("method")
		if err != nil {
			return nil, err
		}
		methods = append(methods, fn)
	}

	if _, err = p.consume(ast.TokenRightBrace, "Expect '}' after class body."); err != nil {
		return nil, err
	}
	return ast.ClassStmt{Name: name, Methods: methods, Superclass: superclass}, nil
}

func (p *Parser) varDeclaration() (ast.Stmt, error) {
	name, err := p.consume(ast.TokenIdentifier, "Expect variable name")
	if err != nil {
		return nil, err
	}
	var initializer ast.Expr
	if p.match(ast.TokenEqual) {
		initializer, err = p.expression()
		if err != nil {
			return nil, err
		}
	}
	if _, err = p.consume(ast.TokenSemicolon, "Expect ';' after variable declaration"); err != nil {
		return nil, err
	}
	return ast.VarStmt{Name: name, Initializer: initializer}, nil
}

// statement parses statements. A statement can be a print,
// if, while, block or expression statement.
func (p *Parser) statement() (ast.Stmt, error) {
	if p.match(ast.TokenPrint) {
		return p.printStatement()
	}
	if p.match(ast.TokenLeftBrace) {
		stmt, err := p.block()
		if err != nil {
			return nil, err
		}
		return ast.BlockStmt{Statements: stmt}, nil
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
			if err := p.error(p.previous(), "Break outside loop"); err != nil {
				return nil, err
			}
		}
		if _, err := p.consume(ast.TokenSemicolon, "Expect ';' after break"); err != nil {
			return nil, err
		}
		return ast.BreakStmt{}, nil
	}
	if p.match(ast.TokenContinue) {
		if p.loop == 0 {
			if err := p.error(p.previous(), "Continue outside loop"); err != nil {
				return nil, err
			}
		}
		if _, err := p.consume(ast.TokenSemicolon, "Expect ';' after continue"); err != nil {
			return nil, err
		}
		return ast.ContinueStmt{}, nil
	}
	if p.match(ast.TokenReturn) {
		return p.returnStatement()
	}
	return p.expressionStatement()
}

func (p *Parser) forStatement() (ast.Stmt, error) {
	if _, err := p.consume(ast.TokenLeftParen, "Expect '(' after 'for'."); err != nil {
		return nil, err
	}

	var initializer ast.Stmt
	var err error
	if p.match(ast.TokenSemicolon) {
		initializer = nil
	} else if p.match(ast.TokenVar) {
		initializer, err = p.varDeclaration()
		if err != nil {
			return nil, err
		}
	} else {
		initializer, err = p.expressionStatement()
		if err != nil {
			return nil, err
		}
	}

	var condition ast.Expr
	if !p.check(ast.TokenSemicolon) {
		condition, err = p.expression()
		if err != nil {
			return nil, err
		}
	}
	if _, err = p.consume(ast.TokenSemicolon, "Expect ';' after look condition."); err != nil {
		return nil, err
	}

	var increment ast.Expr
	if !p.check(ast.TokenRightParen) {
		increment, err = p.expression()
		if err != nil {
			return nil, err
		}
	}
	if _, err = p.consume(ast.TokenRightParen, "Expect ')' after for clauses."); err != nil {
		return nil, err
	}
	body, err := p.statement()
	if err != nil {
		return nil, err
	}

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

	return body, nil
}

func (p *Parser) printStatement() (ast.Stmt, error) {
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(ast.TokenSemicolon, "Expect ';' after value"); err != nil {
		return nil, err
	}
	return ast.PrintStmt{Expr: expr}, nil
}

func (p *Parser) returnStatement() (ast.Stmt, error) {
	keyword := p.previous()
	var value ast.Expr
	var err error
	if !p.check(ast.TokenSemicolon) {
		value, err = p.expression()
		if err != nil {
			return nil, err
		}
	}
	if _, err = p.consume(ast.TokenSemicolon, "Expect ';' after return value."); err != nil {
		return nil, err
	}
	return ast.ReturnStmt{Keyword: keyword, Value: value}, nil
}

func (p *Parser) block() ([]ast.Stmt, error) {
	var statements []ast.Stmt
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		stmt, err := p.declaration()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stmt)
	}
	if _, err := p.consume(ast.TokenRightBrace, "Expect '}' after block."); err != nil {
		return nil, err
	}
	return statements, nil
}

func (p *Parser) ifStatement() (ast.Stmt, error) {
	if _, err := p.consume(ast.TokenLeftParen, "Expect '(' after 'if'."); err != nil {
		return nil, err
	}
	condition, err := p.expression()
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(ast.TokenRightParen, "Expect ')' after if condition."); err != nil {
		return nil, err
	}

	thenBranch, err := p.statement()
	if err != nil {
		return nil, err
	}
	var elseBranch ast.Stmt
	if p.match(ast.TokenElse) {
		elseBranch, err = p.statement()
		if err != nil {
			return nil, err
		}
	}

	return ast.IfStmt{Condition: condition, ThenBranch: thenBranch, ElseBranch: elseBranch}, nil
}

func (p *Parser) whileStatement() (ast.Stmt, error) {
	if _, err := p.consume(ast.TokenLeftParen, "Expect '(' after 'while'."); err != nil {
		return nil, err
	}
	condition, err := p.expression()
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(ast.TokenRightParen, "Expect ')' after while condition."); err != nil {
		return nil, err
	}
	body, err := p.statement()
	if err != nil {
		return nil, err
	}
	return ast.WhileStmt{Condition: condition, Body: body}, nil
}

// expressionStatement parses expression statements
func (p *Parser) expressionStatement() (ast.Stmt, error) {
	// parse the next expression
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	// panic if the next token is not a semicolon
	if _, err = p.consume(ast.TokenSemicolon, "Expect ';' after value"); err != nil {
		return nil, err
	}
	return ast.ExpressionStmt{Expr: expr}, nil
}

func (p *Parser) function(kind string) (ast.FunctionStmt, error) {
	name, err := p.consume(ast.TokenIdentifier, "Expect "+kind+" name.")
	if err != nil {
		return ast.FunctionStmt{}, err
	}

	var parameters []ast.Token

	if kind != "method" || p.check(ast.TokenLeftParen) {
		parameters = make([]ast.Token, 0)
		if _, err = p.consume(ast.TokenLeftParen, "Expect '(' after "+kind+" name."); err != nil {
			return ast.FunctionStmt{}, err
		}

		if !p.check(ast.TokenRightParen) {
			for {
				if len(parameters) >= 255 {
					err := p.error(p.peek(), "Can't have more than 255 parameters.")
					if err != nil {
						return ast.FunctionStmt{}, err
					}
				}
				param, err := p.consume(ast.TokenIdentifier, "Expect parameter name.")
				if err != nil {
					return ast.FunctionStmt{}, err
				}
				parameters = append(parameters, param)
				if !p.match(ast.TokenComma) {
					break
				}
			}
		}

		if _, err = p.consume(ast.TokenRightParen, "Expect ')' after parameters"); err != nil {
			return ast.FunctionStmt{}, err
		}
	}

	if _, err = p.consume(ast.TokenLeftBrace, "Expect '{' before "+kind+" body."); err != nil {
		return ast.FunctionStmt{}, err
	}

	body, err := p.block()
	if err != nil {
		return ast.FunctionStmt{}, err
	}
	return ast.FunctionStmt{Name: name, Params: parameters, Body: body}, nil
}

func (p *Parser) expression() (ast.Expr, error) {
	return p.series()
}

func (p *Parser) series() (ast.Expr, error) {
	expr, err := p.assignment()
	if err != nil {
		return nil, err
	}
	for p.match(ast.TokenComma) {
		operator := p.previous()
		right, err := p.assignment()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr, nil
}

func (p *Parser) assignment() (ast.Expr, error) {
	expr, err := p.ternary()
	if err != nil {
		return nil, err
	}
	if p.match(ast.TokenEqual) {
		equals := p.previous()
		value, err := p.assignment()
		if err != nil {
			return nil, err
		}
		if varExpr, ok := expr.(ast.VariableExpr); ok {
			return ast.AssignExpr{Name: varExpr.Name, Value: value}, nil
		} else if getExpr, ok := expr.(ast.GetExpr); ok {
			return ast.SetExpr{
				Object: getExpr.Object,
				Name:   getExpr.Name,
				Value:  value,
			}, nil
		}
		return nil, p.error(equals, "Invalid assignment target.")
	}

	return expr, nil
}

func (p *Parser) ternary() (ast.Expr, error) {
	expr, err := p.or()
	if err != nil {
		return nil, err
	}

	if p.match(ast.TokenQuestionMark) {
		cond1, err := p.ternary()
		if err != nil {
			return nil, err
		}
		if _, err = p.consume(ast.TokenColon, "Expect ':' after conditional."); err != nil {
			return nil, err
		}
		cond2, err := p.ternary()
		if err != nil {
			return nil, err
		}
		expr = ast.TernaryExpr{Cond: expr, Left: cond1, Right: cond2}
	}

	return expr, nil
}

func (p *Parser) or() (ast.Expr, error) {
	expr, err := p.and()
	if err != nil {
		return nil, err
	}
	for p.match(ast.TokenOr) {
		operator := p.previous()
		right, err := p.and()
		if err != nil {
			return nil, err
		}
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
	}
	return expr, nil
}

func (p *Parser) and() (ast.Expr, error) {
	expr, err := p.equality()
	if err != nil {
		return nil, err
	}
	for p.match(ast.TokenAnd) {
		operator := p.previous()
		right, err := p.equality()
		if err != nil {
			return nil, err
		}
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
	}
	return expr, nil
}

func (p *Parser) equality() (ast.Expr, error) {
	expr, err := p.comparison()
	if err != nil {
		return nil, err
	}

	for p.match(ast.TokenBangEqual, ast.TokenEqualEqual) {
		operator := p.previous()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr, nil
}

func (p *Parser) comparison() (ast.Expr, error) {
	expr, err := p.term()
	if err != nil {
		return nil, err
	}

	for p.match(ast.TokenGreater, ast.TokenGreaterEqual, ast.TokenLess, ast.TokenLessEqual) {
		operator := p.previous()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr, nil
}

func (p *Parser) term() (ast.Expr, error) {
	expr, err := p.factor()
	if err != nil {
		return nil, err
	}

	for p.match(ast.TokenMinus, ast.TokenPlus) {
		operator := p.previous()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr, nil
}

func (p *Parser) factor() (ast.Expr, error) {
	expr, err := p.unary()
	if err != nil {
		return nil, err
	}

	for p.match(ast.TokenSlash, ast.TokenStar) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr, nil
}

func (p *Parser) unary() (ast.Expr, error) {
	if p.match(ast.TokenBang, ast.TokenMinus) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		return ast.UnaryExpr{Operator: operator, Right: right}, nil
	}

	return p.call()
}

func (p *Parser) call() (ast.Expr, error) {
	expr, err := p.primary()
	if err != nil {
		return nil, err
	}

	for {
		if p.match(ast.TokenLeftParen) {
			expr, err = p.finishCall(expr)
			if err != nil {
				return nil, err
			}
		} else if p.match(ast.TokenDot) {
			name, err := p.consume(ast.TokenIdentifier, "Expect property name after '.'.")
			if err != nil {
				return nil, err
			}
			expr = ast.GetExpr{Object: expr, Name: name}
		} else {
			break
		}
	}

	return expr, nil
}

func (p *Parser) finishCall(callee ast.Expr) (ast.Expr, error) {
	args := make([]ast.Expr, 0)
	if !p.check(ast.TokenRightParen) {
		for {
			if len(args) >= 255 {
				err := p.error(p.peek(), "Can't have more than 255 arguments.")
				if err != nil {
					return nil, err
				}
			}
			expr, err := p.assignment()
			if err != nil {
				return nil, err
			}
			args = append(args, expr) // Didn't use p.expression() because an expression can be a series!
			if !p.match(ast.TokenComma) {
				break
			}
		}
	}
	paren, err := p.consume(ast.TokenRightParen, "Expect ')' after arguments.")
	if err != nil {
		return nil, err
	}
	return ast.CallExpr{Callee: callee, Paren: paren, Arguments: args}, nil
}

func (p *Parser) primary() (ast.Expr, error) {
	switch {
	case p.match(ast.TokenFalse):
		return ast.LiteralExpr{Value: false}, nil
	case p.match(ast.TokenTrue):
		return ast.LiteralExpr{Value: true}, nil
	case p.match(ast.TokenNil):
		return ast.LiteralExpr{}, nil
	case p.match(ast.TokenNumber, ast.TokenString):
		return ast.LiteralExpr{Value: p.previous().Literal}, nil
	case p.match(ast.TokenLeftParen):
		expr, err := p.expression()
		if err != nil {
			return nil, err
		}
		if _, err = p.consume(ast.TokenRightParen, "Expect ')' after expression."); err != nil {
			return nil, err
		}
		return ast.GroupingExpr{Expression: expr}, nil
	case p.match(ast.TokenIdentifier):
		return ast.VariableExpr{Name: p.previous()}, nil
	case p.match(ast.TokenFun):
		return p.functionExpression()
	case p.match(ast.TokenThis):
		return ast.ThisExpr{Keyword: p.previous()}, nil
	case p.match(ast.TokenSuper):
		keyword := p.previous()
		if _, err := p.consume(ast.TokenDot, "Expect '.' after 'super'."); err != nil {
			return nil, err
		}
		method, err := p.consume(ast.TokenIdentifier, "Expect superclass method name.")
		if err != nil {
			return nil, err
		}
		return ast.SuperExpr{Keyword: keyword, Method: method}, nil
	}

	return nil, p.error(p.peek(), "Expect expression.")
}

// functionExpression parses a function expression.
// A function expression may be a named or anonymous function.
func (p *Parser) functionExpression() (ast.Expr, error) {
	var name *ast.Token
	if !p.check(ast.TokenLeftParen) {
		fnName, err := p.consume(ast.TokenIdentifier, "Expect function name.")
		if err != nil {
			return nil, err
		}
		name = &fnName
	}

	if _, err := p.consume(ast.TokenLeftParen, "Expect '(' after function name."); err != nil {
		return nil, err
	}

	parameters := make([]ast.Token, 0)
	if !p.check(ast.TokenRightParen) {
		for {
			if len(parameters) >= 255 {
				err := p.error(p.peek(), "Can't have more than 255 parameters.")
				if err != nil {
					return nil, err
				}
			}
			param, err := p.consume(ast.TokenIdentifier, "Expect parameter name.")
			if err != nil {
				return nil, err
			}
			parameters = append(parameters, param)
			if !p.match(ast.TokenComma) {
				break
			}
		}
	}

	if _, err := p.consume(ast.TokenRightParen, "Expect ')' after parameters"); err != nil {
		return nil, err
	}
	if _, err := p.consume(ast.TokenLeftBrace, "Expect '{' before function body."); err != nil {
		return nil, err
	}

	body, err := p.block()
	if err != nil {
		return nil, err
	}

	return ast.FunctionExpr{Name: name, Params: parameters, Body: body}, nil
}

// consume checks that the next ast.Token is of the given ast.TokenType and then
// advances to the next token. If the check fails, it panics with the given message.
func (p *Parser) consume(tokenType ast.TokenType, message string) (ast.Token, error) {
	if p.check(tokenType) {
		return p.advance(), nil
	}
	return ast.Token{}, p.error(p.peek(), message)
}

func (p *Parser) error(Token ast.Token, message string) error {
	reportTokenErr(nil, Token, message)
	return parseError{}
}

func (p *Parser) synchronize() {
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

func (p *Parser) match(types ...ast.TokenType) bool {
	for _, tokenType := range types {
		if p.check(tokenType) {
			p.advance()
			return true
		}
	}

	return false
}

func (p *Parser) check(tokenType ast.TokenType) bool {
	if p.isAtEnd() {
		return false
	}

	return p.peek().TokenType == tokenType
}

func (p *Parser) advance() ast.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) isAtEnd() bool {
	return p.peek().TokenType == ast.TokenEof
}

func (p *Parser) peek() ast.Token {
	return p.tokens[p.current]
}

func (p *Parser) previous() ast.Token {
	return p.tokens[p.current-1]
}
