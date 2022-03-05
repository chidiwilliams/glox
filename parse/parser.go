package parse

import (
	"fmt"
	"io"

	"github.com/chidiwilliams/glox/ast"
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
	tokens   []ast.Token
	current  int
	loop     int
	hadError bool
	stdErr   io.Writer
}

// NewParser returns a new Parser that reads a list of tokens
func NewParser(tokens []ast.Token, stdErr io.Writer) *Parser {
	return &Parser{tokens: tokens, stdErr: stdErr}
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
func (p *Parser) Parse() ([]ast.Stmt, bool) {
	var statements []ast.Stmt
	for !p.isAtEnd() {
		stmt := p.declaration()
		statements = append(statements, stmt)
	}
	return statements, p.hadError
}

// declaration parses declaration statements. A declaration statement is
// a variable declaration or a regular statement. If the statement contains
// a parse error, it skips to the start of the next statement and returns nil.
func (p *Parser) declaration() ast.Stmt {
	defer func() {
		if err := recover(); err != nil {
			// If the error is a parseError, synchronize to
			// the next statement. If not, propagate the panic.
			if _, ok := err.(parseError); ok {
				p.hadError = true
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

func (p *Parser) classDeclaration() ast.Stmt {
	name := p.consume(ast.TokenIdentifier, "Expect class name.")

	var superclass *ast.VariableExpr
	if p.match(ast.TokenLess) {
		p.consume(ast.TokenIdentifier, "Expect superclass name.")
		superclass = &ast.VariableExpr{Name: p.previous()}
	}

	p.consume(ast.TokenLeftBrace, "Expect '{' before class body.")

	methods := make([]ast.FunctionStmt, 0)
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		fn := p.function("method")
		methods = append(methods, fn)
	}

	p.consume(ast.TokenRightBrace, "Expect '}' after class body.")
	return ast.ClassStmt{Name: name, Methods: methods, Superclass: superclass}
}

func (p *Parser) varDeclaration() ast.Stmt {
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
func (p *Parser) statement() ast.Stmt {
	if p.match(ast.TokenPrint) {
		return p.printStatement()
	}
	if p.match(ast.TokenLeftBrace) {
		stmt := p.block()
		return ast.BlockStmt{Statements: stmt}
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
			p.error(p.previous(), "Break outside loop")
		}
		p.consume(ast.TokenSemicolon, "Expect ';' after break")
		return ast.BreakStmt{}
	}
	if p.match(ast.TokenContinue) {
		if p.loop == 0 {
			p.error(p.previous(), "Continue outside loop")
		}
		p.consume(ast.TokenSemicolon, "Expect ';' after continue")
		return ast.ContinueStmt{}
	}
	if p.match(ast.TokenReturn) {
		return p.returnStatement()
	}
	return p.expressionStatement()
}

func (p *Parser) forStatement() ast.Stmt {
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

func (p *Parser) printStatement() ast.Stmt {
	expr := p.expression()
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.PrintStmt{Expr: expr}
}

func (p *Parser) returnStatement() ast.Stmt {
	keyword := p.previous()
	var value ast.Expr
	if !p.check(ast.TokenSemicolon) {
		value = p.expression()
	}
	p.consume(ast.TokenSemicolon, "Expect ';' after return value.")
	return ast.ReturnStmt{Keyword: keyword, Value: value}
}

func (p *Parser) block() []ast.Stmt {
	var statements []ast.Stmt
	for !p.check(ast.TokenRightBrace) && !p.isAtEnd() {
		stmt := p.declaration()
		statements = append(statements, stmt)
	}
	p.consume(ast.TokenRightBrace, "Expect '}' after block.")
	return statements
}

func (p *Parser) ifStatement() ast.Stmt {
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

func (p *Parser) whileStatement() ast.Stmt {
	p.consume(ast.TokenLeftParen, "Expect '(' after 'while'.")
	condition := p.expression()
	p.consume(ast.TokenRightParen, "Expect ')' after while condition.")
	body := p.statement()
	return ast.WhileStmt{Condition: condition, Body: body}
}

// expressionStatement parses expression statements
func (p *Parser) expressionStatement() ast.Stmt {
	// parse the next expression
	expr := p.expression()
	// panic if the next token is not a semicolon
	p.consume(ast.TokenSemicolon, "Expect ';' after value")
	return ast.ExpressionStmt{Expr: expr}
}

func (p *Parser) function(kind string) ast.FunctionStmt {
	name := p.consume(ast.TokenIdentifier, "Expect "+kind+" name.")

	var parameters []ast.Token

	if kind != "method" || p.check(ast.TokenLeftParen) {
		parameters = make([]ast.Token, 0)
		p.consume(ast.TokenLeftParen, "Expect '(' after "+kind+" name.")

		if !p.check(ast.TokenRightParen) {
			for {
				if len(parameters) >= 255 {
					p.error(p.peek(), "Can't have more than 255 parameters.")
				}
				param := p.consume(ast.TokenIdentifier, "Expect parameter name.")
				parameters = append(parameters, param)
				if !p.match(ast.TokenComma) {
					break
				}
			}
		}

		p.consume(ast.TokenRightParen, "Expect ')' after parameters")
	}

	p.consume(ast.TokenLeftBrace, "Expect '{' before "+kind+" body.")

	body := p.block()
	return ast.FunctionStmt{Name: name, Params: parameters, Body: body}
}

func (p *Parser) expression() ast.Expr {
	return p.series()
}

func (p *Parser) series() ast.Expr {
	expr := p.assignment()

	for p.match(ast.TokenComma) {
		operator := p.previous()
		right := p.assignment()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *Parser) assignment() ast.Expr {
	expr := p.ternary()

	if p.match(ast.TokenEqual) {
		equals := p.previous()
		value := p.assignment()

		if varExpr, ok := expr.(ast.VariableExpr); ok {
			return ast.AssignExpr{Name: varExpr.Name, Value: value}
		} else if getExpr, ok := expr.(ast.GetExpr); ok {
			return ast.SetExpr{
				Object: getExpr.Object,
				Name:   getExpr.Name,
				Value:  value,
			}
		}
		p.error(equals, "Invalid assignment target.")
	}

	return expr
}

func (p *Parser) ternary() ast.Expr {
	expr := p.or()

	if p.match(ast.TokenQuestionMark) {
		cond1 := p.ternary()
		p.consume(ast.TokenColon, "Expect ':' after conditional.")
		cond2 := p.ternary()
		expr = ast.TernaryExpr{Cond: expr, Left: cond1, Right: cond2}
	}

	return expr
}

func (p *Parser) or() ast.Expr {
	expr := p.and()

	for p.match(ast.TokenOr) {
		operator := p.previous()
		right := p.and()
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
	}
	return expr
}

func (p *Parser) and() ast.Expr {
	expr := p.equality()

	for p.match(ast.TokenAnd) {
		operator := p.previous()
		right := p.equality()
		expr = ast.LogicalExpr{Left: expr, Operator: operator, Right: right}
	}
	return expr
}

func (p *Parser) equality() ast.Expr {
	expr := p.comparison()

	for p.match(ast.TokenBangEqual, ast.TokenEqualEqual) {
		operator := p.previous()
		right := p.comparison()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *Parser) comparison() ast.Expr {
	expr := p.term()

	for p.match(ast.TokenGreater, ast.TokenGreaterEqual, ast.TokenLess, ast.TokenLessEqual) {
		operator := p.previous()
		right := p.term()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *Parser) term() ast.Expr {
	expr := p.factor()

	for p.match(ast.TokenMinus, ast.TokenPlus) {
		operator := p.previous()
		right := p.factor()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *Parser) factor() ast.Expr {
	expr := p.unary()

	for p.match(ast.TokenSlash, ast.TokenStar) {
		operator := p.previous()
		right := p.unary()
		expr = ast.BinaryExpr{Left: expr, Operator: operator, Right: right}
	}

	return expr
}

func (p *Parser) unary() ast.Expr {
	if p.match(ast.TokenBang, ast.TokenMinus) {
		operator := p.previous()
		right := p.unary()
		return ast.UnaryExpr{Operator: operator, Right: right}
	}

	return p.call()
}

func (p *Parser) call() ast.Expr {
	expr := p.primary()

	for {
		if p.match(ast.TokenLeftParen) {
			expr = p.finishCall(expr)
		} else if p.match(ast.TokenDot) {
			name := p.consume(ast.TokenIdentifier, "Expect property name after '.'.")
			expr = ast.GetExpr{Object: expr, Name: name}
		} else {
			break
		}
	}

	return expr
}

func (p *Parser) finishCall(callee ast.Expr) ast.Expr {
	args := make([]ast.Expr, 0)
	if !p.check(ast.TokenRightParen) {
		for {
			if len(args) >= 255 {
				p.error(p.peek(), "Can't have more than 255 arguments.")
			}
			expr := p.assignment()
			args = append(args, expr) // Didn't use p.expression() because an expression can be a series!
			if !p.match(ast.TokenComma) {
				break
			}
		}
	}
	paren := p.consume(ast.TokenRightParen, "Expect ')' after arguments.")
	return ast.CallExpr{Callee: callee, Paren: paren, Arguments: args}
}

func (p *Parser) primary() ast.Expr {
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
	case p.match(ast.TokenFun):
		return p.functionExpression()
	case p.match(ast.TokenThis):
		return ast.ThisExpr{Keyword: p.previous()}
	case p.match(ast.TokenSuper):
		keyword := p.previous()
		p.consume(ast.TokenDot, "Expect '.' after 'super'.")
		method := p.consume(ast.TokenIdentifier, "Expect superclass method name.")
		return ast.SuperExpr{Keyword: keyword, Method: method}
	}

	p.error(p.peek(), "Expect expression.")
	return nil
}

// functionExpression parses a function expression.
// A function expression may be a named or anonymous function.
func (p *Parser) functionExpression() ast.Expr {
	var name *ast.Token
	if !p.check(ast.TokenLeftParen) {
		fnName := p.consume(ast.TokenIdentifier, "Expect function name.")
		name = &fnName
	}

	p.consume(ast.TokenLeftParen, "Expect '(' after function name.")

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
	p.consume(ast.TokenLeftBrace, "Expect '{' before function body.")

	body := p.block()
	return ast.FunctionExpr{Name: name, Params: parameters, Body: body}
}

// consume checks that the next ast.Token is of the given ast.TokenType and then
// advances to the next token. If the check fails, it panics with the given message.
func (p *Parser) consume(tokenType ast.TokenType, message string) ast.Token {
	if p.check(tokenType) {
		return p.advance()
	}
	p.error(p.peek(), message)
	return ast.Token{}
}

func (p *Parser) error(token ast.Token, message string) {
	err := tokenError(token, message)
	_, _ = p.stdErr.Write([]byte(err.(parseError).msg))
	panic(err)
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

func tokenError(token ast.Token, message string) error {
	var where string
	if token.TokenType == ast.TokenEof {
		where = " at end"
	} else {
		where = " at '" + token.Lexeme + "'"
	}

	return parseError{msg: fmt.Sprintf("[line %d] Error%s: %s\n", token.Line, where, message)}
}
