package typechecker

import (
	"errors"
	"os"
	"testing"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/interpret"
	"github.com/chidiwilliams/glox/parse"
	"github.com/chidiwilliams/glox/resolve"
	"github.com/chidiwilliams/glox/scan"
)

func TestTypeChecker_check(t *testing.T) {
	type args struct {
		expr ast.Expr
	}
	tests := []struct {
		name string
		args args
		want Type
		err  error
	}{
		{
			"",
			args{expr: ast.LiteralExpr{Value: float64(3)}},
			TypeNumber,
			nil,
		},
		{"", args{expr: ast.LiteralExpr{Value: "hello world"}}, TypeString, nil},
		{"", args{
			expr: ast.BinaryExpr{
				Left:     ast.LiteralExpr{Value: float64(3)},
				Operator: ast.Token{Lexeme: "*"},
				Right:    ast.LiteralExpr{Value: float64(9)}},
		}, TypeNumber, nil},
		{"", args{
			expr: ast.BinaryExpr{
				Left:     ast.LiteralExpr{Value: "hello"},
				Operator: ast.Token{Lexeme: "+"},
				Right:    ast.LiteralExpr{Value: "world"},
			},
		}, TypeString, nil},
		{"", args{
			expr: ast.BinaryExpr{
				Left:     ast.LiteralExpr{Value: "hello"},
				Operator: ast.Token{Lexeme: "-"},
				Right:    ast.LiteralExpr{Value: "world"},
			},
		}, 0, errors.New("unexpected type: 1 in {{hello} 0 - %!s(<nil>) {world}}, allowed: [0]")},
		{
			"variable declaration",
			args{
				expr: ast.AssignExpr{
					Name:  ast.Token{Lexeme: "x"},
					Value: ast.LiteralExpr{Value: float64(10)},
				},
			},
			TypeNumber,
			nil,
		},
		{
			"variable access",
			args{
				expr: ast.VariableExpr{
					Name: ast.Token{Lexeme: "x"},
				},
			},
			TypeNumber,
			nil,
		},
	}

	c := NewTypeChecker(nil)
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exprType, err := c.Check(tt.args.expr)
			if err != nil {
				if tt.err == nil {
					t.Errorf("Check() error = %v, want %v", err, tt.err)
				} else {
					if err.Error() != tt.err.Error() {
						t.Errorf("Check() error = %v, want %v", err, tt.err)
					}
				}
			}
			if exprType != tt.want {
				t.Errorf("Check() exprType = %v, want %v", exprType, tt.want)
			}
		})
	}
}

func TestTypeChecker_CheckStmt(t *testing.T) {
	type args struct {
		stmt   ast.Stmt
		source string
	}
	tests := []struct {
		name string
		args args
		err  error
	}{
		{
			"",
			args{
				source: `
{
	var x = 10;
	var y = 20;
	var z: string = x * 10 + y;
	z;
}`,
			},
			errors.New("expected '1' type for {{{21 x %!s(<nil>)} 10 * %!s(<nil>) {%!s(float64=10)}} 7 + %!s(<nil>) {21 y %!s(<nil>)}} in {{{21 x %!s(<nil>)} 10 * %!s(<nil>) {%!s(float64=10)}} 7 + %!s(<nil>) {21 y %!s(<nil>)}}, but got 0"),
		},
		{
			"",
			args{
				source: `
{
	var x = 10;
	{
		var x: string = "";
		x + "hello";
	}
	x - 20;
}`,
			},
			nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {

			statements, interpreter := runSource(t, tt.args.source)

			c := NewTypeChecker(interpreter)
			err := c.CheckStmts(statements)
			if err != nil {
				if tt.err == nil {
					t.Errorf("Check() error = %v, want %v", err, tt.err)
				} else {
					if err.Error() != tt.err.Error() {
						t.Errorf("Check() error = %v, want %v", err, tt.err)
					}
				}
			}
		})
	}
}

func runSource(t *testing.T, source string) ([]ast.Stmt, *interpret.Interpreter) {
	scanner := scan.NewScanner(source, os.Stderr)
	tokens := scanner.ScanTokens()

	parser := parse.NewParser(tokens, os.Stderr)
	var statements []ast.Stmt
	statements, hadError := parser.Parse()
	if hadError {
		t.Fatal("parsing error")
	}

	interpreter := interpret.NewInterpreter(os.Stdout, os.Stderr)

	resolver := resolve.NewResolver(interpreter, os.Stderr)
	hadError = resolver.ResolveStmts(statements)

	if hadError {
		t.Fatal("resolving error")
	}

	return statements, interpreter
}
