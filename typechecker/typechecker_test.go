package typechecker

import (
	"errors"
	"testing"

	"github.com/chidiwilliams/glox/ast"
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

	c := NewTypeChecker()
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
