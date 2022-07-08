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
			errors.New("expected 'string' type for {{{25 x %!s(<nil>)} 14 * %!s(<nil>) {%!s(float64=10)}} 11 + %!s(<nil>) {25 y %!s(<nil>)}} in {{{25 x %!s(<nil>)} 14 * %!s(<nil>) {%!s(float64=10)}} 11 + %!s(<nil>) {25 y %!s(<nil>)}}, but got 'number'"),
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
		{
			"should type-check a variable re-assignment",
			args{
				source: `
var x: number = 20;
x = "hello";
`,
			},
			errors.New("expected 'number' type for {hello} in {25 x %!s(<nil>) {hello}}, but got 'string'"),
		},
		{
			"boolean type",
			args{
				source: `
var x: bool = 30 > 2;`,
			},
			nil,
		},
		{
			"should type-check a ternary expression",
			args{
				source: `
var x: number = true ? 2 : 9;
var y: string = 2 < 10 ? "hello" : "world";
`,
			},
			nil,
		},
		{
			"check while condition",
			args{
				source: `
var x = 5;
while (x >= "hello") {
	x = x - 1;
}`,
			},
			errors.New("expected 'number' type for {hello} in {{25 x %!s(<nil>)} 22 >= %!s(<nil>) {hello}}, but got 'string'"),
		},
		{
			"check function body",
			args{
				source: `
var fn = fun (firstName: string, lastName: string): string {
	return firstName - lastName;
};
fn;`,
			},
			errors.New("unexpected type: string in {{25 firstName %!s(<nil>)} 10 - %!s(<nil>) {25 lastName %!s(<nil>)}}, allowed: [number]"),
		},
		{
			"check function return type",
			args{
				source: `
var fn = fun (firstName: string, lastName: string): number {
	return firstName + lastName;
};
fn;`,
			},
			errors.New("expected enclosing function to return 'number', but got 'string'"),
		},
		{
			"check function inferred type",
			args{
				source: `
var fn: Fn<[string, string], string> = fun (firstName: string, lastName: string): string {
	return firstName + lastName;
};
fn;`,
			},
			nil,
		},
		{
			"check function statement",
			args{
				source: `
fun getName(): string {
	return "name";
}

var fn: Fn<[], string> = getName;
`,
			},
			nil,
		},
		{
			"check function with different return types",
			args{
				source: `
fun getName(): number {
	if (true) return 0;
	return "name";
}
`,
			},
			errors.New("expected enclosing function to return 'number', but got 'string'"),
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
			} else {
				if tt.err != nil {
					t.Errorf("Check() error = %v, want %v", err, tt.err)
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
