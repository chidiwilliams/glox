package typechecker

import (
	"os"
	"path/filepath"
	"strconv"
	"testing"

	"github.com/chidiwilliams/glox/ast"
	"github.com/chidiwilliams/glox/interpret"
	"github.com/chidiwilliams/glox/parse"
	"github.com/chidiwilliams/glox/resolve"
	"github.com/chidiwilliams/glox/scan"
)

func TestTypeChecker_CheckStmts(t *testing.T) {
	paths, err := filepath.Glob(filepath.Join("testdata", "*.input"))
	if err != nil {
		t.Fatal(err)
	}

	for _, path := range paths {
		_, filename := filepath.Split(path)
		testName := filename[:len(filename)-len(filepath.Ext(path))]

		t.Run(testName, func(t *testing.T) {
			source, err := os.ReadFile(path)
			if err != nil {
				t.Fatal("error reading test source file:", err)
			}

			statements, interpreter := runSource(t, string(source))

			typeChecker := NewTypeChecker(interpreter)
			typeErr := typeChecker.CheckStmts(statements)

			goldenFile := filepath.Join("testdata", testName+".golden")
			want, err := os.ReadFile(goldenFile)
			if err != nil {
				t.Fatal("error reading golden file", err)
			}
			wantErr := string(want)

			if typeErr != nil && wantErr == "" ||
				typeErr != nil && typeErr.Error()+"\n" != wantErr ||
				typeErr == nil && wantErr != "" {
				t.Errorf("Check() error = %v, want %v", strconv.Quote(typeErr.Error()), strconv.Quote(wantErr))
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
