package parser

import (
	"testing"

	"github.com/tarek-elmasri/compiler/ast"
	"github.com/tarek-elmasri/compiler/lexer"
)

func checkForErrors(t *testing.T, p *Parser) {
	if len(p.errors) != 0 {
		t.Error(p.Errors())
	}
}

func TestIntLiteralExpression(t *testing.T) {
	l := lexer.New("55\r\n")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	tests := struct {
		tl    string
		value int64
	}{"55", 55}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Errorf("expected statemenst type of ExpressetionStatement. recieved: %T", program.Statements[0])
	}

	intLet, ok := stmt.Expression.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected statemenst type of IntLiteralExpression. recieved: %T", stmt.Expression)
	}

	if tests.tl != intLet.TokenLiteral() {
		t.Errorf("expected TokenLiteral to be %s. recieved: %s", tests.tl, intLet.TokenLiteral())
	}

	if intLet.Value != tests.value {
		t.Errorf("expected value to be %d. recieved: %d", tests.value, intLet.Value)
	}
}

func TestParseIdentifier(t *testing.T) {
	l := lexer.New("abc\r\n")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	tests := struct {
		tl    string
		value string
	}{"abc", "abc"}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Errorf("expected statemenst type of ExpressetionStatement. recieved: %T", program.Statements[0])
	}

	ident, ok := stmt.Expression.(*ast.Identifier)
	if !ok {
		t.Errorf("expected statemenst type of Identifier. recieved: %T", program.Statements[0])
	}

	if tests.tl != ident.TokenLiteral() {
		t.Errorf("expected TokenLiteral to be %s. recieved: %s", tests.tl, ident.TokenLiteral())
	}

	if ident.Value != tests.value {
		t.Errorf("expected value to be %s. recieved: %s", tests.value, ident.Value)
	}
}
