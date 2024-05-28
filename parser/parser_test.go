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

func TestParseVarDeclarationStatement(t *testing.T) {
	l := lexer.New("let a = 12")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	tests := struct {
		tl    string
		name  string
		value int64
	}{"let", "a", 12}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.VarDeclarationStatement)
	if !ok {
		t.Errorf("expected statemenst type of VarDeclarationStatement. recieved: %T", program.Statements[0])
	}

	if tests.tl != stmt.TokenLiteral() {
		t.Errorf("expected TokenLiteral to be %s. recieved: %s", tests.tl, stmt.TokenLiteral())
	}

	if stmt.Name.String() != tests.name {
		t.Errorf("expected statemenst identifier to be %s. recieved: %s", tests.name, stmt.Name.String())
	}

	intLet, ok := stmt.Value.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected statemenst type of IntLiteralExpression. recieved: %T", stmt.Value)
	}

	if intLet.Value != tests.value {
		t.Errorf("expected value to be %d. recieved: %d", tests.value, intLet.Value)
	}

	test := []struct {
		input    string
		expected string
	}{
		{"let a;", "let a"},
		{"let a=1", "let a=1"},
		{"var c;", "var c"},
		{"var c=1;", "var c=1"},
	}

	for _, ts := range test {
		l = lexer.New(ts.input)
		p := New(l)
		stmt := p.parseVarDeclarationStatement()
		if stmt == nil {
			t.Errorf("expected stmt not to be nil")
			return
		}

		if stmt.String() != ts.expected {
			t.Errorf("expected statement to be %s. recieved: %s", ts.expected, stmt.String())
		}
	}
}

func TestIdentifierAssignStatement(t *testing.T) {

	test := []struct {
		input    string
		expected string
	}{
		{"a=12", "a=12"},
		{" c=1;", "c=1"},
	}

	for _, ts := range test {
		l := lexer.New(ts.input)
		p := New(l)
		stmt := p.parseIdentifierAssignStatement()
		if stmt == nil {
			t.Errorf("expected stmt not to be nil")
			return
		}

		if stmt.String() != ts.expected {
			t.Errorf("expected statement to be %s. recieved: %s", ts.expected, stmt.String())
		}
	}
}

func TestBooleanExpression(t *testing.T) {

	test := []struct {
		input    string
		expected string
	}{
		{"true", "true"},
		{"false;", "false"},
	}

	for _, ts := range test {
		l := lexer.New(ts.input)
		p := New(l)
		exp := p.parseBoolean()
		if exp == nil {
			t.Errorf("expected expression not to be nil")
			return
		}

		if exp.String() != ts.expected {
			t.Errorf("expected expression to be %s. recieved: %s", ts.expected, exp.String())
		}
	}
}

func TestParseConstStatement(t *testing.T) {
	test := []struct {
		input    string
		expected string
	}{
		{"const a=1;", "const a=1"},
	}

	for _, ts := range test {
		l := lexer.New(ts.input)
		p := New(l)
		stmt := p.parseConstStatement()
		if stmt == nil {
			t.Errorf("expected stmt not to be nil")
			return
		}

		if stmt.String() != ts.expected {
			t.Errorf("expected statement to be %s. recieved: %s", ts.expected, stmt.String())
		}
	}

}

func TestParseReturnStatement(t *testing.T) {
	l := lexer.New("return 15;")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	tests := struct {
		tl    string
		value int64
	}{"return", 15}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ReturnStatement)
	if !ok {
		t.Errorf("expected statemenst type of ReturnStatement. recieved: %T", program.Statements[0])
	}

	if tests.tl != stmt.TokenLiteral() {
		t.Errorf("expected TokenLiteral to be %s. recieved: %s", tests.tl, stmt.TokenLiteral())
	}

	intLet, ok := stmt.ReturnValue.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected statemenst type of IntLiteralExpression. recieved: %T", stmt.ReturnValue)
	}

	if intLet.Value != tests.value {
		t.Errorf("expected value to be %d. recieved: %d", tests.value, intLet.Value)
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

func TestParsePrefixExpression(t *testing.T) {
	l := lexer.New("!5\r\n")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	tests := struct {
		operator string
		tl       string
		value    int64
	}{
		operator: "!",
		tl:       "5",
		value:    5,
	}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Errorf("expected statemenst type of ExpressetionStatement. recieved: %T", program.Statements[0])
	}

	prefixExp, ok := stmt.Expression.(*ast.PrefixExpression)
	if !ok {
		t.Errorf("expected statemenst type of PrefixExpression. recieved: %T", program.Statements[0])
	}

	if tests.operator != prefixExp.Operator {
		t.Errorf("expected operator to be %s. recieved: %s", tests.operator, prefixExp.Operator)
	}

	intLitExp, ok := prefixExp.Right.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected RightNode type of IntLiteralExpression. recieved: %T", prefixExp.Right)
	}

	if tests.tl != intLitExp.TokenLiteral() {
		t.Errorf("expected IntLiteral to be %s. recieved: %s", tests.tl, intLitExp.TokenLiteral())
	}

	if tests.value != intLitExp.Value {
		t.Errorf("expected IntLiteral to be %d. recieved: %d", tests.value, intLitExp.Value)
	}
}

func TestParseInfixExpression(t *testing.T) {
	l := lexer.New("-5 > -3 \r\n")
	p := New(l)
	program := p.ParseProgram()
	checkForErrors(t, p)

	expected := ast.InfixExpression{
		Token:    lexer.Token{TokenType: lexer.GREATERTHAN, Literal: ">"},
		Operator: ">",
		Left: &ast.PrefixExpression{
			Operator: "-",
			Token:    lexer.Token{TokenType: lexer.MINUS, Literal: "-"},
			Right: &ast.IntLiteralExpression{
				Token: lexer.Token{TokenType: lexer.INT, Literal: "5"},
				Value: 5,
			},
		},
		Right: &ast.PrefixExpression{
			Operator: "-",
			Token:    lexer.Token{TokenType: lexer.MINUS, Literal: "-"},
			Right: &ast.IntLiteralExpression{
				Token: lexer.Token{TokenType: lexer.INT, Literal: "3"},
				Value: 3,
			},
		},
	}

	if len(program.Statements) != 1 {
		t.Errorf("expected statemenst length to be 1, recieved %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Errorf("expected statemenst type of ExpressetionStatement. recieved: %T", program.Statements[0])
	}

	infix, ok := stmt.Expression.(*ast.InfixExpression)
	if !ok {
		t.Errorf("expected statemenst type of InfixExpression. recieved: %T", program.Statements[0])
	}

	if expected.Operator != infix.Operator {
		t.Errorf("expected operator to be %s. recieved: %s", expected.Operator, infix.Operator)
	}

	rightPrefix, ok := infix.Right.(*ast.PrefixExpression)
	if !ok {
		t.Errorf("expected RightNode type of PrefixExpression. recieved: %T", infix.Right)
	}

	rightPrefixExp, ok := rightPrefix.Right.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected RightNodeExpression type of IntLiteralExpression. recieved: %T", rightPrefix.Right)
	}

	expectedRightExp := expected.Right.(*ast.PrefixExpression).Right.(*ast.IntLiteralExpression)
	if rightPrefixExp.Value != expectedRightExp.Value {
		t.Errorf("expected right expression value to be %d. recieved: %d", expectedRightExp.Value, rightPrefixExp.Value)
	}

	leftPrefix, ok := infix.Left.(*ast.PrefixExpression)
	if !ok {
		t.Errorf("expected LeftNode type of PrefixExpression. recieved: %T", infix.Left)
	}

	leftPrefixExp, ok := leftPrefix.Right.(*ast.IntLiteralExpression)
	if !ok {
		t.Errorf("expected LeftNodeExpression type of IntLiteralExpression. recieved: %T", leftPrefix.Right)
	}

	expectedLeftExp := expected.Left.(*ast.PrefixExpression).Right.(*ast.IntLiteralExpression)
	if leftPrefixExp.Value != expectedLeftExp.Value {
		t.Errorf("expected left expression value to be %d. recieved: %d", expectedLeftExp.Value, leftPrefixExp.Value)
	}
}

func TestParsingExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"a + b",
			"(a + b)",
		},
		{
			"!a * -5",
			"((!a) * (-5))",
		},
		{
			"-5+6*7",
			"((-5) + (6 * 7))",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		}, {
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
		{
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if test.expected != exp.String() {
			t.Errorf("expected result to be: %s. recieved: %s.", test.expected, exp.String())
		}
	}
}

func TestGroupExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"(1+5)+3", "((1 + 5) + 3)"},
		{"1+(1+3)", "(1 + (1 + 3))"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("expected expression. recieved nil.")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}
}

func TestParseFunctionExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"function( x , y ){}", "function(x,y){}"},
		{"function(){}", "function(){}"},
		{"(function(){})", "function(){}"},
		{"() => {}", "function(){}"},
		{"(()=>{})=> {}", "function(function(){}){}"},
		{"(()=>{}, abc)=> {}", "function(function(){},abc){}"},
		{"(a+b)=>{}", "function((a + b)){}"},
		{"((a+b), y)=>{}", "function((a + b),y){}"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}
}

func TestParseAsyncFunctionExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"async function( x , y ){}", "async function(x,y){}"},
		{"async function(){}", "async function(){}"},
		{"async (function(){})", "async function(){}"},
		{"async () => {}", "async function(){}"},
		{"async (()=>{})=> {}", "async function(function(){}){}"},
		{"async (()=>{}, abc)=> {}", "async function(function(){},abc){}"},
		{"async (a+b)=>{}", "async function((a + b)){}"},
		{"async ((a+b), y)=>{}", "async function((a + b),y){}"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}
}

func TestParseCallExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"async function( x , y ){}()", "async function(x,y){}()"},
		{"function(){}()", "function(){}()"},
		{"a(function(){})", "a(function(){})"},
		{"a(() => {})()", "a(function(){})()"},
		{"(a,b)=>{}(a,b)", "function(a,b){}(a,b)"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}
}

func TestParseIfExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"if (a > 5){}", "if(a > 5){}"},
		{"if (a > 5){}else{}", "if(a > 5){}else{}"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}
}

func TestParseInterpolatedString(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"`hello world`", "`\"hello world\"`"},
		{"`hello ${world}", "`\"hello \"world`"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}

}

func TestParseString(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"\"hello world\"", "\"hello world\""},
		{"\"hello ${world}\"", "\"hello ${world}\""},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseExpression(LOWEST)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}

}

func TestArrayExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"[1,4,2];", "[1,4,2]"},
		{"[abc,a+b,12]", "[abc,(a + b),12]"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseArrayExpression()
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}

}

func TestObjectExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"{abc: 12};", "{abc:12}"},
		{"{1:abc,2:[a,b]}", "{1:abc,2:[a,b]}"},
		{"{};", "{}"},
	}

	for _, test := range tests {
		l := lexer.New(test.input)
		p := New(l)
		exp := p.parseObjectExpression()
		checkForErrors(t, p)
		if exp == nil {
			t.Errorf("excpected expression not to nil")
			return
		}

		if test.expected != exp.String() {
			t.Errorf("expected: %s. Recieved: %s", test.expected, exp.String())
		}
	}

}
