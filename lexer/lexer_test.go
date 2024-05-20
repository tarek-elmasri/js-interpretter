package lexer

import "testing"

func TestTokenize(t *testing.T) {
	input := `const result= [  x + y ]
	-/*
	let m= 12
	const d = "1"
	var 'cvd' {}
	,:;
	from import
	if switch case
	export default as
	>
	<
	 function
	.
	=>
	==
	===
	12.55
	.55
	.sa
	async await
	new
	!
	!=
	`
	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{CONST, "const"},
		{IDENT, "result"},
		{ASSIGN, "="},
		{LPRACES, "["},
		{IDENT, "x"},
		{PLUS, "+"},
		{IDENT, "y"},
		{RPRACES, "]"},
		{MINUS, "-"},
		{DIVIDE, "/"},
		{ASTROUS, "*"},
		{LET, "let"},
		{IDENT, "m"},
		{ASSIGN, "="},
		{INT, "12"},
		{CONST, "const"},
		{IDENT, "d"},
		{ASSIGN, "="},
		{STRING, "1"},
		{VAR, "var"},
		{STRING, "cvd"},
		{LSQUERLI, "{"},
		{RSQUERLI, "}"},
		{COMMA, ","},
		{COLON, ":"},
		{SEMICOLON, ";"},
		{FROM, "from"},
		{IMPORT, "import"},
		{IF, "if"},
		{SWITCH, "switch"},
		{CASE, "case"},
		{EXPORT, "export"},
		{DEFAULT, "default"},
		{AS, "as"},
		{GREATERTHAN, ">"},
		{LOWERTHAN, "<"},
		{FUNC, "function"},
		{DOT, "."},
		{FUNCARROW, "=>"},
		{EQUAL, "=="},
		{EQUAL, "==="},
		{FLOAT, "12.55"},
		{FLOAT, ".55"},
		{DOT, "."},
		{IDENT, "sa"},
		{ASYNC, "async"},
		{AWAIT, "await"},
		{NEW, "new"},
		{NOT, "!"},
		{NOTEQUAL, "!="},
		{EOF, "EOF"},
	}

	l := New(input)

	for i := 0; i < len(tests); i++ {
		tok := l.NextToken()
		t.Logf("line: %d: %d", tok.LineNumber, tok.PositionNumber)
		if tests[i].expectedType != tok.TokenType || tests[i].expectedLiteral != tok.Literal {
			t.Logf("invalid token. recieved: %+v. expected: %+v\n", tok, tests[i])
			t.Fail()
		}

	}
}
