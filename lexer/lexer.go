package lexer

import (
	"log/slog"
)

type TokenType string

const (
	IDENT              = "IDENT"
	NOT                = "!"
	NOTEQUAL           = "!="
	COMMA              = ","
	COLON              = ":"
	SEMICOLON          = ";"
	GREATERTHAN        = ">"
	LOWERTHAN          = "<"
	FUNCARROW          = "=>"
	ASSIGN             = "="
	EQUAL              = "=="
	BACKTICKS          = "`"
	QUTES              = "\""
	SQUTES             = "'"
	DOT                = "."
	MINUS              = "-"
	PLUS               = "+"
	ASTROUS            = "*"
	DIVIDE             = "/"
	LPRACES            = "]"
	RPRACES            = "["
	RSQUERLI           = "}"
	LSQUERLI           = "{"
	STRING             = "STRING"
	INTERPOLATEDSTRING = "INTERPOLATEDSTRING"
	INT                = "INT"
	FLOAT              = "FLOAT"
	FUNC               = "FUNC"
	IF                 = "IF"
	SWITCH             = "SWITCH"
	CASE               = "CASE"
	RETURN             = "RETURN"
	IMPORT             = "IMPORT"
	DEFAULT            = "default"
	EXPORT             = "export"
	AS                 = "as"
	CONST              = "CONST"
	LET                = "LET"
	FROM               = "FROM"
	VAR                = "VAR"
	ASYNC              = "ASYNC"
	AWAIT              = "AWAIT"
	NEW                = "NEW"
	EOF                = "EOF"
	ILLEG              = "ILLEG"
)

var keywordMap map[string]TokenType = map[string]TokenType{
	"function": FUNC,
	"if":       IF,
	"return":   RETURN,
	"import":   IMPORT,
	"const":    CONST,
	"let":      LET,
	"var":      VAR,
	"from":     FROM,
	"switch":   SWITCH,
	"case":     CASE,
	"export":   EXPORT,
	"default":  DEFAULT,
	"as":       AS,
	"async":    ASYNC,
	"await":    AWAIT,
	"new":      NEW,
}

type Token struct {
	TokenType      TokenType
	Literal        string
	LineNumber     int
	PositionNumber int
}
type Lexer struct {
	source         string
	ch             byte
	position       int
	readPosition   int
	currentLine    int
	inlinePosition int
	// filename     string
}

func New(source string) *Lexer {
	l := &Lexer{
		source:      source,
		currentLine: 1,
	}

	l.readChar()
	return l
}

func (l *Lexer) NextToken() Token {
	var tok Token

	l.skipWhitespace()

	switch l.ch {
	case '!':
		tok = l.readNotOrNotEqual()
	case '=':
		tok = l.readEqualOrAssignOrFuncArrow()
	case '-':
		tok = newToken(MINUS, "-", l.currentLine, l.inlinePosition)
	case '+':
		tok = newToken(PLUS, "+", l.currentLine, l.inlinePosition)
	case '*':
		tok = newToken(ASTROUS, "*", l.currentLine, l.inlinePosition)
	case '/':
		tok = newToken(DIVIDE, "/", l.currentLine, l.inlinePosition)
	case '[':
		tok = newToken(LPRACES, "[", l.currentLine, l.inlinePosition)
	case ']':
		tok = newToken(RPRACES, "]", l.currentLine, l.inlinePosition)
	case '{':
		tok = newToken(LSQUERLI, "{", l.currentLine, l.inlinePosition)
	case '}':
		tok = newToken(RSQUERLI, "}", l.currentLine, l.inlinePosition)
	case ',':
		tok = newToken(COMMA, ",", l.currentLine, l.inlinePosition)
	case ':':
		tok = newToken(COLON, ":", l.currentLine, l.inlinePosition)
	case ';':
		tok = newToken(SEMICOLON, ";", l.currentLine, l.inlinePosition)
	case '"':
		// return as reading string will pass the qutes
		return newToken(STRING, l.readString(), l.currentLine, l.inlinePosition)
	case '\'':
		// return as reading string will pass the qutes
		return newToken(STRING, l.readString(), l.currentLine, l.inlinePosition)
	case '`':
		// return as reading string will pass the qutes
		return newToken(INTERPOLATEDSTRING, l.readString(), l.currentLine, l.inlinePosition)
	case '>':
		tok = newToken(GREATERTHAN, ">", l.currentLine, l.inlinePosition)
	case '<':
		tok = newToken(LOWERTHAN, "<", l.currentLine, l.inlinePosition)
	case '.':
		peekChar := l.peekChars(1)
		if isDigit(peekChar[0]) {
			return l.readDigit()
		}
		tok = newToken(DOT, ".", l.currentLine, l.inlinePosition)
	case 0:
		tok = newToken(EOF, "EOF", l.currentLine, l.inlinePosition)
	default:
		if isLetter(l.ch) {
			tok = l.readIdentifier()
			return tok
		} else if isDigit(l.ch) {
			tok = l.readDigit()
			return tok
		} else {
			slog.Error("ilegal token.", "ch:", string(l.ch))
			tok = newToken(ILLEG, "ILLEG", l.currentLine, l.inlinePosition)
		}
	}

	l.readChar()

	return tok
}

func newToken(tokenType TokenType, tokenLiteral string, line, position int) Token {
	return Token{TokenType: tokenType, Literal: tokenLiteral, LineNumber: line, PositionNumber: position}
}

func (l *Lexer) readNotOrNotEqual() Token {
	peekCh := l.peekChars(1)[0]

	if peekCh == '=' {
		l.readChar()
		return newToken(NOTEQUAL, "!=", l.currentLine, l.inlinePosition)
	} else {
		return newToken(NOT, "!", l.currentLine, l.inlinePosition)
	}
}

func (l *Lexer) readIdentifier() Token {
	startPos := l.position

	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}

	var tt TokenType
	tl := string(l.source[startPos:l.position])

	// match ident. with keyword
	t, ok := keywordMap[tl]
	if ok {
		tt = t
	} else {
		tt = IDENT
	}

	return newToken(tt, tl, l.currentLine, l.inlinePosition)
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.source) {
		l.ch = 0
	} else {
		l.ch = l.source[l.readPosition]
	}

	if l.ch == '\n' {
		l.currentLine++
		l.inlinePosition = 0
	}

	l.position = l.readPosition
	l.readPosition += 1
	l.inlinePosition++
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\n' || l.ch == '\r' || l.ch == '\t' {
		l.readChar()
	}
}

func isLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return ('0' <= ch && ch <= '9')
}

func (l *Lexer) readDigit() Token {
	hasDot := false
	startPosition := l.position
	for isDigit(l.ch) || isDot(l.ch) {
		// break if already has a dot
		if isDot(l.ch) && hasDot {
			break
		}

		if isDot(l.ch) {
			hasDot = true
		}
		l.readChar()
	}

	var tt TokenType
	if hasDot {
		tt = FLOAT
	} else {
		tt = INT
	}
	digit := l.source[startPosition:l.position]
	currentLine := l.currentLine
	inlinePosition := l.inlinePosition

	return newToken(tt, digit, currentLine, inlinePosition)
}

func isDot(ch byte) bool {
	return ch == '.'
}

func (l *Lexer) readString() string {

	startChar := l.ch

	// passing qutes
	l.readChar()

	startPosition := l.position

	for l.ch != startChar && l.ch != 0 {
		l.readChar()
	}
	str := l.source[startPosition:l.position]
	//passing qutes
	l.readChar()
	return str
}

//	func (l *lexer) readFor(ch byte) string {
//		startPosition := l.position
//
//		for l.ch != ch && l.ch != 0 {
//			l.readChar()
//		}
//
//		return l.source[startPosition:l.position]
//	}
func (l *Lexer) peekChars(n int) []byte {
	chars := make([]byte, n, n)
	for i := 0; i < n; i++ {
		if l.readPosition+i >= len(l.source) {
			chars[i] = 0
		} else {
			chars[i] = l.source[l.readPosition+i]
		}
	}
	return chars
}

func (l *Lexer) readEqualOrAssignOrFuncArrow() Token {
	peekChars := l.peekChars(2)

	if peekChars[0] == '>' {
		// read next arrow
		l.readChar()
		return newToken(FUNCARROW, "=>", l.currentLine, l.inlinePosition)
	}

	// Assign. ie. next char not =
	if peekChars[0] != '=' {
		return newToken(ASSIGN, "=", l.currentLine, l.inlinePosition)
	}

	// double Equals. ie. ==
	// have to read next char
	if peekChars[1] != '=' {
		l.readChar()
		return newToken(EQUAL, "==", l.currentLine, l.inlinePosition)
	}

	// triple Equals. ie. ===
	// have to read double next char
	l.readChar()
	l.readChar()
	return newToken(EQUAL, "===", l.currentLine, l.inlinePosition)
}
