package parser

import (
	"fmt"
	"strconv"

	"github.com/tarek-elmasri/compiler/ast"
	"github.com/tarek-elmasri/compiler/lexer"
)

const (
	LOWEST = iota
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	FUNC
)

type Parser struct {
	lexer           *lexer.Lexer
	curToken        lexer.Token
	peekToken       lexer.Token
	recordedTok     lexer.Token
	recordedPeekTok lexer.Token
	prefixFns       map[lexer.TokenType]prefixFn
	infixFns        map[lexer.TokenType]infixFn
	errors          []string
}

type prefixFn = func() ast.Expression
type infixFn = func(expression ast.Expression) ast.Expression

var precedenceMap = map[lexer.TokenType]int{
	lexer.EQUAL:       EQUALS,
	lexer.NOTEQUAL:    EQUALS,
	lexer.GREATERTHAN: LESSGREATER,
	lexer.LOWERTHAN:   LESSGREATER,
	lexer.PLUS:        SUM,
	lexer.MINUS:       SUM,
	lexer.ASTROUS:     PRODUCT,
	lexer.DIVIDE:      PRODUCT,
	lexer.LPARANC:     FUNC,
}

func New(l *lexer.Lexer) *Parser {
	p := Parser{
		lexer:     l,
		prefixFns: make(map[lexer.TokenType]prefixFn),
		infixFns:  make(map[lexer.TokenType]infixFn),
		errors:    []string{},
	}

	// prefix operations
	p.registerPrefixFunc(lexer.IDENT, p.parseIdentifier)
	p.registerPrefixFunc(lexer.INT, p.parseIntLiteral)
	p.registerPrefixFunc(lexer.NOT, p.parsePrefixExpression)
	p.registerPrefixFunc(lexer.MINUS, p.parsePrefixExpression)
	p.registerPrefixFunc(lexer.LPARANC, p.parseGroupExpressionOrArrowFunc)
	p.registerPrefixFunc(lexer.FUNC, p.parseFunctionExpression)
	p.registerPrefixFunc(lexer.IF, p.parseIfExpression)
	p.registerPrefixFunc(lexer.ASYNC, p.parseAsyncFuncExpression)
	// infix operations
	p.registerInfixFunc(lexer.EQUAL, p.parseInfixExpression)
	p.registerInfixFunc(lexer.NOTEQUAL, p.parseInfixExpression)
	p.registerInfixFunc(lexer.GREATERTHAN, p.parseInfixExpression)
	p.registerInfixFunc(lexer.LOWERTHAN, p.parseInfixExpression)
	p.registerInfixFunc(lexer.PLUS, p.parseInfixExpression)
	p.registerInfixFunc(lexer.MINUS, p.parseInfixExpression)
	p.registerInfixFunc(lexer.ASTROUS, p.parseInfixExpression)
	p.registerInfixFunc(lexer.DIVIDE, p.parseInfixExpression)
	p.registerInfixFunc(lexer.LPARANC, p.parseInfixCallExpression)

	// assigning cur and peek tokens
	p.nextToken()
	p.nextToken()

	return &p
}
func (l *Parser) registerPrefixFunc(tt lexer.TokenType, fn prefixFn) {
	l.prefixFns[tt] = fn
}

func (l *Parser) registerInfixFunc(tt lexer.TokenType, fn infixFn) {
	l.infixFns[tt] = fn
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

func (p *Parser) record() {
	p.recordedTok = p.curToken
	p.recordedPeekTok = p.peekToken
	p.lexer.Record()
}

func (p *Parser) recover() {
	p.lexer.Recover()
	p.curToken = p.recordedTok
	p.peekToken = p.recordedPeekTok
}

func (p *Parser) Errors() []string { return p.errors }

func (p *Parser) parseGroupExpressionOrArrowFunc() ast.Expression {
	p.record()
	exp := p.parseArrowFunc()
	if exp == nil {
		p.recover()
		return p.parseGroupExpression()
	}

	return exp
}

func (p *Parser) parseAsyncFuncExpression() ast.Expression {
	exp := &ast.AsyncFunctionExpression{Token: p.curToken}
	p.nextToken()
	exp.Func = p.parseExpression(LOWEST)
	if exp.Func == nil {
		return nil
	}

	return exp
}

func (p *Parser) parseInfixCallExpression(fn ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Func: fn}
	exp.Arguments = p.parseCallArguments()
	return exp
}

func (p *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}
	p.nextToken()

	if p.currentTokenIs(lexer.RPARANC) {
		return args
	}

	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(lexer.RPARANC) {
		return nil
	}

	return args
}

func (p *Parser) parseIfExpression() ast.Expression {
	exp := &ast.IfExpression{Token: p.curToken}

	if !p.expectPeek(lexer.LPARANC) {
		return nil
	}

	p.nextToken()

	exp.Condition = p.parseExpression(LOWEST)
	if exp.Condition == nil {
		return nil
	}

	if !p.expectPeek(lexer.RPARANC) {
		return nil
	}

	if !p.expectPeek(lexer.LSQUERLI) {
		return nil
	}

	exp.Consequences = p.parseBlockStatement()
	if exp.Consequences == nil {
		return nil
	}

	if !p.expectPeek(lexer.ELSE) {
		return exp
	}

	if !p.expectPeek(lexer.LSQUERLI) {
		return nil
	}

	exp.Alternative = p.parseBlockStatement()
	if exp.Alternative == nil {
		return nil
	}

	return exp
}

// "(5+2) + 2"
func (p *Parser) parseGroupExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RPARANC) {
		return nil
	}
	return exp
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	s := &ast.BlockStatement{
		Token:      p.curToken,
		Statements: []ast.Statement{},
	}
	p.nextToken() // "{"

	for !p.currentTokenIs(lexer.RSQUERLI) && !p.currentTokenIs(lexer.EOF) {
		if stmt := p.parseStatement(); stmt != nil {
			s.Statements = append(s.Statements, stmt)
		}

		p.nextToken()
	}

	return s
}

// avoid adding errors as it might be just a try to read
// an arrow func expression before trying to parse group expression
// unless reached the arrow token
func (p *Parser) parseArrowFunc() ast.Expression {
	exp := &ast.FunctionExpression{Token: p.curToken}
	p.nextToken()
	exp.Parameters = p.parseFunctionParameters()
	if exp.Parameters == nil {
		return nil
	}

	if !p.peekTokenIs(lexer.FUNCARROW) {
		return nil
	}

	p.nextToken()

	if !p.expectPeek(lexer.LSQUERLI) {
		return nil
	}

	exp.Block = p.parseBlockStatement()
	if exp.Block == nil {
		return nil
	}

	return exp
}

func (p *Parser) parseFunctionExpression() ast.Expression {
	exp := &ast.FunctionExpression{Token: p.curToken}

	if !p.expectPeek(lexer.LPARANC) {
		return nil
	}

	p.nextToken() // passing LPARANC

	exp.Parameters = p.parseFunctionParameters()
	if exp.Parameters == nil {
		return nil
	}

	if !p.expectPeek(lexer.LSQUERLI) {
		return nil
	}

	exp.Block = p.parseBlockStatement()
	if exp.Block == nil {
		return nil
	}

	return exp
}

// avoid adding errors as it might be just a try to read
// an arrow func expression before trying to parse group expression
func (p *Parser) parseFunctionParameters() []ast.Expression {
	exps := []ast.Expression{}
	if p.currentTokenIs(lexer.RPARANC) {
		return exps
	}
	exp := p.parseExpression(LOWEST)
	if exp == nil {
		return nil
	}
	exps = append(exps, exp)

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		exp = p.parseExpression(LOWEST)
		if exp == nil {
			return nil
		}
		exps = append(exps, exp)
	}

	if !p.peekTokenIs(lexer.RPARANC) {
		return nil
	}

	p.nextToken()

	return exps

}

func (p *Parser) ParseProgram() *ast.Program {
	program := ast.NewProgramAST()
	for p.curToken.TokenType != lexer.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}

		p.nextToken()
	}

	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.TokenType {
	case lexer.LET:
		return p.parseLetStatement()
	case lexer.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)

	return stmt
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	exp := &ast.PrefixExpression{Token: p.curToken, Operator: p.curToken.Literal}
	p.nextToken()
	exp.Right = p.parseExpression(PREFIX)
	return exp
}

func (p *Parser) parseInfixExpression(e ast.Expression) ast.Expression {
	exp := &ast.InfixExpression{
		Left:     e,
		Operator: p.curToken.Literal,
		Token:    p.curToken,
	}

	// grapping current token precedence as parse expresseion gonna compare
	// for next token precedence
	precedence := p.curTokenPrecedence()
	p.nextToken()
	exp.Right = p.parseExpression(precedence)
	return exp
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix, ok := p.prefixFns[p.curToken.TokenType]
	if !ok {
		p.errors = append(p.errors, fmt.Sprintf("no prefix pareser for %s token.", p.curToken.Literal))
		return nil
	}

	leftExpression := prefix()
	// -5 > 5
	// !5
	for precedence < p.peekTokenPrecedence() {
		infix, ok := p.infixFns[p.peekToken.TokenType]
		if !ok {
			return leftExpression
		}

		p.nextToken()
		leftExpression = infix(leftExpression)
	}

	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return leftExpression
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) addUnexpectedTokenError(exp lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("line: %d:%d - expected token to be %s, recieved: %s", p.peekToken.LineNumber, p.peekToken.PositionNumber, exp, p.peekToken.TokenType))
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	// TODO: multi assign statement
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	if !p.expectPeek(lexer.ASSIGN) {
		return nil
	}
	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)
	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) currentTokenIs(tt lexer.TokenType) bool {
	return p.curToken.TokenType == tt
}

func (p *Parser) peekTokenIs(tt lexer.TokenType) bool {
	return p.peekToken.TokenType == tt
}

// if next token == expected -> move forward
func (p *Parser) expectPeek(tt lexer.TokenType) bool {
	if p.peekToken.TokenType == tt {
		p.nextToken()
		return true
	} else {
		p.addUnexpectedTokenError(tt)
		return false
	}
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseIntLiteral() ast.Expression {
	exp := &ast.IntLiteralExpression{Token: p.curToken}
	v, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("error parsing int value. %v", p.curToken.Literal))
		return nil
	}

	exp.Value = v
	return exp
}

func (p *Parser) curTokenPrecedence() int {
	if p, ok := precedenceMap[p.curToken.TokenType]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekTokenPrecedence() int {
	if p, ok := precedenceMap[p.peekToken.TokenType]; ok {
		return p
	}
	return LOWEST
}

// TODO:
// for loops
// switch statements
