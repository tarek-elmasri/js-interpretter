package parser

import (
	"fmt"
	"strconv"

	"github.com/tarek-elmasri/compiler/ast"
	"github.com/tarek-elmasri/compiler/lexer"
)

const (
	LOWEST = iota
	PREFIX
	PLUS
	MINUS
	ASTROUS
	DIVIDE
	CALLBACK
)

type Parser struct {
	lexer     *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	prefixFns map[lexer.TokenType]prefixFn
	infixFns  map[lexer.TokenType]infixFn
	errors    []string
}

type prefixFn = func() ast.Expression
type infixFn = func(expression *ast.Expression) ast.Expression

func New(l *lexer.Lexer) *Parser {
	p := Parser{
		lexer:     l,
		prefixFns: make(map[lexer.TokenType]prefixFn),
		infixFns:  make(map[lexer.TokenType]infixFn),
		errors:    []string{},
	}
	p.registerPrefixFunc(lexer.IDENT, p.parseIdentifier)
	p.registerPrefixFunc(lexer.INT, p.parseIntLiteral)
	p.registerPrefixFunc(lexer.NOT, p.parsePrefixExpression)
	p.registerPrefixFunc(lexer.MINUS, p.parsePrefixExpression)

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

func (p *Parser) Errors() []string { return p.errors }

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

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix, ok := p.prefixFns[p.curToken.TokenType]
	if !ok {
		p.errors = append(p.errors, fmt.Sprintf("no prefix pareser for %s token.", p.curToken.Literal))
		return nil
	}

	leftExpression := prefix()

	return leftExpression
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) addUnexpectedTokenError() {
	p.errors = append(p.errors, fmt.Sprintf("line: %d:%d - unexpected %s.", p.peekToken.LineNumber, p.peekToken.PositionNumber, p.peekToken.Literal))
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	// TODO: multi assign statement
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(lexer.IDENT) {
		p.addUnexpectedTokenError()
		return nil
	}

	stmt.Name = &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	if !p.expectPeek(lexer.ASSIGN) {
		p.addUnexpectedTokenError()
		return nil
	}

	// TODO: read expression value

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
		return false
	}
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()
	// TODO: read return value
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
