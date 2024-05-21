package parser

import (
	"fmt"

	"github.com/tarek-elmasri/compiler/ast"
	"github.com/tarek-elmasri/compiler/lexer"
)

type Parser struct {
	lexer     *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	errors    []string
}

func New(l *lexer.Lexer) *Parser {
	p := Parser{
		lexer:  l,
		errors: []string{},
	}

	p.nextToken()
	p.nextToken()
	return &p
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
		return nil
	}

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
