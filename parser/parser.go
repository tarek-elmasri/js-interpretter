// package parser
//
// import (
// 	"fmt"
//
// 	"github.com/tarek-elmasri/compiler/ast"
// 	"github.com/tarek-elmasri/compiler/lexer"
// )
//
// type Parser struct {
// 	lexer     *lexer.Lexer
// 	curToken  lexer.Token
// 	peekToken lexer.Token
// 	errors    []string
// }
//
// func New(l *lexer.Lexer) *Parser {
// 	p := Parser{
// 		lexer:  l,
// 		errors: []string{},
// 	}
//
// 	p.nextToken()
// 	return &p
// }
//
// func (p *Parser) nextToken() {
// 	p.curToken = p.peekToken
// 	p.peekToken = p.lexer.NextToken()
// }
//
// func (p *Parser) Errors() []string { return p.errors }
//
// func (p *Parser) ParseProgram() *ast.Program {
// 	program := ast.NewProgramAST()
//
// 	for p.curToken.TokenType != lexer.EOF {
// 		switch p.curToken.TokenType {
// 		case lexer.LET, lexer.CONST, lexer.VAR:
// 			if p.peekToken.TokenType != lexer.IDENT || p.peekToken.TokenType != lexer.COMMA {
// 				p.errors = append(p.errors, fmt.Sprintf("unexpected %s.", p.peekToken.Literal))
// 			}
//
// 			s := ast.StatementNode{Identifier: &p.curToken}
//
// 		}
//
// 		p.nextToken()
// 	}
// }
