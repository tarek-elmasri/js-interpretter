package ast

import "github.com/tarek-elmasri/compiler/lexer"

type Node interface {
	tokenLiteral() string
}

type Statement interface {
	statementLiteral() string
}

type Expression interface {
	expressionLiteral() string
}

type Program struct {
	Statements []Statement
}

type StatementNode struct {
	Identifier *lexer.Token
	Name       string
	Value      Expression
}

type ExpressionNode struct {
	Identifier *lexer.Token
	Name       string
	Value      Statement
}

func (s *StatementNode) statementLiteral() string {
	return ""
}

func (s *StatementNode) tokenLiteral() string {
	return s.Identifier.Literal
}

func (e *ExpressionNode) expresseionLiteral() string { return "" }
func (e *ExpressionNode) tokenLiteral() string       { return e.Identifier.Literal }

func NewProgramAST() *Program {
	return &Program{Statements: []Statement{}}
}

func (p *Program) Add(s Statement) {
	p.Statements = append(p.Statements, s)
}
