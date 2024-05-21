package ast

import (
	"bytes"
	"github.com/tarek-elmasri/compiler/lexer"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func NewProgramAST() *Program {
	return &Program{
		Statements: []Statement{},
	}
}

type Identifier struct {
	Token lexer.Token
	Value string
}

type LetStatement struct {
	Token lexer.Token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode() {}
func (ls *LetStatement) TokenLiteral() string {
	return ls.Token.Literal
}
func (ls *LetStatement) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString(ls.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(ls.Name.String())
	out.WriteString(lexer.ASSIGN)
	out.WriteString(ls.Value.String())
	return out.String()
}

func (id *Identifier) expresseionNode() {}
func (id *Identifier) TokenLiteral() string {
	return id.Token.Literal
}
func (id *Identifier) String() string {
	return id.TokenLiteral()
}

type ReturnStatement struct {
	Token       lexer.Token
	ReturnValue Expression
}

func (rs *ReturnStatement) TokenLiteral() string {
	return rs.Token.Literal
}

func (rs *ReturnStatement) statementNode() {}

func (rs *ReturnStatement) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString(rs.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(rs.ReturnValue.String())
	return out.String()
}

type ExpressionStatement struct {
	Token      lexer.Token
	Expression Expression
}

func (es *ExpressionStatement) TokenLiteral() string {
	return es.Token.Literal
}

func (es *ExpressionStatement) statementNode() {}
func (es *ExpressionStatement) String() string {
	// TODO: return expression string for now!
	return es.Expression.String()
}
