package ast

import (
	"bytes"
	"fmt"

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

type PrefixExpression struct {
	Token    lexer.Token
	Right    Expression
	Operator string
}

type InfixExpression struct {
	Token    lexer.Token
	Right    Expression
	Left     Expression
	Operator string
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

func (id *Identifier) expressionNode() {}
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

type IntLiteralExpression struct {
	Token lexer.Token
	Value int64
}

func (es *ExpressionStatement) TokenLiteral() string {
	return es.Token.Literal
}

func (es *ExpressionStatement) statementNode() {}
func (es *ExpressionStatement) String() string {
	// TODO: return expression string for now!
	return es.Expression.String()
}

func (pe *PrefixExpression) TokenLiteral() string {
	return pe.Token.Literal
}

func (pe *PrefixExpression) expressionNode() {}

func (pe *PrefixExpression) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	return out.String()
}

func (in *InfixExpression) TokenLiteral() string {
	return in.Token.Literal
}

func (in *InfixExpression) expressionNode() {}

func (in *InfixExpression) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString(in.Left.String())
	out.WriteString(" ")
	out.WriteString(in.Operator)
	out.WriteString(" ")
	out.WriteString(in.Right.String())
	return out.String()
}

func (e *IntLiteralExpression) TokenLiteral() string {
	return e.Token.Literal
}

func (e *IntLiteralExpression) expressionNode() {}

func (e *IntLiteralExpression) String() string {
	return fmt.Sprint(e.Value)
}
