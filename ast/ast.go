package ast

import (
	"bytes"
	"strings"

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

// function abc {}
type FunctionDeclaration struct {
	Token      lexer.Token
	Identifier *Identifier
	Parameters []*Identifier
	Block      *BlockStatement
}

// const bb = () => {}
// const dd = function(){}
type FunctionExpression struct {
	Token      lexer.Token // "function" or "("
	Parameters []*Identifier
	Block      *BlockStatement
}

// const cc = bb()
// add(as)
// (function(){})()
// (()=>{})()
type CallExpression struct {
	Token     lexer.Token
	Func      Expression // FunctionExpression or Identifier
	Arguments []Expression
}

type BlockStatement struct {
	Token      lexer.Token
	Statements []Statement
}

// if (<condition>) {<block>} else {<block>}
type IfExpression struct {
	Token        lexer.Token
	Condition    Expression
	Consequences *BlockStatement
	Alternative  *BlockStatement
}

func (ie *IfExpression) TokenLiteral() string {
	return ie.Token.Literal
}

func (ie *IfExpression) expressionNode() {}
func (ie *IfExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(ie.TokenLiteral())
	out.WriteString(ie.Condition.String())
	out.WriteString(ie.Consequences.String())
	if ie.Alternative != nil {
		out.WriteString("else")
		out.WriteString(ie.Alternative.String())
	}
	return out.String()
}

func (bs *BlockStatement) TokenLiteral() string {
	return bs.Token.Literal
}

func (bs *BlockStatement) statementNode() {}
func (bs *BlockStatement) String() string {
	out := bytes.Buffer{}
	out.WriteString(bs.TokenLiteral())
	for _, stat := range bs.Statements {
		out.WriteString(stat.String())
	}
	out.WriteString("}")
	return out.String()
}

func (fd *FunctionDeclaration) TokenLiteral() string {
	return fd.Token.Literal
}

func (fd *FunctionDeclaration) statementNode() {}
func (fd *FunctionDeclaration) String() string {
	out := bytes.Buffer{}
	out.WriteString(fd.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(fd.Identifier.String())
	out.WriteString("(")
	parametersStr := []string{}
	for _, p := range fd.Parameters {
		parametersStr = append(parametersStr, p.String())
	}
	out.WriteString(strings.Join(parametersStr, ","))
	out.WriteString("){")
	for _, statement := range fd.Block.Statements {
		out.WriteString(statement.String())
	}
	out.WriteString("}")
	return out.String()
}

func (fe *FunctionExpression) TokenLiteral() string {
	return fe.Token.Literal
}

func (fe *FunctionExpression) expressionNode() {}
func (fe *FunctionExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(fe.TokenLiteral()) // "function" or "("
	if fe.Token.TokenType == lexer.FUNC {
		out.WriteString("(")
	}
	parametersStr := []string{}
	for _, p := range fe.Parameters {
		parametersStr = append(parametersStr, p.String())
	}
	out.WriteString(strings.Join(parametersStr, ","))
	out.WriteString("){")
	for _, statement := range fe.Block.Statements {
		out.WriteString(statement.String())
	}
	out.WriteString("}")
	return out.String()
}

func (ce *CallExpression) TokenLiteral() string {
	return ce.Token.Literal
}

func (ce *CallExpression) expressionNode() {}
func (ce *CallExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(ce.Func.String())
	out.WriteString("(")
	argsStr := []string{}
	for _, arg := range ce.Arguments {
		argsStr = append(argsStr, arg.String())
	}
	out.WriteString(strings.Join(argsStr, ","))
	out.WriteString(")")
	return out.String()
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
	out.WriteString("(")
	out.WriteString(in.Left.String())
	out.WriteString(" ")
	out.WriteString(in.Operator)
	out.WriteString(" ")
	out.WriteString(in.Right.String())
	out.WriteString(")")
	return out.String()
}

func (e *IntLiteralExpression) TokenLiteral() string {
	return e.Token.Literal
}

func (e *IntLiteralExpression) expressionNode() {}

func (e *IntLiteralExpression) String() string {
	return e.TokenLiteral()
}
