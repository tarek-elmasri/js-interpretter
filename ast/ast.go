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

type BooleanExpression struct {
	Token lexer.Token
	Value bool
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
	Parameters []Expression
	Block      *BlockStatement
}

type AsyncFunctionExpression struct {
	Token lexer.Token
	Func  Expression
}

type IdentifierStatement struct {
	Token lexer.Token
	Name  *Identifier
	Value Expression
}

type ArrayExpression struct {
	Token  lexer.Token
	Values []Expression
}

type ObjectExpression struct {
	Token  lexer.Token
	Values map[Expression]Expression
}

func (os *ObjectExpression) TokenLiteral() string {
	return os.Token.Literal
}

func (os *ObjectExpression) expressionNode() {}
func (os *ObjectExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(os.TokenLiteral())
	count := 0
	for k, v := range os.Values {
		out.WriteString(k.String())
		out.WriteString(":")
		out.WriteString(v.String())
		if count != len(os.Values)-1 {
			out.WriteString(",")
		}
		count++
	}
	out.WriteString("}")
	return out.String()
}

func (ae *ArrayExpression) TokenLiteral() string {
	return ae.Token.Literal
}
func (ae *ArrayExpression) expressionNode() {}
func (ae *ArrayExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(ae.TokenLiteral())
	for i, item := range ae.Values {
		out.WriteString(item.String())
		if i != len(ae.Values)-1 {
			out.WriteString(",")
		}
	}
	out.WriteString("]")
	return out.String()
}

func (ids *IdentifierStatement) TokenLiteral() string {
	return ids.Token.Literal
}
func (ids *IdentifierStatement) statementNode() {}
func (ids *IdentifierStatement) String() string {
	out := bytes.Buffer{}
	if ids.TokenLiteral() == "const" || ids.TokenLiteral() == "let" || ids.TokenLiteral() == "let" {
		out.WriteString(ids.TokenLiteral())
		out.WriteString(" ")
	}
	out.WriteString(ids.Name.String())
	out.WriteString("=")
	out.WriteString(ids.Value.String())
	return out.String()
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

func (ase *AsyncFunctionExpression) TokenLiteral() string {
	return ase.Token.Literal
}

func (ase *AsyncFunctionExpression) expressionNode() {}
func (ase *AsyncFunctionExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString(ase.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(ase.Func.String())
	return out.String()
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
	if fe.TokenLiteral() != "function" {
		out.WriteString("function")
	} else {
		out.WriteString(fe.TokenLiteral()) // "function"
	}
	out.WriteString("(")
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

type ConstStatement struct {
	Token lexer.Token
	Name  *Identifier
	Value Expression
}

type StringExpression struct {
	Token lexer.Token
	Value string
}

type IntepolatedString struct {
	Token  lexer.Token
	Values []Expression
}

type VarDeclarationStatement struct {
	Token lexer.Token
	Name  *Identifier
	Value Expression
}

func (vd *VarDeclarationStatement) TokenLiteral() string {
	return vd.Token.Literal
}

func (vd *VarDeclarationStatement) statementNode() {}
func (vd *VarDeclarationStatement) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString(vd.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(vd.Name.String())
	if vd.Value != nil {
		out.WriteString(lexer.ASSIGN)
		out.WriteString(vd.Value.String())
	}
	return out.String()

}

func (se *StringExpression) TokenLiteral() string {
	return se.Token.Literal
}

func (se *StringExpression) expressionNode() {}

func (se *StringExpression) String() string {
	out := bytes.Buffer{}
	out.WriteString("\"")
	out.WriteString(se.Value)
	out.WriteString("\"")
	return out.String()
}

func (is *IntepolatedString) TokenLiteral() string {
	return is.Token.Literal
}

func (is *IntepolatedString) expressionNode() {}

func (is *IntepolatedString) String() string {
	out := bytes.Buffer{}
	out.WriteString("`")
	for _, exp := range is.Values {
		out.WriteString(exp.String())
	}
	out.WriteString("`")
	return out.String()
}

func (cs *ConstStatement) statementNode() {}
func (cs *ConstStatement) TokenLiteral() string {
	return cs.Token.Literal
}
func (cs *ConstStatement) String() string {
	out := bytes.NewBuffer([]byte{})
	out.WriteString(cs.TokenLiteral())
	out.WriteString(" ")
	out.WriteString(cs.Name.String())
	out.WriteString(lexer.ASSIGN)
	out.WriteString(cs.Value.String())
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
	if rs.ReturnValue != nil {
		out.WriteString(" ")
		out.WriteString(rs.ReturnValue.String())
	}
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

func (bs *BooleanExpression) TokenLiteral() string {
	return bs.Token.Literal
}

func (bs *BooleanExpression) expressionNode() {}
func (bs *BooleanExpression) String() string {
	return bs.TokenLiteral()
}
