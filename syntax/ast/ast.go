package ast

import (
	"cool-compiler/lexer"
)

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Feature interface {
	Node
	featureNode()
}

type TypeIdentifier struct {
	Token lexer.Token
	Value string
}

func (ti *TypeIdentifier) TokenLiteral() string { return ti.Token.Literal }

type ObjectIdentifier struct {
	Token lexer.Token
	Value string
}

func (oi *ObjectIdentifier) TokenLiteral() string { return oi.Token.Literal }
func (oi *ObjectIdentifier) expressionNode()      {}

type Program struct {
	Classes []*Class
}

func (p *Program) TokenLiteral() string { return "" }

type Class struct {
	Token    lexer.Token
	Name     string
	Features []Feature
}

func (c *Class) TokenLiteral() string { return c.Token.Literal }

type Attribute struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
}

func (a *Attribute) TokenLiteral() string { return a.Token.Literal }
func (a *Attribute) featureNode()         {}

type Method struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
}

func (m *Method) TokenLiteral() string { return m.Token.Literal }
func (m *Method) featureNode()         {}
