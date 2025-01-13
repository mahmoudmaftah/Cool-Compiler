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
