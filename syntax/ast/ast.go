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












type Formal struct {
    Token    lexer.Token
    Name     *ObjectIdentifier
    TypeDecl *TypeIdentifier
}

func (f *Formal) TokenLiteral() string { return f.Token.Literal }

// Method with formal parameters
type Method struct {
    Token    lexer.Token
    Name     *ObjectIdentifier
    Formals  []*Formal
    TypeDecl *TypeIdentifier
    Body     Expression
}

func (m *Method) TokenLiteral() string { return m.Token.Literal }
func (m *Method) featureNode()         {}













// Updated Attribute to include initialization
type Attribute struct {
    Token    lexer.Token
    Name     *ObjectIdentifier
    TypeDecl *TypeIdentifier
    Init     Expression
}
func (a *Attribute) TokenLiteral() string { return a.Token.Literal }
func (a *Attribute) featureNode()         {}

















// Expression types
type IntegerLiteral struct {
    Token lexer.Token
    Value int64
}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) expressionNode()      {}





type StringLiteral struct {
    Token lexer.Token
    Value string
}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) expressionNode()      {}





type BooleanLiteral struct {
    Token lexer.Token
    Value bool
}
func (bl *BooleanLiteral) TokenLiteral() string { return bl.Token.Literal }
func (bl *BooleanLiteral) expressionNode()      {}




type BinaryExpression struct {
    Token    lexer.Token
    Left     Expression
    Operator string
    Right    Expression
}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BinaryExpression) expressionNode()      {}





type UnaryExpression struct {
    Token    lexer.Token
    Operator string
    Right    Expression
}
func (ue *UnaryExpression) TokenLiteral() string { return ue.Token.Literal }
func (ue *UnaryExpression) expressionNode()      {}





type IfExpression struct {
    Token       lexer.Token
    Condition   Expression
    Consequence Expression
    Alternative Expression
}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) expressionNode()      {}





type WhileExpression struct {
    Token     lexer.Token
    Condition Expression
    Body      Expression
}
func (we *WhileExpression) TokenLiteral() string { return we.Token.Literal }
func (we *WhileExpression) expressionNode()      {}




// We will need BlockExpression as well for the body of (if/ while/ classes ...)
type BlockExpression struct {
    Token       lexer.Token
    Expressions []Expression
}
func (be *BlockExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BlockExpression) expressionNode()      {}






type LetBinding struct {
    Identifier *ObjectIdentifier
    Type       *TypeIdentifier
    Init       Expression
}

type LetExpression struct {
    Token    lexer.Token
    Bindings []*LetBinding
    In       Expression
}

func (le *LetExpression) TokenLiteral() string { return le.Token.Literal }
func (le *LetExpression) expressionNode()      {}

type CaseBranch struct {
    Token      lexer.Token
    Pattern    *ObjectIdentifier
    Type       *TypeIdentifier
    Expression Expression
}



type CaseExpression struct {
    Token    lexer.Token
    Expr     Expression
    Branches []*CaseBranch
}

func (ce *CaseExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CaseExpression) expressionNode()      {}

type NewExpression struct {
    Token lexer.Token
    Type  *TypeIdentifier
}

func (ne *NewExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NewExpression) expressionNode()      {}

type IsVoidExpression struct {
    Token      lexer.Token
    Expression Expression
}

func (iv *IsVoidExpression) TokenLiteral() string { return iv.Token.Literal }
func (iv *IsVoidExpression) expressionNode()      {}

type DispatchExpression struct {
    Token      lexer.Token
    Object     Expression   // Can be nil for implicit self dispatch
    StaticType *TypeIdentifier  // Can be nil for dynamic dispatch
    Method     *ObjectIdentifier
    Arguments  []Expression
}

func (de *DispatchExpression) TokenLiteral() string { return de.Token.Literal }
func (de *DispatchExpression) expressionNode()      {}
