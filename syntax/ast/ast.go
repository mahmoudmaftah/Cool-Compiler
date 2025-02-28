package ast

import (
	"bytes"
	"cool-compiler/lexer"
	"fmt"
)

type Node interface {
	TokenLiteral() string
	String() string // Adding String() method for better debugging
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
	Token    lexer.Token
	Value    string
	IsArray  bool   // Whether this is an array type
	ElemType string // Element type for arrays
}

func (ti *TypeIdentifier) TokenLiteral() string { return ti.Token.Literal }
func (ti *TypeIdentifier) String() string {
	if ti.IsArray {
		return fmt.Sprintf("Array[%s]", ti.ElemType)
	}
	return ti.Value
}

type ObjectIdentifier struct {
	Token lexer.Token
	Value string
}

func (oi *ObjectIdentifier) TokenLiteral() string { return oi.Token.Literal }
func (oi *ObjectIdentifier) expressionNode()      {}
func (oi *ObjectIdentifier) String() string       { return oi.Value }

type Program struct {
	Classes []*Class
}

func (p *Program) TokenLiteral() string { return "" }
func (p *Program) String() string {
	var out bytes.Buffer
	for _, c := range p.Classes {
		out.WriteString(c.String())
		out.WriteString("\n")
	}
	return out.String()
}

type Class struct {
	Token    lexer.Token
	Name     *TypeIdentifier
	Features []Feature
	Parent   *TypeIdentifier
}

func (c *Class) TokenLiteral() string { return c.Token.Literal }
func (c *Class) String() string {
	var out bytes.Buffer
	out.WriteString("class ")
	out.WriteString(c.Name.String())
	if c.Parent != nil {
		out.WriteString(" inherits ")
		out.WriteString(c.Parent.String())
	}
	out.WriteString(" {\n")
	for _, f := range c.Features {
		out.WriteString("  ")
		out.WriteString(f.String())
		out.WriteString("\n")
	}
	out.WriteString("}\n")
	return out.String()
}

type Formal struct {
	Token    lexer.Token
	Name     *ObjectIdentifier
	TypeDecl *TypeIdentifier
}

func (f *Formal) TokenLiteral() string { return f.Token.Literal }
func (f *Formal) String() string {
	var out bytes.Buffer
	out.WriteString(f.Name.String())
	out.WriteString(" : ")
	out.WriteString(f.TypeDecl.String())
	return out.String()
}

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
func (m *Method) String() string {
	var out bytes.Buffer
	out.WriteString(m.Name.String())
	out.WriteString("(")
	for i, f := range m.Formals {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(f.String())
	}
	out.WriteString(") : ")
	out.WriteString(m.TypeDecl.String())
	out.WriteString(" {\n")
	out.WriteString("    ")
	out.WriteString(m.Body.String())
	out.WriteString("\n  }")
	return out.String()
}

// Updated Attribute to include initialization
type Attribute struct {
	Token    lexer.Token
	Name     *ObjectIdentifier
	TypeDecl *TypeIdentifier
	Init     Expression
}

func (a *Attribute) TokenLiteral() string { return a.Token.Literal }
func (a *Attribute) featureNode()         {}
func (a *Attribute) String() string {
	var out bytes.Buffer
	out.WriteString(a.Name.String())
	out.WriteString(" : ")
	out.WriteString(a.TypeDecl.String())
	if a.Init != nil {
		out.WriteString(" <- ")
		out.WriteString(a.Init.String())
	}
	out.WriteString(";")
	return out.String()
}

// Expression types
type IntegerLiteral struct {
	Token lexer.Token
	Value int64
}

func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

type StringLiteral struct {
	Token lexer.Token
	Value string
}

func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) String() string       { return fmt.Sprintf("\"%s\"", sl.Value) }

type BooleanLiteral struct {
	Token lexer.Token
	Value bool
}

func (bl *BooleanLiteral) TokenLiteral() string { return bl.Token.Literal }
func (bl *BooleanLiteral) expressionNode()      {}
func (bl *BooleanLiteral) String() string       { return bl.Token.Literal }

type BinaryExpression struct {
	Token    lexer.Token
	Left     Expression
	Operator string
	Right    Expression
}

func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(be.Left.String())
	out.WriteString(" " + be.Operator + " ")
	out.WriteString(be.Right.String())
	out.WriteString(")")
	return out.String()
}

type UnaryExpression struct {
	Token    lexer.Token
	Operator string
	Right    Expression
}

func (ue *UnaryExpression) TokenLiteral() string { return ue.Token.Literal }
func (ue *UnaryExpression) expressionNode()      {}
func (ue *UnaryExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ue.Operator)
	out.WriteString(ue.Right.String())
	out.WriteString(")")
	return out.String()
}

type IfExpression struct {
	Token       lexer.Token
	Condition   Expression
	Consequence Expression
	Alternative Expression
}

func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) String() string {
	var out bytes.Buffer
	out.WriteString("if ")
	out.WriteString(ie.Condition.String())
	out.WriteString(" then ")
	out.WriteString(ie.Consequence.String())
	out.WriteString(" else ")
	out.WriteString(ie.Alternative.String())
	out.WriteString(" fi")
	return out.String()
}

type WhileExpression struct {
	Token     lexer.Token
	Condition Expression
	Body      Expression
}

func (we *WhileExpression) TokenLiteral() string { return we.Token.Literal }
func (we *WhileExpression) expressionNode()      {}
func (we *WhileExpression) String() string {
	var out bytes.Buffer
	out.WriteString("while ")
	out.WriteString(we.Condition.String())
	out.WriteString(" loop ")
	out.WriteString(we.Body.String())
	out.WriteString(" pool")
	return out.String()
}

// We will need BlockExpression as well for the body of (if/ while/ classes ...)
type BlockExpression struct {
	Token       lexer.Token
	Expressions []Expression
}

func (be *BlockExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BlockExpression) expressionNode()      {}
func (be *BlockExpression) String() string {
	var out bytes.Buffer
	out.WriteString("{\n")
	for _, e := range be.Expressions {
		out.WriteString("  ")
		out.WriteString(e.String())
		out.WriteString(";\n")
	}
	out.WriteString("}")
	return out.String()
}

type LetBinding struct {
	Identifier *ObjectIdentifier
	Type       *TypeIdentifier
	Init       Expression
}

func (lb *LetBinding) String() string {
	var out bytes.Buffer
	out.WriteString(lb.Identifier.String())
	out.WriteString(" : ")
	out.WriteString(lb.Type.String())
	if lb.Init != nil {
		out.WriteString(" <- ")
		out.WriteString(lb.Init.String())
	}
	return out.String()
}

type LetExpression struct {
	Token    lexer.Token
	Bindings []*LetBinding
	In       Expression
}

func (le *LetExpression) TokenLiteral() string { return le.Token.Literal }
func (le *LetExpression) expressionNode()      {}
func (le *LetExpression) String() string {
	var out bytes.Buffer
	out.WriteString("let ")
	for i, b := range le.Bindings {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(b.String())
	}
	out.WriteString(" in ")
	out.WriteString(le.In.String())
	return out.String()
}

type CaseBranch struct {
	Token      lexer.Token
	Pattern    *ObjectIdentifier
	Type       *TypeIdentifier
	Expression Expression
}

func (cb *CaseBranch) String() string {
	var out bytes.Buffer
	out.WriteString(cb.Pattern.String())
	out.WriteString(" : ")
	out.WriteString(cb.Type.String())
	out.WriteString(" => ")
	out.WriteString(cb.Expression.String())
	return out.String()
}

type CaseExpression struct {
	Token    lexer.Token
	Expr     Expression
	Branches []*CaseBranch
}

func (ce *CaseExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CaseExpression) expressionNode()      {}
func (ce *CaseExpression) String() string {
	var out bytes.Buffer
	out.WriteString("case ")
	out.WriteString(ce.Expr.String())
	out.WriteString(" of\n")
	for _, b := range ce.Branches {
		out.WriteString("  ")
		out.WriteString(b.String())
		out.WriteString(";\n")
	}
	out.WriteString("esac")
	return out.String()
}

type NewExpression struct {
	Token lexer.Token
	Type  *TypeIdentifier
}

func (ne *NewExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NewExpression) expressionNode()      {}
func (ne *NewExpression) String() string {
	var out bytes.Buffer
	out.WriteString("new ")
	out.WriteString(ne.Type.String())
	return out.String()
}

type IsVoidExpression struct {
	Token      lexer.Token
	Expression Expression
}

func (iv *IsVoidExpression) TokenLiteral() string { return iv.Token.Literal }
func (iv *IsVoidExpression) expressionNode()      {}
func (iv *IsVoidExpression) String() string {
	var out bytes.Buffer
	out.WriteString("isvoid ")
	out.WriteString(iv.Expression.String())
	return out.String()
}

type DispatchExpression struct {
	Token      lexer.Token
	Object     Expression      // Can be nil for implicit self dispatch
	StaticType *TypeIdentifier // Can be nil for dynamic dispatch
	Method     *ObjectIdentifier
	Arguments  []Expression
}

func (de *DispatchExpression) TokenLiteral() string { return de.Token.Literal }
func (de *DispatchExpression) expressionNode()      {}
func (de *DispatchExpression) String() string {
	var out bytes.Buffer
	if de.Object != nil {
		out.WriteString(de.Object.String())
		if de.StaticType != nil {
			out.WriteString("@")
			out.WriteString(de.StaticType.String())
		}
		out.WriteString(".")
	}
	out.WriteString(de.Method.String())
	out.WriteString("(")
	for i, a := range de.Arguments {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(a.String())
	}
	out.WriteString(")")
	return out.String()
}

type Assignment struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Value Expression
}

func (a *Assignment) TokenLiteral() string { return a.Token.Literal }
func (a *Assignment) expressionNode()      {}
func (a *Assignment) String() string {
	var out bytes.Buffer
	out.WriteString(a.Name.String())
	out.WriteString(" <- ")
	out.WriteString(a.Value.String())
	return out.String()
}

// ArrayAccessExpression represents an array element access: array[index]
type ArrayAccessExpression struct {
	Token lexer.Token // The '[' token
	Array Expression  // The array expression
	Index Expression  // The index expression
}

func (aa *ArrayAccessExpression) expressionNode()      {}
func (aa *ArrayAccessExpression) TokenLiteral() string { return aa.Token.Literal }
func (aa *ArrayAccessExpression) String() string {
	var out bytes.Buffer
	out.WriteString(aa.Array.String())
	out.WriteString("[")
	out.WriteString(aa.Index.String())
	out.WriteString("]")
	return out.String()
}

// ArrayExpression represents an array initialization with size: new Array[Type](size)
type ArrayExpression struct {
	Token lexer.Token     // The 'new' token
	Type  *TypeIdentifier // Array type
	Size  Expression      // Size expression
}

func (ae *ArrayExpression) expressionNode()      {}
func (ae *ArrayExpression) TokenLiteral() string { return ae.Token.Literal }
func (ae *ArrayExpression) String() string {
	var out bytes.Buffer
	out.WriteString("new ")
	out.WriteString(ae.Type.String())
	out.WriteString("(")
	out.WriteString(ae.Size.String())
	out.WriteString(")")
	return out.String()
}



