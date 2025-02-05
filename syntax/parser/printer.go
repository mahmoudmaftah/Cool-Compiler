package parser

import (
	"cool-compiler/ast"
	"fmt"
	"strings"
)

type Printer struct {
	depth int
}

func NewPrinter() *Printer {
	return &Printer{depth: 0}
}

func (p *Printer) indent() string {
	return strings.Repeat("  ", p.depth)
}

func (p *Printer) PrintProgram(program *ast.Program) string {
	var sb strings.Builder
	sb.WriteString("Program\n")
	p.depth++

	for _, class := range program.Classes {
		sb.WriteString(p.PrintClass(class))
	}

	return sb.String()
}

func (p *Printer) PrintClass(class *ast.Class) string {
	var sb strings.Builder
	sb.WriteString(p.indent())
	sb.WriteString("Class: " + class.Name.Value)
	if class.Parent != nil {
		sb.WriteString(" inherits " + class.Parent.Value)
	}
	sb.WriteString("\n")

	p.depth++
	for _, feature := range class.Features {
		sb.WriteString(p.PrintFeature(feature))
	}
	p.depth--

	return sb.String()
}

func (p *Printer) PrintFeature(feature ast.Feature) string {
	switch f := feature.(type) {
	case *ast.Method:
		return p.PrintMethod(f)
	case *ast.Attribute:
		return p.PrintAttribute(f)
	default:
		return p.indent() + "Unknown feature type\n"
	}
}

func (p *Printer) PrintMethod(method *ast.Method) string {
	var sb strings.Builder
	sb.WriteString(p.indent())
	sb.WriteString("Method: " + method.Name.Value + "\n")

	p.depth++

	sb.WriteString(p.indent())
	sb.WriteString("Formals:\n")
	p.depth++
	for _, formal := range method.Formals {
		sb.WriteString(p.indent())
		sb.WriteString(fmt.Sprintf("%s: %s\n", formal.Name.Value, formal.TypeDecl.Value))
	}
	p.depth--

	sb.WriteString(p.indent())
	sb.WriteString("Return Type: " + method.TypeDecl.Value + "\n")

	sb.WriteString(p.indent())
	sb.WriteString("Body:\n")
	p.depth++
	sb.WriteString(p.PrintExpression(method.Body))
	p.depth--

	p.depth--
	return sb.String()
}

func (p *Printer) PrintAttribute(attr *ast.Attribute) string {
	var sb strings.Builder
	sb.WriteString(p.indent())
	sb.WriteString("Attribute: " + attr.Name.Value + " : " + attr.TypeDecl.Value + "\n")

	if attr.Init != nil {
		p.depth++
		sb.WriteString(p.indent())
		sb.WriteString("Init:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(attr.Init))
		p.depth--
		p.depth--
	}

	return sb.String()
}

func (p *Printer) PrintExpression(expr ast.Expression) string {
	if expr == nil {
		return p.indent() + "<nil>\n"
	}

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return p.indent() + fmt.Sprintf("Int: %d\n", e.Value)
	case *ast.StringLiteral:
		return p.indent() + fmt.Sprintf("String: %q\n", e.Value)
	case *ast.BooleanLiteral:
		return p.indent() + fmt.Sprintf("Bool: %v\n", e.Value)
	case *ast.ObjectIdentifier:
		return p.indent() + fmt.Sprintf("Identifier: %s\n", e.Value)
	case *ast.UnaryExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Unary: " + e.Operator + "\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Right))
		p.depth--
		return sb.String()
	case *ast.BinaryExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Binary: " + e.Operator + "\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Left))
		sb.WriteString(p.PrintExpression(e.Right))
		p.depth--
		return sb.String()
	case *ast.IfExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("If\n")
		p.depth++
		sb.WriteString(p.indent() + "Condition:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Condition))
		p.depth--
		sb.WriteString(p.indent() + "Then:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Consequence))
		p.depth--
		sb.WriteString(p.indent() + "Else:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Alternative))
		p.depth--
		p.depth--
		return sb.String()
	case *ast.WhileExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("While\n")
		p.depth++
		sb.WriteString(p.indent() + "Condition:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Condition))
		p.depth--
		sb.WriteString(p.indent() + "Body:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Body))
		p.depth--
		p.depth--
		return sb.String()
	case *ast.BlockExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Block\n")
		p.depth++
		for _, expr := range e.Expressions {
			sb.WriteString(p.PrintExpression(expr))
		}
		p.depth--
		return sb.String()
	case *ast.LetExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Let\n")
		p.depth++
		for _, binding := range e.Bindings {
			sb.WriteString(p.indent())
			sb.WriteString(fmt.Sprintf("Binding: %s: %s\n", binding.Identifier.Value, binding.Type.Value))
			if binding.Init != nil {
				p.depth++
				sb.WriteString(p.indent() + "Init:\n")
				p.depth++
				sb.WriteString(p.PrintExpression(binding.Init))
				p.depth--
				p.depth--
			}
		}
		sb.WriteString(p.indent() + "In:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.In))
		p.depth--
		p.depth--
		return sb.String()
	case *ast.CaseExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Case\n")
		p.depth++
		sb.WriteString(p.indent() + "Expression:\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Expr))
		p.depth--
		sb.WriteString(p.indent() + "Branches:\n")
		p.depth++
		for _, branch := range e.Branches {
			sb.WriteString(p.indent())
			sb.WriteString(fmt.Sprintf("Branch %s: %s =>\n", branch.Pattern.Value, branch.Type.Value))
			p.depth++
			sb.WriteString(p.PrintExpression(branch.Expression))
			p.depth--
		}
		p.depth--
		p.depth--
		return sb.String()
	case *ast.NewExpression:
		return p.indent() + fmt.Sprintf("New: %s\n", e.Type.Value)
	case *ast.IsVoidExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("IsVoid\n")
		p.depth++
		sb.WriteString(p.PrintExpression(e.Expression))
		p.depth--
		return sb.String()
	case *ast.DispatchExpression:
		var sb strings.Builder
		sb.WriteString(p.indent())
		sb.WriteString("Dispatch:\n")
		p.depth++

		if e.Object != nil {
			sb.WriteString(p.indent())
			sb.WriteString("Object:\n")
			p.depth++
			sb.WriteString(p.PrintExpression(e.Object))
			p.depth--
		}

		if e.StaticType != nil {
			sb.WriteString(p.indent())
			sb.WriteString("Static Type: " + e.StaticType.Value + "\n")
		}

		sb.WriteString(p.indent())
		sb.WriteString("Method: " + e.Method.Value + "\n")

		if len(e.Arguments) > 0 {
			sb.WriteString(p.indent())
			sb.WriteString("Arguments:\n")
			p.depth++
			for _, arg := range e.Arguments {
				sb.WriteString(p.PrintExpression(arg))
			}
			p.depth--
		}

		p.depth--
		return sb.String()
	default:
		return p.indent() + fmt.Sprintf("Unknown expression type: %T\n", expr)
	}
}
