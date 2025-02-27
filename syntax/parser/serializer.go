package parser

import (
	"cool-compiler/ast"
	"fmt"
	"strings"
)

func SerializeExpression(exp ast.Expression) string {
	if exp == nil {
		return ""
	}

	switch node := exp.(type) {
	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", node.Value)

	case *ast.StringLiteral:
		return fmt.Sprintf("\"%s\"", node.Value) // Changed from %q to explicitly use double quotes

	case *ast.BooleanLiteral:
		return fmt.Sprintf("%v", node.Value) // Changed from %t to %v to get "true"/"false" instead of "t"/"f"

	case *ast.ObjectIdentifier:
		return node.Value

	case *ast.UnaryExpression:
		return fmt.Sprintf("(%s %s)", node.Operator, SerializeExpression(node.Right))

	case *ast.BinaryExpression:
		return fmt.Sprintf("(%s %s %s)", SerializeExpression(node.Left), node.Operator, SerializeExpression(node.Right))

	case *ast.IfExpression:
		return fmt.Sprintf("if %s then %s else %s fi",
			SerializeExpression(node.Condition),
			SerializeExpression(node.Consequence),
			SerializeExpression(node.Alternative))

	case *ast.BlockExpression:
		exprs := make([]string, len(node.Expressions))
		for i, expr := range node.Expressions {
			exprs[i] = SerializeExpression(expr)
		}
		if len(exprs) > 0 {
			return fmt.Sprintf("{ %s }", strings.Join(exprs, "; "))
		}
		return "{ }"

	case *ast.LetExpression:
		bindings := make([]string, len(node.Bindings))
		for i, binding := range node.Bindings {
			if binding.Init != nil {
				bindings[i] = fmt.Sprintf("%s:%s<-%s",
					binding.Identifier.Value,
					binding.Type.Value,
					SerializeExpression(binding.Init))
			} else {
				bindings[i] = fmt.Sprintf("%s:%s",
					binding.Identifier.Value,
					binding.Type.Value)
			}
		}
		return fmt.Sprintf("let %s in %s",
			strings.Join(bindings, ","),
			SerializeExpression(node.In))

	case *ast.CaseExpression:
		branches := make([]string, len(node.Branches))
		for i, branch := range node.Branches {
			branches[i] = fmt.Sprintf("%s:%s=>%s",
				branch.Pattern.Value,
				branch.Type.Value,
				SerializeExpression(branch.Expression))
		}
		return fmt.Sprintf("case %s of %s esac",
			SerializeExpression(node.Expr),
			strings.Join(branches, "; "))

	case *ast.NewExpression:
		return fmt.Sprintf("new %s", node.Type.Value)

	case *ast.IsVoidExpression:
		return fmt.Sprintf("isvoid %s", SerializeExpression(node.Expression))

	case *ast.DispatchExpression:
		args := make([]string, len(node.Arguments))
		for i, arg := range node.Arguments {
			args[i] = SerializeExpression(arg)
		}

		if node.Object == nil {
			// Simple dispatch (implicit self)
			return fmt.Sprintf("%s(%s)",
				node.Method.Value,
				strings.Join(args, ", "))
		}

		if node.StaticType != nil {
			// Static dispatch
			return fmt.Sprintf("%s@%s.%s(%s)",
				SerializeExpression(node.Object),
				node.StaticType.Value,
				node.Method.Value,
				strings.Join(args, ", "))
		}

		// Normal dispatch
		return fmt.Sprintf("%s.%s(%s)",
			SerializeExpression(node.Object),
			node.Method.Value,
			strings.Join(args, ", "))

	case *ast.WhileExpression:
		return fmt.Sprintf("while %s loop %s pool", SerializeExpression(node.Condition), SerializeExpression(node.Body))

	case *ast.Assignment:
		// we have token, name and value
		return fmt.Sprintf("%s <- %s", node.Name.Value, SerializeExpression(node.Value))

	default:
		return fmt.Sprintf("unknown expression: %T", exp)
	}
}
