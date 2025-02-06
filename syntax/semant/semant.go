package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

// For the semant we will use feature interface for (semantic analysis)
type Feature interface {
	GetName() string
	GetType() string
}

type Method struct {
	name    string
	retType string
	formals []*ast.Formal
	body    ast.Expression
	astNode *ast.Method
}

func (m *Method) GetName() string { return m.name }
func (m *Method) GetType() string { return m.retType }

type Attribute struct {
	name     string
	attrType string
	init     ast.Expression
	astNode  *ast.Attribute
}

func (a *Attribute) GetName() string { return a.name }
func (a *Attribute) GetType() string { return a.attrType }

type ClassNode struct {
	name     string
	parent   string
	features map[string]Feature
	methods  map[string]*Method
	attrs    map[string]*Attribute
}

type InheritanceGraph struct {
	classes map[string]*ClassNode
	roots   []string // Classes that inherit directly from Object
}

type SymbolTable struct {
	symbols map[string]*SymbolEntry
	parent  *SymbolTable
}

type SymbolEntry struct {
	Type     string
	Token    lexer.Token
	AttrType *ast.TypeIdentifier
	Method   *ast.Method
	Scope    *SymbolTable
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		symbols: make(map[string]*SymbolEntry),
		parent:  parent,
	}
}

func (st *SymbolTable) AddEntry(name string, entry *SymbolEntry) {
	st.symbols[name] = entry
}

func (st *SymbolTable) Lookup(name string) (*SymbolEntry, bool) {
	entry, ok := st.symbols[name]
	if !ok && st.parent != nil {
		return st.parent.Lookup(name)
	}
	return entry, ok
}

type SemanticAnalyser struct {
	globalSymbolTable *SymbolTable
	errors            []string
	inheritanceGraph  *InheritanceGraph
	currentClass      string
}

func NewSemanticAnalyser() *SemanticAnalyser {
	return &SemanticAnalyser{
		globalSymbolTable: NewSymbolTable(nil),
		inheritanceGraph: &InheritanceGraph{
			classes: make(map[string]*ClassNode),
			roots:   []string{"Object"},
		},
		errors: []string{},
	}
}

func (sa *SemanticAnalyser) initInheritanceGraph(program *ast.Program) {
	// Add basic classes (Object)
	sa.inheritanceGraph.classes["Object"] = &ClassNode{
		name:     "Object",
		parent:   "",
		features: make(map[string]Feature),
		methods:  make(map[string]*Method),
		attrs:    make(map[string]*Attribute),
	}

	// Add other basic classes (IO, Int, String, Bool)
	basicClasses := []string{"IO", "Int", "String", "Bool"}
	for _, className := range basicClasses {
		sa.inheritanceGraph.classes[className] = &ClassNode{
			name:     className,
			parent:   "Object",
			features: make(map[string]Feature),
			methods:  make(map[string]*Method),
			attrs:    make(map[string]*Attribute),
		}
	}

	// Add user-defined classes
	for _, class := range program.Classes {
		parentName := "Object"
		if class.Parent != nil {
			parentName = class.Parent.Value
		}

		classNode := &ClassNode{
			name:     class.Name.Value,
			parent:   parentName,
			features: make(map[string]Feature),
			methods:  make(map[string]*Method),
			attrs:    make(map[string]*Attribute),
		}

		// Add features to the class node
		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Method:
				method := &Method{
					name:    f.Name.Value,
					retType: f.TypeDecl.Value,
					formals: f.Formals,
					body:    f.Body,
					astNode: f,
				}
				classNode.features[method.name] = method
				classNode.methods[method.name] = method
			case *ast.Attribute:
				attr := &Attribute{
					name:     f.Name.Value,
					attrType: f.TypeDecl.Value,
					init:     f.Init,
					astNode:  f,
				}
				classNode.features[attr.name] = attr
				classNode.attrs[attr.name] = attr
			}
		}

		sa.inheritanceGraph.classes[class.Name.Value] = classNode
		if parentName == "Object" {
			sa.inheritanceGraph.roots = append(sa.inheritanceGraph.roots, class.Name.Value)
		}
	}
}

func (sa *SemanticAnalyser) validateInheritanceGraph() {
	visited := make(map[string]bool)
	temporary := make(map[string]bool)

	var detectCycle func(className string) bool
	detectCycle = func(className string) bool {
		if temporary[className] {
			sa.errors = append(sa.errors, fmt.Sprintf("Inheritance cycle detected involving class %s", className))
			return true
		}
		if visited[className] {
			return false
		}

		temporary[className] = true
		node := sa.inheritanceGraph.classes[className]
		if node.parent != "" && detectCycle(node.parent) {
			return true
		}
		delete(temporary, className)
		visited[className] = true
		return false
	}

	for className := range sa.inheritanceGraph.classes {
		if !visited[className] {
			detectCycle(className)
		}
	}
}

func (sa *SemanticAnalyser) validateMainClass() {
	mainClass, exists := sa.inheritanceGraph.classes["Main"]
	if !exists {
		sa.errors = append(sa.errors, "Program is missing Main class")
		return
	}

	mainMethod, exists := mainClass.methods["main"]
	if !exists {
		sa.errors = append(sa.errors, "Main class is missing main method")
		return
	}

	if len(mainMethod.formals) > 0 {
		sa.errors = append(sa.errors, "main method should not have any parameters")
	}
}

func (sa *SemanticAnalyser) isTypeConformant(type1, type2 string) bool {
	if type1 == type2 {
		return true
	}
	if type1 == "SELF_TYPE" {
		if type2 == "SELF_TYPE" {
			return true
		}
		type1 = sa.currentClass
	}
	if type2 == "SELF_TYPE" {
		type2 = sa.currentClass
	}

	// Walk the inheritance graph to check if type1 is a subtype of type2
	current := type1
	for current != "" {
		if current == type2 {
			return true
		}
		node, exists := sa.inheritanceGraph.classes[current]
		if !exists {
			return false
		}
		current = node.parent
	}
	return false

}

func (sa *SemanticAnalyser) Errors() []string {
	return sa.errors
}

func (sa *SemanticAnalyser) Analyze(program *ast.Program) {
	sa.buildClassesSymboltables(program)
	sa.buildSymboltables(program)
	sa.typeCheck(program)
}

func (sa *SemanticAnalyser) typeCheck(program *ast.Program) {
	for _, class := range program.Classes {
		st := sa.globalSymbolTable.symbols[class.Name.Value].Scope
		sa.typeCheckClass(class, st)
	}
}

func (sa *SemanticAnalyser) typeCheckClass(cls *ast.Class, st *SymbolTable) {
	for _, feature := range cls.Features {
		switch f := feature.(type) {
		case *ast.Attribute:
			sa.typeCheckAttribute(f, st)
		case *ast.Method:
			sa.typeCheckMethod(f, st)
		}
	}
}

func (sa *SemanticAnalyser) typeCheckAttribute(attribute *ast.Attribute, st *SymbolTable) {

	if _, exists := sa.inheritanceGraph.classes[attribute.TypeDecl.Value]; !exists {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Attribute %s has undefined type %s",
			attribute.Name.Value, attribute.TypeDecl.Value))
		return
	}

	if attribute.Init != nil {
		initType := sa.getExpressionType(attribute.Init, st)
		if !sa.isTypeConformant(initType, attribute.TypeDecl.Value) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Attribute %s initialization type %s does not conform to declared type %s",
				attribute.Name.Value, initType, attribute.TypeDecl.Value))
		}
	}

}

func (sa *SemanticAnalyser) typeCheckMethod(method *ast.Method, st *SymbolTable) {
	methodSt := NewSymbolTable(st)

	// we will first check for duplicate formal parameters
	seenParams := make(map[string]bool)
	for _, formal := range method.Formals {
		if seenParams[formal.Name.Value] {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Formal parameter %s is multiply defined in method %s",
				formal.Name.Value, method.Name.Value))
			continue
		}
		seenParams[formal.Name.Value] = true

		// Check formal parameter type exists
		if _, exists := sa.inheritanceGraph.classes[formal.TypeDecl.Value]; !exists {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Formal parameter %s has undefined type %s in method %s",
				formal.Name.Value, formal.TypeDecl.Value, method.Name.Value))
			continue
		}

		// Add formal to method scope
		methodSt.AddEntry(formal.Name.Value, &SymbolEntry{
			Type:  formal.TypeDecl.Value,
			Token: formal.Token,
		})
	}

	// Check if return type exists
	if _, exists := sa.inheritanceGraph.classes[method.TypeDecl.Value]; !exists && method.TypeDecl.Value != "SELF_TYPE" {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Method %s has undefined return type %s",
			method.Name.Value, method.TypeDecl.Value))
		return
	}

	// Type check method body
	bodyType := sa.getExpressionType(method.Body, methodSt)

	// Handle SELF_TYPE in return type
	expectedType := method.TypeDecl.Value
	if expectedType == "SELF_TYPE" {
		expectedType = sa.currentClass
	}

	if !sa.isTypeConformant(bodyType, expectedType) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Method %s body type %s does not conform to declared return type %s",
			method.Name.Value, bodyType, method.TypeDecl.Value))
	}
}

func (sa *SemanticAnalyser) getExpressionType(expression ast.Expression, st *SymbolTable) string {
	switch e := expression.(type) {
	case *ast.IntegerLiteral:
		return "Int"
	case *ast.StringLiteral:
		return "String"
	case *ast.BooleanLiteral:
		return "Bool"
	case *ast.BlockExpression:
		return sa.getBlockExpressionType(e, st)
	case *ast.IfExpression:
		return sa.getIfExpressionType(e, st)
	case *ast.WhileExpression:
		return sa.getWhileExpressionType(e, st)
	case *ast.NewExpression:
		return sa.GetNewExpressionType(e, st)
	case *ast.LetExpression:
		return sa.GetLetExpressionType(e, st)
	// case *ast.Assignment:
	// 	return sa.GetAssignmentExpressionType(e, st)
	case *ast.UnaryExpression:
		return sa.GetUnaryExpressionType(e, st)
	case *ast.BinaryExpression:
		return sa.GetBinaryExpressionType(e, st)
	case *ast.CaseExpression:
		return sa.GetCaseExpressionType(e, st)
	case *ast.IsVoidExpression:
		return "Bool"
	default:
		return "Object"
	}
}

func (sa *SemanticAnalyser) getWhileExpressionType(wexpr *ast.WhileExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(wexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
		return "Object"
	}

	return sa.getExpressionType(wexpr.Body, st)
}

func (sa *SemanticAnalyser) getBlockExpressionType(bexpr *ast.BlockExpression, st *SymbolTable) string {
	lastType := ""
	for _, expression := range bexpr.Expressions {
		lastType = sa.getExpressionType(expression, st)
	}

	return lastType
}

func (sa *SemanticAnalyser) getIfExpressionType(ifexpr *ast.IfExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(ifexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
		return "Object"
	}

	constype := sa.getExpressionType(ifexpr.Consequence, st)
	alttype := sa.getExpressionType(ifexpr.Alternative, st)

	if constype != alttype {
		sa.errors = append(sa.errors, fmt.Sprintf("ambiguous if statement return type %s vs %s", constype, alttype))
		return "Object"
	}

	return constype
}

func (sa *SemanticAnalyser) buildClassesSymboltables(program *ast.Program) {
	sa.globalSymbolTable.AddEntry("Object", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Object"}})
	sa.globalSymbolTable.AddEntry("Int", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Int"}})
	sa.globalSymbolTable.AddEntry("String", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "String"}})
	sa.globalSymbolTable.AddEntry("Bool", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Bool"}})

	for _, class := range program.Classes {
		if _, ok := sa.globalSymbolTable.Lookup(class.Name.Value); ok {
			sa.errors = append(sa.errors, fmt.Sprintf("class %s is already defined", class.Name.Value))
			continue
		}

		sa.globalSymbolTable.AddEntry(class.Name.Value, &SymbolEntry{Type: "Class", Token: class.Name.Token})
	}
}

func (sa *SemanticAnalyser) buildSymboltables(program *ast.Program) {
	for _, class := range program.Classes {
		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
		classEntry.Scope = NewSymbolTable(sa.globalSymbolTable)

		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Attribute:
				if _, ok := classEntry.Scope.Lookup(f.Name.Value); ok {
					sa.errors = append(sa.errors, fmt.Sprintf("attribute %s is already defined in class %s", f.Name.Value, class.Name.Value))
					continue
				}
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, AttrType: f.TypeDecl})
			case *ast.Method:
				methodST := NewSymbolTable(classEntry.Scope)
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, Scope: methodST, Method: f})
			}
		}
	}
}

func (sa *SemanticAnalyser) GetNewExpressionType(ne *ast.NewExpression, st *SymbolTable) string {
	// TODO: handle SELF_TYPE when implemented
	if _, ok := sa.globalSymbolTable.Lookup(ne.Type.Value); !ok {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in new expression", ne.Type.Value))
		return "Object"
	}
	return ne.Type.Value
}

func (sa *SemanticAnalyser) GetLetExpressionType(le *ast.LetExpression, st *SymbolTable) string {
	for _, b := range le.Bindings {
		sa.CheckBindingType(b, st)
	}
	return sa.getExpressionType(le.In, st)
}

func (sa *SemanticAnalyser) CheckBindingType(b *ast.Binding, st *SymbolTable) {
	exprType := sa.getExpressionType(b.Init, st)
	if exprType != b.Type.Value {
		sa.errors = append(sa.errors, fmt.Sprintf("Let binding with wrong type %s", exprType))
	}
}

func (sa *SemanticAnalyser) GetAssignmentExpressionType(a *ast.Assignment, st *SymbolTable) string {
	// TODO: look for object in symbol table walking the scope and then check type
	return ""
}

func (sa *SemanticAnalyser) GetUnaryExpressionType(uexpr *ast.UnaryExpression, st *SymbolTable) string {
	rightType := sa.getExpressionType(uexpr.Right, st)
	switch uexpr.Operator {
	case "~":
		if rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("bitwise negation on non-Int type: %s", rightType))
		}
		return "Int"
	case "not":
		if rightType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("logical negation on non-Bool type: %s", rightType))
		}
		return "Bool"
	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown unary operator %s", uexpr.Operator))
		return "Object"
	}
}

func isComparable(t string) bool {
	return t == "Int" || t == "Bool" || t == "String"
}

func (sa *SemanticAnalyser) GetBinaryExpressionType(be *ast.BinaryExpression, st *SymbolTable) string {
	leftType := sa.getExpressionType(be.Left, st)
	rightType := sa.getExpressionType(be.Left, st)
	switch be.Operator {
	case "+", "*", "/", "-":
		if leftType != "Int" || rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("arithmetic operation on non-Int types: %s %s %s", leftType, be.Operator, rightType))
		}
		return "Int"
	case "<", "<=", "=":
		if leftType != rightType || !isComparable(leftType) {
			sa.errors = append(sa.errors, fmt.Sprintf("comparison between incompatible types: %s %s %s", leftType, be.Operator, rightType))
		}
		return "Bool"
	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown binary operator %s", be.Operator))
		return "Object"
	}
}

func (sa *SemanticAnalyser) GetCaseExpressionType(ce *ast.CaseExpression, st *SymbolTable) string {
	// TODO: Handle case expression correctly. It requires returning the LCA type of all its sub expressions
	return "Object"
}
