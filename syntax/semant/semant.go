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

// buildd the inheritance graph
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

// Validates if the inheritance graph is a directed acyclic graph
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

// Validates presence of the main class and main method
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

// Checks if type1 conforms to type2 (i.e. type1 is a subtype of type2)
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

// Returns the list of errors encountered during semantic analysis
func (sa *SemanticAnalyser) Errors() []string {
	return sa.errors
}

func (sa *SemanticAnalyser) Analyze(program *ast.Program) {
	sa.initInheritanceGraph(program)
	sa.validateInheritanceGraph()

	sa.buildClassesSymboltables(program)
	sa.buildSymboltables(program)

	sa.validateMainClass()
	sa.typeCheck(program)
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
	case *ast.Assignment:
		return sa.GetAssignmentExpressionType(e, st)
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

	// Just like the case expression, we need to find the least upper bound of the types
	return sa.leastUpperBound(constype, alttype)
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
	// Create a new scope for this let expression
	letScope := NewSymbolTable(st)

	// Process each binding in order
	for _, binding := range le.Bindings {
		// First verify the declared type exists
		if binding.Type.Value != "SELF_TYPE" {
			if _, exists := sa.inheritanceGraph.classes[binding.Type.Value]; !exists {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"Let binding uses undefined type %s",
					binding.Type.Value))
				continue
			}
		}

		// If there's an initialization expression
		var initType string
		if binding.Init != nil {
			// Type check the initialization using the current scope
			// This allows init expressions to reference previous bindings
			initType = sa.getExpressionType(binding.Init, letScope)

			// Handle SELF_TYPE in initialization
			if initType == "SELF_TYPE" {
				initType = sa.currentClass
			}

			declaredType := binding.Type.Value
			if declaredType == "SELF_TYPE" {
				declaredType = sa.currentClass
			}

			// Check type conformance
			if !sa.isTypeConformant(initType, declaredType) {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"Let initialization type %s does not conform to declared type %s",
					initType, binding.Type.Value))
			}
		}

		// Add the binding to the scope for use by subsequent initializations
		// and the body expression
		letScope.AddEntry(binding.Identifier.Value, &SymbolEntry{
			Type:  binding.Type.Value,
			Token: binding.Identifier.Token,
		})
	}

	// Type check the body expression in the scope with all bindings
	return sa.getExpressionType(le.In, letScope)
}

// TO VERIFY
func (sa *SemanticAnalyser) GetAssignmentExpressionType(a *ast.Assignment, st *SymbolTable) string {
	// TODO: look for object in symbol table walking the scope and then check type
	entry, exists := st.Lookup(a.Name.Value)
	if !exists {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined identifier %s in assignment", a.Name.Value))
		return "Object"
	}

	// Get the type of the expression being assigned
	exprType := sa.getExpressionType(a.Value, st)

	// Special handling for SELF_TYPE
	if entry.Type == "SELF_TYPE" {
		if !sa.isTypeConformant(exprType, sa.currentClass) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Type %s of assigned expression does not conform to %s",
				exprType, entry.Type))
		}
		return exprType
	}

	// Check type conformance
	if !sa.isTypeConformant(exprType, entry.Type) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Type %s of assigned expression does not conform to identifier type %s",
			exprType, entry.Type))
		return entry.Type
	}

	return exprType
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
	rightType := sa.getExpressionType(be.Right, st)
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
	// First evaluate the type of the case expression and check for void
	exprType := sa.getExpressionType(ce.Expr, st)
	if exprType == "void" {
		sa.errors = append(sa.errors, "Case expression cannot be void")
		return "Object"
	}

	// Keep track of types seen in branches to detect duplicates
	seenTypes := make(map[string]bool)

	// Collect types from all branches
	branchTypes := make([]string, 0)

	for _, branch := range ce.Branches {
		// Check for duplicate types in branches
		if seenTypes[branch.Type.Value] {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Duplicate branch type %s in case expression",
				branch.Type.Value))
			continue
		}
		seenTypes[branch.Type.Value] = true

		// Verify the branch type exists
		if _, exists := sa.inheritanceGraph.classes[branch.Type.Value]; !exists {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Undefined type %s in case branch",
				branch.Type.Value))
			continue
		}

		// Create new scope for the branch
		branchST := NewSymbolTable(st)
		branchST.AddEntry(branch.Pattern.Value, &SymbolEntry{
			Type:  branch.Type.Value,
			Token: branch.Pattern.Token,
		})

		// Get type of branch expression
		branchType := sa.getExpressionType(branch.Expression, branchST)
		branchTypes = append(branchTypes, branchType)
	}

	// The type of a case expression is the least upper bound of all branch types
	if len(branchTypes) == 0 {
		return "Object"
	}

	// Find least upper bound of all branch types
	resultType := branchTypes[0]
	for i := 1; i < len(branchTypes); i++ {
		resultType = sa.leastUpperBound(resultType, branchTypes[i])
	}

	return resultType
}

func (sa *SemanticAnalyser) leastUpperBound(type1, type2 string) string {
	// If either type is SELF_TYPE, use the current class
	if type1 == "SELF_TYPE" {
		type1 = sa.currentClass
	}
	if type2 == "SELF_TYPE" {
		type2 = sa.currentClass
	}

	// If types are the same, that's the LUB
	if type1 == type2 {
		return type1
	}

	// Walk up the inheritance tree of both types until finding a common ancestor
	ancestors1 := sa.getAncestors(type1)
	ancestors2 := sa.getAncestors(type2)

	// Find first common ancestor
	for _, t1 := range ancestors1 {
		for _, t2 := range ancestors2 {
			if t1 == t2 {
				return t1
			}
		}
	}

	return "Object"
}

// Helper function to get all ancestors of a type including itself
func (sa *SemanticAnalyser) getAncestors(typeName string) []string {
	ancestors := make([]string, 0)
	current := typeName

	for current != "" {
		ancestors = append(ancestors, current)
		if node, exists := sa.inheritanceGraph.classes[current]; exists {
			current = node.parent
		} else {
			break
		}
	}

	return ancestors
}

// Implement dispatch expression type checking
func (sa *SemanticAnalyser) GetDispatchExpressionType(de *ast.DispatchExpression, st *SymbolTable) string {
	// Get type of object being dispatched on
	var objectType string
	if de.Object != nil {
		objectType = sa.getExpressionType(de.Object, st)
	} else {
		objectType = sa.currentClass // Implicit self
	}

	// Handle static dispatch
	if de.StaticType != nil {
		if !sa.isTypeConformant(objectType, de.StaticType.Value) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Expression of type %s does not conform to declared static dispatch type %s",
				objectType, de.StaticType.Value))
			return "Object"
		}
		objectType = de.StaticType.Value
	}

	// Look up the method in the class
	classNode, exists := sa.inheritanceGraph.classes[objectType]
	if !exists {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Dispatch on undefined type %s",
			objectType))
		return "Object"
	}

	method, exists := classNode.methods[de.Method.Value]
	if !exists {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Undefined method %s in type %s",
			de.Method.Value, objectType))
		return "Object"
	}

	// Check number of arguments
	if len(de.Arguments) != len(method.formals) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"Method %s called with wrong number of arguments. Expected %d, got %d",
			de.Method.Value, len(method.formals), len(de.Arguments)))
		return "Object"
	}

	// Type check arguments
	for i, arg := range de.Arguments {
		argType := sa.getExpressionType(arg, st)
		if !sa.isTypeConformant(argType, method.formals[i].TypeDecl.Value) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Argument %d of type %s does not conform to formal parameter type %s",
				i+1, argType, method.formals[i].TypeDecl.Value))
		}
	}

	// Handle SELF_TYPE in return type
	if method.retType == "SELF_TYPE" {
		return objectType
	}

	return method.retType
}
