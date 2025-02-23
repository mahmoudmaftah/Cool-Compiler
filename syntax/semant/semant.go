package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

const (
	SELF_TYPE = "SELF_TYPE"
	OBJECT    = "Object"
	IO        = "IO"
	INT       = "Int"
	STRING    = "String"
	BOOL      = "Bool"
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
	currentMethod     string
	variableInit      map[string]bool
	basicClasses      map[string]bool // Track basic classes that can't be inherited from
}

type MethodSignature struct {
	returnType string
	paramTypes []string
	class      string
}

func (sa *SemanticAnalyser) initBasicClassMethods() {
	// Object methods
	objectClass := sa.inheritanceGraph.classes[OBJECT]
	objectClass.methods["abort"] = &Method{
		name:    "abort",
		retType: OBJECT,
		formals: []*ast.Formal{},
	}
	objectClass.methods["type_name"] = &Method{
		name:    "type_name",
		retType: STRING,
		formals: []*ast.Formal{},
	}
	objectClass.methods["copy"] = &Method{
		name:    "copy",
		retType: SELF_TYPE,
		formals: []*ast.Formal{},
	}

	// IO methods
	ioClass := sa.inheritanceGraph.classes[IO]
	ioClass.methods["out_string"] = &Method{
		name:    "out_string",
		retType: SELF_TYPE,
		formals: []*ast.Formal{
			{TypeDecl: &ast.TypeIdentifier{Value: STRING}},
		},
	}
	ioClass.methods["out_int"] = &Method{
		name:    "out_int",
		retType: SELF_TYPE,
		formals: []*ast.Formal{
			{TypeDecl: &ast.TypeIdentifier{Value: INT}},
		},
	}
	ioClass.methods["in_string"] = &Method{
		name:    "in_string",
		retType: STRING,
		formals: []*ast.Formal{},
	}
	ioClass.methods["in_int"] = &Method{
		name:    "in_int",
		retType: INT,
		formals: []*ast.Formal{},
	}

	// String methods
	stringClass := sa.inheritanceGraph.classes[STRING]
	stringClass.methods["length"] = &Method{
		name:    "length",
		retType: INT,
		formals: []*ast.Formal{},
	}
	stringClass.methods["concat"] = &Method{
		name:    "concat",
		retType: STRING,
		formals: []*ast.Formal{
			{TypeDecl: &ast.TypeIdentifier{Value: STRING}},
		},
	}
	stringClass.methods["substr"] = &Method{
		name:    "substr",
		retType: STRING,
		formals: []*ast.Formal{
			{TypeDecl: &ast.TypeIdentifier{Value: INT}},
			{TypeDecl: &ast.TypeIdentifier{Value: INT}},
		},
	}
}

func NewSemanticAnalyser() *SemanticAnalyser {
	sa := &SemanticAnalyser{
		globalSymbolTable: NewSymbolTable(nil),
		inheritanceGraph: &InheritanceGraph{
			classes: make(map[string]*ClassNode),
			roots:   []string{OBJECT},
		},
		errors:       []string{},
		basicClasses: make(map[string]bool),
	}

	// Initialize basic classes
	sa.basicClasses[INT] = true
	sa.basicClasses[STRING] = true
	sa.basicClasses[BOOL] = true

	return sa
}

// helper function to get all ancestors of a type including itself
func (sa *SemanticAnalyser) getTypeAncestors(typeName string) []string {
	ancestors := []string{}
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

		// Check if parent exists
		if node.parent != "" {
			if _, exists := sa.inheritanceGraph.classes[node.parent]; !exists {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"Class %s inherits from undefined class %s",
					className, node.parent))
				return true
			}

			// Check inheritance from basic classes
			if sa.basicClasses[node.parent] {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"Class %s cannot inherit from %s",
					className, node.parent))
				return true
			}

			// Check if trying to redefine basic classes or IO
			if (className == INT || className == STRING || className == BOOL || className == IO) &&
				node.parent != OBJECT {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"Cannot redefine basic class %s",
					className))
				return true
			}

			if detectCycle(node.parent) {
				return true
			}
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

	// Invalid types should not conform
	if _, exists := sa.inheritanceGraph.classes[type1]; !exists {
		return false
	}
	if _, exists := sa.inheritanceGraph.classes[type2]; !exists {
		return false
	}

	// Handle SELF_TYPE special cases
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
	sa.initBasicClassMethods()

	sa.validateInheritanceGraph()

	sa.buildClassesSymboltables(program)
	sa.buildSymboltables(program)

	sa.validateMethodOverrides()

	sa.validateMainClass()
	sa.typeCheck(program)

	// Check initialization for each method
	for _, class := range program.Classes {
		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				initialized := make(map[string]bool)
				sa.checkInitialization(method.Body, classEntry.Scope, initialized)
			}
		}
	}
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

// func (sa *SemanticAnalyser) buildSymboltables(program *ast.Program) {
// 	for _, class := range program.Classes {
// 		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
// 		classEntry.Scope = NewSymbolTable(sa.globalSymbolTable)

// 		for _, feature := range class.Features {
// 			switch f := feature.(type) {
// 			case *ast.Attribute:
// 				if _, ok := classEntry.Scope.Lookup(f.Name.Value); ok {
// 					sa.errors = append(sa.errors, fmt.Sprintf("attribute %s is already defined in class %s", f.Name.Value, class.Name.Value))
// 					continue
// 				}
// 				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, AttrType: f.TypeDecl})
// 			case *ast.Method:
// 				methodST := NewSymbolTable(classEntry.Scope)
// 				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, Scope: methodST, Method: f})

// 				// list all expressions in the method body
// 				fmt.Println(f.Body)

// 			}
// 		}
// 	}
// }

// Enhance buildSymboltables to handle all scopes
func (sa *SemanticAnalyser) buildSymboltables(program *ast.Program) {
	// First pass: Build class level symbol tables
	for _, class := range program.Classes {
		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
		classEntry.Scope = NewSymbolTable(sa.globalSymbolTable)
		sa.currentClass = class.Name.Value

		// Add attributes
		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Attribute:
				if _, ok := classEntry.Scope.Lookup(f.Name.Value); ok {
					sa.errors = append(sa.errors, fmt.Sprintf(
						"attribute %s is already defined in class %s",
						f.Name.Value, class.Name.Value))
					continue
				}
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{
					Token:    f.Name.Token,
					AttrType: f.TypeDecl,
					Type:     f.TypeDecl.Value,
				})
			}
		}

		// Second pass: Build method symbol tables
		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Method:
				sa.currentMethod = f.Name.Value
				methodST := NewSymbolTable(classEntry.Scope)
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{
					Token:  f.Name.Token,
					Scope:  methodST,
					Method: f,
					Type:   f.TypeDecl.Value,
				})

				// Add formal parameters to method scope
				for _, formal := range f.Formals {
					methodST.AddEntry(formal.Name.Value, &SymbolEntry{
						Token: formal.Name.Token,
						Type:  formal.TypeDecl.Value,
					})
				}

				// Build scope for method body
				sa.buildExpressionScope(f.Body, methodST)
			}
		}
	}
}

// New method to build scopes for expressions
func (sa *SemanticAnalyser) buildExpressionScope(expr ast.Expression, parentScope *SymbolTable) *SymbolTable {
	switch e := expr.(type) {
	case *ast.LetExpression:
		letScope := NewSymbolTable(parentScope)
		for _, binding := range e.Bindings {
			letScope.AddEntry(binding.Identifier.Value, &SymbolEntry{
				Token: binding.Identifier.Token,
				Type:  binding.Type.Value,
			})
			if binding.Init != nil {
				sa.buildExpressionScope(binding.Init, letScope)
			}
		}
		sa.buildExpressionScope(e.In, letScope)
		return letScope

	case *ast.BlockExpression:
		blockScope := NewSymbolTable(parentScope)
		for _, expr := range e.Expressions {
			sa.buildExpressionScope(expr, blockScope)
		}
		return blockScope

	case *ast.CaseExpression:
		caseScope := NewSymbolTable(parentScope)
		sa.buildExpressionScope(e.Expr, caseScope)
		for _, branch := range e.Branches {
			branchScope := NewSymbolTable(caseScope)
			branchScope.AddEntry(branch.Pattern.Value, &SymbolEntry{
				Token: branch.Pattern.Token,
				Type:  branch.Type.Value,
			})
			sa.buildExpressionScope(branch.Expression, branchScope)
		}
		return caseScope

	case *ast.IfExpression:
		ifScope := NewSymbolTable(parentScope)
		sa.buildExpressionScope(e.Condition, ifScope)
		sa.buildExpressionScope(e.Consequence, ifScope)
		sa.buildExpressionScope(e.Alternative, ifScope)
		return ifScope

	case *ast.WhileExpression:
		whileScope := NewSymbolTable(parentScope)
		sa.buildExpressionScope(e.Condition, whileScope)
		sa.buildExpressionScope(e.Body, whileScope)
		return whileScope

	default:
		return parentScope
	}
}

// Add method override validation
func (sa *SemanticAnalyser) validateMethodOverrides() {
	for className, classNode := range sa.inheritanceGraph.classes {
		if classNode.parent == "" {
			continue
		}

		// Check if parent class exists
		_, exists := sa.inheritanceGraph.classes[classNode.parent]
		if !exists {
			// Parent class doesn't exist - error already reported in validateInheritanceGraph
			continue
		}

		// Check each method in the class
		for methodName, method := range classNode.methods {
			// Look for the same method in parent classes
			currentParent := classNode.parent
			for currentParent != "" {
				parentClass, exists := sa.inheritanceGraph.classes[currentParent]
				if !exists {
					break // Stop if we hit a non-existent class
				}

				if parentMethod, exists := parentClass.methods[methodName]; exists {
					// Validate method override
					if !sa.isValidMethodOverride(parentMethod, method) {
						sa.errors = append(sa.errors, fmt.Sprintf(
							"Invalid method override: %s.%s doesn't match parent class %s",
							className, methodName, currentParent))
					}
					break
				}
				currentParent = parentClass.parent
			}
		}
	}
}

// Helper method to validate method overrides
func (sa *SemanticAnalyser) isValidMethodOverride(parent, child *Method) bool {
	// Handle SELF_TYPE in return types
	parentRetType := sa.resolveSelfType(parent.retType)
	childRetType := sa.resolveSelfType(child.retType)

	// Return types must be exactly the same when resolved
	if parentRetType != childRetType {
		return false
	}

	// Check number of parameters
	if len(parent.formals) != len(child.formals) {
		return false
	}

	// Parameter types must match exactly
	for i := 0; i < len(parent.formals); i++ {
		// Check for nil pointers
		if parent.formals[i] == nil || parent.formals[i].TypeDecl == nil ||
			child.formals[i] == nil || child.formals[i].TypeDecl == nil {
			return false
		}

		parentParamType := sa.resolveSelfType(parent.formals[i].TypeDecl.Value)
		childParamType := sa.resolveSelfType(child.formals[i].TypeDecl.Value)

		// Verify both types exist in the class hierarchy
		if _, exists := sa.inheritanceGraph.classes[parentParamType]; !exists {
			return false
		}
		if _, exists := sa.inheritanceGraph.classes[childParamType]; !exists {
			return false
		}

		if parentParamType != childParamType {
			return false
		}
	}

	return true
}

func (sa *SemanticAnalyser) resolveSelfType(typeName string) string {
	if typeName == SELF_TYPE {
		return sa.currentClass
	}
	return typeName
}

// Add initialization checking
func (sa *SemanticAnalyser) checkInitialization(expr ast.Expression, st *SymbolTable, initialized map[string]bool) {
	switch e := expr.(type) {
	case *ast.ObjectIdentifier:
		// Check if variable is initialized before use
		if _, exists := initialized[e.Value]; !exists {
			if entry, ok := st.Lookup(e.Value); ok {
				if entry.AttrType == nil { // Not an attribute (which has default initialization)
					sa.errors = append(sa.errors, fmt.Sprintf(
						"Variable %s used before initialization",
						e.Value))
				}
			}
		}

	case *ast.Assignment:
		// Mark variable as initialized
		initialized[e.Name.Value] = true
		sa.checkInitialization(e.Value, st, initialized)

	case *ast.LetExpression:
		// Create new initialization map for let scope
		letInit := make(map[string]bool)
		for k, v := range initialized {
			letInit[k] = v
		}

		for _, binding := range e.Bindings {
			if binding.Init != nil {
				sa.checkInitialization(binding.Init, st, letInit)
				letInit[binding.Identifier.Value] = true
			}
		}
		sa.checkInitialization(e.In, st, letInit)

	// Add cases for other expression types...
	case *ast.BlockExpression:
		for _, expr := range e.Expressions {
			sa.checkInitialization(expr, st, initialized)
		}

	case *ast.IfExpression:
		sa.checkInitialization(e.Condition, st, initialized)

		// Track initialization in both branches
		thenInit := make(map[string]bool)
		elseInit := make(map[string]bool)
		for k, v := range initialized {
			thenInit[k] = v
			elseInit[k] = v
		}

		sa.checkInitialization(e.Consequence, st, thenInit)
		sa.checkInitialization(e.Alternative, st, elseInit)

		// Variable is only considered initialized if it's initialized in both branches
		for k := range initialized {
			initialized[k] = thenInit[k] && elseInit[k]
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
		fmt.Println(formal.Name.Value)
		if seenParams[formal.Name.Value] {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Formal parameter %s is multiply defined in method %s from class %s",
				formal.Name.Value, method.Name.Value, sa.currentClass))
			continue
		}
		seenParams[formal.Name.Value] = true

		// Check formal parameter type exists
		if _, exists := sa.inheritanceGraph.classes[formal.TypeDecl.Value]; !exists {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"Formal parameter %s has undefined type %s in method %s from class %s",
				formal.Name.Value, formal.TypeDecl.Value, method.Name.Value, sa.currentClass))
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

	// fmt.Println(method.Body)

	// print the method name and the body type
	// fmt.Println(method.Name.Value)
	// fmt.Println(bodyType)

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
	case *ast.DispatchExpression:
		return sa.GetDispatchExpressionType(e, st)
	case *ast.ObjectIdentifier:
		a, ok := st.Lookup(e.Value)
		if !ok {
			sa.errors = append(sa.errors, fmt.Sprintf("undefined identifier %s", e.Value))
			return "Object"
		}
		return a.Type
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
