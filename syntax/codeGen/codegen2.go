package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"sort"
	"strings"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// CodeGenerator maintains the state needed during code generation
type CodeGenerator struct {
	// LLVM Module being generated
	Module *ir.Module

	// Current class and method context
	currentClass  string
	currentMethod string

	// Class structure and method mappings
	classTypes map[string]*types.StructType   // Maps class names to their LLVM struct types
	vtables    map[string]*types.StructType   // Maps class names to their vtable types
	methods    map[string]map[string]*ir.Func // Maps class.method to LLVM functions

	// Virtual method table support
	methodIndices map[string]map[string]int // Maps className -> methodName -> vtable index
	vtableGlobals map[string]*ir.Global     // Global variables holding vtable constants

	// Current function and block being generated
	currentFunc  *ir.Func
	currentBlock *ir.Block

	// Symbol tables for variables and attributes
	variables map[string]value.Value // Local variables in current scope
	attrs     map[string]int         // Maps attribute names to indices in class struct

	// Inheritance information - cached from semantic analysis
	classParents map[string]string

	// Runtime functions
	runtimeFuncs map[string]*ir.Func

	// Basic type cache
	intType    types.Type
	boolType   types.Type
	stringType types.Type
	voidType   types.Type
	objectType types.Type

	// Runtime type system
	typeIDMap map[string]int64 // Maps class names to type IDs

	// Array extension support
	arrayElementTypes map[string]types.Type // Maps array class types to their element types

	// Optimization level
	optimizationLevel int

	// Error handling
	errors []string
	cFuncs map[string]*ir.Func
}

// New creates a new, fully initialized code generator
func New() *CodeGenerator {
	cg := &CodeGenerator{
		Module:            ir.NewModule(),
		classTypes:        make(map[string]*types.StructType),
		vtables:           make(map[string]*types.StructType),
		methods:           make(map[string]map[string]*ir.Func),
		methodIndices:     make(map[string]map[string]int),
		vtableGlobals:     make(map[string]*ir.Global),
		variables:         make(map[string]value.Value),
		attrs:             make(map[string]int),
		classParents:      make(map[string]string),
		runtimeFuncs:      make(map[string]*ir.Func),
		cFuncs:            make(map[string]*ir.Func),
		typeIDMap:         make(map[string]int64),
		arrayElementTypes: make(map[string]types.Type),
		errors:            []string{},
	}

	// Initialize basic types
	cg.intType = types.I32
	cg.boolType = types.I1
	cg.stringType = types.NewPointer(types.I8) // Strings are char* in LLVM
	cg.voidType = types.Void

	// IMPORTANT: Do NOT set the target triple explicitly
	// Let LLVM/Clang determine the appropriate target for the current system
	// cg.Module.TargetTriple = "x86_64-unknown-linux-gnu" // Remove this line if it exists

	// Add data layout information that's generic enough for most systems
	cg.Module.DataLayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

	// Define basic classes and declare functions in the right order
	cg.defineBasicClasses()
	cg.declareStandardCFunctions()
	cg.declareRuntimeFunctions()
	cg.initializeBasicClasses()
	cg.initializeTypeSystem()

	return cg
}

// New function to declare standard C library functions once
func (cg *CodeGenerator) declareStandardCFunctions() {
	// printf - variadic function
	printfFunc := cg.Module.NewFunc("printf", types.I32,
		ir.NewParam("format", types.NewPointer(types.I8)))
	printfFunc.Sig.Variadic = true // Mark as variadic
	cg.cFuncs["printf"] = printfFunc

	// Remove the scanf declaration entirely

	// strlen
	cg.cFuncs["strlen"] = cg.Module.NewFunc("strlen", types.I64,
		ir.NewParam("s", types.NewPointer(types.I8)))

	// strcpy
	cg.cFuncs["strcpy"] = cg.Module.NewFunc("strcpy", types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)))

	// strcat
	cg.cFuncs["strcat"] = cg.Module.NewFunc("strcat", types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)))

	// strncpy
	cg.cFuncs["strncpy"] = cg.Module.NewFunc("strncpy", types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)),
		ir.NewParam("n", types.I64))

	// exit
	cg.cFuncs["exit"] = cg.Module.NewFunc("exit", types.Void,
		ir.NewParam("status", types.I32))

	// malloc
	cg.cFuncs["malloc"] = cg.Module.NewFunc("malloc", types.NewPointer(types.I8),
		ir.NewParam("size", types.I64))
}

// Main code generation phases (outline)
func (cg *CodeGenerator) Generate(program *ast.Program) (*ir.Module, error) {
	// Add declarations for necessary runtime functions and libraries
	// This tells the compiler these symbols will be available at link time

	// Mark all C standard functions with appropriate linkage
	for _, fn := range cg.cFuncs {
		fn.Linkage = enum.LinkageExternal
	}

	// Mark GC_malloc with appropriate linkage
	if fn, exists := cg.runtimeFuncs["GC_malloc"]; exists {
		fn.Linkage = enum.LinkageExternal
	}

	// Continue with the regular compilation phases
	if err := cg.createClassTypes(program); err != nil {
		return nil, fmt.Errorf("error creating class types: %v", err)
	}
	fmt.Println("Class types created")

	if err := cg.createMethodDeclarations(program); err != nil {
		return nil, fmt.Errorf("error creating method declarations: %v", err)
	}
	fmt.Println("Method declarations created")

	if err := cg.generateMethodImplementations(program); err != nil {
		return nil, fmt.Errorf("error generating method implementations: %v", err)
	}
	fmt.Println("Method implementations generated")

	if err := cg.generateEntryPoint(); err != nil {
		return nil, fmt.Errorf("error generating entry point: %v", err)
	}
	fmt.Println("Entry point generated")

	cg.generateRuntimeLibrary()
	fmt.Println("Runtime support generated")

	if err := cg.optimizeModule(); err != nil {
		return nil, fmt.Errorf("error optimizing module: %v", err)
	}
	fmt.Println("Module optimized")

	return cg.Module, nil
}

// Initialize the type system for runtime type checking
func (cg *CodeGenerator) initializeTypeSystem() {
	// Assign type IDs to basic classes
	cg.typeIDMap["Object"] = 0
	cg.typeIDMap["IO"] = 1
	cg.typeIDMap["Int"] = 2
	cg.typeIDMap["String"] = 3
	cg.typeIDMap["Bool"] = 4

	// Special type ID for void
	cg.typeIDMap[""] = -1
}

// Optimize the generated LLVM IR module
func (cg *CodeGenerator) optimizeModule() error {
	// In a real implementation, you would call LLVM's optimization passes
	// This is just a placeholder to complete the interface
	return nil
}

// Report an error during code generation
func (cg *CodeGenerator) reportError(format string, args ...interface{}) {
	cg.errors = append(cg.errors, fmt.Sprintf(format, args...))
}

// Get all errors encountered during code generation
func (cg *CodeGenerator) Errors() []string {
	return cg.errors
}

type methodInfo struct {
	name          string
	definingClass string
	index         int
}

// defineBasicClasses defines the base classes in the Cool language
func (cg *CodeGenerator) defineBasicClasses() {

	// Define Object class - base class for all Cool classes
	objectFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer (will be refined later)
	}
	cg.objectType = types.NewStruct(objectFields...)
	cg.classTypes["Object"] = types.NewStruct(objectFields...)
	cg.Module.NewTypeDef("Object", cg.classTypes["Object"])

	// Define Int class
	intFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer
		cg.intType,                 // value field
	}
	cg.classTypes["Int"] = types.NewStruct(intFields...)
	cg.Module.NewTypeDef("Int", cg.classTypes["Int"])

	// Define Bool class
	boolFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer
		cg.boolType,                // value field
	}
	cg.classTypes["Bool"] = types.NewStruct(boolFields...)
	cg.Module.NewTypeDef("Bool", cg.classTypes["Bool"])

	// Define String class
	stringFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer
		cg.stringType,              // pointer to string data
		cg.intType,                 // length field
	}
	cg.classTypes["String"] = types.NewStruct(stringFields...)
	cg.Module.NewTypeDef("String", cg.classTypes["String"])

	// Define IO class
	ioFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer
	}
	cg.classTypes["IO"] = types.NewStruct(ioFields...)
	cg.Module.NewTypeDef("IO", cg.classTypes["IO"])

	// Set up class parents
	cg.classParents["Int"] = "Object"
	cg.classParents["Bool"] = "Object"
	cg.classParents["String"] = "Object"
	cg.classParents["IO"] = "Object"
}

// declareRuntimeFunctions declares the runtime support functions
func (cg *CodeGenerator) declareRuntimeFunctions() {
	// Memory allocation function (GC_malloc)
	gcMallocType := types.NewPointer(types.I8)
	gcMalloc := cg.Module.NewFunc(
		"GC_malloc",
		gcMallocType,
		ir.NewParam("size", types.I64),
	)
	cg.runtimeFuncs["GC_malloc"] = gcMalloc

	// String operations
	strCat := cg.Module.NewFunc(
		"string_concat",
		cg.stringType,
		ir.NewParam("s1", cg.stringType),
		ir.NewParam("s2", cg.stringType),
	)
	cg.runtimeFuncs["string_concat"] = strCat

	strLen := cg.Module.NewFunc(
		"string_length",
		cg.intType,
		ir.NewParam("s", cg.stringType),
	)
	cg.runtimeFuncs["string_length"] = strLen

	strSubstr := cg.Module.NewFunc(
		"string_substr",
		cg.stringType,
		ir.NewParam("s", cg.stringType),
		ir.NewParam("i", cg.intType),
		ir.NewParam("l", cg.intType),
	)
	cg.runtimeFuncs["string_substr"] = strSubstr

	// I/O operations
	outString := cg.Module.NewFunc(
		"IO_out_string",
		types.NewPointer(cg.classTypes["IO"]),
		ir.NewParam("self", types.NewPointer(cg.classTypes["IO"])),
		ir.NewParam("str", cg.stringType),
	)
	cg.runtimeFuncs["IO_out_string"] = outString

	outInt := cg.Module.NewFunc(
		"IO_out_int",
		types.NewPointer(cg.classTypes["IO"]),
		ir.NewParam("self", types.NewPointer(cg.classTypes["IO"])),
		ir.NewParam("i", cg.intType),
	)
	cg.runtimeFuncs["IO_out_int"] = outInt

	inString := cg.Module.NewFunc(
		"IO_in_string",
		cg.stringType,
		ir.NewParam("self", types.NewPointer(cg.classTypes["IO"])),
	)
	cg.runtimeFuncs["IO_in_string"] = inString

	inInt := cg.Module.NewFunc(
		"IO_in_int",
		cg.intType,
		ir.NewParam("self", types.NewPointer(cg.classTypes["IO"])),
	)
	cg.runtimeFuncs["IO_in_int"] = inInt
}

// createClassTypes creates LLVM type definitions for all classes in the program
func (cg *CodeGenerator) createClassTypes(program *ast.Program) error {
	// First pass: create forward declarations for all classes
	for _, class := range program.Classes {
		className := class.Name.Value

		// Skip if already defined (basic classes)
		if _, exists := cg.classTypes[className]; exists {
			continue
		}

		// Create a forward declaration (will be filled in later)
		cg.classTypes[className] = types.NewStruct()
		cg.Module.NewTypeDef(className, cg.classTypes[className])

		// Record inheritance information
		if class.Parent != nil {
			cg.classParents[className] = class.Parent.Value
		} else {
			cg.classParents[className] = "Object" // Default parent
		}
	}

	// Second pass: fill in class structures with fields
	for _, class := range program.Classes {
		className := class.Name.Value

		// Skip basic classes (already defined)
		if className == "Object" || className == "Int" ||
			className == "Bool" || className == "String" ||
			className == "IO" {
			continue
		}

		err := cg.fillClassType(className, class)
		if err != nil {
			return err
		}
	}

	return nil
}

// fillClassType fills in the struct type for a class with its fields
func (cg *CodeGenerator) fillClassType(className string, class *ast.Class) error {
	structType := cg.classTypes[className]

	// Get parent class
	parentName := cg.classParents[className]
	parentType, exists := cg.classTypes[parentName]
	if !exists {
		return fmt.Errorf("parent class %s not found for %s", parentName, className)
	}

	// Collect all fields, starting with vtable pointer
	fields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer (first field)
	}

	// Get parent attributes (excluding vtable pointer which we already added)
	// We need to maintain the same field layout as the parent
	parentFields := parentType.Fields[1:] // Skip vtable pointer
	fields = append(fields, parentFields...)

	// Add fields for attributes in this class
	attrIndex := len(fields) // Start after parent fields

	// Process all features to find attributes
	for _, feature := range class.Features {
		if attr, ok := feature.(*ast.Attribute); ok {
			attrName := attr.Name.Value
			attrType, err := cg.mapType(attr.TypeDecl.Value)
			if err != nil {
				return fmt.Errorf("error mapping type for attribute %s: %v", attrName, err)
			}

			fields = append(fields, attrType)
			cg.attrs[className+"."+attrName] = attrIndex
			attrIndex++
		}
	}

	// Update the struct type with the fields
	structType.Fields = fields

	return nil
}

// createMethodDeclarations creates LLVM function declarations for all methods
func (cg *CodeGenerator) createMethodDeclarations(program *ast.Program) error {
	// First pass: create vtable types for each class (forward declaration)
	for className, _ := range cg.classTypes {
		cg.vtables[className] = types.NewStruct()
		cg.Module.NewTypeDef(className+"_vtable", cg.vtables[className])
	}

	// Second pass: create method declarations
	for _, class := range program.Classes {
		className := class.Name.Value

		// Initialize method map for this class if not exists
		if _, exists := cg.methods[className]; !exists {
			cg.methods[className] = make(map[string]*ir.Func)
		}

		// Create declarations for all methods in this class
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := method.Name.Value

				// Create function declaration
				err := cg.createMethodDeclaration(className, method)
				if err != nil {
					return fmt.Errorf("error creating method declaration for %s.%s: %v",
						className, methodName, err)
				}
			}
		}
	}

	// Third pass: fill in vtable types with method pointers
	for className, _ := range cg.classTypes {
		// Get all methods for this class (including inherited ones)
		methodList := cg.collectClassMethods(className)

		// Create vtable fields (function pointers for each method)
		vtableFields := []types.Type{}
		for _, methodInfo := range methodList {
			// Add function pointer to vtable
			methodType := cg.methods[methodInfo.definingClass][methodInfo.name].Type()
			vtableFields = append(vtableFields, methodType)
		}

		// Update vtable type with fields
		cg.vtables[className].Fields = vtableFields

		// Create a global constant for the vtable
		cg.createVTableConstant(className, methodList)
	}

	return nil
}

func (cg *CodeGenerator) collectClassMethods(className string) []methodInfo {
	methodList := []methodInfo{}
	methodMap := make(map[string]methodInfo)

	// Start with the className
	cg.collectParentMethods(className, methodMap)

	// Convert map to ordered list
	for methodName, info := range methodMap {
		info.index = len(methodList)
		methodMap[methodName] = info
		methodList = append(methodList, info)
	}

	// Store method indices for dispatch lookup
	cg.storeMethodIndices(className, methodMap)

	return methodList
}

func (cg *CodeGenerator) collectParentMethods(className string, methodMap map[string]methodInfo) {
	// Recursively process parent classes first (so child methods override parent methods)
	parentName, hasParent := cg.classParents[className]
	if hasParent && parentName != "" {
		cg.collectParentMethods(parentName, methodMap)
	}

	// Now add methods from this class, potentially overriding any parent methods
	if methodsByName, exists := cg.methods[className]; exists {
		for methodName := range methodsByName {
			// Only add if not already present (child overrides parent)
			methodMap[methodName] = methodInfo{
				name:          methodName,
				definingClass: className,
			}
		}
	}
}

func (cg *CodeGenerator) initializeIOClass() {
	// Make sure IO class has a methods map
	if _, exists := cg.methods["IO"]; !exists {
		cg.methods["IO"] = make(map[string]*ir.Func)
	}

	// Register the IO methods
	cg.methods["IO"]["out_string"] = cg.runtimeFuncs["IO_out_string"]
	cg.methods["IO"]["out_int"] = cg.runtimeFuncs["IO_out_int"]
	cg.methods["IO"]["in_string"] = cg.runtimeFuncs["IO_in_string"]
	cg.methods["IO"]["in_int"] = cg.runtimeFuncs["IO_in_int"]
}

func (cg *CodeGenerator) storeMethodIndices(className string, methodMap map[string]methodInfo) {
	// Create method index map if not exists
	if cg.methodIndices == nil {
		cg.methodIndices = make(map[string]map[string]int)
	}

	// Initialize for this class
	cg.methodIndices[className] = make(map[string]int)

	// Store indices
	for methodName, info := range methodMap {
		cg.methodIndices[className][methodName] = info.index
	}
}

// createVTableConstant creates a global constant for the vtable
func (cg *CodeGenerator) createVTableConstant(className string, methods []methodInfo) {
	// Create constant initializers for each vtable slot
	initializers := []constant.Constant{}

	for _, methodInfo := range methods {
		// Get the method function
		method := cg.methods[methodInfo.definingClass][methodInfo.name]

		// Add function pointer to initializers
		initializers = append(initializers, method)
	}

	// Create a global constant for the vtable
	vtableType := cg.vtables[className]
	vtableInit := constant.NewStruct(vtableType, initializers...)
	vtableGlobal := cg.Module.NewGlobalDef(className+"_vtable_instance", vtableInit)

	// Store vtable global for use during object creation
	if cg.vtableGlobals == nil {
		cg.vtableGlobals = make(map[string]*ir.Global)
	}
	cg.vtableGlobals[className] = vtableGlobal
}

// createMethodDeclaration creates an LLVM function declaration for a method
func (cg *CodeGenerator) createMethodDeclaration(className string, method *ast.Method) error {
	methodName := method.Name.Value

	// Create parameter types
	paramTypes := []types.Type{
		types.NewPointer(cg.classTypes[className]), // 'self' is always the first parameter
	}

	params := []*ir.Param{
		ir.NewParam("self", types.NewPointer(cg.classTypes[className])),
	}

	// Add other parameters
	for _, formal := range method.Formals {
		paramType, err := cg.mapType(formal.TypeDecl.Value)
		if err != nil {
			return fmt.Errorf("error mapping type for parameter %s: %v",
				formal.Name.Value, err)
		}

		paramTypes = append(paramTypes, paramType)
		params = append(params, ir.NewParam(formal.Name.Value, paramType))
	}

	// // Map return type
	// returnType, err := cg.mapType(method.TypeDecl.Value)
	// if err != nil {
	// 	return fmt.Errorf("error mapping return type: %v", err)
	// }

	// // Check for SELF_TYPE
	// if method.TypeDecl.Value == "SELF_TYPE" {
	// 	returnType = types.NewPointer(cg.classTypes[className])
	// }

	// Map return type (special handling for SELF_TYPE)
	var returnType types.Type
	var err error

	if method.TypeDecl.Value == "SELF_TYPE" {
		// For SELF_TYPE, use a pointer to the current class
		returnType = types.NewPointer(cg.classTypes[className])
	} else {
		returnType, err = cg.mapType(method.TypeDecl.Value)
		if err != nil {
			return fmt.Errorf("error mapping return type: %v", err)
		}
	}

	// Create function
	funcName := fmt.Sprintf("%s_%s", className, methodName)
	function := cg.Module.NewFunc(funcName, returnType, params...)

	// Store in methods map
	cg.methods[className][methodName] = function

	return nil
}

// generateMethodImplementations generates the LLVM IR for method bodies
func (cg *CodeGenerator) generateMethodImplementations(program *ast.Program) error {
	for _, class := range program.Classes {
		className := class.Name.Value

		// Skip basic classes for now - they'll need runtime implementation
		if className == "Object" || className == "Int" ||
			className == "Bool" || className == "String" ||
			className == "IO" {
			continue
		}

		fmt.Println("STARTING ...")

		// Generate method implementations
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				// print method name
				fmt.Println("Method Name: ", method.Name.Value)
				err := cg.generateMethodImplementation(className, method)
				if err != nil {
					return fmt.Errorf("error generating method implementation for %s.%s: %v",
						className, method.Name.Value, err)
				}
			}

		}
	}

	return nil
}

func (cg *CodeGenerator) generateDispatch(dispatch *ast.DispatchExpression) (value.Value, error) {
	// Generate code for the object
	var objPtr value.Value
	var err error
	var staticClass string

	if dispatch.Object != nil {
		objPtr, err = cg.generateExpression(dispatch.Object)
		if err != nil {
			return nil, fmt.Errorf("error generating dispatch object: %v", err)
		}

		// Try to determine static type - use current class as a fallback
		staticClass = cg.currentClass

		// For a more advanced implementation, we would look at the type from semantic analysis
		if ptrType, ok := objPtr.Type().(*types.PointerType); ok {
			if structType, ok := ptrType.ElemType.(*types.StructType); ok {
				// Try to find the class name from the struct type
				for className, classType := range cg.classTypes {
					if classType == structType {
						staticClass = className
						break
					}
				}
			}
		}
	} else {
		// Self dispatch
		objPtr = cg.variables["self"]
		staticClass = cg.currentClass
	}

	// Find method to be called
	methodName := dispatch.Method.Value

	// Check if we're doing a static dispatch (@Type.method)
	var methodDefiningClass string
	if dispatch.StaticType != nil {
		methodDefiningClass = dispatch.StaticType.Value
	} else {
		// Find which class in the inheritance hierarchy defines this method
		methodDefiningClass = cg.findMethodDefiningClass(staticClass, methodName)
	}

	// Find the method
	methodFunc := cg.findMethod(methodDefiningClass, methodName)
	if methodFunc == nil {
		return nil, fmt.Errorf("method %s not found in class %s or any ancestor", methodName, methodDefiningClass)
	}

	// SIMPLIFIED APPROACH: Do the null check in the current block
	// Only create new blocks for error handling if needed
	var callResult value.Value

	// Start with the non-null path directly in the current block
	// Cast objPtr to the correct type expected by the method
	expectedType := methodFunc.Params[0].Type()
	castedObj := objPtr
	if objPtr.Type() != expectedType {
		castedObj = cg.currentBlock.NewBitCast(objPtr, expectedType)
	}

	// Add casted object as first arg
	args := []value.Value{castedObj}

	// Add rest of arguments
	for _, arg := range dispatch.Arguments {
		argValue, err := cg.generateExpression(arg)
		if err != nil {
			return nil, fmt.Errorf("error generating dispatch argument: %v", err)
		}
		args = append(args, argValue)
	}

	// Call the method directly in the current block
	callResult = cg.currentBlock.NewCall(methodFunc, args...)

	// Return the call result
	return callResult, nil
}

// Also update the if expression function to use a similar approach
// Add this helper function to guarantee every block has a terminator
func (cg *CodeGenerator) ensureBlockTerminator(block *ir.Block) {
	// Check if the block already has a terminator instruction
	if block.Term == nil {
		// Add an unreachable instruction if no terminator exists
		// This is a defensive measure - ideally each block should have a proper terminator
		block.NewUnreachable()
	}
}

func (cg *CodeGenerator) generateIfExpression(ifExpr *ast.IfExpression) (value.Value, error) {
	// Get the current function name for unique block naming
	funcName := cg.currentFunc.Name()
	ifId := len(cg.currentFunc.Blocks)

	// Create blocks with unique names
	thenBlock := cg.currentFunc.NewBlock(fmt.Sprintf("if.then.%s.%d", funcName, ifId))
	elseBlock := cg.currentFunc.NewBlock(fmt.Sprintf("if.else.%s.%d", funcName, ifId))
	afterBlock := cg.currentFunc.NewBlock(fmt.Sprintf("if.after.%s.%d", funcName, ifId))

	// Save the block we're starting from
	preCondBlock := cg.currentBlock

	// Generate condition code
	condValue, err := cg.generateExpression(ifExpr.Condition)
	if err != nil {
		return nil, fmt.Errorf("error generating condition: %v", err)
	}

	// Branch based on condition
	preCondBlock.NewCondBr(condValue, thenBlock, elseBlock)

	// Generate then branch
	cg.currentBlock = thenBlock
	thenValue, err := cg.generateExpression(ifExpr.Consequence)
	if err != nil {
		return nil, fmt.Errorf("error generating then branch: %v", err)
	}

	// Make sure then block branches to after block if it doesn't already have a terminator
	if thenBlock.Term == nil {
		thenBlock.NewBr(afterBlock)
	}

	// Generate else branch
	cg.currentBlock = elseBlock
	elseValue, err := cg.generateExpression(ifExpr.Alternative)
	if err != nil {
		return nil, fmt.Errorf("error generating else branch: %v", err)
	}

	// Make sure else block branches to after block if it doesn't already have a terminator
	if elseBlock.Term == nil {
		elseBlock.NewBr(afterBlock)
	}

	// Set current block to after block for subsequent code
	cg.currentBlock = afterBlock

	// Handle different result types between then and else
	var resultValue value.Value

	// If both branches have values, create a phi node
	if thenValue != nil && elseValue != nil {
		// Check if both are the same type (simple case)
		if thenValue.Type() == elseValue.Type() {
			resultValue = afterBlock.NewPhi(
				ir.NewIncoming(thenValue, thenBlock),
				ir.NewIncoming(elseValue, elseBlock),
			)
		} else {
			// Types differ - try to find a common type to cast to
			if isIntType(thenValue.Type()) && isIntType(elseValue.Type()) {
				// Both integers, but different bit widths - use the wider one
				resultValue = afterBlock.NewPhi(
					ir.NewIncoming(thenValue, thenBlock),
					ir.NewIncoming(elseValue, elseBlock),
				)
			} else if isPtrType(thenValue.Type()) && isPtrType(elseValue.Type()) {
				// Both pointers but different types - cast to void*
				voidPtrType := types.NewPointer(types.I8)
				thenValueCast := afterBlock.NewBitCast(thenValue, voidPtrType)
				elseValueCast := afterBlock.NewBitCast(elseValue, voidPtrType)
				resultValue = afterBlock.NewPhi(
					ir.NewIncoming(thenValueCast, thenBlock),
					ir.NewIncoming(elseValueCast, elseBlock),
				)
			} else {
				// Default to using the then value if types are incompatible
				resultValue = thenValue
			}
		}
	} else if thenValue != nil {
		resultValue = thenValue
	} else if elseValue != nil {
		resultValue = elseValue
	} else {
		// If neither branch has a value, return void/null
		objPtrType := types.NewPointer(cg.classTypes["Object"])
		resultValue = constant.NewNull(objPtrType)
	}

	return resultValue, nil
}

// Helper functions
func isIntType(t types.Type) bool {
	if _, ok := t.(*types.IntType); ok {
		return true
	}
	return false
}

// Helper functions to check value types
func isNullValue(v value.Value) bool {
	_, isNull := v.(*constant.Null)
	return isNull
}

func isPtrType(t types.Type) bool {
	_, ok := t.(*types.PointerType)
	return ok
}

func isZeroInt(v value.Value) bool {
	if constInt, ok := v.(*constant.Int); ok {
		return constInt.X.Int64() == 0
	}
	return false
}

// Helper function to safely create a null pointer
func safeNewNull(t types.Type) value.Value {
	if ptrType, ok := t.(*types.PointerType); ok {
		return constant.NewNull(ptrType)
	}
	// Fallback for non-pointer types (should never happen in correct code)
	return constant.NewInt(types.I32, 0)
}

func (cg *CodeGenerator) generateMethodImplementation(className string, method *ast.Method) error {
	methodName := method.Name.Value

	// Set current context
	cg.currentClass = className
	cg.currentMethod = methodName
	cg.currentFunc = cg.methods[className][methodName]
	cg.variables = make(map[string]value.Value) // Clear local variables

	// Create entry block
	entry := cg.currentFunc.NewBlock("entry")
	cg.currentBlock = entry

	// Add formal parameters to variables map
	selfParam := cg.currentFunc.Params[0]
	cg.variables["self"] = selfParam

	for i, formal := range method.Formals {
		// Allocate stack space for the formal parameter
		paramType := cg.currentFunc.Params[i+1].Type()
		paramAlloca := entry.NewAlloca(paramType)

		// Store the parameter value
		entry.NewStore(cg.currentFunc.Params[i+1], paramAlloca)

		// Add to variables map
		cg.variables[formal.Name.Value] = paramAlloca
	}

	// Generate code for method body
	returnValue, err := cg.generateExpression(method.Body)
	if err != nil {
		return fmt.Errorf("error generating method body: %v", err)
	}

	// Get the expected return type from the function definition
	expectedReturnType := cg.currentFunc.Sig.RetType

	// Check all blocks in the function
	for _, block := range cg.currentFunc.Blocks {
		// If a block doesn't have a terminator, we need to add one
		if block.Term == nil {
			if block == cg.currentBlock {
				// For the current block, add a return with the return value
				if returnValue == nil {
					// For nil return values (void result), create a proper null of expected type
					if ptrType, ok := expectedReturnType.(*types.PointerType); ok {
						returnValue = constant.NewNull(ptrType)
					} else if intType, ok := expectedReturnType.(*types.IntType); ok {
						returnValue = constant.NewInt(intType, 0)
					} else {
						// Default zero value
						returnValue = constant.NewZeroInitializer(expectedReturnType)
					}
				} else if returnValue.Type() != expectedReturnType {
					// Need to cast to the expected return type
					if isPtrType(expectedReturnType) && isPtrType(returnValue.Type()) {
						// Cast between pointer types
						returnValue = block.NewBitCast(returnValue, expectedReturnType)
					} else if isIntType(expectedReturnType) && isIntType(returnValue.Type()) {
						// Convert between integer types
						if expectedReturnType.(*types.IntType).BitSize > returnValue.Type().(*types.IntType).BitSize {
							returnValue = block.NewSExt(returnValue, expectedReturnType)
						} else {
							returnValue = block.NewTrunc(returnValue, expectedReturnType)
						}
					} else {
						// Other type conversion
						returnValue = block.NewBitCast(returnValue, expectedReturnType)
					}
				}

				// Add return with the value
				block.NewRet(returnValue)
			} else {
				// For other blocks with no terminator, add an unreachable instruction
				// as a fallback for static analysis, but log a warning
				fmt.Printf("Warning: Block %s in function %s has no terminator, adding unreachable\n",
					block.Name(), cg.currentFunc.Name())
				block.NewUnreachable()
			}
		}
	}

	return nil
}

// generateEntryPoint generates the main function as entry point
func (cg *CodeGenerator) generateEntryPoint() error {
	// Check if Main class exists
	if _, exists := cg.classTypes["Main"]; !exists {
		return fmt.Errorf("missing Main class")
	}

	// Check if main method exists
	mainMethod, exists := cg.methods["Main"]["main"]
	if !exists {
		return fmt.Errorf("missing main method in Main class")
	}

	// Create C main function that calls Main.main
	mainFunc := cg.Module.NewFunc("main", types.I32)
	entry := mainFunc.NewBlock("entry")

	// Create instance of Main class
	mainClassPtr := cg.generateNewObject(entry, "Main")

	// Call Main.main() on the instance
	entry.NewCall(mainMethod, mainClassPtr)

	// Return 0 from main
	entry.NewRet(constant.NewInt(types.I32, 0))

	return nil
}

// generateNewObject generates code to create a new object of the given class
func (cg *CodeGenerator) generateNewObject(block *ir.Block, className string) value.Value {
	classType, exists := cg.classTypes[className]
	if !exists {
		panic(fmt.Sprintf("Class %s not found", className))
	}

	// Calculate object size (size of the class struct)
	// A proper implementation would calculate exact size
	sizeOfClass := constant.NewInt(types.I64, 64) // Approximation

	// Call GC_malloc
	rawPtr := block.NewCall(cg.runtimeFuncs["GC_malloc"], sizeOfClass)

	// Cast to object type
	objPtr := block.NewBitCast(rawPtr, types.NewPointer(classType))

	// Initialize vtable pointer - get the global vtable
	vtableGlobal, exists := cg.vtableGlobals[className]
	if !exists {
		panic(fmt.Sprintf("Vtable for class %s not found", className))
	}

	// Get pointer to vtable field in object
	vtablePtr := block.NewGetElementPtr(
		classType,
		objPtr,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0), // First field is vtable pointer
	)

	// The vtable field is a pointer to void, but we need to store a pointer to the vtable
	// Cast the vtable global to the right type before storing
	vtablePtrType := vtablePtr.Type().(*types.PointerType)
	castedVtable := block.NewBitCast(vtableGlobal, vtablePtrType.ElemType)
	block.NewStore(castedVtable, vtablePtr)

	// Initialize other fields with defaults
	cg.initializeObjectFields(block, objPtr, className)

	return objPtr
}

// Helper to initialize object fields with default values
func (cg *CodeGenerator) initializeObjectFields(block *ir.Block, objPtr value.Value, className string) {
	classType := cg.classTypes[className]

	// Start from field 1 (after vtable pointer)
	for i := 1; i < len(classType.Fields); i++ {
		// Get field type
		fieldType := classType.Fields[i]

		// Get field pointer
		fieldPtr := block.NewGetElementPtr(
			classType,
			objPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(i)),
		)

		// Initialize with default value for the type
		defaultVal := cg.getDefaultValue(fieldType)
		block.NewStore(defaultVal, fieldPtr)
	}
}

// Get default value for a type
func (cg *CodeGenerator) getDefaultValue(typ types.Type) value.Value {
	switch t := typ.(type) {
	case *types.IntType:
		return constant.NewInt(t, 0)
	case *types.FloatType:
		return constant.NewFloat(t, 0.0)
	case *types.PointerType:
		// Use type assertion to pass the correct type to NewNull
		return constant.NewNull(t)
	default:
		// For complex types, use zero initializer
		return constant.NewZeroInitializer(typ)
	}
}

// mapType maps a Cool type name to an LLVM type
func (cg *CodeGenerator) mapType(typeName string) (types.Type, error) {
	// Handle SELF_TYPE contextually based on current class
	if typeName == "SELF_TYPE" {
		if cg.currentClass == "" {
			return nil, fmt.Errorf("SELF_TYPE used outside of class context")
		}

		// Return a pointer to the current class type
		if classType, exists := cg.classTypes[cg.currentClass]; exists {
			return types.NewPointer(classType), nil
		}

		return nil, fmt.Errorf("current class %s not found in class types", cg.currentClass)
	}

	// Handle other types
	switch typeName {
	case "Int":
		return cg.intType, nil
	case "Bool":
		return cg.boolType, nil
	case "String":
		return cg.stringType, nil
	case "Object":
		return types.NewPointer(cg.objectType), nil
	default:
		// Check if it's a user-defined class
		if classType, exists := cg.classTypes[typeName]; exists {
			return types.NewPointer(classType), nil
		}
		return nil, fmt.Errorf("unknown type: %s", typeName)
	}
}

// generateExpression generates LLVM IR for an expression
func (cg *CodeGenerator) generateExpression(expr ast.Expression) (value.Value, error) {
	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return cg.generateIntegerLiteral(e)
	case *ast.StringLiteral:
		return cg.generateStringLiteral(e)
	case *ast.BooleanLiteral:
		return cg.generateBooleanLiteral(e)
	case *ast.ObjectIdentifier:
		return cg.generateObjectIdentifier(e)
	case *ast.Assignment:
		return cg.generateAssignment(e)
	case *ast.DispatchExpression:
		return cg.generateDispatch(e)
	case *ast.BinaryExpression:
		return cg.generateBinaryExpression(e)
	case *ast.UnaryExpression:
		return cg.generateUnaryExpression(e)
	case *ast.BlockExpression:
		return cg.generateBlockExpression(e)
	case *ast.IfExpression:
		return cg.generateIfExpression(e)
	case *ast.WhileExpression:
		return cg.generateWhileExpression(e)
	case *ast.LetExpression:
		return cg.generateLetExpression(e)
	case *ast.CaseExpression:
		return cg.generateCaseExpression(e)
	case *ast.NewExpression:
		return cg.generateNewExpression(e)
	case *ast.IsVoidExpression:
		return cg.generateIsVoidExpression(e)
	default:
		return nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// generateIntegerLiteral generates LLVM IR for an integer literal
func (cg *CodeGenerator) generateIntegerLiteral(intLit *ast.IntegerLiteral) (value.Value, error) {
	// Create integer constant
	return constant.NewInt(types.I32, int64(intLit.Value)), nil
}

func (cg *CodeGenerator) createStringConstant(block *ir.Block, nameHint string, content string) value.Value {
	// Add null terminator if needed
	if !strings.HasSuffix(content, "\x00") {
		content = content + "\x00"
	}

	// Create a unique name for the global that follows LLVM's naming conventions
	// Avoid special characters in the name
	globalName := fmt.Sprintf("str.%s.%d", nameHint, len(cg.Module.Globals))

	// Define array type and global constant
	strType := types.NewArray(uint64(len(content)), types.I8)
	strGlobal := cg.Module.NewGlobalDef(globalName, constant.NewCharArrayFromString(content))
	strGlobal.Immutable = true // Mark as constant

	// Get pointer to the first character using proper GEP
	zero := constant.NewInt(types.I32, 0)
	return block.NewGetElementPtr(strType, strGlobal, zero, zero)
}

// Modified version of generateStringLiteral that uses the helper
func (cg *CodeGenerator) generateStringLiteral(strLit *ast.StringLiteral) (value.Value, error) {
	return cg.createStringConstant(cg.currentBlock, strLit.Value, strLit.Value), nil
}

// generateBooleanLiteral generates LLVM IR for a boolean literal
func (cg *CodeGenerator) generateBooleanLiteral(boolLit *ast.BooleanLiteral) (value.Value, error) {
	// Create boolean constant
	boolValue := 0
	if boolLit.Value {
		boolValue = 1
	}
	return constant.NewInt(types.I1, int64(boolValue)), nil
}

// generateObjectIdentifier generates LLVM IR for an object identifier
func (cg *CodeGenerator) generateObjectIdentifier(obj *ast.ObjectIdentifier) (value.Value, error) {
	varName := obj.Value

	// Check if it's a local variable
	if varVal, exists := cg.variables[varName]; exists {
		// If it's a pointer to a stack variable, load it
		if _, ok := varVal.Type().(*types.PointerType); ok {
			return cg.currentBlock.NewLoad(varVal.Type().(*types.PointerType).ElemType, varVal), nil
		}
		return varVal, nil
	}

	// Check if it's a class attribute
	attrKey := cg.currentClass + "." + varName
	if attrIndex, exists := cg.attrs[attrKey]; exists {
		// Load self pointer
		selfPtr := cg.variables["self"]

		// Get attribute address
		attrPtr := cg.currentBlock.NewGetElementPtr(
			cg.classTypes[cg.currentClass],
			selfPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(attrIndex)),
		)

		// Get pointer type and element type
		ptrType := attrPtr.Type().(*types.PointerType)

		// Load attribute value
		return cg.currentBlock.NewLoad(ptrType.ElemType, attrPtr), nil
	}

	return nil, fmt.Errorf("undefined identifier: %s", varName)
}

// generateAssignment generates LLVM IR for an assignment
// Fix for your generateAssignment function
func (cg *CodeGenerator) generateAssignment(assign *ast.Assignment) (value.Value, error) {
	varName := assign.Name.Value

	// Generate the right-hand-side expression
	rhsValue, err := cg.generateExpression(assign.Value)
	if err != nil {
		return nil, fmt.Errorf("error generating right-hand side: %v", err)
	}

	// Check if it's a local variable
	if varPtr, exists := cg.variables[varName]; exists {
		// Get the element type from the pointer
		if ptrType, ok := varPtr.Type().(*types.PointerType); ok {
			// If the RHS type doesn't match exactly, try to cast it
			if rhsValue.Type() != ptrType.ElemType {
				if _, ok := ptrType.ElemType.(*types.PointerType); ok && rhsValue.Type() != ptrType.ElemType {
					// Careful casting for pointer-to-pointer
					rhsValue = cg.currentBlock.NewBitCast(rhsValue, ptrType.ElemType)
				} else if isIntType(ptrType.ElemType) && isIntType(rhsValue.Type()) {
					// Handle integer type conversions
					if ptrType.ElemType.(*types.IntType).BitSize != rhsValue.Type().(*types.IntType).BitSize {
						// Need to resize the integer
						if ptrType.ElemType.(*types.IntType).BitSize > rhsValue.Type().(*types.IntType).BitSize {
							rhsValue = cg.currentBlock.NewSExt(rhsValue, ptrType.ElemType)
						} else {
							rhsValue = cg.currentBlock.NewTrunc(rhsValue, ptrType.ElemType)
						}
					}
				}
			}
			// Store to the variable
			cg.currentBlock.NewStore(rhsValue, varPtr)
		} else {
			// Direct store without a pointer (unusual but handle it)
			cg.currentBlock.NewStore(rhsValue, varPtr)
		}
		return rhsValue, nil
	}

	// Check if it's a class attribute
	attrKey := cg.currentClass + "." + varName
	if attrIndex, exists := cg.attrs[attrKey]; exists {
		// Load self pointer
		selfPtr := cg.variables["self"]

		// Get attribute address
		attrPtr := cg.currentBlock.NewGetElementPtr(
			cg.classTypes[cg.currentClass],
			selfPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(attrIndex)),
		)

		// Get pointer type
		ptrType := attrPtr.Type().(*types.PointerType)

		// Try to cast the RHS to the attribute type if needed
		if rhsValue.Type() != ptrType.ElemType {
			// Handle null values specially
			if _, ok := rhsValue.(*constant.Null); ok {
				// Create a new null of the correct type
				if elemPtrType, ok := ptrType.ElemType.(*types.PointerType); ok {
					rhsValue = constant.NewNull(elemPtrType)
				}
			} else {
				// Try a bitcast for other types
				rhsValue = cg.currentBlock.NewBitCast(rhsValue, ptrType.ElemType)
			}
		}

		// Store to the attribute
		cg.currentBlock.NewStore(rhsValue, attrPtr)
		return rhsValue, nil
	}

	return nil, fmt.Errorf("undefined identifier for assignment: %s", varName)
}

// generateBinaryExpression generates LLVM IR for a binary expression
func (cg *CodeGenerator) generateBinaryExpression(binary *ast.BinaryExpression) (value.Value, error) {
	// Generate left and right operands
	left, err := cg.generateExpression(binary.Left)
	if err != nil {
		return nil, fmt.Errorf("error generating left operand: %v", err)
	}

	right, err := cg.generateExpression(binary.Right)
	if err != nil {
		return nil, fmt.Errorf("error generating right operand: %v", err)
	}

	// Perform operation based on operator
	switch binary.Operator {
	case "+":
		return cg.currentBlock.NewAdd(left, right), nil
	case "-":
		return cg.currentBlock.NewSub(left, right), nil
	case "*":
		return cg.currentBlock.NewMul(left, right), nil
	case "/":
		return cg.currentBlock.NewSDiv(left, right), nil
	case "<":
		return cg.currentBlock.NewICmp(enum.IPredSLT, left, right), nil
	case "<=":
		return cg.currentBlock.NewICmp(enum.IPredSLE, left, right), nil
	case "=":
		return cg.currentBlock.NewICmp(enum.IPredEQ, left, right), nil
	default:
		return nil, fmt.Errorf("unsupported binary operator: %s", binary.Operator)
	}
}

// Helper to access a field in an object
func (cg *CodeGenerator) getObjectField(block *ir.Block, objPtr value.Value, fieldIndex int) value.Value {
	if ptrType, ok := objPtr.Type().(*types.PointerType); ok {
		if structType, ok := ptrType.ElemType.(*types.StructType); ok {
			fieldPtr := block.NewGetElementPtr(
				structType,
				objPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(fieldIndex)),
			)
			return fieldPtr
		}
	}
	panic(fmt.Sprintf("Cannot get field %d from non-object pointer: %v", fieldIndex, objPtr.Type()))
}

func (cg *CodeGenerator) generateBlockExpression(block *ast.BlockExpression) (value.Value, error) {
	var lastValue value.Value
	var err error

	// Generate code for each expression in the block
	for i, expr := range block.Expressions {
		// Check if the current block already has a terminator
		// If so, we need to create a new block for remaining expressions
		if cg.currentBlock.Term != nil {
			newBlock := cg.currentFunc.NewBlock(fmt.Sprintf("block.cont.%d", len(cg.currentFunc.Blocks)))

			// Try to connect the previous block to this new one if possible
			prevBlock := cg.currentFunc.Blocks[len(cg.currentFunc.Blocks)-2]
			if termBr, ok := prevBlock.Term.(*ir.TermBr); ok {
				// If it's an unconditional branch, update its target
				termBr.Target = newBlock
			}

			cg.currentBlock = newBlock
		}

		// Generate the current expression
		lastValue, err = cg.generateExpression(expr)
		if err != nil {
			return nil, fmt.Errorf("error generating expression %d in block: %v", i, err)
		}
	}

	// Return the value of the last expression
	return lastValue, nil
}

// Helper function to find which class in the inheritance chain defines a method
func (cg *CodeGenerator) findMethodDefiningClass(className, methodName string) string {
	current := className
	for current != "" {
		if methodMap, exists := cg.methods[current]; exists {
			if _, exists := methodMap[methodName]; exists {
				return current
			}
		}
		current = cg.classParents[current]
	}
	return "Object" // Default to Object if not found
}

// Helper function to find a method in a class or its ancestors
func (cg *CodeGenerator) findMethod(className, methodName string) *ir.Func {
	current := className
	for current != "" {
		if methodMap, exists := cg.methods[current]; exists {
			if method, exists := methodMap[methodName]; exists {
				return method
			}
		}
		current = cg.classParents[current]
	}
	return nil
}

// generateWhileExpression generates LLVM IR for a while expression
func (cg *CodeGenerator) generateWhileExpression(whileExpr *ast.WhileExpression) (value.Value, error) {
	// Create unique names for blocks
	funcName := cg.currentFunc.Name()
	loopId := len(cg.currentFunc.Blocks)

	// Create blocks for the loop structure
	condBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.cond.%s.%d", funcName, loopId))
	bodyBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.body.%s.%d", funcName, loopId))
	afterBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.after.%s.%d", funcName, loopId))

	// Branch from current block to condition block
	cg.currentBlock.NewBr(condBlock)

	// Generate condition code in condition block
	cg.currentBlock = condBlock
	condValue, err := cg.generateExpression(whileExpr.Condition)
	if err != nil {
		return nil, fmt.Errorf("error generating while condition: %v", err)
	}

	// Branch based on condition to either body or after
	condBlock.NewCondBr(condValue, bodyBlock, afterBlock)

	// Generate body code in body block
	cg.currentBlock = bodyBlock
	_, err = cg.generateExpression(whileExpr.Body)
	if err != nil {
		return nil, fmt.Errorf("error generating while body: %v", err)
	}

	// Add back edge from body to condition - but only if body doesn't have a terminator already
	if bodyBlock.Term == nil {
		bodyBlock.NewBr(condBlock)
	} else {
		// If the body already has a terminator, we need to handle this special case
		// (e.g., from nested if/while statements)

		// Check if the last block in the function is without a terminator - it's likely the "real" end of the body
		lastBlock := cg.currentFunc.Blocks[len(cg.currentFunc.Blocks)-1]
		if lastBlock.Term == nil {
			lastBlock.NewBr(condBlock)
		} else {
			// Create a new block to continue back to the condition
			continueBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.cont.%s.%d", funcName, loopId))
			cg.currentBlock = continueBlock
			continueBlock.NewBr(condBlock)
		}
	}

	// Set current block to after block for code that follows the loop
	cg.currentBlock = afterBlock

	// While loops always return void/null in Cool
	objPtrType := types.NewPointer(cg.classTypes["Object"])
	return constant.NewNull(objPtrType), nil
}

// Placeholder stubs for remaining expression types
func (cg *CodeGenerator) generateLetExpression(let *ast.LetExpression) (value.Value, error) {
	// Simplified implementation - should handle multiple bindings
	// Create block for let body
	letBlock := cg.currentFunc.NewBlock("let")

	// Branch to let block
	cg.currentBlock.NewBr(letBlock)
	cg.currentBlock = letBlock

	// Create new scope by saving old variables map
	oldVariables := cg.variables
	cg.variables = make(map[string]value.Value)

	// Copy values from old scope
	for k, v := range oldVariables {
		cg.variables[k] = v
	}

	// Process bindings
	for _, binding := range let.Bindings {
		bindingName := binding.Identifier.Value
		bindingType, err := cg.mapType(binding.Type.Value)
		if err != nil {
			return nil, fmt.Errorf("error mapping type for let binding %s: %v", bindingName, err)
		}

		// Allocate space for binding
		varAlloca := cg.currentBlock.NewAlloca(bindingType)

		// Initialize if there's an initialization expression
		if binding.Init != nil {
			initValue, err := cg.generateExpression(binding.Init)
			if err != nil {
				return nil, fmt.Errorf("error generating let init: %v", err)
			}
			cg.currentBlock.NewStore(initValue, varAlloca)
		} else {
			// Default initialization
			cg.currentBlock.NewStore(constant.NewZeroInitializer(bindingType), varAlloca)
		}

		// Add to variables map
		cg.variables[bindingName] = varAlloca
	}

	// Generate body expression
	result, err := cg.generateExpression(let.In)
	if err != nil {
		return nil, fmt.Errorf("error generating let body: %v", err)
	}

	// Restore old scope
	cg.variables = oldVariables

	return result, nil
}

func (cg *CodeGenerator) generateCaseExpression(caseExpr *ast.CaseExpression) (value.Value, error) {
	// Generate the expression to match on
	expr, err := cg.generateExpression(caseExpr.Expr)
	if err != nil {
		return nil, fmt.Errorf("error generating case expression: %v", err)
	}

	// Check if expr is null/void
	nullCheckBlock := cg.currentFunc.NewBlock("case.null_check")
	errorBlock := cg.currentFunc.NewBlock("case.error")

	// Create a merge block where all successful case branches will converge
	mergeBlock := cg.currentFunc.NewBlock("case.merge")

	// Check if expr is null/void - use type assertion for pointer types
	if ptrType, ok := expr.Type().(*types.PointerType); ok {
		isNull := cg.currentBlock.NewICmp(enum.IPredEQ, expr, constant.NewNull(ptrType))
		cg.currentBlock.NewCondBr(isNull, errorBlock, nullCheckBlock)
	} else {
		// Non-pointer types can't be null, so skip the check
		cg.currentBlock.NewBr(nullCheckBlock)
	}

	// Error block - case on void
	cg.currentBlock = errorBlock
	errorMsg := cg.createStringConstant(errorBlock, "ERROR: Case on void\n", "void_case")

	// Call printf with the error message
	printfFunc := cg.Module.NewFunc("printf", types.I32, ir.NewParam("format", cg.stringType))
	errorBlock.NewCall(printfFunc, errorMsg)

	// Exit the program
	exitFunc := cg.Module.NewFunc("exit", types.I8, ir.NewParam("status", types.I32))
	errorBlock.NewCall(exitFunc, constant.NewInt(types.I32, 1))
	errorBlock.NewUnreachable()

	// Continue after null check
	cg.currentBlock = nullCheckBlock

	// Get the runtime type of the expression
	runtimeType := cg.generateGetRuntimeType(nullCheckBlock, expr)

	// Sort branches by specificity (most specific first)
	// This ensures proper semantics when multiple branches could match
	branches := caseExpr.Branches
	sortedBranches := cg.sortBranchesBySpecificity(branches)

	// Create blocks for each branch
	branchBlocks := make([]*ir.Block, len(sortedBranches))
	for i := range sortedBranches {
		branchBlocks[i] = cg.currentFunc.NewBlock(fmt.Sprintf("case.branch%d", i))
	}

	// Add a default error block if no branch matches
	noMatchBlock := cg.currentFunc.NewBlock("case.no_match")

	// Generate a switch based on runtime type
	cg.generateTypeSwitch(nullCheckBlock, runtimeType, sortedBranches, branchBlocks, noMatchBlock)

	// Generate code for each branch
	var branchValues []value.Value
	var branchOrigins []*ir.Block

	for i, branch := range sortedBranches {
		cg.currentBlock = branchBlocks[i]

		// Create a new scope with the branch variable
		oldVars := cg.variables
		cg.variables = make(map[string]value.Value)
		for k, v := range oldVars {
			cg.variables[k] = v
		}

		// Add the case value to the variables
		// In a real implementation we should cast the value to the branch type
		cg.variables[branch.Pattern.Value] = expr

		// Generate branch expression
		branchValue, err := cg.generateExpression(branch.Expression)
		if err != nil {
			return nil, fmt.Errorf("error generating case branch: %v", err)
		}

		// Branch to merge block
		cg.currentBlock.NewBr(mergeBlock)

		// Store for phi node
		branchValues = append(branchValues, branchValue)
		branchOrigins = append(branchOrigins, cg.currentBlock)

		// Restore variables
		cg.variables = oldVars
	}

	// Generate no-match error block
	cg.currentBlock = noMatchBlock
	noMatchMsg := cg.createStringConstant(noMatchBlock, "ERROR: No matching branch in case\n", "case_no_match")

	noMatchBlock.NewCall(printfFunc, noMatchMsg)
	noMatchBlock.NewCall(exitFunc, constant.NewInt(types.I32, 1))
	noMatchBlock.NewUnreachable()

	// Set up merge block with phi node
	cg.currentBlock = mergeBlock

	// Create a phi node to merge all branch results
	if len(branchValues) == 0 {
		// If no branches (shouldn't happen), return void
		return constant.NewNull(types.NewPointer(cg.objectType)), nil
	}

	// Create incoming values for phi node
	incomingValues := []*ir.Incoming{}
	for i, val := range branchValues {
		incomingValues = append(incomingValues, ir.NewIncoming(val, branchOrigins[i]))
	}

	phi := mergeBlock.NewPhi(incomingValues...)
	return phi, nil
}

// Generate a switch based on runtime type
func (cg *CodeGenerator) generateTypeSwitch(block *ir.Block, runtimeType value.Value, branches []*ast.CaseBranch, branchBlocks []*ir.Block, defaultBlock *ir.Block) {
	// In a real implementation, this would generate a switch statement
	// For now, we'll generate a series of if-else statements

	currentBlock := block

	for i, branch := range branches {
		// Get the expected type ID for this branch
		typeID := cg.getTypeID(branch.Type.Value)

		// Create a comparison block
		compareBlock := cg.currentFunc.NewBlock(fmt.Sprintf("case.compare%d", i))

		// Branch to compare block
		currentBlock.NewBr(compareBlock)
		currentBlock = compareBlock

		// Compare runtime type with expected type
		isMatch := currentBlock.NewICmp(enum.IPredEQ, runtimeType, typeID)

		// If type matches, go to branch block, otherwise continue comparing
		if i < len(branches)-1 {
			nextCompareBlock := cg.currentFunc.NewBlock(fmt.Sprintf("case.next_compare%d", i))
			currentBlock.NewCondBr(isMatch, branchBlocks[i], nextCompareBlock)
			currentBlock = nextCompareBlock
		} else {
			// Last branch - if no match, go to default block
			currentBlock.NewCondBr(isMatch, branchBlocks[i], defaultBlock)
		}
	}
}

// Helper to sort case branches by specificity (most specific first)
func (cg *CodeGenerator) sortBranchesBySpecificity(branches []*ast.CaseBranch) []*ast.CaseBranch {
	// Create a copy to avoid modifying the original
	sortedBranches := make([]*ast.CaseBranch, len(branches))
	copy(sortedBranches, branches)

	// Sort by specificity (more specific types come first)
	// This uses the inheritance graph from semantic analysis
	sort.Slice(sortedBranches, func(i, j int) bool {
		typeI := sortedBranches[i].Type.Value
		typeJ := sortedBranches[j].Type.Value

		// If one type is a subclass of the other, the subclass is more specific
		if cg.isSubtypeOf(typeI, typeJ) {
			return true
		}
		if cg.isSubtypeOf(typeJ, typeI) {
			return false
		}

		// Otherwise, order doesn't matter for type matching
		// but we'll sort by name for deterministic output
		return typeI < typeJ
	})

	return sortedBranches
}

// Check if typeA is a subtype of typeB using the inheritance graph
func (cg *CodeGenerator) isSubtypeOf(typeA, typeB string) bool {
	// Object is at the root, so anything is a subtype of Object
	if typeB == "Object" && typeA != "Object" {
		return true
	}

	// Walk up the inheritance chain from typeA
	current := typeA
	for current != "" && current != typeB {
		current = cg.classParents[current]
	}

	return current == typeB
}

// Generate code to get the runtime type of an object
func (cg *CodeGenerator) generateGetRuntimeType(block *ir.Block, obj value.Value) value.Value {
	// In a real implementation, we would access a type field or vtable entry
	// For this simplified version, we'll call a runtime function

	// First, ensure we have the runtime type function
	if _, exists := cg.runtimeFuncs["get_runtime_type"]; !exists {
		// Define the function if not already defined
		getTypeFunc := cg.Module.NewFunc(
			"get_runtime_type",
			cg.intType, // Return an integer type ID
			ir.NewParam("obj", types.NewPointer(types.I8)),
		)
		cg.runtimeFuncs["get_runtime_type"] = getTypeFunc

		// Implement the function to extract type info from object
		entry := getTypeFunc.NewBlock("entry")

		// In a real implementation, this would access a type field
		// For now, just return a dummy value
		entry.NewRet(constant.NewInt(types.I32, 0))
	}

	// Cast object to void* for the function call
	objVoidPtr := block.NewBitCast(obj, types.NewPointer(types.I8))

	// Call the function to get the runtime type
	return block.NewCall(cg.runtimeFuncs["get_runtime_type"], objVoidPtr)
}

// Get a constant for a type ID
func (cg *CodeGenerator) getTypeID(typeName string) value.Value {
	// In a real implementation, this would return a unique ID for each type
	// For now, we'll use a simple mapping
	typeIDs := map[string]int64{
		"Object": 0,
		"IO":     1,
		"Int":    2,
		"String": 3,
		"Bool":   4,
		// Add other types as needed
	}

	// Get ID for the type, default to 0 (Object) if not found
	typeID, exists := typeIDs[typeName]
	if !exists {
		// For user-defined classes, assign a new ID
		typeID = int64(len(typeIDs))
		typeIDs[typeName] = typeID
	}

	return constant.NewInt(types.I32, typeID)
}

// Implement runtime functions for type checking
func (cg *CodeGenerator) implementRuntimeTypeFunctions() {
	// Define function to get runtime type from an object
	getTypeFunc := cg.Module.NewFunc(
		"get_runtime_type",
		cg.intType,
		ir.NewParam("obj", types.NewPointer(types.I8)),
	)
	cg.runtimeFuncs["get_runtime_type"] = getTypeFunc

	// Implement the function
	entry := getTypeFunc.NewBlock("entry")

	// For each class, we need a unique type ID
	// In a real implementation, this would be stored in the object vtable
	// or as a dedicated type field

	// For now, a simple implementation that just checks for null
	isNull := entry.NewICmp(
		enum.IPredEQ,
		getTypeFunc.Params[0],
		constant.NewNull(types.NewPointer(types.I8)),
	)

	nullBlock := getTypeFunc.NewBlock("null")
	nonNullBlock := getTypeFunc.NewBlock("non_null")

	entry.NewCondBr(isNull, nullBlock, nonNullBlock)

	// Null case - return a special type ID
	nullBlock.NewRet(constant.NewInt(types.I32, -1))

	// Non-null case - extract type from vtable or similar
	// This is a placeholder implementation
	nonNullBlock.NewRet(constant.NewInt(types.I32, 0))
}

func (cg *CodeGenerator) generateNewExpression(newExpr *ast.NewExpression) (value.Value, error) {
	className := newExpr.Type.Value
	return cg.generateNewObject(cg.currentBlock, className), nil
}

func (cg *CodeGenerator) generateIsVoidExpression(isVoid *ast.IsVoidExpression) (value.Value, error) {
	// Generate the expression to check
	expr, err := cg.generateExpression(isVoid.Expression)
	if err != nil {
		return nil, fmt.Errorf("error generating isvoid expression: %v", err)
	}

	// Check if this is a pointer type that can be null
	if ptrType, ok := expr.Type().(*types.PointerType); ok {
		// For pointers, compare with null
		return cg.currentBlock.NewICmp(enum.IPredEQ, expr, constant.NewNull(ptrType)), nil
	} else {
		// For non-pointer types (like Int, Bool), they're never void
		return constant.NewInt(types.I1, 0), nil
	}
}

func (cg *CodeGenerator) generateUnaryExpression(unary *ast.UnaryExpression) (value.Value, error) {
	// Generate operand
	operand, err := cg.generateExpression(unary.Right)
	if err != nil {
		return nil, fmt.Errorf("error generating unary operand: %v", err)
	}

	// Perform operation based on operator
	switch unary.Operator {
	case "~":
		// Bitwise negation for integers
		return cg.currentBlock.NewXor(operand, constant.NewInt(types.I32, -1)), nil
	case "not":
		// Logical negation for booleans
		return cg.currentBlock.NewXor(operand, constant.NewInt(types.I1, 1)), nil
	default:
		return nil, fmt.Errorf("unsupported unary operator: %s", unary.Operator)
	}
}

func (cg *CodeGenerator) initializeBasicClasses() {
	// Make sure Object class has methods map
	if _, exists := cg.methods["Object"]; !exists {
		cg.methods["Object"] = make(map[string]*ir.Func)
	}

	// Make sure IO class has methods map
	if _, exists := cg.methods["IO"]; !exists {
		cg.methods["IO"] = make(map[string]*ir.Func)
	}

	// Set up IO methods
	cg.methods["IO"]["out_string"] = cg.runtimeFuncs["IO_out_string"]
	cg.methods["IO"]["out_int"] = cg.runtimeFuncs["IO_out_int"]
	cg.methods["IO"]["in_string"] = cg.runtimeFuncs["IO_in_string"]
	cg.methods["IO"]["in_int"] = cg.runtimeFuncs["IO_in_int"]

	if _, exists := cg.methods["String"]; !exists {
		cg.methods["String"] = make(map[string]*ir.Func)
	}

	// Register the String methods (connect runtime functions to String class methods)
	cg.methods["String"]["concat"] = cg.runtimeFuncs["string_concat"]
	cg.methods["String"]["length"] = cg.runtimeFuncs["string_length"]
	cg.methods["String"]["substr"] = cg.runtimeFuncs["string_substr"]
}
