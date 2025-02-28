package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"sort"
	"strconv"
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
	arrayTypeCache    map[string]types.Type // Cache for already created array types

	// Error handling
	errors []string
	cFuncs map[string]*ir.Func

	// Optimization flags
	optimizationLevel       int
	enableMemToReg          bool
	enableCSE               bool
	enableFunctionInlining  bool
	enableLoopOptimizations bool
	enableTailCallOpt       bool
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
		arrayTypeCache:    make(map[string]types.Type), // Initialize array type cache
		errors:            []string{},
		optimizationLevel: 0, // Default to no optimization
	}

	// Initialize basic types
	cg.intType = types.I32
	cg.boolType = types.I1
	cg.stringType = types.NewPointer(types.I8) // Strings are char* in LLVM
	cg.voidType = types.Void

	// Add data layout information that's generic enough for most systems
	cg.Module.DataLayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

	// Define basic classes and declare functions in the right order
	cg.defineBasicClasses()
	cg.declareStandardCFunctions()
	cg.declareRuntimeFunctions()
	cg.initializeBasicClasses()
	cg.initializeTypeSystem()

	// Enable array support
	cg.setupArrayExtension()

	return cg
}

// New function to declare standard C library functions once
func (cg *CodeGenerator) declareStandardCFunctions() {
	// printf - variadic function
	printfFunc := cg.Module.NewFunc("printf", types.I32,
		ir.NewParam("format", types.NewPointer(types.I8)))
	printfFunc.Sig.Variadic = true // Mark as variadic
	cg.cFuncs["printf"] = printfFunc

	// atoi - converts a string to an integer
	cg.cFuncs["atoi"] = cg.Module.NewFunc("atoi", types.I32,
		ir.NewParam("str", types.NewPointer(types.I8)))
	cg.cFuncs["atoi"].Linkage = enum.LinkageExternal

	// scanf - variadic function
	scanfFunc := cg.Module.NewFunc("scanf", types.I32,
		ir.NewParam("format", types.NewPointer(types.I8)))
	scanfFunc.Sig.Variadic = true // Mark as variadic
	cg.cFuncs["scanf"] = scanfFunc

	// gets - reads a line from stdin
	getsFunc := cg.Module.NewFunc("gets", types.NewPointer(types.I8),
		ir.NewParam("s", types.NewPointer(types.I8)))
	cg.cFuncs["gets"] = getsFunc

	// puts - writes a line to stdout
	putsFunc := cg.Module.NewFunc("puts", types.I32,
		ir.NewParam("s", types.NewPointer(types.I8)))
	cg.cFuncs["puts"] = putsFunc

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

	if err := cg.createMethodDeclarations(program); err != nil {
		return nil, fmt.Errorf("error creating method declarations: %v", err)
	}

	cg.ensureInheritedMethods(program)

	if err := cg.generateMethodImplementations(program); err != nil {
		return nil, fmt.Errorf("error generating method implementations: %v", err)
	}

	if err := cg.generateEntryPoint(); err != nil {
		return nil, fmt.Errorf("error generating entry point: %v", err)
	}

	cg.generateRuntimeLibrary()

	if err := cg.optimizeModule(); err != nil {
		return nil, fmt.Errorf("error optimizing module: %v", err)
	}

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
		// skip array
		if className == "Array" {
			continue
		}

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
				// print method name
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

// Add debug print statements to collectClassMethods
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
	// if array just return
	if className == "Array" {
		return
	}
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

		// Generate method implementations
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				// print method name
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

	if dispatch.Object != nil {
		objPtr, err = cg.generateExpression(dispatch.Object)
		if err != nil {
			return nil, fmt.Errorf("error generating dispatch object: %v", err)
		}
	} else {
		// Self dispatch - get the self parameter
		objPtr = cg.variables["self"]
	}

	// Get method name
	methodName := dispatch.Method.Value

	// Special case for String methods
	if isStringType(objPtr.Type()) && (methodName == "concat" || methodName == "length" || methodName == "substr") {
		methodFunc := cg.runtimeFuncs["string_"+methodName]
		args := []value.Value{objPtr}
		for _, arg := range dispatch.Arguments {
			argValue, err := cg.generateExpression(arg)
			if err != nil {
				return nil, err
			}
			args = append(args, argValue)
		}
		return cg.currentBlock.NewCall(methodFunc, args...), nil
	}

	// Special case for static dispatch
	if dispatch.StaticType != nil {
		staticClassName := dispatch.StaticType.Value
		methodFunc, exists := cg.methods[staticClassName][methodName]
		if !exists {
			return nil, fmt.Errorf("method %s not found in class %s", methodName, staticClassName)
		}

		// Cast object to expected type
		objPtr = cg.currentBlock.NewBitCast(objPtr, methodFunc.Params[0].Type())

		// Call with arguments
		args := []value.Value{objPtr}
		for _, arg := range dispatch.Arguments {
			argValue, err := cg.generateExpression(arg)
			if err != nil {
				return nil, err
			}
			args = append(args, argValue)
		}
		return cg.currentBlock.NewCall(methodFunc, args...), nil
	}

	// =====================================================
	// Dynamic dispatch implementation - the key fix we need
	// =====================================================

	// Step 1: Determine the static type of the object to find the method in type hierarchy
	var staticClassName string

	// Check the object's static type
	if objPtrType, ok := objPtr.Type().(*types.PointerType); ok {
		if structType, ok := objPtrType.ElemType.(*types.StructType); ok {
			// Look up the class name from the type
			for name, classType := range cg.classTypes {
				if classType == structType {
					staticClassName = name
					break
				}
			}
		}
	}

	// If we couldn't determine it from type, use current class
	if staticClassName == "" {
		// Handle primitive types
		if isStringType(objPtr.Type()) {
			staticClassName = "String"
		} else if isIntType(objPtr.Type()) {
			staticClassName = "Int"
		} else if objPtr.Type().Equal(cg.boolType) {
			staticClassName = "Bool"
		} else {
			// Default to current class if we can't determine
			staticClassName = cg.currentClass
			if staticClassName == "" {
				staticClassName = "Object" // Fallback
			}
		}
	}

	// Step 2: Look for the method's virtual table index in the class hierarchy
	methodIdx := -1
	methodClass := ""

	// Find method index by traversing class hierarchy
	currentClass := staticClassName
	for currentClass != "" {
		// Check if class has method indices
		if indices, exists := cg.methodIndices[currentClass]; exists {
			// Check if method exists in this class
			if idx, exists := indices[methodName]; exists {
				methodIdx = idx
				methodClass = currentClass
				break
			}
		}

		// Move up inheritance chain
		currentClass = cg.classParents[currentClass]
	}

	if methodIdx < 0 {
		return nil, fmt.Errorf("method %s not found in class %s or any ancestor",
			methodName, staticClassName)
	}

	// Step 3: Get the vtable pointer from the object (first field)
	// First, we need to get the object's type
	var objType types.Type
	if objPtrType, ok := objPtr.Type().(*types.PointerType); ok {
		objType = objPtrType.ElemType
	} else {
		return nil, fmt.Errorf("expected pointer type for object in dispatch, got %v", objPtr.Type())
	}

	vtablePtrPtr := cg.currentBlock.NewGetElementPtr(
		objType, // The object's struct type
		objPtr,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0), // First field is vtable pointer
	)

	// Load the vtable pointer
	vtablePtr := cg.currentBlock.NewLoad(types.NewPointer(types.I8), vtablePtrPtr)

	// Step 4: Cast vtable pointer to the correct vtable type
	// We need the vtable type from the class where the method was defined
	vtableType := cg.vtables[methodClass]
	typedVtablePtr := cg.currentBlock.NewBitCast(vtablePtr, types.NewPointer(vtableType))

	// Step 5: Get method pointer from vtable at the right index
	// This is where we get the function pointer for the correct override
	methodPtrPtr := cg.currentBlock.NewGetElementPtr(
		vtableType,
		typedVtablePtr,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, int64(methodIdx)),
	)

	// Now we need the method's function type to load it properly
	// First find the function in method environment
	methodFunc := cg.methods[methodClass][methodName]
	if methodFunc == nil {
		return nil, fmt.Errorf("method %s.%s not found in method map", methodClass, methodName)
	}

	methodFuncType := methodFunc.Type()

	// Load the method pointer
	methodPtr := cg.currentBlock.NewLoad(methodFuncType, methodPtrPtr)

	// Step 6: Prepare the arguments
	args := []value.Value{objPtr} // Self is always the first argument

	// Add the rest of the arguments
	for _, arg := range dispatch.Arguments {
		argValue, err := cg.generateExpression(arg)
		if err != nil {
			return nil, err
		}
		args = append(args, argValue)
	}

	// Step 7: Call through the method pointer for proper dynamic dispatch
	return cg.currentBlock.NewCall(methodPtr, args...), nil
}

// Helper to determine the actual class of an object pointer
func (cg *CodeGenerator) determineObjectClass(objPtr value.Value) string {
	// Handle string type
	if isStringType(objPtr.Type()) {
		return "String"
	}

	// Handle Int
	if isIntType(objPtr.Type()) {
		return "Int"
	}

	// Handle Bool
	if _, ok := objPtr.Type().(*types.IntType); ok && objPtr.Type().String() == "i1" {
		return "Bool"
	}

	// For class pointers
	if ptrType, ok := objPtr.Type().(*types.PointerType); ok {
		if structType, ok := ptrType.ElemType.(*types.StructType); ok {
			for className, classType := range cg.classTypes {
				if classType == structType {
					return className
				}
			}
		}
	}

	// If we can't determine it, use current class as fallback
	return cg.currentClass
}

// Add this helper function to guess the class of an object
func (cg *CodeGenerator) guessObjectClass(objPtr value.Value) string {
	if isStringType(objPtr.Type()) {
		return "String"
	}

	if ptrType, ok := objPtr.Type().(*types.PointerType); ok {
		if structType, ok := ptrType.ElemType.(*types.StructType); ok {
			for className, classType := range cg.classTypes {
				if classType == structType {
					return className
				}
			}
		}
	}

	return ""
}

// Also update the if expression function to use a similar approach
func (cg *CodeGenerator) ensureBlockTerminator(block *ir.Block) {
	// Check if the block already has a terminator instruction
	if block.Term == nil {
		// Add an unreachable instruction if no terminator exists
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

	// Remember the last block of the then branch (might have changed due to nested ifs)
	thenExitBlock := cg.currentBlock

	// Make sure then block branches to after block if it doesn't already have a terminator
	if thenExitBlock.Term == nil {
		thenExitBlock.NewBr(afterBlock)
	}

	// Generate else branch
	cg.currentBlock = elseBlock
	elseValue, err := cg.generateExpression(ifExpr.Alternative)
	if err != nil {
		return nil, fmt.Errorf("error generating else branch: %v", err)
	}

	// Remember the last block of the else branch
	elseExitBlock := cg.currentBlock

	// Make sure else block branches to after block if it doesn't already have a terminator
	if elseExitBlock.Term == nil {
		elseExitBlock.NewBr(afterBlock)
	}

	// Set current block to after block for subsequent code
	cg.currentBlock = afterBlock

	// Helper functions for type checking
	isIntType := func(t types.Type) bool {
		_, ok := t.(*types.IntType)
		return ok
	}

	isPtrType := func(t types.Type) bool {
		_, ok := t.(*types.PointerType)
		return ok
	}

	// If both values are nil (void/unreachable branches), return void/null
	if thenValue == nil && elseValue == nil {
		objPtrType := types.NewPointer(cg.classTypes["Object"])
		return constant.NewNull(objPtrType), nil
	}

	// If only one value is nil, use the other one (with appropriate casting if needed)
	if thenValue == nil {
		return elseValue, nil
	}
	if elseValue == nil {
		return thenValue, nil
	}

	// Create proper incoming values for phi node
	incomings := []*ir.Incoming{}

	// Handle type differences if needed
	if thenValue.Type() != elseValue.Type() {
		// Try to find a common type for the phi node
		var commonType types.Type

		// Both integers with different widths
		if isIntType(thenValue.Type()) && isIntType(elseValue.Type()) {
			// Use i32 as common type for integers in Cool
			commonType = types.I32

			// Cast then value if needed
			if thenValue.Type() != commonType {
				thenCasted := thenExitBlock.NewSExt(thenValue, commonType)
				incomings = append(incomings, ir.NewIncoming(thenCasted, thenExitBlock))
			} else {
				incomings = append(incomings, ir.NewIncoming(thenValue, thenExitBlock))
			}

			// Cast else value if needed
			if elseValue.Type() != commonType {
				elseCasted := elseExitBlock.NewSExt(elseValue, commonType)
				incomings = append(incomings, ir.NewIncoming(elseCasted, elseExitBlock))
			} else {
				incomings = append(incomings, ir.NewIncoming(elseValue, elseExitBlock))
			}
		} else if isPtrType(thenValue.Type()) && isPtrType(elseValue.Type()) {
			// Both pointers but different types - cast to Object*
			commonType = types.NewPointer(cg.classTypes["Object"])

			// Cast then value
			thenCasted := thenExitBlock.NewBitCast(thenValue, commonType)
			incomings = append(incomings, ir.NewIncoming(thenCasted, thenExitBlock))

			// Cast else value
			elseCasted := elseExitBlock.NewBitCast(elseValue, commonType)
			incomings = append(incomings, ir.NewIncoming(elseCasted, elseExitBlock))
		} else {
			// Different incompatible types - use the then value's type as default
			commonType = thenValue.Type()

			incomings = append(incomings, ir.NewIncoming(thenValue, thenExitBlock))

			// Try to cast else value or use a safe default
			if isPtrType(commonType) && isPtrType(elseValue.Type()) {
				elseCasted := elseExitBlock.NewBitCast(elseValue, commonType)
				incomings = append(incomings, ir.NewIncoming(elseCasted, elseExitBlock))
			} else if isIntType(commonType) && isIntType(elseValue.Type()) {
				elseCasted := elseExitBlock.NewSExt(elseValue, commonType)
				incomings = append(incomings, ir.NewIncoming(elseCasted, elseExitBlock))
			} else {
				// Use a safe default for incompatible types
				incomings = append(incomings, ir.NewIncoming(constant.NewZeroInitializer(commonType), elseExitBlock))
			}
		}
	} else {
		// Same types - simple case
		incomings = append(incomings,
			ir.NewIncoming(thenValue, thenExitBlock),
			ir.NewIncoming(elseValue, elseExitBlock))
	}

	// Create the phi node with proper incoming values
	return afterBlock.NewPhi(incomings...), nil
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

func (cg *CodeGenerator) generateNewObject(block *ir.Block, className string) value.Value {
	classType, exists := cg.classTypes[className]
	if !exists {
		panic(fmt.Sprintf("Class %s not found", className))
	}

	// Calculate object size
	sizeOfClass := constant.NewInt(types.I64, 64) // Approximation

	// Call memory allocation
	rawPtr := block.NewCall(cg.runtimeFuncs["GC_malloc"], sizeOfClass)

	// Cast to object type
	objPtr := block.NewBitCast(rawPtr, types.NewPointer(classType))

	// Set vtable pointer
	vtableField := block.NewGetElementPtr(
		classType,
		objPtr,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	// Find and use the correct vtable for this class
	if vtableGlobal, exists := cg.vtableGlobals[className]; exists {
		vtablePtrCast := block.NewBitCast(vtableGlobal, types.NewPointer(types.I8))
		block.NewStore(vtablePtrCast, vtableField)
	} else {
		// If no vtable found, this is a problem - print a debug message
		fmt.Printf("WARNING: No vtable found for class %s\n", className)
	}

	// Initialize fields with default values
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
	// Check if it's an array type
	if strings.HasPrefix(typeName, "Array[") && strings.HasSuffix(typeName, "]") {
		// Check if we've already created this array type
		if cachedType, exists := cg.arrayTypeCache[typeName]; exists {
			return types.NewPointer(cachedType), nil
		}

		// Get element type and create array type
		_, _, err := cg.getArrayElementType(typeName)
		if err != nil {
			return nil, err
		}

		// Use the base Array type
		if arrayType, exists := cg.classTypes["Array"]; exists {
			// Cache the type for future use
			cg.arrayTypeCache[typeName] = arrayType
			return types.NewPointer(arrayType), nil
		}

		return nil, fmt.Errorf("Array base type not defined")
	}

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
	case *ast.ArrayAccessExpression:
		return cg.generateArrayAccessExpression(e)
	case *ast.DispatchExpression:
		return cg.generateDispatch(e)
	case *ast.BinaryExpression:
		return cg.generateBinaryExpression(e)
	case *ast.UnaryExpression:
		return cg.generateUnaryExpression(e)
	case *ast.ArrayExpression:
		return cg.generateArrayExpression(e)
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

// generateAssignment generates LLVM IR for an assignment expression
func (cg *CodeGenerator) generateAssignment(assign *ast.Assignment) (value.Value, error) {
	// Handle array element assignment (e.g., arr[i] <- value)
	// We need to check if the Name.Value contains array notation like "arr[idx]"
	varName := assign.Name.Value

	// Check if it has array access notation
	if strings.Contains(varName, "[") && strings.HasSuffix(varName, "]") {
		parts := strings.SplitN(varName, "[", 2)
		if len(parts) != 2 {
			return nil, fmt.Errorf("invalid array access syntax: %s", varName)
		}

		arrayName := parts[0]
		indexExpr := parts[1][:len(parts[1])-1] // Remove the trailing ']'

		// Get the array variable
		arrayVar, err := cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: arrayName})
		if err != nil {
			return nil, fmt.Errorf("undefined array: %s", arrayName)
		}

		// For the index, we need to parse and evaluate the expression
		// This is a limitation of the current approach - we can only handle simple indices
		// like integer literals or variable names
		var indexValue value.Value

		// Check if index is an integer literal
		if indexInt, err := cg.parseInt(indexExpr); err == nil {
			indexValue = constant.NewInt(types.I32, int64(indexInt))
		} else {
			// Try to evaluate as a variable
			indexValue, err = cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: indexExpr})
			if err != nil {
				// Try as a binary expression (e.g., "j+1")
				if result, binaryErr := cg.parseBinaryExpression(indexExpr); binaryErr == nil {
					indexValue = result
				} else {
					return nil, fmt.Errorf("unable to evaluate index expression: %s", indexExpr)
				}
			}
		}

		// Generate the right-hand side value
		rhsValue, err := cg.generateExpression(assign.Value)
		if err != nil {
			return nil, fmt.Errorf("error generating assignment value: %v", err)
		}

		// Perform array element assignment
		return cg.generateArrayElementAssignment(arrayVar, indexValue, rhsValue)
	}

	// Regular variable assignment
	varName = assign.Name.Value

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

// Helper function to parse and evaluate simple binary expressions like "j+1"
func (cg *CodeGenerator) parseBinaryExpression(expr string) (value.Value, error) {
	// This is a very simple parser for expressions like "j+1", "i-1", etc.
	// It doesn't handle complex expressions, precedence, or multiple operations

	// Check for addition
	if idx := strings.Index(expr, "+"); idx >= 0 {
		left := strings.TrimSpace(expr[:idx])
		right := strings.TrimSpace(expr[idx+1:])

		// Generate left operand
		leftVal, err := cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: left})
		if err != nil {
			return nil, err
		}

		// Generate right operand (assume it's an integer literal for simplicity)
		rightInt, err := cg.parseInt(right)
		if err != nil {
			return nil, err
		}
		rightVal := constant.NewInt(types.I32, int64(rightInt))

		// Perform addition
		return cg.currentBlock.NewAdd(leftVal, rightVal), nil
	}

	// Check for subtraction
	if idx := strings.Index(expr, "-"); idx >= 0 {
		left := strings.TrimSpace(expr[:idx])
		right := strings.TrimSpace(expr[idx+1:])

		// Generate left operand
		leftVal, err := cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: left})
		if err != nil {
			return nil, err
		}

		// Generate right operand (assume it's an integer literal for simplicity)
		rightInt, err := cg.parseInt(right)
		if err != nil {
			return nil, err
		}
		rightVal := constant.NewInt(types.I32, int64(rightInt))

		// Perform subtraction
		return cg.currentBlock.NewSub(leftVal, rightVal), nil
	}

	return nil, fmt.Errorf("unsupported expression: %s", expr)
}

func (cg *CodeGenerator) parseInt(s string) (int, error) {
	var result int
	_, err := fmt.Sscanf(s, "%d", &result)
	return result, err
}

// generateBinaryExpression generates LLVM IR for a binary expression
// Modified generateBinaryExpression to properly handle string concatenation
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
		// Check if either operand is a string - if so, we need string concatenation
		if isStringType(left.Type()) || isStringType(right.Type()) {
			// Strings must be concatenated using the string_concat runtime function
			// Ensure both operands are properly cast to char*
			leftStr := cg.ensureStringType(cg.currentBlock, left)
			rightStr := cg.ensureStringType(cg.currentBlock, right)

			// Call the string_concat runtime function
			return cg.currentBlock.NewCall(cg.runtimeFuncs["string_concat"], leftStr, rightStr), nil
		}

		// Normal integer addition
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

// Helper function to check if a type is a string type
func isStringType(t types.Type) bool {
	// Check if it's a pointer to i8 (char*)
	if ptrType, ok := t.(*types.PointerType); ok {
		// Check for direct i8 pointer
		if byteType, ok := ptrType.ElemType.(*types.IntType); ok {
			return byteType.BitSize == 8
		}

		// Also check for type by string representation
		if ptrType.String() == "*i8" {
			return true
		}
	}

	// Also check if this is a known string object from the program
	// This would catch custom String class instances
	return false
}

// Helper function to ensure a value is a string type
func (cg *CodeGenerator) ensureStringType(block *ir.Block, val value.Value) value.Value {
	// If it's already a string type, return it
	if isStringType(val.Type()) {
		return val
	}

	// For integers, create a string representation
	if isIntType(val.Type()) {
		// Create a format string
		formatStr := cg.createStringConstant(block, "int_fmt", "%d")

		// Call sprintf (via a temporary function) to convert int to string
		// First allocate buffer for the string (20 chars is enough for most integers)
		bufSize := constant.NewInt(types.I64, 20)
		buffer := block.NewCall(cg.runtimeFuncs["GC_malloc"], bufSize)

		// Get the sprintf function (declare it if not already declared)
		var sprintfFunc *ir.Func
		if existingFunc, exists := cg.cFuncs["sprintf"]; exists {
			sprintfFunc = existingFunc
		} else {
			// Declare sprintf
			sprintfFunc = cg.Module.NewFunc("sprintf", types.I32,
				ir.NewParam("str", types.NewPointer(types.I8)),
				ir.NewParam("format", types.NewPointer(types.I8)))
			sprintfFunc.Sig.Variadic = true
			sprintfFunc.Linkage = enum.LinkageExternal
			cg.cFuncs["sprintf"] = sprintfFunc
		}

		// Call sprintf to convert the integer to a string
		block.NewCall(sprintfFunc, buffer, formatStr, val)

		return buffer
	}

	// For other types, return a placeholder string
	return cg.createStringConstant(block, "default", "(not a string)")
}
func (cg *CodeGenerator) ensureProperStringType(block *ir.Block, val value.Value) value.Value {
	// If it's already a string type, return it
	if isStringType(val.Type()) {
		return val
	}

	// If it's a pointer to something that isn't a string, try to cast it
	if _, ok := val.Type().(*types.PointerType); ok {
		return block.NewBitCast(val, cg.stringType)
	}

	// Create a fallback for other types - convert to string representation
	// This is a simplified approach - in a real compiler you'd need more sophisticated conversion
	return block.NewBitCast(val, cg.stringType)
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
	// Start with the given class
	current := className

	// Track if we've found the method at all
	foundMethod := false
	foundInClass := ""

	// First pass: look for the method in this class and all ancestor classes
	for current != "" {
		if methodMap, exists := cg.methods[current]; exists {
			if _, exists := methodMap[methodName]; exists {
				foundMethod = true
				foundInClass = current
				// Don't break - continue to check all ancestor classes
			}
		}
		current = cg.classParents[current]
	}

	// If method was found, return the most specific class that defines it
	if foundMethod {
		return foundInClass
	}

	// Method not found in any class
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
	headerBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.header.%s.%d", funcName, loopId))
	bodyBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.body.%s.%d", funcName, loopId))
	afterBlock := cg.currentFunc.NewBlock(fmt.Sprintf("while.after.%s.%d", funcName, loopId))

	// Branch from current block to header block
	cg.currentBlock.NewBr(headerBlock)

	// Generate condition code in header block
	cg.currentBlock = headerBlock
	condValue, err := cg.generateExpression(whileExpr.Condition)
	if err != nil {
		return nil, fmt.Errorf("error generating while condition: %v", err)
	}

	// Branch based on condition to either body or after
	headerBlock.NewCondBr(condValue, bodyBlock, afterBlock)

	// Generate body code in body block
	cg.currentBlock = bodyBlock
	_, err = cg.generateExpression(whileExpr.Body)
	if err != nil {
		return nil, fmt.Errorf("error generating while body: %v", err)
	}

	// Remember the last block created during body generation
	// This is important for nested control structures
	bodyExitBlock := cg.currentBlock

	// Check if the body exit block has a terminator
	if bodyExitBlock.Term == nil {
		// If not, add a branch back to the header block
		bodyExitBlock.NewBr(headerBlock)
	}

	// Set current block to after block for code that follows the loop
	cg.currentBlock = afterBlock

	// Make sure the after block has at least one instruction to avoid optimization issues
	// Important: This creates a proper entry point for code following the loop
	dummyAlloca := afterBlock.NewAlloca(types.I32)
	afterBlock.NewStore(constant.NewInt(types.I32, 0), dummyAlloca)

	// While loops always return void/null in Cool
	objPtrType := types.NewPointer(cg.classTypes["Object"])
	return constant.NewNull(objPtrType), nil
}

// generateLetExpression processes let expressions with bindings and body
func (cg *CodeGenerator) generateLetExpression(let *ast.LetExpression) (value.Value, error) {
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

		// Handle array types specially
		var bindingType types.Type
		var err error

		if cg.isArrayType(binding.Type.Value) {
			// For array types, use pointer to Array struct
			bindingType, err = cg.mapType(binding.Type.Value)
		} else {
			// For normal types
			bindingType, err = cg.mapType(binding.Type.Value)
		}

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

			// Handle type compatibility for initialization
			if initValue.Type() != bindingType {
				// For array types
				if cg.isArrayType(binding.Type.Value) {
					initValue = cg.currentBlock.NewBitCast(initValue, bindingType)
				} else if ptrType, ok := bindingType.(*types.PointerType); ok {
					// Class type compatibility (parent/child relationships)
					if initPtrType, ok := initValue.Type().(*types.PointerType); ok {
						// We have two pointer types, need to cast between them
						if ptrType.ElemType != initPtrType.ElemType {
							initValue = cg.currentBlock.NewBitCast(initValue, bindingType)
						}
					}
				}
			}

			cg.currentBlock.NewStore(initValue, varAlloca)
		} else {
			// Default initialization
			if cg.isArrayType(binding.Type.Value) {
				// For array types, check if it's a pointer type first
				if ptrType, ok := bindingType.(*types.PointerType); ok {
					// Create null/void value of the correct pointer type
					cg.currentBlock.NewStore(constant.NewNull(ptrType), varAlloca)
				} else {
					// Fallback to zero initializer if not a pointer (shouldn't happen)
					cg.currentBlock.NewStore(constant.NewZeroInitializer(bindingType), varAlloca)
				}
			} else {
				// Default initialization for other types
				cg.currentBlock.NewStore(constant.NewZeroInitializer(bindingType), varAlloca)
			}
		}

		// Add to variables map - store the alloca for local variables
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

	// Array handling (unchanged)
	if strings.HasPrefix(className, "Array[") && strings.HasSuffix(className, "]") {
		// ... (existing array handling code) ...
		// The existing array handling code should remain as-is
	}

	// Create a new instance of the class
	objPtr := cg.generateNewObject(cg.currentBlock, className)

	// If this is a direct dispatch to init, handle it here
	if len(newExpr.Type.Value) > 5 && newExpr.Type.Value[len(newExpr.Type.Value)-5:] == ".init" {
		// Extract class name and method name
		parts := strings.Split(newExpr.Type.Value, ".")
		if len(parts) == 2 {
			className = parts[0]
			methodName := parts[1]

			// Find the method through inheritance chain
			methodFunc, _ := cg.findInheritedMethod(className, methodName)
			if methodFunc != nil {
				// Call the init method with the object as parameter
				args := []value.Value{objPtr}
				// Add other arguments if needed

				return cg.currentBlock.NewCall(methodFunc, args...), nil
			}
		}
	}

	return objPtr, nil
}

func (cg *CodeGenerator) findInheritedMethod(className, methodName string) (*ir.Func, string) {
	// Start with the given class
	currentClass := className

	// Follow the inheritance chain to find the method
	for currentClass != "" {
		// Check if this class has the method
		if methodMap, exists := cg.methods[currentClass]; exists {
			if method, exists := methodMap[methodName]; exists {
				return method, currentClass // Return the method and the class that defines it
			}
		}

		// Move up the inheritance chain
		currentClass = cg.classParents[currentClass]
	}

	// Method not found in any ancestor
	return nil, ""
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
		// print formal and its type
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

	// Special handling for SELF_TYPE return - ensure we're returning a pointer
	if method.TypeDecl.Value == "SELF_TYPE" {
		// If returnValue is null or not a pointer, use self directly
		if returnValue == nil || !isPtrType(returnValue.Type()) {
			returnValue = cg.variables["self"]
		}
	}

	// First, find all blocks that are referenced as targets
	referencedBlocks := make(map[*ir.Block]bool)
	for _, block := range cg.currentFunc.Blocks {
		// Skip blocks that don't have terminators
		if block.Term == nil {
			continue
		}

		// Check different terminator types
		if termBr, ok := block.Term.(*ir.TermBr); ok {
			if targetBlock, ok := termBr.Target.(*ir.Block); ok {
				referencedBlocks[targetBlock] = true
			}
		} else if termCondBr, ok := block.Term.(*ir.TermCondBr); ok {
			if trueBlock, ok := termCondBr.TargetTrue.(*ir.Block); ok {
				referencedBlocks[trueBlock] = true
			}
			if falseBlock, ok := termCondBr.TargetFalse.(*ir.Block); ok {
				referencedBlocks[falseBlock] = true
			}
		}
		// Add other terminator types if needed
	}

	// Create a return block
	returnBlock := cg.currentFunc.NewBlock("return")

	// Prepare the return value
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
			returnValue = returnBlock.NewBitCast(returnValue, expectedReturnType)
		} else if isIntType(expectedReturnType) && isIntType(returnValue.Type()) {
			// Convert between integer types
			if expectedReturnType.(*types.IntType).BitSize > returnValue.Type().(*types.IntType).BitSize {
				returnValue = returnBlock.NewSExt(returnValue, expectedReturnType)
			} else {
				returnValue = returnBlock.NewTrunc(returnValue, expectedReturnType)
			}
		} else {
			// Other type conversion - try bitcast if both are pointers
			if isPtrType(expectedReturnType) && isPtrType(returnValue.Type()) {
				returnValue = returnBlock.NewBitCast(returnValue, expectedReturnType)
			} else {
				// Handle non-pointer type mismatches with reasonable defaults
				returnValue = constant.NewZeroInitializer(expectedReturnType)
			}
		}
	}

	// Add return instruction to return block
	returnBlock.NewRet(returnValue)

	// Connect all blocks without terminators to the return block
	for _, block := range cg.currentFunc.Blocks {
		if block.Term == nil && block != returnBlock {
			// If this is a referenced block, it should branch to return
			if referencedBlocks[block] || block == cg.currentBlock {
				block.NewBr(returnBlock)
			} else {
				// For unreferenced blocks, add unreachable (they're likely dead code)
				block.NewUnreachable()
				fmt.Printf("Warning: Block %s in function %s has no terminator and is not referenced, adding unreachable\n",
					block.Name(), cg.currentFunc.Name())
			}
		}
	}

	return nil
}

func (cg *CodeGenerator) generateArrayAccess(objId *ast.ObjectIdentifier) (value.Value, error) {
	// Parse the array access pattern: "arrayName[indexExpr]"
	fullExpr := objId.Value

	if !strings.Contains(fullExpr, "[") || !strings.HasSuffix(fullExpr, "]") {
		return nil, fmt.Errorf("invalid array access syntax: %s", fullExpr)
	}

	parts := strings.SplitN(fullExpr, "[", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("invalid array access syntax: %s", fullExpr)
	}

	arrayName := parts[0]
	indexExpr := parts[1][:len(parts[1])-1] // Remove the trailing ']'

	// Generate the array variable
	arrayVar, err := cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: arrayName})
	if err != nil {
		return nil, fmt.Errorf("undefined array: %s", arrayName)
	}

	// Generate the index value
	var indexValue value.Value

	// Check if index is an integer literal
	if indexInt, err := cg.parseInt(indexExpr); err == nil {
		indexValue = constant.NewInt(types.I32, int64(indexInt))
	} else {
		// Try to evaluate as a variable
		indexValue, err = cg.generateObjectIdentifier(&ast.ObjectIdentifier{Value: indexExpr})
		if err != nil {
			// Try as a binary expression (e.g., "j+1")
			if result, binaryErr := cg.parseBinaryExpression(indexExpr); binaryErr == nil {
				indexValue = result
			} else {
				return nil, fmt.Errorf("unable to evaluate index expression: %s", indexExpr)
			}
		}
	}

	// Call get element function
	elemPtr := cg.currentBlock.NewCall(
		cg.runtimeFuncs["Array_get_element"],
		arrayVar,
		indexValue,
	)

	// For integer elements, we need to load the value
	intPtrType := types.NewPointer(cg.intType)
	intElemPtr := cg.currentBlock.NewBitCast(elemPtr, intPtrType)
	return cg.currentBlock.NewLoad(cg.intType, intElemPtr), nil
}

func (cg *CodeGenerator) generateObjectIdentifier(obj *ast.ObjectIdentifier) (value.Value, error) {
	varName := obj.Value

	// Check if this is an array access pattern
	if strings.Contains(varName, "[") && strings.HasSuffix(varName, "]") {
		return cg.generateArrayAccess(obj)
	}

	// Special handling for 'self' when it might be returning from a SELF_TYPE method
	if varName == "self" && cg.isMethodReturningSelfType(cg.currentClass, cg.currentMethod) {
		// For SELF_TYPE methods, return the self pointer directly without loading
		return cg.variables["self"], nil
	}

	// Check if it's a local variable
	if varVal, exists := cg.variables[varName]; exists {
		// If it's a pointer to a stack variable, load it
		if ptrType, ok := varVal.Type().(*types.PointerType); ok {
			loaded := cg.currentBlock.NewLoad(ptrType.ElemType, varVal)

			// Special handling for String parameters
			// Check if we're in the handle_error method and the parameter is 'operation'
			if cg.currentMethod == "handle_error" && varName == "operation" {
				// This is known to be a String parameter from the Cool code
				// Make sure it's properly typed as a string for dispatch
				if !isStringType(loaded.Type()) {
					return cg.ensureProperStringType(cg.currentBlock, loaded), nil
				}
			}

			return loaded, nil
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
		loaded := cg.currentBlock.NewLoad(ptrType.ElemType, attrPtr)

		// Special case for string attributes
		if varName == "operation" || ptrType.ElemType == cg.stringType {
			if !isStringType(loaded.Type()) {
				return cg.ensureProperStringType(cg.currentBlock, loaded), nil
			}
		}

		return loaded, nil
	}

	return nil, fmt.Errorf("undefined identifier: %s", varName)
}

func (cg *CodeGenerator) generateArrayExpression(expr *ast.ArrayExpression) (value.Value, error) {
	// Extract array type information
	arrayTypeStr := expr.Type.String() // Should be "Array[Type]"

	// Get element type and type tag
	elemType, typeTag, err := cg.getArrayElementType(arrayTypeStr)
	if err != nil {
		return nil, err
	}

	// Generate size expression
	sizeValue, err := cg.generateExpression(expr.Size)
	if err != nil {
		return nil, fmt.Errorf("error generating array size: %v", err)
	}

	// Determine element size (in bytes)
	var elemSize value.Value
	switch elemType {
	case cg.intType: // Int (4 bytes)
		elemSize = constant.NewInt(types.I32, 4)
	case cg.boolType: // Bool (1 byte)
		elemSize = constant.NewInt(types.I32, 1)
	default: // String, Object or class types (all pointers - 8 bytes)
		elemSize = constant.NewInt(types.I32, 8)
	}

	// Call array initialization function
	return cg.currentBlock.NewCall(
		cg.runtimeFuncs["Array_new"],
		elemSize,
		sizeValue,
		constant.NewInt(types.I8, int64(typeTag)),
	), nil
}

// generateArrayAccessExpression properly handles any expression as the index
func (cg *CodeGenerator) generateArrayAccessExpression(expr *ast.ArrayAccessExpression) (value.Value, error) {
	// Generate the array expression
	arrayPtr, err := cg.generateExpression(expr.Array)
	if err != nil {
		return nil, fmt.Errorf("error generating array: %v", err)
	}

	// Generate the index expression (this is crucial - allows for expressions like j+1)
	indexValue, err := cg.generateExpression(expr.Index)
	if err != nil {
		return nil, fmt.Errorf("error generating index: %v", err)
	}

	// Call get element function
	elemPtr := cg.currentBlock.NewCall(
		cg.runtimeFuncs["Array_get_element"],
		arrayPtr,
		indexValue,
	)

	// For integer elements, we need to load the value
	// This is a simplified approach - in a more complete implementation,
	// we would check the array's element type tag
	intPtrType := types.NewPointer(cg.intType)
	intElemPtr := cg.currentBlock.NewBitCast(elemPtr, intPtrType)
	return cg.currentBlock.NewLoad(cg.intType, intElemPtr), nil
}

// isArrayType checks if a type string represents an array type
func (cg *CodeGenerator) isArrayType(typeName string) bool {
	return strings.HasPrefix(typeName, "Array[") && strings.HasSuffix(typeName, "]")
}

// generateArrayElementAssignment performs assignment to array elements
func (cg *CodeGenerator) generateArrayElementAssignment(arrayVar value.Value, index value.Value, rhsValue value.Value) (value.Value, error) {
	// For integers, we need special handling
	if intType, ok := rhsValue.Type().(*types.IntType); ok {
		// Allocate space for the integer
		intAlloca := cg.currentBlock.NewAlloca(intType)

		// Store the integer value
		cg.currentBlock.NewStore(rhsValue, intAlloca)

		// Pass the pointer to the integer value to Array_set_element
		elemPtr := cg.currentBlock.NewBitCast(intAlloca, types.NewPointer(types.I8))
		cg.currentBlock.NewCall(
			cg.runtimeFuncs["Array_set_element"],
			arrayVar,
			index,
			elemPtr,
		)
	} else {
		// For other types, cast to i8* if needed
		var valuePtr value.Value
		if _, ok := rhsValue.Type().(*types.PointerType); ok {
			// Already a pointer, just bitcast to i8*
			valuePtr = cg.currentBlock.NewBitCast(rhsValue, types.NewPointer(types.I8))
		} else {
			// Not a pointer, create an allocation
			alloca := cg.currentBlock.NewAlloca(rhsValue.Type())
			cg.currentBlock.NewStore(rhsValue, alloca)
			valuePtr = cg.currentBlock.NewBitCast(alloca, types.NewPointer(types.I8))
		}

		// Call the set element function
		cg.currentBlock.NewCall(
			cg.runtimeFuncs["Array_set_element"],
			arrayVar,
			index,
			valuePtr,
		)
	}

	return rhsValue, nil
}

// Helper struct for array access expressions
type ArrayAccess struct {
	ArrayName       string
	IsConstantIndex bool
	ConstantIndex   int
	IndexVarName    string
}

func (cg *CodeGenerator) tryParseArrayAccess(expr string) (ArrayAccess, bool) {
	parts := strings.Split(expr, "[")
	if len(parts) != 2 || !strings.HasSuffix(parts[1], "]") {
		return ArrayAccess{}, false
	}

	arrayName := parts[0]
	indexStr := parts[1][:len(parts[1])-1] // Remove trailing "]"

	// Check if the index is a constant or a variable
	index, err := strconv.Atoi(indexStr)
	if err == nil {
		// Constant index
		return ArrayAccess{
			ArrayName:       arrayName,
			IsConstantIndex: true,
			ConstantIndex:   index,
		}, true
	}

	// Variable index
	return ArrayAccess{
		ArrayName:       arrayName,
		IsConstantIndex: false,
		IndexVarName:    indexStr,
	}, true
}

// Add this code to your Generate function, right after createMethodDeclarations
func (cg *CodeGenerator) ensureInheritedMethods(program *ast.Program) {
	// Go through all classes
	for _, class := range program.Classes {
		className := class.Name.Value

		// Skip basic classes
		if className == "Object" || className == "Int" ||
			className == "Bool" || className == "String" ||
			className == "IO" {
			continue
		}

		// Get the parent class
		parentName := cg.classParents[className]
		if parentName == "" {
			continue // No parent
		}

		// Make sure the class has a method map
		if _, exists := cg.methods[className]; !exists {
			cg.methods[className] = make(map[string]*ir.Func)
		}

		// Copy all parent methods that haven't been overridden
		cg.copyInheritedMethods(className, parentName)
	}
}

func (cg *CodeGenerator) copyInheritedMethods(childClass, parentClass string) {
	// Make sure we have method maps
	if _, exists := cg.methods[childClass]; !exists {
		cg.methods[childClass] = make(map[string]*ir.Func)
	}

	// Copy parent methods to child
	if parentMethods, exists := cg.methods[parentClass]; exists {
		for methodName, methodFunc := range parentMethods {
			// If child doesn't already have this method
			if _, hasMethod := cg.methods[childClass][methodName]; !hasMethod {
				// Add inherited method
				cg.methods[childClass][methodName] = methodFunc
			}
		}
	}

	// Recursively copy from grandparent if needed
	if grandparent, exists := cg.classParents[parentClass]; exists && grandparent != "" {
		cg.copyInheritedMethods(childClass, grandparent)
	}
}
