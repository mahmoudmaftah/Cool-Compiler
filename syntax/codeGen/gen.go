package codegen

import (
    "cool-compiler/ast"
    "fmt"

    "github.com/llir/llvm/ir"
    "github.com/llir/llvm/ir/constant"
    "github.com/llir/llvm/ir/value"
	"github.com/llir/llvm/ir/types"
)

// Generator holds state for LLVM IR generation
type Generator struct {
    // Main module being generated
    module *ir.Module
    
    // Current function and block for IR generation
    currentFunc *ir.Func
    currentBlock *ir.Block
    
    // Symbol tables
    namedValues map[string]value.Value
    vtables     map[string]*ir.Global
    
    // Type definitions
    coolTypeMap map[string]types.Type
    classTypes  map[string]*types.StructType
    
    // Track current class and method
    currentClass  string
    currentMethod string
}

func NewGenerator() *Generator {
    g := &Generator{
        module:      ir.NewModule(),
        namedValues: make(map[string]value.Value),
        vtables:     make(map[string]*ir.Global),
        coolTypeMap: make(map[string]types.Type),
        classTypes:  make(map[string]*types.StructType),
    }
    g.initializeTypes()
    return g
}

func (g *Generator) initializeTypes() {
    // Define basic Cool types in LLVM
    g.coolTypeMap["Int"] = types.I32
    g.coolTypeMap["Bool"] = types.I1
    
    // String type is a struct containing length and chars
    strType := types.NewStruct(
        types.I32,                     // length
        types.NewArray(0, types.I8),   // chars
    )
    g.coolTypeMap["String"] = types.NewPointer(strType)
    
    // Object type (base class) is a struct with vtable pointer
    objType := types.NewStruct(
        types.NewPointer(types.I8),  // vtable pointer
    )
    g.coolTypeMap["Object"] = types.NewPointer(objType)
}

// Generate LLVM IR for a complete Cool program
func (g *Generator) Generate(program *ast.Program) (*ir.Module, error) {
    // First pass: declare all classes and their types
    if err := g.declareClasses(program); err != nil {
        return nil, err
    }
    
    // Second pass: generate vtables and method implementations
    if err := g.generateClasses(program); err != nil {
        return nil, err
    }
    
    return g.module, nil
}

func (g *Generator) declareClasses(program *ast.Program) error {
    // First declare all class types (needed for recursive references)
    for _, class := range program.Classes {
        structType := types.NewStruct()  // Empty for now
        g.classTypes[class.Name.Value] = structType
        g.coolTypeMap[class.Name.Value] = types.NewPointer(structType)
    }
    
    // Then fill in the struct fields
    for _, class := range program.Classes {
        if err := g.declareClassType(class); err != nil {
            return err
        }
    }
    
    return nil
}

func (g *Generator) declareClassType(class *ast.Class) error {
    g.currentClass = class.Name.Value
    structType := g.classTypes[class.Name.Value]
    
    // Start with vtable pointer
    fields := []types.Type{
        types.NewPointer(types.I8),  // vtable pointer
    }
    
    // Add fields for all attributes
    for _, feature := range class.Features {
        if attr, ok := feature.(*ast.Attribute); ok {
            fieldType, err := g.getLLVMType(attr.TypeDecl.Value)
            if err != nil {
                return err
            }
            fields = append(fields, fieldType)
        }
    }
    
    // Set the actual fields of the struct type
    structType.SetFields(fields)
    
    // Declare methods
    for _, feature := range class.Features {
        if method, ok := feature.(*ast.Method); ok {
            if err := g.declareMethod(method); err != nil {
                return err
            }
        }
    }
    
    return nil
}

func (g *Generator) declareMethod(method *ast.Method) error {
    // Build parameter types
    params := []*ir.Param{
        ir.NewParam("self", g.coolTypeMap[g.currentClass]),
    }
    
    for _, formal := range method.Formals {
        paramType, err := g.getLLVMType(formal.TypeDecl.Value)
        if err != nil {
            return err
        }
        params = append(params, ir.NewParam(formal.Name.Value, paramType))
    }
    
    // Get return type
    retType, err := g.getLLVMType(method.TypeDecl.Value)
    if err != nil {
        return err
    }
    
    // Create function
    name := fmt.Sprintf("%s.%s", g.currentClass, method.Name.Value)
    function := g.module.NewFunc(name, retType, params...)
    
    return nil
}

func (g *Generator) generateClasses(program *ast.Program) error {
    for _, class := range program.Classes {
        if err := g.generateClass(class); err != nil {
            return err
        }
    }
    return nil
}

func (g *Generator) generateClass(class *ast.Class) error {
    g.currentClass = class.Name.Value
    
    // Generate vtable
    if err := g.generateVTable(class); err != nil {
        return err
    }
    
    // Generate method implementations
    for _, feature := range class.Features {
        if method, ok := feature.(*ast.Method); ok {
            if err := g.generateMethod(method); err != nil {
                return err
            }
        }
    }
    
    return nil
}

func (g *Generator) generateMethod(method *ast.Method) error {
    g.currentMethod = method.Name.Value
    funcName := fmt.Sprintf("%s.%s", g.currentClass, method.Name.Value)
    
    // Get the previously declared function
    function := g.module.Func(funcName)
    if function == nil {
        return fmt.Errorf("function %s not found", funcName)
    }
    
    // Create entry block
    g.currentFunc = function
    g.currentBlock = function.NewBlock("entry")
    
    // Add parameters to symbol table
    g.namedValues = make(map[string]value.Value)
    g.namedValues["self"] = function.Params[0]
    for i, param := range function.Params[1:] {
        g.namedValues[method.Formals[i].Name.Value] = param
    }
    
    // Generate code for method body
    returnValue, err := g.generateExpression(method.Body)
    if err != nil {
        return err
    }
    
    // Add return instruction
    g.currentBlock.NewRet(returnValue)
    
    return nil
}

func (g *Generator) generateExpression(expr ast.Expression) (value.Value, error) {
    switch e := expr.(type) {
    case *ast.IntegerLiteral:
        return constant.NewInt(types.I32, int64(e.Value)), nil
        
    case *ast.BooleanLiteral:
        val := int64(0)
        if e.Value {
            val = 1
        }
        return constant.NewInt(types.I1, val), nil
        
    case *ast.BinaryExpression:
        return g.generateBinaryExpression(e)
        
    case *ast.IfExpression:
        return g.generateIfExpression(e)
        
    // Implement other expression types...
        
    default:
        return nil, fmt.Errorf("unsupported expression type: %T", expr)
    }
}

func (g *Generator) generateBinaryExpression(expr *ast.BinaryExpression) (value.Value, error) {
    left, err := g.generateExpression(expr.Left)
    if err != nil {
        return nil, err
    }
    
    right, err := g.generateExpression(expr.Right)
    if err != nil {
        return nil, err
    }
    
    switch expr.Operator {
    case "+":
        return g.currentBlock.NewAdd(left, right), nil
    case "-":
        return g.currentBlock.NewSub(left, right), nil
    case "*":
        return g.currentBlock.NewMul(left, right), nil
    case "/":
        return g.currentBlock.NewSDiv(left, right), nil
    case "<":
        return g.currentBlock.NewICmp(ir.IntSLT, left, right), nil
    case "<=":
        return g.currentBlock.NewICmp(ir.IntSLE, left, right), nil
    case "=":
        return g.currentBlock.NewICmp(ir.IntEQ, left, right), nil
    default:
        return nil, fmt.Errorf("unknown binary operator: %s", expr.Operator)
    }
}

func (g *Generator) generateIfExpression(expr *ast.IfExpression) (value.Value, error) {
    // Generate condition code
    cond, err := g.generateExpression(expr.Condition)
    if err != nil {
        return nil, err
    }

    // Create blocks for then, else, and merge
    thenBlock := g.currentFunc.NewBlock("")
    elseBlock := g.currentFunc.NewBlock("")
    mergeBlock := g.currentFunc.NewBlock("")

    // Add conditional branch
    g.currentBlock.NewCondBr(cond, thenBlock, elseBlock)

    // Generate then block
    g.currentBlock = thenBlock
    thenValue, err := g.generateExpression(expr.Consequence)
    if err != nil {
        return nil, err
    }
    g.currentBlock.NewBr(mergeBlock)

    // Generate else block
    g.currentBlock = elseBlock
    elseValue, err := g.generateExpression(expr.Alternative)
    if err != nil {
        return nil, err
    }
    g.currentBlock.NewBr(mergeBlock)

    // Generate merge block with phi instruction
    g.currentBlock = mergeBlock
    phi := g.currentBlock.NewPhi([]value.Value{thenValue, elseValue}, []*ir.Block{thenBlock, elseBlock})

    return phi, nil
}

// Helper function to get LLVM type from Cool type
func (g *Generator) getLLVMType(coolType string) (types.Type, error) {
    if llvmType, ok := g.coolTypeMap[coolType]; ok {
        return llvmType, nil
    }
    return nil, fmt.Errorf("unknown type: %s", coolType)
}

// Add VTable generation
func (g *Generator) generateVTable(class *ast.Class) error {
    methodList := g.collectMethods(class)
    
    // Create method array type
    vtableType := types.NewArray(uint64(len(methodList)), types.NewPointer(types.I8))
    
    // Create global vtable
    vtableName := fmt.Sprintf("vtable.%s", class.Name.Value)
    vtable := g.module.NewGlobalDef(vtableName, constant.NewZeroInitializer(vtableType))
    g.vtables[class.Name.Value] = vtable
    
    return nil
}

func (g *Generator) collectMethods(class *ast.Class) []*ast.Method {
    methods := make([]*ast.Method, 0)
    // Collect methods including inherited ones
    // This would need to use your inheritance graph
    return methods
}