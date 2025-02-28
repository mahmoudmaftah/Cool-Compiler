package codegen

import (
	"fmt"
	"strings"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/value"

	"github.com/llir/llvm/ir/types"
)

// setupArrayExtension initializes the Array class type and related functions
func (cg *CodeGenerator) setupArrayExtension() {
	// Check if Array type already exists
	if _, exists := cg.classTypes["Array"]; exists {
		return // Array already set up, don't duplicate
	}

	// Define Array class type (parameterized by element type)
	arrayFields := []types.Type{
		types.NewPointer(types.I8), // vtable pointer
		types.NewPointer(types.I8), // data pointer (elements)
		types.I32,                  // size field
		types.I8,                   // element type tag (for runtime type checking)
	}
	arrayType := types.NewStruct(arrayFields...)
	cg.classTypes["Array"] = arrayType
	cg.Module.NewTypeDef("Array", arrayType)

	// Define Array methods
	cg.declareArrayMethods()

	// Set up class parent
	cg.classParents["Array"] = "Object"

	// Register Array as a valid class type
	cg.typeIDMap["Array"] = int64(len(cg.typeIDMap)) // Assign next available ID
}

// declareArrayMethods declares methods for the Array class
func (cg *CodeGenerator) declareArrayMethods() {
	// Initialize methods map for Array if not exists
	if _, exists := cg.methods["Array"]; !exists {
		cg.methods["Array"] = make(map[string]*ir.Func)
	}

	// Check if size method already exists
	if _, exists := cg.methods["Array"]["size"]; exists {
		return // Method already declared
	}

	// Declare size() method
	sizeMethod := cg.Module.NewFunc(
		"Array_size",
		types.I32,
		ir.NewParam("self", types.NewPointer(cg.classTypes["Array"])),
	)
	cg.methods["Array"]["size"] = sizeMethod

	// Implement array size method
	entry := sizeMethod.NewBlock("entry")

	// Get size field
	sizePtr := entry.NewGetElementPtr(
		cg.classTypes["Array"],
		sizeMethod.Params[0],
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 2),
	)

	// Load and return the size
	size := entry.NewLoad(types.I32, sizePtr)
	entry.NewRet(size)

	// Define array initialization function (called by 'new Array')
	// Check if it already exists
	if _, exists := cg.runtimeFuncs["Array_new"]; !exists {
		initFunc := cg.Module.NewFunc(
			"Array_new",
			types.NewPointer(cg.classTypes["Array"]),
			ir.NewParam("elementSize", types.I32),
			ir.NewParam("numElements", types.I32),
			ir.NewParam("typeTag", types.I8),
		)
		cg.runtimeFuncs["Array_new"] = initFunc

		// Implement array initialization
		entryInit := initFunc.NewBlock("entry")

		// Allocate the Array object
		sizeOfArray := constant.NewInt(types.I64, 24) // Size of Array structure
		rawPtr := entryInit.NewCall(cg.runtimeFuncs["GC_malloc"], sizeOfArray)
		arrayPtr := entryInit.NewBitCast(rawPtr, types.NewPointer(cg.classTypes["Array"]))

		// Initialize vtable pointer
		// First, ensure we have a vtable for Array
		if _, exists := cg.vtableGlobals["Array"]; !exists {
			// Create a simple vtable for Array if it doesn't exist
			cg.createSimpleArrayVTable()
		}

		// Get pointer to vtable field
		vtablePtr := entryInit.NewGetElementPtr(
			cg.classTypes["Array"],
			arrayPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)

		// Cast the vtable global to the expected pointer type (i8*)
		vtableGlobal := cg.vtableGlobals["Array"]
		vtableCast := entryInit.NewBitCast(vtableGlobal, types.NewPointer(types.I8))

		// Store the cast vtable pointer
		entryInit.NewStore(vtableCast, vtablePtr)

		// Calculate total size for element data
		elementSize := entryInit.NewZExt(initFunc.Params[0], types.I64)
		numElements := entryInit.NewZExt(initFunc.Params[1], types.I64)
		totalSize := entryInit.NewMul(elementSize, numElements)

		// Allocate memory for elements
		dataPtr := entryInit.NewCall(cg.runtimeFuncs["GC_malloc"], totalSize)

		// Store data pointer
		dataFieldPtr := entryInit.NewGetElementPtr(
			cg.classTypes["Array"],
			arrayPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 1),
		)
		entryInit.NewStore(dataPtr, dataFieldPtr)

		// Store array size
		sizeFieldPtr := entryInit.NewGetElementPtr(
			cg.classTypes["Array"],
			arrayPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 2),
		)
		entryInit.NewStore(initFunc.Params[1], sizeFieldPtr)

		// Store element type tag
		typeTagPtr := entryInit.NewGetElementPtr(
			cg.classTypes["Array"],
			arrayPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 3),
		)
		entryInit.NewStore(initFunc.Params[2], typeTagPtr)

		// Return the array
		entryInit.NewRet(arrayPtr)
	}

	// Define array bounds checking function if it doesn't already exist
	if _, exists := cg.runtimeFuncs["Array_bounds_check"]; !exists {
		boundsCheckFunc := cg.Module.NewFunc(
			"Array_bounds_check",
			types.Void,
			ir.NewParam("array", types.NewPointer(cg.classTypes["Array"])),
			ir.NewParam("index", types.I32),
		)
		cg.runtimeFuncs["Array_bounds_check"] = boundsCheckFunc

		// Implement bounds checking
		entryBounds := boundsCheckFunc.NewBlock("entry")
		valid := boundsCheckFunc.NewBlock("valid")
		invalid := boundsCheckFunc.NewBlock("invalid")

		// Get array size
		sizePtr := entryBounds.NewGetElementPtr(
			cg.classTypes["Array"],
			boundsCheckFunc.Params[0],
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 2),
		)
		arraySize := entryBounds.NewLoad(types.I32, sizePtr)

		// Check if index is negative
		indexNegative := entryBounds.NewICmp(enum.IPredSLT, boundsCheckFunc.Params[1], constant.NewInt(types.I32, 0))

		// Check if index is >= size
		indexTooLarge := entryBounds.NewICmp(enum.IPredSGE, boundsCheckFunc.Params[1], arraySize)

		// Combine conditions
		outOfBounds := entryBounds.NewOr(indexNegative, indexTooLarge)

		// Branch based on bounds check
		entryBounds.NewCondBr(outOfBounds, invalid, valid)

		// Invalid block - print error and abort
		errorMsg := cg.createStringConstant(invalid, "bounds_error", "Array index out of bounds\n")
		invalid.NewCall(cg.cFuncs["printf"], errorMsg)
		invalid.NewCall(cg.cFuncs["exit"], constant.NewInt(types.I32, 1))
		invalid.NewUnreachable()

		// Valid block - just return
		valid.NewRet(nil)
	}

	// Create get and set element functions if needed
	cg.createArrayAccessFunctions()
}

// createSimpleArrayVTable creates a simple vtable for the Array class
func (cg *CodeGenerator) createSimpleArrayVTable() {
	// If vtables is nil, initialize it
	if cg.vtables == nil {
		cg.vtables = make(map[string]*types.StructType)
	}

	// Ensure we have a methods map for Array
	if _, exists := cg.methods["Array"]; !exists {
		cg.methods["Array"] = make(map[string]*ir.Func)
	}

	// Check if vtable global already exists - if so, just return
	if _, exists := cg.vtableGlobals["Array"]; exists {
		return
	}

	// Check if vtable type already exists
	var vtableType *types.StructType
	if existingType, exists := cg.vtables["Array"]; exists {
		vtableType = existingType
	} else {
		// Get the size method
		sizeMethod := cg.methods["Array"]["size"]
		if sizeMethod == nil {
			// Shouldn't happen, but if size method is missing, declare it
			sizeMethod = cg.Module.NewFunc(
				"Array_size",
				types.I32,
				ir.NewParam("self", types.NewPointer(cg.classTypes["Array"])),
			)
			cg.methods["Array"]["size"] = sizeMethod
		}
		vtableType = types.NewStruct(sizeMethod.Type())
		cg.vtables["Array"] = vtableType

		// Define the vtable type in the module
		cg.Module.NewTypeDef("Array_vtable", vtableType)
	}

	// Create the vtable instance as a global variable - with a unique name to avoid redefinition
	vtableInit := constant.NewStruct(vtableType, cg.methods["Array"]["size"])
	vtableGlobal := cg.Module.NewGlobalDef("Array_vtable_instance", vtableInit)

	// Store the vtable global for later use
	if cg.vtableGlobals == nil {
		cg.vtableGlobals = make(map[string]*ir.Global)
	}
	cg.vtableGlobals["Array"] = vtableGlobal
}




func (cg *CodeGenerator) getArrayElementType(arrayTypeStr string) (types.Type, int8, error) {
	// Parse Array[Type] syntax
	if !strings.HasPrefix(arrayTypeStr, "Array[") || !strings.HasSuffix(arrayTypeStr, "]") {
		return nil, 0, fmt.Errorf("invalid array type: %s", arrayTypeStr)
	}
	
	// Extract element type name
	elemTypeName := arrayTypeStr[6 : len(arrayTypeStr)-1]
	
	// Determine element type and type tag
	var elemType types.Type
	var typeTag int8
	
	switch elemTypeName {
	case "Int":
		elemType = types.I32
		typeTag = 1
	case "Bool":
		elemType = types.I8 // Using I8 for Bool for simplicity
		typeTag = 2
	case "String":
		elemType = types.NewPointer(types.I8)
		typeTag = 3
	default:
		// Check if it's a class type
		if _, exists := cg.classTypes[elemTypeName]; exists {
			elemType = types.NewPointer(cg.classTypes[elemTypeName])
			typeTag = 0 // Object type
		} else {
			return nil, 0, fmt.Errorf("unknown element type: %s", elemTypeName)
		}
	}
	
	// Store for later use
	if cg.arrayElementTypes == nil {
		cg.arrayElementTypes = make(map[string]types.Type)
	}
	cg.arrayElementTypes[arrayTypeStr] = elemType
	
	return elemType, typeTag, nil
}


func (cg *CodeGenerator) createArrayAccessFunctions() {
	// Check if the functions already exist
	if _, exists := cg.runtimeFuncs["Array_get_element"]; exists {
		return // Functions already defined
	}

	// Get element function - retrieves an element from the array
	getElemFunc := cg.Module.NewFunc(
		"Array_get_element",
		types.NewPointer(types.I8), // Return void pointer that can be cast to actual type
		ir.NewParam("array", types.NewPointer(cg.classTypes["Array"])),
		ir.NewParam("index", types.I32),
	)
	cg.runtimeFuncs["Array_get_element"] = getElemFunc

	// ---------- Create blocks and implement get_element function ----------
	var getEntryBlock *ir.Block
	var getDataPtr, getTypeTagPtr *ir.InstGetElementPtr
	var getRawDataPtr, getTypeTag value.Value
	var getIntBlock, getBoolBlock, getStringBlock, getObjectBlock, getDefaultBlock *ir.Block
	var getIsInt, getIsBool, getIsString, getIsObject value.Value

	// Create entry block
	getEntryBlock = getElemFunc.NewBlock("entry")

	// Check bounds
	getEntryBlock.NewCall(cg.runtimeFuncs["Array_bounds_check"], getElemFunc.Params[0], getElemFunc.Params[1])

	// Get data pointer
	getDataPtr = getEntryBlock.NewGetElementPtr(
		cg.classTypes["Array"],
		getElemFunc.Params[0],
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	getRawDataPtr = getEntryBlock.NewLoad(types.NewPointer(types.I8), getDataPtr)

	// Get element type tag
	getTypeTagPtr = getEntryBlock.NewGetElementPtr(
		cg.classTypes["Array"],
		getElemFunc.Params[0],
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 3),
	)
	getTypeTag = getEntryBlock.NewLoad(types.I8, getTypeTagPtr)

	// Create blocks for different element types
	getIntBlock = getElemFunc.NewBlock("int_elem")
	getBoolBlock = getElemFunc.NewBlock("bool_elem")
	getStringBlock = getElemFunc.NewBlock("string_elem")
	getObjectBlock = getElemFunc.NewBlock("object_elem")
	getDefaultBlock = getElemFunc.NewBlock("default_elem")

	// Set up conditional branches between blocks
	getIsInt = getEntryBlock.NewICmp(enum.IPredEQ, getTypeTag, constant.NewInt(types.I8, 1))
	getEntryBlock.NewCondBr(getIsInt, getIntBlock, getBoolBlock)

	getIsBool = getBoolBlock.NewICmp(enum.IPredEQ, getTypeTag, constant.NewInt(types.I8, 2))
	getBoolBlock.NewCondBr(getIsBool, getBoolBlock, getStringBlock)

	getIsString = getStringBlock.NewICmp(enum.IPredEQ, getTypeTag, constant.NewInt(types.I8, 3))
	getStringBlock.NewCondBr(getIsString, getStringBlock, getObjectBlock)

	getIsObject = getObjectBlock.NewICmp(enum.IPredEQ, getTypeTag, constant.NewInt(types.I8, 0))
	getObjectBlock.NewCondBr(getIsObject, getObjectBlock, getDefaultBlock)

	// Process Int elements
	var getIntElemPtr *ir.InstBitCast
	var getIntOffset *ir.InstGetElementPtr
	var getIntValue *ir.InstLoad
	var getIntAlloca *ir.InstAlloca
	var getIntResult *ir.InstBitCast

	getIntElemPtr = getIntBlock.NewBitCast(getRawDataPtr, types.NewPointer(types.I32))
	getIntOffset = getIntBlock.NewGetElementPtr(types.I32, getIntElemPtr, getElemFunc.Params[1])
	getIntValue = getIntBlock.NewLoad(types.I32, getIntOffset)
	getIntAlloca = getIntBlock.NewAlloca(types.I32)
	getIntBlock.NewStore(getIntValue, getIntAlloca)
	getIntResult = getIntBlock.NewBitCast(getIntAlloca, types.NewPointer(types.I8))
	getIntBlock.NewRet(getIntResult)

	// Process Bool elements
	var getBoolElemPtr *ir.InstBitCast
	var getBoolOffset *ir.InstGetElementPtr
	var getBoolValue *ir.InstLoad
	var getBoolAlloca *ir.InstAlloca
	var getBoolResult *ir.InstBitCast

	getBoolElemPtr = getBoolBlock.NewBitCast(getRawDataPtr, types.NewPointer(types.I8))
	getBoolOffset = getBoolBlock.NewGetElementPtr(types.I8, getBoolElemPtr, getElemFunc.Params[1])
	getBoolValue = getBoolBlock.NewLoad(types.I8, getBoolOffset)
	getBoolAlloca = getBoolBlock.NewAlloca(types.I8)
	getBoolBlock.NewStore(getBoolValue, getBoolAlloca)
	getBoolResult = getBoolBlock.NewBitCast(getBoolAlloca, types.NewPointer(types.I8))
	getBoolBlock.NewRet(getBoolResult)

	// Process String elements
	var getStrElemPtr *ir.InstBitCast
	var getStrOffset *ir.InstGetElementPtr
	var getStrElem *ir.InstLoad

	getStrElemPtr = getStringBlock.NewBitCast(getRawDataPtr, types.NewPointer(types.NewPointer(types.I8)))
	getStrOffset = getStringBlock.NewGetElementPtr(types.NewPointer(types.I8), getStrElemPtr, getElemFunc.Params[1])
	getStrElem = getStringBlock.NewLoad(types.NewPointer(types.I8), getStrOffset)
	getStringBlock.NewRet(getStrElem)

	// Process Object elements
	var getObjElemPtr *ir.InstBitCast
	var getObjOffset *ir.InstGetElementPtr
	var getObjElem *ir.InstLoad

	getObjElemPtr = getObjectBlock.NewBitCast(getRawDataPtr, types.NewPointer(types.NewPointer(types.I8)))
	getObjOffset = getObjectBlock.NewGetElementPtr(types.NewPointer(types.I8), getObjElemPtr, getElemFunc.Params[1])
	getObjElem = getObjectBlock.NewLoad(types.NewPointer(types.I8), getObjOffset)
	getObjectBlock.NewRet(getObjElem)

	// Default case - return null
	getDefaultBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))

	// ---------- Create and implement set_element function ----------

	// Define the function
	setElemFunc := cg.Module.NewFunc(
		"Array_set_element",
		types.Void,
		ir.NewParam("array", types.NewPointer(cg.classTypes["Array"])),
		ir.NewParam("index", types.I32),
		ir.NewParam("value", types.NewPointer(types.I8)),
	)
	cg.runtimeFuncs["Array_set_element"] = setElemFunc

	// Create all the blocks and variables we'll need
	var setEntryBlock *ir.Block
	var setDataPtr, setTypeTagPtr *ir.InstGetElementPtr
	var setRawDataPtr, setTypeTag value.Value
	var setInt, setString, setObject, setBool, setDefault *ir.Block
	var setIsInt, setIsBool, setIsString, setIsObject value.Value

	// Create the entry block and fill it
	setEntryBlock = setElemFunc.NewBlock("entry")
	setEntryBlock.NewCall(cg.runtimeFuncs["Array_bounds_check"], setElemFunc.Params[0], setElemFunc.Params[1])

	// Get data pointer from array
	setDataPtr = setEntryBlock.NewGetElementPtr(
		cg.classTypes["Array"],
		setElemFunc.Params[0],
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	setRawDataPtr = setEntryBlock.NewLoad(types.NewPointer(types.I8), setDataPtr)

	// Get type tag from array
	setTypeTagPtr = setEntryBlock.NewGetElementPtr(
		cg.classTypes["Array"],
		setElemFunc.Params[0],
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 3),
	)
	setTypeTag = setEntryBlock.NewLoad(types.I8, setTypeTagPtr)

	// Create all the type-specific blocks
	setInt = setElemFunc.NewBlock("int_set")
	setBool = setElemFunc.NewBlock("bool_set")
	setString = setElemFunc.NewBlock("string_set")
	setObject = setElemFunc.NewBlock("object_set")
	setDefault = setElemFunc.NewBlock("default_set")

	// Set up branches based on type tag
	setIsInt = setEntryBlock.NewICmp(enum.IPredEQ, setTypeTag, constant.NewInt(types.I8, 1))
	setEntryBlock.NewCondBr(setIsInt, setInt, setBool)

	setIsBool = setBool.NewICmp(enum.IPredEQ, setTypeTag, constant.NewInt(types.I8, 2))
	setBool.NewCondBr(setIsBool, setBool, setString)

	setIsString = setString.NewICmp(enum.IPredEQ, setTypeTag, constant.NewInt(types.I8, 3))
	setString.NewCondBr(setIsString, setString, setObject)

	setIsObject = setObject.NewICmp(enum.IPredEQ, setTypeTag, constant.NewInt(types.I8, 0))
	setObject.NewCondBr(setIsObject, setObject, setDefault)

	// Process Int elements
	var intPtr *ir.InstBitCast
	var intVal *ir.InstLoad
	var intArrPtr *ir.InstBitCast
	var intElemPtr *ir.InstGetElementPtr

	intPtr = setInt.NewBitCast(setElemFunc.Params[2], types.NewPointer(types.I32))
	intVal = setInt.NewLoad(types.I32, intPtr)
	intArrPtr = setInt.NewBitCast(setRawDataPtr, types.NewPointer(types.I32))
	intElemPtr = setInt.NewGetElementPtr(types.I32, intArrPtr, setElemFunc.Params[1])
	setInt.NewStore(intVal, intElemPtr)
	setInt.NewRet(nil)

	// Process Bool elements
	var boolPtr *ir.InstBitCast
	var boolVal *ir.InstLoad
	var boolArrPtr *ir.InstBitCast
	var boolElemPtr *ir.InstGetElementPtr

	boolPtr = setBool.NewBitCast(setElemFunc.Params[2], types.NewPointer(types.I8))
	boolVal = setBool.NewLoad(types.I8, boolPtr)
	boolArrPtr = setBool.NewBitCast(setRawDataPtr, types.NewPointer(types.I8))
	boolElemPtr = setBool.NewGetElementPtr(types.I8, boolArrPtr, setElemFunc.Params[1])
	setBool.NewStore(boolVal, boolElemPtr)
	setBool.NewRet(nil)

	// Process String elements
	var strArrPtr *ir.InstBitCast
	var strElemPtr *ir.InstGetElementPtr

	strArrPtr = setString.NewBitCast(setRawDataPtr, types.NewPointer(types.NewPointer(types.I8)))
	strElemPtr = setString.NewGetElementPtr(types.NewPointer(types.I8), strArrPtr, setElemFunc.Params[1])
	setString.NewStore(setElemFunc.Params[2], strElemPtr)
	setString.NewRet(nil)

	// Process Object elements
	var objArrPtr *ir.InstBitCast
	var objElemPtr *ir.InstGetElementPtr

	objArrPtr = setObject.NewBitCast(setRawDataPtr, types.NewPointer(types.NewPointer(types.I8)))
	objElemPtr = setObject.NewGetElementPtr(types.NewPointer(types.I8), objArrPtr, setElemFunc.Params[1])
	setObject.NewStore(setElemFunc.Params[2], objElemPtr)
	setObject.NewRet(nil)

	// Default case
	setDefault.NewRet(nil)
}
