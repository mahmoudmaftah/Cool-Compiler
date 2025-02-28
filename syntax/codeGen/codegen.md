# Cool Compiler: LLVM-Based Code Generation

## Overview

This repository contains the **Code Generation module** for the **Cool (Classroom Object-Oriented Language) compiler**, implemented using **LLVM IR** through the **llir/llvm** Go bindings. The code generator transforms an **Abstract Syntax Tree (AST)** into **LLVM Intermediate Representation (LLVM IR)**, which can then be compiled to executable machine code.

---

## Introduction to LLVM and llir/llvm

### **What is LLVM?**
LLVM is a powerful compiler infrastructure that provides a common intermediate representation (IR) for multiple programming languages. It allows for advanced optimizations and efficient code generation.

### **What is llir/llvm?**
[llir/llvm](https://github.com/llir/llvm) is a Go package that provides bindings for LLVM IR, enabling the generation and manipulation of LLVM IR code in pure Go.

### **How We Use LLVM in Code Generation**
- **AST Traversal:** The generator walks the AST and converts it into LLVM IR.
- **LLVM Structs for Cool Classes:** Each Cool class is represented as an LLVM **struct**.
- **Virtual Method Tables (VMT):** Used for dynamic dispatch and method resolution.
- **Standard Library Integration:** The code generator interfaces with built-in Cool functions like `IO.out_string()` and memory allocation via **Garbage Collection (GC_malloc)**.

---

## Features

### 1. **LLVM Module Setup**
- Initializes an LLVM **module** (`cg.Module`), which contains all generated functions and global definitions.
- Declares **runtime functions** (`IO_out_string`, `Object.abort`, `string_concat`, etc.).
- Implements **Garbage Collection (GC)** by integrating with `GC_malloc`.

### 2. **Class Representation in LLVM**
- Each Cool class is defined as an LLVM `struct`, containing:
  - A **vtable pointer** for method dispatch.
  - Fields representing attributes of the class.

```go
objectFields := []types.Type{
    types.NewPointer(types.I8), // vtable pointer
}
cg.objectType = types.NewStruct(objectFields...)
cg.classTypes["Object"] = types.NewStruct(objectFields...)
cg.Module.NewTypeDef("Object", cg.classTypes["Object"])
```

### 3. **Method and Function Code Generation**
- Methods are stored in a `map[string]*ir.Func`, allowing lookup and calls.
- Dynamic dispatch is implemented using **vtable lookup**.

```go
funcName := fmt.Sprintf("%s_%s", className, methodName)
function := cg.Module.NewFunc(funcName, returnType, params...)
cg.methods[className][methodName] = function
```

### 4. **Inheritance and Virtual Method Tables (VMTs)**
- **Virtual tables (VTables)** store method pointers for dynamic method resolution.
- Methods in parent classes are inherited automatically in the child's vtable.
- **Dynamic dispatch** finds the correct method implementation at runtime.

```go
methodPtrPtr := cg.currentBlock.NewGetElementPtr(
    vtableType, typedVtablePtr,
    constant.NewInt(types.I32, 0),
    constant.NewInt(types.I32, int64(methodIdx)),
)
methodPtr := cg.currentBlock.NewLoad(methodFuncType, methodPtrPtr)
return cg.currentBlock.NewCall(methodPtr, args...), nil
```

### 5. **Memory Management and Object Creation**
- Uses `GC_malloc` for automatic memory management.
- Initializes object attributes to default values.

```go
sizeOfClass := constant.NewInt(types.I64, 64)
rawPtr := block.NewCall(cg.runtimeFuncs["GC_malloc"], sizeOfClass)
objPtr := block.NewBitCast(rawPtr, types.NewPointer(classType))
```

### 6. **Built-in Functions Implementation**
- Implements key runtime functions such as:
  - `Object.abort()`
  - `String.length()`
  - `IO.out_string()`

```go
outString := cg.runtimeFuncs["IO_out_string"]
entry := outString.NewBlock("entry")
formatStr := cg.createStringConstant(entry, ".str.fmt", "%s\x00")
entry.NewCall(cg.cFuncs["printf"], formatStr, str)
entry.NewRet(self)
```

### 7. **Array Support and Bounds Checking**
- Implements **Array** as a first-class type with:
  - `size()` method to get the array length.
  - **Bounds checking** for safe access.

```go
sizePtr := entry.NewGetElementPtr(
    cg.classTypes["Array"],
    sizeMethod.Params[0],
    constant.NewInt(types.I32, 0),
    constant.NewInt(types.I32, 2),
)
size := entry.NewLoad(types.I32, sizePtr)
entry.NewRet(size)
```

---

## Example Usage

### **Generating LLVM IR from a Cool Program**
Given a simple Cool program:

```cool
class Main inherits IO {
   main() : Object {
      {
         out_string("Hello, Cool!");
      }
   };
};
```

We generate LLVM IR using:

```go
cg := codegen.New()
irModule, err := cg.Generate(astProgram)
if err != nil {
    log.Fatal("Code generation failed:", err)
}
fmt.Println(irModule.String())
```

### **Output: LLVM IR Example**
```llvm
define i32 @main() {
entry:
  %str = getelementptr [13 x i8], [13 x i8]* @.str.hello, i32 0, i32 0
  call i32 @printf(i8* %str)
  ret i32 0
}
```

---

## Conclusion

The **Cool Compiler Code Generator** efficiently translates an **AST** into **LLVM IR**, leveraging **vtable-based dynamic dispatch**, **runtime memory management**, and **built-in function support**. By integrating with **LLVM’s powerful optimization capabilities**, it ensures **efficient execution** of Cool programs.

