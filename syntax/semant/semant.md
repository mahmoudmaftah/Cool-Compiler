# Semantic Analyzer for Cool Compiler

## Overview

This repository contains a **semantic analyzer** for a Cool (Classroom Object-Oriented Language) compiler written in **Go**. The semantic analyzer is responsible for verifying the correctness of the program's type system, method resolution, inheritance, and other semantic rules.

## Features

The semantic analyzer implements multiple checks and validations to ensure that a Cool program adheres to the language rules. Here are the key features:

### 1. **Type Checking**

- Ensures that expressions conform to expected types.
- Supports basic types (`Int`, `String`, `Bool`, `Array`).
- Handles **SELF\_TYPE**, a special type in Cool.
- Validates assignments to ensure compatibility between types.

### 2. **Inheritance Graph Management**

- Constructs an **inheritance graph** to track class relationships.
- Detects **inheritance cycles** and reports them as errors.
- Ensures that user-defined classes correctly inherit from valid base classes.

### 3. **Symbol Table Management**

- Maintains a **global symbol table** for class-level symbols.
- Constructs **method scopes** and ensures method **overrides are type-safe**.
- Supports **nested scopes** (e.g., within `let` and `case` expressions).

### 4. **Method Resolution & Validation**

- Checks if methods exist before calling them.
- Ensures correct number of arguments in method calls.
- Enforces **method overriding** rules.

### 5. **Variable Initialization Tracking**

- Detects **use of uninitialized variables**.
- Ensures variables are properly initialized before access.

### 6. **Built-in Methods Handling**

- Implements built-in class methods, such as:
  - `Object.abort()`, `Object.type_name()`, `Object.copy()`
  - `IO.out_string()`, `IO.out_int()`, `IO.in_string()`, `IO.in_int()`
  - `String.length()`, `String.concat()`, `String.substr()`
  - `Array.size()`

### 7. **Support for Arrays**

- Adds `Array` as a basic type in Cool.
- Implements `size()` method for arrays.
- Allows defining arrays with element types (e.g., `Array[Int]`).

### 8. **Main Class Validation**

- Ensures the presence of a `Main` class.
- Validates that `Main` contains a `main` method with no parameters.

## Completeness

The **semantic analyzer** is feature-complete for validating Cool programs. It ensures that:

- **All type rules** are followed.
- **All inheritance constraints** are respected.
- **Method resolution and overriding** work correctly.
- **All required built-in classes and methods** are properly defined.
- **Arrays** are correctly managed, including element type validation.

## Usage

Using the semantic analyzer is straightforward. Given an **Abstract Syntax Tree (AST)** representing a Cool program, pass it to the analyzer:

### **Basic Usage**

```go
analyzer := semant.NewSemanticAnalyser()
analyzer.Analyze(program)
```

where `program` is an `ast.Program` representing the Cool program.

### **Error Reporting**

After analysis, you can retrieve any semantic errors found:

```go
errors := analyzer.Errors()
if len(errors) > 0 {
    fmt.Println("Semantic Errors:")
    for _, err := range errors {
        fmt.Println(err)
    }
} else {
    fmt.Println("No semantic errors detected.")
}
```

## Example: Analyzing a Simple Cool Program

Given the following Cool program:

```cool
class Main inherits IO {
  main(): SELF_TYPE {
    out_string("Hello, Cool!")
  };
};
```

### **Generating the AST and Running the Analyzer**

```go
// Assume we have parsed the Cool source file into an AST.
program := parseCoolProgram("example.cl") // Hypothetical function

analyzer := semant.NewSemanticAnalyser()
analyzer.Analyze(program)
errors := analyzer.Errors()
if len(errors) > 0 {
    fmt.Println("Errors found:")
    for _, err := range errors {
        fmt.Println(err)
    }
} else {
    fmt.Println("Program is semantically correct.")
}
```

## Conclusion

This **semantic analyzer** provides a robust implementation of Cool's type system, method resolution, and inheritance validation. It ensures that Cool programs adhere strictly to the language's semantic rules before proceeding to code generation or execution.

