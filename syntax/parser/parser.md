# Cool Compiler: Parser and Semantic Analyzer

## Overview

This repository contains a **Cool (Classroom Object-Oriented Language) compiler** written in **Go**, focusing on the **parser** and **semantic analyzer**. The parser is responsible for **syntax analysis**, while the semantic analyzer verifies **type correctness, inheritance, and method resolution**.

---

# Parser

## Features
The **parser** is responsible for constructing an **Abstract Syntax Tree (AST)** from the tokenized input. It follows a **precedence-based parsing** approach and includes:

### 1. **Lexical Integration**
- Uses the **lexer** to tokenize the Cool source code.
- Maintains **current and peek tokens** for lookahead parsing.

### 2. **Precedence-Based Parsing**
- Supports **prefix, infix, and postfix expressions**.
- Implements **precedence levels** to ensure correct parsing of expressions.
- Handles **operator associativity**.

### 3. **AST Construction**
- Parses **classes, attributes, and methods**.
- Supports **expressions** such as `if`, `while`, `case`, and `let`.
- Handles **array indexing (`array[index]`) and method calls (`obj.method()`)**.

### 4. **Error Recovery**
- Detects **unexpected tokens** and provides meaningful error messages.
- Skips tokens intelligently to recover from syntax errors.

## Usage

To use the parser, initialize the **lexer and parser**, then parse the program:

```go
// Initialize lexer and parser
l := lexer.NewLexer(strings.NewReader(string(code)))
p := parser.New(l)
program := p.ParseProgram()
```
where `code` is a string containing the Cool source code.

### **Error Handling**
```go
errors := p.Errors()
if len(errors) > 0 {
    fmt.Println("Parsing Errors:")
    for _, err := range errors {
        fmt.Println(err)
    }
} else {
    fmt.Println("Parsing successful.")
}
```

---


For more details on the **Cool language** and its features, refer to the [Cool Manual](https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf).