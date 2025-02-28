# Cool Compiler: Pretty Printer

## Overview

This repository contains the **Pretty Printer** for the **Cool (Classroom Object-Oriented Language) compiler** written in **Go**. The Pretty Printer takes an **Abstract Syntax Tree (AST)** and converts it into a structured, human-readable representation of the program, maintaining indentation and clarity.

---

## Features

### 1. **AST Pretty Printing**
- Converts an **AST** into a structured textual representation.
- Preserves **indentation** and **formatting** for readability.
- Prints **classes, methods, attributes, and expressions** in an easy-to-read format.

### 2. **Support for Various Expressions**
- **Binary expressions** (`+`, `-`, `*`, `/`, `<=`, etc.).
- **Control structures** (`if`, `while`, `case`, `let`).
- **Method dispatches** and **nested expressions**.

### 3. **Block and Nested Expression Handling**
- Properly formats **nested blocks**.
- Ensures structured and readable output.

## Example Output

Given the following Cool program:

```cool
class Main inherits IO {
   fibonacci(n : Int) : Int {
      if n <= 1 then 1 else fibonacci(n-1) + fibonacci(n-2) fi
   };

   main() : Object {
      {
         out_string("Fibonacci(8): ");
         out_int(fibonacci(8));
         out_string("\n");
      }
   };
};
```

The **Pretty Printer** outputs:

```
Program
  Class: Main inherits IO
    Method: fibonacci
      Formals:
        n: Int
      Return Type: Int
      Body:
        Block
          If
            Condition:
              Binary: <=
                Identifier: n
                Int: 1
            Then:
              Int: 1
            Else:
              Binary: +
                Dispatch:
                  Method: fibonacci
                  Arguments:
                    Binary: -
                      Identifier: n
                      Int: 1
                Dispatch:
                  Method: fibonacci
                  Arguments:
                    Binary: -
                      Identifier: n
                      Int: 2
    Method: main
      Formals:
      Return Type: Object
      Body:
        Block
          Block
            Dispatch:
              Method: out_string
              Arguments:
                String: "Fibonacci(8): "
            Dispatch:
              Method: out_int
              Arguments:
                Dispatch:
                  Method: fibonacci
                  Arguments:
                    Int: 8
            Dispatch:
              Method: out_string
              Arguments:
                String: "\n"
```

## Usage

To use the Pretty Printer, simply pass an **AST Program** to it:

```go
printer := parser.NewPrinter()
output := printer.PrintProgram(program)
fmt.Println(output)
```
where `program` is an instance of `ast.Program`.

## Conclusion

The **Pretty Printer** ensures that Cool programs are **formatted correctly**, making them **easier to read and debug**. By maintaining proper **indentation** and **logical structuring**, it helps visualize the **AST** more clearly and is a valuable tool for compiler debugging.