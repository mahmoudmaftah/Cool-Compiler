# Cool Compiler

> A compiler for the Classroom Object-Oriented Language (Cool), transforming Cool source code into executable programs.

## Overview

The **Cool Compiler** is designed to compile programs written in the Classroom Object-Oriented Language (Cool). Cool is a pedagogical language that incorporates features of modern programming languages, including objects, automatic memory management, and strong static typing. This project translates Cool source code into executable binaries, facilitating the learning of compiler construction and language design.

## Features

- **Lexical Analysis**: Tokenizes Cool source code.
- **Parsing**: Generates an Abstract Syntax Tree (AST) from tokens.
- **Semantic Analysis**: Performs type checking and ensures semantic correctness.
- **Code Generation**: Produces LLVM Intermediate Representation (IR) from the AST.
- **Optimization**: Applies various optimization levels to the generated IR.
- **Executable Generation**: Compiles LLVM IR to machine code, producing executable binaries.

## Getting Started

### Prerequisites

- **Go Programming Language**: Ensure Go is installed on your system.
- **LLVM Tools**: Install `llvm` and `clang` for code generation and compilation.

### Installation

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/yourusername/cool-compiler.git
   cd cool-compiler
   ```

2. **Build the Compiler**:

   ```bash
   go build -o coolc main.go
   ```

   This command compiles the `main.go` file into an executable named `coolc`.

### Usage

The compiler supports various command-line flags:

- `-i`: Path to the Cool source file (required).
- `-o`: Path for the output file (optional; defaults to the input file name with a `.ll` extension).
- `-p`: Print the AST to a `.ast` file.
- `--emit-binary`: Generate an executable binary.
- `-v`: Verbose output.

**Example**: Compiling a Cool program with the most basic setup:

```bash
./coolc -i examples/hello_world.cool -o hello_world.ll
```


This command compiles `hello_world.cool` with optimization level 2, produces an executable, and provides verbose output.

## Documentation

- **Cool Reference Manual**: Detailed information about the Cool language can be found in the [Cool Reference Manual](https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf).

- **LLVM Documentation**: For more information on LLVM IR and optimization passes, refer to the [LLVM Documentation](https://llvm.org/docs/).

- **Go Programming Language**: Learn more about Go by visiting the [Go Documentation](https://golang.org/doc/).

## Contributing

Contributions are welcome! Please fork the repository and submit a pull request. For major changes, open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License.

---
