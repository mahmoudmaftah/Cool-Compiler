package main

import (
	codegen "cool-compiler/codeGen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func main() {

	inputFilePath := flag.String("i", "", "Path to your program")
	flag.Parse()

	if *inputFilePath == "" {
		fmt.Println("Error: Input file path is required.")
		os.Exit(1)
	}

	code, err := os.ReadFile(*inputFilePath)
	if err != nil {
		fmt.Printf("Error reading input file: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Parsing...")
	l := lexer.NewLexer(strings.NewReader(string(code)))
	p := parser.New(l)
	program := p.ParseProgram()

	if len(p.Errors()) > 0 {
		fmt.Println("Parsing Errors:")
		for _, msg := range p.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}

	fmt.Println("Program Parsed successfully!")

	analyzer := semant.NewSemanticAnalyser()
	analyzer.Analyze(program)

	if len(analyzer.Errors()) > 0 {
		fmt.Println("Semantic Errors:")
		for _, msg := range analyzer.Errors() {
			fmt.Println(msg)
		}
		// os.Exit(1)
	}

	fmt.Println("Semantic Analysis completed successfully!")

	fmt.Println("Generating LLVM IR code...")
	codeGen := codegen.New()
	fmt.Println("instance created")
	module, err := codeGen.Generate(program)
	if err != nil {
		fmt.Printf("Code generation error: %v\n", err)
		for _, msg := range codeGen.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}

	// Write LLVM IR to a file
	outputPath := strings.TrimSuffix(*inputFilePath, filepath.Ext(*inputFilePath)) + ".ll"
	f, err := os.Create(outputPath)
	if err != nil {
		fmt.Printf("Error creating output file: %v\n", err)
		os.Exit(1)
	}
	defer f.Close()

	module.WriteTo(f)
	fmt.Printf("LLVM IR code written to %s\n", outputPath)

	fmt.Println("Done compiling!")

	fmt.Println("Done compiling!")
}
