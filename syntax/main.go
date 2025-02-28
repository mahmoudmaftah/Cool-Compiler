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

	// the command will have another flag to specify the output file
	// it will also have a flag to specify wether to print the AST or not
	// go run main.go -i input.cl -o output.ll -p
	// if the user uses the -p flag, the program will print the AST into a .ast file with the same name as the output file
	// it will also have a a flag to add optimizations to the code and the optimization level
	// go run main.go -i input.cl -o output.ll -p -O 3

	// the other flags are optional, the main one is -i

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
	// create a new printer

	// print all tokens
	// for {
	// 	tok := l.NextToken()
	// 	fmt.Printf("Token: %v\n", tok)
	// 	if tok.Type == lexer.EOF {
	// 		break
	// 	}
	// }

	p := parser.New(l)
	program := p.ParseProgram()

	pr := parser.NewPrinter()
	s := pr.PrintProgram(program)
	fmt.Println(s)

	if len(p.Errors()) > 0 {
		fmt.Println("Parsing Errors:")
		for _, msg := range p.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}
	// os.Exit(1)

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
