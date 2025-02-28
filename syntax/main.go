package main

import (
	codegen "cool-compiler/codeGen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func main() {
	// Define all command-line flags
	inputFilePath := flag.String("i", "", "Path to your program (required)")
	outputFilePath := flag.String("o", "", "Path for output file (optional, defaults to input file name with .ll extension)")
	printAst := flag.Bool("p", false, "Print AST to a .ast file")
	optimizationLevel := flag.Int("O", 0, "Optimization level (0-3)")
	emitBinary := flag.Bool("emit-binary", false, "Generate executable binary")
	verbose := flag.Bool("v", false, "Verbose output")

	flag.Parse()

	// Validate required flags
	if *inputFilePath == "" {
		fmt.Println("Error: Input file path is required. Use -i flag.")
		fmt.Println("Usage: go run main.go -i input.cl [-o output.ll] [-p] [-O level] [--emit-binary] [-v]")
		os.Exit(1)
	}

	// Determine output file path if not specified
	finalOutputPath := *outputFilePath
	if finalOutputPath == "" {
		finalOutputPath = strings.TrimSuffix(*inputFilePath, filepath.Ext(*inputFilePath)) + ".ll"
	}

	// Read input file
	code, err := os.ReadFile(*inputFilePath)
	if err != nil {
		fmt.Printf("Error reading input file: %v\n", err)
		os.Exit(1)
	}

	if *verbose {
		fmt.Println("Lexing and parsing...")
	}

	// Initialize lexer and parser
	l := lexer.NewLexer(strings.NewReader(string(code)))
	p := parser.New(l)
	program := p.ParseProgram()

	// Print AST if requested
	if *printAst {
		pr := parser.NewPrinter()
		astStr := pr.PrintProgram(program)
		astPath := strings.TrimSuffix(finalOutputPath, filepath.Ext(finalOutputPath)) + ".ast"

		if err := os.WriteFile(astPath, []byte(astStr), 0644); err != nil {
			fmt.Printf("Error writing AST to file: %v\n", err)
		} else if *verbose {
			fmt.Printf("AST written to %s\n", astPath)
		}
	}

	// Check for parser errors
	if len(p.Errors()) > 0 {
		fmt.Println("Parsing Errors:")
		for _, msg := range p.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}

	if *verbose {
		fmt.Println("Program parsed successfully!")
		fmt.Println("Running semantic analysis...")
	}

	// Semantic analysis
	analyzer := semant.NewSemanticAnalyser()
	analyzer.Analyze(program)

	if len(analyzer.Errors()) > 0 {
		fmt.Println("Semantic Errors:")
		for _, msg := range analyzer.Errors() {
			fmt.Println(msg)
		}
		// os.Exit(1)
	}

	if *verbose {
		fmt.Println("Semantic analysis completed successfully!")
		fmt.Println("Generating LLVM IR code...")
	}

	// Code generation with optimization level
	codeGen := codegen.New()
	// codeGen.SetOptimizationLevel(*optimizationLevel) // This method needs to be added to your codeGen package

	module, err := codeGen.Generate(program)
	if err != nil {
		fmt.Printf("Code generation error: %v\n", err)
		for _, msg := range codeGen.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}

	// Write LLVM IR to file
	f, err := os.Create(finalOutputPath)
	if err != nil {
		fmt.Printf("Error creating output file: %v\n", err)
		os.Exit(1)
	}

	module.WriteTo(f)
	f.Close()

	if *verbose {
		fmt.Printf("LLVM IR code written to %s\n", finalOutputPath)
	}

	// Handle optimization with opt tool if needed (external to LLIR/LLVM)
	if *optimizationLevel > 0 {
		if *verbose {
			fmt.Printf("Applying optimization level %d...\n", *optimizationLevel)
		}

		// Define optimization passes based on level
		var optPasses string
		switch *optimizationLevel {
		case 1:
			optPasses = "-mem2reg -sroa -early-cse -indvars"
		case 2:
			optPasses = "-mem2reg -sroa -early-cse -simplifycfg -instcombine -tailcallelim -loop-simplify -reassociate -licm -indvars"
		case 3:
			optPasses = "-mem2reg -sroa -early-cse -simplifycfg -instcombine -tailcallelim -loop-simplify -reassociate -licm -indvars -gvn -sccp -inline -dce -loop-unroll"
		}

		// Create temporary file for optimized output
		optimizedPath := finalOutputPath + ".opt"

		// Run optimization with opt tool
		optCmd := exec.Command("opt", "-S", optPasses, "-o", optimizedPath, finalOutputPath)
		optOutput, err := optCmd.CombinedOutput()

		if err != nil {
			fmt.Printf("Error during optimization: %v\n", err)
			fmt.Println(string(optOutput))
		} else {
			// Replace original file with optimized version
			if err := os.Rename(optimizedPath, finalOutputPath); err != nil {
				fmt.Printf("Error replacing file with optimized version: %v\n", err)
			} else if *verbose {
				fmt.Printf("Applied optimizations: %s\n", optPasses)
			}
		}
	}

	// Generate binary if requested
	if *emitBinary {
		if *verbose {
			fmt.Println("Generating executable binary...")
		}

		// Generate object file using llc
		objectPath := strings.TrimSuffix(finalOutputPath, filepath.Ext(finalOutputPath)) + ".o"
		llcCmd := exec.Command("llc", "-filetype=obj", "-o", objectPath, finalOutputPath)
		llcOutput, err := llcCmd.CombinedOutput()

		if err != nil {
			fmt.Printf("Error generating object file: %v\n", err)
			fmt.Println(string(llcOutput))
			os.Exit(1)
		}

		// Generate executable with clang
		exePath := strings.TrimSuffix(finalOutputPath, filepath.Ext(finalOutputPath))
		clangCmd := exec.Command("clang", "-o", exePath, objectPath)
		clangOutput, err := clangCmd.CombinedOutput()

		if err != nil {
			fmt.Printf("Error generating executable: %v\n", err)
			fmt.Println(string(clangOutput))
			os.Exit(1)
		}

		// Clean up object file
		os.Remove(objectPath)

		if *verbose {
			fmt.Printf("Executable binary generated at %s\n", exePath)
		}
	}

	fmt.Println("Compilation completed successfully!")
}
