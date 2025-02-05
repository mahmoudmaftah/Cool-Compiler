package parser

import (
    "fmt"
    "os"
	"strings"
	"cool-compiler/lexer"
)

// DumpASTToFile parses a Cool program and writes its AST to a file
func DumpASTToFile(coolProgram string, outputPath string) error {
    l := lexer.NewLexer(strings.NewReader(coolProgram))
    p := New(l)
    program := p.ParseProgram()
    
    if len(p.errors) > 0 {
        return fmt.Errorf("parsing errors:\n%s", strings.Join(p.errors, "\n"))
    }
    
    printer := NewPrinter()
    output := printer.PrintProgram(program)
    
    return os.WriteFile(outputPath, []byte(output), 0644)
}
