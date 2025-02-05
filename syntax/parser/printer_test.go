package parser

import (
    "cool-compiler/lexer"
    "strings"
    "testing"
)

func TestPrinterBasicExpressions(t *testing.T) {
    tests := []struct {
        input    string
        expected string
    }{
        {
            "5",
            "Int: 5\n",
        },
        {
            `"hello"`,
            `String: "hello"` + "\n",
        },
        {
            "true",
            "Bool: true\n",
        },
        {
            "x",
            "Identifier: x\n",
        },
    }

    printer := NewPrinter()
    for i, tt := range tests {
        l := lexer.NewLexer(strings.NewReader(tt.input))
        p := New(l)
        expr := p.parseExpression(LOWEST)
        
        if len(p.errors) > 0 {
            t.Errorf("test[%d] parser had %d errors", i, len(p.errors))
            continue
        }

        actual := printer.PrintExpression(expr)
        if actual != tt.expected {
            t.Errorf("test[%d] - expected=\n%s\ngot=\n%s", i, tt.expected, actual)
        }
    }
}









func TestPrinterFullProgram(t *testing.T) {
    input := `
class Main {
    main(): Int {
        let x: Int <- 1 in
        if x then
            2
        else
            3
        fi
    };
};
`
    expected := `Program
  Class: Main
    Method: main
      Formals:
      Return Type: Int
      Body:
        Let
          Binding: x: Int
            Init:
              Int: 1
          In:
            If
              Condition:
                Identifier: x
              Then:
                Int: 2
              Else:
                Int: 3
`

    l := lexer.NewLexer(strings.NewReader(input))
    p := New(l)
    program := p.ParseProgram()
    
    if len(p.errors) > 0 {
        t.Errorf("parser had %d errors", len(p.errors))
        for _, err := range p.errors {
            t.Errorf("parser error: %q", err)
        }
        return
    }

    printer := NewPrinter()
    actual := printer.PrintProgram(program)
    
    if actual != expected {
        t.Errorf("expected=\n%s\ngot=\n%s", expected, actual)
    }
}