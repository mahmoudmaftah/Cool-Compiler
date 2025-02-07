package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"
	"testing"
)

func newParserFromInput(input string) *Parser {
	l := lexer.NewLexer(strings.NewReader(input))
	return New(l)
}

func checkParserErrors(t *testing.T, p *Parser, i int) {
	errors := p.Errors()
	if len(errors) > 0 {
		t.Errorf("parser has %d errors for test case %d", len(errors), i)
		for _, msg := range errors {
			t.Errorf("parser error: %q", msg)
		}
		t.FailNow()
	}
}

func TestProgramParser(t *testing.T) {
	tests := []struct {
		input           string
		expectedClasses []string
	}{
		{
			// Test case 1: Single empty class
			input:           "class Main {};",
			expectedClasses: []string{"Main"},
		},
		{
			// Test case 2: Multiple simple classes
			input:           "class A {}; class B {};",
			expectedClasses: []string{"A", "B"},
		},
		{
			// Test case 3: Class with basic attribute and method
			input: `
                class Main {
                    x : Int;
                    getX() : Int { x };
                };
            `,
			expectedClasses: []string{"Main"},
		},
		{
			// Test case 4: Class with inheritance
			input: `
                class A {
                    a : Int;
                };
                class B inherits A {
                    b : Int;
                };
            `,
			expectedClasses: []string{"A", "B"},
		},
		{
			input:           "class Test { test(): Int { 1 }; };",
			expectedClasses: []string{"Test"},
		},
		{
			input:           "class A { a: Int; }; class B { b: String; };",
			expectedClasses: []string{"A", "B"},
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		program := parser.ParseProgram()

		checkParserErrors(t, parser, i)

		if len(program.Classes) != len(tt.expectedClasses) {
			t.Fatalf("test[%d] - wrong number of classes. expected=%d, got=%d",
				i, len(tt.expectedClasses), len(program.Classes))
		}

		for j, className := range tt.expectedClasses {
			if program.Classes[j].Name.Value != className {
				t.Errorf("test[%d] - wrong class name for class %d. expected=%q, got=%q",
					i, j, className, program.Classes[j].Name.Value)
			}
		}
	}
}

func TestComplexExpressions(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			// Complex arithmetic with precedence
			input:    "1 + 2 * 3 / 4 - 5",
			expected: "((1 + ((2 * 3) / 4)) - 5)",
		},
		{
			// Mixed operators and dispatch
			input:    "(x + y).foo() + (a * b).bar()",
			expected: "((x + y).foo() + (a * b).bar())",
		},
		{
			// Complex let with nested expressions
			input: `let x: Int <- 1 + 2,
                        y: String <- "hello"
                    in x + y.length()`,
			expected: `let x:Int<-(1 + 2),y:String<-"hello" in (x + y.length())`,
		},
		{
			// Complex case with nested expressions
			input: `case x + y of
                        a: Int => if z then 1 else 2 fi;
                        b: String => let w: Int <- 3 in w;
                    esac`,
			expected: "case (x + y) of a:Int=>if z then 1 else 2 fi; b:String=>let w:Int<-3 in w esac",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		expression := parser.parseExpression(START)
		checkParserErrors(t, parser, i)

		actual := SerializeExpression(expression)
		if actual != tt.expected {
			t.Errorf("test[%d] - expected=%q, got=%q", i, tt.expected, actual)
		}
	}
}

func TestClassParser(t *testing.T) {
	tests := []struct {
		input          string
		expectedName   string
		expectedParent string
	}{
		{
			input:          "class Main {};",
			expectedName:   "Main",
			expectedParent: "",
		},
		{
			input:          "class A {age:Integer<-30;};",
			expectedName:   "A",
			expectedParent: "",
		},
		{
			input:          "class B {func(): Void {};};",
			expectedName:   "B",
			expectedParent: "",
		},
		{
			input:          "class B inherits A {func(): Void {};};",
			expectedName:   "B",
			expectedParent: "A",
		},
		{
			input:          "class Test { x: Int; y: String; };",
			expectedName:   "Test",
			expectedParent: "",
		},
		{
			input:          "class Child inherits Parent { method(): Int { 1 }; };",
			expectedName:   "Child",
			expectedParent: "Parent",
		},
		{
			input:          "class Complex { attr: Int <- 42; method(x: Int): Int { x }; };",
			expectedName:   "Complex",
			expectedParent: "",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		class := parser.ParseClass()

		checkParserErrors(t, parser, i)

		if class.Name.Value != tt.expectedName {
			t.Fatalf("[%q]: expected class name to be %q got %q", tt.input, tt.expectedName, class.Name.Value)
		}

		if class.Parent != nil {
			if class.Parent.Value != tt.expectedParent {
				t.Fatalf("[%q]: expected class parent to be %q got %q", tt.input, tt.expectedParent, class.Parent.Value)
			}
		} else if tt.expectedParent != "" {
			t.Fatalf("[%q]: expected class parent to be %q got nil", tt.input, tt.expectedParent)
		}
	}
}

func TestCaseExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input: `case x of
                    a : Int => 1;
                    b : String => 2;
                    c : Bool => 3;
                esac`,
			expected: "case x of a:Int=>1; b:String=>2; c:Bool=>3 esac",
		},
		{
			// Nested case expression
			input: `case y of
                    a : Int => case x of
                        b : Int => 1;
                        c : String => 2;
                    esac;
                    d : String => 3;
                esac`,
			expected: "case y of a:Int=>case x of b:Int=>1; c:String=>2 esac; d:String=>3 esac",
		},
		{
			// Case with complex expressions
			input: `case x + y of
                    a : Int => b + c;
                    d : String => e.method();
                esac`,
			expected: "case (x + y) of a:Int=>(b + c); d:String=>e.method() esac",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		expression := parser.parseExpression(START)
		checkParserErrors(t, parser, i)

		actual := SerializeExpression(expression)
		if actual != tt.expected {
			t.Errorf("test[%d] - expected=%q, got=%q", i, tt.expected, actual)
		}
	}
}

func TestFormalParsing(t *testing.T) {
	tests := []struct {
		input         string
		expectedNames []string
		expectedTypes []string
	}{
		{
			input:         "var1:Integer",
			expectedNames: []string{"var1"},
			expectedTypes: []string{"Integer"},
		},
		{
			input:         "var1:Integer,var2:Boolean,var3:String",
			expectedNames: []string{"var1", "var2", "var3"},
			expectedTypes: []string{"Integer", "Boolean", "String"},
		},
	}

	for _, tt := range tests {
		parser := newParserFromInput(tt.input)
		formals := parser.parseFormals()

		if len(parser.errors) > 0 {
			for _, err := range parser.errors {
				t.Errorf("Parsing Error %s\n", err)
			}
			t.Fatalf("[%q]: Found errors while parsing", tt.input)
		}

		if len(formals) != len(tt.expectedNames) {
			t.Fatalf("[%q]: expected %d formals got %d: %v", tt.input, len(tt.expectedNames), len(formals), formals)
		}

		for i, formal := range formals {
			if formal.Name.Value != tt.expectedNames[i] {
				t.Fatalf("[%q]: expected formal name to be %q got %q", tt.input, tt.expectedNames[i], formal.Name.Value)
			}
			if formal.TypeDecl.Value != tt.expectedTypes[i] {
				t.Fatalf("[%q]: expected formal type to be %q got %q", tt.input, tt.expectedNames[i], formal.Name.Value)
			}
		}
	}
}

func TestMethodParsing(t *testing.T) {
	tests := []struct {
		input               string
		expectedMethodName  string
		expectedFormalNames []string
		expectedFormalTypes []string
		expectedMethodType  string
	}{
		{
			// In testcases these are methods and are thus expected to start with a lower case letter
			// To indicate that these are object and not type identifiers
			// This took me a while to figure out :(
			input:               "main(): Void {};",
			expectedMethodName:  "main",
			expectedFormalNames: []string{},
			expectedFormalTypes: []string{},
			expectedMethodType:  "Void",
		},
		{
			input:               "sum(a:Integer,b:Integer): Integer {};",
			expectedMethodName:  "sum",
			expectedFormalNames: []string{"a", "b"},
			expectedFormalTypes: []string{"Integer", "Integer"},
			expectedMethodType:  "Integer",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		method := parser.parseMethod()
		checkParserErrors(t, parser, i)

		if method.Name.Value != tt.expectedMethodName {
			t.Fatalf("[%q]: Expected method name to be %q found %q", tt.input, tt.expectedMethodName, method.Name.Value)
		}

		for i, formal := range method.Formals {
			if formal.Name.Value != tt.expectedFormalNames[i] {
				t.Fatalf("[%q]: Expected formal name to be %q found %q", tt.input, tt.expectedFormalNames[i], formal.Name.Value)
			}
			if formal.TypeDecl.Value != tt.expectedFormalTypes[i] {
				t.Fatalf("[%q]: Expected formal type to be %q found %q", tt.input, tt.expectedFormalTypes[i], formal.TypeDecl.Value)
			}
		}

		if method.TypeDecl.Value != tt.expectedMethodType {
			t.Fatalf("[%q]: Expected method type to be %q found %q", tt.input, tt.expectedMethodType, method.TypeDecl.Value)
		}
	}
}

func TestAttributeParsing(t *testing.T) {
	tests := []struct {
		input              string
		expectedName       string
		expectedType       string
		expectedExpression ast.Expression
	}{
		{
			input:        "firstName:String",
			expectedName: "firstName",
			expectedType: "String",
		},
		{
			input:        "age:Integer<-0",
			expectedName: "age",
			expectedType: "Integer",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		attribute := parser.parseAttribute()

		checkParserErrors(t, parser, i)
		if attribute.Name.Value != tt.expectedName {
			t.Fatalf("[%q]: Expected attribute name to be %q got %q", tt.input, tt.expectedName, attribute.Name.Value)
		}
		if attribute.TypeDecl.Value != tt.expectedType {
			t.Fatalf("[%q]: Expected attribute type to be %q got %q", tt.input, tt.expectedType, attribute.TypeDecl.Value)
		}
	}
}

func TestExpressionParssing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"5", "5"},
		{`"hello world"`, `"hello world"`},
		{"true", "true"},
		{"false", "false"},
		{"x", "x"},
		{"not true", "(not true)"},
		{"1 + 2", "(1 + 2)"},
		{"1 < 2", "(1 < 2)"},
		{"1 <= 2", "(1 <= 2)"},
		{"~1", "(~ 1)"},
		{"1 = 2", "(1 = 2)"},
		{"1 * 2", "(1 * 2)"},
		{"isvoid 1", "isvoid 1"},
		{"1 / 2", "(1 / 2)"},
		// TODO: Implement parenthesis parsing
		// {"(1 + 2)", "(1 + 2)"},
		{"new Object", "new Object"},
		{"x <- 5", "(x <- 5)"},
		{"if true then 1 else 2 fi", "if true then 1 else 2 fi"},
		{"while true loop 1 pool", "while true loop 1 pool"},
	}

	for i, tt := range tests {
		p := newParserFromInput(tt.input)
		checkParserErrors(t, p, i)

		expression := p.parseExpression(START)
		actual := SerializeExpression(expression)
		if actual != tt.expected {
			t.Errorf("test [%d] expected expression to be '%s', got '%s'", i, tt.expected, actual)
		}
	}

}





func TestMethodDispatch(t *testing.T) {
    tests := []struct {
        input    string
        expected string
    }{
        {
            // Simple method call
            input:    "obj.method()",
            expected: "obj.method()",
        },
        {
            // Method call with arguments
            input:    "obj.method(1, 2, 3)",
            expected: "obj.method(1, 2, 3)",
        },
        // {
        //     // Static dispatch
        //     input:    "obj@Type.method()",
        //     expected: "obj@Type.method()",
        // },
        {
            // Complex dispatch with nested expressions
            input:    "(a + b).method((x + y), z.foo())",
            expected: "(a + b).method((x + y), z.foo())",
        },
    }

    for i, tt := range tests {
        parser := newParserFromInput(tt.input)
        expression := parser.parseExpression(START)
        checkParserErrors(t, parser, i)

		fmt.Println(expression)
        actual := SerializeExpression(expression)
        if actual != tt.expected {
            t.Errorf("test[%d] - expected=%q, got=%q", i, tt.expected, actual)
        }
    }
}




func TestNestedExpressions(t *testing.T) {
    tests := []struct {
        input    string
        expected string
    }{
        {
            // Nested if expressions
            input: `if if x then true else false fi then 1 else 2 fi`,
            expected: "if if x then true else false fi then 1 else 2 fi",
        },
        {
            // Nested let expressions
            input: `let x: Int <- 1 in let y: Int <- 2 in x + y`,
            expected: "let x:Int<-1 in let y:Int<-2 in (x + y)",
        },
        {
            // Mixed nesting
            input: `if x then
                        let y: Int <- 1 in y + 2
                    else
                        3
                    fi`,
            expected: "if x then let y:Int<-1 in (y + 2) else 3 fi",
        },
    }

    for i, tt := range tests {
        parser := newParserFromInput(tt.input)
        expression := parser.parseExpression(START)
        checkParserErrors(t, parser, i)

        actual := SerializeExpression(expression)
        if actual != tt.expected {
            t.Errorf("test[%d] - expected=%q, got=%q", i, tt.expected, actual)
        }
    }
}





func TestLetExpressions(t *testing.T) {
    tests := []struct {
        input    string
        expected string
    }{
        {
            // Basic let with no initialization
            input: `let x: Int in x + 1`,
            expected: "let x:Int in (x + 1)",
        },
        {
            // Let with initialization
            input: `let x: Int <- 5 in x + 1`,
            expected: "let x:Int<-5 in (x + 1)",
        },
        {
            // Multiple bindings
            input: `let x: Int <- 5, y: String <- "hello" in x + y.length()`,
            expected: "let x:Int<-5,y:String<-\"hello\" in (x + y.length())",
        },
        {
            // Mix of initialized and uninitialized bindings
            input: `let x: Int <- 5, y: Int, z: String <- "hello" in x + y`,
            expected: "let x:Int<-5,y:Int,z:String<-\"hello\" in (x + y)",
        },
        {
            // Nested let expressions
            input: `let x: Int <- 1 in let y: Int <- x + 1 in x + y`,
            expected: "let x:Int<-1 in let y:Int<-(x + 1) in (x + y)",
        },
        {
            // Let with complex initialization
            input: `let x: Int <- if true then 1 else 2 fi in x + 1`,
            expected: "let x:Int<-if true then 1 else 2 fi in (x + 1)",
        },
        {
            // Let with multiple types
            input: `let x: Int <- 1, y: Bool <- true, z: String <- "hello" in x`,
            expected: "let x:Int<-1,y:Bool<-true,z:String<-\"hello\" in x",
        },
        {
            // Let with self type
            input: `let x: SELF_TYPE <- self in x.method()`,
            expected: "let x:SELF_TYPE<-self in x.method()",
        },
        {
            // Let within a block
            input: `{
                let x: Int <- 1 in x + 1;
                let y: Int <- 2 in y + 2
            }`,
            expected: "{ let x:Int<-1 in (x + 1); let y:Int<-2 in (y + 2) }",
        },
        {
            // Let with method call initialization
            input: `let x: Int <- foo(), y: String <- bar(1, 2) in x + y.length()`,
            expected: "let x:Int<-foo(),y:String<-bar(1,2) in (x + y.length())",
        },
        {
            // Let with complex nested expressions
            input: `let x: Int <- let y: Int <- 1 in y + 1 in x * 2`,
            expected: "let x:Int<-let y:Int<-1 in (y + 1) in (x * 2)",
        },
        {
            // Let with multiple nested complex initializations
            input: `let x: Int <- if true then 1 else 2 fi,
                       y: Int <- while false loop 1 pool,
                       z: Int <- { 1; 2; 3 }
                   in x + y + z`,
            expected: "let x:Int<-if true then 1 else 2 fi,y:Int<-while false loop 1 pool,z:Int<-{1;2;3} in ((x + y) + z)",
        },
    }

    for i, tt := range tests {
        parser := newParserFromInput(tt.input)
        expression := parser.parseExpression(START)
        checkParserErrors(t, parser, i)

        actual := SerializeExpression(expression)
        if actual != tt.expected {
            t.Errorf("test[%d] - expected=%q, got=%q", i, tt.expected, actual)
        }
    }
}