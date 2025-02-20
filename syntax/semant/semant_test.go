package semant

import (
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"strings"
	"testing"
)

func newParserFromInput(input string) *parser.Parser {
	l := lexer.NewLexer(strings.NewReader(input))
	return parser.New(l)
}

func TestSemanticAnalysis(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		hasError bool
		errorMsg string
	}{

		{
			name: "Invalid method overriding",
			input: `
                class A {
                    foo(x: Int): Int { x };
                };
                class B inherits A {
                    foo(x: String): Int { 1 };
                };
                class Main {
                    main(): Object { 1 };
                };
            `,
			hasError: true,
			errorMsg: "Invalid method override",
		},
		{
			name: "Case expression with valid types",
			input: `
                class Main {
                    main(): Object {
                        case x of
                            i: Int => i + 1;
                            s: String => 2;
                        esac
                    };
                };
            `,
			hasError: false,
		},
		{
			name: "Case expression with duplicate types",
			input: `
                class Main {
                    main(): Object {
                        case x of
                            i: Int => i + 1;
                            j: Int => j + 2;
                        esac
                    };
                };
            `,
			hasError: true,
			errorMsg: "Duplicate branch type",
		},
		{
			name: "If expression type conformance",
			input: `
                class Main {
                    main(): Object {
                        if true then 1 else "string" fi
                    };
                };
            `,
			hasError: false, // Should find Object as LUB
		},
		{
			name: "SELF_TYPE handling",
			input: `
                class Main {
                    x: SELF_TYPE;
                    main(): SELF_TYPE { self };
                };
            `,
			hasError: false,
		},
		{
			name: "Method dispatch type checking",
			input: `
                class A {
                    foo(x: Int): Int { x };
                };
                class Main {
                    a: A;
                    main(): Object {
                        a.foo("string")
                    };
                };
            `,
			hasError: true,
			errorMsg: "does not conform to formal parameter type",
		},
		{
			name: "Static dispatch type checking",
			input: `
                class A {
                    foo(): Int { 1 };
                };
                class B inherits A {
                    foo(): Int { 2 };
                };
                class Main {
                    main(): Object {
                        (new B)@A.foo()
                    };
                };
            `,
			hasError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parser := newParserFromInput(tt.input)
			program := parser.ParseProgram()

			analyzer := NewSemanticAnalyser()
			analyzer.Analyze(program)

			hasError := len(analyzer.Errors()) > 0
			if hasError != tt.hasError {
				t.Errorf("expected hasError=%v, got=%v", tt.hasError, hasError)
				// print errors
				for _, err := range analyzer.Errors() {
					t.Log(err)
				}

			}

			if tt.hasError && tt.errorMsg != "" {
				found := false
				for _, err := range analyzer.Errors() {
					if strings.Contains(err, tt.errorMsg) {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("expected error containing %q, got errors: %v",
						tt.errorMsg, analyzer.Errors())
				}
			}
		})
	}
}

func TestSemanticAnalysis2(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		hasError bool
		errorMsg string
	}{

				// {
		// 	name: "Basic class definition",
		// 	input: `
        //         class Main {
        //             main(): Object { 1 };
        //         };
        //     `,
		// 	hasError: false,
		// },
		// {
		// 	name: "Undefined type in attribute",
		// 	input: `
        //         class Main {
        //             x: NonExistentType;
        //             main(): Object { 1 };
        //         };
        //     `,
		// 	hasError: true,
		// 	errorMsg: "undefined type NonExistentType",
		// },
		// {
		// 	name: "Type mismatch in initialization",
		// 	input: `
        //         class Main {
        //             x: Int <- "string";
        //             main(): Object { 1 };
        //         };
        //     `,
		// 	hasError: true,
		// 	errorMsg: "does not conform to declared type",
		// },
		// {
		// 	name: "Let with type checking",
		// 	input: `
        //         class Main {
        //             main(): Object {
        //                 let x: Int <- "string" in x + 1
        //             };
        //         };
        //     `,
		// 	hasError: true,
		// 	errorMsg: "does not conform to declared type",
		// },
		// {
		// 	name: "Formals with same name",
		// 	input: `
		// 		class Test {
		// 			x: Int;
		// 			y(a: Int, a: Bool): Int { a };
		// 		};
        //         class Main {
        //             main(): Object {
        //                 let x: Int <- "string" in x + 1
        //             };
        //         };
        //     `,
		// 	hasError: true,
		// 	errorMsg: "does not conform to declared type",
		// },
		// {
		// 	name: "Correct method overriding",
		// 	input: `
        //         class A {
        //             foo(x: Int): Int { x };
        //         };
        //         class B inherits A {
        //             foo(x: Int): Int { x + 1 };
        //         };
        //         class Main {
        //             main(): Object { 1 };
        //         };
        //     `,
		// 	hasError: false,
		// },

		// {
		// 	name: "Formals with same name",
		// 	input: `
		// 		class Typ {
		// 			x: Int;
		// 		}

		// 		class Test {
		// 			x: Int;
		// 			y(a: Int, b: Typ): Int { a };
		// 		};
		//         class Main {
		//             main(): Object {
		//                 let x: Int <- "string" in x + 1
		//             };
		//         };
		//     `,
		// 	hasError: true,
		// 	errorMsg: "does not conform to declared type",
		// },

		// {
		// 	name: "Cycle inheritance",
		// 	input: `
		// 		class A inherits B {
		// 			x: Int;
		// 		};

		// 		class B inherits A {
		// 			y: Int;
		// 		};

		// 		class Main {
		// 			main(): Object { 1 };
		// 		};
		// 	`,
		// 	hasError: true,
		// 	errorMsg: "cycle detected",
		// },

		// Test type conformance in checking
		// {
		// 	name: "Type conformance in checking",
		// 	input: `
		// 		class A {
		// 			foo(x: Int): Int { x };
		// 		};
		// 		class B inherits A {
		// 			foo(x: Int): Int { x + 1 };
		// 		};
		// 		class Main {
		// 			main(): Object {
		// 				let a: A <- new B in a.foo(1)
		// 			};
		// 		};
		// 	`,
		// 	hasError: false,
		// },
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parser := newParserFromInput(tt.input)
			program := parser.ParseProgram()

			analyzer := NewSemanticAnalyser()
			analyzer.Analyze(program)

			hasError := len(analyzer.Errors()) > 0
			if hasError != tt.hasError {
				t.Errorf("expected hasError=%v, got=%v", tt.hasError, hasError)
				// print errors
				for _, err := range analyzer.Errors() {
					t.Log(err)
				}

			}

			if tt.hasError && tt.errorMsg != "" {
				// print errors
				for _, err := range analyzer.Errors() {
					t.Log(err)
				}
				found := false
				for _, err := range analyzer.Errors() {
					if strings.Contains(err, tt.errorMsg) {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("expected error containing %q, got errors: %v",
						tt.errorMsg, analyzer.Errors())
				}
			}
		})
	}
}
