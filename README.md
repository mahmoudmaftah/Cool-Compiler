https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf



###### Types of expressions according to the cool mannual:

- Constants
- Identifiers
- Assignment 
- Dispatch
- Conditionals
- Loops
- Blocks
- Let
- Case
- New
- Isvoid
- Arithmetic


We added (Node) structs to represent each of these types of expressions. We also added a struct for : 
* the Program, which is the root of the AST.
* the Class, which represents a class definition.



This is a part of my semant.go file, which is not complete i guess, because we are not builduing the entire symbot tables, we are stopping at classes and methods but not the expressions withing methods,