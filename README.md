# How to Run

```
$ python3 decaf_checker.py <File Name>

ex: python3 decaf_checker.py rfib.decaf       
```

# Contents of other files

decaf_lexer.py:
-Scans the input file
-Tokenizes the input and validates each character

decaf_parser.py:
-Parses the tokens from decaf_lexer and validates the syntax of the input and constructs the AST
-Reduces the file till EOF to make sure it works as intended and spits out the first syntax error found

decaf_checker.py:
-Holds the main runner class that combines the process of input file reading, lexing, and parsing, alongside typechecking

decaf_ast.py:
-Holds the main AST node classes for building said AST 
-Holds relevant functionality to ensure syntactic correctness and visiblity/existence of classes/variables/methods referenced
-Holds relevant functionality to perform typechecking operations throughout the program and ensure type correctness, alongside printing any errors found.

decaf_absmc.py
-Responsible for holding the stacks, register binding tables, heap, and static var area necessary for machine code
-Prints the final formatted string from codegen to the correctly formatted output file

decaf_codegen.py
-Responsible for using absmc funcs correctly to use registers, stacks, etc.
-Responsible for building a complex string representing the machine code correctly built for input program