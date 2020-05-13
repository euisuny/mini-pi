# miniPI

This is a mini Pi-calculus interpreter.
The language representation is based on the Core Pict language, and the interpreter gives an interpretation of all possible nondeterministic communications of the expression.

## Dependencies

- OCaml >= 4.06.0
- `menhir` : Use `opam install menhir`

## Files

- `ast.ml` : AST for pi-calculus expressions
- `eval.ml` : interpreter for ASTs
- `lexer.mll` `parser.mly` : lexer and parser specification, uses `menhir` to generate a parser
- `repl.ml` : REPL for the program
- `test.ml` : unit test cases
- `util.ml` : utility functions


## Uses

Run `make` and then `./pi` to run the REPL. 

- `load <filename>` to load in some source code from a file. There are a couple instantiated in the `example/` directory.
- `list` to print out the current program.
- `run` to execute the current program.
- `fuel <num>` to execute the current program given `<num>` amount of evaluation steps.
- `help` to see a reminder of these options.
- `quit` to exit.

