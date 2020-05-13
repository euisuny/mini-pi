SOURCES = util.ml ast.ml parser.mli parser.ml lexer.ml eval.ml repl.ml
GENERATED = parser.mli parser.ml parser.output lexer.ml

.PHONY: all

all: pi

clean:
	rm -f pi *.out *.cmo *.cmx *.cmi $(GENERATED)

pi: $(SOURCES)
	ocamlc -o pi -g str.cma $(SOURCES)

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll parser.ml
	ocamllex lexer.mll
