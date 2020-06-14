SOURCES = util.ml ast.ml parser.mli parser.ml lexer.ml eval.ml repl.ml
GENERATED = parser.mli parser.ml parser.output lexer.ml

# UNIT_TEST = util.native ast.native eval.native test.native

.PHONY: all test

all: pi

# test: $(UNIT_TEST)

# %.native: %.ml
# 	ocamlbuild -cflags -g -lflags -g -cflags -w -cflags -8 $@

clean:
	rm -f pi *.out *.cmo *.cmx *.cmi $(GENERATED)

pi: $(SOURCES)
	ocamlc -o pi str.cma $(SOURCES)

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll parser.ml
	ocamllex lexer.mll

