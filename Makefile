SOURCES = ast.ml eval.ml
EXE = ast.native eval.native

.PHONY: all

all: fl

clean:
	rm -f *.out *.cmo *.cmx *.cmi

fl: $(SOURCES)
	ocamlc -o fl -g str.cma $(SOURCES)

%.native: %.ml
	ocamlbuild -cflags -g -lflags -g -cflags -w -cflags -8 $@
