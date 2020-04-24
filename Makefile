SOURCES = ast.ml

.PHONY: all

all: pi

clean:
	rm -f *.out *.cmo *.cmx *.cmi

pi: $(SOURCES)
	ocamlc -o pi -g str.cma $(SOURCES)
