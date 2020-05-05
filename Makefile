# SOURCES = util.ml ast.ml vanilla.ml
EXE = util.native ast.native eval.native vanilla.native

.PHONY: all

all: $(EXE)

clean:
	rm -rf _build/

# fl: $(SOURCES)
# 	ocamlc -o fl -g str.cma $(SOURCES)

%.native: %.ml
	ocamlbuild -cflags -g -lflags -g -cflags -w -cflags -8 $@
