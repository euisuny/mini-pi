EXE = util.native ast.native eval.native test.native

.PHONY: all

all: $(EXE)

clean:
	rm -rf _build/

# fl: $(SOURCES)
# 	ocamlc -o fl -g str.cma $(SOURCES)

%.native: %.ml
	ocamlbuild -cflags -g -lflags -g -cflags -w -cflags -8 $@
