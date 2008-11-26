include ../Makefile.inc

all: build tests

build: flarelib.ml yaml_utils.ml
	$(ML_LIB) flarelib.ml
	$(OCAMLFIND) $(OCAMLC) -c       \
		-package "pcre yaml netplex"	\
		flarelib.cmo                  \
		yaml_utils.ml                 \
		-linkpkg

tests:

clean:
	rm yaml_utils.cmo yaml_utils.cmi
	rm flarelib.cmo flarelib.cmi
