include ../Makefile.inc

all: build tests

build: first_level_parser.mll
	$(OCAMLLEX) first_level_parser.mll
	$(ML_LIB) first_level_parser.ml

tests:

clean:
	rm first_level_parser.cmo first_level_parser.cmi first_level_parser.ml
