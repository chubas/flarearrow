include ../Makefile.inc

all: build tests

build: first_level_parser.mll grammar_flp.mly position.ml template_evaluator.ml
	$(ML_LIB) position.ml
	ocamlyacc grammar_flp.mly
	rm grammar_flp.mli
	$(ML_LIB) grammar_flp.ml
	$(OCAMLLEX) first_level_parser.mll
	$(ML_LIB) first_level_parser.ml
	$(ML_LIB) template_evaluator.ml

tests:

clean:
	rm position.cmo position.cmi
	rm grammar_flp.cmo grammar_flp.cmi grammar_flp.ml
	rm first_level_parser.cmo first_level_parser.cmi first_level_parser.ml
	rm template_evaluator.cmo template_evaluator.cmi
