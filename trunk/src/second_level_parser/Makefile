include ../Makefile.inc

all: build tests

build: basic_types.ml grammar.mly second_level_parser.mll expression_evaluator.ml
	$(ML_LIB) basic_types.ml
	ocamlyacc grammar.mly
	rm grammar.mli							
			# I don't know which is the correct way to do it,
			# but after creating the .mli file it complains
			# when linking the library. - RAMC
	$(ML_LIB) grammar.ml
	$(OCAMLLEX) second_level_parser.mll
	$(ML_LIB) second_level_parser.ml
	$(ML_LIB) expression_evaluator.ml
	
tests:

clean:
	rm basic_types.cmo basic_types.cmi
	rm grammar.cmo grammar.cmi grammar.ml
	rm second_level_parser.cmo second_level_parser.cmi second_level_parser.ml
	rm expression_evaluator.cmo expression_evaluator.cmi
