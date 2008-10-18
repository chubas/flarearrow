LIBS 		= -I ../lib -I ../first_level_parser -I ../second_level_parser -I ../tests
OCAMLLEX 	= ocamllex
OCAMLC 		= ocamlc -g $(LIBS)
OCAMLFIND	= ocamlfind
ML_LIB		= $(OCAMLFIND) $(OCAMLC) -c

MAKE 		= make
ECHO		= echo
