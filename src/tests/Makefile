include ../Makefile.inc

OUNIT = "unix,oUnit"

all: build

build: 	test_first_level_parser.ml \
		test_expression_evaluator.ml			\
		test_control_blocks.ml \
		test_all.ml
	$(OCAMLFIND) $(OCAMLC) -c \
		-package $(OUNIT)				\
		test_first_level_parser.ml			\
		-linkpkg
	$(OCAMLFIND) $(OCAMLC) -c \
		-package $(OUNIT)				\
		test_control_blocks.ml			\
		-linkpkg
	$(OCAMLFIND) $(OCAMLC) -c \
		-package $(OUNIT)				\
		test_expression_evaluator.ml			\
		-linkpkg
	$(OCAMLFIND) $(OCAMLC) \
		-o test_all					\
		flarelib.cmo					\
		position.cmo				\
		grammar_flp.cmo				\
		first_level_parser.cmo				\
		template_evaluator.cmo				\
		test_first_level_parser.cmo			\
		basic_types.cmo					\
		grammar.cmo					\
		second_level_parser.cmo				\
		expression_evaluator.cmo			\
		test_control_blocks.cmo			\
		test_expression_evaluator.cmo			\
		-package $(OUNIT)				\
		test_all.ml					\
		-linkall					\
		-linkpkg

tests: 	build							\
		test_first_level_parser.html			\
		test_expression_evaluator.html			\
		test_expression_evaluator_result.html
	#cd ../web_server; ./server
	./test_all
	#$(ECHO) "(!!!) TODO:"
	#$(ECHO) "(!!!) Make sure to kill server process after the tests!"
	#python test_web_server.py

clean:
	rm test_all test_all.cmo test_all.cmi
	rm test_expression_evaluator.cmo test_expression_evaluator.cmi
	rm test_control_blocks.cmo test_control_blocks.cmi
	rm test_first_level_parser.cmo test_first_level_parser.cmi
