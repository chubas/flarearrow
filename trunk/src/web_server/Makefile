include ../Makefile.inc

WEB_PKG = "yaml,netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex"

all: build tests

build: yaml_config_parser.ml server_utils.ml server.ml 
	$(OCAMLFIND) $(OCAMLC) -c \
		-package $(WEB_PKG)				\
		flarelib.cmo				\
		yaml_config_parser.ml			\
		-linkpkg
	$(OCAMLFIND) $(OCAMLC) -c	\
		-package $(WEB_PKG)	-thread		\
		flarelib.cmo				\
		first_level_parser.cmo				\
		basic_types.cmo					\
		grammar.cmo					\
		second_level_parser.cmo				\
		expression_evaluator.cmo			\
		yaml_config_parser.cmo		\
		server_utils.ml -linkpkg
	$(OCAMLFIND) $(OCAMLC) -o server		\
		-package $(WEB_PKG)	-thread		\
		flarelib.cmo				\
		first_level_parser.cmo				\
		basic_types.cmo					\
		grammar.cmo					\
		second_level_parser.cmo				\
		expression_evaluator.cmo			\
		yaml_config_parser.cmo		\
		server_utils.cmo		\
		server.ml -linkpkg
		
tests: 

clean:
	rm server.cmo server.cmi server
	rm server_utils.cmo server_utils.cmi
	rm yaml_config_parser.cmo yaml_config_parser.cmi
