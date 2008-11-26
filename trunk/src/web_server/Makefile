include ../Makefile.inc

WEB_PKG = "yaml,netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex,pcre,mysql"

all: build tests

build: server_config.ml server_utils.ml server.ml
	$(OCAMLFIND) $(OCAMLC) -c       \
		-package $(WEB_PKG)           \
		flarelib.cmo                  \
		position.cmo                  \
		grammar_flp.cmo               \
		first_level_parser.cmo        \
		template_evaluator.cmo        \
		basic_types.cmo               \
		grammar.cmo                   \
		second_level_parser.cmo       \
		expression_evaluator.cmo      \
		server_utils.ml -linkpkg
	$(OCAMLFIND) $(OCAMLC) -c       \
		-package $(WEB_PKG)           \
		flarelib.cmo                  \
		position.cmo                  \
		grammar_flp.cmo               \
		first_level_parser.cmo        \
		template_evaluator.cmo        \
		basic_types.cmo               \
		grammar.cmo                   \
		second_level_parser.cmo       \
		expression_evaluator.cmo      \
		server_utils.cmo              \
		../../controllers/controllers.ml -linkpkg
	$(OCAMLFIND) $(OCAMLC) -c       \
		-package $(WEB_PKG)           \
		flarelib.cmo                  \
		yaml_utils.cmo                \
		controllers.cmo               \
		server_config.ml              \
		-linkpkg
	$(OCAMLFIND) $(OCAMLC) -o server\
		-package $(WEB_PKG) -thread   \
		flarelib.cmo                  \
		yaml_utils.cmo                \
		position.cmo                  \
		grammar_flp.cmo               \
		first_level_parser.cmo        \
		template_evaluator.cmo        \
		basic_types.cmo               \
		grammar.cmo                   \
		second_level_parser.cmo       \
		expression_evaluator.cmo      \
		server_utils.cmo              \
		controllers.cmo               \
		server_config.cmo             \
		server.ml -linkpkg
tests: 

clean:
	rm server.cmo server.cmi server
	rm server_utils.cmo server_utils.cmi
	rm server_config.cmo server_config.cmi
	rm ../../controllers/controllers.cmo ../../controllers/controllers.cmi
