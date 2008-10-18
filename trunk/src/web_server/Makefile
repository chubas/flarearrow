include ../Makefile.inc

WEB_PKG = "yaml,netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex"

all: build tests

build: server.ml
	$(OCAMLFIND) $(OCAMLC) -o server		\
		-package $(WEB_PKG)	-thread		\
		server.ml -linkpkg
		
tests: 

clean:
	rm server.cmo server.cmi server

