include Makefile.inc

all: build tests

build: lib first_level_parser second_level_parser web_server

##### INDIVIDUAL MAKE SECTION #####

lib: force_look
	cd lib; $(MAKE) $(MFLAGS)

first_level_parser: lib force_look
	cd first_level_parser; $(MAKE) $(MFLAGS)

second_level_parser: lib first_level_parser force_look
	cd second_level_parser;	$(MAKE) $(MFLAGS)

web_server: force_look
	cd web_server; $(MAKE) $(MFLAGS)

##### TEST BUILD SECTION #####

build_tests: force_look
	cd tests; $(MAKE) $(MFLAGS)

tests: build build_tests
	cd tests; $(MAKE) tests $(MFLAGS)

##### CLEAN SECTION #####

clean:	clean_lib 								\
				clean_first_level_parser 	\
				clean_second_level_parser	\
				clean_web_server					\
				clean_tests

clean_lib: force_look
	cd lib; $(MAKE) clean $(MFLAGS)

clean_first_level_parser: force_look
	cd first_level_parser; $(MAKE) clean $(MFLAGS)

clean_second_level_parser: force_look
	cd second_level_parser; $(MAKE) clean $(MFLAGS)

clean_web_server: force_look
	cd web_server; $(MAKE) clean $(MFLAGS)

clean_tests: force_look
	cd tests; $(MAKE) clean $(MFLAGS)
	

force_look:
