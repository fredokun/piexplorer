
TAR=tar

SOURCES= version.ml utils.ml pisyntax.ml pinorm.ml control.ml piparser.mly pilexer.mll piexplorer.ml
RESULT= piexplorer

all: byte-code

-include OCamlMakefile

archive: Makefile OCamlMakefile utils.ml pisyntax.ml control.ml piparser.mly pilexer.mll piexplorer.ml
	mkdir piexplorer-${shell date +%F}
	mkdir piexplorer-${shell date +%F}/tests
	cp -f $? piexplorer-${shell date +%F}
	cp -f tests/*.js piexplorer-${shell date +%F}/tests/
	$(TAR) cvzf piexplorer-${shell date +%F}.tar.gz piexplorer-${shell date +%F}
	rm -rf piexplorer-${shell date +%F}

cleanall: clean
	rm -f *~
	rm -f piexplorer
	rm -f piexplorer-*.tar.gz
