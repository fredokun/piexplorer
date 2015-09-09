
OCAMLC=ocamlc


all: piexplorer


utils.cmo: utils.ml
	$(OCAMLC) -c $< -o $@

pisyntax.cmo: pisyntax.ml utils.cmo
	$(OCAMLC) -c $< -o $@

piexplorer: utils.cmo pisyntax.cmo piexplorer.ml
	$(OCAMLC) $? -o $@


clean:
	rm -f *~
	rm -f *.cmo
	rm -f *.cmi

cleanall: clean
	rm -f piexplorer

