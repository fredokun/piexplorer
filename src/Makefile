
OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

all: piexplorer


utils.cmo: utils.ml
	$(OCAMLC) -c $< -o $@

pisyntax.cmo: pisyntax.ml utils.cmo
	$(OCAMLC) -c $< -o $@

control.cmo: control.ml pisyntax.cmo utils.cmo
	$(OCAMLC) -c $< -o $@

piparser.ml: piparser.mly
	$(OCAMLYACC) $<

piparser.cmo: piparser.ml pisyntax.cmo
	$(OCAMLC) -c $< -o $@

pilexer.ml: pilexer.mll piparser.mly
	$(OCAMLLEX) $<

pilexer.cmo: pilexer.ml piparser.cmo
	$(OCAMLC) -c $< -o $@

piexplorer: utils.cmo pisyntax.cmo control.cmo piparser.cmo pilexer.cmo piexplorer.ml
	$(OCAMLC) $? -o $@


clean:
	rm -f *~
	rm -f *.cmo
	rm -f *.cmi

cleanall: clean
	rm -f piexplorer

