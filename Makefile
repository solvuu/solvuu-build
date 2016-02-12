PROJECT=$(shell grep "^name" opam/opam | cut -d\" -f2)
VERSION=$(shell grep "^version" opam/opam | cut -d\" -f2)
OCAMLBUILD=ocamlbuild -use-ocamlfind

native: $(PROJECT).cmxa $(PROJECT).cmxs
byte: $(PROJECT).cma

%.cma %.cmxa %.cmxs:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean

_build/META:
	rm -f $@
	@echo "version = \"$(VERSION)\"" >> $@
	@echo "archive(byte) = \"$(PROJECT).cma\"" >> $@
	@echo "archive(native) = \"$(PROJECT).cmxa\"" >> $@
	@echo "exists_if = \"$(PROJECT).cma\"" >> $@
	@echo "requires = \"findlib ocamlbuild ocamlgraph opam-lib\"" >> $@

.PHONY: byte native clean
