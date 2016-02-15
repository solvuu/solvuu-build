PROJECT=solvuu_build
VERSION=0.0.1
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
	@echo "requires = \"findlib ocamlbuild ocamlgraph\"" >> $@

.PHONY: byte native clean
