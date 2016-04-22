PROJECT=solvuu_build
VERSION=dev
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

solvuu_build.install:
	ocaml bin/make_install_file.ml > $@

.PHONY: byte native clean
