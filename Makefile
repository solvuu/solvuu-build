PROJECT=solvuu_build
VERSION=dev
OCAMLBUILD=ocamlbuild -use-ocamlfind
DEMOS=$(sort $(dir $(wildcard demo/*/)))

native: $(PROJECT).cmxa $(PROJECT).cmxs
byte: $(PROJECT).cma

%.cma %.cmxa %.cmxs:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean
	$(foreach demo,$(DEMOS),make -C $(demo) clean;)

_build/META:
	rm -f $@
	@echo "version = \"$(VERSION)\"" >> $@
	@echo "archive(byte) = \"$(PROJECT).cma\"" >> $@
	@echo "archive(native) = \"$(PROJECT).cmxa\"" >> $@
	@echo "exists_if = \"$(PROJECT).cma\"" >> $@
	@echo "requires = \"findlib ocamlbuild ocamlgraph\"" >> $@

solvuu_build.install:
	ocaml bin/make_install_file.ml > $@

test:
	$(foreach demo,$(DEMOS),make -C $(demo);)

.PHONY: byte native clean test
