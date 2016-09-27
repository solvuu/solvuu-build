PROJECT=solvuu-build
PACK=solvuu_build
VERSION=0.1.0
OCAMLBUILD=ocamlbuild -use-ocamlfind
DEMOS=$(sort $(dir $(wildcard demo/*/)))

default: byte

native: $(PROJECT).cmxa $(PROJECT).cmxs
byte: $(PROJECT).cma

%.cma %.cmxa %.cmxs:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean
	rm -f $(PROJECT).install
	$(foreach demo,$(DEMOS),make -C $(demo) clean;)

_build/META:
	mkdir -p _build
	ocaml bin/make-helper.ml META $(PROJECT) $(VERSION) > $@

solvuu-build.install:
	ocaml bin/make-helper.ml install $(PROJECT) $(PACK) > $@

test:
	$(foreach demo,$(DEMOS),make -C $(demo);)

.PHONY: byte native clean test
