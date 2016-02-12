OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(solvuu_build)"

include _build/Makefile.rules

_build/Makefile.rules:
	$(OCAMLBUILD) $(notdir $@)
