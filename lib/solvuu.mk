OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(solvuu_build)"

include _build/project.mk

_build/project.mk:
	$(OCAMLBUILD) $(notdir $@)
