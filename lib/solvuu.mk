OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(solvuu-build)"

include _build/project.mk

_build/project.mk:
	$(OCAMLBUILD) $(notdir $@)
