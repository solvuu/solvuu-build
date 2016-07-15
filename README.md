# Solvuu's build system.

## Install
Do `opam install solvuu-build`.

## Instructions
This project provides:

  * An ocamlbuild plugin  
    Use it by starting your project's `myocamlbuild.ml` file with
    `open Solvuu_build`.

  * solvuu.mk  
    A standard makefile, which assumes you are using the above
    ocamlbuild plugin. Your project's Makefile usually will need only
    the single line `include $(shell opam config var
    solvuu-build:lib)/solvuu.mk`.
