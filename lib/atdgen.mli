(** atdgen support. *)
open Ocamlbuild_plugin

val atdgen :
  ?j:unit ->
  ?j_std:unit ->
  ?t:unit ->
  Pathname.t ->
  Command.t
