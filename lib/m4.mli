(** m4 support. *)
open Ocamlbuild_plugin

val m4 :
  ?_D:(string * string option) list ->
  infile:Pathname.t ->
  outfile:Pathname.t ->
  Command.t
