(** m4 support. *)
open Ocamlbuild_plugin

val m4 :
  ?_D:(string * string option) list ->
  infile:Pathname.t ->
  outfile:Pathname.t ->
  Command.t

(** Register a rule to process a .m4 file. By default, [prod = "%.ml"]
    and [dep = prod ^ ".m4"]. *)
val m4_rule :
  ?_D:(string * string option) list ->
  ?dep:string ->
  ?prod:string ->
  unit ->
  unit
