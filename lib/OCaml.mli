(** OCaml commands. *)
open Ocamlbuild_plugin

val ocamlc
  :  [`c]
  -> ?i:string list -> ?o:string
  -> Pathname.t list
  -> Command.t

val ocamlopt
  :  [`c]
  -> ?i:string list -> ?o:string
  -> Pathname.t list -> Command.t

val ocamldep
  :  ?modules:unit
  -> ?i:string list
  -> Pathname.t list
  -> (string * string list) list
