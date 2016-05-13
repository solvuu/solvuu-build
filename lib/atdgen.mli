(** atdgen support. *)
open Ocamlbuild_plugin

val atdgen :
  ?j:unit ->
  ?j_std:unit ->
  ?t:unit ->
  Pathname.t ->
  Command.t

(** Register a rule to run [atdgen -t] on the given [dep], which must
    have a ".atd" suffix. Default [dep] is "%.atd". Files produced
    will be "%_t.ml" and "%_t.mli". *)
val atdgen_t_rule : ?dep:string -> ?j_std:unit -> unit -> unit

(** Register a rule to run [atdgen -j] on the given [dep], which must
    have a ".atd" suffix. Default [dep] is "%.atd". Files produced
    will be "%_j.ml" and "%_j.mli". *)
val atdgen_j_rule : ?dep:string -> ?j_std:unit -> unit -> unit
