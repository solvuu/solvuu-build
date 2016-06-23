(** Unix tools. See {!Tools} for general documentation. *)
open Ocamlbuild_plugin

val cp : ?f:unit -> Pathname.t -> Pathname.t -> Command.t
val cp_rule : ?f:unit -> dep:Pathname.t -> prod:Pathname.t -> unit

(** Return most recent git commit ID if possible. Assumes there is a
    .git directory in the current working directory. *)
val git_last_commit : unit -> string option

val m4 :
  ?_D:(string * string option) list ->
  infile:Pathname.t ->
  outfile:Pathname.t ->
  Command.t

(** Defaults: [prod = "%.ml"] and [dep = prod ^ ".m4"]. *)
val m4_rule :
  ?_D:(string * string option) list ->
  ?dep:string ->
  ?prod:string ->
  unit ->
  unit
