(** Solvuu's ocamlbuild plugin. This is very particular to Solvuu's
    internal needs, and not of much general purpose use. *)

module type PROJECT = sig
  val name : string
  val version : string
  val items : Items.t
  val ocamlinit_postfix : string list
end

module Make(Project:PROJECT) : sig

  (** [mllib_file dir lib] returns lines of the .mllib file for
      library with short name [lib]. *)
  val mllib_file : string -> string -> string list

  val merlin_file : string list
  val meta_file : string list
  val install_file : string list
  val ocamlinit_file : string list
  val makefile_rules_file : string list

  val plugin : Ocamlbuild_plugin.hook -> unit
  val dispatch : unit -> unit
end
