(** Project information. A project consists of libraries and
    executable applications.
*)

type name = string
(** Project's name. *)

type version = string
(** Project's version. *)

type t = private {
  libs : Item.lib list;
  apps : Item.app list;

  tags_file : string list;
  merlin_file : string list;
  meta_file : string list;
  install_file : string list;
  ocamlinit_file : string list;
  makefile_rules_file : string list;

  plugin : Ocamlbuild_plugin.hook -> unit;
  dispatch : unit -> unit;
}

val make
  :  ?ocamlinit_postfix:string list
  -> name:name
  -> version:version
  -> Item.t list
  -> t

(******************************************************************************)
(** {2 Low Level Functions} *)
(******************************************************************************)
type content = string list
(** Content of a file represented as a list of lines. *)

val git_commit : unit -> string option
val tags_file : Item.t list -> content
val mllib_file : Item.lib -> content
val merlin_file : Item.t list -> content
val meta_file : Item.lib list -> version -> content
val install_file : Item.t list -> content
val ocamlinit_file : Item.t list -> postfix:string list -> content
val makefile_rules_file : Item.t list -> name -> content

val make_static_file : string -> content -> unit
(** [make_static_file path content] registers a rule to create a file
    at [path] with given [content]. *)
