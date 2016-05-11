(** Project information. A project consists of libraries and
    executable applications.
*)

type name = string
(** Project's name. *)

type version = string
(** Project's version. *)

type t = private {
  name : name;
  version : version;
  git_commit : string option;
  libs : Item.lib list;
  apps : Item.app list;
  merlin_file : string list;
  meta_file : Fl_metascanner.pkg_expr;
  install_file : string list;
  ocamlinit_file : string list;
  makefile_rules_file : string list;
}

val make
  :  ?ocamlinit_postfix:string list
  -> name:name
  -> version:version
  -> Item.t list
  -> t

val plugin : t -> (Ocamlbuild_plugin.hook -> unit)
val dispatch : t -> unit

(******************************************************************************)
(** {2 Static Files} *)
(******************************************************************************)
type content = string list
(** Content of a file represented as a list of lines. *)

val merlin_file : Item.t list -> content
val meta_file : Item.lib list -> version -> Fl_metascanner.pkg_expr
val install_file : Item.t list -> content
val ocamlinit_file : Item.t list -> postfix:string list -> content
val makefile_rules_file : Item.t list -> name -> content

(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
module Rule : sig

  (** [static_file path content] registers a rule to create a file at
      [path] with given [content]. *)
  val static_file : string -> content -> unit

  (* val clib : Item.lib -> unit *)
  val project_files : unit -> unit
end
