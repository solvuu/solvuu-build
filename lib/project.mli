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
(** {2 Static Files} *)
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

(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
module Rule : sig

  (** [static_file path content] registers a rule to create a file at
      [path] with given [content]. *)
  val static_file : string -> content -> unit

  (** Register lines of a _tags file. Note an _tags file is not
      printed. *)
  val tags_file : content -> unit

  val ml_m4_to_ml : git_commit:string option -> version:version -> unit
  val atd_to_t : unit -> unit
  val atd_to_j : unit -> unit
  val mlpack : Item.lib -> unit
  val mllib : Item.lib -> unit
  val libs_byte_native : Item.lib -> unit
  val clib : Item.lib -> unit
  val project_files : unit -> unit
end
