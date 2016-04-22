(** Project information. A project consists of libraries and
    executable applications.
*)

type name = string
(** Project's name. *)

type version = string
(** Project's version. *)

type t

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
