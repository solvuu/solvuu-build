(** Solvuu's ocamlbuild plugin. This is very particular to Solvuu's
    internal needs, and not of much general purpose use. *)

(** Information about a library or app provided by this project. *)
module Info : sig

  (** Library or app name. *)
  type name =  [`Lib of string | `App of string]

  (** Predicate on an item *)
  type condition = [
    | `Pkgs_installed (** all findlib packages dependencies are present *)
  ]

  type item = {
    name : name ;

    libs : string list ;
    (** Internal libraries this library or app directly depends on. *)

    pkgs : string list ;
    (** Additional ocamlfind packages this library or app depends
	on. By "additional", we mean it is not necessary to list
	packages that are already listed for one of this item's
	[libs]. *)

    build_if : condition list ;
    (** Conditions on which to build the item. Empty list means
        unconditionally build the package. *)
  }

  type t = private item list

  val of_list : item list -> t

  val libs : t -> t
  val apps : t -> t
  val names : t -> string list

  val name_as_string : name -> string

  (** Returns item in [t] with given [name]. *)
  val get : t -> name -> item

  (** Returns direct dependencies of item with given [name]. *)
  val libs_direct : t -> name -> string list

  (** Returns all dependencies of item with given [name]. *)
  val libs_all : t -> name -> string list

  (** Returns all packages item with given [name] directly depends
      on. *)
  val pkgs_direct : t -> name -> string list

  (** Returns all packages item with given [name] depends on. *)
  val pkgs_all : t -> name -> string list

end

module type PROJECT = sig
  val info : Info.t
  val ocamlinit_postfix : string list
end

module Make(Project:PROJECT) : sig
  val project_name : string
  val project_version : string
  val modules_of_dir : string -> string list
  val plugin : Ocamlbuild_plugin.hook -> unit
  val dispatch : unit -> unit
end
