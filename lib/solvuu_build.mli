(** Solvuu's ocamlbuild plugin. This is very particular to Solvuu's
    internal needs, and not of much general purpose use. *)

(** Findlib operations. *)
module Findlib : sig
  type pkg = string
  val installed : pkg -> bool
end

(** Type of items that can be built, defined by:

    - [typ]: Type of item, either an executable app or an OCaml
      library.

    - [name]: Base name of the item. For an app, this would be the
      desired filename of the executable. However, .byte and .native
      extensions might be present at various stages. For a library,
      this name will have suffixes such as .cmo and .cma added. In
      both cases, dashes are valid.

    - [libs]: List of other items that this item directly
      depends on. The normal case is to depend on libraries. However,
      we allow depending on apps too, e.g. building a library might
      require first building an app that will be used to auto-generate
      some of the code for the library. The list should include only
      other items that will be built by the same project. Only {i
      direct} dependencies should be listed; indirect dependencies
      will be inferred automatically.

    - [pkgs]: In addition to internal dependencies, a given
      item can depend on other findlib packages. Again, only {i
      direct} dependencies should be listed.

    - [build_if]: Normally failure to build an item is an error, but
      sometimes you want to consider an item to be optional. An item
      is built only if the conjunction of all conditions specified are
      satisfied.

*)
module Item : sig
  type name = string

  (** Predicate on an item *)
  type condition = [
    | `Pkgs_installed (** all finlid pkgs are present *)
  ]

  type app = {
    name : name;
    libs : t list;
    pkgs : Findlib.pkg list;
    build_if : condition list;
  }

  and lib = {
    name : name;
    libs : t list;
    pkgs : Findlib.pkg list;
    build_if : condition list;
  }

  and t = Lib of lib | App of app

  type typ = [`Lib | `App]

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val typ : t -> typ
  val name : t -> name
  val libs : t -> t list
  val pkgs : t -> Findlib.pkg list
  val build_if : t -> condition list

  val is_lib : t -> bool
  val is_app : t -> bool

  val typ_to_string : typ -> string
end

(** Collection of items comprising a project. The items'
    [libs] must not lead to a cycle. *)
module Items : sig
  type t = private Item.t list

  val of_list : Item.t list -> t

  val filter_libs : Item.t list -> Item.lib list
  val filter_apps : Item.t list -> Item.app list

  (** [get t typ name] returns the item with given [typ] and
      [name]. Raise exception if no such item in [t]. *)
  val get : t -> Item.typ -> Item.name -> Item.t

  val find_lib : t -> Item.name -> Item.lib
  val find_app : t -> Item.name -> Item.app

  (** Return direct internal dependencies of item with given [typ] and
      [name]. *)
  val lib_deps : t -> Item.typ -> Item.name -> Item.t list

  (** Return all internal dependencies of item with given [typ] and
      [name]. *)
  val lib_deps_all : t -> Item.typ -> Item.name -> Item.t list

  (** Return findlib packages that item with given [typ] and [name]
      directly depends on. *)
  val pkgs_deps : t -> Item.typ -> Item.name -> Findlib.pkg list

  (** Return all findlib packages item with given [typ] and [name]
      depends on. *)
  val pkgs_deps_all : t -> Item.typ -> Item.name -> Findlib.pkg list

  (** Return all findlib packages mentioned in all items. *)
  val all_findlib_pkgs : t -> Findlib.pkg list

  (** Return [true] if given item should be built according to its
      [build_if] conditions. The set of items [t] is needed because
      dependencies matter; an item should be built only if all of its
      dependencies should. *)
  val should_build : t -> Item.t -> bool

  val topologically_sorted : t -> t

end

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

module Util : sig

  (** [readdir dir] returns the contents of [dir]. Returns empty list
      if [dir] doesn't exist or is not a directory. *)
  val readdir : string -> string list

  (** Given a filename, return the 1 or more module names. Examples:

      - "foo.ml" yields "Foo"
      - "foo.mli" yields "Foo"

      - "foo.atd" yields ["Foo_t"; "Foo_j"], i.e. we assume atdgen is
         used in a specific way.
  *)
  val modules_of_file : string -> string list

  (** [modules_of_dir dir] applies [modules_of_file] to every file in
      [dir] and returns the concatenated result. *)
  val modules_of_dir : string -> string list

  (** Return ".c" files in given dir, with the ".c" extension chopped
      off. *)
  val c_units_of_dir : string -> string list

  (** Return ".h" files in given dir, with the ".h" extension preserved. *)
  val h_files_of_dir : string -> string list

  (** Generate list of modules from contents of given [dir], and
      return corresponding mlpack file. *)
  val mlpack_file : string -> string list

  (** [clib_file dir lib] returns lines if the clib file corresponding
      to any .c files in [dir], if any. *)
  val clib_file : string -> string -> string list option

end
