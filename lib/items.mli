(** Collection of items comprising a project. The items'
    [internal_deps] must not lead to a cycle. *)

type pkg = Solvuu_build_findlib.pkg
type name = Item.name
type typ = Item.typ
type lib = Item.lib
type app = Item.app

type item = Item.t = Lib of lib | App of app

type t = private item list

val of_list : item list -> t

val filter_libs : item list -> lib list
val filter_apps : item list -> app list

(** [find t typ name] returns the item with given [typ] and
    [name]. Raise exception if no such item in [t]. *)
val find : t -> typ -> name -> item

val find_lib : t -> name -> lib
val find_app : t -> name -> app

(** Return direct internal dependencies of item with given [typ] and
    [name]. *)
val internal_deps : t -> typ -> name -> item list

(** Return all internal dependencies of item with given [typ] and
    [name]. *)
val internal_deps_all : t -> typ -> name -> item list

(** Return findlib packages that item with given [typ] and [name]
    directly depends on. *)
val findlib_deps : t -> typ -> name -> pkg list

(** Return all findlib packages item with given [typ] and [name]
    depends on. *)
val findlib_deps_all : t -> typ -> name -> pkg list

(** Return all findlib packages mentioned in all items. *)
val all_findlib_pkgs : t -> pkg list

(** Return [true] if given item should be built according to its
    [build_if] conditions. The set of items [t] is needed because
    dependencies matter; an item should be built only if all of its
    dependencies should. *)
val should_build : t -> item -> bool

val topologically_sorted : t -> item list
