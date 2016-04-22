(** Type of items that can be built, i.e. an app or lib. Defined by:

    - [typ]: Type of item, either an executable app or an OCaml
      library.

    - [name]: Base name of the item. For an app, this would be the
      desired filename of the executable. However, .byte and .native
      extensions might be present at various stages. For a library,
      this name will have suffixes such as .cmo and .cma added. In
      both cases, dashes are valid.

    - [internal_deps]: List of other items that this item directly
      depends on. The normal case is to depend on libraries. However,
      we allow depending on apps too, e.g. building a library might
      require first building an app that will be used to auto-generate
      some of the code for the library. The list should include only
      other items that will be built by the same project. Only {i
      direct} dependencies should be listed; indirect dependencies
      will be inferred automatically.

    - [findlib_deps]: In addition to internal dependencies, a given
      item can depend on other findlib packages. Again, only {i
      direct} dependencies should be listed.

    - [build_if]: Normally failure to build an item is an error, but
      sometimes you want to consider an item to be optional. An item
      is built only if the conjunction of all conditions specified are
      satisfied.
*)

type name = string

(** Predicate on an item.

    - [`Pkgs_installed]: all findlib pkgs are present.
*)
type condition = [
  | `Pkgs_installed
]

(** Findlib package name. *)
type pkg = string

type app = {
  name : name;
  internal_deps : t list;
  findlib_deps : pkg list;
  build_if : condition list;
}

and lib = {
  name : name;
  internal_deps : t list;
  findlib_deps : pkg list;
  build_if : condition list;
}

and t = Lib of lib | App of app

type typ = [`Lib | `App]

val lib
  :  ?internal_deps:t list
  -> ?findlib_deps:pkg list
  -> ?build_if:condition list
  -> name
  -> t

val app
  :  ?internal_deps:t list
  -> ?findlib_deps:pkg list
  -> ?build_if:condition list
  -> name
  -> t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val typ : t -> typ
val name : t -> name
val internal_deps : t -> t list
val findlib_deps : t -> pkg list
val build_if : t -> condition list

val internal_deps_all : t -> t list
val findlib_deps_all : t -> pkg list

val is_lib : t -> bool
val is_app : t -> bool

val typ_to_string : typ -> string

(******************************************************************************)
(** {2 Graph Operations} *)
(******************************************************************************)

(** Items types and functions wrapped into a module. *)
module T : sig
  type nonrec t = t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Graph : sig
  include module type of Graph.Persistent.Digraph.Concrete(T)

  module Dfs : module type of Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(T)
  )

  module Topological : module type of Graph.Topological.Make(
    Graph.Persistent.Digraph.Concrete(T)
  )

  (** Construct a graph from a list of items. Raise exception if there
      are cycles or any other errors. *)
  val of_list : T.t list -> t
end
