(** Findlib operations. Findlib supports package hierarchies, which
    are denoted as dot separated path. For example, "foo.bar"
    represents the "bar" sub-package of the parent package "foo".
*)

type pkg = string

val installed : pkg -> bool

(** Generate ocamlbuild's "use_foo" tag for given package
    name. Example: [to_use_tag "foo.bar"] returns ["use_foo_bar"]. *)
val to_use_tag : pkg -> string

(** Split [pkg] on dots to expose its path structure. *)
val to_path : pkg -> string list

val build_meta_file : ?prod:string -> Fl_metascanner.pkg_expr -> unit
(** Register a rule to print out contents of given [pkg_expr] to
    path [prod], which by default is "./META". *)

(******************************************************************************)
(** {2 Graph Operations} *)
(******************************************************************************)

(** Items types and functions wrapped into a module. *)
module T : sig
  type t = pkg
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Graph : sig
  include module type of Graph.Persistent.Digraph.Concrete(T)

  module Dfs : module type of Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(T)
  )

  module Topological : sig
    include module type of Graph.Topological.Make(
      Graph.Persistent.Digraph.Concrete(T)
    )

    (** Return a topologically sorted vertex list of given graph. *)
    val sort : t -> V.t list
  end

  module Gml : sig
    val print : t -> string
  end

  val roots : t -> V.t list

  (** Construct a graph from a list of items. Raise exception if there
      are cycles or any other errors. *)
  val of_list : V.t list -> t
end
