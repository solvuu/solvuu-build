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
