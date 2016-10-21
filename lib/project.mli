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


    Libs additionally have the fields:

    - [pack_name]: Make the library consist of a single module with
      given name, constructed as a pack of all the modules that would
      be directly accessible.

    - [dir]: The [dir] in which the library's modules are implemented,
      relative to the repo root. It is assumed that all files in this
      [dir] and no other files comprise the library.

    - [pkg]: The findlib package name for this library. If your
      project installs a single package, you likely want this equal to
      the project name. If it installs several packages, you likely
      want this equal to "project_name.lib_name". In the latter case,
      the dot is interpreted leading to findlib sub-packages.


    Apps additionally have the fields:

    - [file]: Path to the file implementing the app, relative to the
      repo root.
*)


(** Findlib package name. *)
type pkg = string

type app = {
  name : string;
  internal_deps : item list;
  findlib_deps : pkg list;
  file : string;

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
}

and lib = {
  name : string;
  internal_deps : item list;
  findlib_deps : pkg list;
  style : [ `Basic | `Pack of string ];
  dir : string;
  ml_files : string list;
  mli_files : string list;
  c_files : string list;
  pkg : Solvuu_build_findlib.pkg;
  build_plugin : bool;

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
  linkall : unit option;
}

and item = Lib of lib | App of app

(** Construct a [lib]. Most arguments correspond to the OCaml
    compiler or to fields in type [lib]. Other arguments are:

    - [ml_files], [mli_files]: The default list of [ml_files] and
      [mli_files] is the files statically present in [dir]. You can
      [`Add] to these, e.g. when you will be declaring rules to
      generate ml or mli files, or entirely [`Replace] the lists with
      ones you provide. All paths should be relative to [dir].

    - [build_plugin]: Default is true, which means compile cmxs files.
*)
val lib
  :  ?annot:unit
  -> ?bin_annot:unit
  -> ?g:unit
  -> ?safe_string:unit
  -> ?short_paths:unit
  -> ?thread:unit
  -> ?w:string
  -> ?linkall:unit
  -> ?internal_deps:item list
  -> ?findlib_deps:pkg list
  -> ?ml_files:[`Add of string list | `Replace of string list]
  -> ?mli_files:[`Add of string list | `Replace of string list]
  -> ?c_files:[`Add of string list | `Replace of string list]
  -> ?build_plugin:bool
  -> pkg : Solvuu_build_findlib.pkg
  -> style : [ `Basic | `Pack of string ]
  -> dir:string
  -> string
  -> item

val app
  :  ?annot:unit
  -> ?bin_annot:unit
  -> ?g:unit
  -> ?safe_string:unit
  -> ?short_paths:unit
  -> ?thread:unit
  -> ?w:string
  -> ?internal_deps:item list
  -> ?findlib_deps:pkg list
  -> file:string
  -> string
  -> item

val name : item -> string
val internal_deps : item -> item list
val findlib_deps : item -> pkg list

val internal_deps_all : item -> item list
val findlib_deps_all : item -> pkg list

val is_lib : item -> bool
val is_app : item -> bool

val dep_opts_sat : item -> Solvuu_build_findlib.pkg list -> bool
(** [dep_opt_sat x pkgs] returns true if the optional dependencies
    [pkgs] are satisfied for [x], i.e. either [x] doesn't depend on
    the [pkgs] or any package it does depend on is installed. *)

val path_of_lib : suffix:string -> lib -> string
(** Return path to lib file with given suffix. Files are siblings of
    the lib's [dir]. For example, given a lib [x] with [x.dir =
    "src/mylib"], we choose to install files in the directory
    "src/". Thus, [path_of_lib ~suffix:".cma" x] will return
    "src/mylib.cma". *)

val path_of_pack : suffix:string -> lib -> string
(** Return path to packed module file with given suffix. Raise
    exception if given [lib]'s style is not [`Pack _]. *)

val path_of_app : suffix:string -> app -> string
(** Return path to app file with given suffix. Files are in the same
    directory as the app's implementation [file]. For example, assume
    app [x] defines [x.file = "app/myapp.ml"]. Then [path_of_app
    ~suffix:".byte" x] will return "app/myapp.byte". *)

val file_base_of_module : lib -> (string -> string option)
(** For given lib, return a function that can be used to get the base
    path for a module. Example, say [lib] is defined with:
    [lib.ml_files = ["foo.ml"; "bar.ml";]] and [lib.dir = "src"]. Then the
    returned function [f] will give:

    - [f "Foo" = Some "src/foo"]. There is no suffix on the result, so
      that you can append whatever suffix you need.

    - [f "foo" = Some "src/foo"]. The module name is automatically
      capitalized, so you don't have to do this yourself.

    - [f "Car" = None]. No file in lib corresponds to the requested
      module, so we return None.
*)

val module_paths : style_matters:bool -> lib -> string list
(** Return module paths for the modules of given lib. A {i module
    path} is the path to a file implementing a module without any
    suffix. For example, if [lib.ml_files] includes "src/foo.ml", then
    "src/foo" will be in the returned list.

    If [style_matters] is [false], the returned list is based directly
    on [lib.ml_files] and [lib.mli_files]. Setting [style_matters] to
    [true] means the real final modules comprising the library are
    returned. For example, a library that is [Pack]ed actually
    consists of only a single module.
*)

val internal_deps_files : [`Byte | `Native] -> item -> string list
(** Return list of file paths corresponding to the internal
    dependencies of given item, for either byte or native mode.
*)


(******************************************************************************)
(** {2 Static Files} *)
(******************************************************************************)
type content = string list
(** Content of a file represented as a list of lines. *)

val merlin_file : item list -> content

val meta_file : version:string -> lib list -> Fl_metascanner.pkg_expr option
(** Return a findlib META file for given libs, where [version] should
    be the version of your project. Return None if given list is
    empty. *)

val install_file : item list -> content
val ocamlinit_file : ?postfix:string list -> item list -> content
val makefile : project_name:string -> item list -> content

(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
val build_lib : lib -> unit
val build_app : app -> unit

(** [static_file path content] registers a rule to create a file at
    [path] with given [content]. *)
val build_static_file : string -> content -> unit

(******************************************************************************)
(** {2 Plugins} *)
(******************************************************************************)
val basic1 :
  ?additional_rules:((unit -> unit) list) ->
  ?ocamlinit_postfix:string list ->
  project_name:string ->
  version:string ->
  item list ->
  unit

val solvuu1 :
  ?additional_rules:((unit -> unit) list) ->
  ?ocamlinit_postfix:string list ->
  project_name:string ->
  version:string ->
  item list ->
  unit


(******************************************************************************)
(** {2 List Operations} *)
(******************************************************************************)

(** Return all findlib packages mentioned in all given items. *)
val all_findlib_pkgs : item list -> pkg list

val filter_libs : item list -> lib list
val filter_apps : item list -> app list

(******************************************************************************)
(** {2 Item Module} *)
(******************************************************************************)
module Item : sig
  type t = item
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  type typ = [`Lib | `App]
  val typ : t -> typ
  val typ_to_string : typ -> string
end

(******************************************************************************)
(** {2 Graph Operations} *)
(******************************************************************************)
module Graph : sig
  include module type of Graph.Persistent.Digraph.Concrete(Item)

  module Dfs : module type of Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(Item)
  )

  module Topological : sig
    include module type of Graph.Topological.Make(
      Graph.Persistent.Digraph.Concrete(Item)
    )

    (** Return a topologically sorted vertex list of given graph. *)
    val sort : t -> V.t list
  end

  (** Construct a graph from a list of items. Raise exception if there
      are cycles or any other errors. *)
  val of_list : Item.t list -> t
end
