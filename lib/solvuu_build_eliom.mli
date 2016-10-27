(** Build Eliom projects. This module is analogous to the main
    {!Project} module but tailored to Eliom. In your own projects, you
    will likely use either this module or {!Project}, not both.

    With Eliom, a single set of source files is compiled twice, once
    for the server side and once for the client side. The two
    libraries work in unison and thus the main [lib] type here really
    defines 2 libraries. Several constructs are parameterized over a
    [host] type, which can be either [`Server] or [`Client], to
    indicate this.

    We assume a lot more here than in {!Project}. For example,
    {!Project} very much supports the definition of multiple libraries
    within a single project. Here, we assume you're building just a
    single library ("single" for each of client and server).
*)

(** Findlib package name. *)
type pkg = Solvuu_build_findlib.pkg

type host = [ `Server | `Client ]

type filename = string
(** A filename without any directory component. A value [x] of this
    type is always used in the context of some [lib], and thus the
    full path of the file is understood to be [lib.dir/x]. *)

type lib = private {
  name : string;
  style : [ `Pack of string ];
  dir : string;
  findlib_deps : (host -> pkg list);
  ml_files : (host -> filename list);
  mli_files : (host -> filename list);

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
}

(** Construct a lib from the given directory. Raise exception in case
    of any errors. *)
val lib
  :  ?annot:unit
  -> ?bin_annot:unit
  -> ?g:unit
  -> ?safe_string:unit
  -> ?short_paths:unit
  -> ?thread:unit
  -> ?w:string
  -> ?findlib_deps:(host -> pkg list)
  -> ml_files:(host -> filename list)
  -> mli_files:(host -> filename list)
  -> style : [ `Pack of string ]
  -> dir:string
  -> string
  -> lib

(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
val build_lib : lib -> unit


(******************************************************************************)
(** {2 Low-level Functions} *)
(******************************************************************************)
val lib_path : host -> lib -> string
(** Return path of library file for given host. The result is provided
    without any suffix. Add whichever you need, such as .cma, .cmx, or
    .js. *)

val pack_path : host -> lib -> string
(** Return path to packed module for given lib without any suffix. Add
    whichever you need, such as .cmo or .cmx. Raise exception if given
    [lib]'s style is not [`Pack _]. *)

val module_path : host -> lib -> filename -> string
(** [module_path host lib src] returns the path to the compiled module
    for given [src] file of [lib]. Result doesn't include any
    suffix. Add whichever you need, such as .cmi, .cmo, or .cmx. *)

val intf_of_impl : string -> string
(** Map given file's extension from .ml to .mli, or .eliom to
    .eliomi. Raise exception if given extension is neither of
    these. *)

val intf_exists : host -> lib -> filename -> bool
(** [intf_exists lib impl] returns true if an interface file is
    defined for the given implementation file [impl]. *)

val is_shared_impl : lib -> filename -> bool
(** [is_shared_impl lib x] returns true if [x] is an implementation
    file for both the client and server sides of [lib]. *)
