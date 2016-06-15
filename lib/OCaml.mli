(** OCaml commands. *)
open Ocamlbuild_plugin

(******************************************************************************)
(** {2 Abstraction over ocamlc/ocamlopt} *)
(******************************************************************************)
(** Arguments common to both ocamlc and ocamlopt. *)
type 'a ocaml_compiler_args =
  ?a:unit ->
  ?absname:unit ->
  ?annot:unit ->
  ?bin_annot:unit ->
  ?c:unit ->
  ?cc:string ->
  ?cclib:string ->
  ?ccopt:string ->
  ?color:[`auto | `always | `never] ->
  ?config:unit ->
  ?for_pack:string ->
  ?g:unit ->
  ?i:unit ->
  ?_I:string list ->
  ?impl:string ->
  ?intf:string ->
  ?intf_suffix:string ->
  ?labels:unit ->
  ?linkall:unit ->
  ?make_runtime:unit ->
  ?no_alias_deps:unit ->
  ?no_app_funct:unit ->
  ?noassert:unit ->
  ?noautolink:unit ->
  ?nolabels:unit ->
  ?nostdlib:unit ->
  ?o:string ->
  ?open_:string list ->
  ?output_obj:unit ->
  ?pack:unit ->
  ?pp:string ->
  ?ppx:string ->
  ?principal:unit ->
  ?rectypes:unit ->
  ?runtime_variant:string ->
  ?safe_string:unit ->
  ?short_paths:unit ->
  ?strict_sequence:unit ->
  ?strict_formats:unit ->
  ?thread:unit ->
  ?unsafe:unit ->
  ?unsafe_string:unit ->
  ?use_runtime:string ->
  ?v:unit ->
  ?verbose:unit ->
  ?version:unit ->
  ?w:string ->
  ?warn_error:string ->
  ?warn_help:unit ->
  ?where:unit ->
  ?help:unit ->
  'a

(** Abstraction over ocamlc and ocamlopt to help construct commands
    uniformly over either compiler. If you need options specific to
    either compiler, please see {!ocamlc} and {!ocamlopt} below. *)
val ocaml_compiler :
  ([`Byte | `Native] -> Pathname.t list -> Command.t) ocaml_compiler_args

(******************************************************************************)
(** {2 ocamlc} *)
(******************************************************************************)
type 'a ocamlc_args = (
  ?compat_32:unit ->
  ?custom:unit ->
  ?dllib:string ->
  ?dllpath:string ->
  ?vmthread:unit ->
  'a
) ocaml_compiler_args

val ocamlc : (Pathname.t list -> Command.t) ocamlc_args

(******************************************************************************)
(** {2 ocamlopt} *)
(******************************************************************************)
type 'a ocamlopt_args = (
  ?compact:unit ->
  ?inline:int ->
  ?nodynlink:unit ->
  ?p:unit ->
  ?_S:unit ->
  ?shared:unit ->
  'a
) ocaml_compiler_args

val ocamlopt : (Pathname.t list -> Command.t) ocamlopt_args

(******************************************************************************)
(** {2 ocamlfind} *)
(******************************************************************************)
type 'a ocamlfind_args =
  ?package:string list ->
  ?linkpkg:unit ->
  'a

val ocamlfind_ocaml_compiler :
  (
    [`Byte | `Native] -> Pathname.t list -> Command.t
  ) ocaml_compiler_args ocamlfind_args

val ocamlfind_ocamlc :
  (Pathname.t list -> Command.t) ocamlc_args ocamlfind_args

val ocamlfind_ocamlopt :
  (Pathname.t list -> Command.t) ocamlopt_args ocamlfind_args

(******************************************************************************)
(** {2 ocamlmklib} *)
(******************************************************************************)
val ocamlmklib :
  ?cclib:string ->
  ?ccopt:string ->
  ?custom:unit ->
  ?g:unit ->
  ?dllpath:string ->
  ?framework:string ->
  ?_I:string list ->
  ?failsafe:unit ->
  ?ldopt:string ->
  ?linkall:unit ->
  ?l:string ->
  ?_L:string list ->
  ?ocamlc:string ->
  ?ocamlcflags:string ->
  ?ocamlopt:string ->
  ?ocamloptflags:string ->
  ?o:string ->
  ?oc:string ->
  ?verbose:unit ->
  Pathname.t list ->
  Command.t


(******************************************************************************)
(** {2 ocamldep} *)
(******************************************************************************)

(** Return an association list mapping each given input file to its
    list of dependencies. Note ocamldep ignores files that don't
    exist. You may want to assert that the given files exist prior to
    calling this function. *)
val ocamldep
  :  ?modules:unit
  -> ?_I:string list
  -> Pathname.t list
  -> (string * string list) list

(** Similar to [ocamldep] but more convenient when you want the
    dependencies of a single file. We directly return the dependencies
    of the given file. In this case, we also raise an exception if the
    given file doesn't already exist since ocamldep can't compute
    anything in this case. *)
val ocamldep1
  :  ?modules:unit
  -> ?_I:string list
  -> Pathname.t
  -> string list

(** Sort given files in dependency order, i.e. later files depend on
    earlier ones. Note that ocamldep ignores files that don't exist,
    so there is no guarantee that the returned list contains all files
    in the input list. *)
val ocamldep_sort : Pathname.t list -> Pathname.t list

(******************************************************************************)
(** {2 ocamllex/menhir} *)
(******************************************************************************)
val ocamllex : ?ml:unit -> ?q:unit -> ?o:string -> Pathname.t -> Command.t

(** Register a rule to run ocamllex. By default, [dep = "%.mll"] and
    [prod = "%.ml"]. *)
val ocamllex_rule :
  ?ml:unit -> ?q:unit -> ?dep:string -> ?prod:string -> unit -> unit

val menhir : ?base:string -> Pathname.t -> Command.t

(** Register a rule to run menhir. By default, [dep = "%.mly"]. *)
val menhir_rule : ?base:string -> ?dep:string -> unit -> unit


(******************************************************************************)
(** {2 js_of_ocaml} *)
(******************************************************************************)
type 'a js_of_ocaml_args =
  ?custom_header:string ->
  ?debug:unit ->
  ?debug_info:unit ->
  ?disable:string ->
  ?enable:string ->
  ?no_inline:unit ->
  ?no_runtime:unit ->
  ?o:string ->
  ?opt:int ->
  ?pretty:unit ->
  ?quiet:unit ->
  ?set:(string * string) list ->
  ?source_map_inline:unit ->
  ?source_map_no_source:unit ->
  ?source_map_root:string ->
  ?source_map:unit ->
  ?version:unit ->
  ?extern_fs:unit ->
  ?file:string list ->
  ?_I:string list ->
  ?ofs:string ->
  ?linkall:unit ->
  ?no_cmis:unit ->
  ?toplevel:unit ->
  'a

val js_of_ocaml :
  (?js_files:Pathname.t list -> Pathname.t -> Command.t) js_of_ocaml_args


(******************************************************************************)
(** {2 eliomc/eliomopt} *)
(******************************************************************************)
type 'a eliom_args =
  ?package:string list ->
  ?no_autoload:unit ->
  ?type_conv:unit ->
  ?infer:unit ->
  ?dir:string ->
  ?type_dir:string ->
  ?server_types_ext:string ->
  ?ppopt:string ->
  ?predicates:string ->
  ?eliom_ppx:unit ->
  'a

val eliomc   : (Pathname.t list -> Command.t) ocamlc_args   eliom_args
val eliomopt : (Pathname.t list -> Command.t) ocamlopt_args eliom_args
