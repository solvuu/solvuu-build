(** OCaml commands. *)
open Ocamlbuild_plugin

(** Arguments common to both ocamlc and ocamlopt. *)
type 'a common_compiler_args =
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
  ?custom:unit ->
  ?dllib:string ->
  ?dllpath:string ->
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

type ocamlc = (
  ?compat_32:unit ->
  ?vmthread:unit ->
  Pathname.t list ->
  Command.t
) common_compiler_args

type ocamlopt = (
  ?compact:unit ->
  ?inline:int ->
  ?nodynlink:unit ->
  ?p:unit ->
  ?_S:unit ->
  ?shared:unit ->
  Pathname.t list ->
  Command.t
) common_compiler_args

type 'a ocamlfind_args =
  ?package:string list ->
  ?linkpkg:unit ->
  'a

val ocamlc   : ocamlc
val ocamlopt : ocamlopt

val ocamlfind_ocamlc   : ocamlc ocamlfind_args
val ocamlfind_ocamlopt : ocamlopt ocamlfind_args

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
