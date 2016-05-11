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

val ocamldep
  :  ?modules:unit
  -> ?_I:string list
  -> Pathname.t list
  -> (string * string list) list

val ocamldep_sort : Pathname.t list -> Pathname.t list
