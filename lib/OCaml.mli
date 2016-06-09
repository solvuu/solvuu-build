(** OCaml commands. *)
open Ocamlbuild_plugin

(** Arguments common to both ocamlc and ocamlopt. *)
type 'a ocaml_args =
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

type ocamlc = (
  ?compat_32:unit ->
  ?custom:unit ->
  ?dllib:string ->
  ?dllpath:string ->
  ?vmthread:unit ->
  Pathname.t list ->
  Command.t
) ocaml_args

type ocamlopt = (
  ?compact:unit ->
  ?inline:int ->
  ?nodynlink:unit ->
  ?p:unit ->
  ?_S:unit ->
  ?shared:unit ->
  Pathname.t list ->
  Command.t
) ocaml_args

(** Abstraction over ocamlc and ocamlopt to help construct commands
    uniformly over either compiler. *)
type ocaml = (
  [`Byte | `Native] ->
  Pathname.t list ->
  Command.t
) ocaml_args

type 'a ocamlfind_args =
  ?package:string list ->
  ?linkpkg:unit ->
  'a

val ocamlc   : ocamlc
val ocamlopt : ocamlopt
val ocaml : ocaml

val ocamlfind_ocamlc   : ocamlc ocamlfind_args
val ocamlfind_ocamlopt : ocamlopt ocamlfind_args
val ocamlfind_ocaml    : ocaml ocamlfind_args

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

module Menhir : sig
  val command :
    ?base:string ->
    Pathname.t ->
    Command.t

  (** Register a rule to run menhir. By default, [dep = "%.mly"]. *)
  val rule :
    ?base:string ->
    ?dep:string ->
    unit ->
    unit

end

module Ocamllex : sig
  val command : ?ml:unit -> ?q:unit -> ?o:string -> Pathname.t -> Command.t

  val rule : ?ml:unit -> ?q:unit -> ?dep:string -> ?prod:string -> unit -> unit
  (** Register a rule to run ocamllex. By default, [dep = "%.mll"] and
      [prod = "%.ml"]. *)

end
