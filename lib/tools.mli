(** Command line tools. We provided convenient constructors for
    command line calls as needed by ocamlbuild. Each function
    corresponds to a Unix command of the same (or similar) name with
    flags mapped to labeled arguments.

    Most functions return a value of type [Command.t], which is what
    you need to define ocamlbuild rules. Sometimes we provide a
    function to register a rule directly. For example, {!ocamllex}
    constructs a command and {!ocamllex_rule} registers a
    corresponding rule that takes care of defining the dependency and
    target for you. Finally, sometimes you want to run a tool right
    away, as opposed to registering it to be run later. We provide
    some convenience functions for this too, e.g. {!run_ocamldep}
    immediately runs ocamldep, captures its output, and returns the
    parsed result.

    Command line flags are mapped to labeled arguments with the exact
    same name whenever possible, e.g. ocamlc's [-c] flag is represented
    by a [~c] argument to the {!ocamlc} function provided
    here. Sometimes this is not possible and we resolve the mapping as
    follows:

    - The flag is an OCaml keyword, in which case we suffix with an
      underscore. For example, ocamlc takes an [-open] flag, which is
      mapped to an [~open_] argument here.

    - The flag begins with a capital letter, in which case we choose
      an alternate name that represents the meaning of the flag. A
      commonly occuring case of this is the [-I] flag, which we map to
      [~pathI]. Other cases are documented where they occur.

    Command line tools sometimes allow a flag to be passed multiple
    times. We represent this by making the type of the corresponding
    argument a list. For example, ocamlc's [-open] argument takes a
    string value, and this can be given any number of times. Thus the
    [~open_] argument is of type [string list].

    The majority of the functions provided here correspond to OCaml
    tools, but see the very end for other common Unix tools, e.g. [cp]
    and [git].
*)
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
  ?pathI:string list ->
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
  ?nopervasives:unit ->
  ?dsource:unit ->
  ?dparsetree:unit ->
  ?dtypedtree:unit ->
  ?drawlambda:unit ->
  ?dlambda:unit ->
  ?dinstr:unit ->
  ?help:unit ->
  'a

(** Abstraction over ocamlc and ocamlopt to help construct commands
    uniformly over either compiler. If you need options specific to
    either compiler, please see {!ocamlc} and {!ocamlopt} below. *)
val ocaml_compiler :
  ([`Byte | `Native] -> Pathname.t list -> Command.t) ocaml_compiler_args

(** Extra arguments common to ocamlfind ocamlc/ocamlopt. *)
type 'a ocamlfind_ocaml_compiler_args =
  ?package:string list ->
  ?linkpkg:unit ->
  ?predicates:string ->
  ?dontlink:string list ->
  ?ppopt:string ->
  ?ppxopt:(string * string) list ->
  ?dllpath_pkg:string list ->
  ?dllpath_all:unit ->
  ?ignore_error:unit ->
  ?passopt:string list ->
  ?only_show:unit ->
  'a

(** Abstraction over [ocamlfind ocamlc/ocamlopt]. *)
val ocamlfind_ocaml_compiler :
  (
    [`Byte | `Native] -> Pathname.t list -> Command.t
  ) ocaml_compiler_args ocamlfind_ocaml_compiler_args


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

val ocamlfind_ocamlc :
  (Pathname.t list -> Command.t) ocamlc_args ocamlfind_ocaml_compiler_args


(******************************************************************************)
(** {2 ocamlopt} *)
(******************************************************************************)

(** The [~keep_assembly] argument corresponds to ocamlopt's [-S]
    argument. *)
type 'a ocamlopt_args = (
  ?compact:unit ->
  ?inline:int ->
  ?nodynlink:unit ->
  ?p:unit ->
  ?keep_assembly:unit ->
  ?shared:unit ->
  'a
) ocaml_compiler_args

val ocamlopt : (Pathname.t list -> Command.t) ocamlopt_args

val ocamlfind_ocamlopt :
  (Pathname.t list -> Command.t) ocamlopt_args ocamlfind_ocaml_compiler_args


(******************************************************************************)
(** {2 ocamlmklib} *)
(******************************************************************************)

(** The [~pathL] labeled argument corresponds to ocamlmklib's [-L]
    flag. *)
val ocamlmklib :
  ?cclib:string ->
  ?ccopt:string ->
  ?custom:unit ->
  ?g:unit ->
  ?dllpath:string ->
  ?framework:string ->
  ?pathI:string list ->
  ?failsafe:unit ->
  ?ldopt:string ->
  ?linkall:unit ->
  ?l:string ->
  ?pathL:string list ->
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
type 'a ocamldep_args0 =
  ?absname:unit ->
  ?all:unit ->
  ?pathI:string list ->
  ?impl:string list ->
  ?intf:string list ->
  ?ml_synonym:string ->
  ?mli_synonym:string ->
  ?modules:unit ->
  ?native:unit ->
  ?one_line:unit ->
  ?open_:string list ->
  ?pp:string ->
  ?ppx:string ->
  ?slash:unit ->
  'a

type 'a ocamldep_args =
  (
    ?sort:unit ->
    ?version:unit ->
    'a
  ) ocamldep_args0

val ocamldep : (Pathname.t list -> Command.t) ocamldep_args

(** Return an association list mapping each given input file to its
    list of dependencies. Note ocamldep ignores files that don't
    exist. You may want to assert that the given files exist prior to
    calling this function.

    To simplify parsing the output, the [?one_line] argument is
    ignored and interally always set.
*)
val run_ocamldep
  : (Pathname.t list -> (string * string list) list) ocamldep_args

(** Similar to [run_ocamldep] but more convenient when you want the
    dependencies of a single file. We directly return the dependencies
    of the given file. In this case, we also raise an exception if the
    given file doesn't already exist since ocamldep can't compute
    anything in this case. *)
val run_ocamldep1
  : (Pathname.t -> string list) ocamldep_args

(** Sort given files in dependency order, i.e. later files depend on
    earlier ones. Note that ocamldep ignores files that don't exist,
    so there is no guarantee that the returned list contains all files
    in the input list. *)
val run_ocamldep_sort : (Pathname.t list -> Pathname.t list) ocamldep_args0

type 'a ocamlfind_ocamldep_args =
  ?package:string list ->
  ?predicates:string ->
  ?native_filter:unit ->
  ?bytecode_filter:unit ->
  ?only_show:unit ->
  ?verbose:unit ->
  'a

val ocamlfind_ocamldep
  : (Pathname.t list -> Command.t) ocamldep_args ocamlfind_ocamldep_args

(** In the following [run_ocamlfind_ocamldep*] functions, the
    [~verbose] option is ignored and internally not set. Otherwise the
    output contains text that can't be parsed. *)
val run_ocamlfind_ocamldep :
  (
    Pathname.t list -> (string * string list) list
  ) ocamldep_args ocamlfind_ocamldep_args

val run_ocamlfind_ocamldep1
  : (Pathname.t -> string list) ocamldep_args ocamlfind_ocamldep_args

val run_ocamlfind_ocamldep_sort :
  (Pathname.t list -> Pathname.t list) ocamldep_args0 ocamlfind_ocamldep_args


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
  ?debug:string ->
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
  ?pathI:string list ->
  ?ofs:string ->
  ?linkall:unit ->
  ?no_cmis:unit ->
  ?toplevel:unit ->
  'a

val js_of_ocaml :
  (Pathname.t list -> Pathname.t -> Command.t) js_of_ocaml_args


(******************************************************************************)
(** {2 eliomc/eliomopt} *)
(******************************************************************************)

(** Arguments specific to eliomc/eliomopt. Note that -ppx is
    duplicated here and in ocamlc/ocamlopt. Thus, you can pass [~ppx]
    twice to {!eliomc} and {!eliomopt}. This mimics the (flawed)
    command line {{:https://github.com/ocsigen/eliom/issues/273}API of
    eliomc/eliomopt}. *)
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
  ?ppx:unit ->
  'a

val eliomc   : (Pathname.t list -> Command.t) ocamlc_args   eliom_args
val eliomopt : (Pathname.t list -> Command.t) ocamlopt_args eliom_args


(******************************************************************************)
(** {2 eliomdep} *)
(******************************************************************************)
type 'a eliomdep_args =
  ?dir:string ->
  ?type_dir:string ->
  ?eliom_inc:string list ->
  ?package:string list ->
  ?no_autoload:unit ->
  ?type_conv:unit ->
  ?ppopt:string list ->
  ?predicates:string ->
  ?verbose:unit ->
  ?ppx:unit ->
  'a

val eliomdep :
  (
    [`Client | `Server] -> Pathname.t list -> Command.t
  ) ocamldep_args eliomdep_args

(** If [~fix320] is set, modify output to workaround [eliomdep] issue
    {:{https://github.com/ocsigen/eliom/issues/320}320}. *)
val run_eliomdep :
  (
    ?fix320:unit ->
    [`Client | `Server] ->
    Pathname.t list ->
    (string * string list) list
  ) ocamldep_args eliomdep_args

val run_eliomdep_sort :
  (
    [`Client | `Server] -> Pathname.t list -> Pathname.t list
  ) ocamldep_args0 eliomdep_args


(******************************************************************************)
(** {2 js_of_eliom} *)
(******************************************************************************)
type 'a js_of_eliom_args =
  ?package:string list ->
  ?no_autoload:unit ->
  ?type_conv:unit ->
  ?dir:string ->
  ?type_dir:string ->
  ?server_types_ext:string ->
  ?jsopt:string ->
  ?ppopt:string ->
  ?predicates:string ->
  ?ppx:unit ->
  ?dont_force_linkall:unit ->
  'a

val js_of_eliom :
  (Pathname.t list -> Command.t) ocamlc_args js_of_ocaml_args js_of_eliom_args


(******************************************************************************)
(** {2 atdgen} *)
(******************************************************************************)
val atdgen : ?j:unit -> ?j_std:unit -> ?t:unit -> Pathname.t -> Command.t

(** Register a rule to run [atdgen -t] on the given [dep], which must
    have a ".atd" suffix. Default [dep] is "%.atd". Files produced
    will be "%_t.ml" and "%_t.mli". *)
val atdgen_t_rule : ?dep:string -> ?j_std:unit -> unit -> unit

(** Register a rule to run [atdgen -j] on the given [dep], which must
    have a ".atd" suffix. Default [dep] is "%.atd". Files produced
    will be "%_j.ml" and "%_j.mli". *)
val atdgen_j_rule : ?dep:string -> ?j_std:unit -> unit -> unit


(******************************************************************************)
(** Other Unix tools. *)
(******************************************************************************)
val cp : ?f:unit -> Pathname.t -> Pathname.t -> Command.t
val cp_rule : ?f:unit -> dep:Pathname.t -> prod:Pathname.t -> unit

(** Return most recent git commit ID if possible. Assumes there is a
    .git directory in the current working directory. *)
val git_last_commit : unit -> string option

val m4 :
  ?_D:(string * string option) list ->
  infile:Pathname.t ->
  outfile:Pathname.t ->
  Command.t

(** Defaults: [prod = "%.ml"] and [dep = prod ^ ".m4"]. *)
val m4_rule :
  ?_D:(string * string option) list ->
  ?dep:string ->
  ?prod:string ->
  unit ->
  unit
