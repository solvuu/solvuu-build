(* Implementation notes:

   The list of arguments in many functions of this module seem unruly,
   but they should be copied/pasted in almost all cases. Thus, the
   work to write and maintain the code is simpler than it might at
   first seem. Factoring the type definitions as done here is key to
   this maintainability.

   Many functions are implemented in two steps, first a function
   [foo_specs] is defined and then [foo]. This is to support factoring
   out code dependent on the long list of arguments that are needed in
   multiple places. For instance [ocamlc_args_specs] is needed to
   implement [ocamlc] and [ocamlfind_ocamlc].
*)
open Printf
open Ocamlbuild_plugin
open Util

(******************************************************************************)
(** {2 Low Level Functions} *)
(******************************************************************************)
let string ~delim (flag:string) (value:string option) =
  match value with
  | None -> [None]
  | Some value ->
    match delim with
    | `Space -> [Some (A flag); Some (A value)]
    | `None -> [Some (A (flag ^ value))]
    | `Equal -> [Some (A (sprintf "%s=%s" flag value))]

let string_list ~delim (flag:string) (value:string list option) =
  match value with
  | None -> [None]
  | Some l ->
    List.map l ~f:(fun x ->
      string ~delim flag (Some x)
    ) |>
    List.flatten

let unit (flag:string) (value:unit option) = match value with
  | None -> [None]
  | Some () -> [Some (A flag)]

let int (flag:string) (value:int option) = match value with
  | None -> [None]
  | Some value -> [Some (A flag); Some (A (string_of_int value))]

let specs_to_command (specs : spec option list list) : Command.t =
  List.flatten specs
  |> List.filter_map ~f:Fn.id
  |> fun l -> Cmd (S l)

let spec_of_command (x:Command.t) : Command.spec =
  match x with
  | Command.Cmd x -> x
  | Command.Seq _ -> failwith "cannot extract spec from sequence of commands"
  | Command.Echo _ -> failwith "cannot convert Command.Echo to spec"
  | Command.Nop -> failwith "cannot convert Command.Nop to spec"


(******************************************************************************)
(** {2 Abstraction over ocamlc/ocamlopt} *)
(******************************************************************************)
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
  ?help:unit ->
  'a

let ocaml_compiler_args_specs

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    ()
  : spec option list list =
  let string = string ~delim:`Space in
  let string_list = string_list ~delim:`Space in
  [
    unit "-a" a;
    unit "-absname" absname;
    unit "-annot" annot;
    unit "-bin-annot" bin_annot;
    unit "-c" c;
    string "-cc" cc;
    string "-cclib" cclib;
    string "-ccopt" ccopt;
    string "-color" (match color with
      | None -> None
      | Some `auto -> Some "auto"
      | Some `always -> Some "always"
      | Some `never -> Some "never"
    );
    unit "-config" config;
    string "-for-pack" for_pack;
    unit "-g" g;
    unit "-i" i;
    string_list "-I" pathI;
    string "-impl" impl;
    string "-intf" intf;
    string "-intf-suffix" intf_suffix;
    unit "-labels" labels;
    unit "-linkall" linkall;
    unit "-make-runtime" make_runtime;
    unit "-no-alias-deps" no_alias_deps;
    unit "-no_app_funct" no_app_funct;
    unit "-noassert" noassert;
    unit "-noautolink" noautolink;
    unit "-nolabels" nolabels;
    unit "-nostdlib" nostdlib;
    string "-o" o;
    string_list "-open" open_;
    unit "-output-obj" output_obj;
    unit "-pack" pack;
    string "-pp" pp;
    string "-ppx" ppx;
    unit "-principal" principal;
    unit "-rectypes" rectypes;
    string "-runtime-variant" runtime_variant;
    unit "-safe-string" safe_string;
    unit "-short-paths" short_paths;
    unit "-strict-sequence" strict_sequence;
    unit "-strict-formats" strict_formats;
    unit "-thread" thread;
    unit "-unsafe" unsafe;
    unit "-unsafe-string" unsafe_string;
    string "-use-runtime" use_runtime;
    unit "-v" v;
    unit "-verbose" verbose;
    unit "-version" version;
    string "-w" w;
    string "-warn-error" warn_error;
    unit "-warn-help" warn_help;
    unit "-where" where;
    unit "-help" help;
  ]

let ocaml_compiler

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    mode files
  =
  [[Some (A (match mode with `Byte -> "ocamlc" | `Native -> "ocamlopt"))]]
  @(ocaml_compiler_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?pathI
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help ()
  )@[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


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

let ocamlc_args_specs

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlc_args *)
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread

    ()

  : spec option list list
  =
  let string = string ~delim:`Space in
  (ocaml_compiler_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?pathI
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help ()
  )@[
    unit "-compat-32" compat_32;
    unit "-custom" custom;
    string "-dllib" dllib;
    string "-dllpath" dllpath;
    unit "-vmthread" vmthread;
  ]

let ocamlc

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlc_args *)
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread

    files
  =
  [[Some (A "ocamlc")]]
  @(ocamlc_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?pathI
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help
     ?compat_32 ?vmthread ()
  )
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamlopt} *)
(******************************************************************************)
type 'a ocamlopt_args = (
  ?compact:unit ->
  ?inline:int ->
  ?nodynlink:unit ->
  ?p:unit ->
  ?keep_assembly:unit ->
  ?shared:unit ->
  'a
) ocaml_compiler_args

let ocamlopt_args_specs

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlopt_args *)
    ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared

    ()
  : spec option list list
  =
  (ocaml_compiler_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?pathI
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help ()
  )@[
    unit "-compact" compact;
    int "-inline" inline;
    (match inline with
     | None -> [None]
     | Some x -> [Some (A "-inline"); Some (A (string_of_int x))]
    );
    unit "-nodynlink" nodynlink;
    unit "-p" p;
    unit "-S" keep_assembly;
    unit "-shared" shared;
  ]

let ocamlopt

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlopt_args *)
    ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared

    files
  =
  [[Some (A "ocamlopt")]]
  @(ocamlopt_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?pathI
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help
     ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared ()
  )@[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamlfind} *)
(******************************************************************************)
type 'a ocamlfind_args =
  ?package:string list ->
  ?linkpkg:unit ->
  'a

let ocamlfind_args_specs ?package ?linkpkg () : spec option list list =
  [
    (match package with
     | None -> [None]
     | Some x ->
       string ~delim:`Space "-package" (Some (String.concat ~sep:"," x))
    );
    unit "-linkpkg" linkpkg;
  ]

let ocamlfind_ocaml_compiler

    (* ocamlfind_args *)
    ?package ?linkpkg

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    mode files
  =
  [[
    Some (A "ocamlfind");
    Some (A (match mode with `Byte -> "ocamlc" | `Native -> "ocamlopt"));
  ]]
  @(
    ocaml_compiler_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help ()
  )
  @(ocamlfind_args_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command

let ocamlfind_ocamlc

    (* ocamlfind_args *)
    ?package ?linkpkg

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlc_args *)
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread

    files
  =
  [[Some (A "ocamlfind"); Some (A "ocamlc")]]
  @(ocamlc_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compat_32 ?custom ?dllib ?dllpath ?vmthread ()
   )
  @(ocamlfind_args_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command

let ocamlfind_ocamlopt

    (* ocamlfind_args *)
    ?package ?linkpkg

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlopt_args *)
    ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared

    files
  =
  [[Some (A "ocamlfind"); Some (A "ocamlopt")]]
  @(ocamlopt_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared ()
   )
  @(ocamlfind_args_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamlmklib} *)
(******************************************************************************)
let ocamlmklib
    ?cclib ?ccopt ?custom ?g ?dllpath ?framework ?pathI
    ?failsafe ?ldopt ?linkall ?l ?pathL
    ?ocamlc ?ocamlcflags ?ocamlopt ?ocamloptflags
    ?o ?oc ?verbose
    files
  =
  let string = string ~delim:`Space in
  [
    [Some (A "ocamlmklib")];
    string "-cclib" cclib;
    string "-ccopt" ccopt;
    unit "-custom" custom;
    unit "-g" g;
    string "-dllpath" dllpath;
    string "-framework" framework;
    string_list ~delim:`Space "-I" pathI;
    unit "-failsafe" failsafe;
    string "-ldopt" ldopt;
    unit "-linkall" linkall;
    string "-l" l;
    string_list ~delim:`None "-L" pathL;
    string "-ocamlc" ocamlc;
    string "-ocamlcflags" ocamlcflags;
    string "-ocamlopt" ocamlopt;
    string "-ocamloptflags" ocamloptflags;
    string "-o" o;
    string "-oc" oc;
    unit "-verbose" verbose;
    (List.map files ~f:(fun file -> Some (A file)));
  ]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamldep} *)
(******************************************************************************)
type 'a ocamldep_args =
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
  ?sort:unit ->
  ?version:unit ->
  'a

let ocamldep_args_specs

    (* ocamldep_args *)
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line ?open_ ?pp ?ppx ?slash
    ?sort ?version

    ()
  : spec option list list
  =
  let string = string ~delim:`Space in
  let string_list = string_list ~delim:`Space in
  [
    unit "-absname" absname;
    unit "-all" all;
    string_list "-I" pathI;
    string_list "-impl" impl;
    string_list "-intf" intf;
    string "-ml-synonym" ml_synonym;
    string "-mli-synonym" mli_synonym;
    unit "-modules" modules;
    unit "-native" native;
    unit "-one-line" one_line;
    string_list "-open" open_;
    string "-pp" pp;
    string "-ppx" ppx;
    unit "-slash" slash;
    unit "-sort" sort;
    unit "-version" version;
  ]

let ocamldep

    (* ocamldep_args *)
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line ?open_ ?pp ?ppx ?slash
    ?sort ?version

    files
  =
  [[Some (A "ocamldep")]]
  @(ocamldep_args_specs
      ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
      ?modules ?native ?one_line ?open_ ?pp ?ppx ?slash
      ?sort ?version ()
   )
  @[List.map files ~f:(fun x -> Some (A x))]
  |> specs_to_command

let run_ocamldep

    (* ocamldep_args *)
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line:_ ?open_ ?pp ?ppx ?slash
    ?sort ?version

    files
  =
  let one_line = Some () in
  let cmd =
    ocamldep
      ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
      ?modules ?native ?one_line ?open_ ?pp ?ppx ?slash
      ?sort ?version
      files
    |> spec_of_command
    |> Command.string_of_command_spec
  in
  Ocamlbuild_pack.My_unix.run_and_read cmd |>
  String.split ~on:'\n' |>
  List.filter ~f:(function "" -> false | _ -> true) |>
  List.map ~f:(fun line ->
    String.split line ~on:':' |> function
    | target::deps::[] ->
      let target = String.trim target in
      let deps =
        String.split deps ~on:' ' |>
        List.map ~f:String.trim |>
        List.filter ~f:(function "" -> false | _ -> true) |>
        List.sort_uniq ~cmp:String.compare
      in
      target,deps
    | _ -> failwithf "unexpected output from %s, invalid line \"%s\""
             cmd line ()
  ) |>
  List.filter ~f:(function "",[] -> false  | _ -> true) |>
  List.map ~f:(function x,[""] -> x,[] | x,y -> x,y)

let run_ocamldep1


    (* ocamldep_args *)
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line:_ ?open_ ?pp ?ppx ?slash
    ?sort ?version

    file
  =
  assert (Sys.file_exists file);
  let one_line = Some () in

  run_ocamldep
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line ?open_ ?pp ?ppx ?slash
    ?sort ?version
    [file]
  |> function
  | [] -> failwithf "ocamldep returned no output for existing file %s"
            file ()
  | (x,deps)::[] ->
    if x = file then deps
    else failwithf "ocamldep returned output for unexpected file %s when \
                    called on %s" x file ()
  | _ -> failwithf "ocamldep returned multiple outputs for single file %s"
           file ()

let run_ocamldep_sort files =
  let cmd =
    ["ocamldep"; "-sort"]@files |>
    String.concat ~sep:" "
  in
  Ocamlbuild_pack.My_unix.run_and_read cmd |>
  String.split ~on:' ' |>
  List.filter ~f:(fun x -> not @@ String.for_all x ~f:Char.is_whitespace)


(******************************************************************************)
(** {2 ocamllex/menhir} *)
(******************************************************************************)
let ocamllex ?ml ?q ?o mll =
  let string = string ~delim:`Space in
  specs_to_command [
    [Some (A "ocamllex")];
    unit "-ml" ml;
    string "-o" o;
    unit "-q" q;
    [Some (A mll)];
  ]

let ocamllex_rule ?ml ?q ?(dep="%.mll") ?(prod="%.ml") () =
  Rule.rule ~deps:[dep] ~prods:[prod] (fun env _ ->
    ocamllex ?ml ?q ~o:(env prod) (env dep)
  )

let menhir ?base mly =
  let string = string ~delim:`Space in
  specs_to_command [
    [Some (A "menhir")];
    string "--base" base;
    [Some (A mly)];
  ]

let menhir_rule ?base ?(dep="%.mly") () =
  let prods =
    let base = match base with
      | None -> Filename.chop_extension dep
      | Some x -> x
    in
    [base ^ ".ml"; base ^ ".mli"]
  in
  Rule.rule ~deps:[dep] ~prods (fun env _ ->
    menhir ?base (env dep)
  )


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
  ?pathI:string list ->
  ?ofs:string ->
  ?linkall:unit ->
  ?no_cmis:unit ->
  ?toplevel:unit ->
  'a

let js_of_ocaml_args_specs

    (* js_of_ocaml_args *)
    ?custom_header ?debug ?debug_info ?disable ?enable
    ?no_inline ?no_runtime ?o ?opt ?pretty ?quiet ?set
    ?source_map_inline ?source_map_no_source
    ?source_map_root ?source_map
    ?version ?extern_fs ?file ?pathI ?ofs
    ?linkall ?no_cmis ?toplevel

    ()
  : spec option list list
  =
  let string = string ~delim:`Equal in
  [
    string "--custom-header" custom_header;
    unit "--debug" debug;
    unit "--debug-info" debug_info;
    string "--disable" disable;
    string "--enable" enable;
    unit "--no-inline" no_inline;
    unit "--no-runtime" no_runtime;
    string "-o" o;
    int "--opt" opt;
    unit "--pretty" pretty;
    unit "--quiet" quiet;
    (match set with
     | None -> [None]
     | Some l ->
       List.map l ~f:(fun (x,y) ->
         string "--set" (Some (sprintf "%s=%s" x y))
       ) |>
       List.flatten
    );
    unit "--source-map-inline" source_map_inline;
    unit "--source-map-no-source" source_map_no_source;
    string "--source-map-root" source_map_root;
    unit "--source-map" source_map;
    unit "--version" version;
    unit "--extern-fs" extern_fs;
    string_list ~delim:`Equal "--file" file;
    string_list ~delim:`Space "-I" pathI;
    string "--ofs" ofs;
    unit "--linkall" linkall;
    unit "--no-cmis" no_cmis;
    unit "--toplevel" toplevel;
  ]

let js_of_ocaml

    (* js_of_ocaml_args *)
    ?custom_header ?debug ?debug_info ?disable ?enable
    ?no_inline ?no_runtime ?o ?opt ?pretty ?quiet ?set
    ?source_map_inline ?source_map_no_source
    ?source_map_root ?source_map
    ?version ?extern_fs ?file ?pathI ?ofs
    ?linkall ?no_cmis ?toplevel

    js_files cma
  =
  [[Some (A "js_of_ocaml")]]
  @(js_of_ocaml_args_specs
     ?custom_header ?debug ?debug_info ?disable ?enable
     ?no_inline ?no_runtime ?o ?opt ?pretty ?quiet ?set
     ?source_map_inline ?source_map_no_source
     ?source_map_root ?source_map
     ?version ?extern_fs ?file ?pathI ?ofs
     ?linkall ?no_cmis ?toplevel ()
  )
  @[
    List.map (js_files@[cma]) ~f:(fun x -> Some (A x))
  ]
  |> specs_to_command

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
  ?ppx:unit ->
  'a

let eliom_args_specs

    (* eliom_args *)
    ?package ?no_autoload ?type_conv ?infer
    ?dir ?type_dir ?server_types_ext
    ?ppopt ?predicates ?ppx

    ()
  =
  let string = string ~delim:`Space in
  [
    (match package with
     | None -> [None]
     | Some l -> string "-package" (Some (String.concat ~sep:"," l))
    );
    unit "-no-autoload" no_autoload;
    unit "-type-conv" type_conv;
    unit "-infer" infer;
    string "-dir" dir;
    string "-type-dir" type_dir;
    string "-server-types-ext" server_types_ext;
    string "-ppopt" ppopt;
    string "-predicates" predicates;
    unit "-ppx" ppx;
  ]

let eliomc

    (* eliom_args *)
    ?package ?no_autoload ?type_conv ?infer
    ?dir ?type_dir ?server_types_ext
    ?ppopt ?predicates ?ppx:ppx_eliom

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlc_args *)
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread

    files
  =
  [[Some (A "eliomc")]]
  @(eliom_args_specs
     ?package ?no_autoload ?type_conv ?infer
     ?dir ?type_dir ?server_types_ext
     ?ppopt ?predicates ?ppx:ppx_eliom ()
  )
  @(ocamlc_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compat_32 ?custom ?dllib ?dllpath ?vmthread ()
   )
  @[List.map files ~f:(fun x -> Some (A x))]
  |> specs_to_command

let eliomopt

    (* eliom_args *)
    ?package ?no_autoload ?type_conv ?infer
    ?dir ?type_dir ?server_types_ext
    ?ppopt ?predicates ?ppx:ppx_eliom

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlopt_args *)
    ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared

    files
  =
  [[Some (A "eliomopt")]]
  @(eliom_args_specs
     ?package ?no_autoload ?type_conv ?infer
     ?dir ?type_dir ?server_types_ext
     ?ppopt ?predicates ?ppx:ppx_eliom ()
  )
  @(ocamlopt_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compact ?inline ?nodynlink ?p ?keep_assembly ?shared ()
   )
  @[List.map files ~f:(fun x -> Some (A x))]
  |> specs_to_command


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

let eliomdep_args_specs

    (* eliomdep_args *)
    ?dir ?type_dir ?eliom_inc ?package ?no_autoload ?type_conv
    ?ppopt ?predicates ?verbose ?ppx

    ()
  : spec option list list
  =
  let string = string ~delim:`Space in
  let string_list = string_list ~delim:`Space in
  [
    string "-dir" dir;
    string "-type-dir" type_dir;
    string_list "-eliom-inc" eliom_inc;
    (match package with
     | None -> [None]
     | Some x ->
       string "-package" (Some (String.concat ~sep:"," x))
    );
    unit "-no-autoload" no_autoload;
    unit "-type-conv" type_conv;
    string_list "-ppopt" ppopt;
    string "-predicates" predicates;
    unit "-verbose" verbose;
    unit "-ppx" ppx;
  ]

let eliomdep
    host

    (* ocamldep_args *)
    ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
    ?modules ?native ?one_line ?open_ ?pp ?ppx:ppx_ocamldep ?slash
    ?sort ?version

    (* eliomdep_args *)
    ?dir ?type_dir ?eliom_inc ?package ?no_autoload ?type_conv
    ?ppopt ?predicates ?verbose ?ppx:ppx_eliomdep

    files
  =
  [[
    Some (A "eliomdep");
    (match host with
     | `Client -> Some (A "-client")
     | `Server -> Some (A "-server")
    );
  ]]
  @(eliomdep_args_specs
      ?dir ?type_dir ?eliom_inc ?package ?no_autoload ?type_conv
      ?ppopt ?predicates ?verbose ?ppx:ppx_eliomdep ()
   )
  @(ocamldep_args_specs
      ?absname ?all ?pathI ?impl ?intf ?ml_synonym ?mli_synonym
      ?modules ?native ?one_line ?open_ ?pp ?ppx:ppx_ocamldep ?slash
      ?sort ?version ()
   )
  @[List.map files ~f:(fun x -> Some (A x))]
  |> specs_to_command

let eliomdep_client = eliomdep `Client
let eliomdep_server = eliomdep `Server


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

let js_of_eliom_args_specs

    (* js_of_eliom_args *)
    ?package ?no_autoload ?type_conv
    ?dir ?type_dir ?server_types_ext
    ?jsopt ?ppopt ?predicates ?ppx
    ?dont_force_linkall

    ()
  =
  let string = string ~delim:`Space in
  [
    (match package with
     | None -> [None]
     | Some l -> string "-package" (Some (String.concat ~sep:"," l))
    );
    unit "-no-autoload" no_autoload;
    unit "-type-conv" type_conv;
    string "-dir" dir;
    string "-type-dir" type_dir;
    string "-server-types-ext" server_types_ext;
    string "-jsopt" jsopt;
    string "-ppopt" ppopt;
    string "-predicates" predicates;
    unit "-ppx" ppx;
    unit "-dont-force-linkall" dont_force_linkall;
  ]

let js_of_eliom

    (* js_of_eliom_args *)
    ?package ?no_autoload ?type_conv
    ?dir ?type_dir ?server_types_ext
    ?jsopt ?ppopt ?predicates ?ppx:ppx_eliom
    ?dont_force_linkall

    (* js_of_ocaml_args
       - Beware some are overwritten by ocamlc_args below. I guess it
         is a bug in the js_of_eliom command line API.
    *)
    ?custom_header ?debug ?debug_info ?disable ?enable
    ?no_inline ?no_runtime ?o:o_js_of_ocaml ?opt ?pretty ?quiet ?set
    ?source_map_inline ?source_map_no_source
    ?source_map_root ?source_map
    ?version:version_js_of_ocaml ?extern_fs ?file ?pathI:pathI_js_of_ocaml ?ofs
    ?linkall:linkall_js_of_ocaml ?no_cmis ?toplevel

    (* ocaml_compiler_args *)
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?pathI:pathI_ocaml
    ?impl ?intf ?intf_suffix ?labels ?linkall:linkall_ocaml ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o:o_ocaml ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version:version_ocaml
    ?w ?warn_error ?warn_help ?where ?help

    (* ocamlc_args *)
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread

    files
  =
  [[Some (A "js_of_eliom")]]
  @(js_of_eliom_args_specs
      ?package ?no_autoload ?type_conv
      ?dir ?type_dir ?server_types_ext
      ?jsopt ?ppopt ?predicates ?ppx:ppx_eliom
      ?dont_force_linkall ()
   )
  @(js_of_ocaml_args_specs
      ?custom_header ?debug ?debug_info ?disable ?enable
      ?no_inline ?no_runtime ?o:o_js_of_ocaml ?opt ?pretty ?quiet ?set
      ?source_map_inline ?source_map_no_source
      ?source_map_root ?source_map
      ?version:version_js_of_ocaml ?extern_fs ?file
      ?pathI:pathI_js_of_ocaml ?ofs
      ?linkall:linkall_js_of_ocaml ?no_cmis ?toplevel ()
   )
  @(ocamlc_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?pathI:pathI_ocaml
      ?impl ?intf ?intf_suffix ?labels ?linkall:linkall_ocaml ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o:o_ocaml ?open_ ?output_obj ?pack ?pp ?ppx:ppx_ocaml
      ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version:version_ocaml
      ?w ?warn_error ?warn_help ?where ?help
      ?compat_32 ?custom ?dllib ?dllpath ?vmthread ()
   )
  @[List.map files ~f:(fun x -> Some (A x))]
  |> specs_to_command
