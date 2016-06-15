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

let ocaml_compiler_args_specs compiler
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help ()
  : spec option list list =
  let string = string ~delim:`Space in
  let string_list = string_list ~delim:`Space in
  [
    [Some (A (match compiler with `Byte -> "ocamlc" | `Native -> "ocamlopt"))];
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
    string_list "-I" _I;
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
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    mode files
  =
  (ocaml_compiler_args_specs mode
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?_I
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
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread ()
  : spec option list list
  =
  let string = string ~delim:`Space in
  (ocaml_compiler_args_specs `Byte
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?_I
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
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread files
  =
  (ocamlc_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
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
  ?_S:unit ->
  ?shared:unit ->
  'a
) ocaml_compiler_args

let ocamlopt_args_specs
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared ()
  : spec option list list
  =
  (ocaml_compiler_args_specs `Native
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?_I
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
    unit "-S" _S;
    unit "-shared" shared;
  ]

let ocamlopt
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared files
  =
  (ocamlopt_args_specs
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?for_pack ?g ?i ?_I
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help
     ?compact ?inline ?nodynlink ?p ?_S ?shared ()
  )@[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamlfind} *)
(******************************************************************************)
type 'a ocamlfind_args =
  ?package:string list ->
  ?linkpkg:unit ->
  'a

let ocamlfind_specs ?package ?linkpkg () : spec option list list =
  [
    (match package with
     | None -> [None]
     | Some x ->
       string ~delim:`Space "-package" (Some (String.concat ~sep:"," x))
    );
    unit "-linkpkg" linkpkg;
  ]

let ocamlfind_ocaml_compiler
    ?package ?linkpkg
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    mode files
  =
  [[Some (A "ocamlfind")]]
  @(
    ocaml_compiler_args_specs mode
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?_I
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help ()
  )
  @(ocamlfind_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command

let ocamlfind_ocamlc
    ?package ?linkpkg
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?custom ?dllib ?dllpath ?vmthread files
  =
  [[Some (A "ocamlfind")]]
  @(ocamlc_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?_I
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compat_32 ?custom ?dllib ?dllpath ?vmthread ()
   )
  @(ocamlfind_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command

let ocamlfind_ocamlopt
    ?package ?linkpkg
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared files
  =
  [[Some (A "ocamlfind")]]
  @(ocamlopt_args_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?for_pack ?g ?i ?_I
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compact ?inline ?nodynlink ?p ?_S ?shared ()
   )
  @(ocamlfind_specs ?package ?linkpkg ())
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


(******************************************************************************)
(** {2 ocamlmklib} *)
(******************************************************************************)
let ocamlmklib
    ?cclib ?ccopt ?custom ?g ?dllpath ?framework ?_I
    ?failsafe ?ldopt ?linkall ?l ?_L
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
    string_list ~delim:`Space "-I" _I;
    unit "-failsafe" failsafe;
    string "-ldopt" ldopt;
    unit "-linkall" linkall;
    string "-l" l;
    string_list ~delim:`None "-L" _L;
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
let ocamldep ?modules ?(_I=[]) files =
  let cmd =
    [
      ["ocamldep"; "-one-line"];
      List.map _I ~f:(sprintf "-I %s");
      (match modules with None -> [] | Some () -> ["-modules"]);
    ] |>
    List.flatten |> fun l ->
    String.concat ~sep:" " (l@files)
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
        List.sort_uniq String.compare
      in
      target,deps
    | _ -> failwithf "unexpected output from %s, invalid line \"%s\""
             cmd line ()
  ) |>
  List.filter ~f:(function "",[] -> false  | _ -> true) |>
  List.map ~f:(function x,[""] -> x,[] | x,y -> x,y)

let ocamldep1 ?modules ?_I file =
  assert (Sys.file_exists file);
  ocamldep ?modules ?_I [file] |> function
  | [] -> failwithf "ocamldep returned no output for existing file %s"
            file ()
  | (x,deps)::[] ->
    if x = file then deps
    else failwithf "ocamldep returned output for unexpected file %s when \
                    called on %s" x file ()
  | _ -> failwithf "ocamldep returned multiple outputs for single file %s"
           file ()

let ocamldep_sort files =
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
