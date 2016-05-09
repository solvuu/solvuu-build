(* Implementation notes:

   This list of arguments in many functions of this module seem
   unruly, but they should be copied/pasted in almost all cases. Thus,
   the work to write and maintain the code is simpler than it might at
   first seem. In the mli, factoring the type definitions is key to
   this maintainability.

   Many functions are implemented in two steps, first a function
   [foo_specs] is defined and then [foo]. This is to support factoring
   out code dependent on the long list of arguments that are needed in
   multiple places. For instance [ocamlc_specs] is needed to implement
   [ocamlc] and [ocamlfind_ocamlc].
*)
open Printf
open Ocamlbuild_plugin
open Util

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

let string (flag:string) (value:string option) = match value with
  | None -> [None]
  | Some value -> [Some (A flag); Some (A value)]

let string_list (flag:string) (value:string list option) = match value with
  | None -> [None]
  | Some l -> List.map l ~f:(fun x -> string flag (Some x)) |> List.flatten

let string_list_comma_sep (flag:string) (value:string list option) =
  match value with
  | None -> [None]
  | Some l -> (
      String.concat "," l |> fun x ->
      string flag (Some x)
    )

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

let ocaml_specs compiler
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help ()
  : spec option list list =
  [
    [Some (A (match compiler with `byte -> "ocamlc" | `native -> "ocamlopt"))];
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
    unit "-custom" custom;
    string "-dllib" dllib;
    string "-dllpath" dllpath;
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

let ocamlc_specs
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?vmthread files
  : spec option list list
  =
  (ocaml_specs `byte
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
     ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
     ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
     ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
     ?rectypes ?runtime_variant ?safe_string ?short_paths
     ?strict_sequence ?strict_formats ?thread ?unsafe
     ?unsafe_string ?use_runtime ?v ?verbose ?version
     ?w ?warn_error ?warn_help ?where ?help ()
  )@[
    unit "-compat-32" compat_32;
    unit "-vmthread" vmthread;
    (List.map files ~f:(fun file -> Some (A file)));
  ]

let ocamlc
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?vmthread files
  =
  ocamlc_specs
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?vmthread files
  |> specs_to_command

let ocamlopt_specs
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared files
  : spec option list list
  =
  (ocaml_specs `native
     ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
     ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
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
    (List.map files ~f:(fun file -> Some (A file)));
  ]

let ocamlopt
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared files
  =
  ocamlopt_specs
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compact ?inline ?nodynlink ?p ?_S ?shared files
  |> specs_to_command

let ocamlfind_specs
    ?package ?linkpkg
    ()
  : spec option list list
  =
  [
    string_list_comma_sep "-package" package;
    unit "-linkpkg" linkpkg;
  ]

let ocamlfind_ocamlc
    ?package ?linkpkg
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
    ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
    ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
    ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
    ?rectypes ?runtime_variant ?safe_string ?short_paths
    ?strict_sequence ?strict_formats ?thread ?unsafe
    ?unsafe_string ?use_runtime ?v ?verbose ?version
    ?w ?warn_error ?warn_help ?where ?help
    ?compat_32 ?vmthread files
  =
  [[Some (A "ocamlfind")]]
  @(ocamlc_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compat_32 ?vmthread files
   )
  @(ocamlfind_specs ?package ?linkpkg ())
  |> specs_to_command

let ocamlfind_ocamlopt
    ?package ?linkpkg
    ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
    ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
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
  @(ocamlopt_specs
      ?a ?absname ?annot ?bin_annot ?c ?cc ?cclib ?ccopt ?color
      ?config ?custom ?dllib ?dllpath ?for_pack ?g ?i ?_I
      ?impl ?intf ?intf_suffix ?labels ?linkall ?make_runtime
      ?no_alias_deps ?no_app_funct ?noassert ?noautolink ?nolabels
      ?nostdlib ?o ?open_ ?output_obj ?pack ?pp ?ppx ?principal
      ?rectypes ?runtime_variant ?safe_string ?short_paths
      ?strict_sequence ?strict_formats ?thread ?unsafe
      ?unsafe_string ?use_runtime ?v ?verbose ?version
      ?w ?warn_error ?warn_help ?where ?help
      ?compact ?inline ?nodynlink ?p ?_S ?shared files
   )
  @(ocamlfind_specs ?package ?linkpkg ())
  |> specs_to_command

let ocamldep ?modules ?(_I=[]) files =
  let cmd =
    [
      ["ocamldep"; "-one-line"];
      List.map _I ~f:(sprintf "-I %s");
      (match modules with None -> [] | Some () -> ["-modules"]);
    ] |>
    List.flatten |> fun l ->
    String.concat " " (l@files)
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
