open Printf
open Util
module Findlib = Solvuu_build_findlib
let (/) = Filename.concat

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
  pack_name : string;
  dir : string;
  ml_files : string list;
  mli_files : string list;
  pkg : Solvuu_build_findlib.pkg;

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
}

and item = Lib of lib | App of app

let lib
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
    ?(internal_deps=[]) ?(findlib_deps=[])
    ?ml_files ?mli_files ~pkg ~pack_name ~dir name
  =
  let open Filename in
  let files =
    try Sys.readdir dir |> Array.to_list
    with _ -> []
  in
  let ml_files =
    List.filter files ~f:(fun x -> check_suffix x ".ml") |> fun l ->
    (match ml_files with
     |None -> l | Some (`Add x) -> x@l | Some (`Replace x) -> x
    )
    |> List.sort_uniq String.compare
  in
  let mli_files =
    List.filter files ~f:(fun x -> check_suffix x ".mli") |> fun l ->
    (match mli_files with
     | None -> l | Some (`Add x) -> x@l | Some (`Replace x) -> x
    )
    |> List.sort_uniq String.compare
  in
  Lib {
    name; internal_deps; findlib_deps; pack_name;
    dir; ml_files; mli_files; pkg;
    annot; bin_annot; g; safe_string; short_paths; thread; w;
  }

let app
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
    ?(internal_deps=[]) ?(findlib_deps=[])
    ~file name
  =
  App {
    name; internal_deps; findlib_deps; file;
    annot; bin_annot; g; safe_string; short_paths; thread; w;
  }

let name = function Lib x -> x.name | App x -> x.name

let internal_deps = function
  | Lib x -> x.internal_deps | App x -> x.internal_deps

let findlib_deps = function
  | Lib x -> x.findlib_deps | App x -> x.findlib_deps

let internal_deps_all t =
  let count = ref 0 in
  let rec loop t =
    incr count;
    if !count > 10000 then
      failwith "max recursion count exceeded, likely cycle in internal_deps"
    else
      let direct = internal_deps t in
      let further =
        List.map direct ~f:loop
        |> List.flatten
      in
      List.sort_uniq compare (direct@further)
  in
  loop t

let findlib_deps_all t =
  let count = ref 0 in
  let rec loop t =
    incr count;
    if !count > 10000 then
      failwith "max recursion count exceeded, likely cycle in internal_deps"
    else
      let direct = findlib_deps t in
      let further =
        List.map (internal_deps t) ~f:loop
        |> List.flatten
      in
      List.sort_uniq String.compare (direct@further)
  in
  loop t

let is_lib = function Lib _ -> true | App _ -> false
let is_app = function App _ -> true | Lib _ -> false

let dep_opts_sat x optional_deps =
  let all_deps = findlib_deps_all x in
  List.for_all optional_deps ~f:(fun optional_dep ->
    not (List.mem optional_dep ~set:all_deps)
    || Findlib.installed optional_dep
  )

let path_of_lib ~suffix (x:lib) : string =
  sprintf "%s/%s%s" (Filename.dirname x.dir) x.pack_name suffix

let path_of_app ~suffix (x:app) : string =
  sprintf "%s/%s%s" (Filename.dirname x.file) x.name suffix


(******************************************************************************)
(** {2 List Operations} *)
(******************************************************************************)
let filter_libs t =
  List.filter_map t ~f:(function Lib x -> Some x | App _ -> None)

let filter_apps t =
  List.filter_map t ~f:(function App x -> Some x | Lib _ -> None)

let all_findlib_pkgs t =
  List.map t ~f:findlib_deps
  |> List.flatten
  |> List.sort_uniq String.compare

(******************************************************************************)
(** {2 Item Module} *)
(******************************************************************************)
module Item = struct
  type t = item

  type typ = [`Lib | `App]
  let typ = function Lib _ -> `Lib | App _ -> `App
  let typ_to_string = function `Lib -> "lib" | `App -> "app"

  let hash t = Hashtbl.hash (typ t, name t)

  let compare t u =
    match compare (typ t) (typ u) with
    | -1 -> -1
    | 1 -> 1
    | 0 -> String.compare (name t) (name u)
    | _ -> assert false

  let equal t u = compare t u = 0

end

(******************************************************************************)
(** {2 Graph Operations} *)
(******************************************************************************)
module Graph = struct
  include Graph.Persistent.Digraph.Concrete(Item)

  module Dfs = Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(Item)
  )

  module Topological = struct
    include Graph.Topological.Make(
        Graph.Persistent.Digraph.Concrete(Item)
      )

    let sort g = fold (fun x l -> x::l) g []
  end

  let of_list items =
    if not (List.is_uniq ~cmp:compare items) then
      failwith "multiple libs or apps have an identical name"
    else
      let g = List.fold_left items ~init:empty ~f:(fun g x ->
        match internal_deps x with
        | [] -> add_vertex g x
        | _ ->
          List.fold_left (internal_deps x) ~init:g ~f:(fun g y ->
            add_edge g x y
          )
      )
      in
      if Dfs.has_cycle g then
        failwith "internal dependencies form a cycle"
      else
        g

end


(******************************************************************************)
(** {2 Static Files} *)
(******************************************************************************)
type content = string list

let merlin_file items : string list =
  [
    ["B +threads"; "PKG solvuu_build"];

    (* libs *)
    List.map (filter_libs items) ~f:(fun x ->
      [
        sprintf "S %s" x.dir;
        sprintf "B _build/%s" x.dir;
        sprintf "B _build/%s" (Filename.dirname x.dir);
      ]
    ) |> List.concat;

    (* apps *)
    List.map (filter_apps items) ~f:(fun x ->
      [
        sprintf "S %s" (Filename.dirname x.file);
        sprintf "B _build/%s" (Filename.dirname x.file);
      ]
    ) |> List.concat;

    (* findlib packages *)
    (
      all_findlib_pkgs items
      |> List.map ~f:(sprintf "PKG %s")
    );
  ]
  |> List.concat
  |> List.sort_uniq String.compare

let meta_file ~version libs : Fl_metascanner.pkg_expr =
  let def ?(preds=[]) var value =
      {
        Fl_metascanner.def_var = var;
        def_preds = preds;
        def_flav = `BaseDef;
        def_value = value;
      }
  in
  let pkg_defs_of_lib (x:lib) =
    [
      def "directory"
        (Findlib.to_path x.pkg |> List.tl |> String.concat "/");
      def "version" version;
      def ~preds:[`Pred "byte"] "archive" (sprintf "%s.cma" x.name);
      def ~preds:[`Pred "native"] "archive" (sprintf "%s.cmxa" x.name);
      def "requires" (
        (findlib_deps_all (Lib x))
        @(
          internal_deps (Lib x)
          |> filter_libs
          |> List.map ~f:(fun x -> x.pkg)
        )
        |> String.concat " "
      );
      def "exists_if" (sprintf "%s.cma" x.name);
    ]
  in
  let pkgs = List.map libs ~f:(fun x -> x.pkg) in
  let graph = Findlib.Graph.of_list pkgs in
  let root = match Findlib.Graph.roots graph with
    | x::[] -> x
    | [] -> failwith "findlib packages have no root"
    | l -> failwithf "cannot create META file for findlib packages with \
                      multiple roots: %s" (String.concat "," l) ()
  in
  let pkg_defs (pkg:Findlib.pkg) =
    try pkg_defs_of_lib @@ List.find libs ~f:(fun x -> x.pkg = pkg)
    with _ -> []
  in
  let rec pkg_expr (pkg:Findlib.pkg) =
    {
      Fl_metascanner.pkg_defs = pkg_defs pkg;
      pkg_children =
        List.map (Findlib.Graph.succ graph pkg) ~f:(fun x ->
          (Findlib.to_path x |> List.last_exn),
          (pkg_expr x)
        )
    }
  in
  pkg_expr root

let install_file items : string list =
  let all_libs = filter_libs items in
  let all_apps = filter_apps items in
  let suffixes = [
    "a";"annot";"cma";"cmi";"cmo";"cmt";"cmti";"cmx";"cmxa";
    "cmxs";"dll";"o"]
  in
  (
    List.map all_libs ~f:(fun lib ->
      List.map suffixes ~f:(fun suffix ->
        sprintf "  \"?_build/%s/%s.%s\" { \"%s/%s.%s\" }"
          (Filename.dirname lib.dir) lib.name suffix
          (Findlib.to_path lib.pkg |> List.tl |> String.concat "/")
          lib.name suffix
      )
    )
    |> List.flatten
    |> fun l -> "  \"_build/META\""::l
                |> fun l -> ["lib: ["]@l@["]"]
  )
  @(
    let lines =
      List.map all_libs ~f:(fun lib ->
        sprintf "  \"?_build/%s/dll%s_stub.so\""
          (Filename.dirname lib.dir) lib.name
      )
    in
    "stublibs: [" :: lines @ ["]"]
  )
  @(
    List.map all_apps ~f:(fun app ->
      List.map ["byte"; "native"] ~f:(fun suffix ->
        sprintf "  \"?_build/%s.%s\" {\"%s\"}"
          (Filename.chop_extension app.file)
          suffix
          app.name
      )
    )
    |> List.flatten
    |> function
    | [] -> []
    | l -> ["bin: ["]@l@["]"]
  )

let ocamlinit_file items ~postfix =
  let graph = Graph.of_list items in
  [
    [
      "let () =";
      "  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")";
      "  with Not_found -> ()";
      ";;" ;
      "";
    ];
    [
      "#use \"topfind\";;";
      "#thread;;";
      "";
    ];
    (
      let pkgs = "solvuu_build"::(all_findlib_pkgs items) in
      [sprintf "#require \"%s\";;"(String.concat " " pkgs); ""]
    );
    [
      "(* Load each lib provided by this project. *)";
    ];
    (
      Graph.Topological.sort graph |>
      filter_libs |>
      List.map ~f:(fun x ->
        sprintf "#directory \"_build/%s\";;" (Filename.dirname x.dir)
      )
      |> List.sort_uniq String.compare
    );
    (
      Graph.Topological.sort graph |>
      filter_libs |>
      List.map ~f:(fun (x:lib) ->
        sprintf "#load \"%s.cma\";;" x.name
      )
    );
    [""];
    postfix;
  ]
  |> List.concat

let makefile_rules_file ~project_name items : string list =
  let all_libs = filter_libs items in
  let all_apps = filter_apps items in
  let native =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxa" (Filename.dirname x.dir) x.name);
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxs" (Filename.dirname x.dir) x.name);
      List.map all_apps ~f:(path_of_app ~suffix:".native");
    ]
    |> String.concat " "
    |> sprintf "native: %s"
  in
  let byte =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cma" (Filename.dirname x.dir) x.name);
      List.map all_apps ~f:(path_of_app ~suffix:".byte");
    ]
    |> String.concat " "
    |> sprintf "byte: %s"
  in
  let static = [
    "default: .merlin .ocamlinit byte";

    "%.cma %.cmxa %.cmxs %.native %.byte %.mlpack:";
    "\t$(OCAMLBUILD) $@";

    "META:";
    "\t$(OCAMLBUILD) $@";

    sprintf ".merlin %s.install .ocamlinit:" project_name;
    "\t$(OCAMLBUILD) $@ && ln -s _build/$@ $@";

    "clean:";
    "\t$(OCAMLBUILD) -clean";

    ".PHONY: default native byte clean";
  ]
  in
  static@[native ; byte]


(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
let build_lib (x:lib) =
  let open Filename in
  let annot = x.annot in
  let bin_annot = x.bin_annot in
  let g = x.g in
  let safe_string = x.safe_string in
  let short_paths = x.short_paths in
  let thread = x.thread in
  let w = x.w in
  let ocaml ?pack ?o ?a ?c ?_I ?package ?for_pack mode files =
    OCaml.ocamlfind_ocaml mode files
      ?pack ?o ?a ?c ?_I ?package ?for_pack
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
  in
  let package = findlib_deps_all (Lib x) in
  let ml_files = List.map x.ml_files ~f:(fun y -> x.dir/y) in
  let mli_files = List.map x.mli_files ~f:(fun y -> x.dir/y) in

  let _I =
    x.dir
    ::(
      filter_libs x.internal_deps |>
      List.map ~f:(fun x -> Filename.dirname x.dir)
    )
  in

  let file_base_of_module mod_name : string option =
    let ml_bases = List.map ml_files ~f:(fun x -> chop_suffix x ".ml") in
    let mli_bases = List.map mli_files ~f:(fun x -> chop_suffix x ".mli") in
    let bases = List.sort_uniq String.compare (ml_bases@mli_bases) in
    List.filter bases ~f:(fun x -> String.capitalize (basename x) = mod_name)
    |> function
    | [] -> None (* Module is presumably from an external library. *)
    | x::[] -> Some x
    | l -> failwithf "module %s defined by multiple files: %s"
             mod_name
             (List.map l ~f:(fun x -> x ^ ".ml[i]") |> String.concat ",")
             ()
  in

  (* .cmo*/.cmx* -> packed .cmo/.cmx *)
  List.iter [`Byte; `Native] ~f:(fun mode ->
    let suffix = match mode with `Byte -> ".cmo" | `Native -> ".cmx" in
    let prod = path_of_lib x ~suffix in
    Rule.rule ~deps:ml_files ~prods:[prod] (fun _ build ->
      let deps =
        OCaml.ocamldep_sort ml_files |>
        List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:suffix)
      in
      List.iter deps ~f:(fun x ->
        match build [[x]] with
        | [Ocamlbuild_plugin.Outcome.Good _] -> ()
        | [Ocamlbuild_plugin.Outcome.Bad exn] -> raise exn
        | _ -> assert false
      );
      ocaml mode ~pack:() ~o:prod deps
    )
  );


  (* packed .cmo/.cmx -> .cma/.cmxa *)
  List.iter [`Byte; `Native] ~f:(fun mode ->
    let dep = sprintf "%s/%s.%s" (dirname x.dir) x.pack_name
        (match mode with `Byte -> "cmo" | `Native -> "cmx")
    in
    let prod = sprintf "%s/%s.%s" (dirname x.dir) x.pack_name
        (match mode with `Byte -> "cma" | `Native -> "cmxa")
    in
    Rule.rule ~deps:[dep] ~prods:[prod]
      (fun _ _ -> ocaml mode ~a:() ~o:prod [dep])
  );

  (* .mli -> .cmi *)
  List.iter mli_files ~f:(fun mli ->
    let base = chop_suffix mli ".mli" in
    let cmi = sprintf "%s.cmi" base in
    Rule.rule ~deps:[mli] ~prods:[cmi]
      (fun _ build ->
         let _ =
           OCaml.ocamldep1 ~modules:() ~_I mli |>
           List.filter_map ~f:file_base_of_module |>
           List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
           build |>
           assert_all_outcomes
         in
         ocaml `Byte ~c:() ~_I ~package ~o:cmi [mli]
      )
  );

  (* .ml -> ... *)
  List.iter ml_files ~f:(fun ml ->
    let base = chop_suffix ml ".ml" in
    let mli = sprintf "%s.mli" base in
    let cmi = sprintf "%s.cmi" base in
    let mli_exists = List.mem ~set:mli_files mli in

    let c = () in
    let for_pack = String.capitalize x.pack_name in

    (* .ml -> .cmo/.cmx and .cmi if no corresponding .mli *)
    List.iter [`Byte; `Native] ~f:(fun mode ->
      let obj = sprintf "%s.%s" base
          (match mode with `Byte -> "cmo" | `Native -> "cmx")
      in
      let deps = if mli_exists then [ml;cmi] else [ml] in
      let prods = if mli_exists then [obj] else [obj;cmi] in
      Rule.rule ~deps ~prods
        (fun _ build ->
           let _ =
             OCaml.ocamldep1 ~modules:() ~_I ml |>
             List.filter_map ~f:file_base_of_module |>
             List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
             build |>
             assert_all_outcomes
           in
           ocaml mode ~c ~_I ~package ~for_pack ~o:obj [ml]
        )
    )
  )
;;

let build_app (x:app) =
  let annot = x.annot in
  let bin_annot = x.bin_annot in
  let g = x.g in
  let safe_string = x.safe_string in
  let short_paths = x.short_paths in
  let thread = x.thread in
  let w = x.w in
  let package = findlib_deps_all (App x) in
  let _I =
    internal_deps_all (App x) |>
    List.filter_map ~f:(function
      | Lib x -> Some (Filename.dirname x.dir)
      | App _ -> None )
  in
  List.iter [`Byte; `Native] ~f:(fun mode ->
    let ocaml ?o files = OCaml.ocamlfind_ocaml mode files
        ?o
        ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
        ~package ~_I ~linkpkg:()
    in
    let path_of_lib (x:lib) = match mode with
      | `Byte -> path_of_lib ~suffix:".cma" x
      | `Native -> path_of_lib ~suffix:".cmxa" x
    in
    let path_of_app (x:app) = match mode with
      | `Byte -> path_of_app ~suffix:".byte" x
      | `Native -> path_of_app ~suffix:".native" x
    in
    let files =
      (
        internal_deps_all (App x) |>
        Graph.of_list |>
        Graph.Topological.sort |>
        List.filter_map ~f:(function
          | Lib x -> Some (path_of_lib x)
          | App _ -> None
        )
      )@
      [x.file]
    in
    let deps =
      (
        List.map x.internal_deps ~f:(function
          | Lib x -> path_of_lib x
          | App x -> path_of_app x
        )
      )@
      [x.file]
    in
    let prod = path_of_app x in
    Rule.rule ~deps ~prods:[prod]
      (fun _ _ -> ocaml ~o:prod files)
  )
;;

let build_static_file path content =
  let open Ocamlbuild_plugin in
  let open Util in
  (* Workaround ocamlbuild bug: https://github.com/ocaml/ocamlbuild/issues/76. *)
  let path = match String.split ~on:'/' path with
    | "."::l -> String.concat "/" l
    | _ -> path
  in
  let content = List.map content ~f:(sprintf "%s\n") in
  rule path ~prod:path (fun _ _ ->
    Seq [
      Cmd (Sh (sprintf "mkdir -p %s" (Filename.dirname path)));
      Echo (content,path);
    ]
  )
;;

  (* let clib lib = *)
  (*   match Util.clib_file lib.Item.dir lib.Item.name with *)
  (*   | None -> () *)
  (*   | Some file -> *)
  (*     let cstub = sprintf "%s_stub" lib.Item.name in *)
  (*     let stub_tag = "use_"^cstub in *)
  (*     let headers = *)
  (*       Util.h_files_of_dir lib.Item.dir *)
  (*       |> List.map ~f:(fun x -> lib.Item.dir/x) *)
  (*     in *)
  (*     dep ["c" ; "compile"] headers ; *)
  (*     dep ["link";"ocaml";stub_tag] [ *)
  (*       sprintf "%s/lib%s.a" (Filename.dirname lib.Item.dir) cstub ; *)
  (*     ] ; *)
  (*     flag *)
  (*       ["link";"ocaml";"byte";stub_tag] *)
  (*       (S[A"-dllib";A("-l"^cstub);A"-cclib";A("-l"^cstub)]) ; *)
  (*     flag *)
  (*       ["link";"ocaml";"native";stub_tag] *)
  (*       (S[A"-cclib";A("-l"^cstub)]) ; *)
  (*     static_file *)
  (*       (sprintf "%s/lib%s.clib" (Filename.dirname lib.Item.dir) cstub) *)
  (*       file *)

(******************************************************************************)
(** {2 Plugins} *)
(******************************************************************************)
let basic1 ?(ocamlinit_postfix=[]) ~project_name ~version items =
  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  let merlin_file = merlin_file items in
  let meta_file = meta_file ~version libs in
  let install_file = install_file items in
  let ocamlinit_file = ocamlinit_file items ~postfix:ocamlinit_postfix in
  let makefile_rules_file = makefile_rules_file items ~project_name in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      Findlib.build_meta_file meta_file;
      build_static_file ".merlin" merlin_file;
      build_static_file (sprintf "%s.install" project_name) install_file;
      build_static_file ".ocamlinit" ocamlinit_file;
      build_static_file "Makefile.rules" makefile_rules_file;
    )
  | _ -> ()

let solvuu1 ?(ocamlinit_postfix=[]) ~project_name ~version items =
  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let annot = Some () in
  let bin_annot = Some () in
  let g = Some () in
  let safe_string = Some () in
  let short_paths = Some () in
  let thread = Some () in
  let w = Some "A-4-33-41-42-44-45-48" in

  let items = List.map items ~f:(function
    | Lib x ->
      Lib {x with annot; bin_annot; g; safe_string; short_paths; thread; w}
    | App x ->
      App {x with annot; bin_annot; g; safe_string; short_paths; thread; w}
  )
  in

  let libs = filter_libs items in
  let apps = filter_apps items in

  let git_commit = Git.last_commit() in
  let merlin_file = merlin_file items in
  let meta_file = meta_file ~version libs in
  let install_file = install_file items in
  let ocamlinit_file = ocamlinit_file items ~postfix:ocamlinit_postfix in
  let makefile_rules_file = makefile_rules_file items ~project_name in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      M4.m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match git_commit with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some version;
        ];

      Atdgen.atdgen_t_rule ~j_std:() ();
      Atdgen.atdgen_j_rule ~j_std:() ();

      OCaml.Menhir.rule ();
      OCaml.Ocamllex.rule ();

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;
      (* List.iter libs ~f:clib; *)

      Findlib.build_meta_file meta_file;
      build_static_file ".merlin" merlin_file;
      build_static_file (sprintf "%s.install" project_name) install_file;
      build_static_file ".ocamlinit" ocamlinit_file;
      build_static_file "Makefile.rules" makefile_rules_file;
    )
  | _ -> ()
