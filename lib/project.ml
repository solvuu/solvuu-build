open Printf
module Findlib = Solvuu_build_findlib
open Tools
open Util
open Util.Filename
let (/) = Util.Filename.concat

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
  style : [ `Basic | `Pack of string];
  dir : string;
  ml_files : string list;
  mli_files : string list;
  c_files : string list;
  pkg : Solvuu_build_findlib.pkg;

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
  linkall : unit option;
  shared : unit option;
}

and item = Lib of lib | App of app

let lib
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w ?linkall
    ?shared
    ?(internal_deps=[]) ?(findlib_deps=[])
    ?ml_files ?mli_files ?c_files ~pkg ~style ~dir name
  =
  let all_files =
    try Sys.readdir dir |> Array.to_list
    with _ -> []
  in
  let select_files ?add_replace suffix =
    List.filter all_files ~f:(fun x -> check_suffix x suffix) |> fun l ->
    (match add_replace with
     | None -> l
     | Some (`Add x) -> x@l
     | Some (`Replace x) -> x
    ) |>
    List.sort_uniq ~cmp:String.compare
  in
  let ml_files = select_files ".ml" ?add_replace:ml_files in
  let mli_files = select_files ".mli" ?add_replace:mli_files in
  let c_files = select_files ".c" ?add_replace:c_files in
  Lib {
    name; internal_deps; findlib_deps; style;
    dir; ml_files; mli_files; c_files; pkg;
    annot; bin_annot; g; safe_string; short_paths; thread; w; linkall;
    shared;
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
      List.sort_uniq ~cmp:compare (direct@further)
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
      List.sort_uniq ~cmp:String.compare (direct@further)
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
  sprintf "%s/%s%s" (dirname x.dir) x.name suffix

let path_of_pack ~suffix (x:lib) : string =
  match x.style with
  | `Basic -> failwithf "path_of_pack undefined for un-packed lib %s" x.name ()
  | `Pack pack_name -> sprintf "%s/%s%s" (dirname x.dir) pack_name suffix

let path_of_app ~suffix (x:app) : string =
  sprintf "%s/%s%s" (dirname x.file) x.name suffix

let file_base_of_module x =
  let ml_bases =
    List.map x.ml_files ~f:(fun y -> x.dir/y) |>
    List.map ~f:(fun x -> chop_suffix x ".ml")
  in
  let mli_bases =
    List.map x.mli_files ~f:(fun y -> x.dir/y) |>
    List.map ~f:(fun x -> chop_suffix x ".mli")
  in
  let bases = List.sort_uniq ~cmp:String.compare (ml_bases@mli_bases) in
  fun mod_name ->
    List.filter bases ~f:(fun x -> String.capitalize (basename x) = mod_name)
    |> function
    | [] -> None (* Module is presumably from an external library. *)
    | x::[] -> Some x
    | l -> failwithf "module %s defined by multiple files: %s"
             mod_name
             (List.map l ~f:(fun x -> x ^ ".ml[i]") |> String.concat ~sep:",")
             ()

(* Must consider mli files too, not just ml files. Though it is a
   rarely used feature, technically OCaml does allow inferring an
   implementation from only a signature. *)
let module_paths ~style_matters lib =
  let module_paths_orig =
    (lib.ml_files@lib.mli_files) |>
    List.sort_uniq ~cmp:String.compare |>
    List.map ~f:(fun x -> lib.dir/(Filename.chop_extension x))
  in
  match style_matters with
  | false -> module_paths_orig
  | true -> match lib.style with
    | `Basic -> module_paths_orig
    | `Pack _ -> [path_of_pack ~suffix:"" lib]

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
  |> List.sort_uniq ~cmp:String.compare

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
    ["B +threads"; "PKG solvuu-build"];

    (* libs *)
    List.map (filter_libs items) ~f:(fun x ->
      [
        sprintf "S %s" x.dir;
        sprintf "B _build/%s" x.dir;
        sprintf "B _build/%s" (dirname x.dir);
      ]
    ) |> List.concat;

    (* apps *)
    List.map (filter_apps items) ~f:(fun x ->
      [
        sprintf "S %s" (dirname x.file);
        sprintf "B _build/%s" (dirname x.file);
      ]
    ) |> List.concat;

    (* findlib packages *)
    (
      all_findlib_pkgs items
      |> List.map ~f:(sprintf "PKG %s")
    );
  ]
  |> List.concat
  |> List.sort_uniq ~cmp:String.compare

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
        (Findlib.to_path x.pkg |> List.tl |> String.concat ~sep:"/");
      def "version" version;
      def ~preds:[`Pred "byte"] "archive"
        (sprintf "%s.cma" x.name);
      def ~preds:[`Pred "byte"; `Pred "plugin"] "archive"
        (sprintf "%s.cma" x.name);
      def ~preds:[`Pred "native"] "archive"
        (sprintf "%s.cmxa" x.name);
      def ~preds:[`Pred "native"; `Pred "plugin"] "archive"
        (sprintf "%s.cmxs" x.name);
      def "requires" (
        (findlib_deps_all (Lib x))
        @(
          internal_deps (Lib x)
          |> filter_libs
          |> List.map ~f:(fun x -> x.pkg)
        )
        |> String.concat ~sep:" "
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
                      multiple roots: %s" (String.concat ~sep:"," l) ()
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

  (* Return lines for given section, or [None] if [items] is empty. *)
  let section name (items : (string * string option) list)
    : string list option
    =
    match items with
    | [] -> None
    | _ ->
      let items =
        List.map items ~f:(fun (src,dst) ->
          let src = Filename.normalize src in
          match dst with
          | None -> sprintf "  \"%s\"" src
          | Some dst ->
            let dst = Filename.normalize dst in
            sprintf "  \"%s\" {\"%s\"}" src dst
        )
      in
      Some ((sprintf "%s: [" name)::items@["]"])
  in

  (* Directory to install most lib files in for given dir. *)
  let install_dir x =
    Findlib.to_path x.pkg |>
    List.tl |>
    List.fold_left ~init:"" ~f:Filename.concat
  in

  (* META file for lib section. *)
  let meta : (string * string option) list =
    ["_build/META",None]
  in

  (* C lib files for lib section. *)
  let clibs : (string * string option) list =
    filter_libs items |>
    List.filter_map ~f:(fun (x:lib) ->
      let install_dir = install_dir x in
      match x.c_files with
      | [] -> None
      | _ ->
        let file = sprintf "lib%s.a" x.name in
        let src = sprintf "?_build/%s/%s" (dirname x.dir) file in
        let dst = Some (install_dir/file) in
        Some (src,dst)
    )
  in

  (* OCaml lib files for lib section. *)
  let libs : (string * string option) list =
    filter_libs items |>
    List.map ~f:(fun (x:lib) ->
      let install_dir = install_dir x in
      [".a"; ".cma"; ".cmxa"; ".cmxs"; ".dll"; ".o"] |>
      List.map ~f:(fun suffix ->
        let src = "?_build"/(path_of_lib ~suffix x) in
        let base = basename src in
        let dst = Some (install_dir/base) in
        src,dst
      )
    ) |>
    List.flatten
  in

  (* Files related to modules for lib section. *)
  let module_files : (string * string option) list =
    filter_libs items |>
    List.map ~f:(fun (x:lib) ->
      let install_dir = install_dir x in
      let module_paths = module_paths ~style_matters:true x in
      let suffixes = [".annot";".cmi";".cmo";".cmt";".cmti";".cmx"] in
      List.map suffixes ~f:(fun suffix ->
        List.map module_paths ~f:(fun module_path ->
          let src = "?_build"/(module_path ^ suffix) in
          let base = basename src in
          let dst = Some (install_dir/base) in
          src,dst
        )
      ) |>
      List.flatten
    ) |>
    List.flatten
  in

  let lib =
    section "lib" (meta@clibs@libs@module_files)
  in

  let stublibs = section "stublibs" (
    filter_libs items |>
    List.filter_map ~f:(fun (x:lib) -> match x.c_files with
      | [] -> None
      | _ -> (
          let file = sprintf "dll%s.so" x.name in
          let src = sprintf "?_build/%s/%s" (dirname x.dir) file in
          let dst = Some file in
          Some (src,dst)
        )
    )
  )
  in

  let bin = section "bin" (
    filter_apps items |>
    List.map ~f:(fun (x:app) ->
      List.map [".byte"; ".native"] ~f:(fun suffix ->
        let src = "?_build"/(path_of_app x ~suffix) in
        let dst = Some x.name in
        src,dst
      )
    ) |>
    List.flatten
  )
  in

  [lib; stublibs; bin] |>
  List.filter_map ~f:Fn.id |>
  List.flatten


let ocamlinit_file ?(postfix=[]) items =
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
      let pkgs = "solvuu-build"::(all_findlib_pkgs items) in
      [sprintf "#require \"%s\";;"(String.concat ~sep:" " pkgs); ""]
    );
    [
      "(* Load each lib provided by this project. *)";
    ];
    (
      Graph.Topological.sort graph |>
      filter_libs |>
      List.map ~f:(fun x ->
        sprintf "#directory \"_build/%s\";;%s" (dirname x.dir)
          (match x.style with
           | `Pack _ -> ""
           | `Basic -> sprintf "\n#directory \"_build/%s\";;" x.dir)
      )
      |> List.sort_uniq ~cmp:String.compare
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

let makefile ~project_name items : string list =
  let all_libs = filter_libs items in
  let all_apps = filter_apps items in
  let native =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxa" (dirname x.dir) x.name);
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxs" (dirname x.dir) x.name);
      List.map all_apps ~f:(path_of_app ~suffix:".native");
    ]
    |> String.concat ~sep:" "
    |> sprintf "native: %s"
  in
  let byte =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cma" (dirname x.dir) x.name);
      List.map all_apps ~f:(path_of_app ~suffix:".byte");
    ]
    |> String.concat ~sep:" "
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

(* Compile all given ml files in dependency order. Return list of
   generated object files, also in dependency order. *)
let build_ml_files_sorted mode build ml_files =
  let suffix = match mode with `Byte -> ".cmo" | `Native -> ".cmx" in
  let obj_files =
    run_ocamldep_sort ml_files |>
    List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:suffix)
  in
  let () = List.iter obj_files ~f:(fun x ->
    match build [[x]] with
    | [Ocamlbuild_plugin.Outcome.Good _] -> ()
    | [Ocamlbuild_plugin.Outcome.Bad exn] -> raise exn
    | _ -> assert false
  )
  in
  obj_files

let build_lib (x:lib) =
  let annot = x.annot in
  let bin_annot = x.bin_annot in
  let g = x.g in
  let safe_string = x.safe_string in
  let short_paths = x.short_paths in
  let thread = x.thread in
  let w = x.w in
  let linkall = x.linkall in

  let ocamlc ?pack ?o ?a ?c ?pathI ?package ?for_pack ?custom files =
    ocamlfind_ocamlc files
      ?pack ?o ?a ?c ?pathI ?package ?for_pack ?custom
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w ?linkall
  in

  let ocamlopt ?pack ?o ?a ?shared ?c ?pathI ?package ?for_pack files =
    ocamlfind_ocamlopt files
      ?pack ?o ?a ?shared ?c ?pathI ?package ?for_pack
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w ?linkall
  in

  (* Abstraction of ocamlc/ocamlopt above. Use above if any options
     specific to just ocamlc or ocamlopt are required. This function is
     for when the flags are identical regardless of byte or native
     compilation. *)
  let ocaml ?pack ?o ?a ?c ?pathI ?package ?for_pack mode files =
    match mode with
    | `Byte ->
      ocamlc ?pack ?o ?a ?c ?pathI ?package ?for_pack files
    | `Native ->
      ocamlopt ?pack ?o ?a ?c ?pathI ?package ?for_pack files
  in

  let package = findlib_deps_all (Lib x) in
  let ml_files = List.map x.ml_files ~f:(fun y -> x.dir/y) in
  let mli_files = List.map x.mli_files ~f:(fun y -> x.dir/y) in
  let c_files = List.map x.c_files ~f:(fun y -> x.dir/y) in

  let pathI = List.sort_uniq ~cmp:String.compare @@
    x.dir
    ::(dirname x.dir)
    ::(
      filter_libs x.internal_deps |>
      List.map ~f:(fun x -> dirname x.dir)
    )
  in

  let file_base_of_module : string -> string option = file_base_of_module x in

  (* .cmo*/.cmx* -> packed .cmo/.cmx *)
  (match x.style with
  | `Basic          -> () (* Handle the dynamic cmo dependency when building the cma. *)
  | `Pack _ ->
    List.iter [`Byte; `Native] ~f:(fun mode ->
      let suffix = match mode with `Byte -> ".cmo" | `Native -> ".cmx" in
      let prod = path_of_pack x ~suffix in
      Rule.rule ~deps:ml_files ~prods:[prod] (fun _ build ->
        let deps = build_ml_files_sorted mode build ml_files in
        ocaml mode ~pack:() ~o:prod deps
      )
  ));

  ((* .cmo/.cmx,.o -> .cma/.cmxa/.cmxs *)
    let ml_packed_obj mode = path_of_pack x
        ~suffix:(match mode with `Byte -> ".cmo" | `Native -> ".cmx")
    in
    let ml_lib mode = path_of_lib x
        ~suffix:(match mode with `Byte -> ".cma" | `Native -> ".cmxa")
    in
    let plugin = path_of_lib x ~suffix:".cmxs" in
    match c_files with
    | [] -> ( (* No C files. Call ocamlc/ocamlopt directly. *)
        List.iter [`Byte; `Native] ~f:(fun mode ->
          let prod = ml_lib mode in
          match x.style with
          | `Pack _ -> (
              let deps = [ ml_packed_obj mode] in
              Rule.rule ~deps ~prods:[prod]
              (fun _ _ ->
                ocaml mode ~a:() ~o:prod deps)
              ;
              (match mode with
               | `Byte -> ()
               | `Native ->
                 Rule.rule ~deps ~prods:[plugin] (fun _ _ ->
                   ocamlopt ~shared:() ~o:plugin deps
                 )
              )
            )
          | `Basic          -> ( (* Ensure all cmo files exist *)
              Rule.rule ~deps:ml_files ~prods:[prod] (fun _ build ->
                let deps = build_ml_files_sorted mode build ml_files in
                ocaml mode ~a:() ~o:prod deps
              )
              ;
              (match mode with
               | `Byte -> ()
               | `Native ->
                 Rule.rule ~deps:ml_files ~prods:[plugin] (fun _ build ->
                   let deps = build_ml_files_sorted mode build ml_files in
                   ocamlopt ~shared:() ~o:plugin deps
                 )
              )
            )
        )
      )
    | _ -> ( (* There is C code. Call ocamlmklib. *)

        let clibs = [
          sprintf "%s/dll%s.so" (dirname x.dir) x.name;
          sprintf "%s/lib%s.a" (dirname x.dir) x.name;
        ]
        in

        List.iter [`Byte;`Native] ~f:(fun mode ->
          let deps =
            match x.style with
            | `Pack _ -> [ ml_packed_obj mode]
            | `Basic          ->
                let suffix = match mode with `Byte -> ".cmo" | `Native -> ".cmx" in
                (* Does order matter here? *)
                List.map ml_files ~f:(replace_suffix_exn ~old:".ml" ~new_:suffix)
          in
          let prod = ml_lib mode in
          let o =
            sprintf "%s/%s" (dirname x.dir) x.name |>
            Filename.normalize
          in
          Rule.rule ~deps:(deps@clibs) ~prods:[prod] (fun _ _ ->
            ocamlmklib ~verbose:() ~o deps
          )
        );

        ((* .c -> .o *)
          let deps = List.map c_files ~f:(fun c_file ->
            sprintf "%s.o" (chop_suffix c_file ".c") )
          in
          let o =
            sprintf "%s/%s" (dirname x.dir) x.name |>
            Filename.normalize
          in
          Rule.rule ~deps ~prods:clibs (fun _ _ ->
            ocamlmklib ~verbose:() ~o deps
          )
        )
      )
  );

  (* .mli -> .cmi *)
  List.iter mli_files ~f:(fun mli ->
    let base = chop_suffix mli ".mli" in
    let cmi = sprintf "%s.cmi" base in
    Rule.rule ~deps:[mli] ~prods:[cmi]
      (fun _ build ->
         let _ =
           run_ocamldep1 ~modules:() ~pathI mli |>
           List.filter_map ~f:file_base_of_module |>
           List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
           build |>
           assert_all_outcomes
         in
         ocaml `Byte ~c:() ~pathI ~package ~o:cmi [mli]
      )
  );

  (* .ml -> ... *)
  List.iter ml_files ~f:(fun ml ->
    let base = chop_suffix ml ".ml" in
    let mli = sprintf "%s.mli" base in
    let cmi = sprintf "%s.cmi" base in
    let mli_exists = List.mem ~set:mli_files mli in

    let c = () in

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
             run_ocamldep1 ~modules:() ~pathI ml |>
             List.filter_map ~f:file_base_of_module |>
             List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
             build |>
             assert_all_outcomes
           in
           match x.style with
           | `Basic ->
              ocaml mode ~c ~pathI ~package ~o:obj [ml]
           | `Pack pack_name ->
              let for_pack = String.capitalize pack_name in
              ocaml mode ~c ~pathI ~package ~for_pack ~o:obj [ml]
        )
    )
  );

  (* .c -> .o *)
  List.iter c_files ~f:(fun c_file ->
    let base = chop_suffix c_file ".c" in
    let obj = sprintf "%s.o" base in
    let c = () in
    Rule.rule ~deps:[c_file] ~prods:[obj] (fun _ _ ->
      Ocamlbuild_plugin.(Seq [
        ocamlc ~c ~pathI ~o:obj [c_file];

        (* OCaml compiler appears to ignore the -o option set
           above. Output file always goes into _build, so moving
           manually. *)
        Cmd (S [A"mv"; A"-f"; A(basename obj); A obj]);
      ])
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
  let pathI =
    internal_deps_all (App x) |>
    List.filter_map ~f:(function
      | Lib x -> Some (dirname x.dir)
      | App _ -> None
    ) |>
    List.sort_uniq ~cmp:String.compare
  in
  List.iter [`Byte; `Native] ~f:(fun mode ->
    let ocaml ?o files = ocamlfind_ocaml_compiler mode files
        ?o
        ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
        ~package ~pathI ~linkpkg:()
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
  let path = Filename.normalize path in
  let content = List.map content ~f:(sprintf "%s\n") in
  rule path ~prod:path (fun _ _ ->
    Seq [
      Cmd (Sh (sprintf "mkdir -p %s" (dirname path)));
      Echo (content,path);
    ]
  )
;;

(******************************************************************************)
(** {2 Plugins} *)
(******************************************************************************)
let basic1
    ?(additional_rules=[]) ?(ocamlinit_postfix=[])
    ~project_name ~version items
  =
  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter additional_rules ~f:(fun f -> f());

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      build_static_file ".merlin" (merlin_file items);
      build_static_file ".ocamlinit"
        (ocamlinit_file ~postfix:ocamlinit_postfix items);
      build_static_file "project.mk" (makefile ~project_name items);
      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
    )
  | _ -> ()

let solvuu1
    ?(additional_rules=[]) ?(ocamlinit_postfix=[])
    ~project_name ~version items
  =
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

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter additional_rules ~f:(fun f -> f());

      m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match git_last_commit() with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some version;
        ];

      atdgen_t_rule ~j_std:() ();
      atdgen_j_rule ~j_std:() ();

      menhir_rule ();
      ocamllex_rule ();

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      build_static_file ".merlin" (merlin_file items);
      build_static_file ".ocamlinit"
        (ocamlinit_file ~postfix:ocamlinit_postfix items);
      build_static_file "project.mk" (makefile ~project_name items);
      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
    )
  | _ -> ()
