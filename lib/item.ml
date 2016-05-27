open Printf
open Util
module Findlib = Solvuu_build_findlib
let (/) = Filename.concat

type name = string

type condition = [
  | `Pkgs_installed
]

type pkg = string

type app = {
  name : name;
  internal_deps : t list;
  findlib_deps : pkg list;
  build_if : condition list;
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
  name : name;
  internal_deps : t list;
  findlib_deps : pkg list;
  build_if : condition list;
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

and t = Lib of lib | App of app

type typ = [`Lib | `App]

let lib
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
    ?(internal_deps=[]) ?(findlib_deps=[]) ?(build_if=[])
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
    name; internal_deps; findlib_deps; build_if; pack_name;
    dir; ml_files; mli_files; pkg;
    annot; bin_annot; g; safe_string; short_paths; thread; w;
  }

let app
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
    ?(internal_deps=[]) ?(findlib_deps=[]) ?(build_if=[])
    ~file name
  =
  App {
    name; internal_deps; findlib_deps; build_if; file;
    annot; bin_annot; g; safe_string; short_paths; thread; w;
  }

let typ = function Lib _ -> `Lib | App _ -> `App
let name = function Lib x -> x.name | App x -> x.name

let hash t = Hashtbl.hash (typ t, name t)

let compare t u =
  match compare (typ t) (typ u) with
  | -1 -> -1
  | 1 -> 1
  | 0 -> String.compare (name t) (name u)
  | _ -> assert false

let equal t u = compare t u = 0

let internal_deps = function
  | Lib x -> x.internal_deps | App x -> x.internal_deps

let findlib_deps = function
  | Lib x -> x.findlib_deps | App x -> x.findlib_deps

let build_if = function Lib x -> x.build_if | App x -> x.build_if

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

let typ_to_string = function `Lib -> "lib" | `App -> "app"

let rec should_build (i:t) =
  List.for_all (build_if i) ~f:(function
    | `Pkgs_installed ->
      List.for_all (findlib_deps i) ~f:Findlib.installed
  )
  &&
  List.for_all (internal_deps i) ~f:(fun x ->
    should_build x
  )


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
(** {2 Graph Operations} *)
(******************************************************************************)
module T = struct
  type nonrec t = t
  let compare = compare
  let hash = hash
  let equal = equal
end

module Graph = struct
  include Graph.Persistent.Digraph.Concrete(T)

  module Dfs = Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(T)
  )

  module Topological = struct
    include Graph.Topological.Make(
        Graph.Persistent.Digraph.Concrete(T)
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
(** {2 Rules} *)
(******************************************************************************)
let path_of_lib ~suffix (x:lib) : string =
  sprintf "%s/%s%s" (Filename.dirname x.dir) x.pack_name suffix

let path_of_app ~suffix (x:app) : string =
  sprintf "%s/%s%s" (Filename.dirname x.file) x.name suffix

let build_lib (x:lib) =
  let open Filename in
  let annot = x.annot in
  let bin_annot = x.bin_annot in
  let g = x.g in
  let safe_string = x.safe_string in
  let short_paths = x.short_paths in
  let thread = x.thread in
  let w = x.w in
  let ocamlc = OCaml.ocamlfind_ocamlc
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
  in
  let ocamlopt = OCaml.ocamlfind_ocamlopt
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

  ( (* .cmo* -> packed .cmo *)
    let prod = path_of_lib ~suffix:".cmo" x in
    Rule.rule ~deps:ml_files ~prods:[prod] (fun _ build ->
      OCaml.ocamldep_sort ml_files |>
      List.map ~f:(fun x -> [replace_suffix_exn ~old:".ml" ~new_:".cmo" x]) |>
      build |>
      List.map ~f:Ocamlbuild_plugin.Outcome.good |> fun deps ->
      ocamlc ~pack:() ~o:prod deps
    )
  );

  ( (* .cmx* -> packed .cmx *)
    let prod = path_of_lib ~suffix:".cmx" x in
    Rule.rule ~deps:ml_files ~prods:[prod] (fun _ build ->
      OCaml.ocamldep_sort ml_files |>
      List.map ~f:(fun x -> [replace_suffix_exn ~old:".ml" ~new_:".cmx" x]) |>
      build |>
      List.map ~f:Ocamlbuild_plugin.Outcome.good |> fun deps ->
      ocamlopt ~pack:() ~o:prod deps
    )
  );

  ( (* packed .cmo -> .cma *)
    let dep = sprintf "%s/%s.cmo" (dirname x.dir) x.pack_name in
    let prod = sprintf "%s/%s.cma" (dirname x.dir) x.pack_name in
    Rule.rule ~deps:[dep] ~prods:[prod]
      (fun _ _ -> ocamlc ~a:() ~o:prod [dep])
  );

  ( (* packed .cmx -> .cmxa *)
    let dep = sprintf "%s/%s.cmx" (dirname x.dir) x.pack_name in
    let prod = sprintf "%s/%s.cmxa" (dirname x.dir) x.pack_name in
    Rule.rule ~deps:[dep] ~prods:[prod]
      (fun _ _ -> ocamlopt ~a:() ~o:prod [dep])
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
         ocamlc ~c:() ~_I ~package ~o:cmi [mli]
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

    (* .ml -> .cmo and .cmi if no corresponding .mli *)
    let cmo = sprintf "%s.cmo" base in
    let deps = if mli_exists then [ml;cmi] else [ml] in
    let prods = if mli_exists then [cmo] else [cmo;cmi] in
    Rule.rule ~deps ~prods
      (fun _ build ->
         let _ =
           OCaml.ocamldep1 ~modules:() ~_I ml |>
           List.filter_map ~f:file_base_of_module |>
           List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
           build |>
           assert_all_outcomes
         in
         ocamlc ~c ~_I ~package ~for_pack ~o:cmo [ml]
      );

    (* .ml -> .cmx and .cmi if no corresponding .mli *)
    let cmx = sprintf "%s.cmx" base in
    let deps = if mli_exists then [ml;cmi] else [ml] in
    let prods = if mli_exists then [cmx] else [cmx;cmi] in
    Rule.rule ~deps ~prods
      (fun _ build ->
         let _ =
           OCaml.ocamldep1 ~modules:() ~_I ml |>
           List.filter_map ~f:file_base_of_module |>
           List.map ~f:(fun x -> [sprintf "%s.cmi" x]) |>
           build |>
           assert_all_outcomes
         in
         ocamlopt ~c ~_I ~package ~for_pack ~o:cmx [ml]
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
  let ocamlc = OCaml.ocamlfind_ocamlc
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
      ~package ~_I ~linkpkg:()
  in
  let ocamlopt = OCaml.ocamlfind_ocamlopt
      ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
      ~package ~_I ~linkpkg:()
  in
  let path_of_lib mode (x:lib) = match mode with
    | `byte -> path_of_lib ~suffix:".cma" x
    | `native -> path_of_lib ~suffix:".cmxa" x
  in
  let path_of_app mode (x:app) = match mode with
    | `byte -> path_of_app ~suffix:".byte" x
    | `native -> path_of_app ~suffix:".native" x
  in
  let deps mode =
    (
      List.map x.internal_deps ~f:(function
        | Lib x -> path_of_lib mode x
        | App x -> path_of_app mode  x
      )
    )@
    [x.file]
  in
  let files mode =
    (
      internal_deps_all (App x) |>
      Graph.of_list |>
      Graph.Topological.sort |>
      List.filter_map ~f:(function
        | Lib x -> Some (path_of_lib mode x)
        | App _ -> None
      )
    )@
    [x.file]
  in
  let prod mode = path_of_app mode x in
  (
    Rule.rule ~deps:(deps `byte) ~prods:[prod `byte]
      (fun _ _ -> ocamlc ~o:(prod `byte) (files `byte))
  );
  (
    Rule.rule ~deps:(deps `native) ~prods:[prod `native]
      (fun _ _ -> ocamlopt ~o:(prod `native) (files `native))
  )
