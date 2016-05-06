open Printf
open Util
module Findlib = Solvuu_build_findlib

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
}

and lib = {
  name : name;
  internal_deps : t list;
  findlib_deps : pkg list;
  build_if : condition list;
  pack_name : string;
  dir : string;
  pkg : Solvuu_build_findlib.pkg;
}

and t = Lib of lib | App of app

type typ = [`Lib | `App]

let lib ?(internal_deps=[]) ?(findlib_deps=[]) ?(build_if=[])
    ~pkg ~pack_name ~dir name
  =
  Lib {name;internal_deps;findlib_deps;build_if;pack_name;dir;pkg}

let app ?(internal_deps=[]) ?(findlib_deps=[]) ?(build_if=[])
    ~file name
  =
  App {name;internal_deps;findlib_deps;build_if;file}

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
(** {2 Rules} *)
(******************************************************************************)
let rule ~deps ~prods cmd =
  let deps = List.map deps ~f:Filename.normalize in
  let prods = List.map prods ~f:Filename.normalize in
  let name = sprintf "%s -> %s"
      (String.concat "," deps) (String.concat "," prods)
  in
  Ocamlbuild_plugin.rule name ~deps ~prods (fun _ _ -> cmd)

let path_of_lib ~suffix (x:lib) : string =
  sprintf "%s/%s%s" (Filename.dirname x.dir) x.pack_name suffix

let path_of_app ~suffix (x:app) : string =
  sprintf "%s/%s%s" (Filename.dirname x.file) x.name suffix

let w = "A-4-33-41-42-44-45-48"

let ocamlc = OCaml.ocamlc
    ~thread:() ~bin_annot:() ~annot:()
    ~short_paths:() ~safe_string:() ~g:() ~w

let ocamlopt = OCaml.ocamlopt
    ~thread:() ~bin_annot:() ~annot:()
    ~short_paths:() ~safe_string:() ~g:() ~w

let build_lib ?git_commit ~project_version (x:lib) =
  let open Filename in

  let _I = [x.dir] in

  let files =
    Sys.readdir x.dir |> Array.to_list |>
    List.map ~f:(fun file -> concat x.dir file)
  in

  let ml_files =
    List.filter_map files ~f:(fun x ->
      if check_suffix x ".ml" then
        Some [x]
      else if check_suffix x ".atd" then
        let base = basename x in
        Some [sprintf "%s_j.ml" base; sprintf "%s_t.ml" base]
      else if check_suffix x ".ml.m4" then
        Some [chop_suffix x ".m4"]
      else
        None
    ) |> List.flatten
  in

  let mli_files =
    List.filter_map files ~f:(fun x ->
      if check_suffix x ".mli" then
        Some [x]
      else if check_suffix x ".atd" then
        let base = basename x in
        Some [sprintf "%s_j.mli" base; sprintf "%s_t.mli" base]
      else
        None
    ) |>
    List.flatten
  in

  let ml_m4_files =
    List.filter files ~f:(fun x -> check_suffix x ".ml.m4")
  in

  let atd_files =
    List.filter files ~f:(fun x -> check_suffix x ".atd")
  in

  let deps = OCaml.ocamldep ~_I files in
  let deps_of file =
    try List.assoc file deps
    with Not_found -> []
  in

  ( (* .cmo* -> packed .cmo *)
    let deps = List.map ml_files
        ~f:(replace_suffix_exn ~old:".ml" ~new_: ".cmo")
    in
    let prod = path_of_lib ~suffix:".cmo" x in
    rule ~deps ~prods:[prod] (ocamlc ~pack:() ~o:prod deps)
  );

  ( (* .cmx* -> packed .cmx *)
    let deps = List.map ml_files
        ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmx")
    in
    let prod = path_of_lib ~suffix:".cmx" x in
    rule ~deps ~prods:[prod] (ocamlopt ~pack:() ~o:prod deps)
  );

  ( (* packed .cmo -> .cma *)
    let dep = sprintf "%s/%s.cmo" (dirname x.dir) x.pack_name in
    let prod = sprintf "%s/%s.cma" (dirname x.dir) x.pack_name in
    rule ~deps:[dep] ~prods:[prod] (ocamlc ~a:() ~o:prod [dep])
  );

  ( (* packed .cmx -> .cmxa *)
    let dep = sprintf "%s/%s.cmx" (dirname x.dir) x.pack_name in
    let prod = sprintf "%s/%s.cmxa" (dirname x.dir) x.pack_name in
    rule ~deps:[dep] ~prods:[prod] (ocamlopt ~a:() ~o:prod [dep])
  );

  (* .mli -> .cmi *)
  List.iter mli_files ~f:(fun file ->
    let base = chop_suffix file ".mli" in
    let cmi = sprintf "%s.cmi" base in
    let deps = file::(deps_of cmi) in
    rule ~deps ~prods:[cmi] (ocamlc ~c:() ~_I ~o:cmi [file])
  );

  (* .ml -> ... *)
  List.iter ml_files ~f:(fun file ->
    let base = chop_suffix file ".ml" in
    let mli = sprintf "%s.mli" base in
    let cmi = sprintf "%s.cmi" base in
    let mli_exists = Sys.file_exists mli in

    let c = () in
    let for_pack = String.capitalize x.pack_name in

    (* .ml -> .cmo and .cmi if no corresponding .mli *)
    let cmo = sprintf "%s.cmo" base in
    let deps,prods =
      if mli_exists
      then file::(deps_of cmo), [cmo]
      else file::(deps_of cmo)@(deps_of cmi), [cmo;cmi]
    in
    rule ~deps ~prods (ocamlc ~c ~_I ~for_pack ~o:cmo [file]);

    (* .ml -> .cmx and .cmi if no corresponding .mli *)
    let cmx = sprintf "%s.cmx" base in
    let deps,prods =
      if mli_exists
      then file::(deps_of cmx), [cmx]
      else file::(deps_of cmx)@(deps_of cmi), [cmx;cmi]
    in
    rule ~deps ~prods (ocamlopt ~c ~_I ~for_pack ~o:cmx [file]);
  );

  (* .ml.m4 -> .ml *)
  List.iter ml_m4_files ~f:(fun file ->
    let base = chop_suffix file ".ml.m4" in
    let ml = sprintf "%s.ml" base in
    let git_commit = match git_commit with
      | None -> "None"
      | Some x -> sprintf "Some \"%s\"" x
    in
    let _D = [
      "GIT_COMMIT", Some git_commit;
      "VERSION", Some project_version;
    ]
    in
    rule ~deps:[file] ~prods:[ml] (M4.m4 ~_D ~infile:file ~outfile:ml)
  );

  (* .atd -> ... *)
  List.iter atd_files ~f:(fun file ->
    let base = chop_suffix file ".atd" in

    (* .atd -> _t.ml, _t.mli *)
    let prods = [sprintf "%s_t.ml" base; sprintf "%s_t.mli" base] in
    rule ~deps:[file] ~prods (Atdgen.atdgen ~t:() ~j_std:() file);

    (* .atd -> _j.ml, _j.mli *)
    let prods = [sprintf "%s_j.ml" base; sprintf "%s_j.mli" base] in
    rule ~deps:[file] ~prods (Atdgen.atdgen ~j:() ~j_std:() file)
  )
;;


let build_app (x:app) =
  let _I = List.filter_map x.internal_deps ~f:(function
    | Lib x -> Some (Filename.dirname x.dir)
    | App _ -> None )
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
      List.filter_map x.internal_deps ~f:(function
        | Lib x -> Some (path_of_lib mode x)
        | App _ -> None
      )
    )@
    [x.file]
  in
  let prod mode = path_of_app mode x in
  rule ~deps:(deps `byte) ~prods:[prod `byte]
    (ocamlc ~_I ~o:(prod `byte) (files `byte))
  ;
  rule ~deps:(deps `native) ~prods:[prod `native]
    (ocamlopt ~_I ~o:(prod `native) (files `native))


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
