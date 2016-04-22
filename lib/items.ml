module List = Util.List
module Findlib = Solvuu_build_findlib
let failwithf = Util.failwithf

type pkg = Findlib.pkg
type name = Item.name
type typ = Item.typ
type lib = Item.lib
type app = Item.app

type item = Item.t = Lib of lib | App of app

type t = item list

let of_list items =
  ignore (Item.Graph.of_list items);
  items

let filter_libs t =
  List.filter_map t ~f:(function Item.Lib x -> Some x | Item.App _ -> None)

let filter_apps t =
  List.filter_map t ~f:(function Item.App x -> Some x | Item.Lib _ -> None)

let find t typ name =
  try List.find t ~f:(fun x -> Item.typ x = typ && Item.name x = name)
  with Not_found ->
    failwithf "unknown %s %s" (Item.typ_to_string typ) name ()

let find_lib t name =
  filter_libs t |> fun l ->
  try List.find l ~f:(fun (x:lib) -> x.Item.name = name)
  with Not_found ->
    failwithf "unknown lib %s" name ()

let find_app t name =
  filter_apps t |> fun l ->
  try List.find l ~f:(fun (x:app) -> x.Item.name = name)
  with Not_found ->
    failwithf "unknown app %s" name ()

let internal_deps t typ name =
  Item.internal_deps (find t typ name)

let rec internal_deps_all t typ name =
  let item = find t typ name in
  (Item.internal_deps item)
  @(
    List.map (Item.internal_deps item) ~f:(fun x ->
      internal_deps_all t (Item.typ x) (Item.name x)
    )
    |> List.flatten
  )
  |> List.sort_uniq Item.compare

let findlib_deps t typ name =
  Item.findlib_deps (find t typ name)

let rec findlib_deps_all t typ name =
  let item = find t typ name in
  (Item.findlib_deps item)
  @(
    List.map (Item.internal_deps item) ~f:(fun x ->
      findlib_deps_all t (Item.typ x) (Item.name x)
    )
    |> List.flatten
  )
  |> List.sort_uniq String.compare

let all_findlib_pkgs t =
  List.map t ~f:Item.findlib_deps
  |> List.flatten
  |> List.sort_uniq String.compare

let rec should_build items (i:item) =
  List.for_all (Item.build_if i) ~f:(function
    | `Pkgs_installed ->
      List.for_all (Item.findlib_deps i) ~f:Findlib.installed
  )
  &&
  List.for_all (Item.internal_deps i) ~f:(fun x ->
    should_build items (find items (Item.typ x) (Item.name x))
  )

let topologically_sorted t =
  Item.Graph.Topological.fold
    (fun x t -> x::t)
    (Item.Graph.of_list t)
    []
