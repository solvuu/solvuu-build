open Printf
module Util = Util
module List = Util.List
module String = Util.String

type pkg = string

(* This is necessary to query findlib *)
let () = Findlib.init ()

let all_packages =
  Fl_package_base.list_packages ()

let installed x = List.mem x ~set:all_packages

let to_use_tag x =
  String.map (function '.' -> '_' | c -> c) x
  |> sprintf "use_%s"

let to_path x =
  String.split x ~on:'.'
