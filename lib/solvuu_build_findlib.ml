module Util = Util
module List = Util.List

type pkg = string

(* This is necessary to query findlib *)
let () = Findlib.init ()

let all_packages =
  Fl_package_base.list_packages ()

let installed x = List.mem x ~set:all_packages
