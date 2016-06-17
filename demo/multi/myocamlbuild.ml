open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "my_project"
let version = "dev"

let thread = ()

let make_lib ?findlib_deps ?internal_deps lib_name =
  Project.lib (sprintf "%s_%s" project_name lib_name)
    ~thread
    ?findlib_deps ?internal_deps
    ~dir:(sprintf "lib/%s" lib_name)
    ~pack_name:(sprintf "%s_%s" project_name lib_name)
    ~pkg:(sprintf "%s.%s" project_name lib_name)

let make_app = Project.app ~thread

let async = make_lib "async"
  ~findlib_deps:["async"]

let lwt = make_lib "lwt"
  ~findlib_deps:["lwt"]

let app = make_app "my_app"
  ~file:"app/my_app.ml"
  ~internal_deps:[async]

let optional_pkgs = ["async"; "lwt"]

let items =
  [async;lwt;app] |>
  List.filter ~f:(fun x -> Project.dep_opts_sat x optional_pkgs)

;;
let () = Project.basic1 ~project_name ~version items
