open Printf
open Solvuu_build.Std

let name = "my_project"
let version = "dev"

let make_lib ?findlib_deps ?internal_deps ?build_if lib_name =
  Item.lib (sprintf "%s_%s" name lib_name)
    ?findlib_deps ?internal_deps ?build_if
    ~dir:(sprintf "lib/%s" lib_name)
    ~pack_name:(sprintf "%s_%s" name lib_name)
    ~pkg:(sprintf "%s.%s" name lib_name)

let async = make_lib "async"
  ~findlib_deps:["async"]

let lwt = make_lib "lwt"
  ~internal_deps:[async] ~findlib_deps:["lwt"] ~build_if:[`Pkgs_installed]

let app = Item.app "my_app"
  ~file:"app/my_app.ml"
  ~internal_deps:[async]

let project = Project.make ~name ~version [async;lwt;app]

;;
let () = Project.dispatch project
