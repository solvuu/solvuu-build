open Printf
open Solvuu_build.Std

let name = "my_project"
let version = "dev"

let thread = ()

let make_lib ?findlib_deps ?internal_deps ?build_if lib_name =
  Item.lib (sprintf "%s_%s" name lib_name)
    ~thread
    ?findlib_deps ?internal_deps ?build_if
    ~dir:(sprintf "lib/%s" lib_name)
    ~pack_name:(sprintf "%s_%s" name lib_name)
    ~pkg:(sprintf "%s.%s" name lib_name)

let make_app = Item.app ~thread

let async = make_lib "async"
  ~findlib_deps:["async"]

let lwt = make_lib "lwt"
  ~findlib_deps:["lwt"] ~build_if:[`Pkgs_installed]

let app = make_app "my_app"
  ~file:"app/my_app.ml"
  ~internal_deps:[async]

let project = Project.make ~name ~version [async;lwt;app]

;;
let () = Project.dispatch project
