open Printf
open Solvuu_build.Std

let project_name = "my_project"
let version = "dev"

let lib = Item.lib project_name
  ~dir:"lib"
  ~pack_name:project_name
  ~pkg:project_name

let app = Item.app "my_app"
  ~file:"app/my_app.ml"
  ~internal_deps:[lib]

let project = Project.make ~name:project_name ~version [lib;app]

;;
let () = Project.dispatch project
