open Printf
open Solvuu_build.Std

let project_name = "my_project"
let version = "dev"

let lib : Project.item = Project.lib project_name
  ~dir:"lib"
  ~pack_name:project_name
  ~pkg:project_name

let app : Project.item = Project.app "my-app"
  ~file:"app/my_app.ml"
  ~internal_deps:[lib]

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize project_name);
]

;;
let () = Project.basic1 ~project_name ~version [lib;app]
    ~ocamlinit_postfix
