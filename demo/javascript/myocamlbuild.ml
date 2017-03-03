open Solvuu_build.Std
open Solvuu_build.Util

let app = Project.app "hello"
    ~file:"hello.ml"
    ~findlib_deps:[
      "js_of_ocaml";
      "js_of_ocaml.ppx";
    ]


let () =
  let items = [app] in

  ignore (Project.Graph.of_list items);

  let libs = Project.filter_libs items in
  let apps = Project.filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter libs ~f:Project.build_lib;
      List.iter apps ~f:Project.build_app;
      List.iter items ~f:Project.build_js_of_ocaml;

      Project.build_static_file ".merlin"
        (fun () -> Project.merlin_file items);
      Project.build_static_file ".ocamlinit"
        (fun () -> Project.ocamlinit_file items);
      Project.build_static_file "project.mk"
        (fun () -> Project.makefile ~project_name:"hello" items);
    )
  | _ -> ()
