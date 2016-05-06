(** Build system. *)
open Printf
module Findlib = Solvuu_build_findlib
module List = Util.List
module String = Util.String

type name = string
type version = string

type t = {
  name : name;
  version : version;
  git_commit : string option;
  libs : Item.lib list;
  apps : Item.app list;
  merlin_file : string list;
  meta_file : string list;
  install_file : string list;
  ocamlinit_file : string list;
  makefile_rules_file : string list;
}


(******************************************************************************)
(** {2 Static Files} *)
(******************************************************************************)
type content = string list

let merlin_file items : string list =
  [
    ["B +threads"; "PKG solvuu_build"];

    (* libs *)
    List.map (Item.filter_libs items) ~f:(fun x ->
      [
        sprintf "S %s" x.Item.dir;
        sprintf "B _build/%s" x.Item.dir;
        sprintf "B _build/%s" (Filename.dirname x.Item.dir);
      ]
    ) |> List.concat;

    (* apps *)
    List.map (Item.filter_apps items) ~f:(fun x ->
      [
        sprintf "S %s" (Filename.dirname x.Item.file);
        sprintf "B _build/%s" (Filename.dirname x.Item.file);
      ]
    ) |> List.concat;

    (* findlib packages *)
    (
      Item.all_findlib_pkgs items
      |> List.map ~f:(sprintf "PKG %s")
    );
  ]
  |> List.concat
  |> List.sort_uniq String.compare

let meta_file libs version : string list =
  List.map libs ~f:(fun x ->
    let requires : string list =
      (Item.findlib_deps_all (Item.Lib x))
      @(
        Item.internal_deps (Item.Lib x)
        |> Item.filter_libs
        |> List.map ~f:(fun x -> x.Item.pkg)
      )
    in
    [
      sprintf "package \"%s\" (" x.Item.name;
      sprintf "  directory = \"%s\"" x.Item.dir;
      sprintf "  version = \"%s\"" version;
      sprintf "  archive(byte) = \"%s.cma\"" x.Item.name;
      sprintf "  archive(native) = \"%s.cmxa\"" x.Item.name;
      sprintf "  requires = \"%s\"" (String.concat " " requires);
      sprintf "  exists_if = \"%s.cma\"" x.Item.name;
      sprintf ")";
    ]
  )
  |> List.flatten
  |> List.filter ~f:((<>) "")

let install_file items : string list =
  let all_libs = Item.filter_libs items in
  let all_apps = Item.filter_apps items in
  let suffixes = [
    "a";"annot";"cma";"cmi";"cmo";"cmt";"cmti";"cmx";"cmxa";
    "cmxs";"dll";"o"]
  in
  (
    List.map all_libs ~f:(fun lib ->
      List.map suffixes ~f:(fun suffix ->
        sprintf "  \"?_build/%s/%s.%s\" { \"%s/%s.%s\" }"
          (Filename.dirname lib.Item.dir) lib.Item.name suffix
          (Findlib.to_path lib.Item.pkg |> List.tl |> String.concat "/")
          lib.Item.name suffix
      )
    )
    |> List.flatten
    |> fun l -> "  \"_build/META\""::l
                |> fun l -> ["lib: ["]@l@["]"]
  )
  @(
    let lines =
      List.map all_libs ~f:(fun lib ->
        sprintf "  \"?_build/%s/dll%s_stub.so\""
          (Filename.dirname lib.Item.dir) lib.Item.name
      )
    in
    "stublibs: [" :: lines @ ["]"]
  )
  @(
    List.map all_apps ~f:(fun app ->
      List.map ["byte"; "native"] ~f:(fun suffix ->
        sprintf "  \"?_build/%s.%s\" {\"%s\"}"
          (Filename.chop_extension app.Item.file)
          suffix
          app.Item.name
      )
    )
    |> List.flatten
    |> function
    | [] -> []
    | l -> ["bin: ["]@l@["]"]
  )

let ocamlinit_file items ~postfix =
  let graph = Item.Graph.of_list items in
  [
    [
      "let () =";
      "  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")";
      "  with Not_found -> ()";
      ";;" ;
      "";
    ];
    [
      "#use \"topfind\";;";
      "#thread;;";
      "";
    ];
    [
      sprintf "#require \"%s\";;"
        (String.concat " " @@ Item.all_findlib_pkgs items);
      "";
    ];
    [
      "(* Load each lib provided by this project. *)";
    ];
    (
      Item.Graph.Topological.sort graph |>
      Item.filter_libs |>
      List.map ~f:(fun x ->
        sprintf "#directory \"_build/%s\";;" (Filename.dirname x.Item.dir)
      )
      |> List.sort_uniq String.compare
    );
    (
      Item.Graph.Topological.sort graph |>
      Item.filter_libs |>
      List.map ~f:(fun (x:Item.lib) ->
        sprintf "#load \"%s.cma\";;" x.Item.name
      )
    );
    [""];
    postfix;
  ]
  |> List.concat

let makefile_rules_file items project_name : string list =
  let all_libs = Item.filter_libs items in
  let all_apps = Item.filter_apps items in
  let native =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxa" (Filename.dirname x.Item.dir) x.Item.name);
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cmxs" (Filename.dirname x.Item.dir) x.Item.name);
      List.map all_apps ~f:(fun (x:Item.app) ->
        sprintf "%s.native" (Filename.chop_extension x.Item.file));
    ]
    |> String.concat " "
    |> sprintf "native: %s"
  in
  let byte =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cma" (Filename.dirname x.Item.dir) x.Item.name);
      List.map all_apps ~f:(fun x ->
        sprintf "%s.byte" (Filename.chop_extension x.Item.file));
    ]
    |> String.concat " "
    |> sprintf "byte: %s"
  in
  let static = [
    "default: byte project_files.stamp";

    "%.cma %.cmxa %.cmxs %.native %.byte %.mlpack:";
    "\t$(OCAMLBUILD) $@";

    "project_files.stamp META:";
    "\t$(OCAMLBUILD) $@";

    sprintf ".merlin %s.install .ocamlinit:" project_name;
    "\t$(OCAMLBUILD) $@ && ln -s _build/$@ $@";

    "clean:";
    "\t$(OCAMLBUILD) -clean";

    ".PHONY: default native byte clean";
  ]
  in
  static@[native ; byte]


(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
module Rule = struct
  open Ocamlbuild_plugin

  (* override some modules from Ocamlbuild_plugin *)
  module List = Util.List
  module String = Util.String
  module Findlib = Solvuu_build_findlib

  let static_file path content =
    (* Workar ocamlbuild bug: https://github.com/ocaml/ocamlbuild/issues/76. *)
    let path = match String.split ~on:'/' path with
      | "."::l -> String.concat "/" l
      | _ -> path
    in
    let content = List.map content ~f:(sprintf "%s\n") in
    rule path ~prod:path (fun _ _ ->
      Seq [
        Cmd (Sh (sprintf "mkdir -p %s" (Filename.dirname path)));
        Echo (content,path);
      ]
    )

  (* let clib lib = *)
  (*   match Util.clib_file lib.Item.dir lib.Item.name with *)
  (*   | None -> () *)
  (*   | Some file -> *)
  (*     let cstub = sprintf "%s_stub" lib.Item.name in *)
  (*     let stub_tag = "use_"^cstub in *)
  (*     let headers = *)
  (*       Util.h_files_of_dir lib.Item.dir *)
  (*       |> List.map ~f:(fun x -> lib.Item.dir/x) *)
  (*     in *)
  (*     dep ["c" ; "compile"] headers ; *)
  (*     dep ["link";"ocaml";stub_tag] [ *)
  (*       sprintf "%s/lib%s.a" (Filename.dirname lib.Item.dir) cstub ; *)
  (*     ] ; *)
  (*     flag *)
  (*       ["link";"ocaml";"byte";stub_tag] *)
  (*       (S[A"-dllib";A("-l"^cstub);A"-cclib";A("-l"^cstub)]) ; *)
  (*     flag *)
  (*       ["link";"ocaml";"native";stub_tag] *)
  (*       (S[A"-cclib";A("-l"^cstub)]) ; *)
  (*     static_file *)
  (*       (sprintf "%s/lib%s.clib" (Filename.dirname lib.Item.dir) cstub) *)
  (*       file *)

  let project_files () =
    rule "project files"
      ~stamp:"project_files.stamp"
      (fun _ build ->
         let project_files = [
           [".merlin"];
           [".ocamlinit"];
         ]
         in
         List.map (build project_files) ~f:Outcome.good
         |> List.map ~f:(fun result ->
           Cmd (S [A "ln"; A "-sf";
                   P ((Filename.basename !Options.build_dir)/result);
                   P Pathname.pwd] )
         )
         |> fun l -> Seq l
      )

end

(******************************************************************************)
(** {2 Main Functions} *)
(******************************************************************************)
let make ?(ocamlinit_postfix=[]) ~name ~version items =
  (* Compute graph to check for cycles and other errors. *)
  ignore (Item.Graph.of_list items);

  (* Filter for items that should be built. *)
  let items = List.filter items ~f:Item.should_build in
  let libs = Item.filter_libs items in
  let apps = Item.filter_apps items in
  {
    name;
    version;
    git_commit = Git.last_commit();
    libs = libs;
    apps = apps;
    merlin_file = merlin_file items;
    meta_file = meta_file libs version;
    install_file = install_file items;
    ocamlinit_file = ocamlinit_file items ~postfix:ocamlinit_postfix;
    makefile_rules_file = makefile_rules_file items name;
  }

let plugin t =
  let git_commit = t.git_commit in
  let project_version = t.version in
  function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter t.libs ~f:(Item.build_lib ?git_commit ~project_version);
      List.iter t.apps ~f:Item.build_app;
      (* List.iter t.libs ~f:Rule.clib; *)

      Rule.static_file ".merlin" t.merlin_file;
      Rule.static_file "META" t.meta_file;
      Rule.static_file (sprintf "%s.install" t.name) t.install_file;
      Rule.static_file ".ocamlinit" t.ocamlinit_file;
      Rule.static_file "Makefile.rules" t.makefile_rules_file;

      Rule.project_files();
    )
  | _ -> ()

let dispatch t =
  Ocamlbuild_plugin.dispatch (plugin t)
