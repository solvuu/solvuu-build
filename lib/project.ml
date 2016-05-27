(** Build system. *)
open Printf
module Findlib = Solvuu_build_findlib
open Util

type name = string
type version = string

type t = {
  name : name;
  version : version;
  git_commit : string option;
  libs : Item.lib list;
  apps : Item.app list;
  merlin_file : string list;
  meta_file : Fl_metascanner.pkg_expr;
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

let meta_file libs version : Fl_metascanner.pkg_expr =
  let def ?(preds=[]) var value =
      {
        Fl_metascanner.def_var = var;
        def_preds = preds;
        def_flav = `BaseDef;
        def_value = value;
      }
  in
  let pkg_defs_of_lib (x:Item.lib) =
    [
      def "directory"
        (Findlib.to_path x.Item.pkg |> List.tl |> String.concat "/");
      def "version" version;
      def ~preds:[`Pred "byte"] "archive" (sprintf "%s.cma" x.Item.name);
      def ~preds:[`Pred "native"] "archive" (sprintf "%s.cmxa" x.Item.name);
      def "requires" (
        (Item.findlib_deps_all (Item.Lib x))
        @(
          Item.internal_deps (Item.Lib x)
          |> Item.filter_libs
          |> List.map ~f:(fun x -> x.Item.pkg)
        )
        |> String.concat " "
      );
      def "exists_if" (sprintf "%s.cma" x.Item.name);
    ]
  in
  let pkgs = List.map libs ~f:(fun x -> x.Item.pkg) in
  let graph = Findlib.Graph.of_list pkgs in
  let root = match Findlib.Graph.roots graph with
    | x::[] -> x
    | [] -> failwith "findlib packages have no root"
    | l -> failwithf "cannot create META file for findlib packages with \
                      multiple roots: %s" (String.concat "," l) ()
  in
  let pkg_defs (pkg:Findlib.pkg) =
    try pkg_defs_of_lib @@ List.find libs ~f:(fun x -> x.Item.pkg = pkg)
    with _ -> []
  in
  let rec pkg_expr (pkg:Findlib.pkg) =
    {
      Fl_metascanner.pkg_defs = pkg_defs pkg;
      pkg_children =
        List.map (Findlib.Graph.succ graph pkg) ~f:(fun x ->
          (Findlib.to_path x |> List.last_exn),
          (pkg_expr x)
        )
    }
  in
  pkg_expr root

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
      List.map all_apps ~f:(Item.path_of_app ~suffix:".native");
    ]
    |> String.concat " "
    |> sprintf "native: %s"
  in
  let byte =
    List.concat [
      List.map all_libs ~f:(fun x ->
        sprintf "%s/%s.cma" (Filename.dirname x.Item.dir) x.Item.name);
      List.map all_apps ~f:(Item.path_of_app ~suffix:".byte");
    ]
    |> String.concat " "
    |> sprintf "byte: %s"
  in
  let static = [
    "default: .merlin .ocamlinit byte";

    "%.cma %.cmxa %.cmxs %.native %.byte %.mlpack:";
    "\t$(OCAMLBUILD) $@";

    "META:";
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
let build_static_file path content =
  let open Ocamlbuild_plugin in
  let open Util in
  (* Workaround ocamlbuild bug: https://github.com/ocaml/ocamlbuild/issues/76. *)
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
  function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      M4.m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match t.git_commit with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some t.version;
        ];

      Atdgen.atdgen_t_rule ~j_std:() ();
      Atdgen.atdgen_j_rule ~j_std:() ();

      OCaml.Menhir.rule ();
      OCaml.Ocamllex.rule ();

      List.iter t.libs ~f:Item.build_lib;
      List.iter t.apps ~f:Item.build_app;
      (* List.iter t.libs ~f:Rule.clib; *)

      build_static_file ".merlin" t.merlin_file;

      Ocamlbuild_plugin.rule "META" ~prod:"META" (fun _ _ ->
        let oc = open_out "META" in
        Fl_metascanner.print oc t.meta_file;
        close_out oc;
        Ocamlbuild_plugin.Nop
      );

      build_static_file (sprintf "%s.install" t.name) t.install_file;
      build_static_file ".ocamlinit" t.ocamlinit_file;
      build_static_file "Makefile.rules" t.makefile_rules_file;
    )
  | _ -> ()

let dispatch t =
  Ocamlbuild_plugin.dispatch (plugin t)
