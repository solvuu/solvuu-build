(** Build system. *)
open Printf
module List = Util.List
module String = Util.String
let failwithf = Util.failwithf

type t = {
  libs : Item.lib list;
  apps : Item.app list;

  mllib_file : Item.lib -> string list;

  tags_file : string list;
  merlin_file : string list;
  meta_file : string list;
  install_file : string list;
  ocamlinit_file : string list;
  makefile_rules_file : string list;

  plugin : Ocamlbuild_plugin.hook -> unit;
  dispatch : unit -> unit;
}

let make ?(ocamlinit_postfix=[]) ~name ~version items =
  (* Compute graph to check for cycles and other errors. *)
  let graph = Item.Graph.of_list items in

  let open Ocamlbuild_plugin in

  (* override some modules from Ocamlbuild_plugin *)
  let module List = Util.List in
  let module Findlib = Solvuu_build_findlib in

  (** All libs to build. *)
  let all_libs : Item.lib list =
    items
    |> List.filter ~f:Item.should_build
    |> Item.filter_libs
  in

  (** All apps that should be built. *)
  let all_apps : Item.app list =
    items
    |> List.filter ~f:Item.should_build
    |> Item.filter_apps
  in

  let git_commit =
    if Sys.file_exists ".git" then
      sprintf "Some \"%s\""
        (
          Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD"
          |> fun x -> String.sub x 0 (String.length x - 1)
        )
    else
      "None"
  in

  let tags_file : string list =
    [
      "true: thread, bin_annot, annot, short_paths, safe_string, debug";
      "true: warn(A-4-33-41-42-44-45-48)";
      "true: use_menhir";
      "\"lib\": include";
    ]

    (* === for-pack tags *)
    @(
      List.map all_libs ~f:(fun x ->
          sprintf
            "<%s/*.cmx>: for-pack(%s)"
            x.Item.dir (String.capitalize x.Item.pack_name) )
    )

    (* === use_foo for libs *)
    @(
      List.map all_libs ~f:(fun lib ->
        lib, Item.filter_libs lib.Item.internal_deps
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (lib,libs) ->
        sprintf "<%s/*>: %s"
          lib.Item.dir
          (
            List.map libs ~f:(fun x -> Findlib.to_use_tag x.Item.pkg)
            |> String.concat ", "
          )
      )
    )

    (* === use_foo_stub tags for cm{a,xa,xs} *)
    @(
      all_libs
      |> List.filter ~f:(fun x -> Util.c_units_of_dir x.Item.dir <> [])
      |> List.map ~f:(fun x ->
          sprintf "<%s.{cma,cmxa,cmxs}>: %s_stub"
            x.Item.dir (Findlib.to_use_tag x.Item.pkg)
      )
    )

    (* === package tags for libs *)
    @(
      List.map all_libs ~f:(fun lib ->
        lib, Item.findlib_deps_all (Item.Lib lib)
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (lib,pkgs) ->
        sprintf "<%s/*>: %s"
          lib.Item.dir
          (List.map pkgs ~f:(sprintf "package(%s)") |> String.concat ", ")
      )
    )

    (* === use_foo for apps *)
    @(
      List.map all_apps ~f:(fun (app:Item.app) ->
        app, Item.internal_deps_all (Item.App app) |> Item.filter_libs
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (app,libs) ->
          sprintf "<%s.*>: %s"
            (Filename.chop_extension app.Item.file)
            (
              List.map libs ~f:(fun x -> sprintf "use_%s" x.Item.name)
              |> String.concat ", "
            )
      )
    )

    (* === package tags for apps *)
    @(
      List.map all_apps ~f:(fun (app:Item.app) ->
        app, Item.findlib_deps_all (Item.App app)
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (app,pkgs) ->
        sprintf "<%s.*>: %s"
          (Filename.chop_extension app.Item.file)
          (
            List.map pkgs ~f:(sprintf "package(%s)")
            |> String.concat ","
          )
      )
    )

    (* === use_foo_stub for apps *)
    @(
      List.map all_apps ~f:(fun (app:Item.app) ->
        app, Item.internal_deps_all (Item.App app) |> Item.filter_libs
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (app,libs) ->
        sprintf "<%s.*>: %s"
          (Filename.chop_extension app.Item.file)
          (
            String.concat "," @@
            List.map libs ~f:(fun x -> sprintf "use_%s" x.Item.name)
          )
      )
    )
  in

  let mllib_file (x:Item.lib) : string list =
    if not (Sys.file_exists x.Item.dir && Sys.is_directory x.Item.dir) then
      failwithf "cannot create mllib file for dir %s" x.Item.dir ()
    else [ Filename.dirname x.Item.dir / String.capitalize x.Item.name ]
  in

  let merlin_file : string list =
    [
      ["B +threads"; "PKG solvuu_build"];

      (* libs *)
      List.map all_libs ~f:(fun x ->
        [
          sprintf "S %s" x.Item.dir;
          sprintf "B _build/%s" x.Item.dir;
          sprintf "B _build/%s" (Filename.dirname x.Item.dir);
        ]
      ) |> List.concat;

      (* apps *)
      List.map all_apps ~f:(fun x ->
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
  in

  let meta_file : string list =
    List.map all_libs ~f:(fun x ->
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
  in

  let install_file : string list =
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
  in

  let ocamlinit_file =
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
      ocamlinit_postfix;
    ]
    |> List.concat
  in

  let makefile_rules_file : string list =
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

      sprintf ".merlin %s.install .ocamlinit:" name;
      "\t$(OCAMLBUILD) $@ && ln -s _build/$@ $@";

      "clean:";
      "\t$(OCAMLBUILD) -clean";

      ".PHONY: default native byte clean";
    ]
    in
    static@[native ; byte]
  in

  let make_static_file path contents =
    let contents = List.map contents ~f:(sprintf "%s\n") in
    rule path ~prod:path (fun _ _ ->
        Seq [
          Cmd (Sh (sprintf "mkdir -p %s" (Filename.dirname path)));
          Echo (contents,path);
        ]
      )
  in

  let plugin = function
    | Before_options -> (
        Options.use_ocamlfind := true;
        List.iter tags_file ~f:Ocamlbuild_pack.Configuration.parse_string
      )
    | After_rules -> (
        rule "m4: ml.m4 -> ml"
          ~prod:"%.ml"
          ~dep:"%.ml.m4"
          (fun env _ ->
             let ml_m4 = env "%.ml.m4" in
             Cmd (S [
                 A "m4";
                 A "-D"; A ("VERSION=" ^ version);
                 A "-D"; A ("GIT_COMMIT=" ^ git_commit);
                 P ml_m4;
                 Sh ">";
                 P (env "%.ml");
               ]) )
        ;

        rule "atd: .atd -> _t.ml, _t.mli"
          ~dep:"%.atd"
          ~prods:["%_t.ml"; "%_t.mli"]
          (fun env _ ->
             Cmd (S [A "atdgen"; A "-t"; A "-j-std"; P (env "%.atd")])
          )
        ;

        rule "atd: .atd -> _j.ml, _j.mli"
          ~dep:"%.atd"
          ~prods:["%_j.ml"; "%_j.mli"]
          (fun env _ ->
             Cmd (S [A "atdgen"; A "-j"; A "-j-std"; P (env "%.atd")])
          )
        ;

        List.iter all_libs ~f:(fun lib ->
          make_static_file
            (sprintf "%s/%s.mlpack"
               (Filename.dirname lib.Item.dir) lib.Item.name
            )
            (Util.mlpack_file lib.Item.dir)
        );

        List.iter all_libs ~f:(fun lib ->
          make_static_file
            (sprintf "%s/%s.mllib"
               (Filename.dirname lib.Item.dir) lib.Item.name)
            (mllib_file lib)
        );

        List.iter all_libs ~f:(fun lib ->
            let lib_name = lib.Item.name in
            let lib_tag = Findlib.to_use_tag lib.Item.pkg in
            let dir = Filename.dirname lib.Item.dir in
            flag ["link";"ocaml";lib_tag] (S[A"-I"; P dir]);
            ocaml_lib ~tag_name:lib_tag ~dir (dir ^ "/" ^ lib_name) ;
            dep ["ocaml";"byte";lib_tag] [sprintf "%s/%s.cma" dir lib_name] ;
            dep ["ocaml";"native";lib_tag] [sprintf "%s/%s.cmxa" dir lib_name]
        );

        List.iter all_libs ~f:(fun lib ->
            match Util.clib_file lib.Item.dir lib.Item.name with
            | None -> ()
            | Some file ->
              let cstub = sprintf "%s_stub" lib.Item.name in
              let stub_tag = "use_"^cstub in
              let headers =
                Util.h_files_of_dir lib.Item.dir
                |> List.map ~f:(fun x -> lib.Item.dir/x)
              in
              dep ["c" ; "compile"] headers ;
              dep ["link";"ocaml";stub_tag] [
                sprintf "%s/lib%s.a" (Filename.dirname lib.Item.dir) cstub ;
              ] ;
              flag
                ["link";"ocaml";"byte";stub_tag]
                (S[A"-dllib";A("-l"^cstub);A"-cclib";A("-l"^cstub)]) ;
              flag
                ["link";"ocaml";"native";stub_tag]
                (S[A"-cclib";A("-l"^cstub)]) ;
              make_static_file
                (sprintf "%s/lib%s.clib" (Filename.dirname lib.Item.dir) cstub)
                file
        ) ;

        make_static_file ".merlin" merlin_file;
        make_static_file "META" meta_file;
        make_static_file (sprintf "%s.install" name) install_file;
        make_static_file ".ocamlinit" ocamlinit_file;
        make_static_file "Makefile.rules" makefile_rules_file ;

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
      )
    | _ -> ()
  in

  {
    libs = all_libs;
    apps = all_apps;
    mllib_file;
    tags_file;
    merlin_file;
    meta_file;
    install_file;
    ocamlinit_file;
    makefile_rules_file;
    plugin;
    dispatch = fun () -> dispatch plugin;
  }
