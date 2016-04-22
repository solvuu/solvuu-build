(** Build system. *)
open Printf
module List = Util.List
module String = Util.String
let failwithf = Util.failwithf

module type PROJECT = sig
  val name : string
  val version : string
  val items : Items.t
  val ocamlinit_postfix : string list
end

module Make(Project:PROJECT) = struct
  open Ocamlbuild_plugin

  (* override the one from Ocamlbuild_plugin *)
  module List = Util.List

  (* Override values of [Items] module that take a [Items.t]. Here we
     set [Items.t = Project.items]. *)
  module Items = struct
    include Items

    (** All libs to build. *)
    let libs : Item.lib list =
      (Project.items : Items.t :> Item.t list)
      |> List.filter ~f:(Items.should_build Project.items)
      |> Items.filter_libs

    (** All apps that should be built. *)
    let apps : Item.app list =
      (Project.items : Items.t :> Item.t list)
      |> List.filter ~f:(Items.should_build Project.items)
      |> Items.filter_apps

    let libs_names = List.map libs ~f:(fun (x:Item.lib) -> x.Item.name)
    let apps_names = List.map apps ~f:(fun (x:Item.app) -> x.Item.name)

    (* let find = Items.find Project.items *)
    let internal_deps = Items.internal_deps Project.items
    let internal_deps_all = Items.internal_deps_all Project.items
    (* let findlib_deps = Items.findlib_deps Project.items *)
    let findlib_deps_all = Items.findlib_deps_all Project.items
    let all_findlib_pkgs = Items.all_findlib_pkgs Project.items
    let topologically_sorted = Items.topologically_sorted Project.items
  end

  let git_commit =
    if Sys.file_exists ".git" then
      sprintf "Some \"%s\""
        (
          Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD"
          |> fun x -> String.sub x 0 (String.length x - 1)
        )
    else
      "None"

  let tags_lines : string list =
    [
      "true: thread, bin_annot, annot, short_paths, safe_string, debug";
      "true: warn(A-4-33-41-42-44-45-48)";
      "true: use_menhir";
      "\"lib\": include";
    ]
    (* === for-pack tags *)
    @(
      List.map Items.libs_names ~f:(fun x ->
          sprintf
            "<lib/%s/*.cmx>: for-pack(%s_%s)"
            x (String.capitalize Project.name) x )
    )
    (* === use_foo for libs *)
    @(
      List.map Items.libs ~f:(fun (lib:Item.lib) ->
        lib.Item.name,
        Items.internal_deps `Lib lib.Item.name |>
        List.filter ~f:Item.is_lib |>
        List.map ~f:Item.name
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,libs) ->
          sprintf "<lib/%s/*>: %s"
            name
            (String.concat ", " (List.map libs ~f:(sprintf "use_%s_%s" Project.name)))
        )
    )
    (* === use_foo_stub tags for cm{a,xa,xs} *)
    @(
      Items.libs
      |> List.map ~f:(fun (x:Item.lib) -> x.Item.name)
      |> List.filter ~f:(fun x ->
          Util.c_units_of_dir ("lib"/x) <> []
        )
      |> List.map ~f:(fun x ->
          sprintf "<lib/%s_%s.{cma,cmxa,cmxs}>: use_%s_%s_stub"
            Project.name x Project.name x
        )
    )
    (* === package tags for libs *)
    @(
      List.map Items.libs ~f:(fun (lib:Item.lib) ->
        lib.Item.name, Items.findlib_deps_all `Lib lib.Item.name
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,pkgs) ->
        sprintf "<lib/%s/*>: %s"
          name
          (String.concat ", " (List.map pkgs ~f:(sprintf "package(%s)")))
      )
    )
    (* === use_foo for apps *)
    @(
      List.map Items.apps ~f:(fun (app:Item.app) ->
        app.Item.name,
        Items.internal_deps_all `App app.Item.name |>
        Items.filter_libs |>
        List.map ~f:(fun (x:Item.lib) -> x.Item.name)
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,libs) ->
          sprintf "<app/%s.*>: %s"
            name
            (String.concat ", " (List.map libs ~f:(sprintf "use_%s_%s" Project.name)))
        )
    )
    (* === package tags for apps *)
    @(
      List.map Items.apps ~f:(fun (app:Item.app) ->
        app.Item.name, Items.findlib_deps_all `App app.Item.name )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,pkgs) ->
        sprintf "<app/%s.*>: %s"
          name
          (String.concat "," (List.map pkgs ~f:(sprintf "package(%s)")))
      )
    )
    (* === use_foo_stub for apps *)
    @(
      List.map Items.apps ~f:(fun (app:Item.app) ->
        app.Item.name,
        Items.internal_deps_all `App app.Item.name |>
        Items.filter_libs |>
        List.map ~f:(fun (x:Item.lib) -> x.Item.name)
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,libs) ->
        sprintf "<app/%s.*>: %s"
          name
          (String.concat
             ","
             (List.map libs ~f:(sprintf "use_%s_%s" Project.name)))
      )
    )

  let mllib_file dir lib : string list =
    let path = dir / lib in
    if not (Sys.file_exists path && Sys.is_directory path) then
      failwithf "cannot create mllib file for dir %s" path ()
    else [ dir / String.capitalize (Project.name ^ "_" ^ lib) ]

  let merlin_file : string list =
    [
      "S ./lib/**";
      "S ./app/**";
      "B ./_build/lib";
      "B ./_build/lib/**";
      "B ./_build/app/**";
      "B +threads";
      "PKG solvuu_build";
    ]
    @(
      List.map Items.all_findlib_pkgs ~f:(fun x -> sprintf "PKG %s" x)
    )

  let meta_file : string list =
    List.map Items.libs_names ~f:(fun x ->
        let lib_name = sprintf "%s_%s" Project.name x in
        let requires : string list =
          (Items.findlib_deps_all `Lib x)
          @(List.map
              (Items.internal_deps `Lib x |> List.map ~f:Item.name)
              ~f:(sprintf "%s.%s" Project.name)
           )
        in
        [
          sprintf "package \"%s\" (" x;
          sprintf "  directory = \"%s\"" x;
          sprintf "  version = \"%s\"" Project.version;
          sprintf "  archive(byte) = \"%s.cma\"" lib_name;
          sprintf "  archive(native) = \"%s.cmxa\"" lib_name;
          sprintf "  requires = \"%s\"" (String.concat " " requires);
          sprintf "  exists_if = \"%s.cma\"" lib_name;
          sprintf ")";
        ]
      )
    |> List.flatten
    |> List.filter ~f:((<>) "")

  let install_file : string list =
    let suffixes = [
      "a";"annot";"cma";"cmi";"cmo";"cmt";"cmti";"cmx";"cmxa";
      "cmxs";"dll";"o"]
    in
    (
      List.map Items.libs_names ~f:(fun lib ->
          List.map suffixes ~f:(fun suffix ->
              sprintf "  \"?_build/lib/%s_%s.%s\" { \"%s/%s_%s.%s\" }"
                Project.name lib suffix
                lib Project.name lib suffix
            )
        )
      |> List.flatten
      |> fun l -> "  \"_build/META\""::l
                  |> fun l -> ["lib: ["]@l@["]"]
    )
    @(
      let lines =
        List.map Items.libs_names ~f:(fun lib ->
            sprintf "  \"?_build/lib/dll%s_%s_stub.so\"" Project.name lib
          )
      in
      "stublibs: [" :: lines @ ["]"]
    )
    @(
      List.map Items.apps_names ~f:(fun app ->
          List.map ["byte"; "native"] ~f:(fun suffix ->
              sprintf "  \"?_build/app/%s.%s\" {\"%s\"}" app suffix app
            )
        )
      |> List.flatten
      |> function
      | [] -> []
      | l -> ["bin: ["]@l@["]"]
    )

  let ocamlinit_file =
    [
      "let () =" ;
      "  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")" ;
      "  with Not_found -> ()" ;
      ";;" ;
      "" ;
      "#use \"topfind\";;" ;
      "#thread;;" ;
      sprintf "#require \"%s\";;" (String.concat " " Items.all_findlib_pkgs);
      "" ;
      "(* Load each lib provided by this project. *)" ;
      "#directory \"_build/lib\";;" ;
    ]
    @(
      Items.topologically_sorted |>
      Items.filter_libs |>
      List.map ~f:(fun (x:Item.lib) ->
        sprintf "#load \"%s_%s.cma\";;" Project.name x.Item.name )
    )
    @ Project.ocamlinit_postfix

  let makefile_rules_file : string list =
    let map_name xs = List.map xs ~f:(sprintf "%s_%s" Project.name) in
    let native =
      List.concat [
        map_name Items.libs_names |> List.map ~f:(sprintf "lib/%s.cmxa") ;
        map_name Items.apps_names |> List.map ~f:(sprintf "lib/%s.cmxs") ;
        List.map Items.apps_names ~f:(sprintf "app/%s.native") ;
      ]
      |> String.concat " "
      |> sprintf "native: %s"
    in
    let byte =
      List.concat [
        map_name Items.libs_names |> List.map ~f:(sprintf "lib/%s.cma") ;
        List.map Items.apps_names ~f:(sprintf "app/%s.byte") ;
      ]
      |> String.concat " "
      |> sprintf "byte: %s"
    in
    let static = [
      "default: byte project_files.stamp";

      "%.cma %.cmxa %.cmxs %.native %.byte lib/%.mlpack:";
      "\t$(OCAMLBUILD) $@";

      "project_files.stamp META:";
      "\t$(OCAMLBUILD) $@";

      sprintf ".merlin %s.install .ocamlinit:" Project.name;
      "\t$(OCAMLBUILD) $@ && ln -s _build/$@ $@";

      "clean:";
      "\t$(OCAMLBUILD) -clean";

      ".PHONY: default native byte clean";
    ]
    in
    static@[native ; byte]

  let make_static_file path contents =
    let contents = List.map contents ~f:(sprintf "%s\n") in
    rule path ~prod:path (fun _ _ ->
        Seq [
          Cmd (Sh (sprintf "mkdir -p %s" (Filename.dirname path)));
          Echo (contents,path);
        ]
      )

  let plugin = function
    | Before_options -> (
        Options.use_ocamlfind := true;
        List.iter tags_lines ~f:Ocamlbuild_pack.Configuration.parse_string
      )
    | After_rules -> (
        rule "m4: ml.m4 -> ml"
          ~prod:"%.ml"
          ~dep:"%.ml.m4"
          (fun env _ ->
             let ml_m4 = env "%.ml.m4" in
             Cmd (S [
                 A "m4";
                 A "-D"; A ("VERSION=" ^ Project.version);
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

        List.iter Items.libs_names ~f:(fun lib ->
            make_static_file
              (sprintf "lib/%s_%s.mlpack" Project.name lib)
              (Util.mlpack_file ("lib"/lib))
          );

        List.iter Items.libs_names ~f:(fun lib ->
            make_static_file
              (sprintf "lib/%s_%s.mllib" Project.name lib)
              (mllib_file "lib" lib)
          );

        List.iter Items.libs_names ~f:(fun lib ->
            let lib_name = sprintf "%s_%s" Project.name lib in
            let lib_tag = sprintf "use_%s_%s" Project.name lib in
            flag ["link";"ocaml";lib_tag] (S[A"-I"; P "lib"]);
            ocaml_lib ~tag_name:lib_tag ~dir:"lib" ("lib/" ^ lib_name) ;
            dep ["ocaml";"byte";lib_tag] [sprintf "lib/%s.cma" lib_name] ;
            dep ["ocaml";"native";lib_tag] [sprintf "lib/%s.cmxa" lib_name]
          );

        List.iter Items.libs_names ~f:(fun lib ->
            match Util.clib_file "lib" lib with
            | None -> ()
            | Some file ->
              let cstub = sprintf "%s_%s_stub" Project.name lib in
              let stub_tag = "use_"^cstub in
              let headers =
                Util.h_files_of_dir ("lib"/lib)
                |> List.map ~f:(fun x -> "lib"/lib/x)
              in
              dep ["c" ; "compile"] headers ;
              dep ["link";"ocaml";stub_tag] [
                sprintf "lib/lib%s.a" cstub ;
              ] ;
              flag
                ["link";"ocaml";"byte";stub_tag]
                (S[A"-dllib";A("-l"^cstub);A"-cclib";A("-l"^cstub)]) ;
              flag
                ["link";"ocaml";"native";stub_tag]
                (S[A"-cclib";A("-l"^cstub)]) ;
              make_static_file
                (sprintf "lib/lib%s.clib" cstub)
                file
          ) ;

        make_static_file ".merlin" merlin_file;
        make_static_file "META" meta_file;
        make_static_file (sprintf "%s.install" Project.name) install_file;
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

  let dispatch () = dispatch plugin

end
