open Printf
module Findlib = Solvuu_build_findlib
open Util
open Util.Filename
let (/) = Util.Filename.concat

type pkg = Solvuu_build_findlib.pkg
type host = [ `Server | `Client ]
type filename = string

type lib = {
  name : string;
  style : [ `Pack of string ];
  dir : string;
  findlib_deps : (host -> pkg list);
  ml_files : (host -> filename list);
  mli_files : (host -> filename list);

  annot : unit option;
  bin_annot : unit option;
  g : unit option;
  safe_string : unit option;
  short_paths : unit option;
  thread : unit option;
  w : string option;
}

(* Logically one should include eliom.ppx.server and eliom.ppx.client
   in [findlib_deps]. However one must not do so when using eliom's
   command line tools. They add these packages automatically and
   adding them again causes mysterious runtime errors.  *)
let eliom_tools_workaround findlib_deps = function
  | `Server -> (
      findlib_deps `Server |>
      List.filter ~f:((<>) "eliom.ppx.server")
    )
  | `Client -> (
      findlib_deps `Client |>
      List.filter ~f:((<>) "eliom.ppx.client")
    )

let lib
    ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
    ?findlib_deps ~ml_files ~mli_files ~style ~dir name
  =
  {
    annot;
    bin_annot;
    g;
    safe_string;
    short_paths;
    thread;
    w;
    findlib_deps = (match findlib_deps with
      | Some f -> f
      | None -> function `Server | `Client -> []
    );
    ml_files;
    mli_files;
    style;
    dir;
    name;
  }

(******************************************************************************)
(** {2 Low-level Functions} *)
(******************************************************************************)
let outdir = function `Server -> "_server" | `Client -> "_client"

let lib_path host lib =
  (outdir host)/(dirname lib.dir)/lib.name

let pack_path host lib =
  match lib.style with
  | `Pack pack_name -> (outdir host)/(dirname lib.dir)/pack_name

let module_path host lib file =
  (outdir host)/lib.dir/(chop_extension file)

let intf_of_impl x =
  match String.chop_suffix x ~suffix:".ml" with
  | Some base -> base^".mli"
  | None ->
    match String.chop_suffix x ~suffix:".eliom" with
    | Some base -> base^".eliomi"
    | None ->
      failwithf "implementation file %s has unexpected extension" x ()

let intf_exists host lib impl =
  intf_of_impl impl |>
  List.mem ~set:(lib.mli_files host)

let is_shared_impl lib x =
  (List.mem ~set:(lib.ml_files `Client) x)
  && (List.mem ~set:(lib.ml_files `Server) x)

let obj_suffix = function `Byte -> ".cmo" | `Native -> ".cmx"
let lib_suffix =
  function `Byte -> ".cma" | `Native -> ".cmxa" | `JavaScript -> ".js"


(******************************************************************************)
(** {2 Rules} *)
(******************************************************************************)
let mkdir dir =
  let cmd = sprintf "mkdir -p %s" dir in
  match Sys.command cmd with
  | 0 -> ()
  | x -> failwithf "%s: returned with exit code %d" cmd x ()

(* Assure all source files have been copied (or built in case any are
   generated) to build directory. *)
let build_all_src_files build lib =
  List.map [`Server; `Client] ~f:(fun host ->
    [lib.ml_files host; lib.mli_files host]
  ) |>
  List.flatten |>
  List.flatten |>
  List.sort_uniq ~cmp:String.compare |>
  List.map ~f:(fun x -> [lib.dir/x]) |>
  build |> assert_all_outcomes |> ignore

(* Compile all given implementation files in dependency order. Return
   list of generated object files, also in dependency order. *)
let build_impl_files_sorted build host mode ~package impl_files =
  let suffix = obj_suffix mode in
  let obj_files =
    Tools.run_eliomdep_sort ~ppx:() ~package host impl_files |>
    List.map ~f:(fun x -> (outdir host)/(chop_extension x)^suffix)
  in
  printf "building sorted object files: %s\n"
    (String.concat ~sep:"," obj_files)
  ;
  let () = List.iter obj_files ~f:(fun x ->
    match build [[x]] with
    | [Ocamlbuild_plugin.Outcome.Good _] -> ()
    | [Ocamlbuild_plugin.Outcome.Bad exn] -> raise exn
    | _ -> assert false
  )
  in
  obj_files

(* Determine [file]'s dependencies and build them, where [typ]
   controls which type of file you care about the dependencies of. For
   example, given [file = "foo.ml"], you might either want to build
   foo.cmx's or foo.cmo's dependencies. If [file] is an mli, surely
   you want [typ = `cmi]. *)
let build_deps build host typ ~pathI ~package lib file =

  (* Doing this always might be overkill, but it definitely matters in
     some cases. If all source files aren't present, sometimes you get
     incomplete dependencies. *)
  build_all_src_files build lib;

  let base = chop_extension file in
  let suffix = match typ with
    | `type_mli -> ".type_mli"
    | `cmx -> ".cmx"
    | `cmo -> ".cmo"
    | `cmi -> ".cmi"
  in
  let target = (outdir host)/base^suffix in
  let deps =
    Tools.run_eliomdep ~fix320:() ~pathI ~package ~ppx:() host [file]
    |> fun l ->
    match List.Assoc.find l target with
    | None -> []
    | Some deps -> deps
  in
  printf "building dynamic dependencies of %s: %s\n"
    target (String.concat ~sep:"," deps)
  ;
  List.map deps ~f:(fun x -> [x]) |>
  build |> assert_all_outcomes |> ignore

let build_lib_of_host host lib =
  let lib = {
    lib with
    findlib_deps = eliom_tools_workaround lib.findlib_deps;
  }
  in

  (****************************************************************************)
  (* Parameters *)
  (****************************************************************************)
  let annot = lib.annot in
  let bin_annot = lib.bin_annot in
  let g = lib.g in
  let safe_string = lib.safe_string in
  let short_paths = lib.short_paths in
  let thread = lib.thread in
  let w = lib.w in

  let ml_files = lib.ml_files host in
  let mli_files = lib.mli_files host in
  let package = lib.findlib_deps host in
  let for_pack = match lib.style with
    | `Pack x -> Some (String.capitalize x)
  in
  let pathI = [(outdir host)/lib.dir] in
  mkdir ("_build"/(outdir host)/lib.dir);

  (****************************************************************************)
  (* Partially apply several functions *)
  (****************************************************************************)
  let lib_path = lib_path host lib in
  let pack_path = pack_path host lib in
  let module_path_server file = (* needed even for `Client *)
    module_path `Server lib file
  in
  let module_path file = module_path host lib file in
  let intf_exists x = intf_exists host lib x in
  let is_shared_impl x = is_shared_impl lib x in

  let compile ?c ?a ?o ?for_pack ?pack ?linkall mode files =
    match host,mode with
    | `Server,`Byte ->
      Tools.eliomc ?c ?a ?o ?for_pack ?pack ?linkall files
        ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
        ~package ~pathI ~ppx:()
    | `Server,`Native ->
      Tools.eliomopt ?c ?a ?o ?for_pack ?pack ?linkall files
        ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
        ~package ~pathI ~ppx:()
    | `Client,`Byte ->
      Tools.js_of_eliom ?c ?a ?o ?for_pack ?pack ?linkall:None ?linkall files
        ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
        ~package ~pathI ~ppx:()
    | `Client,`Native ->
      failwith "native compilation for client side is not supported"
  in

  let build_deps build mode x =
    build_deps ~package ~pathI:[lib.dir] build host mode lib x
  in

  (****************************************************************************)
  (* Register rules *)
  (****************************************************************************)
  (* .mli/.eliomi -> .cmi *)
  List.iter mli_files ~f:(fun intf ->
    let intf_path = lib.dir/intf in
    let cmi = (module_path intf)^".cmi" in
    Rule.rule ~deps:[intf_path] ~prods:[cmi] (fun _ build ->
      build_deps build `cmi intf_path;
      compile `Byte ~c:() ~o:cmi ?for_pack [intf_path]
    )
  );

  ((* .ml/.eliom -> .type_mli *)
    match host with
    | `Client -> ()
    | `Server ->
      List.iter ml_files ~f:(fun impl ->
        let impl_path = lib.dir/impl in
        let prod = (module_path impl)^".type_mli" in
        Rule.rule ~deps:[impl_path] ~prods:[prod] (fun _ build ->
          build_deps build `type_mli impl_path;

          Tools.eliomc ~infer:() ~o:prod [impl_path]
            ~package:("eliom.ppx.type"::package)
            ?annot ?bin_annot ?g ?safe_string ?short_paths ?thread ?w
            ~pathI ~ppx:()
        )
      )
  );

  (* .ml,.eliom -> .cmo/.cmx, and .cmi if no corresponding .mli/.eliomi *)
  List.iter [`Byte;`Native] ~f:(fun mode ->
    match host,mode with
    | `Client,`Native -> ()
    | _ ->
      let typ = match mode with `Native -> `cmx | `Byte -> `cmo in
      List.iter ml_files ~f:(fun impl ->
        let impl_path = lib.dir/impl in
        let obj = (module_path impl)^(obj_suffix mode) in
        let cmi = (module_path impl)^".cmi" in
        let type_mli = (module_path_server impl)^ ".type_mli" in
        let intf_exists = intf_exists impl in
        let deps =
          let l = if intf_exists then [impl_path; cmi] else [impl_path] in
          if is_shared_impl impl then type_mli::l else l
        in
        let prods = if intf_exists then [obj] else [obj;cmi] in
        Rule.rule ~deps ~prods (fun _ build ->
          build_deps build typ impl_path;
          compile mode ~c:() ~o:obj ?for_pack [lib.dir/impl]
        )
      )
  );

  ((* .cmo*/.cmx* -> packed .cmo/.cmx *)
    List.iter [`Byte; `Native] ~f:(fun mode ->
      match host,mode with
      | `Client,`Native -> ()
      | _ ->
        match lib.style with
        | `Pack _ ->
          let prod = pack_path^(obj_suffix mode) in
          let ml_paths = List.map ml_files ~f:(fun x -> lib.dir/x) in
          Rule.rule ~deps:ml_paths ~prods:[prod] (fun _ build ->
            let deps =
              build_impl_files_sorted build host mode ~package ml_paths
            in
            compile mode ~pack:() ~o:prod deps
          )
    )
  );

  ((* .cmo*/.cmx* -> .cma,.cmxa *)
    List.iter [`Byte;`Native] ~f:(fun mode ->
      match host,mode with
      | `Client,`Native -> ()
      | _ ->
        let ml_lib = lib_path^(lib_suffix mode) in
        match lib.style with
        | `Pack _ ->
          let obj = pack_path^(obj_suffix mode) in
          Rule.rule ~deps:[obj] ~prods:[ml_lib] (fun _ _ ->
            compile mode ~a:() ~linkall:() ~o:ml_lib [obj]
          )
    )
  );

  ((* .cma -> .js *)
    match host with
    | `Server -> ()
    | `Client ->
      let ml = lib_path^(lib_suffix `Byte) in
      let js = lib_path^(lib_suffix `JavaScript) in
      Rule.rule ~deps:[ml] ~prods:[js] (fun _ _ ->
        compile `Byte ~linkall:() ~o:js [ml]
      )
  )

let build_lib lib =
  List.iter [`Server;`Client] ~f:(fun host ->
    build_lib_of_host host lib
  )
