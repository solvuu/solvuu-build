open Printf

module List = struct
  include List
  include ListLabels
end

(** Print .install file to stdout. *)
let make_install_file ~project_name ~pack_name : unit =
  let module_files =
    Sys.readdir "lib" |> Array.to_list |>
    List.filter ~f:(fun x -> Filename.check_suffix x ".ml") |>
    List.map ~f:Filename.chop_extension |>
    List.map ~f:(fun x ->
      ["annot";"cmt";"cmti";"ml";"mli"] |>
      List.map ~f:(fun suffix -> sprintf "?_build/lib/%s.%s" x suffix)
    ) |>
    List.flatten
  in
  let pack_files =
    ["o";"obj";"cmi";"cmo";"cmt";"cmx"] |>
    List.map ~f:(fun suffix ->
      sprintf "?_build/lib/%s.%s" pack_name suffix
    )
  in
  let lib_files =
    ["a";"cma";"cmxa";"cmxs"] |>
    List.map ~f:(fun suffix ->
      sprintf "?_build/lib/%s.%s" project_name suffix
    )
  in
  let all_files =
    ["lib/solvuu.mk";"_build/META"]@lib_files@pack_files@module_files
  in
  let lines =
    ["lib: ["]
    @(List.map all_files ~f:(sprintf "  \"%s\""))
    @["]"]
  in
  List.iter lines ~f:print_endline

let make_META_file ~project_name ~version =
  [
    sprintf "version = \"%s\"" version;
    sprintf "archive(byte) = \"%s.cma\"" project_name;
    sprintf "archive(native) = \"%s.cmxa\"" project_name;
    sprintf "exists_if = \"%s.cma\"" project_name;
    sprintf "requires = \"findlib ocamlbuild ocamlgraph\"";
  ] |>
  List.iter ~f:print_endline

;;
let () = match Sys.argv with
  | [|_; "install"; project_name; pack_name|] ->
    make_install_file ~project_name ~pack_name
  | [|_; "META"; project_name; version|] ->
    make_META_file ~project_name ~version
  | _ ->
    failwith "make-helper.ml: invalid usage"
