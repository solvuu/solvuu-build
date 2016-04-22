open Printf

module List = struct
  include List
  include ListLabels
end

let modules =
  Sys.readdir "lib" |> Array.to_list
  |> List.filter ~f:(fun x -> Filename.check_suffix x ".mli")
  |> List.map ~f:Filename.chop_extension
  |> List.map ~f:(sprintf "lib/%s")

let suffixes = [
  "a";"annot";"cma";"cmi";"cmo";"cmt";"cmti";
  "cmx";"cmxa";"cmxs";"dll";"o";"so"
]

let lib_lines =
  [
    "lib/solvuu.mk";
    "_build/META";
  ]
  @(
    List.map modules ~f:(fun m ->
      List.map suffixes ~f:(fun suffix ->
        sprintf "?_build/%s.%s" m suffix
      )
    )
    |> List.concat
  )

let install_file =
  ["lib: ["]
  @(List.map lib_lines ~f:(sprintf "  \"%s\""))
  @["]"]

let () =
  List.iter install_file ~f:print_endline
