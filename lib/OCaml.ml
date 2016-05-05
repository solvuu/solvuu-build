open Printf
open Ocamlbuild_plugin
open Util

let ocaml compiler mode ?(i=[]) ?o files =
  [
    [Some (A (match compiler with `byte -> "ocamlc" | `native -> "ocamlopt"))];
    [Some (A (match mode with `c -> "-c"))];
    (List.map i ~f:(fun dir -> [Some (A "-I"); Some (A dir)]) |> List.flatten);
    (match o with
     | None -> [None]
     | Some outfile -> [Some (A "-o"); Some (A outfile)]
    );
    List.map files ~f:(fun file -> Some (A file));
  ] |>
  List.flatten |>
  List.filter_map ~f:Fn.id |> fun l ->
  Cmd (S l)

let ocamlc = ocaml `byte
let ocamlopt = ocaml `native

let ocamldep ?modules ?(i=[]) files =
  let cmd =
    [
      ["ocamldep"];
      List.map i ~f:(sprintf "-I %s");
      (match modules with None -> [] | Some () -> ["-modules"]);
    ] |>
    List.flatten |> fun l ->
    String.concat " " (l@files)
  in
  Ocamlbuild_pack.My_unix.run_and_read cmd |>
  String.split ~on:'\n' |>
  List.filter ~f:(function "" -> false | _ -> true) |>
  List.map ~f:(fun line ->
    String.split line ~on:':' |> function
    | target::deps::[] ->
      let target = String.trim target in
      let deps =
        String.split deps ~on:' ' |>
        List.map ~f:String.trim |>
        List.filter ~f:(function "" -> false | _ -> true) |>
        List.sort_uniq String.compare
      in
      target,deps
    | _ -> failwithf "unexpected output from %s, invalid line \"%s\""
             cmd line ()
  ) |>
  List.filter ~f:(function "",[] -> false  | _ -> true) |>
  List.map ~f:(function x,[""] -> x,[] | x,y -> x,y)
