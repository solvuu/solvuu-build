open Printf
open Ocamlbuild_plugin
open Util

let m4 ?(_D=[]) ~infile ~outfile =
  [
    [A "m4"];
    (
      List.map _D ~f:(fun (x,y) ->
        let value = match y with
          | None -> x
          | Some y -> sprintf "%s=%s" x y
        in
        [A "-D"; A value]
      ) |> List.flatten
    );
    [P infile];
    [Sh ">"];
    [P outfile];
  ] |>
  List.flatten |> fun l ->
  Cmd (S l)
