open Printf
open Util

let m4 ?(_D=[]) ~infile ~outfile =
  let open Ocamlbuild_plugin in
  let open Util in
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

let m4_rule ?_D ?dep ?(prod="%.ml") () =
  let open Filename in
  let dep = match dep with
    | Some x -> x
    | None -> sprintf "%s.m4" prod
  in
  Rule.rule ~deps:[dep] ~prods:[prod] (fun env _ ->
    m4 ?_D ~infile:(env dep) ~outfile:(env prod)
  )
