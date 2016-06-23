(* Developers: functions in this module should be provided in roughly
   alphabetical order, both in the ml and mli. *)
open Ocamlbuild_plugin
open Printf
open Util
open Util.Spec

let cp ?f src dst =
  [
    [Some (A "cp")];
    unit "-f" f;
    [Some (A src)];
    [Some (A dst)];
  ] |>
  specs_to_command

let cp_rule ?f ~dep ~prod =
  Rule.rule ~deps:[dep] ~prods:[prod] (fun _ _ -> cp ?f dep prod)

let git_last_commit () =
  if Sys.file_exists ".git" then
    Some (
      Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD"
      |> fun x -> String.sub x 0 (String.length x - 1)
    )
  else
    None

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

let m4_rule ?_D ?dep ?(prod="%.ml") () =
  let open Filename in
  let dep = match dep with
    | Some x -> x
    | None -> sprintf "%s.m4" prod
  in
  Rule.rule ~deps:[dep] ~prods:[prod] (fun env _ ->
    m4 ?_D ~infile:(env dep) ~outfile:(env prod)
  )
