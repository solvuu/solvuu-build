open Printf
open Ocamlbuild_plugin
open Util

let unit (flag:string) (value:unit option) = match value with
  | None -> None
  | Some () -> Some (A flag)

let atdgen ?j ?j_std ?t file =
  [
    Some (A "atdgen");
    unit "-j" j;
    unit "-j-std" j_std;
    unit "-t" t;
    Some (P file);
  ] |>
  List.filter_map ~f:Fn.id |> fun l ->
  Cmd (S l)
