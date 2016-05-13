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

let atdgen_t_rule ?(dep="%.atd") ?j_std () =
  let open Filename in
  let base =
    if check_suffix dep ".atd"
    then chop_suffix dep ".atd"
    else failwithf "atdgen_t_rule: invalid ~dep = %s" dep ()
  in
  let deps = [dep] in
  let prods = [sprintf "%s_t.ml" base; sprintf "%s_t.mli" base] in
  let name = Rule.name ~deps ~prods in
  rule name ~deps ~prods (fun env _ -> atdgen ~t:() ?j_std (env dep))

let atdgen_j_rule ?(dep="%.atd") ?j_std () =
  let open Filename in
  let base =
    if check_suffix dep ".atd"
    then chop_suffix dep ".atd"
    else failwithf "atdgen_j_rule: invalid ~dep = %s" dep ()
  in
  let deps = [dep] in
  let prods = [sprintf "%s_j.ml" base; sprintf "%s_j.mli" base] in
  let name = Rule.name ~deps ~prods in
  rule name ~deps ~prods (fun env _ -> atdgen ~j:() ?j_std (env dep))
