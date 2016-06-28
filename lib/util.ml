open Printf
let (/) = Ocamlbuild_plugin.(/)

let failwithf fmt = ksprintf (fun s () -> failwith s) fmt

(* Code in this module copied from the Core suite
   [https://github.com/janestreet/]. See there for license
   information. *)
module Core = struct
  module List = struct
    let rev_filter_map l ~f =
      let rec loop l accum =
        match l with
        | [] -> accum
        | hd :: tl ->
          match f hd with
          | Some x -> loop tl (x :: accum)
          | None   -> loop tl accum
      in
      loop l []

    let filter_map l ~f = List.rev (rev_filter_map l ~f)
  end

  module String = struct
    module String = BytesLabels

    let for_all =
      let rec loop s i ~len ~f =
        i = len || (f s.[i] && loop s (i + 1) ~len ~f)
      in
      fun s ~f -> loop s 0 ~len:(String.length s) ~f

    let rec char_list_mem l (c:char) =
      match l with
      | [] -> false
      | hd::tl -> hd = c || char_list_mem tl c

    let split_gen str ~on =
      let is_delim =
        match on with
        | `char c' -> (fun c -> c = c')
        | `char_list l -> (fun c -> char_list_mem l c)
      in
      let len = String.length str in
      let rec loop acc last_pos pos =
        if pos = -1 then
          String.sub str ~pos:0 ~len:last_pos :: acc
        else
        if is_delim str.[pos] then
          let pos1 = pos + 1 in
          let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
          loop (sub_str :: acc) pos (pos - 1)
        else loop acc last_pos (pos - 1)
      in
      loop [] len (len - 1)

    let split str ~on = split_gen str ~on:(`char on) ;;
  end
end

module Fn = struct
  let id x = x
end

module Char = struct
  include Char

  let is_whitespace = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false

end

module String = struct
  include String

  let concat = StringLabels.concat
  let for_all = Core.String.for_all

  let hash = Hashtbl.hash
  let equal = ( = )
  let split = Core.String.split

  let is_prefix x ~prefix =
    let n = String.length prefix in
    try ignore (String.sub x 0 n); true
    with Invalid_argument _ -> false

  module Map = struct
    include Map.Make(String)

    let to_list t =
      fold (fun key a accum -> (key,a)::accum) t [] |>
      List.rev
  end

  module Set = Set.Make(String)
end

module List = struct
  include List
  include ListLabels

  let filter_map = Core.List.filter_map

  let diff a b =
    filter a ~f:(fun x -> not (mem x ~set:b))

  (* ListLabels.sort_uniq was introduced in OCaml 4.03.0. Prior
     versions (since 4.02.0) had only List.sort_uniq without a labeled
     argument. The implementation below, including turning off warning
     6, allows us to compile this code on all OCaml >= 4.02.0. *)
  let sort_uniq ~cmp l =
    sort_uniq cmp l [@@ocaml.warning "-6"]

  let is_uniq ~cmp (l : 'a list) : bool =
    let m = length l in
    let n = length (sort_uniq ~cmp l) in
    m = n

  let last l = match List.rev l with
    | [] -> None
    | x::_ -> Some x

  let last_exn l = List.rev l |> List.hd

  module Assoc = struct
    let find l x = try Some (assoc x l) with Not_found -> None
  end

end

module Filename = struct
  include Filename

  let replace_suffix ~old ~new_ s =
    match check_suffix s old with
    | false -> None
    | true -> Some (sprintf "%s%s" (chop_suffix s old) new_)

  let replace_suffix_exn ~old ~new_ s =
    match replace_suffix ~old ~new_ s with
    | Some x -> x
    | None -> failwithf "%s doesn't end with %s" s old ()

  let normalize x =
    String.split ~on:'/' x |>
    List.filter_map ~f:(function "." | "" -> None | x -> Some x) |>
    String.concat ~sep:"/" |> function
    | "" -> "."
    | x -> x

end

module Rule = struct

  let name0 ~deps ~prods =
    sprintf "%s -> %s"
      (String.concat ~sep:"," deps)
      (String.concat ~sep:"," prods)

  let name = name0

  let rule ?name ?deps ?prods ?stamp ?insert ?doc action =
    let normalize_l = function
      | None -> None
      | Some l -> Some (List.map l ~f:Filename.normalize)
    in
    let deps = normalize_l deps in
    let prods = normalize_l prods in
    let name = match name with
      | Some x -> x
      | None ->
        let deps = match deps with None -> [] | Some x -> x in
        let stamp = match stamp with None -> [] | Some x -> [x] in
        let prods = (match prods with None -> [] | Some x -> x)@stamp in
        name0 ~deps ~prods
    in
    Ocamlbuild_plugin.rule name ?deps ?prods ?stamp ?insert ?doc action

end

module Spec = struct
  module List0 = List
  open Ocamlbuild_plugin
  module List = List0

  let string ~delim (flag:string) (value:string option) =
    match value with
    | None -> [None]
    | Some value ->
      match delim with
      | `Space -> [Some (A flag); Some (A value)]
      | `None -> [Some (A (flag ^ value))]
      | `Equal -> [Some (A (sprintf "%s=%s" flag value))]

  let string_list ~delim (flag:string) (value:string list option) =
    match value with
    | None -> [None]
    | Some l ->
      List.map l ~f:(fun x ->
        string ~delim flag (Some x)
      ) |>
      List.flatten

  let unit (flag:string) (value:unit option) = match value with
    | None -> [None]
    | Some () -> [Some (A flag)]

  let int ~delim (flag:string) (value:int option) = match value with
    | None -> [None]
    | Some value ->
      match delim with
      | `Space -> [Some (A flag); Some (A (string_of_int value))]
      | `None -> [Some (A (flag ^ (string_of_int value)))]
      | `Equal -> [Some (A (sprintf "%s=%d" flag value))]

  let specs_to_command (specs : spec option list list) : Command.t =
    List.flatten specs
    |> List.filter_map ~f:Fn.id
    |> fun l -> Cmd (S l)

  let spec_of_command (x:Command.t) : Command.spec =
    match x with
    | Command.Cmd x -> x
    | Command.Seq _ -> failwith "cannot extract spec from sequence of commands"
    | Command.Echo _ -> failwith "cannot convert Command.Echo to spec"
    | Command.Nop -> failwith "cannot convert Command.Nop to spec"

end

let assert_all_outcomes l =
  let rec loop accum = function
    | [] -> accum
    | (Ocamlbuild_plugin.Outcome.Good x)::l -> loop (x::accum) l
    | (Ocamlbuild_plugin.Outcome.Bad exn)::_ -> raise exn
  in
  loop [] l |>
  List.rev

let readdir dir : string list =
  match Sys.file_exists dir && Sys.is_directory dir with
  | false -> []
  | true -> (Sys.readdir dir |> Array.to_list)

let c_units_of_dir dir : string list =
  readdir dir
  |> List.filter ~f:(fun p -> Filename.check_suffix p ".c")
  |> List.map ~f:Filename.chop_extension

let h_files_of_dir dir : string list =
  readdir dir
  |> List.filter ~f:(fun p -> Filename.check_suffix p ".h")

let clib_file dir lib =
  let path = dir / lib in
  match c_units_of_dir path with
  | [] -> None
  | xs ->
    Some (List.map xs ~f:(fun x -> lib ^ "/" ^ x ^ ".o"))
