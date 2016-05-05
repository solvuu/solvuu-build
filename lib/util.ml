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

module String = struct
  include String
  let hash = Hashtbl.hash
  let equal = ( = )
  let split = Core.String.split

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

  let is_uniq ~cmp (l : 'a list) : bool =
    let m = length l in
    let n = length (sort_uniq cmp l) in
    m = n
end

let readdir dir : string list =
  match Sys.file_exists dir && Sys.is_directory dir with
  | false -> []
  | true -> (Sys.readdir dir |> Array.to_list)

let modules_of_file filename : string list =
  List.fold_left [".ml"; ".mli"; ".ml.m4"; ".mll"; ".mly"; ".atd"]
    ~init:[] ~f:(fun accum suffix ->
      let new_items = match suffix with
        | ".atd" -> (
            if Filename.check_suffix filename suffix then (
              Filename.chop_suffix filename suffix
              |> fun x -> [x^"_j"; x^"_t"]
            )
            else
              []
          )
        | _ -> (
            if Filename.check_suffix filename suffix then
              [Filename.chop_suffix filename suffix]
            else
              []
          )
      in
      new_items@accum
    )
  |> List.map ~f:String.capitalize

let modules_of_dir dir : string list =
  readdir dir
  |> List.map ~f:modules_of_file
  |> List.concat
  |> List.sort_uniq compare

let c_units_of_dir dir : string list =
  readdir dir
  |> List.filter ~f:(fun p -> Filename.check_suffix p ".c")
  |> List.map ~f:Filename.chop_extension

let h_files_of_dir dir : string list =
  readdir dir
  |> List.filter ~f:(fun p -> Filename.check_suffix p ".h")

let mlpack_file dir : string list =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    failwithf "cannot create mlpack file for dir %s" dir ()
  else (
    modules_of_dir dir
    |> List.map ~f:(fun x -> dir/x)
  )

let clib_file dir lib =
  let path = dir / lib in
  match c_units_of_dir path with
  | [] -> None
  | xs ->
    Some (List.map xs ~f:(fun x -> lib ^ "/" ^ x ^ ".o"))
