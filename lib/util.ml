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

  let for_all = Core.String.for_all

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

  let last l = match List.rev l with
    | [] -> None
    | x::_ -> Some x

  let last_exn l = List.rev l |> List.hd

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
    String.concat "/" |> function
    | "" -> "."
    | x -> x

end

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
