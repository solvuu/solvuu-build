open Printf

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

    let intersperse t ~sep =
      match t with
      | [] -> []
      | x :: xs -> x :: List.fold_right (fun y acc -> sep :: y :: acc) xs []

  end

  module Char = struct
    let equal = (=)
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

    let is_suffix_gen =
      let rec loop s ~suffix ~char_equal idx_suff idx =
        idx_suff < 0
        || ((char_equal suffix.[idx_suff] s.[idx])
            && loop s ~suffix ~char_equal (idx_suff - 1) (idx - 1))
      in
      fun s ~suffix ~char_equal ->
        let len = String.length s in
        let len_suffix = String.length suffix in
        len >= len_suffix
        && loop s ~suffix ~char_equal (len_suffix - 1) (len - 1)
    ;;

    let is_prefix_gen =
      let rec loop s ~prefix ~char_equal i =
        i < 0
        || ((char_equal prefix.[i] s.[i])
            && loop s ~prefix ~char_equal (i - 1))
      in
      fun s ~prefix ~char_equal ->
        let prefix_len = String.length prefix in
        String.length s >= prefix_len
        && loop s ~prefix ~char_equal (prefix_len - 1)
    ;;

    let is_suffix s ~suffix = is_suffix_gen s ~suffix ~char_equal:Char.equal
    let is_prefix s ~prefix = is_prefix_gen s ~prefix ~char_equal:Char.equal

    let wrap_sub_n t n ~name ~pos ~len ~on_error =
      if n < 0 then
        invalid_arg (name ^ " expecting nonnegative argument")
      else
        try
          String.sub t ~pos ~len
        with _ ->
          on_error

    let drop_prefix t n =
      wrap_sub_n ~name:"drop_prefix" t n ~pos:n
        ~len:(String.length t - n) ~on_error:""
    let drop_suffix t n =
      wrap_sub_n ~name:"drop_suffix" t n ~pos:0
        ~len:(String.length t - n) ~on_error:""
    let prefix t n =
      wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~on_error:t
    let suffix t n =
      wrap_sub_n ~name:"suffix" t n ~pos:(String.length t - n)
        ~len:n ~on_error:t

    let chop_prefix s ~prefix =
      if is_prefix s ~prefix then
        Some (drop_prefix s (String.length prefix))
      else
        None

    let chop_prefix_exn s ~prefix =
      match chop_prefix s ~prefix with
      | Some str -> str
      | None ->
        raise (Invalid_argument
                 (Printf.sprintf "chop_prefix_exn %S %S" s prefix))

    let chop_suffix s ~suffix =
      if is_suffix s ~suffix then
        Some (drop_suffix s (String.length suffix))
      else
        None

    let chop_suffix_exn s ~suffix =
      match chop_suffix s ~suffix with
      | Some str -> str
      | None ->
        raise (Invalid_argument
                 (Printf.sprintf "chop_suffix_exn %S %S" s suffix))

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

  let is_suffix = Core.String.is_suffix
  let is_prefix = Core.String.is_prefix
  let drop_suffix = Core.String.drop_suffix
  let drop_prefix = Core.String.drop_prefix
  let chop_suffix_exn = Core.String.chop_suffix_exn
  let chop_prefix_exn = Core.String.chop_prefix_exn
  let chop_suffix = Core.String.chop_suffix
  let chop_prefix = Core.String.chop_prefix
  let suffix = Core.String.suffix
  let prefix = Core.String.prefix

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

  let intersperse = Core.List.intersperse

  module Assoc = struct
    let find l x = try Some (assoc x l) with Not_found -> None
  end

end

module Option = struct
  let map x ~f = match x with None -> None | Some y -> Some (f y)

  let compare cmp x y =
    match x,y with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some x, Some y -> cmp x y

  let is_some = function Some _ -> true | None -> false
  let is_none = function Some _ -> false | None -> true

  let value_exn = function
    | Some x -> x
    | None -> failwith "value_exn: got None"

end

module Unit = struct
  let compare () () = 0
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

module Sys = struct
  include Sys

  let sub_dirs ?(depth=1) root =
    let root = Filename.normalize root in
    let (/) = Filename.concat in
    let immediate_sub_dirs dir =
      Sys.readdir dir |> Array.to_list |>
      List.filter_map ~f:(fun x ->
        let x = dir/x in
        match file_exists x && is_directory x with
        | true -> Some x
        | false -> None
      )
    in
    let rec loop depth accum dirs =
      let immediate_sub_dirs =
        List.map dirs ~f:immediate_sub_dirs |>
        List.flatten
      in
      let accum = accum@immediate_sub_dirs in
      if depth <= 1
      then accum
      else loop (depth-1) accum immediate_sub_dirs
    in
    loop depth [] [root] |>
    List.map ~f:(String.chop_prefix_exn ~prefix:(sprintf "%s/" root))

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
