open Printf
open Util

type pkg = string

(* This is necessary to query findlib *)
let () = Findlib.init ()

let all_packages =
  Fl_package_base.list_packages ()

let installed x = List.mem x ~set:all_packages

let to_use_tag x =
  String.map (function '.' -> '_' | c -> c) x
  |> sprintf "use_%s"

let to_path x =
  String.split x ~on:'.'


(******************************************************************************)
(** {2 Graph Operations} *)
(******************************************************************************)
module T = struct
  type nonrec t = pkg
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Graph = struct
  include Graph.Persistent.Digraph.Concrete(T)

  module Dfs = Graph.Traverse.Dfs(
    Graph.Persistent.Digraph.Concrete(T)
  )

  module Topological = struct
    include Graph.Topological.Make(
        Graph.Persistent.Digraph.Concrete(T)
      )

    let sort g = fold (fun x l -> x::l) g []
  end

  module Gml = struct
    include Graph.Gml.Print
        (Graph.Persistent.Digraph.Concrete(T))
        (struct
          let node x  = [x, Graph.Gml.List []]
          let edge () = ["", Graph.Gml.List []]
        end)

    let print t =
      let buf = Buffer.create 30 in
      let fmt = Format.formatter_of_buffer buf in
      print fmt t;
      Buffer.contents buf

  end

  let roots g =
    fold_vertex
      (fun x accum -> if in_degree g x = 0 then x::accum else accum)
      g
      []

  let of_list xs =
    let concat (prefix:string list) (x:string) =
      String.concat "." (prefix@[x])
    in
    if not (List.is_uniq ~cmp:T.compare xs) then
      failwith "multiple findlib packages have an identical name"
    else
      let g =
        List.fold_left xs ~init:empty ~f:(fun g x ->
          let rec loop g prefix parts = match parts with
            | [] -> g
            | x::[] -> (
                let x' = concat prefix x in
                add_vertex g x'
              )
            | x::y::l -> (
                let x' = concat prefix x in
                let y' = concat (prefix@[x]) y in
                loop (add_edge g x' y') (prefix@[x]) (y::l)
              )
          in
          loop g [] (to_path x)
        )
      in
      if Dfs.has_cycle g then
        failwith "findlib package names form a cycle"
      else
        g

end
