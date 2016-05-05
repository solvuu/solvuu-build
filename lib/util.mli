val failwithf : ('r, unit, string, unit -> 'a) format4 -> 'r

(** [readdir dir] returns the contents of [dir]. Returns empty list if
    [dir] doesn't exist or is not a directory. *)
val readdir : string -> string list

(** Given a filename, return the 1 or more module names. Examples:

    - "foo.ml" yields "Foo"
    - "foo.mli" yields "Foo"

    - "foo.atd" yields ["Foo_t"; "Foo_j"], i.e. we assume atdgen is
       used in a specific way.
*)
val modules_of_file : string -> string list

(** [modules_of_dir dir] applies [modules_of_file] to every file in
    [dir] and returns the concatenated result. *)
val modules_of_dir : string -> string list

(** Return ".c" files in given dir, with the ".c" extension chopped
    off. *)
val c_units_of_dir : string -> string list

(** Return ".h" files in given dir, with the ".h" extension
    preserved. *)
val h_files_of_dir : string -> string list

(** Generate list of modules from contents of given [dir], and return
    corresponding mlpack file. *)
val mlpack_file : string -> string list

(** [clib_file dir lib] returns lines if the clib file corresponding
    to any .c files in [dir], if any. *)
val clib_file : string -> string -> string list option

module Fn : sig
  val id : 'a -> 'a
end

module String : sig
  include module type of String
  val hash : string -> int
  val equal : string -> string -> bool
  val split : string -> on:char -> string list

  module Map : sig
    include module type of Map.Make(String)
    val to_list : 'a t -> (string * 'a) list
  end

  module Set : module type of Set.Make(String)
end

module List : sig
  include module type of List
  include module type of ListLabels

  val filter_map : 'a list -> f:('a -> 'b option) -> 'b list

  (** [diff a b] returns all items in [a] that are not in [b]. *)
  val diff : 'a list -> 'a list -> 'a list

  (** [is_uniq cmp l] returns true if every item in list [l] is unique
      according to [cmp]. *)
  val is_uniq : cmp:('a -> 'a -> int) -> 'a list -> bool

end
