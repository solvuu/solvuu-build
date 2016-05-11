val failwithf : ('r, unit, string, unit -> 'a) format4 -> 'r

(** [readdir dir] returns the contents of [dir]. Returns empty list if
    [dir] doesn't exist or is not a directory. *)
val readdir : string -> string list

(** Return ".c" files in given dir, with the ".c" extension chopped
    off. *)
val c_units_of_dir : string -> string list

(** Return ".h" files in given dir, with the ".h" extension
    preserved. *)
val h_files_of_dir : string -> string list

(** [clib_file dir lib] returns lines if the clib file corresponding
    to any .c files in [dir], if any. *)
val clib_file : string -> string -> string list option

module Fn : sig
  val id : 'a -> 'a
end

module Char : sig
  include module type of Char
  val is_whitespace : char -> bool
end

module String : sig
  include module type of String

  val for_all : string -> f:(char -> bool) -> bool

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

module Filename : sig
  include module type of Filename

  (** [replace_suffix old new s] replaces suffix [old] in [s] with
      [new]. Returns [None] if [s] doesn't end with suffix [old]. *)
  val replace_suffix : old:string -> new_:string -> string -> string option

  (** Like [replace_suffix] but raise exception if [s] doesn't end
      with [old]. *)
  val replace_suffix_exn : old:string -> new_:string -> string -> string

  (** Make some effort to normalize paths, so that semantically
      equivalent paths will be syntactically equivalent. In particular,
      this helps to workaround an
      {{:https://github.com/ocaml/ocamlbuild/issues/76}ocamlbuild bug}. *)
  val normalize : string -> string
end
