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

(** Raise exception if any item in given list is [exn], else return
    the list of all good pathnames. Note the input argument is the
    output type of Ocamlbuild's [builder]. *)
val assert_all_outcomes :
  (Ocamlbuild_plugin.Pathname.t, exn) Ocamlbuild_plugin.Outcome.t list ->
  Ocamlbuild_plugin.Pathname.t list

module Fn : sig
  val id : 'a -> 'a
end

module Char : sig
  include module type of Char
  val is_whitespace : char -> bool
end

module String : sig
  include module type of String

  val concat : sep:string -> string list -> string

  val for_all : string -> f:(char -> bool) -> bool

  val hash : string -> int
  val equal : string -> string -> bool
  val split : string -> on:char -> string list

  val is_suffix : t -> suffix:t -> bool
  val is_prefix : string -> prefix:string -> bool

  val drop_suffix : t -> int -> t
  val drop_prefix : t -> int -> t

  val chop_suffix_exn : t -> suffix:t -> t
  val chop_prefix_exn : t -> prefix:t -> t
  val chop_suffix : t -> suffix:t -> t option
  val chop_prefix : t -> prefix:t -> t option

  val suffix : t -> int -> t
  val prefix : t -> int -> t

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

  val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list

  (** [is_uniq cmp l] returns true if every item in list [l] is unique
      according to [cmp]. *)
  val is_uniq : cmp:('a -> 'a -> int) -> 'a list -> bool

  val last : 'a list -> 'a option
  val last_exn : 'a list -> 'a

  val intersperse : 'a list -> sep:'a -> 'a list

  module Assoc : sig
    val find : ('a * 'b) list -> 'a -> 'b option
  end

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

(** Functions related to Ocamlbuild's rules. *)
module Rule : sig
  open Ocamlbuild_plugin

  (** Generate a name from given [deps] and [prods]. This saves the
      trouble of having to think of a name manually, and makes rule names
      systematic. *)
  val name : deps:string list -> prods:string list -> string

  (** Like Ocamlbuild's [rule] function, except:

      - [name]: By default it is computed by the [name] function
        above.

      - [dep],[prod]: Not provided as you can always use [deps],
        [prods].

      - [deps], [prods]: All items are passed through
        [Filename.normalize].
  *)
  val rule :
    ?name:string ->
    ?deps:string list ->
    ?prods:string list ->
    ?stamp:string ->
    ?insert:[`top | `before of string | `after of string | `bottom] ->
    ?doc:string ->
    action ->
    unit

end

(******************************************************************************)
(** Command.spec helper functions *)
(******************************************************************************)
module Spec : sig
  open Ocamlbuild_plugin

  val string :
    delim:[`Space | `None | `Equal] ->
    string -> string option -> spec option list

  val string_list :
    delim:[`Space | `None | `Equal] ->
    string -> string list option -> spec option list

  val unit : string -> unit option -> spec option list

  val int :
    delim:[`Space | `None | `Equal] ->
    string -> int option -> spec option list

  val specs_to_command : spec option list list -> Command.t

  (** Return a spec from given command. Raise exception if not
      possible. *)
  val spec_of_command : Command.t -> spec
end
