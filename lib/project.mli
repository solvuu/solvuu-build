(** Project information. A project consists of libraries and
    executable applications.
*)

type t = private {
  libs : Item.lib list;
  apps : Item.app list;

  (** [mllib_file dir lib] returns lines of the .mllib file for
      library with short name [lib]. *)
  mllib_file : Item.lib -> string list;

  tags_file : string list;
  merlin_file : string list;
  meta_file : string list;
  install_file : string list;
  ocamlinit_file : string list;
  makefile_rules_file : string list;

  plugin : Ocamlbuild_plugin.hook -> unit;
  dispatch : unit -> unit;
}

val make
  :  ?ocamlinit_postfix:string list
  -> name:string
  -> version:string
  -> items:Item.ts
  -> t
