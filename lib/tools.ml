(** Command line tools. We provided convenient constructors for
    command line calls as needed by ocamlbuild. Each function
    corresponds to a Unix command of the same (or similar) name with
    flags mapped to labeled arguments.

    Most functions return a value of type [Command.t], which is what
    you need to define ocamlbuild rules. Sometimes we provide a
    function to register a rule directly. For example, {!ocamllex}
    constructs a command and {!ocamllex_rule} registers a
    corresponding rule that takes care of defining the dependency and
    target for you. Finally, sometimes you want to run a tool right
    away, as opposed to registering it to be run later. We provide
    some convenience functions for this too, e.g. {!run_ocamldep}
    immediately runs ocamldep, captures its output, and returns the
    parsed result.

    Command line flags are mapped to labeled arguments with the exact
    same name whenver possible, e.g. ocamlc's [-c] flag is represented
    by a [~c] argument to the {!ocamlc} function provided
    here. Sometimes this is not possible and we resolve the mapping as
    follows:

    - The flag is an OCaml keyword, in which case we suffix with an
      underscore. For example, ocamlc takes an [-open] flag, which is
      mapped to an [~open_] argument here.

    - The flag begins with a capital letter, in which case we choose
      an alternate name that represents the meaning of the flag. A
      commonly occuring case of this is the [-I] flag, which we map to
      [~pathI]. Other cases are documented where they occur.

    Command line tools sometimes allow a flag to be passed multiple
    times. We represent this by making the type of the corresponding
    argument a list. For example, ocamlc's [-open] argument takes a
    string value, and this can be given any number of times. Thus the
    [~open_] argument is of type [string list].
*)

include Unix_tools
include OCaml_tools
