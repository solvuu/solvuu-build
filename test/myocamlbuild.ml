open Solvuu_build

include Make(struct
  let name = "this_is"
  let version = "dev"

  let info = Info.of_list [
    {
      Info.name = `Lib "unix";
      libs = [];
      pkgs = ["core"];
      build_if = [];
    };

    {
      Info.name = `Lib "async";
      libs = ["unix"];
      pkgs = ["async"];
      build_if = [`Pkgs_installed];
    };

    {
      Info.name = `Lib "lwt";
      libs = ["unix"];
      pkgs = ["lwt.preemptive"];
      build_if = [`Pkgs_installed];
    };

    {
      Info.name = `App "app";
      libs = ["unix" ; "lwt"];
      pkgs = [];
      build_if = [];
    };
]

  let ocamlinit_postfix = []

end)

let () = dispatch()
