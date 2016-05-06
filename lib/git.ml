open Util

let last_commit () =
  if Sys.file_exists ".git" then
    Some (
      Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD"
      |> fun x -> String.sub x 0 (String.length x - 1)
    )
  else
    None
