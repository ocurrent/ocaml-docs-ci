let with_tmp_dir f =
  let dir = Bos.OS.Dir.tmp "day11_test_%s" |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Bos.OS.Dir.delete ~recurse:true dir |> ignore)
    (fun () -> f dir)

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f ~sw (env :> Eio_unix.Stdenv.base)

let ok_or_fail msg = function
  | Ok v -> v
  | Error (`Msg e) -> Alcotest.fail (msg ^ ": " ^ e)

let mkdir path = Bos.OS.Dir.create ~path:true path |> Result.get_ok |> ignore
let write_file path contents = Bos.OS.File.write path contents |> Result.get_ok

let is_integration () =
  try Sys.getenv "DAY11_INTEGRATION" = "true" with Not_found -> false

let sudo_available () = Sys.command "sudo -n true >/dev/null 2>&1" = 0

let opam_repository () =
  match Sys.getenv_opt "OPAM_REPOSITORY" with
  | Some path when Sys.file_exists path -> path
  | Some path ->
      Printf.printf "OPAM_REPOSITORY=%s does not exist\n%!" path;
      Alcotest.skip ()
  | None ->
      Printf.printf "OPAM_REPOSITORY not set\n%!";
      Alcotest.skip ()
