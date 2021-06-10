let pool = Current.Pool.create ~label:"git-live" 1

let folder repo = Fpath.(Current.state_dir "live" / "repos" / Git_store.string_of_repository repo)

let to_cmd t = Bos.Cmd.to_list t |> Array.of_list

let write_file file content =
  let open Lwt.Syntax in
  let* file = Lwt_io.open_file ~mode:Output (Fpath.to_string file) in
  let* () = Lwt_io.write file content in
  Lwt_io.close file

module Op = struct
  type t = Config.Ssh.t

  let id = "publish-live"

  let auto_cancel = false

  let pp f ((r, k), v) =
    Fmt.pf f "%s: Publish %d commits to %s" (Git_store.string_of_repository r) (List.length v) k

  module Key = struct
    type t = Git_store.repository * string

    let digest (repo, branch) = Git_store.string_of_repository repo ^ ":" ^ branch
  end

  module Value = struct
    type t = (Git_store.Branch.t * [ `Commit of string ]) list

    let digest t =
      t
      |> List.rev_map (fun (b, `Commit c) -> Git_store.Branch.to_string b ^ ":" ^ c)
      |> String.concat "\n" |> Digest.string |> Digest.to_hex
  end

  module Outcome = Current.Unit

  let write_info ~folder (b, `Commit c) =
    let open Lwt.Syntax in
    let file = Fpath.(folder / Git_store.Branch.to_string b) in
    write_file file c

  let publish ssh job (repo, branch) commits =
    let exec ?cwd cmd = Current.Process.exec ~cancellable:false ~job ?cwd ("", to_cmd cmd) in
    let ( let** ) = Lwt_result.bind in
    let open Lwt.Syntax in
    let* () = Current.Job.start_with ~pool ~level:Average job in
    let folder = folder repo in
    (* clone repository if it doesn't exist (but only the main branch so nothing is setup)*)
    let** () =
      if Bos.OS.Path.exists folder |> Result.get_ok then Lwt.return_ok ()
      else exec (Git_store.Local.clone ~branch:"main" ~directory:folder repo ssh)
    in
    (* checkout the correct branch *)
    let** () = exec ~cwd:folder (Git_store.Local.checkout_or_create ~branch ssh) in
    (* empty folder *)
    let** () = exec ~cwd:folder (Bos.Cmd.(v "bash" % "-c" % "rm * || echo 'nothing to remove'")) in
    (* write commit info *)
    let* () = Lwt_list.iter_p (write_info ~folder) commits in
    (* sync changes *)
    let** () = exec ~cwd:folder Bos.Cmd.(v "bash" % "-c" % "git add --all && (git diff HEAD --exit-code --quiet || git commit -m 'update')") in
    exec (Git_store.Local.push ~directory:folder ssh)
end

module Cache = Current_cache.Output (Op)

let publish ~ssh ~repository ~branch ~commits =
  let open Current.Syntax in
  Current.component "live commits"
  |> let> branch = branch and> commits = commits in
     Cache.set ssh (repository, branch) commits

module OpSetLiveTo = struct
  type t = Config.Ssh.t * string

  let id = "set-live-to"

  let auto_cancel = false

  let pp f (k, v) = Fmt.pf f "Set live branch of %s to %s" (Git_store.string_of_repository k) v

  module Key = struct
    type t = Git_store.repository

    let digest = Git_store.string_of_repository
  end

  module Value = Current.String
  module Outcome = Current.Unit

  let publish (ssh, message) job repo branch =
    let exec ?cwd cmd = Current.Process.exec ~cancellable:false ~job ?cwd ("", to_cmd cmd) in
    let ( let** ) = Lwt_result.bind in
    let open Lwt.Syntax in
    let* () = Current.Job.start_with ~pool ~level:Dangerous job in
    let folder = folder repo in
    (* clone repository if it doesn't exist (but only the main branch so nothing is setup)*)
    let** () =
      if Bos.OS.Path.exists folder |> Result.get_ok then Lwt.return_ok ()
      else exec (Git_store.Local.clone ~branch:"main" ~directory:folder repo ssh)
    in
    (* checkout the correct branch *)
    let** () = exec ~cwd:folder (Git_store.Local.checkout_or_create ~branch:"live" ssh) in
    (* write which branch is the live one *)
    let* () = write_file Fpath.(folder / "live") branch in
    (* sync changes *)
    let** () = exec ~cwd:folder Bos.Cmd.(v "git" % "add" % "--all") in
    let** () = exec ~cwd:folder Bos.Cmd.(v "git" % "commit" % "-m" % message % "--allow-empty") in
    exec (Git_store.Local.push ~directory:folder ssh)
end

module CacheSetLiveTo = Current_cache.Output (OpSetLiveTo)

let set_live_to ~ssh ~repository ~branch ~message =
  let open Current.Syntax in
  Current.component "set live to"
  |> let> branch = branch and> message = message in
     CacheSetLiveTo.set (ssh, message) repository branch
