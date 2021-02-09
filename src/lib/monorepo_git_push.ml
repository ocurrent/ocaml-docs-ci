let pool = Current.Pool.create ~label:"git checkout" 8

module GitPush = struct
  type t = No_context

  module Key = struct
    type t = { remote_push : string; remote_pull : string; branch : string }

    let digest t = t.remote_pull ^ "/" ^ t.remote_push ^ "#" ^ t.branch
  end

  module Value = struct
    type t = Current_git.Commit.t list

    let digest t =
      let json = `List (List.map (fun x -> `String (Current_git.Commit.hash x)) t) in
      Yojson.to_string json
  end

  module Outcome = struct
    type t = Current_git.Commit_id.t

    type info = { repo : string; hash : string; gref : string } [@@deriving yojson]

    let t_of_info { repo; gref; hash } = Current_git.Commit_id.v ~repo ~gref ~hash

    let info_of_t t =
      let open Current_git.Commit_id in
      { repo = repo t; hash = hash t; gref = gref t }

    let marshal t = t |> info_of_t |> info_to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> info_of_yojson |> Result.get_ok |> t_of_info
  end

  let auto_cancel = true

  let pp f _ = Fmt.string f "Monorepo git push"

  let id = "mirage-ci-monorepo-git-push"

  let fold_ok res =
    Lwt.map (fun value ->
        match (res, value) with
        | Ok a, Ok b -> Ok (b :: a)
        | Ok _, Error b -> Error b
        | Error (`Msg a), Error (`Msg b) -> Error (`Msg (a ^ "\n" ^ b))
        | Error a, _ -> Error a)

  let publish No_context job { Key.remote_pull; remote_push; branch } commits =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* () = Current.Job.start ~level:Average job in
    Current.Process.with_tmpdir @@ fun tmpdir ->
    let cmd cmd = Current.Process.exec ~cwd:tmpdir ~cancellable:true ~job ("", cmd) in
    let read cmd = Current.Process.check_output ~cwd:tmpdir ~cancellable:true ~job ("", cmd) in
    let** () = cmd [| "git"; "init" |] in
    let** () = cmd [| "git"; "remote"; "add"; "origin"; remote_push |] in
    let** () = cmd [| "git"; "checkout"; "-b"; branch |] in
    let** () = cmd [| "git"; "submodule"; "init" |] in
    let** _ =
      Lwt_list.fold_left_s
        (fun status commit ->
          match status with
          | Ok () ->
              Current_git.with_checkout ~pool ~job commit @@ fun commit_dir ->
              let repo = commit |> Current_git.Commit.id |> Current_git.Commit_id.repo in
              let branch = commit |> Current_git.Commit.id |> Current_git.Commit_id.gref in
              let repo_name = repo |> Filename.basename |> Filename.remove_extension in
              let** () =
                cmd
                  [|
                    "cp"; "-R"; Fpath.to_string commit_dir; Fpath.(to_string (tmpdir / repo_name));
                  |]
              in
              let** () = cmd [| "git"; "submodule"; "add"; "-b"; branch; repo; repo_name |] in
              Lwt.return_ok ()
          | err -> Lwt.return err)
        (Ok ()) commits
    in
    let** () =
      cmd
        [|
          "git";
          "commit";
          "-m";
          "monorepo-git-push";
          "--author";
          "Mirage CI pipeline <ci@mirage.io>";
        |]
    in
    let** () = cmd [| "git"; "push"; "--force"; "origin"; branch |] in
    let** hash = read [| "git"; "rev-parse"; "HEAD" |] in
    Lwt.return_ok (Current_git.Commit_id.v ~repo:remote_pull ~gref:branch ~hash)
end

module GitPushCache = Current_cache.Output (GitPush)

let v ~remote_push ~remote_pull ~branch commits =
  let open Current.Syntax in
  Current.component "Monorepo git push"
  |> let> commits = commits in
     GitPushCache.set No_context { remote_push; remote_pull; branch } commits
