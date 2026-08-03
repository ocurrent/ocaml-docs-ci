open Lwt.Infix
module Store = Git_unix.Store

let get_git_repo_store_and_hash_commit_lwt repo_path commit_opt =
  Store.v (Fpath.v repo_path) >>= function
  | Error e ->
      Fmt.failwith "Failed to open git store at %s: %a" repo_path Store.pp_error
        e
  | Ok store ->
      (match commit_opt with
      | Some commit_str -> (
          let ref_name = Git.Reference.v commit_str in
          Store.Ref.resolve store ref_name >>= function
          | Ok hash -> Lwt.return hash
          | Error _ -> (
              try
                let hash = Store.Hash.of_hex commit_str in
                Lwt.return hash
              with _ ->
                Fmt.failwith "Cannot resolve commit %s in %s" commit_str
                  repo_path))
      | None -> (
          Store.Ref.resolve store Git.Reference.head >>= function
          | Error e ->
              Fmt.failwith "Failed to resolve HEAD in %s: %a" repo_path
                Store.pp_error e
          | Ok hash -> Lwt.return hash))
      >>= fun hash -> Lwt.return (store, hash)

let get_git_repo_store_and_hash_commit repo_path commit_opt =
  Lwt_main.run (get_git_repo_store_and_hash_commit_lwt repo_path commit_opt)

let get_git_repo_store_and_hash_lwt repo_path =
  get_git_repo_store_and_hash_commit_lwt repo_path None

let get_git_repo_store_and_hash repo_path =
  get_git_repo_store_and_hash_commit repo_path None

let resolve_commit_in_store_lwt store commit_opt =
  match commit_opt with
  | Some commit_str -> (
      let ref_name = Git.Reference.v commit_str in
      Store.Ref.resolve store ref_name >>= function
      | Ok hash -> Lwt.return hash
      | Error _ -> (
          try
            let hash = Store.Hash.of_hex commit_str in
            Lwt.return hash
          with _ -> Fmt.failwith "Cannot resolve commit %s" commit_str))
  | None -> (
      Store.Ref.resolve store Git.Reference.head >>= function
      | Error e -> Fmt.failwith "Failed to resolve HEAD: %a" Store.pp_error e
      | Ok hash -> Lwt.return hash)

let resolve_commit_in_store store commit_opt =
  Lwt_main.run (resolve_commit_in_store_lwt store commit_opt)
