module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

let git_branch branch = Git.Reference.(of_string ("refs/heads/" ^ branch) |> Result.get_ok)

let ( let** ) = Lwt_result.bind

let ( let++ ) a b = Lwt_result.map b a

open Lwt.Syntax

module Packages = struct
  module Prefix = struct
    type t = Fpath.t

    let v path =
      match Fpath.segs path with
      | "packages" :: name :: version :: _ -> Fpath.(v "packages" / name / version)
      | "universes" :: id :: name :: version :: _ -> Fpath.(v "universes" / id / name / version)
      | _ -> Fpath.(v "")
  end

  type t = Store.hash Git.Tree.t

  let search_unwrap commit result =
    result
    |> Lwt.map (Option.map Result.ok)
    |> Lwt.map (Option.value ~default:(Error (`Not_found commit)))

  let get_blob_content store hash =
    let** content = Store.read store hash in
    Lwt.return
    @@
    match content with
    | Git.Value.Blob b -> Ok (Git.Blob.to_string b)
    | _ -> Error (`Msg "get_blob_content: not a blob")

  let get_tree store hash =
    let** content = Store.read store hash in
    Lwt.return
    @@ match content with Git.Value.Tree b -> Ok b | _ -> Error (`Msg "get_tree: not a tree")

  let get_commit store hash =
    let** content = Store.read store hash in
    Lwt.return
    @@
    match content with Git.Value.Commit b -> Ok b | _ -> Error (`Msg "get_commit: not a commit")

  let get_current_live_branch store =
    let** live_commit = Store.Ref.resolve store (git_branch "live") in
    let** live_file_hash =
      Search.find store live_commit (`Commit (`Path [ "live" ])) |> search_unwrap live_commit
    in
    get_blob_content store live_file_hash

  let get_commits_of_live_branch store branch =
    let open Lwt.Syntax in
    let** live_branch_commit = Store.Ref.resolve store (git_branch branch) in
    let** live_branch_tree_hash =
      Search.find store live_branch_commit (`Commit (`Path [])) |> search_unwrap live_branch_commit
    in
    let** tree_content = Store.read store live_branch_tree_hash in
    let** hashes =
      match tree_content with
      | Git.Value.Tree t -> Lwt.return_ok (Git.Tree.hashes t)
      | _ -> Lwt.return_error (`Msg "get_commits_of_live_branch: not a tree")
    in
    let* commits = Lwt_list.map_p (get_blob_content store) hashes in
    Lwt.return_ok (List.filter_map Result.to_option commits)

  module StringMap = Map.Make (String)

  let update key fn =
    StringMap.update key (function
      | None -> Some (fn StringMap.empty)
      | Some value -> Some (fn value))

  let merge_tree ~store lst =
    let tree =
      List.rev_map (fun (name, tree) -> Git.Tree.entry ~name `Dir tree) lst |> Git.Tree.v
    in
    let++ hash, _ = Store.write store (Git.Value.Tree tree) in
    hash

  let rec merge_trees ~store trees =
    let data = ref StringMap.empty in
    List.iter
      (Git.Tree.iter (fun ({ Git.Tree.name; _ } as entry) ->
           data :=
             StringMap.update name
               (function None -> Some [ entry ] | Some entries -> Some (entry :: entries))
               !data))
      trees;
    let* new_tree_entries =
      StringMap.bindings !data
      |> Lwt_list.map_p (function
           | _, [ v ] -> Lwt.return_ok v
           | _, ({ Git.Tree.name; perm = `Dir; _ } :: _ as lst) ->
               let* nodes = Lwt_list.map_p (fun { Git.Tree.node; _ } -> get_tree store node) lst in
               let nodes = List.filter_map Result.to_option nodes in
               let++ node = merge_trees ~store nodes in
               { Git.Tree.name; perm = `Dir; node }
           | _, v :: _ -> Lwt.return_ok v
           | _, [] -> assert false)
    in
    let++ hash, _ =
      Store.write store
        (Git.Value.Tree (new_tree_entries |> List.filter_map Result.to_option |> Git.Tree.v))
    in
    hash

  let merge_commits ~store commits =
    let* trees =
      (* error handling ? *)
      Lwt_list.map_p
        (fun commit ->
          let commit = Digestif.SHA1.of_hex commit in
          let** tree_hash =
            Search.find store commit (`Commit (`Path [ "content" ])) |> search_unwrap commit
          in
          get_tree store tree_hash)
        commits
    in
    List.filter_map Result.to_option trees |> merge_trees ~store

  let analyse store =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let** current_live_branch = get_current_live_branch store in
    Fmt.pr "Live: %s\n%!" current_live_branch;
    let** commits = get_commits_of_live_branch store current_live_branch in
    Fmt.pr "Commits: %d\n%!" (List.length commits);
    let++ merged = merge_commits ~store commits in
    Fmt.pr "Ready!\n%!";
    merged
end

module Server = struct
  open Httpaf
  open Httpaf_lwt_unix

  let rec serve_tree store root request =
    let** object_hash =
      Search.find store root (`Path request) |> Lwt.map (Option.to_result ~none:`Not_found)
    in
    let** target = Store.read store object_hash |> Lwt_result.map_err (fun e -> `Git e) in
    match target with
    | Git.Value.Blob v -> Lwt.return_ok (Git.Blob.to_string v)
    | Git.Value.Tree t ->
        List.map
          (fun { Git.Tree.name; _ } ->
            Fmt.str "<a href='/%s'>%s</a><br/>" (String.concat "/" (request @ [ name ])) name)
          (Git.Tree.to_list t)
        |> String.concat "" |> Lwt.return_ok
    | _ -> Lwt.return_error `Not_a_valid_object

  let invalid_request reqd status body =
    (* Responses without an explicit length or transfer-encoding are
       close-delimited. *)
    let headers = Headers.of_list [ ("Connection", "close") ] in
    Reqd.respond_with_string reqd (Response.create ~headers status) body

  let request_handler ~store ~root _ reqd =
    let { Request.meth; target; _ } = Reqd.request reqd in
    match meth with
    | `GET -> (
        match String.split_on_char '/' target with
        | "" :: req -> (
            Lwt.async @@ fun () ->
            let+ result = serve_tree store root (List.filter (fun t -> String.length t > 0) req) in
            match result with
            | Ok content ->
                (* Specify the length of the response. *)
                let headers =
                  Headers.of_list [ ("Content-length", string_of_int (String.length content)) ]
                in
                Reqd.respond_with_string reqd (Response.create ~headers `OK) content
            | Error `Not_found -> invalid_request reqd `Not_found "Path not found"
            | Error (`Git e) ->
                invalid_request reqd `Not_found (Fmt.str "Git error: %a" Store.pp_error e)
            | Error `Not_a_valid_object -> invalid_request reqd `Not_found "Not a valid object" )
        | _ ->
            let response_body = Printf.sprintf "%S not found\n" target in
            invalid_request reqd `Not_found response_body )
    | meth ->
        let response_body =
          Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth)
        in
        invalid_request reqd `Method_not_allowed response_body

  let error_handler _ ?request:_ error _ =
    let error =
      match error with
      | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
      | _ -> "Invalid error"
    in
    Format.eprintf "Error handling response: %s\n%!" error

  let serve ~store ~root port =
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    let request_handler = request_handler ~store ~root in

    Lwt_io.establish_server_with_client_socket listen_address
      (Server.create_connection_handler ~request_handler ~error_handler)
end

let main port repo =
  let forever, _ = Lwt.wait () in
  let repo = Fpath.of_string repo |> Result.get_ok in
  let** store = Store.v ~dotgit:repo repo in
  let** root = Packages.analyse store in
  Lwt.async (fun () ->
      let+ _ = Server.serve ~store ~root port in
      Fmt.pr "Listening on port %d.\n" port);
  forever

let main port repo =
  Lwt_main.run
    (let+ main = main port repo in
     match main with Ok _ -> () | Error e -> Fmt.epr "%a\n" Store.pp_error e)

open Cmdliner

let port = Arg.value @@ Arg.opt Arg.int 8000 @@ Arg.info ~doc:"HTTP port" ~docv:"PORT" [ "port" ]

let repo =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Local git repository containing docs ci output" ~docv:"REPO" [ "repo" ]

let cmd =
  let doc = "an OCurrent pipeline" in
  (Term.(const main $ port $ repo), Term.info "githttpserver" ~doc)

let () = Term.(exit @@ eval cmd)
