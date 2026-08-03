open Lwt.Infix
module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

type t = OpamFile.OPAM.t OpamPackage.Version.Map.t Lazy.t OpamPackage.Name.Map.t

let empty = OpamPackage.Name.Map.empty

let read_dir store hash =
  Store.read store hash >|= function
  | Error e -> Fmt.failwith "Failed to read tree: %a" Store.pp_error e
  | Ok (Git.Value.Tree tree) -> Some tree
  | Ok _ -> None

let load_opam_from_string pkg opam_content =
  let opam = OpamFile.OPAM.read_from_string opam_content in
  let opam = OpamFile.OPAM.with_name (OpamPackage.name pkg) opam in
  OpamFile.OPAM.with_version (OpamPackage.version pkg) opam

let read_package store pkg hash =
  Search.find store hash (`Path [ "opam" ]) >>= function
  | None ->
      Fmt.failwith "opam file not found for %s" (OpamPackage.to_string pkg)
  | Some hash -> (
      Store.read store hash >|= function
      | Ok (Git.Value.Blob blob) -> (
          try
            let opam_content = Store.Value.Blob.to_string blob in
            load_opam_from_string pkg opam_content
          with ex ->
            Fmt.failwith "Error parsing %s: %s"
              (OpamPackage.to_string pkg)
              (Printexc.to_string ex))
      | _ ->
          Fmt.failwith "Bad Git object type for %s!" (OpamPackage.to_string pkg)
      )

let read_versions_lwt store (entry : Store.Value.Tree.entry) =
  read_dir store entry.node >>= function
  | None -> Lwt.return OpamPackage.Version.Map.empty
  | Some tree ->
      Store.Value.Tree.to_list tree
      |> Lwt_list.fold_left_s
           (fun acc (entry : Store.Value.Tree.entry) ->
             match OpamPackage.of_string_opt entry.name with
             | Some pkg ->
                 Lwt.catch
                   (fun () ->
                     read_package store pkg entry.node >|= fun opam ->
                     OpamPackage.Version.Map.add pkg.version opam acc)
                   (fun _exn -> Lwt.return acc)
             | None -> Lwt.return acc)
           OpamPackage.Version.Map.empty

let read_packages_eager ~store tree =
  Store.Value.Tree.to_list tree
  |> List.filter_map (fun (entry : Store.Value.Tree.entry) ->
      match OpamPackage.Name.of_string entry.name with
      | exception _ -> None
      | name ->
          let versions = read_versions_lwt store entry in
          Some (name, versions))

let overlay v1 v2 =
  lazy
    (let v1 = Lazy.force v1 in
     let v2 = Lazy.force v2 in
     OpamPackage.Version.Map.union (fun _ v2 -> v2) v1 v2)

(* Eager version: force all version maps inside the Lwt event loop
   before wrapping in [Lazy.t]. When the resulting [t] is later
   consumed from another Lwt context (e.g. an OCurrent Op), forcing
   the lazy cells no longer triggers [Lwt_main.run] — the values are
   already computed. *)
let of_commit_lwt ?(super = empty) store commit : t Lwt.t =
  Search.find store commit (`Commit (`Path [ "packages" ])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory!"
  | Some tree_hash -> (
      read_dir store tree_hash >>= function
      | None -> Fmt.failwith "'packages' is not a directory!"
      | Some tree ->
          let entries = read_packages_eager ~store tree in
          Lwt_list.map_s
            (fun (name, versions_lwt) ->
              versions_lwt >|= fun versions -> (name, lazy versions))
            entries
          >|= fun resolved ->
          let packages = OpamPackage.Name.Map.of_list resolved in
          OpamPackage.Name.Map.union overlay super packages)

let of_commit ?(super = empty) store commit : t =
  Lwt_main.run (of_commit_lwt ~super store commit)

(* ── Incremental loading ─────────────────────────────────────────
   [of_commit_lwt] is deliberately eager (see its comment), which
   means a full ~38k-opam-file parse per load — the dominant cost of
   reloading a Profile_ctx on every upstream commit. The incremental
   variant reuses the previous load's parsed version maps for every
   package name whose [packages/<name>] tree OID is unchanged,
   re-reading only the diff. Sound because the version map is a pure
   function of the name's tree content, which the OID fingerprints. *)

type name_cache =
  (string, string * OpamFile.OPAM.t OpamPackage.Version.Map.t) Hashtbl.t
(* name → (name-tree OID, parsed versions) from a previous load. *)

let of_commit_incremental_lwt ?(super = empty) ?prev store commit :
    (t * name_cache) Lwt.t =
  Search.find store commit (`Commit (`Path [ "packages" ])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory!"
  | Some tree_hash -> (
      read_dir store tree_hash >>= function
      | None -> Fmt.failwith "'packages' is not a directory!"
      | Some tree ->
          let fresh : name_cache = Hashtbl.create 8192 in
          Store.Value.Tree.to_list tree
          |> Lwt_list.filter_map_s (fun (entry : Store.Value.Tree.entry) ->
              match OpamPackage.Name.of_string entry.name with
              | exception _ -> Lwt.return_none
              | name ->
                  let oid = Store.Hash.to_hex entry.node in
                  let reuse =
                    match prev with
                    | None -> None
                    | Some prev -> (
                        match Hashtbl.find_opt prev entry.name with
                        | Some (prev_oid, versions)
                          when String.equal prev_oid oid ->
                            Some versions
                        | _ -> None)
                  in
                  (match reuse with
                    | Some versions -> Lwt.return versions
                    | None -> read_versions_lwt store entry)
                  >|= fun versions ->
                  Hashtbl.replace fresh entry.name (oid, versions);
                  Some (name, lazy versions))
          >|= fun resolved ->
          let packages = OpamPackage.Name.Map.of_list resolved in
          (OpamPackage.Name.Map.union overlay super packages, fresh))

let of_commit_eager store commit : t =
  Lwt_main.run
  @@ ( Search.find store commit (`Commit (`Path [ "packages" ])) >>= function
       | None -> Fmt.failwith "Failed to find packages directory!"
       | Some tree_hash -> (
           read_dir store tree_hash >>= function
           | None -> Fmt.failwith "'packages' is not a directory!"
           | Some tree ->
               let entries = read_packages_eager ~store tree in
               Lwt_list.map_s
                 (fun (name, versions_lwt) ->
                   versions_lwt >|= fun versions -> (name, lazy versions))
                 entries
               >|= fun resolved -> OpamPackage.Name.Map.of_list resolved) )

let of_opam_repository repo_path =
  let store, commit = Git_utils.get_git_repo_store_and_hash repo_path in
  let packages = of_commit store commit in
  (packages, store, commit)

let of_repositories_lwt repos =
  assert (repos <> []);
  Lwt_list.map_s
    (fun (repo_path, commit_opt) ->
      Git_utils.get_git_repo_store_and_hash_lwt repo_path
      >>= fun (store, head) ->
      (match commit_opt with
        | Some sha -> Git_utils.resolve_commit_in_store_lwt store (Some sha)
        | None -> Lwt.return head)
      >|= fun commit -> (repo_path, store, commit))
    repos
  >>= fun stores_and_commits ->
  Lwt_list.fold_left_s
    (fun super (_, store, commit) -> of_commit_lwt ~super store commit)
    empty stores_and_commits
  >|= fun packages ->
  let repos_with_shas =
    List.map
      (fun (repo_path, _store, commit) -> (repo_path, Store.Hash.to_hex commit))
      stores_and_commits
  in
  (packages, repos_with_shas)

let of_repositories repos = Lwt_main.run (of_repositories_lwt repos)

(* Multi-repo incremental variant: [prev] maps repo path → the
   {!name_cache} returned by the previous load of that repo. Returns
   the merged index, the resolved shas, and fresh per-repo caches to
   feed the next load. *)
let of_repositories_incremental_lwt ~prev repos =
  assert (repos <> []);
  Lwt_list.map_s
    (fun (repo_path, commit_opt) ->
      Git_utils.get_git_repo_store_and_hash_lwt repo_path
      >>= fun (store, head) ->
      (match commit_opt with
        | Some sha -> Git_utils.resolve_commit_in_store_lwt store (Some sha)
        | None -> Lwt.return head)
      >|= fun commit -> (repo_path, store, commit))
    repos
  >>= fun stores_and_commits ->
  Lwt_list.fold_left_s
    (fun (super, caches) (path, store, commit) ->
      of_commit_incremental_lwt ~super ?prev:(List.assoc_opt path prev) store
        commit
      >|= fun (t, cache) -> (t, (path, cache) :: caches))
    (empty, []) stores_and_commits
  >|= fun (packages, caches) ->
  let repos_with_shas =
    List.map
      (fun (repo_path, _store, commit) -> (repo_path, Store.Hash.to_hex commit))
      stores_and_commits
  in
  (packages, repos_with_shas, List.rev caches)

let force_all (t : t) =
  OpamPackage.Name.Map.iter
    (fun _name versions -> try ignore (Lazy.force versions) with _ -> ())
    t

let get_versions (t : t) name =
  match OpamPackage.Name.Map.find_opt name t with
  | None -> OpamPackage.Version.Map.empty
  | Some versions -> (
      try Lazy.force versions with _ -> OpamPackage.Version.Map.empty)

let get_package (t : t) pkg =
  let versions = get_versions t (OpamPackage.name pkg) in
  OpamPackage.Version.Map.find (OpamPackage.version pkg) versions

let find_package (t : t) pkg =
  let versions = get_versions t (OpamPackage.name pkg) in
  OpamPackage.Version.Map.find_opt (OpamPackage.version pkg) versions

let all_names (t : t) =
  OpamPackage.Name.Map.fold (fun name _ acc -> name :: acc) t []

(* List every package version under [packages/] at [commit] with its
   version-directory {e tree OID} — a per-package change fingerprint
   that covers the opam file and any [files/] patches, obtained purely
   from tree reads (1 + one per package name; no blob reads, no
   checkout). Entries whose name doesn't parse as a package version
   (stray READMEs etc.) are skipped. *)
let list_package_versions_lwt ~store commit =
  Search.find store commit (`Commit (`Path [ "packages" ])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory"
  | Some tree_oid -> (
      read_dir store tree_oid >>= function
      | None -> Fmt.failwith "'packages' is not a directory"
      | Some tree ->
          Store.Value.Tree.to_list tree
          |> Lwt_list.map_s (fun (name_entry : Store.Value.Tree.entry) ->
              read_dir store name_entry.node >|= function
              | None -> [] (* non-directory entry under packages/ *)
              | Some versions ->
                  Store.Value.Tree.to_list versions
                  |> List.filter_map (fun (v_entry : Store.Value.Tree.entry) ->
                      match OpamPackage.of_string v_entry.name with
                      | exception _ -> None
                      | pkg -> Some (pkg, Store.Hash.to_hex v_entry.node)))
          >|= List.concat)

let diff_packages_lwt ~store commit1 commit2 =
  Search.find store commit1 (`Commit (`Path [ "packages" ])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory in commit1"
  | Some tree1_hash -> (
      read_dir store tree1_hash >>= function
      | None -> Fmt.failwith "'packages' is not a directory in commit1"
      | Some tree1 -> (
          Search.find store commit2 (`Commit (`Path [ "packages" ]))
          >>= function
          | None -> Fmt.failwith "Failed to find packages directory in commit2"
          | Some tree2_hash -> (
              read_dir store tree2_hash >>= function
              | None -> Fmt.failwith "'packages' is not a directory in commit2"
              | Some tree2 ->
                  let tree1_list = Store.Value.Tree.to_list tree1 in
                  let htbl = Hashtbl.create (List.length tree1_list) in
                  let tree2_list = Store.Value.Tree.to_list tree2 in
                  List.iter
                    (fun (entry : Store.Value.Tree.entry) ->
                      Hashtbl.add htbl entry.name entry)
                    tree2_list;
                  Lwt.return
                    (List.fold_left
                       (fun acc (entry : Store.Value.Tree.entry) ->
                         match Hashtbl.find_opt htbl entry.name with
                         | Some entry2 when entry.node = entry2.node -> acc
                         | _ -> (
                             match OpamPackage.Name.of_string entry.name with
                             | exception _ -> acc
                             | name -> name :: acc))
                       [] tree1_list))))

let diff_packages ~store commit1 commit2 =
  Lwt_main.run (diff_packages_lwt ~store commit1 commit2)
