(** cascade command: derive cascade attribution from the planned DAG plus
    per-package history. *)

open Cmdliner
module Cascade = Day11_lib.Cascade
module Dag_marshal = Day11_lib.Dag_marshal

let kind_label : Dag_marshal.kind -> string = function
  | Build -> "build"
  | Tool -> "tool"
  | Compile -> "compile"
  | Doc_all -> "doc-all"
  | Link -> "link"

let short h = String.sub h 0 (min 12 (String.length h))

let pkg_kind_label (e : Dag_marshal.entry) =
  Printf.sprintf "%s [%s]" (OpamPackage.to_string e.pkg) (kind_label e.kind)

let load_dag ~snapshot_dir =
  match Dag_marshal.read ~snapshot_dir with
  | Ok entries -> Some entries
  | Error (`Msg m) ->
      Printf.eprintf "  warning: could not read dag.json: %s\n%!" m;
      None

(** Show the cascade story for a single package. *)
let show_for_package ~entries ~table pkg_str =
  let matches =
    List.filter
      (fun (e : Dag_marshal.entry) ->
        OpamPackage.to_string e.pkg = pkg_str
        || OpamPackage.Name.to_string (OpamPackage.name e.pkg) = pkg_str)
      entries
  in
  if matches = [] then (
    Printf.printf "No DAG entries found for %s\n" pkg_str;
    1)
  else (
    Printf.printf "DAG entries for %s (%d):\n\n" pkg_str (List.length matches);
    List.iter
      (fun (e : Dag_marshal.entry) ->
        let r = Hashtbl.find table e.hash in
        let status_s =
          match r.Cascade.status with
          | Ok -> "ok"
          | Failed -> "FAILED (root cause)"
          | Cascade src ->
              let src_e =
                List.find (fun (x : Dag_marshal.entry) -> x.hash = src) entries
              in
              Printf.sprintf "cascade ← %s [%s] (%s)"
                (OpamPackage.to_string src_e.pkg)
                (kind_label src_e.kind) (short src)
          | Pending -> "pending (no upstream failure)"
        in
        Printf.printf "  %s  %s  %s\n" (short e.hash) (kind_label e.kind)
          status_s)
      matches;
    0)

(** Group cascaded nodes under their root cause, sort by fanout. *)
let summarise ~entries ~table ~verbose =
  let by_root : (string, Dag_marshal.entry list) Hashtbl.t =
    Hashtbl.create 32
  in
  let pending = ref [] in
  List.iter
    (fun (e : Dag_marshal.entry) ->
      let r = Hashtbl.find table e.hash in
      match r.Cascade.status with
      | Ok -> ()
      | Failed ->
          let prev = try Hashtbl.find by_root e.hash with Not_found -> [] in
          Hashtbl.replace by_root e.hash (e :: prev)
      | Cascade src ->
          let prev = try Hashtbl.find by_root src with Not_found -> [] in
          Hashtbl.replace by_root src (e :: prev)
      | Pending -> pending := e :: !pending)
    entries;
  let groups =
    Hashtbl.fold
      (fun root members acc ->
        let root_e =
          List.find (fun (x : Dag_marshal.entry) -> x.hash = root) entries
        in
        (* Members include the root itself; cascaded count excludes it. *)
        let cascaded =
          List.filter (fun (x : Dag_marshal.entry) -> x.hash <> root) members
        in
        (root_e, cascaded) :: acc)
      by_root []
  in
  let groups =
    List.sort
      (fun (_, a) (_, b) -> compare (List.length b) (List.length a))
      groups
  in
  if groups = [] then Printf.printf "No failures.\n"
  else (
    Printf.printf "%d root cause%s, %d cascaded:\n\n" (List.length groups)
      (if List.length groups = 1 then "" else "s")
      (List.fold_left (fun acc (_, cs) -> acc + List.length cs) 0 groups);
    List.iter
      (fun (root, cascaded) ->
        Printf.printf "  %s  (hash %s)  → %d cascaded\n" (pkg_kind_label root)
          (short root.hash) (List.length cascaded);
        if verbose && cascaded <> [] then
          List.iter
            (fun (e : Dag_marshal.entry) ->
              Printf.printf "      %s  (%s)\n" (pkg_kind_label e) (short e.hash))
            (List.sort
               (fun (a : Dag_marshal.entry) b ->
                 compare
                   (OpamPackage.to_string a.pkg)
                   (OpamPackage.to_string b.pkg))
               cascaded))
      groups);
  if !pending <> [] then
    Printf.printf "\n%d pending (in-flight or not yet attempted)\n"
      (List.length !pending);
  0

let run profile_name profile_dir package verbose =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) -> (
      match Common.latest_snapshot_dir paths with
      | None ->
          Printf.printf "No snapshots found\n";
          1
      | Some snapshot_dir -> (
          match load_dag ~snapshot_dir with
          | None ->
              Printf.printf "No dag.json in latest snapshot\n";
              1
          | Some entries -> (
              let packages_dir = Fpath.(snapshot_dir / "packages") in
              let table = Cascade.classify ~packages_dir entries in
              match package with
              | Some pkg -> show_for_package ~entries ~table pkg
              | None -> summarise ~entries ~table ~verbose)))

let package_term =
  Arg.(
    value
    & opt (some string) None
    & info [ "package"; "p" ] ~docv:"PACKAGE"
        ~doc:"Show cascade detail for one package (full version or name).")

let verbose_term =
  Arg.(
    value
    & flag
    & info [ "v"; "verbose" ]
        ~doc:"List cascaded packages under each root cause.")

let cmd =
  let info =
    Cmd.info "cascade"
      ~doc:"Derive cascade attribution from the planned DAG and history"
  in
  let term =
    Term.(
      const run
      $ Common.profile_term
      $ Common.profile_dir_term
      $ package_term
      $ verbose_term)
  in
  Cmd.v info term
