(* Persistent [version-dir tree OID + package → effective-part digest]
   store.

   The effective-part digest of an opam file is a pure function of the
   file's content {e and the package name/version} — the loader stamps
   both into the parsed opam from the directory name, and
   [OpamFile.OPAM.effective_part] keeps them. The version-directory
   tree OID changes whenever the content does, so [(OID, name.version)]
   → digest can be cached {e forever}, across processes and commits.
   This is what lets a warm daemon compute layer hashes for ~18k
   packages without parsing a single unchanged opam file: only versions
   whose tree OID is new get parsed (a handful per upstream commit).

   The store key MUST include the package identity, not just the OID:
   two different packages can share a version-dir tree OID when their
   dirs are byte-identical, which really happens — multi-package
   releases with templated opam files (js_of_ocaml-ppx /
   js_of_ocaml-ppx_deriving_json since 6.1.0), re-releases like
   hidapi.1.0-1 / hidapi.1.0. Keyed by bare OID, the first twin's
   digest was served for both; identical per-package digests collapse
   twin layer hashes (layer_hash digests only the per-package digests,
   not names), and [Dag.build_dag]'s memo-by-hash then silently drops
   one twin from the plan — its dependents build without it.

   On-disk format: one "<oid>:<name.version> <digest>\n" line per
   entry, append-only (O_APPEND writes of short lines are atomic
   enough; a torn final line is skipped on load). The file is shared
   by every profile. *)
module Digest_store = struct
  type t = {
    path : string;
    tbl : (string, string) Hashtbl.t;
    mutable oc : out_channel option;
  }

  let load path =
    let path = Fpath.to_string path in
    let tbl = Hashtbl.create 65536 in
    (try
       let ic = open_in path in
       Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
         try
           while true do
             let line = input_line ic in
             match String.index_opt line ' ' with
             | Some i when i > 0 && i < String.length line - 1 ->
               Hashtbl.replace tbl
                 (String.sub line 0 i)
                 (String.sub line (i + 1) (String.length line - i - 1))
             | _ -> ()
           done
         with End_of_file -> ())
     with Sys_error _ -> ());
    { path; tbl; oc = None }

  let find t oid = Hashtbl.find_opt t.tbl oid

  let add t oid digest =
    if not (Hashtbl.mem t.tbl oid) then begin
      Hashtbl.replace t.tbl oid digest;
      let oc = match t.oc with
        | Some oc -> oc
        | None ->
          let oc = open_out_gen [ Open_append; Open_creat ] 0o644 t.path in
          t.oc <- Some oc;
          oc
      in
      output_string oc (oid ^ " " ^ digest ^ "\n");
      flush oc
    end
end

type t = {
  find_opam : OpamPackage.t -> OpamFile.OPAM.t option;
  find_oid : (OpamPackage.t -> string option) option;
  (* Package → its version-dir tree OID at the current repo state
     (from {!Day11_opam.Git_packages.list_package_versions_lwt});
     keys {!digest_store} lookups. *)
  digest_store : Digest_store.t option;
  patches : Patches.t option;
  per_pkg : (string, string) Hashtbl.t;
  per_layer : (string, string) Hashtbl.t;
}

let create ~find_opam ?find_oid ?digest_store ?patches () =
  { find_opam; find_oid; digest_store; patches;
    per_pkg = Hashtbl.create 256;
    per_layer = Hashtbl.create 256; }

let pkg_opam_hash t pkg =
  let key = OpamPackage.to_string pkg in
  match Hashtbl.find_opt t.per_pkg key with
  | Some h -> h
  | None ->
      (* [parse ()] is the ground truth: parse the opam file and digest
         its effective part. [None] when the package has no opam file
         (shouldn't happen for planned packages) — never persisted. *)
      let parse () =
        match t.find_opam pkg with
        | Some opam ->
            Some (opam
                  |> OpamFile.OPAM.effective_part
                  |> OpamFile.OPAM.write_to_string
                  |> Digest.string |> Digest.to_hex)
        | None -> None
      in
      let opam_h =
        let via_store =
          match t.find_oid, t.digest_store with
          | Some find_oid, Some store ->
            (match find_oid pkg with
             | None -> None
             | Some oid ->
               (* [key] (name.version) must be part of the store key —
                  see the {!Digest_store} comment: byte-identical twin
                  dirs share an OID but not a digest. *)
               let store_key = oid ^ ":" ^ key in
               (match Digest_store.find store store_key with
                | Some d -> Some d
                | None ->
                  (match parse () with
                   | Some d -> Digest_store.add store store_key d; Some d
                   | None -> None)))
          | _ -> None
        in
        match via_store with
        | Some d -> d
        | None ->
          (match parse () with Some d -> d | None -> "missing-" ^ key)
      in
      let h = match t.patches with
        | Some patches ->
          let ph = Patches.hash_for patches pkg in
          if ph = "" then opam_h
          else Digest.string (opam_h ^ ph) |> Digest.to_hex
        | None -> opam_h
      in
      Hashtbl.replace t.per_pkg key h;
      h

let layer_hash t ~base_hash pkgs =
  (* Digest the (base, pkg_list) concatenation so the Hashtbl key is
     a fixed-size 32-byte digest rather than a 1-10KB string. With
     universes of 50-100 packages and 4000+ solutions, an un-digested
     key made per-entry Hashtbl ops O(universe_size) and dominated
     [build_dag] wall-clock. *)
  let str = String.concat ","
    (base_hash :: List.map OpamPackage.to_string pkgs) in
  let key = Digest.to_hex (Digest.string str) in
  match Hashtbl.find_opt t.per_layer key with
  | Some h -> h
  | None ->
      let hashes =
        List.map (fun pkg -> pkg_opam_hash t pkg) pkgs
      in
      let h = Day11_layer.Hash.of_strings (base_hash :: hashes) in
      Hashtbl.replace t.per_layer key h;
      h
