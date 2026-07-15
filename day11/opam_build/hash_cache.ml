(* Process-global [name.version → (version-dir tree OID, effective-part
   digest)] cache.

   The effective-part digest of an opam file is a pure function of the
   version dir's content {e and the package name/version} — the loader
   stamps both into the parsed opam from the directory name, and
   [OpamFile.OPAM.effective_part] keeps them. The version-dir tree OID
   fingerprints the content, so an entry is valid for as long as the
   package's OID is unchanged. Living outside {!t}, the cache survives
   Profile_ctx reloads: a warm daemon computes layer hashes for ~18k
   packages without parsing a single unchanged opam file — only
   versions whose tree OID moved get re-parsed (a handful per upstream
   commit). Same pattern as [Profile_ctx.name_caches]; process
   lifetime only, so a fresh daemon or a one-shot CLI run pays one
   full digest sweep (a few seconds) and is warm thereafter.

   The cache is keyed by the {e package}, with the OID as a freshness
   validator — never by the OID alone. Two different packages can
   share a version-dir tree OID when their dirs are byte-identical,
   which really happens: multi-package releases with templated opam
   files (js_of_ocaml-ppx / js_of_ocaml-ppx_deriving_json since
   6.1.0), re-releases like hidapi.1.0-1 / hidapi.1.0. A predecessor
   of this cache (an on-disk store keyed by bare OID) served the first
   twin's digest for both; identical per-package digests collapse the
   twins' layer hashes ([layer_hash] digests only the per-package
   digests, not names), and [Dag.build_dag]'s memo-by-hash then
   silently dropped one twin from the plan — its dependents built
   without it. Keying by package makes that collision impossible.

   Shared across profiles; when profiles resolve the same package to
   different OIDs (overlay repos) they displace each other's entry,
   which is correct (the validator mismatches) and merely costs the
   occasional re-parse. *)
let global_digests : (string, string * string) Hashtbl.t =
  Hashtbl.create 65536

type t = {
  find_opam : OpamPackage.t -> OpamFile.OPAM.t option;
  find_oid : (OpamPackage.t -> string option) option;
  (* Package → its version-dir tree OID at the current repo state
     (from {!Day11_opam.Git_packages.list_package_versions_lwt});
     validates {!global_digests} entries. *)
  patches : Patches.t option;
  per_pkg : (string, string) Hashtbl.t;
  per_layer : (string, string) Hashtbl.t;
}

let create ~find_opam ?find_oid ?patches () =
  { find_opam; find_oid; patches;
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
        let via_global =
          match t.find_oid with
          | None -> None
          | Some find_oid ->
            (match find_oid pkg with
             | None -> None
             | Some oid ->
               (match Hashtbl.find_opt global_digests key with
                | Some (o, d) when String.equal o oid -> Some d
                | _ ->
                  (match parse () with
                   | Some d ->
                     Hashtbl.replace global_digests key (oid, d);
                     Some d
                   | None -> None)))
        in
        match via_global with
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
