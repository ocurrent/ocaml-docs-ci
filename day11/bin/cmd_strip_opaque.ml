(** strip-opaque command: remove stale [trusted.overlay.opaque] xattrs
    from cached layers.

    A one-time fixup for layers built before [attr] (getfattr/setfattr)
    was installed in the daemon image: the per-build strip in
    {!Day11_opam_build.Container_backend} shelled out to those tools and,
    with them absent, silently no-op'd. Opaque markers therefore survived
    into promoted layers, where they shadow sibling lowers when the layer
    stacks above them — e.g. conf-libX11's [/usr/include/X11] hiding
    conf-libXft's [Xft/], which breaks every build of [graphics].

    Walks each os_dir under the profile's cache and strips the marker
    from every layer fs that carries it. Stripping does not change layer
    hashes (they cover build inputs, not fs xattrs), so cached layers
    stay valid — they just stop shadowing. *)

open Cmdliner

let is_hex12 name =
  String.length name = 12
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) name

(* A cache subdir is an os_dir if it holds 12-hex layer dirs. *)
let os_dirs_under cache_dir =
  match Bos.OS.Dir.contents cache_dir with
  | Error _ -> []
  | Ok entries ->
    List.filter (fun d ->
      match Bos.OS.Dir.contents ~rel:true d with
      | Ok inner -> List.exists (fun p -> is_hex12 (Fpath.to_string p)) inner
      | Error _ -> false)
      entries

let strip_os_dir ~sw env os_dir =
  (* Opaque markers only ever land on system dirs written by a depext
     apt install (/usr, /etc, /var); the vast majority of layers only
     touch [/home/opam/.opam] and can't carry one. So we walk only each
     layer's [fs/{usr,etc,var}] rather than every file in every layer —
     the difference between a handful of subtrees and hundreds of
     millions of files across 300k+ layers. [getfattr -R] is fanned out
     over those subtrees; the marker is stripped from each hit. Needs
     root (fs/ is root-owned); fails loudly if [attr] is missing. *)
  let script =
    Printf.sprintf
      "command -v getfattr >/dev/null && command -v setfattr >/dev/null \
         || { echo 'attr (getfattr/setfattr) not installed' >&2; exit 127; }; \
       list=$(find %s -mindepth 3 -maxdepth 3 -type d \
                \\( -path '*/fs/usr' -o -path '*/fs/etc' -o -path '*/fs/var' \\) \
                2>/dev/null \
              | xargs -r -P 16 getfattr -h -R -m trusted.overlay.opaque \
                  --absolute-names 2>/dev/null \
              | awk '/^# file:/ {print $3}' | sort -u); \
       n=$(printf '%%s' \"$list\" | grep -c . || true); \
       echo \"$n\"; \
       if [ \"$n\" -gt 0 ]; then \
         printf '%%s\\n' \"$list\" | grep . \
           | xargs -r -I{} setfattr -x trusted.overlay.opaque {}; \
       fi"
      (Filename.quote (Fpath.to_string os_dir))
  in
  match Day11_sys.Sudo.run ~sw env Bos.Cmd.(v "bash" % "-c" % script) with
  | Error (`Msg m) ->
    Printf.eprintf "  %s: FAILED (%s)\n%!" (Fpath.to_string os_dir) m; 0
  | Ok run ->
    match run.Day11_sys.Run.status with
    | `Exited 0 ->
      let n = try int_of_string (String.trim run.Day11_sys.Run.output)
              with _ -> 0 in
      Printf.printf "  %s: stripped %d\n%!" (Fpath.to_string os_dir) n; n
    | `Exited c ->
      Printf.eprintf "  %s: exit %d %s\n%!"
        (Fpath.to_string os_dir) c run.Day11_sys.Run.errors; 0
    | `Signaled s ->
      Printf.eprintf "  %s: signal %d\n%!" (Fpath.to_string os_dir) s; 0

let run profile_name profile_dir =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) -> Printf.eprintf "Error: %s\n%!" e; 1
  | Ok (_profile, paths) ->
    Common.with_eio @@ fun ~sw env ->
    let os_dirs = os_dirs_under paths.cache_dir in
    Printf.printf "Stripping trusted.overlay.opaque across %d os_dir(s)...\n%!"
      (List.length os_dirs);
    let total =
      List.fold_left (fun acc d -> acc + strip_os_dir ~sw env d) 0 os_dirs in
    Printf.printf "Done. Stripped %d opaque marker(s).\n%!" total;
    0

let cmd =
  let info = Cmd.info "strip-opaque"
    ~doc:"Remove stale trusted.overlay.opaque xattrs from cached layers \
          (one-time fixup for layers built before the daemon had 'attr')" in
  let term =
    Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
