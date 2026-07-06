let src = Logs.Src.create "day11.build.base" ~doc:"Base image management"
module Log = (val Logs.src_log src)

let hash ~image = Day11_layer.Hash.base_hash ~image

let build_hash ~os_distribution ~os_version ~arch:_ ?digest () =
  match digest with
  | Some d -> hash ~image:d
  | None ->
    let image = Printf.sprintf "%s:%s" os_distribution os_version in
    hash ~image

let make_base_layer ~image ~base_dir : Day11_layer.Base.t =
  { hash = hash ~image; dir = base_dir; image }

(* Per-OS directory name under [cache_dir]. Single source of truth —
   [Profile.os_dir_name] delegates here so the base dir the profile
   context computes ([os_dir / "base"]) and the one [build] materialises
   ([cache_dir / os_dir_name / "base"]) are always the same path. *)
let os_dir_name ~os_distribution ~os_version ~arch =
  Printf.sprintf "%s-%s-%s" os_distribution os_version arch

(* The base layer lives {b under} its os_dir, not in a shared
   [cache_dir/base]. A shared base can hold only one OS at a time: with
   both a debian and an ubuntu profile, whichever ran [ensure-base]
   first won, and every other profile silently built its packages
   against that OS's root filesystem while labelling them its own. *)
let base_dir_of_os_dir os_dir = Fpath.(os_dir / "base")

let digest_file base_dir = Fpath.(base_dir / "base-digest")

let save_digest base_dir digest =
  ignore (Bos.OS.File.write (digest_file base_dir) digest)

let load_digest base_dir =
  match Bos.OS.File.read (digest_file base_dir) with
  | Ok d -> Some (String.trim d)
  | Error _ -> None

(* Serialise materialisation of a base dir across fibers and processes —
   the same {!Day11_sys.Dir_lock} mechanism the layer builds use
   (build_layer.ml). Two fibers/processes ensuring the same base import
   into one directory; unlocked, their concurrent [docker export | tar x]
   extractions interleave and fail with spurious EEXIST/ENOENT (and the
   daemon's ensure-base op then caches the error, freezing the profile
   until a manual rebuild). The lock is a sibling of the base dir
   ([<os_dir>/base.lock]), so it is per-OS: distinct-OS bases now live in
   distinct dirs and never contend. The [fs/usr] marker is a directory,
   so we can't use [with_lock]'s [?marker_file] (a file check) — callers
   re-check the marker inside [f] instead: the winner materialises,
   waiters see the marker and return the already-built layer. *)
let with_base_lock base_dir f =
  let lock_file = Fpath.(base_dir + ".lock") in
  let result = ref None in
  let (_ : (unit, [ `Msg of string ]) result) =
    Day11_sys.Dir_lock.with_lock ~lock_file base_dir
      (fun ~set_temp_log_path:_ _dir ->
        result := Some (f ());
        Ok ())
  in
  match !result with
  | Some r -> r
  | None -> Rresult.R.error_msg "base lock: body did not run"

let ensure ~sw env ~os_dir ~image =
  let base_dir = base_dir_of_os_dir os_dir in
  let marker = Fpath.(base_dir / "fs" / "usr") in
  if Bos.OS.Dir.exists marker |> Result.get_ok then
    Ok (make_base_layer ~image ~base_dir)
  else
    with_base_lock base_dir @@ fun () ->
    if Bos.OS.Dir.exists marker |> Result.get_ok then
      Ok (make_base_layer ~image ~base_dir)
    else
      match Day11_layer.Import.from_docker ~sw env ~image ~layer_dir:base_dir with
      | Ok () -> Ok (make_base_layer ~image ~base_dir)
      | Error _ as e -> e

let platform = function
  | "x86_64" | "amd64" -> "linux/amd64"
  | "aarch64" -> "linux/arm64"
  | "armv7l" -> "linux/arm/v7"
  | "i386" | "i486" | "i586" | "i686" -> "linux/386"
  | "ppc64le" -> "linux/ppc64le"
  | "riscv64" -> "linux/riscv64"
  | "s390x" -> "linux/s390x"
  | arch -> "linux/" ^ arch

let opam_arch = function
  | "x86_64" | "amd64" -> "x86_64"
  (* opam's release assets use [arm64], not the kernel's [aarch64]. *)
  | "aarch64" | "arm64" -> "arm64"
  | "armv7l" -> "armhf"
  | "i386" | "i486" | "i586" | "i686" -> "i686"
  | arch -> arch

(* Separate Dockerfile just for building opam-build *)
let generate_opam_build_dockerfile ~arch ~local_opam_build =
  let open Dockerfile in
  let base_image = "debian:bookworm" in
  let plat = platform arch in
  let get_opam =
    from ~platform:plat base_image
    @@ run "apt update && apt install -y curl build-essential git unzip bubblewrap"
    @@ run "curl -fsSL https://github.com/ocaml/opam/releases/download/2.4.1/opam-2.4.1-%s-linux -o /usr/local/bin/opam && chmod +x /usr/local/bin/opam"
         (opam_arch arch)
  in
  let get_source =
    if local_opam_build then
      copy ~src:[ "opam-build" ] ~dst:"/tmp/opam-build" ()
    else
      run "git clone --depth 1 --branch apt-update https://github.com/jonludlam/opam-build.git /tmp/opam-build"
  in
  get_opam
  @@ run "opam init --disable-sandboxing -a --bare -y"
  @@ get_source
  @@ workdir "/tmp/opam-build"
  @@ run "opam switch create . 5.3.0 --deps-only -y"
  @@ run "opam exec -- dune build --release"
  @@ run "install -m 755 _build/default/bin/main.exe /usr/local/bin/opam-build"

let generate_dockerfile ~os_distribution ~os_version ~arch ~uid ~gid
    ~has_opam_build_bin ?digest () =
  let open Dockerfile in
  let base_image = match digest with
    | Some d -> Printf.sprintf "%s@%s" os_distribution d
    | None -> Printf.sprintf "%s:%s" os_distribution os_version
  in
  let plat = platform arch in
  (* Stage 1: download opam binary *)
  let stage1 =
    from ~platform:plat ~alias:"opam-builder" base_image
    @@ run "apt update && apt install -y curl"
    @@ run "curl -fsSL https://github.com/ocaml/opam/releases/download/2.4.1/opam-2.4.1-%s-linux -o /usr/local/bin/opam && chmod +x /usr/local/bin/opam"
         (opam_arch arch)
  in
  (* Final stage *)
  let final =
    from ~platform:plat base_image
    @@ run "apt update && apt upgrade -y"
    @@ run "apt install build-essential unzip bubblewrap git sudo curl rsync -y"
    (* Upstream binaryen (>= 119) for conf-binaryen / wasm_of_ocaml-compiler
       (a hard dep of eliom). Debian bookworm's apt binaryen is too old
       (~105); the release tarball drops wasm-merge/wasm-opt into
       /usr/local (ahead of /usr/bin on PATH), and ldconfig picks up its
       libbinaryen. *)
    @@ run "curl -fsSL https://github.com/WebAssembly/binaryen/releases/download/version_120/binaryen-version_120-x86_64-linux.tar.gz | tar -xz -C /usr/local --strip-components=1 && ldconfig"
    @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ]
         ~dst:"/usr/local/bin/opam" ()
    @@ (if has_opam_build_bin then
         (* COPY from context doesn't preserve exec for runc overlayfs,
            so copy then chmod inside the image *)
         copy ~src:[ "opam-build-bin" ] ~dst:"/tmp/opam-build-bin" ()
         @@ run "install -m 755 /tmp/opam-build-bin /usr/local/bin/opam-build && rm /tmp/opam-build-bin"
       else empty)
    @@ run "echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections"
    @@ run "if getent passwd %i; then userdel -r $(id -nu %i); fi" uid uid
    @@ run "groupadd --gid %i opam" gid
    @@ run "adduser --disabled-password --gecos '@opam' --no-create-home --uid %i --gid %i --home /home/opam opam" uid gid
    @@ run "mkdir -p /home/opam && chown -R %i:%i /home/opam" uid gid
    @@ run "echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam"
    @@ run "chmod 440 /etc/sudoers.d/opam"
    @@ run "chown root:root /etc/sudoers.d/opam"
    @@ user "%i:%i" uid gid
    @@ workdir "/home/opam"
    (* day11 builds never use the base image's [default] repo: every
       build and tool node mounts a per-package opam-repository slice
       over [~/.opam/repo/default] at run time (see
       [Container_backend.build]). So we init opam against an *empty*
       repo — just enough that [default] is configured and
       [~/.opam/repo/default] exists as a valid mount target — instead
       of baking the full ~18k-package opam-repository (≈160 MB), which
       only ever served as an unused, slow fallback (opam re-parsed all
       of it on switch-state load when no slice was mounted). *)
    @@ run "mkdir -p /home/opam/empty-repo/packages"
    @@ run "echo 'opam-version: \"2.0\"' > /home/opam/empty-repo/repo"
    @@ run "opam init -k local -a /home/opam/empty-repo --bare --disable-sandboxing -y"
    (* Pull source archives through opam.ocaml.org's cache rather than
       hitting each package's upstream (GitHub etc.) directly — those
       fail intermittently and stall builds. Appended to the root config
       after [opam init] creates it, so it applies to every build that
       runs in this base image. *)
    @@ run "echo 'archive-mirrors: \"https://opam.ocaml.org/cache\"' \
            >> /home/opam/.opam/config"
    @@ run "opam switch create default --empty"
  in
  stage1 @@ final

(** Cache key for the opam-build binary. Profiles with different
    [opam_build_repo] settings need distinct binaries (local forks
    often handle packages upstream can't), so the binary cache is
    keyed by the source rather than shared globally. *)
let opam_build_bin_path ~cache_dir ~opam_build_repo =
  let key = match opam_build_repo with
    | None -> "upstream"
    | Some path ->
      let s = Fpath.to_string path in
      let h = Digest.string s |> Digest.to_hex in
      "local-" ^ String.sub h 0 12
  in
  Fpath.(cache_dir / ("opam-build-bin-" ^ key))

let build_opam_build ~sw env ~cache_dir ~arch ?opam_build_repo () =
  let bin_path = opam_build_bin_path ~cache_dir ~opam_build_repo in
  if Bos.OS.File.exists bin_path |> Result.get_ok then
    Ok bin_path
  else begin
    Log.info (fun m -> m "Building opam-build binary...");
    let temp_dir = Bos.OS.Dir.tmp "day11_opam_build_%s" |> Result.get_ok in
    let local_opam_build = match opam_build_repo with
      | Some src ->
        let dst = Fpath.(temp_dir / "opam-build") in
        (match Day11_sys.Tree.copy ~source:src ~target:dst with
         | Ok () ->
           ignore (Sys.command (Printf.sprintf "rm -rf %s"
             (Fpath.to_string Fpath.(dst / "_build"))));
           true
         | Error _ -> false)
      | None -> false
    in
    let dockerfile =
      generate_opam_build_dockerfile ~arch ~local_opam_build in
    let dockerfile_path = Fpath.(temp_dir / "Dockerfile") in
    Bos.OS.File.write dockerfile_path
      (Dockerfile.string_of_t dockerfile) |> ignore;
    (* Tag by source so concurrent builds for different
       [opam_build_repo] values don't race on the same image tag. *)
    let tag_suffix = match opam_build_repo with
      | None -> "upstream"
      | Some p ->
        let h = Digest.string (Fpath.to_string p) |> Digest.to_hex in
        "local-" ^ String.sub h 0 12
    in
    let tag = Printf.sprintf "day11-opam-build:%s" tag_suffix in
    let build_log = Fpath.(temp_dir / "docker-build.log") in
    (* Docker's containerd image store refuses to re-tag an existing image
       (the legacy builder silently overwrote), so drop any prior tag before
       this --no-cache rebuild. Harmless no-op when the tag doesn't exist. *)
    Day11_sys.Run.run ~sw env
      Bos.Cmd.(v "docker" % "image" % "rm" % "-f" % tag) None |> ignore;
    let build_run =
      Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "docker" % "build" % "--network=host" % "--no-cache"
                 % "-t" % tag % Fpath.to_string temp_dir)
        (Some build_log)
    in
    match build_run.status with
    | `Exited 0 ->
      (* Extract just the binary from the image *)
      let extract_run =
        Day11_sys.Run.run ~sw env
          Bos.Cmd.(v "docker" % "run" % "--rm" % tag
                   % "cat" % "/usr/local/bin/opam-build")
          None
      in
      (match extract_run.status with
       | `Exited 0 ->
         let oc = open_out_bin (Fpath.to_string bin_path) in
         output_string oc extract_run.output;
         close_out oc;
         Unix.chmod (Fpath.to_string bin_path) 0o755;
         Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
         Ok bin_path
       | _ ->
         Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
         Rresult.R.error_msgf "Failed to extract opam-build binary")
    | `Exited n ->
      let saved_log = Fpath.(cache_dir / "opam-build-failed.log") in
      ignore (Bos.OS.File.read build_log |> Result.map (fun content ->
        ignore (Bos.OS.File.write saved_log content)));
      Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
      Rresult.R.error_msgf
        "opam-build Docker build failed (exit %d). Log: %a"
        n Fpath.pp saved_log
    | `Signaled n ->
      Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
      Rresult.R.error_msgf "opam-build Docker build signaled %d" n
  end

let opam_build_mount ~cache_dir ?opam_build_repo () =
  let bin_path = opam_build_bin_path ~cache_dir ~opam_build_repo in
  if Bos.OS.File.exists bin_path |> Result.get_ok then
    Some (Day11_container.Mount.bind_ro
      ~src:(Fpath.to_string bin_path)
      "/usr/local/bin/opam-build")
  else
    None

let load_cached ~os_dir ~os_distribution ~os_version =
  let base_dir = base_dir_of_os_dir os_dir in
  let marker = Fpath.(base_dir / "fs" / "usr") in
  if Bos.OS.Dir.exists marker |> Result.get_ok then begin
    (* Use stored digest if available, otherwise fall back to tag *)
    let image = match load_digest base_dir with
      | Some d -> d
      | None -> Printf.sprintf "%s:%s" os_distribution os_version
    in
    Some (make_base_layer ~image ~base_dir)
  end else
    None

let build ~sw env ~cache_dir ~os_distribution ~os_version ~arch
    ~uid ~gid ?digest () =
  (* [cache_dir] still holds the OS-independent bits (the opam-build
     binary, failed-build logs); only the base layer itself is per-OS. *)
  let os_dir =
    Fpath.(cache_dir / os_dir_name ~os_distribution ~os_version ~arch) in
  let base_dir = base_dir_of_os_dir os_dir in
  let marker = Fpath.(base_dir / "fs" / "usr") in
  let image = Printf.sprintf "%s:%s" os_distribution os_version in
  if Bos.OS.Dir.exists marker |> Result.get_ok then begin
    Log.info (fun m -> m "Base layer cached");
    Ok (make_base_layer ~image ~base_dir)
  end else begin
    with_base_lock base_dir @@ fun () ->
    if Bos.OS.Dir.exists marker |> Result.get_ok then begin
      (* Another profile's ensure-base materialised the shared base
         while we waited for the lock. Mirror the fresh-build return
         ([image_for_hash]): the winner saved the digest, so hash by
         it when we have one. *)
      Log.info (fun m -> m "Base layer cached (built concurrently)");
      let image_for_hash = match digest with Some d -> d | None -> image in
      Ok (make_base_layer ~image:image_for_hash ~base_dir)
    end else begin
    Log.info (fun m -> m "Building base image from %s:%s"
      os_distribution os_version);
    let temp_dir = Bos.OS.Dir.tmp "day11_base_%s" |> Result.get_ok in
    (* The base image no longer bakes any opam-repository — builds mount
       a per-package slice at run time — so nothing repo-related needs
       staging into the Docker build context. *)
    (* Copy opam-build binary into Docker context if available *)
    let has_opam_build_bin =
      let cached = Fpath.(cache_dir / "opam-build-bin") in
      if Bos.OS.File.exists cached |> Result.get_ok then begin
        ignore (Sys.command (Printf.sprintf "cp %s %s"
          (Fpath.to_string cached)
          (Fpath.to_string Fpath.(temp_dir / "opam-build-bin"))));
        true
      end else false
    in
    let dockerfile =
      generate_dockerfile ~os_distribution ~os_version ~arch ~uid ~gid
        ~has_opam_build_bin ?digest ()
    in
    let dockerfile_path = Fpath.(temp_dir / "Dockerfile") in
    Bos.OS.File.write dockerfile_path
      (Dockerfile.string_of_t dockerfile) |> ignore;
    (* Docker build *)
    let tag = Printf.sprintf "day11-%s:%s" os_distribution os_version in
    let build_log = Fpath.(temp_dir / "docker-build.log") in
    Log.info (fun m -> m "Running docker build (tag: %s)" tag);
    (* Docker's containerd image store refuses to re-tag an existing image
       (the legacy builder silently overwrote), so drop any prior tag before
       this --no-cache rebuild. Harmless no-op when the tag doesn't exist. *)
    Day11_sys.Run.run ~sw env
      Bos.Cmd.(v "docker" % "image" % "rm" % "-f" % tag) None |> ignore;
    let build_run =
      Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "docker" % "build" % "--network=host" % "--no-cache"
                 % "-t" % tag % Fpath.to_string temp_dir)
        (Some build_log)
    in
    match build_run.status with
    | `Exited 0 ->
        Log.info (fun m -> m "Docker build succeeded, importing");
        (* Import the built image *)
        Bos.OS.Dir.create ~path:true base_dir |> ignore;
        let result =
          Day11_layer.Import.from_docker ~sw env ~image:tag ~layer_dir:base_dir
        in
        (* Clean up temp dir *)
        Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
        (* Clean up state cache files *)
        (match result with
         | Ok () ->
             ignore (Day11_sys.Sudo.run ~sw env
               Bos.Cmd.(v "sh" % "-c"
                        % Printf.sprintf "rm -f %s"
                            (Fpath.to_string
                               Fpath.(base_dir / "fs" / "home" / "opam"
                                      / ".opam" / "repo"
                                      / "state-*.cache"))));
             (* Save the digest so future loads use it for the hash *)
             (match digest with
              | Some d -> save_digest base_dir d
              | None -> ());
             let image_for_hash = match digest with
               | Some d -> d | None -> image in
             Ok (make_base_layer ~image:image_for_hash ~base_dir)
         | Error _ as e -> e)
    | `Exited n ->
        (* Preserve log and Dockerfile before deleting temp dir *)
        let saved_log = Fpath.(cache_dir / "docker-build-failed.log") in
        let saved_df = Fpath.(cache_dir / "Dockerfile.failed") in
        ignore (Bos.OS.File.read build_log |> Result.map (fun content ->
          ignore (Bos.OS.File.write saved_log content)));
        ignore (Bos.OS.File.read dockerfile_path |> Result.map (fun content ->
          ignore (Bos.OS.File.write saved_df content)));
        Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
        Rresult.R.error_msgf
          "Docker build failed (exit %d). Log saved to: %a\nSTDERR:\n%s"
          n Fpath.pp saved_log build_run.errors
    | `Signaled n ->
        Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
        Rresult.R.error_msgf "Docker build signaled %d" n
    end
  end
