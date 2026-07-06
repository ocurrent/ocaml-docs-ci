type t = {
  profile : Profile.t;
  cache_dir : Fpath.t;
  os_dir : Fpath.t;
  git_packages : Day11_opam.Git_packages.t;
  repos_with_shas : (string * string) list;
  opam_env : string -> OpamVariable.variable_contents option;
  ocaml_version : OpamPackage.t option;
  driver_compiler : OpamPackage.t option;
  patches : Day11_opam_build.Patches.t option;
  base : Day11_layer.Base.t;
  benv : Day11_opam_build.Types.build_env;
  hash_cache : Day11_opam_build.Hash_cache.t;
}

let parse_ocaml_version = function
  | None | Some "" -> None
  | Some s -> Some (OpamPackage.of_string s)

let image_of_profile (profile : Profile.t) =
  match profile.base_image_digest with
  | Some d -> d
  | None ->
    Printf.sprintf "%s:%s" profile.os_distribution profile.os_version

let finalise_load (profile : Profile.t) ~cache_dir
    git_packages repos_with_shas =
  let os_dir = Fpath.(cache_dir / Profile.os_dir_name profile) in
  (* ocaml-git clobbers Bos's temp dir default; reset for downstream
     callers that use Bos.OS.Dir.tmp. *)
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:profile.arch
    ~os:"linux"
    ~os_distribution:profile.os_distribution
    ~os_family:profile.os_distribution
    ~os_version:profile.os_version
    ()
  in
  let ocaml_version = parse_ocaml_version profile.compiler in
  let driver_compiler =
    if profile.driver_compiler = "" then None
    else Some (OpamPackage.of_string profile.driver_compiler)
  in
  let patches = Option.map
    (fun dir -> Day11_opam_build.Patches.create (Fpath.v dir))
    profile.patches_dir
  in
  let base_dir = Day11_opam_build.Base.base_dir_of_os_dir os_dir in
  let image = image_of_profile profile in
  let base : Day11_layer.Base.t =
    { hash = Day11_opam_build.Base.build_hash
        ~os_distribution:profile.os_distribution
        ~os_version:profile.os_version
        ~arch:profile.arch
        ?digest:profile.base_image_digest ();
      dir = base_dir;
      image }
  in
  let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir () in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let hash_cache = Day11_opam_build.Hash_cache.create ~find_opam ?patches () in
  { profile; cache_dir; os_dir;
    git_packages; repos_with_shas; opam_env;
    ocaml_version; driver_compiler; patches;
    base; benv; hash_cache }

let load (profile : Profile.t) ~cache_dir =
  let repos_with_heads =
    List.map (fun r -> (r, None)) profile.opam_repositories in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories repos_with_heads in
  finalise_load profile ~cache_dir git_packages repos_with_shas

let load_lwt (profile : Profile.t) ~cache_dir =
  let open Lwt.Infix in
  let repos_with_heads =
    List.map (fun r -> (r, None)) profile.opam_repositories in
  Day11_opam.Git_packages.of_repositories_lwt repos_with_heads
  >|= fun (git_packages, repos_with_shas) ->
  finalise_load profile ~cache_dir git_packages repos_with_shas

let base_materialised (base : Day11_layer.Base.t) =
  let dir = base.Day11_layer.Base.dir in
  let marker = Fpath.(dir / "fs" / "usr") in
  Bos.OS.Dir.exists marker |> Result.value ~default:false

let rebuild_base_with ~base ctx =
  { ctx with base;
             benv = Day11_opam_build.Types.make_build_env ~base
               ~os_dir:ctx.os_dir ~uid:ctx.benv.uid ~gid:ctx.benv.gid
               ?cpu_slots:ctx.benv.cpu_slots () }

let with_cpu_slots ctx pool =
  { ctx with benv = Day11_opam_build.Types.make_build_env
               ~base:ctx.base ~os_dir:ctx.os_dir
               ~uid:ctx.benv.uid ~gid:ctx.benv.gid
               ~cpu_slots:pool () }

let with_base_digest ctx digest =
  let profile = { ctx.profile with base_image_digest = Some digest } in
  let base_dir = ctx.base.dir in
  let base : Day11_layer.Base.t =
    { hash = Day11_opam_build.Base.build_hash
        ~os_distribution:profile.os_distribution
        ~os_version:profile.os_version
        ~arch:profile.arch
        ~digest ();
      dir = base_dir;
      image = digest }
  in
  rebuild_base_with ~base { ctx with profile }

let ensure_base ~sw env ctx =
  (* Build the per-profile [opam-build] binary first (idempotent if
     cached). It's mounted at container-run time and overrides the
     binary that gets baked into the base image, so profiles with
     different [opam_build_repo] settings don't step on each other
     even when they share a base image. *)
  let opam_build_repo =
    Option.map Fpath.v ctx.profile.opam_build_repo in
  match
    Day11_opam_build.Base.build_opam_build ~sw env
      ~cache_dir:ctx.cache_dir ~arch:ctx.profile.arch
      ?opam_build_repo ()
  with
  | Error _ as e -> e
  | Ok _ ->
    if base_materialised ctx.base then Ok ctx
    else begin
      let uid = Unix.getuid () and gid = Unix.getgid () in
      (* The base image is repo-agnostic now (empty [default] repo;
         per-package slices are mounted at build time), so it takes no
         [opam_repositories]. *)
      match Day11_opam_build.Base.build ~sw env ~cache_dir:ctx.cache_dir
              ~os_distribution:ctx.profile.os_distribution
              ~os_version:ctx.profile.os_version
              ~arch:ctx.profile.arch
              ~uid ~gid
              ?digest:ctx.profile.base_image_digest ()
      with
      | Ok base -> Ok (rebuild_base_with ~base ctx)
      | Error _ as e -> e
    end

let require_base ctx =
  if base_materialised ctx.base then Ok ctx
  else
    Rresult.R.error_msgf
      "No base image for profile %s — run 'day11 batch' first"
      ctx.profile.name
