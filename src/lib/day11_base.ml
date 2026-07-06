(** Wrap [Day11_batch.Profile_ctx.ensure_base] as a Current_cache
    op so the Docker build + import show up at [/jobs] with live
    logs. Downstream pipeline stages can then depend on the op's
    [unit Current.t] output instead of polling a background-fiber
    var. *)

module Profile_ctx = Day11_batch.Profile_ctx

module Op = struct
  type t = {
    env : Eio_unix.Stdenv.base;
    ctx : Profile_ctx.t;
  }

  module Key = struct
    type t = {
      profile_name : string;
      image_digest : string;
      opam_build_repo : string option;
    }

    (* Keyed by profile name, base image digest, and the profile's
       [opam_build_repo] setting. When the upstream image changes
       (periodic [docker manifest inspect] picks up a new digest),
       or when the profile switches opam-build source (e.g. from
       upstream master to a local checkout), the key changes and
       the op re-runs — which is what makes the per-profile
       [opam-build-bin-<key>] cache actually get populated. *)
    let digest t =
      let obr = match t.opam_build_repo with
        | None -> "upstream"
        | Some p -> "local:" ^ p
      in
      t.profile_name ^ ":" ^ t.image_digest ^ ":" ^ obr
  end

  module Value = struct
    type t = unit

    let marshal () = "{}"
    let unmarshal _ = ()
  end

  let id = "day11-ensure-base"
  let auto_cancel = false

  let pp f (key : Key.t) =
    Fmt.pf f "ensure-base %s" key.profile_name

  let build (op_ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Average in
    Current.Job.log job
      "Ensuring base image for profile %s (digest %s)"
      key.profile_name key.image_digest;
    Lwt_eio.run_eio @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let ctx = Profile_ctx.with_base_digest op_ctx.ctx key.image_digest in
    match Profile_ctx.ensure_base ~sw op_ctx.env ctx with
    | Ok _ready ->
      Current.Job.log job "Base image ready";
      Ok ()
    | Error (`Msg msg) ->
      Current.Job.log job "Base image build failed: %s" msg;
      Error (`Msg msg)
end

module Cache = Current_cache.Make (Op)

(* Force [ensure] to re-run for [ctx] at the given [image_digest].

   The ensure-base success is keyed on profile + image digest and does
   not track whether the base dir still exists on disk. If the base is
   removed out-of-band (an os_dir migration, a cache wipe) the stale
   success makes OCurrent skip the rebuild, and every dependent build
   then fails [require_base]. Callers invalidate when they observe the
   base dir missing so the cached success is rebuilt rather than trusted.
   Sets [rebuild=1] in the cache db (persisted). *)
let invalidate ~image_digest (ctx : Profile_ctx.t) =
  Cache.invalidate
    Op.Key.{
      profile_name = ctx.profile.name;
      image_digest;
      opam_build_repo = ctx.profile.opam_build_repo;
    }

let ensure ~env ~digest (ctx : Profile_ctx.t) : unit Current.t =
  let open Current.Syntax in
  Current.component "ensure-base %s" ctx.profile.name |>
  let> image_digest = digest in
  Cache.get { env; ctx }
    Op.Key.{
      profile_name = ctx.profile.name;
      image_digest;
      opam_build_repo = ctx.profile.opam_build_repo;
    }
