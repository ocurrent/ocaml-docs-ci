(** OCurrent op that resolves the current registry digest of a Docker image tag
    on a periodic schedule.

    Output is the per-architecture [sha256:...] manifest digest of [IMAGE_TAG].
    Downstream pipeline stages feed this digest into
    {!Day11_batch.Profile_ctx.with_base_digest}, so the base layer hash — and
    therefore every dependent layer hash — picks up any upstream image change
    automatically. No manual [day11 profile refresh-base] needed.

    Runs [docker manifest inspect TAG], parses the multi-arch manifest, and
    picks the entry matching the requested arch. *)

module Op = struct
  type t = unit

  module Key = struct
    type t = { image : string; arch : string (* opam arch, e.g. "x86_64" *) }

    let digest t = t.image ^ "|" ^ t.arch
  end

  module Value = struct
    type t = string (* sha256:... *)

    let marshal t = t
    let unmarshal t = t
  end

  let id = "day11-base-digest"
  let auto_cancel = false
  let pp f (key : Key.t) = Fmt.pf f "resolve %s (%s)" key.image key.arch

  let docker_arch = function
    | "x86_64" | "amd64" -> "amd64"
    | "aarch64" -> "arm64"
    | a -> a

  let build () job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    Current.Job.log job "docker manifest inspect %s (arch=%s)" key.image
      key.arch;
    (* Use Lwt_process so stdout/stderr flow into the OCurrent job
       log in real time. *)
    let cmd = ("", [| "docker"; "manifest"; "inspect"; key.image |]) in
    let* output = Lwt_process.pread ~stderr:`Dev_null cmd in
    try
      let json = Yojson.Safe.from_string output in
      let open Yojson.Safe.Util in
      let target = docker_arch key.arch in
      let manifests = json |> member "manifests" |> to_list in
      let pick =
        List.find_map
          (fun m ->
            let plat = m |> member "platform" in
            let m_arch = plat |> member "architecture" |> to_string in
            let m_os = plat |> member "os" |> to_string in
            if m_arch = target && m_os = "linux" then
              Some (m |> member "digest" |> to_string)
            else None)
          manifests
      in
      match pick with
      | Some d ->
          Current.Job.log job "Resolved digest: %s" d;
          Lwt.return (Ok d)
      | None ->
          Lwt.return
            (Error
               (`Msg
                  (Printf.sprintf "No %s/linux manifest in %s" target key.image)))
    with exn ->
      Lwt.return
        (Error
           (`Msg
              (Printf.sprintf "Failed to parse manifest JSON for %s: %s"
                 key.image (Printexc.to_string exn))))
end

module Cache = Current_cache.Make (Op)

(** Resolve the digest of [image] for [arch], refreshed per [schedule].

    [Current.cutoff]: a scheduled re-resolve that returns the same digest string
    must not re-fire downstream — without it, every daily refresh re-plans the
    entire profile even when the upstream image is unchanged. See
    {!Docs_ci_lib.Remote_opam_repo.maintain} for the mechanics. *)
let current ~schedule ~image ~arch : string Current.t =
  let open Current.Syntax in
  Current.component "base-digest %s" image
  |> (let> () = Current.return () in
      Cache.get ~schedule () Op.Key.{ image; arch })
  |> Current.cutoff ~eq:String.equal
