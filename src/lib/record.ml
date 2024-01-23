module Git = Current_git

module Record = struct
  type t = No_context

  module Value = struct
    type t = int

    let marshal t = `Int t |> Yojson.Safe.to_string

    let unmarshal t =
      let json = Yojson.Safe.from_string t in
      json |> Yojson.Safe.Util.to_int
  end

  module Key = struct
    type t = { voodoo : Voodoo.t; config : Config.t }

    let key { voodoo; config = _} =
      let t = Epoch.v voodoo in
      Fmt.str "%a" Epoch.pp t

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let id = "record-pipeline"

  let build No_context (job : Current.Job.t) Key.{ config; voodoo } =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Harmless job in

    let generation = Epoch.v voodoo in
    let voodoo_do_commit = Voodoo.Do.v voodoo |> Voodoo.Do.digest in
    let voodoo_gen_commit =
      Voodoo.Gen.v voodoo |> Voodoo.Gen.commit |> Git.Commit_id.hash
    in
    let voodoo_repo = Config.voodoo_repo config in
    let voodoo_branch = Config.voodoo_branch config in
    let voodoo_prep_commit = Voodoo.Prep.v voodoo |> Voodoo.Prep.digest in
    let epoch_linked = (Epoch.digest `Linked) generation in
    let epoch_html = (Epoch.digest `Html) generation in

    let result =
      Index.record_new_pipeline ~voodoo_do_commit ~voodoo_gen_commit
        ~voodoo_prep_commit ~voodoo_repo ~voodoo_branch ~epoch_html
        ~epoch_linked
    in
    match result with
    | Ok pipeline_id -> Lwt.return_ok (pipeline_id |> Int64.to_int)
    | Error msg -> Lwt.return_error (`Msg msg)

  let pp f Key.{ config = _; voodoo } =
    let generation = Epoch.v voodoo in
    Epoch.pp f generation

  let auto_cancel = true
end

module RecordCache = Current_cache.Make (Record)

let v config voodoo =
  let open Current.Syntax in
  Current.component "record"
  |> let> voodoo in
     let output = RecordCache.get No_context Record.Key.{ config; voodoo } in
     Current.Primitive.map_result
       (Result.map (fun pipeline_id -> pipeline_id))
       output
