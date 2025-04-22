let v config =
  let generation = Epoch.v config in
  let odoc_commit = Config.odoc config in
  let epoch_linked = Epoch.digest generation in
  let epoch_html = Epoch.digest generation in

  let result =
    Index.record_new_pipeline ~odoc_commit ~epoch_html ~epoch_linked
  in
  match result with
  | Ok pipeline_id -> Lwt.return_ok (pipeline_id |> Int64.to_int)
  | Error msg -> Lwt.return_error (`Msg msg)
