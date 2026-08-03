(** Route table for the dashboard pages.

    The host wires this in alongside its own routes by appending [routes ~ctx]
    to the [Routes.[ ... ]] list passed to [Current_web.Site.v]. *)

module Resource = Current_web.Resource

let routes ~(ctx : Pages.ctx) =
  let p = Pages.{ profile_dir = ctx.profile_dir; cache_dir = ctx.cache_dir } in
  let open Routes in
  [
    (s "profiles" /? nil) @--> (Pages.profiles_index ~ctx:p :> Resource.t);
    ( (s "profiles" / str /? nil) @--> fun name ->
      (Pages.profile_dashboard ~ctx:p name :> Resource.t) );
    ( (s "profiles" / str / s "snapshots" /? nil) @--> fun name ->
      (Pages.snapshots_list ~ctx:p name :> Resource.t) );
    ( (s "profiles" / str / s "recent" /? nil) @--> fun name ->
      (Pages.recent_changes ~ctx:p name :> Resource.t) );
    ( (s "profiles" / str / s "snapshots" / str /? nil) @--> fun name key ->
      (Pages.snapshot_detail ~ctx:p name key :> Resource.t) );
    ( (s "profiles" / str / s "snapshots" / str / s "diff" / str /? nil)
    @--> fun name key_old key_new ->
      (Pages.snapshot_diff ~ctx:p name key_old key_new :> Resource.t) );
    ( (s "profiles" / str / s "p" / str /? nil) @--> fun name pkg ->
      (Pages.package_index ~ctx:p name pkg :> Resource.t) );
    ( (s "profiles" / str / s "p" / str / str /? nil) @--> fun name pkg ver ->
      (Pages.package_version ~ctx:p name pkg ver :> Resource.t) );
    ( (s "profiles" / str / s "u" / str /? nil) @--> fun name hash ->
      (Pages.universe_page ~ctx:p name hash :> Resource.t) );
    ( (s "profiles" / str / s "builds" / str / s "log" /? nil)
    @--> fun name hash -> (Pages.build_log_view ~ctx:p name hash :> Resource.t)
    );
    ( (s "profiles" / str / s "docs" /? wildcard) @--> fun name parts ->
      let tail = Parts.wildcard_match parts in
      (Static_docs.resource ~profile_dir:ctx.profile_dir name tail
        :> Resource.t) );
  ]
