(** Build configuration persistence.

    Saves batch run parameters so rerun/cascade commands don't need CLI
    arguments. *)

type t = {
  opam_repositories : Fpath.t list;
  local_repos : (Fpath.t * string list) list;
  with_doc : bool;
  with_jtw : bool;
  html_output : Fpath.t option;
  jtw_output : Fpath.t option;
}

val save : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [save path config] writes [build-config.json]. *)

val load : Fpath.t -> (t, [> Rresult.R.msg ]) result
(** [load path] reads [build-config.json]. *)
