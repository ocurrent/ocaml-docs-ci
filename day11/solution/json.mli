(** Solution persistence.

    Serializes dependency solutions (package → dependency set maps) to and from
    JSON. Used for caching solved results on disk and for inter-process
    communication. *)

type t = Deps.t
(** Alias for {!Deps.t}. *)

val to_json : t -> Yojson.Safe.t
(** Serialize a solution to JSON. *)

val of_json : Yojson.Safe.t -> (t, [> Rresult.R.msg ]) result
(** Deserialize a solution from JSON. Returns [Error] on malformed input. *)

val save : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [save path solution] writes the solution to a JSON file. *)

val load : Fpath.t -> (t, [> Rresult.R.msg ]) result
(** [load path] reads a solution from a JSON file. *)

val to_string : t -> string
(** Serialize a solution to a JSON string. *)

val of_string : string -> (t, [> Rresult.R.msg ]) result
(** Deserialize a solution from a JSON string. *)
