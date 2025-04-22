type solve_result = Solver_api.Worker.solve_result = {
  compile_universes : (string * string * string list) list;
  link_universes : (string * string * string list) list;
}
[@@deriving yojson]

val test : Git_unix.Store.Hash.t -> unit
(** [test hash] runs a test with commit [hash] in opam-repository. *)

val main : Git_unix.Store.Hash.t -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes
    results to stdout, using commit [hash] in opam-repository. *)
