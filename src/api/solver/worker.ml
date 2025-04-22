(** Communication between ocaml-ci and the workers. *)

(** Variables describing a build environment. *)
module Vars = struct
  type t = {
    arch : string;
    os : string;
    os_family : string;
    os_distribution : string;
    os_version : string;
  }
  [@@deriving yojson]
end

type solve_result = {
  compile_universes : (string * string * string list) list;
  link_universes : (string * string * string list) list;
}
[@@deriving yojson, ord]

(** A set of packages for a single build. *)
module Selection = struct
  type t = {
    id : string;  (** The platform ID from the request. *)
    packages : solve_result;
        (** The selected packages ("name.version") and their universes. *)
    commit : string;  (** A commit in opam-repository to use. *)
  }
  [@@deriving yojson, ord]
end

(** A request to select sets of packages for the builds. *)
module Solve_request = struct
  type rel = [ `Eq | `Geq | `Gt | `Leq | `Lt | `Neq ] [@@deriving yojson]

  type t = {
    opam_repository_commit : string;  (** Commit in opam repository to use. *)
    pkgs : string list;  (** Name of packages to solve. *)
    constraints : (string * rel * string) list;  (** Version locks *)
    platforms : (string * Vars.t) list;  (** Possible build platforms, by ID. *)
  }
  [@@deriving yojson]

  let pp_constraint ppf (pkg, rel, vsn) =
    Format.fprintf ppf "%s%s%s" pkg
      (match rel with
      | `Eq -> "="
      | `Geq -> ">="
      | `Gt -> ">"
      | `Leq -> "<="
      | `Lt -> "<"
      | `Neq -> "<>")
      vsn
end

(** The response from the solver. *)
module Solve_response = struct
  type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
  [@@deriving yojson]

  type t = (Selection.t list, [ `Msg of string ]) result [@@deriving yojson]
end
