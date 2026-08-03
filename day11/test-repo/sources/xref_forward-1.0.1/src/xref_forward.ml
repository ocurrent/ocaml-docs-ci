(** {!Xref_forward} (1.0.1) — the 1.0.0 canary xref to {!Later.t}
    now resolves because the [Later] module is defined below. *)

(** See {!Later.t}. *)
type t = int

(** The previously-missing counterpart to {!t}. *)
module Later = struct
  (** A string-based partner type. *)
  type t = string

  (** A fixed instance. *)
  let value : t = "later"
end
