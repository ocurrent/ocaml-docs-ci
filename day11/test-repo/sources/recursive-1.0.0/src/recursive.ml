(** {!Recursive} exercises odoc's rendering of [module rec]
    mutually-recursive modules. The xrefs between {!A.t} and
    {!B.t} form a cycle that odoc must resolve without infinite
    looping. *)

(** See {!B.t} for the partner type. *)
module rec A : sig
  (** Optional {!B.t}. *)
  type t = B.t option

  (** Fixed instance — [None]. *)
  val none : t
end = struct
  type t = B.t option
  let none = None
end

(** See {!A.t} for the partner type. *)
and B : sig
  (** An integer carrier. *)
  type t = int

  (** Fixed instance — [42]. *)
  val answer : t
end = struct
  type t = int
  let answer = 42
end
