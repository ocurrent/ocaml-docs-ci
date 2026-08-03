(** {!Functor} exercises odoc's functor rendering. Defines a
    signature [S], a functor [Make] producing a describer, and a
    concrete instance [Int_inst]. The generated doc should show
    the functor argument and body plus the instance's derived
    bindings. *)

(** Signature of a named thing. *)
module type S = sig
  (** The carrier type. *)
  type t

  (** Name a value. *)
  val name : t -> string
end

(** [Make(M)] lifts an [S]-implementation into a describer. *)
module Make (M : S) = struct
  (** Describe a value using [M.name]. *)
  let describe (x : M.t) = "item: " ^ M.name x
end

(** Integer instance produced by applying {!Make}. *)
module Int_inst = Make (struct
  type t = int
  let name = string_of_int
end)
