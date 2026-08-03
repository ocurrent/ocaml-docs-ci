(** {!Inline_include} exercises odoc's rendering of [include M]
    signatures — the included module's bindings should appear
    inline in the parent's documentation, ideally linked back to
    their definition. *)

(** The base contents. *)
module Base = struct
  (** An alpha alias. *)
  type alpha = int

  (** Greet. *)
  let greet () = "hello from Base"
end

(** Lift everything in {!Base} into this module's signature. *)
include Base

(** An additional binding not from {!Base}. *)
let extra = "extra"
