(** {!Xref_intra} exercises odoc's intra-library xref resolution.
    The docstrings on {!B.t} reference {!A.t} defined in the same
    compilation unit. *)

module A = struct
  (** The alpha type — an alias for [int]. *)
  type t = int
end

module B = struct
  (** A list of {!A.t} values. *)
  type t = A.t list

  (** Sum the elements. *)
  let sum (xs : t) = List.fold_left ( + ) 0 xs
end
