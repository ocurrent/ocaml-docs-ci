(** Extract tool binaries for bind-mounting into containers.

    Instead of stacking all tool build layers (which can be 90+ layers), we just
    bind-mount the specific binaries we need. *)

val find_binary : Day11_opam_layer.Tool.t -> string -> Fpath.t option
(** [find_binary tool name] finds a binary called [name] in the tool layer's
    installed binaries. Returns [None] if not found. *)

val doc_tool_mounts :
  Day11_opam_layer.Tool.t -> Day11_container.Mount.t list * string * string
(** [doc_tool_mounts tool] returns [(mounts, odoc_bin, odoc_md_bin)] where
    [mounts] are bind mounts for the tool binaries and [odoc_bin]/[odoc_md_bin]
    are the container paths. *)
