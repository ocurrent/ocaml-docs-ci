(** Packages that are doc-pipeline tools, never runtime deps. They are built
    once per [driver_compiler] in the tool layer
    ({!Day11_opam_build.Tools.plan_tool}) and mounted into doc containers by
    {!Day11_doc.Doc_build.make_tool_mounts}. They must not be solver roots in
    per-target solves: each target solve would pull a slightly different
    transitive closure (different [cmdliner] / [yojson] / [eio] / [progress]
    versions), inflating the doc-deps universe and forcing a fresh per-target
    build of an otherwise-shared tool.

    [odoc] and [odoc-parser] are deliberately {b not} on this list: [odoc]
    parses [.cmt]/[.cmti] files and must match the target's OCaml ABI, so it's
    already a per-compiler tool via [odoc_tool]. The names here are the
    compiler-agnostic ones — they exec [odoc] rather than linking against the
    compiler libraries. *)
let names = [ "odoc-driver"; "odoc-md"; "sherlodoc"; "odig" ]

let name_set =
  List.fold_left
    (fun acc n -> OpamPackage.Name.Set.add (OpamPackage.Name.of_string n) acc)
    OpamPackage.Name.Set.empty names

let is_tool_only name = OpamPackage.Name.Set.mem name name_set
