# runner — Run a command in a layered container

A small library that composes [`day11_layer`](../layer/) and
[`day11_container`](../container/) to run an OCI container in an
overlayfs rootfs assembled from a base layer plus a set of
dependency layers.

This is the intersection of "layered storage" and "container
runtime". The two leaf libraries deliberately don't depend on each
other; this library is the explicit place where they meet.

## What it does

```ocaml
val Run_in_layers.run :
  Eio_unix.Stdenv.base ->
  base:Day11_layer.Base.t ->
  build_dirs:Fpath.t list ->
  ?prep_upper:(upper:Fpath.t -> lowers:Fpath.t list -> unit) ->
  Day11_container.Oci_spec.t ->
  (Day11_exec.Run.t * Fpath.t * (string * float) list, _) result
```

The single function `Run_in_layers.run`:

1. Makes a temp dir with overlayfs upper / work / merged subdirs.
2. Marks every dep layer as recently used (LRU bookkeeping).
3. Decides whether to pass dep layers as separate overlayfs lowerdirs
   (the fast path) or to cp-merge some of them into a single lower dir
   (the fallback when the kernel mount-options string would overflow
   PAGE_SIZE). Uses `Day11_layer.Stack.plan_lowerdir` for the
   decision.
4. Calls the optional `prep_upper` callback so the caller can seed
   the upper with whatever domain-specific files / chowns / mkdirs
   the container needs (e.g. an opam switch-state file, a
   `/home/$USER/odoc-out` mount point).
5. Mounts overlayfs at the temp dir's `merged/` subdir.
6. Instantiates the caller's `Oci_spec.t` template with the merged
   path as the rootfs and writes `config.json` into the bundle.
7. Runs runc, with `Fun.protect` cleanup.
8. Unmounts the overlay and removes everything except `upper/`.
9. Returns the run result, the upper directory, and a per-phase
   timing alist.

## What it does NOT do

- Does not write `layer.json` or any sidecar metadata. That's the
  caller's job — the caller takes ownership of the returned
  `upper` and decides what to do with it.
- Does not know about opam, opam switches, opam package builds, or
  documentation. The opam-flavoured spec defaults (cwd `/home/opam`,
  bash wrapping, network on, etc.) live in `Day11_opam_build.Build_layer`,
  not here.
- Does not know about cache layout, layer naming conventions, dep
  DAGs, or universes. Those are layer/opam concerns.

## Dependencies

```
(libraries day11_container day11_sys day11_layer
           bos eio fpath logs rresult unix)
```

Notably no `day11_opam_layer`, no `opam-format`, no `day11_opam_build`.
This library can be linked from any consumer that wants the
"layered container runner" primitive.

## Why a separate library

`Run_in_layers` is the intersection of `day11_layer` and
`day11_container`, which deliberately don't depend on each other:

- `day11_layer` is the on-disk data layer (read sidecars, plan
  overlayfs lowerdirs, do cp-merge); a doc-browsing tool can
  consume it without pulling in runc.
- `day11_container` is the OCI runtime wrapper (Overlay, Runc,
  Oci_spec); a non-day11 container runner can consume it without
  pulling in the cache format.

Putting `Run_in_layers` into either of those libraries would force
that library to depend on the other, breaking its isolation.
A separate `day11_runner` library is the right home: it depends
on both, and is itself depended on by callers that need the
combined primitive (currently `day11_opam_build`).

## Testing

There are no unit tests in this library — the function is too
end-to-end to be meaningfully tested without runc and sudo.
Integration tests live in `day11/container/test/test_integration.ml`
where they exercise overlayfs mounts and runc directly. Those
tests cover the same kernel/runc interactions
`Run_in_layers.run` performs, just one step lower in the stack.
