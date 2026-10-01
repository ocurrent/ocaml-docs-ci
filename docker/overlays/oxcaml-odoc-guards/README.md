# oxcaml-odoc-guards

A tiny opam overlay for the `oxcaml-odoc-master` profile. It is layered
last, after the oxcaml repo and the odoc-master pin overlay.

With the oxcaml compiler installed, oxcaml/opam-repository allows odoc and
sherlodoc to be either not installed (`oxcaml-odoc.guard`,
`oxcaml-sherlodoc.guard`) or exactly `3.2.0+ox2` (the `-patches.enabled`
packages). That makes odoc master's builds (`3.2.1+master.…`) unsolvable.
These two files replace the `-patches.enabled` packages, relaxing `=` to
`>=`. They are otherwise copied unchanged from upstream.

The image bakes this directory in. docker/entrypoint.sh recreates it as a git
repo under `~/.day11/overlays/oxcaml-odoc-guards/repo` on every start, with a
fixed commit date so the SHA is stable. If upstream changes these
packages, re-copy them and re-apply the relaxation.
