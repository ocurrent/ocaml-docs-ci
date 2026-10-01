#!/bin/sh
# Container entrypoint: prepare ~/.day11 from what the image ships, then exec
# the command (the daemon, or a one-off CLI run). See the Dockerfile.
set -e

# TMPDIR lives under the volume, so it can't be created at image-build time.
mkdir -p "$TMPDIR" "$HOME/.day11/profiles" "$HOME/.day11/overlays"

# The image is the source of truth for the shipped profiles: the named
# volume was only seeded from the image when it was first created.
cp -f "$HOME"/profiles-image/*.json "$HOME/.day11/profiles/"

# Baked opam overlays. Profiles track repos by git commit, so each one is
# recreated as a git repo. The commit identity and date are fixed, so an
# unchanged tree gets the same SHA on every start and doesn't look like a
# repo change to the pipeline.
for src in "$HOME"/overlays-image/*/; do
  [ -d "$src" ] || continue
  name=$(basename "$src")
  dst="$HOME/.day11/overlays/$name/repo"
  rm -rf "$dst"
  mkdir -p "$dst"
  cp -R "$src". "$dst"/
  (
    cd "$dst"
    git init -q -b master
    git add -A
    GIT_AUTHOR_DATE="2000-01-01T00:00:00Z" GIT_COMMITTER_DATE="2000-01-01T00:00:00Z" \
      git -c user.name=ocaml-docs-ci -c user.email=docs-ci@localhost \
      commit -q -m "baked overlay $name"
  )
done

exec "$@"
