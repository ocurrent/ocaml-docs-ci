#!/bin/bash
set -e

cd $1
eval $(opam env)
opam pin sexplib v0.14.0 -n
mirage configure -t $2 --extra-repo=https://github.com/dune-universe/opam-overlays.git
make depend
dune build
