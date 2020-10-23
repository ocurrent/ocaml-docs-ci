#!/bin/bash
set -e

cd $1
eval $(opam env)
mirage configure -t $2
opam monorepo lock
opam monorepo pull
dune build
