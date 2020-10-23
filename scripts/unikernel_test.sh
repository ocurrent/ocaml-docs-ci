#!/bin/sh

cd $1
mirage configure -t $2
opam monorepo lock
opam monorepo pull
dune build
