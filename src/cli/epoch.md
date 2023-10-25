# Epoch management tool

`epoch` - command line tool for managing epochs and storage in ocaml-docs-ci

What is an Epoch?

Directory structure

CLI tool can be installed as `epoch` in the current opam switch.

```sh
$ dune install epoch
...
[1]
```

The primary use of epoch is to trim the directories that exist in `prep` and `compile` that are no longer linked from an `epoch-*`. These directories can accumulate many Gb of data, causing ocaml-docs-ci pipelines to fail with not enough disk space.

```sh
$ mkdir -d -p tmp
...
$ epoch --base-dir ./tmp
```