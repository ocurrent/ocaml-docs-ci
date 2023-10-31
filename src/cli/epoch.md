# Epoch management tool

`epoch` - command line tool for managing epochs and storage in ocaml-docs-ci

_What is an Epoch?_ An Epoch is a collection of package documentation artifacts that are compatiable with each other. They either contain:

  * compiled html for use in ocaml.org aka `html-epoch`
  * intermediate OCaml artifacts used to generate docs `linked-epoch`

A typical directory structure is:

```shell skip
$ tree -L 2
.
├── compile
│   ├── p
│   └── u
├── content.current
├── content.live
├── epoch-097e46a4d589b9e34ed2903beecd1a04
│   └── html-raw
├── epoch-410108220dc0168ea4d9bd697dfa8e34
│   └── linked
├── epoch-5daeecab2ad7a2d07a12742d4cc0ab6f
│   └── linked
├── epoch-ae8bf595b8594945ee40c58377e03730
│   └── html-raw
├── html-current -> /data/epoch-3d6c8218acb41c692c8219169dcb77df
├── html-current.log
├── html-live -> /data/epoch-097e46a4d589b9e34ed2903beecd1a04
├── html-live.log
├── linked
├── linked-current -> /data/epoch-19384d079d5e686e2887866602764c38
├── linked-current.log
├── linked-live -> /data/epoch-410108220dc0168ea4d9bd697dfa8e34
├── linked-live.log
├── live -> html-live
└── prep
    └── universes

```
The primary use of epoch is to trim the directories that exist in `prep` and `compile` that are no longer linked from an active `epoch-*`. These directories can accumulate many Gb of data, causing ocaml-docs-ci pipelines to fail with not enough disk space.

CLI tool can be installed as `epoch` in the current opam switch.

```sh
$ dune install epoch
...
[1]
```

It is distributed in the `infra_storage-server` docker image and can be run as:
```shell skip
DATA=$(docker volume inspect infra_docs-data -f '{{.Mountpoint}}')
$ epoch --base-dir $DATA --dry-run

# Will print out the directories it has found to be deleted.

$ epoch --base-dir $DATA

# Will delete the directories it has found.
```
