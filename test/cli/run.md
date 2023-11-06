# Epoch management tool

`epoch` - command line tool for managing epochs and storage in ocaml-docs-ci

Epoch tool provides version information about git version it was built with:
```sh
$ epoch --version
n/a
```

Epoch provides a manpage with help options:
```sh
$ epoch --help
NAME
       epoch - Epoch pruning

SYNOPSIS
       epoch [--base-dir=BASE_DIR] [--dry-run] [-s] [OPTION]…

OPTIONS
       --base-dir=BASE_DIR (required)
           Base directory containing epochs. eg
           /var/lib/docker/volumes/infra_docs-data/_data

       --dry-run
           If set, only list the files to be deleted but do not deleted them

       -s  Run epoch tool silently emitting no progress bars.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       epoch exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

```

The primary use of epoch is to trim the directories that exist in `prep` and `compile` that are no longer linked from an `epoch-*`. These directories can accumulate many Gb of data, causing ocaml-docs-ci pipelines to fail with not enough disk space.

Running the tests should delete orphan universes and leave linked universe alone:
```sh
$ ./run
Creating linked universe bf6f7d00b40806e7dd74ad1828a0aa6d
Creating linked universe 7ee85f63014c898d8cb21b3436d42150
Created orphan universe 3e4e2c1d81edea2e42fbfaba428f5965
Created orphan universe 5e2dcd36d81e7c2394110782b5bf906f
Files to be deleted in prep/universes
3e4e2c1d81edea2e42fbfaba428f5965
5e2dcd36d81e7c2394110782b5bf906f
Deleting 2 files in prep/universes
Files to be deleted in compile/u
3e4e2c1d81edea2e42fbfaba428f5965
5e2dcd36d81e7c2394110782b5bf906f
Deleting 2 files in compile/u
```
