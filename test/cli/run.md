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
$ ./run
+ EPOCH_BIN='dune exec -- epoch'
+ dune exec -- epoch --version
n/a
+ dune exec -- epoch --help
EPOCH(1)                         Epoch Manual                         EPOCH(1)

NNAAMMEE
       epoch - Epoch pruning

SSYYNNOOPPSSIISS
       eeppoocchh [----bbaassee--ddiirr=_B_A_S_E___D_I_R] [_O_P_T_I_O_N]…

OOPPTTIIOONNSS
       ----bbaassee--ddiirr=_B_A_S_E___D_I_R (required)
           Base directory containing epochs. eg
           /var/lib/docker/volumes/infra_docs-data/_data

CCOOMMMMOONN OOPPTTIIOONNSS
       ----hheellpp[=_F_M_T] (default=aauuttoo)
           Show this help in format _F_M_T. The value _F_M_T must be one of aauuttoo,
           ppaaggeerr, ggrrooffff or ppllaaiinn. With aauuttoo, the format is ppaaggeerr or ppllaaiinn
           whenever the TTEERRMM env var is dduummbb or undefined.

       ----vveerrssiioonn
           Show version information.

EEXXIITT SSTTAATTUUSS
       eeppoocchh exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

Epoch n/a                                                             EPOCH(1)
++ mktemp -d
+ EPOCH_DATA_TEMP=/var/folders/_l/v2016jrx2kndvkdf6p9phy_80000gn/T/tmp.BZBvj9Cj
+ trap 'rm -rf "/var/folders/_l/v2016jrx2kndvkdf6p9phy_80000gn/T/tmp.BZBvj9Cj"' EXIT
+ rm -rf /var/folders/_l/v2016jrx2kndvkdf6p9phy_80000gn/T/tmp.BZBvj9Cj
```
