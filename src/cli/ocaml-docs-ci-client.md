# Ocaml-docs-ci-client CLI

ocaml-docs-ci-client - command line tool for interacting with ocaml-docs-ci.

Running the default command displays basic usage.
```sh
$ ocaml-docs-ci-client
ocaml-docs-ci-client: required COMMAND name is missing, must be one of 'diff-pipelines', 'health-check', 'status', 'status-by-pipeline' or 'steps'.
Usage: ocaml-docs-ci-client COMMAND …
Try 'ocaml-docs-ci-client --help' for more information.
[124]
```

Runnning the help command brings up the manpage.

```sh
$ ocaml-docs-ci-client --help=plain
NAME
       ocaml-docs-ci-client - Cli client for ocaml-docs-ci.

SYNOPSIS
       ocaml-docs-ci-client COMMAND …

DESCRIPTION
       Command line client for ocaml-docs-ci.

COMMANDS
       diff-pipelines [--ci-cap=CAP] [--dry-run] [OPTION]…
           Packages that have started failing in the latest pipeline.

       health-check [--ci-cap=CAP] [--dry-run] [OPTION]…
           Information about a pipeline.

       status [--ci-cap=CAP] [--dry-run] [--package=package] [OPTION]…
           Build status of a package.

       status-by-pipeline [--ci-cap=CAP] [--dry-run] [--package=package]
       [OPTION]…
           Build status of a package in the two most recent pipeline runs.

       steps [--ci-cap=CAP] [--dry-run] [--package=package] [OPTION]…
           Build steps of a package.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       ocaml-docs-ci-client exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

```

Running the status command queries the current status of a package, showing all versions, or a specific package version.


```sh
$ ocaml-docs-ci-client status --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI" --package="fmt"
...
$ ocaml-docs-ci-client status --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI" --package="fmt" --version="0.9.0"
...
```

You can query the specific steps for a package version as:

```sh
$ ocaml-docs-ci-client steps --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI" --package="fmt" --version="0.9.0"
...
```

Health check shows meta-data about the last 2 pipeline runs. It prints out the voodoo commit SHAs, epochs, and the number of failing packages, passing packages and running packages for the latest and latest-but-one pipelines.
```sh skip
$ ocaml-docs-ci-client health-check --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI"
...
```

```sh skip
$ ocaml-docs-ci-client health-check --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI" | jq .
{
  "latest": {
    "epoch_html": "3d6c8218acb41c692c8219169dcb77df",
    "epoch_linked": "19384d079d5e686e2887866602764c38",
    "voodoo_do": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_prep": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_gen": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "odoc": "https://github.com/ocaml/odoc.git#34a48e2543f6ea5716e9ee922954fa0917561dd7",
    "voodoo_repo": "https://github.com/ocaml-doc/voodoo.git",
    "voodoo_branch": "main",
    "failed_packages": 25255,
    "running_packages": 30,
    "passed_packages": 0
  },
  "latest-but-one": {
    "epoch_html": "097e46a4d589b9e34ed2903beecd1a04",
    "epoch_linked": "410108220dc0168ea4d9bd697dfa8e34",
    "voodoo_do": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_prep": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_gen": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "odoc": "https://github.com/ocaml/odoc.git#b4f11fcff450691a74987a3bf1131f0a52154cc3",
    "voodoo_repo": "https://github.com/ocaml-doc/voodoo.git",
    "voodoo_branch": "main",
    "failed_packages": 1205,
    "running_packages": 126,
    "passed_packages": 23954
  }
}
```

Diff pipelines shows the changes that have happened between two pipeline runs (epochs), showing new packages added or package documentation that has failed to build.
This is useful to understand the health of the current pipeline and whether it can be promoted to live (and used by ocaml.org).

```sh
$ ocaml-docs-ci-client diff-pipelines --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI"
...
```

We can then query the difference between specific packages in the last two pipeline runs:

```sh
$ ocaml-docs-ci-client status-by-pipeline --dry-run --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI"
...
```

For example on live pipeline it might show this for the `lwt` package:
```sh skip

$ ocaml-docs-ci-client status-by-pipeline --ci-cap="capnp://sha-256:lsLPZ6Q4jYcTxiitvBg02B3xfds7KwwJ4FIptUe2qew@localhost:9080/BuaVTt00ZvXq83VUGrCD2I_qw-e9POjLoGmgHfxMtGI" -p lwt | jq .
{
  "note": "Status of package lwt",
  "latest_pipeline": [
    {
      "version": "5.6.1",
      "status": "failed"
    },
    {
      "version": "5.7.0",
      "status": "failed"
    }
  ],
  "latest_but_one_pipeline": [

    {
      "version": "5.6.1",
      "status": "passed"
    },
    {
      "version": "5.7.0",
      "status": "passed"
    }
  ]
}
```

## Unimplemented

Show the build status of a single job:
```sh skip
$ ocaml-docs-ci-client status --ci-cap <path to cap file> --job <job-id>
```

Display logs for an individual job (with a URL)
```sh skip
$ ocaml-docs-ci-client logs --ci-cap <path to cap file> --job <job-id>
 ```

Rebuild a specific job
```sh skip
$ ocaml-docs-ci rebuild --ci-cap <path to cap file> --job <job-id>
```

## Reference

https://github.com/ocaml/infrastructure/wiki/Using-the-opam-ci-tool
