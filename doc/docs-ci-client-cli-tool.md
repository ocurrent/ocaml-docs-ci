A cli tool `ocaml-docs-ci-client` is available to interact with the production and staging instances of `docs.ci.ocaml.org`

### Installation

#### The cli tool

1. Clone the `ocaml-docs-ci` repository and [follow the directions](https://github.com/ocurrent/ocaml-docs-ci#installation) to build it locally. Once it has built you should get sensible output from:

```
dune exec -- ocaml-docs-ci-client --help
```

2. You can install the cli tool by doing `dune install ocaml-docs-ci-client`

#### Cap files

The client cli tool communicates with a [capnp](https://github.com/mirage/capnp-rpc) API on `docs.ci.ocaml.org` and requires a capability file for each environment (these are like credentials). To obtain capability files for staging and production please contact the CI / Ops team via slack. You should save these cap files so that you can clearly identify them by the environment that they will connect to.

#### jq

`jq` is an incredibly useful tool for working with JSON data (which is what the cli tool outputs). You should install it following the instructions [here.](https://jqlang.github.io/jq/download/) If you are unfamiliar with `jq` please take a look at its [short tutorial](https://jqlang.github.io/jq/tutorial/) to get you started.

### Usage

The cli tool has subcommands `status`, `steps`, `health-check`, `status-by-pipeline` and `diff-pipelines`

--

**`status`** will give you the statuses of all known versions of a package. For example:

```
❯ ocaml-docs-ci-client status --ci-cap=<path-to-cap-file> --package "fmt"


package: fmt
Version/Status:
0.9.0/passed
0.8.9/failed
0.8.8/failed
0.8.6/failed
```

**`steps`** returns returns an array of json objects for each known version of a package. The json objects contain the version, status and an array of steps (corresponding to jobs that were run in docs-ci). For example - in the case of `fmt`

```
❯ ocaml-docs-ci-client steps --ci-cap=<path-to-cap-file> -p "fmt" | jq .
[
  {
    "version": "0.9.0",
    "status": "passed",
    "steps": [
      {
        "typ": "prep fmt.0.9.0-7327e140e1aeb42b7944e88e03dcc002",
        "job_id": "2023-06-28/051739-voodoo-prep-a0cc8c",
        "status": "passed"
      },
      ...
    ]
  }
]
```

Now we can use `jq` to just get the versions and their statuses like so:

```

❯ ocaml-docs-ci-client steps --ci-cap=<path-to-cap-file> --package "fmt" |  jq '.[] | {version: .version, status: .status}'
{
  "version": "0.8.6",
  "status": "failed"
}
{
  "version": "0.8.8",
  "status": "failed"
}
{
  "version": "0.8.9",
  "status": "failed"
}
{
  "version": "0.9.0",
  "status": "passed"
}
```

And further, to get the steps that failed:

```
❯ ocaml-docs-ci-client steps --ci-cap=<path-to-cap-file> -p "fmt" | jq . | jq '.[].steps[] | select(.status | test("failed"))'
{
  "typ": "prep fmt.0.9.0-7327e140e1aeb42b7944e88e03dcc002",
  "job_id": "2023-06-28/051739-voodoo-prep-a0cc8c",
  "status": "failed"
}
```

And a bit of `sed` gets us to the urls of the jobs of the failing steps. Assuming here that we are working with staging, we would do:

```
❯ ocaml-docs-ci-client steps --ci-cap=<path-to-cap-file> -p "fmt" | jq '.[].steps[] | select(.status | test("failed"))' | jq '.job_id' | sed 's@"@@g' | sed 's@(^.*$\)@http://staging.docs.ci.ocaml.org/job/\1@'
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-bebea7
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-33ed0a
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-e90e25
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-1b4eef
http://staging.docs.ci.ocaml.org/job/2023-06-27/104411-voodoo-prep-e9df46
http://staging.docs.ci.ocaml.org/job/2023-06-27/104411-voodoo-prep-e9df46
http://staging.docs.ci.ocaml.org/job/2023-06-27/104411-voodoo-prep-e9df46
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-175795
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-1b4eef
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-63df71
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-0dc3a5
http://staging.docs.ci.ocaml.org/job/2023-06-27/104410-voodoo-prep-2cd771
```

**`health-check`** returns a json object containing information about the last two consecutive pipelines. In particular it provides the number of packages in each of `failed`, `running` and `passed` states so that the most recent run can be readily compared to the previous one.

```
❯ dune exec -- ocaml-docs-ci-client health-check --ci-cap="/Users/navin/src/tarides/ocaml-docs-ci/capnp-secrets/local-docs-ci.cap" | jq .
{
  "latest": {
    "epoch_html": "ae8bf595b8594945ee40c58377e03730",
    "epoch_linked": "5daeecab2ad7a2d07a12742d4cc0ab6f",
    "voodoo_do": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_prep": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_gen": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_repo": "https://github.com/ocaml-doc/voodoo.git",
    "voodoo_branch": "main",
    "failed_packages": 15,
    "running_packages": 0,
    "passed_packages": 0
  },
  "latest-but-one": {
    "epoch_html": "ae8bf595b8594945ee40c58377e03730",
    "epoch_linked": "5daeecab2ad7a2d07a12742d4cc0ab6f",
    "voodoo_do": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_prep": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_gen": "67ccabec49b5f4d24147839291fcae7c19d3e8c9",
    "voodoo_repo": "https://github.com/ocaml-doc/voodoo.git",
    "voodoo_branch": "main",
    "failed_packages": 0,
    "running_packages": 15,
    "passed_packages": 0
  }
}
```

**`diff-pipelines`** returns a json object that contains a list of packages that fail in the latest pipeline, that did not fail in the latest-but-one pipeline.hat fail in the latest pipeline, that did not fail in the latest-but-one pipeline. (At the time of writing this document we did not have two pipelines recorded in production so cannot provide meaningful example here.

**`status-by-pipeline`** takes a package as an argument and returns a json object that contains the status of that package in the latest and latest-but-one pipelines.

```
❯ dune exec -- ocaml-docs-ci-client status-by-pipeline --ci-cap="/Users/navin/src/tarides/ocaml-docs-ci/capnp-secrets/production-docs-ci.cap" --package "fmt"
{"note":"Only one pipeline has been recorded.","latest_pipeline":[{"version":"0.7.0","status":"pending"},{"version":"0.7.1","status":"pending"},{"version":"0.8.0","status":"pending"},{"version":"0.8.1","status":"pending"},{"version":"0.8.10","status":"pending"},{"version":"0.8.2","status":"pending"},{"version":"0.8.3","status":"pending"},{"version":"0.8.4","status":"pending"},{"version":"0.8.5","status":"pending"},{"version":"0.8.6","status":"pending"},{"version":"0.8.7","status":"pending"},{"version":"0.8.8","status":"pending"},{"version":"0.8.9","status":"pending"},{"version":"0.9.0","status":"pending"}],"latest_but_one_pipeline":[{"version":"0.7.0","status":"pending"},{"version":"0.7.1","status":"pending"},{"version":"0.8.0","status":"pending"},{"version":"0.8.1","status":"pending"},{"version":"0.8.10","status":"pending"},{"version":"0.8.2","status":"pending"},{"version":"0.8.3","status":"pending"},{"version":"0.8.4","status":"pending"},{"version":"0.8.5","status":"pending"},{"version":"0.8.6","status":"pending"},{"version":"0.8.7","status":"pending"},{"version":"0.8.8","status":"pending"},{"version":"0.8.9","status":"pending"},{"version":"0.9.0","status":"pending"}]}
```
