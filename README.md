# Docs CI

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Focaml.ci.dev%2Fbadge%2Focurrent%2Focaml-docs-ci%2Fmain&logo=ocaml)](https://ocaml.ci.dev/github/ocurrent/ocaml-docs-ci)

OCaml Docs CI (aka ocaml-docs-ci or just docs-ci) is an OCurrent pipeline used to build the documentation for ocaml.org website.
It uses the metadata from opam-repository to work out how to build documentation for individual packages using [voodoo](https://github.com/ocaml-doc/voodoo), the OCaml package documentation generator, and generates a HTML output suitable for ocaml.org server.

## Installation

Get the code with:

```shell
git clone --recursive https://github.com/ocurrent/ocaml-docs-ci.git
cd ocaml-docs-ci
```

Then you need an opam 2.1 switch using OCaml 4.14. Recommend using this command to setup a local switch just for `docs-ci`.

```shell
# Create a local switch with packages and test dependencies installed
opam switch create . 4.14.1 --deps-only --with-test -y

# Run the build
dune build

# Run the tests
dune build @runtest
```

## Architecture

At a high level `docs-ci` purpose is to compile the documentation of every package in the `opamverse`. To do this it generates
a dependency universe. For each package (along with the version), the documentation is generated for it plus all of its
dependencies. This documentation is then collected into a `documentation set` and provided to the ocaml.org service.
The [voodoo](https://github.com/ocaml-doc/voodoo) tool defines the on disk format for the `documentation set`.

For further details on how `docs-ci` works read the [pipeline diagram](doc/pipeline-diagram.md).

## Deployment

`ocaml-docs-ci` is deployed as into two environments, with [ocurrent-deployer](https://deploy.ci.ocaml.org/?repo=ocurrent/ocaml-docs-ci&). The application is deployed as a series of Docker containers from a git branch.

Environments:

| Environment | www                       | pipeline                          | git branch | data                               | voodoo branch |
| ----------- | ------------------------- | --------------------------------- | ---------- | ---------------------------------- | ------------- |
| Production  | https://ocaml.org         | https://docs.ci.ocaml.org         | live       | http://docs-data.ocaml.org         | main          |
| Staging     | https://staging.ocaml.org | https://staging.docs.ci.ocaml.org | staging    | http://staging.docs-data.ocaml.org | staging       |

OAuth integration provided by GitHub OAuth Apps hosted under the OCurrent organisation.
See https://github.com/organizations/ocurrent/settings/applications

The infrastructure for `docs-ci` is managed via Ansible, contact @tmcgilchrist or @mtelvers if you need access or have questions.

To deploy a new version of `docs-ci`:

1. Create a PR and wait for the GH Checks to run (ocaml-ci compiles the code and ocurrent-deployer checks it can build the Dockerfiles for the project)
1. Test the changes on `staging` environment by git cherry picking the commits to that branch and pushing it
1. Check [deploy.ci.ocaml.org](https://deploy.ci.ocaml.org/?repo=ocurrent/ocaml-docs-ci&) for `docs-ci`

Follow a similar process for `live` exercising extra caution as it could impact the live ocaml.org service.

The git history on `live` and `staging` **MUST** be kept in sync with the default branch.
Those branches should be the same as `main` plus or minus some commits from a PR.

## Remote API

`docs-ci` has a cli tool (`ocaml-docs-ci-client`) for interacting with the pipeline over CapnP. It provides commands to:

 * diff-pipelines - to show the changes between two pipeline runs
 * health-check - to provide information about a specific pipeline run
 * status - build status of a package
 * status-by-pipeline - build status of a package in the two most recent pipeline runs
 * steps - build steps of a package

The output is via json, which is intended to be combined with `jq` to display and query for pieces of information.

## Local Development

`ocaml-docs-ci` is runable as:

```
dune exec -- ocaml-docs-ci \
    --ocluster-submission cap/XXX.cap \
    --ssh-host ci.mirage.io \
    --ssh-user docs \
    --ssh-privkey cap/id_rsa \
    --ssh-pubkey cap/id_rsa.pub \
    --ssh-folder /data/ocaml-docs-ci \
    --ssh-endpoint https://ci.mirage.io/staging \
    --jobs 6 \
    --filter mirage \
    --limit 6
```

A [docker-compose.yml](docker-compose.yml) is provided to setup an entire `docs-ci` environment including:

- ocluster scheduler
- ocluster Linux x86 worker
- nginx webserver for generated docs
- ocaml-docs-ci built from the local git checkout

Run this command to create an environment:

```shell
$ docker-compose -f docker-compose.yml up
```

You should then be able to watch the pipeline in action at `http://localhost:8080`.

### Migrations

Migrations are managed using [omigrate](https://github.com/tmattio/omigrate). If you are using an opam switch for ocaml-docs-ci then omigrate should be installed and you can create a new migration by doing this from the project root:

``` shell
$ omigrate create --dir migrations <migration-name>
```

This will create timestamped files in the migrations directory that you can then populate with the sql necessary to introduce and remove the migration (in the up and down files respectively).

Migrations will not run unless the --migration-path flag is present when invoking ocaml-docs-ci-service.

### Epoch management
Epochs are used in ocaml-docs-ci to organise sets of artifacts all produced by the same odoc/voodoo version.
There is a cli tool for managing epochs described in [epoch.md](./src/cli/epoch.md).
