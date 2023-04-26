# Using the ocaml-docs ci tool

`ocaml-docs-ci status`  shows a dashboard of documentation build results across opam-repository packages. Packages can be filtered by maintainer substrings or tag names in the opam package description.

`ocaml-docs-ci status <project_name>` show the build status of all versions of a project.

`ocaml-docs-ci status <project_name> <version>` show the build status of a single version of a project.

`ocaml-docs-ci status <project_name> <version> jobs` show the build status of a single job

`ocaml-docs-ci logs <job-id>` - Display logs for an individual job

## Reference
https://github.com/ocaml/infrastructure/wiki/Using-the-opam-ci-tool
