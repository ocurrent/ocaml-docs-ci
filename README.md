# Mirage CI

This CI is a set of ocurrent pipelines testing various things for the Mirage project.

In `src/pipelines/`, there are three kind of pipelines:

- `monorepo`: assemble monorepos and use `dune` to test the buildability of mirage projects as 
  released packages but also by testing the development branches altogether.
- `skeleton`: test the mirage project using the `mirage-skeleton` unikernel repository, by performing
  a multi-stage set of builds.
- `PR`: test PRs against `mirage/mirage`, `mirage/mirage-dev` and `mirage/mirage-skeleton`. 

## Running

Copy `config.sample.json` to `config.json` and edit it accordingly:
- `cap_file`: Capability file for the ocluster submissions.
- `remote_push`: a git repository remote to which the head node can push.
- `remote_pull`: a publish remote endpoint to the same git repository, from which the ocluster workers can pull.
- `enable_commit_status`: use the github API to push commit statuses.

Obtain a Github personal access token that has the `repo:status` authorisation and save it in a file. 

Then, use `dune exec -- mirage-ci --github-token-file <TOKEN_FILE>` to launch the CI pipeline. 


## Mirage docs

A docker service needs to be created to serve the docs, based on the `mirage-docs` image:
`docker service create --name mirage-docs -p 8081:80 mirage-docs`
