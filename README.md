# Docs CI

Building docs using odoc.

## Running

Copy `config.sample.json` to `config.json` and edit it accordingly:
- `cap_file`: Capability file for the ocluster submissions.
- `remote_push`: a git repository remote to which the head node can push.
- `remote_pull`: a publish remote endpoint to the same git repository, from which the ocluster workers can pull.

Then, use `dune exec -- docs-ci` to launch the CI pipeline. 

