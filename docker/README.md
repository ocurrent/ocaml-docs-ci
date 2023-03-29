# Helper dockerfiles

They must be built using the root of the project as context: `docker build -f docker/storage/Dockerfile .`.

## storage

The server in charge of storing the data:
- ssh: endpoint exposed to the workers
- rsync: prep / compile artifacts transfer
- git: html artifacts storage

## init

The initialization program: it generates keys for the storage server

## html-data-website

Expose docs-data volume over http using nginx.

## docs-ci

OCurrent pipeline for building ocaml docs for ocaml.org package index.

