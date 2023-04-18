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
Uses the top level Dockerfile at the root of this project.

## worker

ocluster worker to run in a Linux x86_64 pool to test local builds.
The worker uses Docker in Docker to run builds as the production cluster would on Linux.