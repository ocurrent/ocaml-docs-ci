# Helper dockerfiles

They must be built using the root of the project as context: `docker build -f docker/storage/Dockerfile .`.

## storage

The server in charge of storing the data:
- ssh: endpoint exposed to the workers
- rsync: prep / compile artifacts transfer
- git: html artifacts storage

## init

The initialization program: it generates keys for the storage server

## git-http

Expose /data/git repository over http

