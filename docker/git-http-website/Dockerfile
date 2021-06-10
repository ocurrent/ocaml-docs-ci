# syntax=docker/dockerfile:experimental
FROM ocaml/opam:debian-ocaml-4.12 AS build
RUN cd ~/opam-repository && git pull origin master && git reset --hard a147c4ecea9e6eaa71a0f3bc152d502695c569c4 && opam update
WORKDIR /src
RUN sudo mv /usr/bin/opam-2.1 /usr/bin/opam
COPY --chown=opam docs-ci-tools.opam /src/
RUN opam install --deps-only .
ADD --chown=opam . .
RUN sudo chown opam:opam .
RUN --mount=type=cache,target=./_build/,uid=1000,gid=1000 opam exec -- dune build ./_build/install/default/bin/githttpserver && cp ./_build/install/default/bin/githttpserver .
FROM debian:10
RUN apt-get update && apt-get install libev4 curl gnupg2 dumb-init git ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
WORKDIR /var/lib/githttpserver
ENTRYPOINT ["dumb-init", "/usr/local/bin/githttpserver"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/githttpserver /usr/local/bin/
