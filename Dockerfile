# syntax=docker/dockerfile:experimental
FROM ocaml/opam:debian-12-ocaml-4.14@sha256:45b04e2a4c933c57549382045dfac12cb7e872cace0456f92f4b022066e48111 AS build
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 4f8d9820693746275a0438102c57ef7e43e745b9 && opam update
COPY --chown=opam \
    vendor/ocurrent/current_github.opam \
    vendor/ocurrent/current_git.opam \
    vendor/ocurrent/current.opam \
    vendor/ocurrent/current_web.opam \
    /src/vendor/ocurrent/
COPY --chown=opam \
    vendor/ocluster/current_ocluster.opam \
    vendor/ocluster/ocluster-api.opam \
    /src/vendor/ocluster/
WORKDIR /src
RUN sudo mv /usr/bin/opam-2.1 /usr/bin/opam
RUN opam pin add -yn current_github.dev "./vendor/ocurrent" && \
    opam pin add -yn current_git.dev "./vendor/ocurrent" && \
    opam pin add -yn current.dev "./vendor/ocurrent" && \
    opam pin add -yn current_web.dev "./vendor/ocurrent" && \
    opam pin add -yn current_ocluster.dev "./vendor/ocluster" && \
    opam pin add -yn ocluster-api.dev "./vendor/ocluster"

COPY --chown=opam ocaml-docs-ci.opam /src/
RUN sudo apt-get update && sudo apt-get install -y capnproto graphviz libcapnp-dev libev-dev libffi-dev libgmp-dev libsqlite3-dev pkg-config
RUN opam install --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build ./_build/install/default/bin/ocaml-docs-ci ./_build/install/default/bin/ocaml-docs-ci-solver && cp ./_build/install/default/bin/ocaml-docs-ci ./_build/install/default/bin/ocaml-docs-ci-solver .

FROM debian:12
RUN apt-get update && apt-get install rsync libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN git config --global user.name "docs" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/ocaml-docs-ci"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/ocaml-docs-ci /src/ocaml-docs-ci-solver /usr/local/bin/
# Create migration directory
RUN mkdir -p /migrations
COPY --from=build /src/migrations /migrations
