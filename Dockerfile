FROM ocaml/opam:debian-12-ocaml-4.14@sha256:14f4cc396d19e5eba0c4cd8258eedd1045091f887920ba53431e1e05110311fc AS build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install -y capnproto graphviz libcapnp-dev libev-dev libffi-dev libgmp-dev libsqlite3-dev pkg-config
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 56e31a3bc1fd0bfd87e5251972e806b8f78082a5 && opam update

WORKDIR /src
# See https://github.com/ocurrent/ocaml-docs-ci/pull/177#issuecomment-2445338172
RUN sudo chown opam:opam $(pwd)

# We want to cache the installation of dependencies prior to pulling in changes from the source dir
COPY --chown=opam ./ocaml-docs-ci.opam /src/
RUN opam install -y --deps-only .

COPY --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/ocaml-docs-ci ./_build/install/default/bin/ocaml-docs-ci-solver
RUN cp ./_build/install/default/bin/ocaml-docs-ci ./_build/install/default/bin/ocaml-docs-ci-solver .

FROM debian:12
RUN apt-get update && apt-get install rsync libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN git config --global user.name "docs" && git config --global user.email "ci"
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb https://download.docker.com/linux/debian bookworm stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce docker-buildx-plugin -y --no-install-recommends
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/ocaml-docs-ci"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/ocaml-docs-ci /src/ocaml-docs-ci-solver /usr/local/bin/
# Create migration directory
RUN mkdir -p /migrations
COPY --from=build /src/migrations /migrations
