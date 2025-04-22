FROM ocaml/opam:debian-12-ocaml-5.3@sha256:e86a49c28b4aba9fdac4929c6ab8302b80106e5824615d5d7d2601dc4ff983d5 AS build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 56e31a3bc1fd0bfd87e5251972e806b8f78082a5 && opam update

WORKDIR /src
# See https://github.com/ocurrent/ocaml-docs-ci/pull/177#issuecomment-2445338172
RUN sudo chown opam:opam $(pwd)

COPY --chown=opam ocaml-docs-ci.opam ocaml-docs-ci-client.opam /src/
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/epoch
RUN cp ./_build/install/default/bin/epoch .

FROM debian:12
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV NOTVISIBLE="in users profile"

RUN apt-get update && \
    apt-get install -y openssh-server rsync git && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir /var/run/sshd
RUN echo 'MaxSessions 1000' >> /etc/ssh/sshd_config
RUN echo 'MaxStartups 200' >> /etc/ssh/sshd_config
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
RUN echo "export VISIBLE=now" >> /etc/profile
RUN git config --global user.email "docker@ci"
RUN git config --global user.name "CI"
COPY ./docker/storage/entrypoint.sh /entrypoint.sh
RUN chmod 744 /entrypoint.sh

## TODO Fixup --version git sha information. Not present in current build. Maybe use dune install ...
COPY --from=build /src/epoch /usr/local/bin/

EXPOSE 22
EXPOSE 873

CMD ["rsync_server"]
ENTRYPOINT ["/entrypoint.sh"]
