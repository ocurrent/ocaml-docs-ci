FROM ocaml/opam:debian-12-ocaml-4.14@sha256:45b04e2a4c933c57549382045dfac12cb7e872cace0456f92f4b022066e48111 AS build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard cf93548ddc4f36b87b006f4858fac7ae73ccaa0f && opam update
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
RUN opam pin add -yn current_github.dev "./vendor/ocurrent" && \
    opam pin add -yn current_git.dev "./vendor/ocurrent" && \
    opam pin add -yn current.dev "./vendor/ocurrent" && \
    opam pin add -yn current_web.dev "./vendor/ocurrent" && \
    opam pin add -yn current_ocluster.dev "./vendor/ocluster" && \
    opam pin add -yn ocluster-api.dev "./vendor/ocluster"

COPY --chown=opam ocaml-docs-ci.opam ocaml-docs-ci-client.opam /src/
RUN opam install --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/epoch && cp ./_build/install/default/bin/epoch .

FROM debian:12
ENV DEBIAN_FRONTEND noninteractive
ENV LANG C.UTF-8
ENV NOTVISIBLE "in users profile"

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
