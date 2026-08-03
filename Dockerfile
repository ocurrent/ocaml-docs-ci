# Build stage — compile the daemon and day11's helper binaries.
FROM ocaml/opam:debian-12-ocaml-5.4@sha256:24382fcfc3d320ec62ea7f018f1084b0d7c1dff2b236e2aed2e375928c247fbb AS build

RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install -y \
      capnproto graphviz libcapnp-dev libev-dev libffi-dev libgmp-dev \
      libsqlite3-dev pkg-config

# Sync to a known good opam-repository commit so [opam install
# --deps-only] is reproducible.
RUN cd ~/opam-repository \
    && git fetch -q origin master \
    && git reset --hard 6bda456f941d6c1e0773fcb1fd57c0eb54b8a2c1 \
    && opam update

# The OCurrent close-after-exec2 patch — same as before.
RUN opam pin add -n git+https://github.com/jonludlam/ocurrent.git#close-after-exec2

WORKDIR /src
# See https://github.com/ocurrent/ocaml-docs-ci/pull/177#issuecomment-2445338172
RUN sudo chown opam:opam $(pwd)

# Resolve deps from the in-tree opam files. There are now three: the
# day11 sub-package, ocaml-docs-ci, ocaml-docs-ci-client.
COPY --chown=opam ./day11.opam ./ocaml-docs-ci.opam ./ocaml-docs-ci-client.opam /src/
RUN opam install -y --deps-only .

COPY --chown=opam . .
RUN opam exec -- dune build \
      ./_build/install/default/bin/ocaml-docs-ci \
      ./_build/install/default/bin/day11-solver-worker \
      ./_build/install/default/bin/day11-fork-helper \
      ./_build/install/default/bin/day11
# Stash the binaries somewhere that won't collide with the [day11/]
# source tree at the workspace root. /src is opam-owned so a
# subdirectory is writable without sudo.
RUN mkdir /src/artifacts \
    && cp ./_build/install/default/bin/ocaml-docs-ci \
          ./_build/install/default/bin/day11-solver-worker \
          ./_build/install/default/bin/day11-fork-helper \
          ./_build/install/default/bin/day11 \
          /src/artifacts/

# Runtime stage. The daemon drives [runc] directly to build each
# opam package in a container of its own, so this image needs runc
# plus the bits day11 expects on PATH (git for opam-repo mirrors;
# rsync for layer transfers; bash/sed/etc. for the dispatch
# scripts). Container is launched with --privileged from
# docker-compose so runc-in-container has the caps it needs.
#
# Build args UID / GID select the in-container user. Set them to
# match the host user that owns the bind-mounted [~/.day11/] tree
# so files the daemon writes are still owned by that host UID and
# git stops complaining about "dubious ownership". Defaults to
# 1000:1000.
FROM debian:12@sha256:49ba348354a28e39c70beffd6cf43bdb8d55d81ce4b746b0428717d054b8bbc4

ARG UID=1000
ARG GID=1000
# Avoid names debian already uses ([daemon], [www-data], [_apt], ...).
ARG USERNAME=app
ARG HOME_DIR=/home/app

RUN apt-get update && apt-get install -y --no-install-recommends \
      runc \
      git rsync sudo dumb-init ca-certificates \
      libev4 libsqlite3-0 sqlite3 libgmp10 libffi8 \
      bash coreutils util-linux procps psmisc \
      curl bzip2 gzip xz-utils unzip tar \
      graphviz \
      attr \
      gnupg \
    && curl -fsSL https://download.docker.com/linux/debian/gpg \
       | gpg --dearmor -o /usr/share/keyrings/docker.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/docker.gpg] https://download.docker.com/linux/debian bookworm stable" \
       > /etc/apt/sources.list.d/docker.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends docker-ce-cli docker-buildx-plugin \
    && rm -rf /var/lib/apt/lists/*

# Create a group + user at the supplied numeric GID/UID. If something
# already occupies the numeric id (rare for 1000+, but debian
# reserves 1-999), delete the stale entry and recreate ours.
RUN existing_group=$(getent group ${GID} | cut -d: -f1) \
    && if [ -n "$existing_group" ] && [ "$existing_group" != "${USERNAME}" ]; then \
         groupdel "$existing_group" || true; \
       fi \
    && groupadd --gid ${GID} ${USERNAME} \
    && existing_user=$(getent passwd ${UID} | cut -d: -f1) \
    && if [ -n "$existing_user" ] && [ "$existing_user" != "${USERNAME}" ]; then \
         userdel -r "$existing_user" 2>/dev/null || true; \
       fi \
    && useradd --uid ${UID} --gid ${GID} --home ${HOME_DIR} \
               --create-home --shell /bin/bash ${USERNAME} \
    && echo "${USERNAME} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0440 /etc/sudoers.d/${USERNAME}

# git complains if the cwd's ownership doesn't match the running
# UID (the bind-mounted [.day11/overlays/...] dirs are owned by the
# host UID, which now matches ours, so this is mostly belt-and-
# braces — but '*' means we never have to plumb specific paths).
RUN git config --system safe.directory '*' \
    && git config --system user.name "docs-ci" \
    && git config --system user.email "ci@day11"

# TMPDIR holds the daemon's scratch: per-build overlay upper/work dirs
# (day11_run_*), the layer-merge target, and git temp files it
# rename()s into the repos/cache. It must satisfy:
#   1. NOT be overlayfs — overlayfs can't be an overlay upperdir, so the
#      build container would come up with an empty rootfs
#      ("/usr/bin/env: no such file"). The container's /tmp is overlayfs.
#   2. Be on the SAME MOUNT as the layer cache. Stack.merge composes a
#      build's dependency layers with [cp --link] (hardlinks) from the
#      cache into a scratch dir, and hardlinks cannot cross mount points
#      — even bind-mounts of one ext4. A separate tmp mount makes the
#      merge fail (and the failure is swallowed, silently dropping deps,
#      so large closures like odoc-driver fail to find sexplib/etc.).
# Putting TMPDIR *under* the bind-mounted cache satisfies both: it's the
# same ext4 mount as the layers (hardlinks work, valid upperdir) and the
# same superblock as the repo/overlay mounts (rename works).
ENV OCAMLRUNPARAM=a=2 \
    HOME=${HOME_DIR} \
    TMPDIR=${HOME_DIR}/.day11/cache/tmp

# All four binaries live on PATH. solver_worker / fork_helper are
# discovered by day11 via [Filename.dirname Sys.executable_name].
COPY --from=build \
     /src/artifacts/ocaml-docs-ci \
     /src/artifacts/day11-solver-worker \
     /src/artifacts/day11-fork-helper \
     /src/artifacts/day11 \
     /usr/local/bin/

# Run from /var/lib/ocurrent so OCurrent's [var/] state lands there
# (matches the original layout, and the bind-mount destination used
# by docker-compose.yml).
WORKDIR /var/lib/ocurrent
RUN chown ${UID}:${GID} /var/lib/ocurrent

USER ${UID}:${GID}

# Bake the three profiles the daemon ships with straight into the
# image. They are committed as plain JSON under [docker/profiles/] and
# land in [${HOME_DIR}/.day11/profiles/] — the daemon's default
# --profile-dir, since HOME=${HOME_DIR}. Their [opam_repositories] use
# paths relative to the .day11 root ([repo/opam-repository],
# [repo/oxcaml/opam-repository], [overlays/odoc-master/repo]), which
# Profile.load resolves against ${HOME_DIR}/.day11 — matching the
# --remote / --github-pin-overlay targets in docker-compose.yml:
#
#   quick             = mainline opam-repository, Jane Street pkgs only.
#   ocaml-odoc-master = mainline opam-repository, with odoc pinned to
#                       its master branch via the github-pin overlay
#                       (layered last so it wins).
#   oxcaml            = mainline opam-repository with the oxcaml overlay
#                       layered on top.
#   html_dir          = [<cache>/<os-dir>/html-<name>], exactly what the
#                       Caddy front-end serves as [/profiles/<name>/docs/].
#
# docker-compose.yml mounts the whole [.day11] tree as a single named
# volume. A fresh named volume is seeded from the image's directory
# contents (and uid:gid ownership) on first creation, so this baked
# JSON — and the user-owned [repo/]/[overlays/] dirs below — survive
# into the volume (a bind-mount would instead shadow them). mkdir as
# the runtime user so the daemon can write under [~/.day11] at startup.
#
# [repo/] holds the opam-repository mirrors: the daemon's [--remote]
# step runs [git clone] when [<path>/.git] is absent, populating them
# on first run (git creates the [repo/...] leaf dirs itself) — no
# checkout needed on the host. We pre-create [repo/] user-owned so the
# clone has a writable parent in the seeded volume.
RUN mkdir -p ${HOME_DIR}/.day11/profiles \
      ${HOME_DIR}/.day11/overlays ${HOME_DIR}/.day11/repo
COPY --chown=${UID}:${GID} docker/profiles/ ${HOME_DIR}/.day11/profiles/

# Ensure TMPDIR exists and is owned by the runtime user before the
# command starts. It lives under the bind-mounted cache, so it can't be
# created at image-build time (the mount shadows it), and several code
# paths mkdir subdirs of it non-recursively / bind a socket there,
# assuming it's present. Creating it here (as the app user, before any
# sudo path can make it root-owned) keeps it writable.
#
# The entrypoint is deliberately generic — it just preps TMPDIR and
# execs whatever command it's given, so the same image serves both the
# daemon (docker-compose's [command: ocaml-docs-ci ...]) and one-off
# CLI runs ([docker compose run --rm daemon day11 batch ...]).
ENTRYPOINT ["dumb-init", "sh", "-c", "mkdir -p \"$TMPDIR\" && exec \"$@\"", "sh"]
CMD ["ocaml-docs-ci", "--help"]
