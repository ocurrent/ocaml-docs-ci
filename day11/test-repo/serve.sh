#!/bin/bash
# Serve synthetic tarballs over HTTP so the build container (which
# runs on the host network) can fetch them via 127.0.0.1:$PORT.
#
# Binds to 127.0.0.1 only — no external exposure.
# Serves from <OUT>/cache/ so URLs match the opam url.src paths.

set -euo pipefail

OUT="${OUT:-$HOME/.day11/test-repo}"
PORT="${PORT:-8765}"

if [ ! -d "$OUT/cache" ]; then
  echo "no cache dir at $OUT/cache — run build-timeline.sh first" >&2
  exit 1
fi

cd "$OUT/cache"
exec python3 -m http.server "$PORT" --bind 127.0.0.1
