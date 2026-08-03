#!/bin/bash
# Iterate through T01..T12 tags in the assembled test opam-repo,
# checking out each tag and running day11 batch against the
# testlab profile. Produces one snapshot per tag in
# ~/.day11/snapshots/testlab/ — the sweep data the GUI tests
# assert against.
#
# Requires: test-repo assembled at $OUT (build-timeline.sh run),
# tarball HTTP server running (serve.sh), testlab profile in
# ~/.day11/profiles/.

set -euo pipefail

OUT="${OUT:-$HOME/.day11/test-repo}"
TAGS=(T01 T02 T03 T04 T05 T06 T07 T08 T09 T10 T11 T12)
LOG="${LOG:-/tmp/testlab-sweep.log}"

: > "$LOG"

echo "Sweep log: $LOG"

for tag in "${TAGS[@]}"; do
  echo "=== $tag ==="
  echo "=== $tag ===" >> "$LOG"
  (cd "$OUT" && git -c advice.detachedHead=false checkout -q "$tag")
  start=$(date +%s)
  if day11 batch --profile=testlab -j 4 --cores-per-build 2 >> "$LOG" 2>&1; then
    echo "  $tag ok (elapsed $(( $(date +%s) - start ))s)"
  else
    echo "  $tag had build failures — expected for some tags"
  fi
done

echo
echo "Final snapshot count:"
ls -d ~/.day11/snapshots/testlab/*/ 2>/dev/null | wc -l

# Leave repo at HEAD (T12) so day11 can keep serving the default state.
(cd "$OUT" && git checkout -q main)
