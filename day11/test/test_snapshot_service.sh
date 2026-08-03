#!/bin/bash
# Test snapshot service flow: simulate running day11 as a service
# across opam-repository commits spanning the OCaml 5.4.1 release,
# using the small universe profile.
#
# Tests: snapshot creation, layer caching across snapshots,
# compiler transitions, doc regeneration, and incremental behaviour.
#
# Usage: ./test_snapshot_service.sh [day11-binary]

set -e

DAY11="${1:-$(dirname "$0")/../../_build/default/day11/bin/main.exe}"
OPAM_REPO="$HOME/ocaml/opam-repository"
PROFILE_DIR="/tmp/day11-snapshot-test"
PROFILE_NAME="snapshot-test"

# Commits spanning the OCaml 5.4.1 release and subsequent package updates
# All commits are merge commits on master for deterministic state
COMMITS=(
  "809faa59ae"  # Just before OCaml 5.4.1 (2026-02-17)
  "d10e5a9919"  # OCaml 5.4.1 released (compiler change!)
  "9c0b4e947c"  # lwt 6.1.1 (2026-02-23)
  "6185871ccd"  # ppxlib 0.38.0 (2026-03-20)
  "6185871ccd"  # Same commit again (should be a complete no-op)
)

LABELS=(
  "Before OCaml 5.4.1"
  "OCaml 5.4.1 released"
  "lwt 6.1.1"
  "ppxlib 0.38.0"
  "Repeat (no-op)"
)

# Save current state
ORIG_REF=$(git -C "$OPAM_REPO" symbolic-ref HEAD 2>/dev/null || git -C "$OPAM_REPO" rev-parse HEAD)

cleanup() {
  echo ""
  echo "Restoring opam-repository..."
  git -C "$OPAM_REPO" checkout -q "$ORIG_REF" 2>/dev/null || true
  echo "Done."
}
trap cleanup EXIT

# Ensure fork helper is linked
BINDIR=$(dirname "$DAY11")
HELPER=$(find "$BINDIR/../.." -name fork_helper.exe -type f 2>/dev/null | head -1)
[ -n "$HELPER" ] && ln -sf "$HELPER" "$BINDIR/day11-fork-helper" 2>/dev/null

echo "=========================================="
echo "  Snapshot Service Test"
echo "=========================================="
echo "Binary:      $DAY11"
echo "Repo:        $OPAM_REPO"
echo "Profile dir: $PROFILE_DIR"
echo ""

# Clean start
sudo rm -rf "$PROFILE_DIR"
mkdir -p "$PROFILE_DIR"

# Create profile — no driver_compiler pin, auto-detect from solutions
"$DAY11" profile create \
  --name "$PROFILE_NAME" \
  --profile-dir "$PROFILE_DIR" \
  --opam-repository "$OPAM_REPO" \
  --small-universe \
  --with-doc \
  2>&1 | sed 's/^/  /'
echo ""

PREV_SNAPSHOT=""

for i in "${!COMMITS[@]}"; do
  COMMIT="${COMMITS[$i]}"
  LABEL="${LABELS[$i]}"
  STEP=$((i + 1))
  TOTAL=${#COMMITS[@]}

  echo "=========================================="
  echo "  Step $STEP/$TOTAL: $LABEL"
  echo "  Commit: $COMMIT"
  echo "=========================================="

  # Checkout the commit
  git -C "$OPAM_REPO" checkout -q "$COMMIT"

  # Run batch
  OUTPUT=$("$DAY11" batch \
    --profile "$PROFILE_NAME" \
    --profile-dir "$PROFILE_DIR" \
    -j 4 \
    2>&1) || true

  # Extract key metrics
  SNAPSHOT=$(echo "$OUTPUT" | grep "^Snapshot:" | awk '{print $2}')
  TARGETS=$(echo "$OUTPUT" | grep "^Targets:" | awk '{print $2}')
  SOLVE_CACHED=$(echo "$OUTPUT" | grep "^Solving:" | head -1 | grep -oP '^\S+\s+\K\d+(?= cached)')
  SOLVE_NEED=$(echo "$OUTPUT" | grep "^Solving:" | head -1 | grep -oP '\d+(?= need)')
  LAYERS_CACHED=$(echo "$OUTPUT" | grep "^Layers:" | grep -oP '\d+(?= cached)')
  LAYERS_NEED=$(echo "$OUTPUT" | grep "^Layers:" | grep -oP '\d+(?= need)')

  # Count doc results from output
  DOC_ALL_OK=$(echo "$OUTPUT" | grep -c 'doc-all OK' || true)
  LINKED=$(echo "$OUTPUT" | grep -c ': linked$' || true)
  DOC_LINE=$(echo "$OUTPUT" | grep "^=== Docs:" || echo "none")

  # Executor stats
  EXEC_LINE=$(echo "$OUTPUT" | grep "Executor:" | tail -1 || echo "")

  # Snapshot reuse check
  if [ "$SNAPSHOT" = "$PREV_SNAPSHOT" ]; then
    SNAP_STATUS="reused"
  elif [ -n "$PREV_SNAPSHOT" ]; then
    SNAP_STATUS="new"
  else
    SNAP_STATUS="first"
  fi

  echo "  Snapshot:      $SNAPSHOT ($SNAP_STATUS)"
  echo "  Targets:       $TARGETS"
  echo "  Solver:        $SOLVE_CACHED cached, $SOLVE_NEED to solve"
  echo "  Build layers:  $LAYERS_CACHED cached, $LAYERS_NEED to build"
  echo "  Executor:      $EXEC_LINE"
  echo "  Doc-all OK:    $DOC_ALL_OK"
  echo "  Linked:        $LINKED"
  echo "  Docs summary:  $DOC_LINE"

  # Verify snapshot dir
  if [ -n "$SNAPSHOT" ]; then
    SNAP_DIR="$PROFILE_DIR/snapshots/$PROFILE_NAME/$SNAPSHOT"
    [ -d "$SNAP_DIR" ] && echo "  Snapshot dir:  exists" || echo "  Snapshot dir:  MISSING!"
  fi

  # Check for errors
  ERRORS=$(echo "$OUTPUT" | grep -c 'FAIL\|Error\|failed' || true)
  if [ "$ERRORS" -gt 0 ]; then
    echo "  Warnings:      $ERRORS lines with FAIL/Error/failed"
    echo "$OUTPUT" | grep 'FAIL\|solve failed\|Error' | head -3 | sed 's/^/    /'
  fi

  PREV_SNAPSHOT="$SNAPSHOT"
  echo ""
done

echo "=========================================="
echo "  Summary"
echo "=========================================="

# Count snapshots
N_SNAPSHOTS=$(ls -d "$PROFILE_DIR/snapshots/$PROFILE_NAME/"*/ 2>/dev/null | wc -l)
echo "Snapshots created: $N_SNAPSHOTS (expected 4 — step 5 reuses step 4)"

# Count layers in shared cache
CACHE_OS="$PROFILE_DIR/cache/debian-bookworm-x86_64"
if [ -d "$CACHE_OS" ]; then
  N_LAYERS=$(ls "$CACHE_OS" | grep -cE '^[0-9a-f]{12}$' || true)
  echo "Layers in shared cache: $N_LAYERS"
  HTML_COUNT=$(find "$CACHE_OS/html" -name '*.html' 2>/dev/null | wc -l)
  echo "HTML files: $HTML_COUNT"
fi

echo ""
echo "Done."
