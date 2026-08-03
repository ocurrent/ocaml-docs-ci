#!/bin/bash
# Mirror the base-layer packages listed in base-packages.list from an
# upstream opam-repository into ./base/ (a real opam-repo on disk).
#
# URLs in the mirrored opam files are kept as-is — opam downloads and
# caches the tarballs on first use, so the mirrored tree stays small.
# Re-run safely: wipes ./base/packages/ each time.

set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
UPSTREAM="${UPSTREAM:-/home/jjl25/ocaml/opam-repository}"
DEST="$HERE/base"
LIST="$HERE/base-packages.list"

if [ ! -d "$UPSTREAM/packages" ]; then
  echo "upstream opam-repo not found at $UPSTREAM" >&2
  echo "set UPSTREAM=/path/to/opam-repository to override" >&2
  exit 1
fi
if [ ! -f "$LIST" ]; then
  echo "package list not found at $LIST" >&2
  exit 1
fi

echo "Mirroring from $UPSTREAM → $DEST"
rm -rf "$DEST/packages"
mkdir -p "$DEST/packages"

missing=0
copied=0
while IFS= read -r pv; do
  [ -z "$pv" ] && continue
  name="${pv%%.*}"
  src="$UPSTREAM/packages/$name/$pv"
  if [ ! -d "$src" ]; then
    echo "MISSING: $pv" >&2
    missing=$((missing + 1))
    continue
  fi
  mkdir -p "$DEST/packages/$name"
  cp -r "$src" "$DEST/packages/$name/"
  copied=$((copied + 1))
done < "$LIST"

# Minimal repo marker so opam recognizes this as a repository.
cat > "$DEST/repo" <<'EOF'
opam-version: "2.0"
EOF

echo "Copied: $copied, missing: $missing"
if [ "$missing" -gt 0 ]; then
  exit 1
fi
