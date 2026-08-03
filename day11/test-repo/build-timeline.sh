#!/bin/bash
# Assemble the test opam-repo as a real on-disk git repository.
#
# Driven by timeline.txt: one line per "<tag> <pkg-ver>" (or the
# literal "<tag> base" for T00 which only seeds the base layer).
# Each group of consecutive same-tag rows becomes one commit,
# tagged with the tag name.
#
# Sources are packed into tarballs under <out>/cache/; synthetic
# opam files are substituted so url.src points at those local
# tarballs via http://127.0.0.1:$PORT/... — serve.sh runs the
# matching http server. The container has host networking, so
# 127.0.0.1 reaches the host.
#
# Default output: $HOME/.day11/test-repo. Override with $OUT.
# Re-runs wipe and rebuild.

set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
OUT="${OUT:-$HOME/.day11/test-repo}"
PORT="${PORT:-8765}"
TIMELINE="$HERE/timeline.txt"

if [ ! -d "$HERE/base/packages" ]; then
  echo "base layer not populated — run populate-base.sh first" >&2
  exit 1
fi
if [ ! -f "$TIMELINE" ]; then
  echo "timeline.txt not found at $TIMELINE" >&2
  exit 1
fi

echo "Assembling test opam-repo at $OUT"
rm -rf "$OUT"
mkdir -p "$OUT"

pack_source () {
  local src_dir="$1" tarball="$2"
  local staging basename
  staging=$(mktemp -d)
  basename=$(basename "$src_dir")
  cp -r "$src_dir" "$staging/$basename"
  tar -C "$staging" -czf "$tarball" "$basename"
  rm -rf "$staging"
}

install_synthetic () {
  local name_ver="$1"
  local name="${name_ver%-*}"
  local ver="${name_ver##*-}"
  local src_dir="$HERE/sources/$name_ver"
  local tarball="$OUT/cache/$name_ver.tar.gz"
  local opam_in="$HERE/packages/$name/$name.$ver/opam"
  local pkg_dir="$OUT/packages/$name/$name.$ver"
  [ -d "$src_dir" ] || { echo "MISSING source: $src_dir" >&2; return 1; }
  [ -f "$opam_in" ] || { echo "MISSING opam:   $opam_in" >&2; return 1; }
  mkdir -p "$(dirname "$tarball")"
  pack_source "$src_dir" "$tarball"
  local sha ident
  sha=$(sha256sum "$tarball" | awk '{print $1}')
  ident=$(echo "$name_ver" | tr 'a-z.-' 'A-Z__')
  mkdir -p "$pkg_dir"
  sed -e "s|@@PORT@@|$PORT|g" \
      -e "s|@@SHA256_${ident}@@|$sha|g" \
      "$opam_in" > "$pkg_dir/opam"
}

# Stage the base layer and initialize git.
cp -r "$HERE/base/packages" "$OUT/packages"
cat > "$OUT/repo" <<'EOF'
opam-version: "2.0"
EOF
cd "$OUT"
git init -q
git checkout -q -b main

commit_and_tag () {
  local tag="$1" subject="$2"
  git add .
  git -c user.email=testlab@example.com -c user.name=testlab \
    commit -q -m "$tag: $subject"
  git tag "$tag"
}

# Walk timeline.txt, collecting every (tag, pkg) pair first, then
# grouping by tag and committing each group. Two-pass keeps the
# logic simple and avoids array-reset subtleties inside the loop.

tags=()
pvs=()
while IFS= read -r line; do
  line="${line%%#*}"
  line=$(echo "$line" | xargs || true)
  [ -z "$line" ] && continue
  tag="${line%% *}"
  pv="${line#* }"
  tags+=("$tag")
  pvs+=("$pv")
done < "$TIMELINE"

process_tag () {
  local tag="$1"; shift
  local pkgs=("$@")
  if [ "$tag" = "T00" ]; then
    commit_and_tag "T00-base" "base layer (ocaml 5.3.0 + odoc-driver.3.1.0 stack)"
    return
  fi
  local pv
  for pv in "${pkgs[@]}"; do
    install_synthetic "$pv"
  done
  local summary
  summary=$(IFS=,; echo "${pkgs[*]}")
  commit_and_tag "$tag" "$summary"
}

# Group consecutive same-tag entries and process each group.
i=0
n=${#tags[@]}
while [ "$i" -lt "$n" ]; do
  tag="${tags[$i]}"
  group=()
  while [ "$i" -lt "$n" ] && [ "${tags[$i]}" = "$tag" ]; do
    group+=("${pvs[$i]}")
    i=$((i + 1))
  done
  process_tag "$tag" "${group[@]}"
done

echo
echo "Built:"
git log --oneline --decorate
echo
echo "Repo root: $OUT"
