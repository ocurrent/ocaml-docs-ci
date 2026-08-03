#!/bin/bash
# Test: build fmt's docs and assert that references to Stdlib are
# rendered as live links, not [xref-unresolved] spans.
#
# Catches regressions where the compile-phase for the real compiler
# package (oxcaml-compiler / ocaml-compiler / ocaml-base-compiler)
# doesn't produce Stdlib.odoc, breaking every downstream package's
# Stdlib references — see [Day11_doc.Doc_build.is_ocaml_package]
# and [Day11_opam_build.Compiler_pkg.is_compiler].
#
# Usage: test_stdlib_xrefs.sh [--profile NAME] [PACKAGE]
# Default: fmt.0.11.0 with profile oxcaml-small

set -euo pipefail

PROFILE="${PROFILE:-oxcaml-small}"
PACKAGE="${1:-fmt.0.11.0}"
OUTPUT_DIR=$(mktemp -d)
trap 'rm -rf "$OUTPUT_DIR"' EXIT

echo "Testing Stdlib xref resolution: $PACKAGE under profile $PROFILE"
echo "Output dir: $OUTPUT_DIR"

# Build with docs. day11 build runs the full compile + link doc DAG
# for the package and its transitive deps, so the compiler package's
# compile phase runs and emits Stdlib.odoc.
day11 build --profile "$PROFILE" "$PACKAGE" --with-doc "$OUTPUT_DIR" -j 4

# Pick the package's main module page. The fmt opam package has
# multiple library subdirs; we want any Fmt index.html.
FMT_HTML=$(find "$OUTPUT_DIR" -name "index.html" -path "*/Fmt/*" 2>/dev/null | head -1)
if [ -z "$FMT_HTML" ]; then
  echo "FAIL: no Fmt module HTML produced under $OUTPUT_DIR"
  echo "Available HTML files:"
  find "$OUTPUT_DIR" -name "*.html" | head -10
  exit 1
fi
echo "Inspecting: $FMT_HTML"

# Negative assertion: no unresolved Stdlib spans. fmt frequently
# refers to Stdlib.Format.formatter / Stdlib.out_channel; if any of
# those land as <span class="xref-unresolved">Stdlib</span>.X, the
# compiler-package compile-phase didn't run (or its output isn't
# being stacked at link time).
UNRESOLVED=$(grep -c 'xref-unresolved">Stdlib<' "$FMT_HTML" || true)
if [ "$UNRESOLVED" != "0" ]; then
  echo "FAIL: $UNRESOLVED unresolved Stdlib xref(s) in $FMT_HTML"
  echo "Sample references:"
  grep -oE 'xref-unresolved">Stdlib[^<]*' "$FMT_HTML" | sort -u | head -5
  exit 1
fi

# Positive assertion: at least one resolved Stdlib href. We require
# both — a file with no Stdlib mentions at all could pass the
# negative check vacuously.
RESOLVED=$(grep -oE 'href="[^"]+">Stdlib(\.[A-Z][a-zA-Z_0-9]*)*' "$FMT_HTML" | wc -l)
if [ "$RESOLVED" = "0" ]; then
  echo "FAIL: no resolved Stdlib links found in $FMT_HTML"
  echo "fmt should reference Stdlib.Format / Stdlib.out_channel etc."
  exit 1
fi

echo "OK: $RESOLVED resolved Stdlib link(s), 0 unresolved Stdlib spans"
