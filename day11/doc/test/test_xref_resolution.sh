#!/bin/bash
# Test that doc generation produces no unresolved cross-references.
#
# This catches the bug where doc layers are cached with incomplete
# dep compile layers, producing HTML with xref-unresolved spans.
#
# Usage: test_xref_resolution.sh [--profile NAME] [PACKAGE]
# Default: base.v0.17.3 with profile ocaml-ci

set -e

PROFILE="${PROFILE:-ocaml-ci}"
PACKAGE="${1:-base.v0.17.3}"
OUTPUT_DIR=$(mktemp -d)

echo "Testing xref resolution for $PACKAGE"
echo "Output: $OUTPUT_DIR"

# Build with docs
day11 build --profile "$PROFILE" "$PACKAGE" --with-doc "$OUTPUT_DIR" -j 4

# Check for unresolved xrefs
UNRESOLVED=$(find "$OUTPUT_DIR" -name "*.html" -exec grep -l 'xref-unresolved' {} \; 2>/dev/null)

if [ -n "$UNRESOLVED" ]; then
  echo "FAIL: Found unresolved cross-references in:"
  echo "$UNRESOLVED" | head -10
  echo ""
  echo "Example:"
  echo "$UNRESOLVED" | head -1 | xargs grep -o 'class="xref-unresolved">[^<]*<' | head -5
  rm -rf "$OUTPUT_DIR"
  exit 1
fi

N_HTML=$(find "$OUTPUT_DIR" -name "*.html" | wc -l)
echo "OK: $N_HTML HTML files, no unresolved xrefs"
rm -rf "$OUTPUT_DIR"
