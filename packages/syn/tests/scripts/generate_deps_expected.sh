#!/usr/bin/env bash

set -euo pipefail

fixtures_dir="${1:-packages/syn/tests/deps_fixtures}"
ocamldep_bin="${OCAMLDEP:-$HOME/.riot/toolchains/5.5.0-riot.1/aarch64-apple-darwin/bin/ocamldep}"

find "$fixtures_dir" -type f \( -name '*.ml' -o -name '*.mli' \) | sort | while read -r file; do
  expected_path="${file%.*}.expected.ocamldep"
  "$ocamldep_bin" -modules "$file" > "$expected_path"
done
