#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

# Default output file requested for live polling.
OUT_FILE="${1:-${REPO_ROOT}/krasny_verify_results.log}"
INTERVAL_SECONDS="${INTERVAL_SECONDS:-2}"
RUN_TIMEOUT_SECONDS="${RUN_TIMEOUT_SECONDS:-900}"

while true; do
  tmp="$(mktemp)"
  started_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

  {
    echo "started_at=${started_at}"
    echo "cwd=${REPO_ROOT}"
    echo "cmd=timeout ${RUN_TIMEOUT_SECONDS} riot test krasny:fixture_tests"
    echo "------------------------------------------------------------"
    (
      cd "${REPO_ROOT}"
      timeout "${RUN_TIMEOUT_SECONDS}" riot test krasny:fixture_tests
    )
    rc=$?
    echo "------------------------------------------------------------"
    echo "exit_code=${rc}"
    echo "finished_at=$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  } >"$tmp" 2>&1 || true

  mv "$tmp" "$OUT_FILE"
  sleep "$INTERVAL_SECONDS"
done
