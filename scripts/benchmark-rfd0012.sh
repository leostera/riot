#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

RUN_ID="$(date +%Y%m%d-%H%M%S)"
LOG_DIR="/tmp/rfd0012-bench-${RUN_ID}"
mkdir -p "$LOG_DIR"

BASELINE_BIN="${BASELINE_TUSK:-$(command -v tusk)}"
CANDIDATE_PIN="${CANDIDATE_TUSK:-/tmp/tusk-rfd0012-candidate}"

if [[ ! -x "$BASELINE_BIN" ]]; then
  echo "error: baseline tusk not executable: $BASELINE_BIN" >&2
  exit 1
fi

build_candidate_binary() {
  echo "[1/4] Building candidate tusk-cli with baseline binary: $BASELINE_BIN"
  "$BASELINE_BIN" build tusk-cli >"$LOG_DIR/candidate-bootstrap.log" 2>&1

  local candidate_src
  candidate_src="$(find _build -type f -path '*/out/tusk-cli/tusk' | head -n 1 || true)"

  if [[ -z "$candidate_src" ]]; then
    echo "error: could not locate candidate tusk binary under _build" >&2
    echo "bootstrap log: $LOG_DIR/candidate-bootstrap.log" >&2
    exit 1
  fi

  cp "$candidate_src" "$CANDIDATE_PIN"
  chmod +x "$CANDIDATE_PIN"
  echo "Pinned candidate binary: $CANDIDATE_PIN"
}

run_sequence() {
  local name="$1"
  local bin="$2"
  local clean_log="$LOG_DIR/${name}-clean.log"
  local build1_log="$LOG_DIR/${name}-build1.log"
  local build2_log="$LOG_DIR/${name}-build2.log"

  if [[ ! -x "$bin" ]]; then
    echo "error: ${name} binary not executable: $bin" >&2
    exit 1
  fi

  echo "[2/4] Running ${name}: clean"
  /usr/bin/time -p "$bin" clean >"$clean_log" 2>&1
  echo "[3/4] Running ${name}: first build"
  /usr/bin/time -p "$bin" build >"$build1_log" 2>&1
  echo "[4/4] Running ${name}: second build"
  /usr/bin/time -p "$bin" build >"$build2_log" 2>&1
}

read_metric() {
  local metric="$1"
  local file="$2"
  awk -v m="$metric" '$1==m {print $2; exit}' "$file"
}

fmt_ratio() {
  local numer="$1"
  local denom="$2"
  awk -v n="$numer" -v d="$denom" 'BEGIN { if (d == 0) { print "inf" } else { printf "%.2fx", n / d } }'
}

build_candidate_binary
run_sequence "baseline" "$BASELINE_BIN"
run_sequence "candidate" "$CANDIDATE_PIN"

BASELINE_BUILD1_REAL="$(read_metric real "$LOG_DIR/baseline-build1.log")"
BASELINE_BUILD2_REAL="$(read_metric real "$LOG_DIR/baseline-build2.log")"
CANDIDATE_BUILD1_REAL="$(read_metric real "$LOG_DIR/candidate-build1.log")"
CANDIDATE_BUILD2_REAL="$(read_metric real "$LOG_DIR/candidate-build2.log")"

echo
echo "Benchmark logs: $LOG_DIR"
echo "Baseline bin: $BASELINE_BIN"
echo "Candidate bin: $CANDIDATE_PIN"
echo
echo "real (seconds)"
echo "baseline first build : $BASELINE_BUILD1_REAL"
echo "baseline second build: $BASELINE_BUILD2_REAL"
echo "candidate first build : $CANDIDATE_BUILD1_REAL"
echo "candidate second build: $CANDIDATE_BUILD2_REAL"
echo
echo "candidate/baseline first-build ratio : $(fmt_ratio "$CANDIDATE_BUILD1_REAL" "$BASELINE_BUILD1_REAL")"
echo "candidate/baseline second-build ratio: $(fmt_ratio "$CANDIDATE_BUILD2_REAL" "$BASELINE_BUILD2_REAL")"
