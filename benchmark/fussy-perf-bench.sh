#!/usr/bin/env bash
# fussy-perf-bench.sh — run the bench under two separate `emacs -Q' children,
# one for fussy and one for flex, then concatenate the per-style reports.
#
# Usage:
#   bash fussy-perf-bench.sh                    # writes ~/fussy-perf-report.txt
#   bash fussy-perf-bench.sh /tmp/out.txt       # custom path
#
# Each child boots from a clean state (`emacs -Q'), adds the elpa
# subdirectories to load-path, mirrors the company config from
# jn-completion.el, and bootstraps its style (fussy: `fussy-setup-fzf'
# + `fussy-eglot-setup' + `fussy-company-setup'; flex: just sets
# `completion-styles' to '(flex)').
#
# This means the flex side never has fussy loaded at all — no risk of
# fussy advice or fussy-mutated defcustoms leaking into the baseline.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-$HOME/fussy-perf-report.txt}"

FUSSY_OUT="$(mktemp -t fussy-perf-fussy.XXXXXX)"
FLEX_OUT="$(mktemp -t fussy-perf-flex.XXXXXX)"

# Prefer Emacs 31 — the packages under ~/.emacs.d/elpa/31/ target it,
# and loading them into Emacs 30 produces misleading numbers.  Pick the
# first candidate whose --version says 31, fall back to $EMACS or `emacs'.
pick_emacs() {
  local candidates=(
    "/Users/james/Code/emacs/src/emacs-31.0.50.1"
    "/Applications/Emacs.app/Contents/MacOS/Emacs"
  )
  local c
  for c in "${candidates[@]}"; do
    if [[ -x "$c" ]] && "$c" --version 2>/dev/null | head -1 | grep -q ' 31\.'; then
      echo "$c"; return
    fi
  done
  echo "${EMACS:-emacs}"
}
EMACS="${EMACS:-$(pick_emacs)}"
echo "Using $EMACS ($("$EMACS" --version 2>&1 | head -1))"

cleanup() { rm -f "$FUSSY_OUT" "$FLEX_OUT"; }
trap cleanup EXIT

run() {
  local style="$1" out="$2" bootstrap_form="$3"
  echo "[$(date +%H:%M:%S)] running $style child -> $out"
  "$EMACS" -Q --batch \
    --eval "(setq load-prefer-newer t)" \
    -L "$HERE" \
    --load fussy-perf-bench \
    --eval "(fussy-perf-bench-bootstrap-load-path)" \
    --eval "(fussy-perf-bench-bootstrap-company)" \
    --eval "$bootstrap_form" \
    --eval "(fussy-perf-bench-run '$style \"$out\")" \
    2>&1 | sed "s/^/  [$style] /"
}

# --- fussy child: mirror the user's use-package fussy block ---
run fussy "$FUSSY_OUT" "(fussy-perf-bench-bootstrap-fussy)"

# --- flex child: no fussy at all, completion-styles=(flex) ---
run flex  "$FLEX_OUT"  "(fussy-perf-bench-bootstrap-flex)"

{
  echo "==============================================================="
  echo " fussy-perf-bench combined report"
  echo " fussy child : $FUSSY_OUT (now copied below)"
  echo " flex  child : $FLEX_OUT  (now copied below)"
  echo "==============================================================="
  cat "$FUSSY_OUT"
  echo
  echo "==============================================================="
  cat "$FLEX_OUT"
} > "$OUT"

echo "Combined report: $OUT"
