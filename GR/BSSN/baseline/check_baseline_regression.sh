#!/usr/bin/env bash
# Regression guard for the 96x192 6000-step vacuum Kerr baseline.
#
# Usage: baseline/check_baseline_regression.sh <run.log>
#
# This is a regression guard, not an acceptance test.  The thresholds below
# were set from the 2026-11-04 final-tree baseline and deliberately leave
# margin; tighten them only with a new archived baseline and a documented
# reason.  It does not require the horizon finder to succeed at late times:
# the coverage-buffer rejection is a measured geometry result, not an abort.
set -euo pipefail

log=${1:?usage: $0 <run.log>}
if [[ ! -s "$log" ]]; then
  echo "baseline-regression: missing or empty log: $log" >&2
  exit 2
fi

# Hard failures that must never be normalized away.
if grep -qiE 'non-positive|not finite|NaN|Fortran runtime error|ERROR STOP' "$log"; then
  echo "baseline-regression: hard failure pattern found in $log" >&2
  grep -inE 'non-positive|not finite|NaN|Fortran runtime error|ERROR STOP' "$log" | head >&2
  exit 1
fi

last=$(grep '^ step ' "$log" | tail -1 || true)
if [[ -z "$last" ]]; then
  echo "baseline-regression: no ' step ' line in $log" >&2
  exit 2
fi

read -r step maxh rmsh maxm alphamin <<< "$(awk '/^ step /{s=$2; h=$6; hr=$8; m=$10; am=$14} END{print s, h, hr, m, am}' "$log")"

echo "baseline-regression: step=$step max|H|=$maxh rms|H|=$rmsh max|M|=$maxm alpha_min=$alphamin"

status=0
if [[ "$step" != "6000" ]]; then
  echo "baseline-regression: FAIL expected step 6000, got $step" >&2
  status=1
fi
awk -v v="$maxh" 'BEGIN{exit !(v <= 6.0e-2)}' || { echo "baseline-regression: FAIL max|H|=$maxh > 6.0e-2" >&2; status=1; }
awk -v v="$rmsh" 'BEGIN{exit !(v <= 3.0e-3)}' || { echo "baseline-regression: FAIL rms|H|=$rmsh > 3.0e-3" >&2; status=1; }
awk -v v="$maxm" 'BEGIN{exit !(v <= 4.0e-2)}' || { echo "baseline-regression: FAIL max|M|=$maxm > 4.0e-2" >&2; status=1; }
awk -v v="$alphamin" 'BEGIN{exit !(v >= 2.5e-1)}' || { echo "baseline-regression: FAIL alpha_min=$alphamin < 2.5e-1" >&2; status=1; }

if [[ "$status" -ne 0 ]]; then
  exit 1
fi
echo "baseline-regression: PASS"
