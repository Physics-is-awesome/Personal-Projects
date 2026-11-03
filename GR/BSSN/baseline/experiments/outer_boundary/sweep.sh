#!/bin/sh
# Controlled outer-boundary sweep at fixed physical resolution.
#
# Each pair keeps dx approximately constant while changing only the domain:
#   12M: 48 x 96
#   18M: 72 x 144
#
# The run is stopped at t ~= 0.5, with horizon finding and snapshots disabled.
# The final step line is emitted by the driver and is the machine-readable
# summary for this small experiment.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"

if [ ! -x ./bin/vacuum_kerr ]; then
  echo "missing ./bin/vacuum_kerr; run: make bin/vacuum_kerr" >&2
  exit 1
fi

printf '%-8s %-8s %-8s %-8s %s\n' domain width strength N
for cfg in "12 48 96 560" "18 72 144 560"; do
  set -- $cfg
  domain=$1
  nx=$2
  nz=$3
  steps=$4
  for sponge in "0.0 0.0" "2.0 0.05" "4.0 0.1"; do
    set -- $sponge
    width=$1
    strength=$2
    par="/tmp/bssn_outer_${domain}_${width}.par"
    sed \
      -e "s/^nx   = .*/nx   = $nx/" \
      -e "s/^nz   = .*/nz   = $nz/" \
      -e "s/^xmax = .*/xmax = ${domain}.0/" \
      -e "s/^zmax = .*/zmax = ${domain}.0/" \
      -e "s/^nsteps    = .*/nsteps    = 1000000/" \
      -e "s/^out_every = .*/out_every = 1000000/" \
      -e "s/^outer_sponge_width    = .*/outer_sponge_width    = $width/" \
      -e "s/^outer_sponge_strength = .*/outer_sponge_strength = $strength/" \
      parameter_files/vacuum_kerr.par > "$par"
    printf '%-8s %-8s %-8s %-8s ' "$domain" "$width" "$strength" "$nx"
    GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS="$steps" \
      GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 \
      ./bin/vacuum_kerr "$par" 2>&1 | grep '^ step' | tail -1
  done
done
