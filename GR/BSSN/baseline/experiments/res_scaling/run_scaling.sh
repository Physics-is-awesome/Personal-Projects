#!/bin/sh
# Resolution-scaling measurement.
#
# Fixed physical setup: frozen gauge (GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2),
# r_excise = 1.0 M, outer domain 12 M, a = 0.8, exact Kerr-Schild initial data.
# Only Nx/Nz vary, always Nz = 2*Nx.
#
# Each run advances to t = 1.0 exactly and reports max|H|(ext) there.  Because
# max|H| grows essentially linearly from t=0, its value at a FIXED physical time
# is the cleanest available proxy for the error-injection rate, and it costs
# 4514 steps instead of running to failure (~7500-80000).
#
# out_every is huge so no HDF5 is written; GR_HORIZON_EVERY=0 disables the
# horizon solve.  The driver still prints the step line at n == nsteps.
set -e
cd "$(dirname "$0")/../../.."
for N in 48 96 192 384; do
  case $N in
    48)  NS=1120 ;;
    96)  NS=2251 ;;
    192) NS=4514 ;;
    384) NS=9039 ;;
  esac
  sed -e "s/^nx   = 48/nx   = $N/" \
      -e "s/^nz   = 96/nz   = $((2*N))/" \
      -e "s/^out_every = 100/out_every = 1000000/" \
      parameter_files/vacuum_kerr.par > /tmp/res_$N.par
  printf 'N=%-4s ' "$N"
  GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=$NS \
  GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 \
    ./bin/vacuum_kerr /tmp/res_$N.par 2>&1 | grep "^ step" | tail -1
done
