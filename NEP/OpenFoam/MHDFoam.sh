#!/bin/bash
#------------------------------------------------------------------------------
# MPDFoam_repo_setup.sh
#
# Prepares a version-controlled MPDFoam development environment in the OpenFOAM
# USER directory -- never in $WM_PROJECT_DIR.
#
# What it does:
#   1. Verifies the OpenFOAM environment is sourced and sane.
#   2. Reports whether you have (a) compile capability and (b) readable
#      mhdFoam sources -- these come from different packages.
#   3. Creates $WM_PROJECT_USER_DIR and git-inits it with an OpenFOAM-aware
#      .gitignore (build detritus, time directories, processor dirs).
#   4. Snapshots the pristine mhdFoam into reference/ and chmods it read-only,
#      so your Phase 0 ground truth cannot be edited by accident.
#   5. Writes top-level Allwmake / Allwclean that build the model libraries
#      before the solver, in dependency order.
#
# Run this BEFORE MPDFoam_bootstrap.sh.
#
# Usage:
#   source /usr/lib/openfoam/openfoam2412/etc/bashrc
#   chmod +x MPDFoam_repo_setup.sh
#   ./MPDFoam_repo_setup.sh
#------------------------------------------------------------------------------

set -eu

hr()  { printf '%*s\n' 74 '' | tr ' ' '-'; }
sec() { echo; hr; echo "== $*"; hr; }
ok()  { echo "  [ OK ]  $*"; }
warn(){ echo "  [WARN]  $*"; }
bad() { echo "  [FAIL]  $*"; }

#==============================================================================
# 1. ENVIRONMENT
#==============================================================================
sec "ENVIRONMENT CHECK"

if [ -z "${WM_PROJECT_DIR:-}" ]; then
    bad "WM_PROJECT_DIR unset. Source the OpenFOAM etc/bashrc first, e.g."
    echo "          source /usr/lib/openfoam/openfoam2412/etc/bashrc"
    exit 1
fi

ok "WM_PROJECT_DIR      = $WM_PROJECT_DIR"
ok "WM_PROJECT_VERSION  = ${WM_PROJECT_VERSION:-<unset>}"
ok "FOAM_API            = ${FOAM_API:-<unset>}"
ok "WM_COMPILE_OPTION   = ${WM_COMPILE_OPTION:-<unset>}"
ok "WM_LABEL_SIZE       = ${WM_LABEL_SIZE:-<unset>}"
ok "WM_PRECISION_OPTION = ${WM_PRECISION_OPTION:-<unset>}"

# ESI convention: $HOME/OpenFOAM/$USER-v2412
ROOT="${WM_PROJECT_USER_DIR:-$HOME/OpenFOAM/$USER-$WM_PROJECT_VERSION}"
ok "WM_PROJECT_USER_DIR = $ROOT"

if [ "${ROOT#"$WM_PROJECT_DIR"}" != "$ROOT" ]; then
    bad "User dir is INSIDE the OpenFOAM install tree. Refusing to continue."
    echo "        A package upgrade would destroy your work."
    exit 1
fi

#==============================================================================
# 2. CAPABILITY CHECK -- compile vs. read are separate packages
#==============================================================================
sec "CAPABILITY CHECK"

CAN_COMPILE=0
CAN_READ_MHD=0

# (a) Can we compile? Needs lnInclude headers + wmake.
if [ -f "$WM_PROJECT_DIR/src/finiteVolume/lnInclude/fvCFD.H" ]; then
    ok "Headers present   -> you can compile user solvers."
    CAN_COMPILE=1
else
    bad "fvCFD.H not found under src/finiteVolume/lnInclude."
    echo "        Install the dev package, e.g.:"
    echo "          sudo apt install openfoam${FOAM_API:-2412}-dev"
fi

if command -v wmake >/dev/null 2>&1; then
    ok "wmake on PATH     -> $(command -v wmake)"
else
    bad "wmake not on PATH. The environment is only partially sourced."
    CAN_COMPILE=0
fi

# (b) Can we read mhdFoam? Needs the source package.
MHD="$WM_PROJECT_DIR/applications/solvers/electromagnetics/mhdFoam"
if [ -d "$MHD" ]; then
    ok "mhdFoam sources   -> $MHD"
    CAN_READ_MHD=1
else
    FOUND=$(find "$WM_PROJECT_DIR/applications" -type d -name mhdFoam 2>/dev/null | head -n1 || true)
    if [ -n "$FOUND" ]; then
        MHD="$FOUND"
        ok "mhdFoam sources   -> $MHD  (non-standard path)"
        CAN_READ_MHD=1
    else
        warn "mhdFoam sources NOT found. Phase 0 inspection will have nothing"
        echo "          to read. Install them with:"
        echo "            sudo apt install openfoam${FOAM_API:-2412}-source"
        echo "          You can still compile without this."
    fi
fi

# (c) Which properties dictionary does this release expect?
if [ "$CAN_READ_MHD" -eq 1 ]; then
    if grep -rqI 'physicalProperties' "$MHD" 2>/dev/null; then
        warn "This release uses 'physicalProperties' (not 'transportProperties')."
        echo "          Reconcile dictionary names in the MPDFoam case setup."
    elif grep -rqI 'transportProperties' "$MHD" 2>/dev/null; then
        ok "This release uses 'transportProperties'."
    fi
fi

[ "$CAN_COMPILE" -eq 1 ] || { echo; bad "Cannot compile. Fix the above first."; exit 1; }

#==============================================================================
# 3. DIRECTORY SKELETON
#==============================================================================
sec "CREATING REPOSITORY SKELETON"

mkdir -p "$ROOT"/applications/solvers
mkdir -p "$ROOT"/src
mkdir -p "$ROOT"/tutorials/MPD
mkdir -p "$ROOT"/reference
mkdir -p "$ROOT"/doc
ok "Skeleton created under $ROOT"

#==============================================================================
# 4. .gitignore -- OpenFOAM generates a LOT of build detritus
#==============================================================================
sec "WRITING .gitignore"

cat > "$ROOT/.gitignore" <<'EOF'
#--- OpenFOAM build artefacts -------------------------------------------------
lnInclude/
Make/linux*/
Make/darwin*/
*.dep
*.o
*.so
*.a
platforms/

#--- Compiled binaries (they live in FOAM_USER_APPBIN anyway) -----------------
applications/bin/
bin/

#--- Run-time output ----------------------------------------------------------
log.*
*.log
logs/
processor*/
postProcessing/
VTK/
*.OpenFOAM
*.foam

#--- Time directories: keep 0/ and 0.orig/, ignore every other numeric dir ----
[1-9]*/
0.[0-9]*/
!0/
!0.orig/

#--- Mesh output (regenerable from blockMeshDict; keep the dict itself) -------
constant/polyMesh/
constant/extendedFeatureEdgeMesh/
constant/triSurface/*.eMesh

#--- Editor / OS --------------------------------------------------------------
*~
*.swp
.DS_Store
.vscode/
.idea/
compile_commands.json
EOF
ok ".gitignore written"

#==============================================================================
# 5. PRISTINE mhdFoam SNAPSHOT (read-only Phase 0 reference)
#==============================================================================
if [ "$CAN_READ_MHD" -eq 1 ]; then
    sec "SNAPSHOTTING PRISTINE mhdFoam"

    REF="$ROOT/reference/mhdFoam"
    if [ -d "$REF" ]; then
        warn "reference/mhdFoam already exists -- leaving it untouched."
    else
        mkdir -p "$REF"
        cp -r "$MHD"/. "$REF"/
        # Strip any build products that came along
        rm -rf "$REF"/Make/linux* "$REF"/lnInclude 2>/dev/null || true
        # Read-only: this is ground truth for diffing, not a scratch pad
        chmod -R a-w "$REF"
        ok "Snapshot at reference/mhdFoam (read-only)"
        echo "          Diff against it any time:"
        echo "            diff -u reference/mhdFoam/createFields.H \\"
        echo "                    applications/solvers/MPDFoam/createFields.H"
    fi

    # Record exactly which build this came from -- invaluable six months later
    cat > "$ROOT/reference/PROVENANCE.txt" <<EOF
mhdFoam reference snapshot
==========================
Captured      : $(date -u '+%Y-%m-%d %H:%M:%S UTC')
Source path   : $MHD
WM_PROJECT_DIR: $WM_PROJECT_DIR
Version       : ${WM_PROJECT_VERSION:-unknown}
FOAM_API      : ${FOAM_API:-unknown}
Build option  : ${WM_COMPILE_OPTION:-unknown}
Label size    : ${WM_LABEL_SIZE:-unknown}
Precision     : ${WM_PRECISION_OPTION:-unknown}
Host          : $(uname -srm)

This directory is READ-ONLY on purpose. It is the Phase 0 ground truth.
To refresh it:  chmod -R u+w reference/mhdFoam && rm -rf reference/mhdFoam
                then re-run MPDFoam_repo_setup.sh
EOF
    ok "Provenance recorded"
fi

#==============================================================================
# 6. TOP-LEVEL Allwmake / Allwclean (dependency order matters)
#==============================================================================
sec "WRITING TOP-LEVEL BUILD WRAPPERS"

cat > "$ROOT/Allwmake" <<'EOF'
#!/bin/bash
#------------------------------------------------------------------------------
# Build MPDFoam and its model libraries in dependency order.
#
# The libraries MUST precede the solver: Make/options links
# -lMPDconductivityModels and -lMPDplasmaTransport from $FOAM_USER_LIBBIN.
#
#   ./Allwmake          optimised build
#   ./Allwmake -j 8     parallel
#   ./Allwmake debug    -g -O0 for YOUR code, optimised OpenFOAM libs
#------------------------------------------------------------------------------
cd "${0%/*}" || exit 1

if [ -z "${WM_PROJECT_DIR:-}" ]; then
    echo "ERROR: source the OpenFOAM etc/bashrc first." >&2
    exit 1
fi

if [ "${1:-}" = "debug" ]; then
    shift
    export WM_CXXFLAGS="${WM_CXXFLAGS:-} -g -O0 -DFULLDEBUG"
    echo ">>> Debug flags active for user code (OpenFOAM libs stay optimised)"
fi

set -x

wmake libso src/conductivityModels    "$@" || exit 1
wmake libso src/plasmaTransport       "$@" || exit 1

wmake applications/solvers/MPDFoam    "$@" || exit 1

set +x
echo
echo "Build complete. Binary: $FOAM_USER_APPBIN/MPDFoam"
EOF

cat > "$ROOT/Allwclean" <<'EOF'
#!/bin/bash
#------------------------------------------------------------------------------
# Remove all MPDFoam build products. Does not touch $WM_PROJECT_DIR.
#------------------------------------------------------------------------------
cd "${0%/*}" || exit 1

wclean applications/solvers/MPDFoam
wclean libso src/plasmaTransport
wclean libso src/conductivityModels

rm -f "$FOAM_USER_APPBIN/MPDFoam"
rm -f "$FOAM_USER_LIBBIN/libMPDconductivityModels.so"
rm -f "$FOAM_USER_LIBBIN/libMPDplasmaTransport.so"

echo "Cleaned."
EOF

chmod +x "$ROOT/Allwmake" "$ROOT/Allwclean"
ok "Allwmake / Allwclean written and made executable"

#==============================================================================
# 7. GIT
#==============================================================================
sec "INITIALISING GIT"

cd "$ROOT"

if [ -d .git ]; then
    warn "Git repository already exists -- not re-initialising."
else
    git init -q
    ok "git init"

    cat > README.md <<EOF
# MPDFoam

Compressible resistive-MHD solver for self-field magnetoplasmadynamic
thrusters, built on OpenFOAM ${WM_PROJECT_VERSION:-v2412}.

## Formulation

2-D axisymmetric, poloidal-current-function form. The field is purely
azimuthal, B = B_theta(r,z) e_theta, so the solver transports

    psi = r * B_theta = mu0 * I_enclosed / (2 pi)

instead of the Cartesian vector B. Consequences, exact by construction
rather than by iteration:

  * div(B) = 0 identically  -> no magnetic-pressure projection, no pB field
  * div(J) = 0 identically  -> current conservation is a round-off diagnostic
  * psi IS the enclosed current -> electrode BCs are physically transparent

## Layout

    applications/solvers/MPDFoam/   solver
    src/conductivityModels/         runtime-selectable sigma(T, rho)
    src/plasmaTransport/            runtime-selectable mu(T,rho), k(T,rho)
    tutorials/MPD/                  cases
    reference/mhdFoam/              pristine Phase 0 reference (READ-ONLY)

## Build

    source \$WM_PROJECT_DIR/etc/bashrc
    ./Allwmake              # or: ./Allwmake debug

## Scope

Excluded from the baseline, by design: Hall effect, two-temperature
physics, finite-rate ionisation, radiation transport, anisotropic
transport, applied-field operation. The psi formulation cannot represent
applied-field or 3-D devices -- those need poloidal B components that do
not exist in this architecture.
EOF
    ok "README.md written"

    git add -A
    git -c user.name="${GIT_AUTHOR_NAME:-MPDFoam}" \
        -c user.email="${GIT_AUTHOR_EMAIL:-mpdfoam@localhost}" \
        commit -q -m "Initial skeleton: build wrappers, gitignore, mhdFoam reference" \
        || warn "Commit skipped (configure git user.name/user.email)."
    ok "Initial commit"
fi

#==============================================================================
# 8. NEXT STEPS
#==============================================================================
sec "NEXT STEPS"
cat <<EOF

  1. Phase 0 -- read before writing:
       ./inspect_mhdFoam.sh | tee doc/mhdFoam_phase0_dump.txt

  2. Generate the source tree, libraries and dictionaries:
       ./MPDFoam_bootstrap.sh

  3. Copy in the nine physics files:
       MPDFoam.C  createFields.H  solveFluid.H  pEqn.H  solveEnergy.H
       solveMagneticField.H  calculateLorentzForce.H  solvePotential.H
       calculateDiagnostics.H
     -> applications/solvers/MPDFoam/

  4. Build:
       ./Allwmake

  5. Commit a known-good baseline BEFORE touching physics:
       git add -A && git commit -m "Phase 1: compiling MPDFoam skeleton"

  Tag each validated milestone. When Phase 4 misbehaves you will want to
  bisect against a Phase 3 that demonstrably worked:
       git tag -a phase2-hydro-validated -m "Sod + nozzle within tolerance"

  Repository: $ROOT
  Binary:     \$FOAM_USER_APPBIN/MPDFoam

EOF
hr

