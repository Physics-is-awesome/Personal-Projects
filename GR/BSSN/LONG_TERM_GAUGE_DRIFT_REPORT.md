# BSSN Kerr Cartoon Solver: Architecture, Drift Failure, and Repair Plan

**Report date:** 2026-10-12  
**Repository:** `GR/BSSN` (the enclosing Git root is one directory above this project)  
**Status:** investigation and experimental work are uncommitted. The long-term drift is **not fixed**.

This report describes the current implementation and the evidence available for its failure. It separates source-code facts and reproduced gates from interpretations that remain hypotheses. It is intended as a technical handoff, not a claim that the solver is production-ready.

## 1. Executive summary

The code is an axisymmetric vacuum BSSN evolution in a 2D Cartesian cartoon plane. The state is stored on `y=0`, with `x >= 0` and both signs of `z`; off-plane values are synthesized by radial interpolation and rotation. Initial data are a spinning Kerr black hole in Kerr–Schild coordinates. The production RHS is a second-order-in-time, second-order-in-space BSSN system with finite-difference geometry, 1+log lapse by default, and a legacy Gamma-driver shift by default. Excision is imposed on a Kerr-radius surface; points inside it are filled by extrapolation. RK4 advances the exterior, with optional KO dissipation, outer Sommerfeld terms, and an opt-in experimental inner SAT.

The long-time failure mechanism is not solved. The code is not initialized in stationary moving-puncture/trumpet coordinates, and its default shift driver is explicitly not a stationary puncture gauge. The resulting gauge transient moves the apparent horizon relative to the fixed excision surface. At later times, the conformal metric loses positive determinant near the inner boundary and the run aborts. Moving the excision boundary inward has not cured the instability; it can make failure earlier because the region is poorly resolved. Freezing lapse and shift also failed in prior runs, so the problem cannot yet be attributed to gauge drift alone.

The present safeguards improve diagnosis: the determinant failure is detected at the first invalid state, the apparent-horizon finder can monitor coordinate/gauge motion, and an inner-boundary diagnostic now reports the **lapse-gauge characteristic family only** using the Kerr-surface normal and the slicing-dependent Bona–Masso speed. That diagnostic is not the full characteristic spectrum. The existing SAT is explicitly marked reduced/experimental rather than exact. `make check` passed after these code changes. A 700-step baseline previously matched the documented legacy output, but that particular run preceded the latest normal-vector diagnostic-only adjustment.

The most credible route to a lasting repair is staged: establish a stationary Kerr trumpet or otherwise stationary Kerr data/gauge pair; build and verify the principal symbol of the exact production evolution system; supply a boundary treatment for all incoming fields consistent with that system; then demonstrate convergence and bounded horizon/constraint measures over long physical time. The existing Schwarzschild 1+log module is a valuable reference and exact anchor, but it does not supply Kerr trumpet data.

## 2. Evidence labels used below

- **Verified in source:** directly visible in the current Fortran implementation/configuration.
- **Verified gate/run:** passed a test or has a recorded run result. Historical result means it is reported from the prior work record; this report did not rerun every long simulation.
- **Observed failure:** recorded behavior, not a complete causal explanation.
- **Hypothesis:** plausible explanation that still requires a discriminating experiment.
- **Not established:** must not be treated as fact or used to justify a “fix.”

The verified computational findings supplied in the project handoff remain authoritative. In particular, the earlier general-metric derivations, constraint results, and Schwarzschild targets are not re-derived here; their limitations for Kerr are stated explicitly.

## 3. What the production program does

### 3.1 State and coordinate convention

`init/vars.F90` defines 24 evolved fields, stored as `u(i,k,ivar)`:

| Slot(s) | Meaning |
|---|---|
| 1 | `phi`, logarithmic BSSN conformal factor |
| 2–7 | symmetric conformal metric `gtilde_ij`: `xx,xy,xz,yy,yz,zz` |
| 8 | trace of physical extrinsic curvature, `K` |
| 9–14 | symmetric trace-free conformal extrinsic curvature `Atilde_ij` |
| 15–17 | contracted conformal connection `Gammatilde^i` |
| 18 | lapse `alpha` |
| 19–21 | shift `beta^i` |
| 22–24 | auxiliary Gamma-driver variable `B^i` |

The physical 3-metric is `gamma_ij = exp(4 phi) gtilde_ij`. The code uses a determinant-one conformal metric convention after projection. The stored tensor/vector components are Cartesian components, including the suppressed `y` components; they are not spherical or orthonormal components.

The computational plane is `y=0`: `x` spans `[0,xmax]`, and `z` spans `[-zmax,zmax]`. `dy=dx`. Axisymmetry means invariance under rotations about the z-axis, not that y-components vanish. In particular, off-plane vector and tensor components must be rotated.

### 3.2 Cartoon reconstruction

`helper/cartoon.F90:get_neighbor` first returns an ordinary stored neighbor when the requested point has `y=0` and lies within the stored positive-x range. For y-offset points or negative-x axis ghosts, it computes the cylindrical radius `varpi=sqrt(X^2+Y^2)` and azimuth `theta=atan2(Y,X)`, quadratically interpolates every stored field along the x-row, then rotates:

- scalar fields are copied;
- vectors (`Gammatilde`, `beta`, `B`) rotate in the x-y plane;
- symmetric rank-2 tensors (`gtilde`, `Atilde`) receive the two-index tensor rotation.

The interpolation is a three-point Lagrange polynomial, with the stencil clamped to the first or last three x points near the grid ends. This is a central piece of the 2D-to-3D operator, not merely ghost-cell bookkeeping. Any parity/order error here affects y and mixed derivatives and can contaminate the axis. `make test-cartoon` exercises this module; a passing unit test does not establish long-time stability of its coupling to the near-excision stencil.

### 3.3 Initial Kerr–Schild data

`init/init_data.F90` builds the Kerr–Schild spatial metric from
`gamma_ij = delta_ij + 2 H l_i l_j`, with the spin-dependent Kerr radius, Kerr–Schild null direction, lapse `alpha=1/sqrt(1+2H)`, and covariant shift `beta_i=2H l_i`. It inverts the spatial metric for `beta^i`, obtains `K_ij` from spatial derivatives of the shift, and converts `(gamma_ij,K_ij)` into BSSN fields. The conformal factor has two selectable definitions; default mode 0 sets `phi=(1/12) ln det(gamma)`. Mode 1 uses the Kerr–Schild `psi^4=1+2H` convention. Initial `Gammatilde` and `B` are set to zero before contracted conformal connections are computed.

The default data have the Kerr–Schild lapse and shift, not a stationary 1+log lapse/shift. The source contains an optional lapse-shaping knob, but its own parameter comments say it injects a constraint violation and is diagnostic only. It is not a substitute for solving equilibrium initial data. The center parameters are present in configuration; the visible Kerr point routine evaluates coordinates supplied by the caller and the driver supplies grid coordinates. Nonzero-center support should be audited separately before it is relied on.

### 3.4 Finite differences and geometry

`helper/stencil.F90` assembles values and derivatives using `get_neighbor`, the cartoon reconstruction, and the excision status mask. It uses fourth-order centered x/z derivatives when a full two-point stencil is safe, second-order centered derivatives when only nearest neighbors are available, and one-sided second-order formulas near the excision edge. The y derivatives come from cartoon points at `y=+dy` and `-dy`; mixed derivatives are assembled from corresponding off-plane/diagonal samples. Thus formal order is spatially nonuniform near both the axis and the excision surface.

`helper/geometry.F90` reconstructs the conformal metric and its inverse, builds conformal Christoffels, computes the conformal Ricci tensor from derivatives and the contracted connection, adds the conformal-factor Ricci terms, and computes the physical Hessian of lapse. `integrators/bssn_rhs.F90` consumes these fields. A small determinant or inverse-metric failure can therefore affect many RHS terms at once; the validity projection checks the determinant before normalizing it.

### 3.5 BSSN and gauge evolution

The point RHS in `integrators/bssn_rhs.F90` computes the standard vacuum BSSN evolution structure:

- `phi` evolves from `-alpha K/6`, shift advection, and one-sixth shift divergence;
- `gtilde_ij` evolves from `-2 alpha Atilde_ij` plus shift Lie/advection terms with the trace part removed;
- `K` uses the physical lapse Laplacian, `Atilde_ij Atilde^ij`, `K^2/3`, and shift advection;
- `Atilde_ij` uses lapse Hessian, Ricci, trace-free projection, quadratic curvature, and shift terms;
- `Gammatilde^i` uses second derivatives of shift, connection/metric terms, lapse gradients, curvature terms, and derivatives of K.

Matter-source branches are present in the RHS, although the `vacuum_kerr` driver calls it without matter. Constraints are calculated separately by `helper/constraints.F90`.

Default slicing mode 0 is 1+log in coordinate-time form:
`partial_t alpha = beta^i partial_i alpha - 2 alpha K`.
Mode 1 uses maximal slicing form `partial_t alpha = beta^i partial_i alpha - alpha K`; mode 2 freezes the lapse. Default gamma-driver mode 0 retains the legacy Lie/material derivative conversions and damping. Source comments explicitly state it damps the driver variable toward zero, not toward a stationary puncture profile.

Experimental shift modes in the RHS/configuration are:

| Mode | Implemented intent | Evidence/limit |
|---|---|---|
| 0 | Legacy Gamma-driver with Lie/material conversions | Production default; baseline preserved; not a stationary puncture gauge |
| 1 | Analytic profile driver toward `puncture_sign*2M x^i/r^3` | Experimental and explicitly unvalidated; profile normalization is not established |
| 2 | Freeze Kerr–Schild shift and `B` | Control experiment; prior long run failed, so fixed gauge alone is insufficient |
| 3 | Advective Brown-style Gamma-driver | Manufactured RHS test; long-term Kerr stability not established |
| 4 | Non-advective `partial_t beta=3B/4`, `partial_t B=partial_t Gammatilde-eta B` | Stationarity candidate; Schwarzschild fixture lowers RHS_B but Kerr stability not established |

The lapse/shift modes are not interchangeable with initial data. A gauge equation may be implemented correctly while the initial slice has a large transient; likewise, a stationary-shift ansatz does not guarantee a stationary full Kerr BSSN state.

### 3.6 Excision geometry and fill

`helper/excision.F90` uses the oblate Kerr radius
`r_K^2 = [rho^2-a^2 + sqrt((rho^2-a^2)^2+4a^2 z^2)]/2`, where `rho^2=x^2+z^2`, to classify points inside `r_excise`. It maintains a permanent geometric mask and a per-fill working status. The working mask is reset before each fill; previously filled interior points are then repopulated, rather than accidentally frozen at their first extrapolated values. The driver saves initial background data for the optional SAT.

Excision points are excluded from the RHS. `extrapolate_excised` fills the interior/needed stencil region from exterior data using a bounded extension of configured order. This is not a characteristic boundary condition: extrapolation supplies values to finite-difference stencils, but does not itself establish that all incoming continuum modes are controlled. The test suite covers mask/fill behavior and SAT smoke checks.

### 3.7 Time integration and outer boundary

`integrators/time_step.F90` advances with classical four-stage RK4. Before stage 1 and each intermediate stage it refreshes excision values. After intermediate states it projects BSSN algebraic conditions; after all RHS assemblies each stage applies, in sequence, the optional legacy SAT, KO dissipation, and Sommerfeld boundary contributions. The final state is projected too.

The projection floors lapse at `1e-6`, rejects NaN/nonpositive conformal determinant with a location and step diagnostic, rescales `gtilde` to unit determinant, and removes the trace of `Atilde`. It does **not** project Hamiltonian or momentum constraints and cannot correct the continuum gauge/excision mismatch. The driver chooses `dt = cfl*min(dx,dz)/sqrt(2)` to account for the fastest 1+log lapse characteristic estimate. This is a CFL choice, not a proof of stability for all coupled BSSN, shift, cartoon, or boundary modes.

`integrators/ko_diss.F90` supplies high-frequency dissipation. `integrators/sommerfeld.F90` supplies outer-boundary terms. The exact numerical effectiveness of those terms in the observed failure is not yet isolated. `t` is accumulated by `t=t+dt` each step; reducing the step count or changing output cadence does not constitute a drift fix. Long-time comparisons must use equal physical time, as established in prior work.

### 3.8 Driver, diagnostics, horizons, and files

`drivers/vacuum_kerr.F90` reads the parameter file, lays out the grid/state, initializes Kerr data and conformal connections, initializes excision/stencils, fills the excised region, reports initial constraints and the inner lapse-gauge characteristic estimate, optionally initializes the horizon finder, writes the step-zero diagnostics, and enters the RK4 loop. It can report near-boundary field magnitudes and optionally inspect shift/connection behavior. Environment variables override run settings (for example `GR_NSTEPS`, `GR_HORIZON_EVERY`, `GR_HZ_ABORT`, `GR_REXCISE`, `GR_GAMMA_DRIVER_MODE`, `GR_SLICING_MODE`, and `GR_EXCISION_SAT`).

`helper/horizon.F90` finds an apparent horizon from the evolved slice and computes area, irreducible mass, angular momentum, and horizon mass diagnostics. It is an independent measurement of the evolved data, not a gauge condition. The finder has passed Kerr exact-data and multistart tests. Horizon coordinate motion is a useful symptom; it does not alone distinguish a pure coordinate drift from a physical/numerical change in the data.

`helper/output_test.F90` is linked by the production source list as the output implementation (despite the filename); it writes snapshots and diagnostics including HDF5 state/diagnostics in the current version. `helper/output.F90` is also present and should not be assumed to be the linked implementation without checking the current Makefile. Output files in `output/` and `horizon.png` are generated artifacts, not source evidence.

## 4. Current parameters and reproducible commands

`parameter_files/vacuum_kerr.par` currently specifies `Nx=48`, `Nz=96`, `xmax=zmax=12M`, `M=1`, spin `a=0.8M`, centered hole, `eta=2/M`, `cfl=0.005`, one million steps, output and horizon cadence 100, KO strength 0.2, `r_excise=1.0M`, extension order 2, and `excision_sat=false`. With this grid, `dx=12/47` and `dz=24/95`, approximately 0.255M and 0.253M. The outer boundary is 12M from the center.

The historic baseline command is:

```sh
GR_HORIZON_EVERY=0 GR_HZ_ABORT=0 GR_NSTEPS=700 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
```

The recorded output was:

```text
step 700  t=0.6252  max|H|(ext)=1.3162E-01  rms|H|(ext)=3.8587E-03  max|M|(ext)=5.6664E-02  alpha(probe)=0.910348
```

This is a regression baseline, **not** a long-time accuracy target. The external constraints are visibly nonzero at this resolution/time. The exact run was recorded before the final edit to the lapse-gauge diagnostic normal; that edit does not enter the evolution RHS, but the baseline has not been rerun after that diagnostic-only change.

`make check` is the current code gate. Its Makefile checks that every module compiled by a test appears in production `SOURCES` (`check-parity`), builds production, and runs the test targets. The most recent check after the diagnostic and SAT-label corrections passed with `check: production build and full test suite OK`. This verifies build/test consistency and component tests; it does not test million-step stability.

The 700-step time is not one million-step validation. At the present timestep, 20,000 steps are only a finite physical-time experiment; acceptance must state both steps and accumulated time.

## 5. What is going wrong: established facts

1. **The default evolution is not initialized in equilibrium 1+log gauge.** Initial data are Kerr–Schild; default lapse is 1+log and default shift driver is mode 0. The mismatch necessarily launches a gauge adjustment. The apparent horizon is allowed to move relative to the fixed excision surface.
2. **Long-run candidates have failed at the inner region.** Historical mode-4 runs with `r_excise=1` and `0.5` failed after about 18,285 and 14,342 steps, respectively, on a nonpositive conformal determinant. A mode-4 order-1 run at `r_excise=1` failed near step 14,889. An attempted `r_excise=0.25` run failed much earlier (~1,314 steps), with the excision surface substantially under-resolved. These are recorded runs, not rerun for this report.
3. **The horizon finder eventually loses a usable horizon in some candidates.** Recorded in prior experiments, around step ~3,000 in some mode-4 cases. That is consistent with deteriorating evolved fields but is not by itself a root-cause proof.
4. **Freezing the initial Kerr–Schild gauge did not yield a stable long run.** Prior controls with frozen lapse/shift failed around 7,536 steps; enabling the reduced SAT failed around 8,600. Thus gauge transient is a real concern but not a sufficient explanation of all failures.
5. **The last failure is reported as a negative/non-finite conformal determinant.** The projector catches it before normalizing `gtilde`; this is the correct invariant to guard against. A negative determinant indicates loss of a positive-definite spatial metric representation, but does not identify which preceding RHS term or stencil caused it.
6. **The failure coordinates cluster near the excision region.** Examples include locations at/near the excision surface. This points investigation toward inner boundary, stencil, gauge, and resolution interactions; it does not establish which is causal.
7. **The inner characteristic diagnostic was previously over-described.** It now uses a Kerr level-set normal and the Bona–Masso lapse speed. For 1+log, the gauge contribution is `-beta_n + sqrt(2 alpha gamma^{nn})`; for maximal slicing it is `-beta_n + sqrt(alpha gamma^{nn})`; frozen lapse has no propagating lapse-gauge speed in this check. It still checks only that family, so its point count is not the incoming-mode count of the entire BSSN system.
8. **The current SAT is not a validated production characteristic boundary treatment.** It applies selected reduced 3x3 blocks to a second-order-in-space BSSN state. The code now declares `exact_excision_characteristics=.false.` and the test makes sure that reduced SAT cannot be mislabeled exact. SAT remains disabled by default.

## 6. Main theories, ranked by current evidence

These are competing or interacting explanations, not established roots.

### A. Gauge/data mismatch and horizon migration — strong evidence as a contributor

The reason for the original failure is independently described in the project history: no active puncture gauge keeps the horizon away from the excision surface. The code facts support that: Kerr–Schild initial data, default 1+log lapse, and legacy Gamma driver not anchored to a trumpet profile. Mode-4 stationary-shift behavior is only a candidate, and the analytic puncture profile mode has unvalidated normalization. This theory explains horizon drift and why changing the shift gauge can alter run lifetime. It does not explain why frozen gauge also eventually fails.

**Discriminating test:** use a stationary Kerr data/gauge pair with independently checked constraints and Killing/stationarity residuals, then measure horizon geometry and coordinate location over equal physical-time intervals. Do not tune the published output values as shooting inputs.

### B. Inner-boundary characteristic/stencil mismatch — strong structural concern

The evolved domain has a curved Kerr-radius excision surface crossing a Cartesian stencil. Interior values are extrapolated. A generic incoming lapse gauge mode is detected, but the full principal symbol and characteristic decomposition for the actual production RHS are not available. The optional SAT is reduced and cannot be used as proof that all incoming modes are controlled. This is a concrete structural gap, although not yet proven as the numerical trigger.

**Discriminating test:** derive/evaluate the normal principal symbol from the production RHS and its precise variables/reduction; compare eigenvalues and eigenfields against an independent symbolic or manufactured linearized calculation at nonzero lapse, shift, and curved normal. Verify each incoming field with nonzero manufactured data and test the implemented penalty’s energy estimate/sign convention. Then run matched SAT/no-SAT short tests with fixed data and track the first growing near-boundary mode.

### C. Near-excision resolution and nonuniform stencil order — plausible contributor

Default spacing is about 0.25M; a 0.25M excision radius is consequently only one coordinate cell in scale. The early failure at that radius is consistent with inadequate resolution and wide stencils across a highly curved mask. Stencil order drops near the excision boundary. No controlled convergence study currently proves this is dominant.

**Discriminating test:** hold physical time, gauge, radius in units of M, and outer domain fixed while refining Nx/Nz together; report horizon invariants, all constraint components, determinant minima, and convergence of the first-failure time. Separately vary r_excise at fixed resolution in a range with several points across the geometric scale, ensuring the surface remains inside the horizon.

### D. Cartoon axis parity/interpolation or momentum-constraint defect — active lead, not yet isolated

The cartoon method synthesizes y derivatives and negative-x ghosts through interpolation and rotation. A historical momentum-constraint convergence test had a plateau near 2.5e-2, with an axis-parity artifact suspected. The enhanced frozen-Kerr–Schild diagnostic now shows that `M_z` near-band maximum remains about `1.1e-1` to `1.25e-1` under 2x refinements and occurs at the first positive x point (`x=dx`) close to the polar horizon. Its near-band RMS does decrease (`3.15e-2`, `2.09e-2`, `1.39e-2` on 48x96, 96x192, 192x384), suggesting a localized feature, but the max does not converge. This materially strengthens the axis-adjacent lead while leaving two possibilities: a cartoon derivative/initial-data axis issue or a momentum-constraint implementation error. This is distinct from, and not yet shown to cause, the long-time determinant failure.

**Discriminating test:** first compare the BSSN momentum constraint against an independently evaluated physical ADM `D_j(K^{ij}-gamma^{ij}K)` at the same generic and axis-adjacent points, using an analytic or automatic-differentiation route that does not differentiate already finite-differenced `K_ij`. Then feed analytic axisymmetric scalar/vector/tensor fields through the exact production stencil and measure each derivative, including every component and mixed derivative as x approaches zero. Report all components and compare to a genuinely independent 3D Cartesian evaluation.

### E. Bulk RHS or formulation-stability issue — still possible

The frozen Kerr–Schild control fails after thousands of steps. That points to possible discretization, outer boundary, algebraic projection, or formulation stability problems beyond a moving gauge. The code has an exact-Kerr stationary RHS diagnostic module, but its outputs need to be distinguished from evolution stability and from finite-difference/ghost effects. The current baseline constraints are nonzero and the run is low resolution.

**Discriminating test:** establish RHS residuals for exact Kerr–Schild data at increasing resolution and multiple radii, then run frozen-gauge evolution with larger domain/refinement and compare the field/time of first growth. Examine all constraint components and characteristic/constraint variables, not only scalar H or one momentum component.

### F. Outer boundary and dissipation interaction — lower evidence, not excluded

The outer boundary is at 12M and Sommerfeld and KO terms are active. Failure points reported so far are near the inner surface, so this is less directly suggested. Long-time reflected/growing modes can still return from the outer boundary. No controlled outer-radius or KO-strength study establishes their role.

**Discriminating test:** vary outer radius at fixed near-hole resolution and compare equal-time near-hole histories; vary KO strength only after a convergent baseline is established. Dissipation must not be tuned to mask a wrong boundary or gauge.

## 7. Why prior apparent fixes were insufficient

- **Changing only Gamma-driver mode:** can change the transient and the failure time, but mode 4 is not a Kerr equilibrium solve and does not correct the inner boundary. Shorter time-to-failure in some choices is not proof the equation is “more wrong”; it can expose a different unstable mode.
- **Moving `r_excise` inward:** the event horizon condition alone is insufficient. A smaller excision radius puts the computational boundary closer to the singular/strong-field region and gives fewer grid points per curvature scale. The 0.25M run failed early.
- **Turning on SAT:** the reduced block penalty is a smoke-tested experimental intervention. The frozen-gauge SAT run failing later than no-SAT is not enough to validate the map or show causation.
- **Reducing timestep / comparing steps:** a smaller timestep can improve time-discretization stability but cannot repair a continuum gauge mismatch, unresolved boundary, or incorrect characteristic treatment. Compare equal physical time and convergence, not equal step count.
- **Clamping determinant or lapse harder:** lapse flooring and algebraic projection already exist. They protect representation/avoid invalid arithmetic, not the Hamiltonian/momentum constraints or continuum evolution. A determinant floor would conceal loss of positive definiteness rather than solve it.
- **Using Schwarzschild trumpet numbers to fit Kerr:** the equilibrium module validates exact spherical Schwarzschild outputs. It does not establish a rotating solution, and target values must remain outputs from the solve.

## 8. Changes made in the current line of work

The project history includes many experimental changes. These are the currently verifiable recent changes relevant to long-term drift and diagnostics:

1. **Diagnostic speed corrected for slicing dependence.** The Bona–Masso characteristic factor in `helper/excision.F90` now follows `alpha*sqrt(f gamma^{nn})`. It no longer uses the old `alpha*sqrt(2 gamma^{nn})` expression for 1+log. Maximal and frozen-lapse modes have separate handling.
2. **Diagnostic normal corrected to the Kerr excision surface.** The check now computes the Euclidean normal covector from the gradient of the Kerr-radius level set, rather than using `(x/r,z/r)` as if the surface were spherical. Its contractions use the physical inverse metric.
3. **Diagnostic wording narrowed.** `drivers/vacuum_kerr.F90` calls the output “incoming lapse-gauge points/modes.” It no longer claims that this count describes the whole system.
4. **SAT capability label corrected.** `helper/excision_sat.F90` now sets `exact_excision_characteristics=.false.` and documents the selected blocks as reduced/experimental for production. `helper/test_excision_sat.F90` enforces this expectation. This is an important correction to interpretation, not a new stabilizing boundary algorithm.
5. **Test gate passed.** `make check` passed after these changes. No full-duration run after the latest diagnostic-normal edit has been recorded. None of these recent changes changes the BSSN RHS evolution; they affect the diagnostic, SAT metadata/comments, and test assertion.

6. **Stationary Kerr–Schild diagnostic expanded during the current work.** `helper/diagnose_stationary_ks_rhs.F90` now reports per-field max/RMS RHS in fixed physical-radius bands, plus Hamiltonian and all three momentum components in those bands. `make diagnose-ks-stationarity` completed at 48x96, 96x192, and 192x384 (same 12M domain, spin 0.8, frozen Kerr–Schild lapse/shift). The non-axis RHS components and Hamiltonian generally decrease with refinement; the near-band `M_z` maximum does not. Its maximum is at `x=dx`, approximately `z=-1.64,-1.57,-1.535M` as dx halves, approaching the polar horizon location. The corresponding near-band max is about `0.111, 0.120, 0.125`; near-band RMS decreases about `0.0315, 0.0209, 0.0139`. This is new diagnostic evidence, not an independent proof that the momentum implementation is wrong: the diagnostic uses the production cartoon stencil and production BSSN constraint routine. It also does not establish that this issue causes the long-run determinant failure.

The prior work also added/maintained gauge modes, excision mask refresh, early invalid-state detection, horizon monitoring, Schwarzschild equilibrium tools, FO/SBP/characteristic research modules, and numerous tests. They remain uncommitted in the shared worktree; use `git diff` before treating any source as a clean baseline.

## 9. Required repair programme

This is the shortest defensible sequence. Do not skip ahead to a production claim.

### Phase 1: Freeze a reproducible baseline

Record compiler/version/flags, exact git diff, full parameter file and environment, grid, physical final time, and output hashes. Run `make check`; reproduce the 700-step baseline. Run a short horizon-monitored legacy baseline and archive complete logs. Keep output directories separate for each experiment.

### Phase 2: Validate the exact production RHS and geometry

Use `diagnose_stationary_ks_rhs` and analytic Kerr–Schild data to verify all evolved RHS components at all exterior points under frozen gauge, with separate bulk/axis/excision-ring norms. Repeat with refinement and an independent derivative/metric route. Establish whether the stationary residual converges and whether the first unstable component is already visible at t=0.

### Phase 3: Build a Kerr equilibrium data/gauge pair

The immediate physical target in the earlier programme was stationary 1+log Schwarzschild; its verified module reproduces `alpha_AH=0.376179486830` and `R_c/M=1.540569415042`. Those are valuable regression outputs but are not Kerr fit parameters. For spin 0.8, choose an analytically defined stationary Kerr trumpet family or solve the stationary gauge/Einstein system. Initialize the full BSSN state including lapse, shift, extrinsic curvature, conformal connection and driver variables. Verify:

- all Hamiltonian and three momentum components;
- stationarity residuals for every evolved field under the production RHS;
- horizon area, mass and angular momentum against independent Kerr values;
- regularity at the trumpet surface and asymptotic behavior;
- convergence in resolution and coordinate transformations.

The cited Kerr trumpet paper provides an independent family of stationary Kerr trumpet coordinates. The code must still implement and validate a compatible coordinate/data construction; a paper citation is not validation of this implementation.

### Phase 4: Derive the actual inner-boundary system

Specify the exact PDE principal part and any first-order reduction used at the inner boundary. Derive the normal characteristic matrix for the variables actually evolved, including gauge and constraint sectors. Validate the spectrum and all fields independently. Then implement a SAT/penalty or another excision condition with a documented sign convention and full coverage of incoming modes. Keep the current reduced SAT disabled until that gate passes.

### Phase 5: Convergence and equal-time drift experiment

Use at least three resolutions, fixed physical domain and excision radius in M, identical continuum data/gauge, and compare at the same physical times. Track:

- apparent-horizon area, spin, mass, shape/coordinate location;
- exterior H and each of the three M components, as max and RMS;
- minimum conformal determinant and its coordinate;
- lapse minimum and near-boundary metric/A/K maxima;
- RHS stationarity norms and incoming characteristic amplitudes;
- first-failure time and whether it moves outward/inward with resolution.

Acceptance means bounded/convergent diagnostics for the requested physical duration and no determinant failure, with a well-defined horizon throughout. A run that merely survives longer, or has smaller residual in one constraint component, is not acceptance.

### Phase 6: Production defaults

Only after the preceding gates pass should the production parameter file/defaults change. Keep legacy baseline reproducible under an explicit legacy configuration, document the accepted Kerr parameter range, and add a long-run regression with equal-time comparisons. Do not decrease elapsed simulation time to create a passing test.

## 10. Research resources and what they establish

1. **Baumgarte & Shapiro, stationary 1+log Schwarzschild trumpet**, [arXiv:0810.0006](https://arxiv.org/abs/0810.0006). This is the authority for the spherical stationary 1+log solution and the output anchors reported by `helper/equilibrium_1pluslog.F90`. The supplied project notes give the paper source at `/tmp/opencode/baum/paper.tex`. It does not by itself give spinning Kerr initial data.
2. **Dennison, Baumgarte & Montero, “Trumpet Slices in Kerr Spacetimes,”** [arXiv:1409.1887](https://arxiv.org/abs/1409.1887). It presents stationary analytical coordinate families and trumpet slices for general Kerr. It is directly relevant to a no-transient Kerr data/gauge pair, but its coordinates/data need to be implemented and cross-checked here.
3. **Brown et al., “Numerical simulations with a first order BSSN formulation,”** [arXiv:1202.1038](https://arxiv.org/abs/1202.1038), [Phys. Rev. D 85, 084004](https://doi.org/10.1103/PhysRevD.85.084004). It derives a fully first-order strongly hyperbolic BSSN system and characteristic fields. The production code here is second order in space; published fields cannot simply be declared the exact characteristic map of this RHS without matching the formulation and reduction.
4. **Yo, Baumgarte & Shapiro, “Improved numerical stability of stationary black hole evolution calculations,”** [arXiv:gr-qc/0209066](https://arxiv.org/abs/gr-qc/0209066), [Phys. Rev. D 66, 084026](https://doi.org/10.1103/PhysRevD.66.084026). It demonstrates stable excision evolutions of Kerr–Schild black holes for particular BSSN variants. This shows that the general problem is tractable; it does not certify the present RHS, gauge, stencils, or boundary condition.
5. **Spherical Schwarzschild equilibrium implementation:** `helper/equilibrium_1pluslog.F90` and `helper/test_equilibrium_1pluslog.F90`. Verified outputs include the two targets above, `C/M^2=1.246719531498`, both regularity conditions, and the first-integral identity as an output. The isotropic lapse crosses zero at `r=1.307M`; the current Kerr default `r_excise=1M` must not be interpreted as a valid excision radius for that isotropic Schwarzschild data.
6. **General spherical-metric and BSSN reference research:** external scratch resources in `/tmp/opencode/psi/` (`bssn1d.py`, `gen_gamma.py`, `validate_all.py`, `STEP1_STATUS.md`, `FINDINGS.md`) as documented in the supplied session state. These are scratch/reference analysis, not production files and not part of this repository’s acceptance gate. The earlier verified results must be retained as such; §5.1 of the handoff remains explicitly unresolved and must not be bypassed by normalizing a factor to fit PG.

## 11. Source inventory

This is a map of the Fortran files present in the project tree, grouped by responsibility. It is intended to make navigation efficient; “present” does not imply every experimental module is linked into the production run.

### Build and driver

- `Makefile` — source ordering, production binary, run target, parity check, and tests.
- `drivers/vacuum_kerr.F90` — production startup, evolution loop, diagnostics, horizon/output control.
- `parameter_files/vacuum_kerr.par` — default Kerr test configuration.

### State, parameters, grids, and initial data

- `helper/kinds_mod.F90` — floating-point kind.
- `init/param.F90` — defaults, file parser, environment overrides, validation, grid spacings.
- `init/grid.F90` — coordinate arrays and grid setup.
- `init/vars.F90` — 24-field production BSSN layout.
- `init/hydro_vars.F90` — hydrodynamic state layout, unused by vacuum driver.
- `init/fo_vars.F90` — earlier/compact first-order variable layout.
- `init/fo_complete_vars.F90` — expanded first-order experimental state layout.
- `init/fd_weights.F90` — finite-difference weights for first-order/experimental operators.
- `init/init_data.F90` — Kerr–Schild data and conformal-connection initialization.
- `init/stencils.F90` — derivative-stencil initialization/configuration.

### Cartoon, geometry, constraints, horizon, and excision

- `helper/cartoon.F90` — radial interpolation and x-y vector/tensor rotation.
- `helper/stencil.F90` — production finite-difference stencils with cartoon/excision neighbors.
- `helper/geometry.F90` — conformal inverse, Christoffels, Ricci, lapse Hessian.
- `helper/constraints.F90` — Hamiltonian and three-component momentum constraint diagnostics.
- `helper/excision.F90` — Kerr-radius mask, interior fill, background cache, lapse-gauge speed diagnostic.
- `helper/excision_sat.F90` — optional reduced production-state SAT; explicitly not full exact characteristics.
- `helper/fd_excision.F90` — excision helpers for experimental first-order operators.
- `helper/horizon.F90` — apparent-horizon finding and horizon/ADM quantities.

### Production evolution and boundary operators

- `integrators/bssn_rhs.F90` — production BSSN point/interior RHS plus gauge modes.
- `integrators/time_step.F90` — RK4 stages, projections, error detection, optional boundary/dissipation terms.
- `integrators/ko_diss.F90` — KO dissipation.
- `integrators/sommerfeld.F90` — outer boundary treatment.

### First-order/SBP/characteristic research track

- `helper/fo_sbp.F90` — SBP operator infrastructure.
- `helper/fo_sat.F90` — SAT support for the SBP track.
- `helper/fo_curved_sat.F90` — curved-boundary SAT utilities.
- `helper/fo_normal_system.F90` — reduced/partial normal characteristic system; comments explicitly leave production couplings unsupported.
- `helper/fo_constraint_damping.F90` — experimental first-order constraint damping.
- `helper/fo_constraints.F90` — first-order constraint evaluation.
- `integrators/fo_rhs.F90` — compact first-order RHS track.
- `integrators/fo_complete_rhs.F90` — expanded first-order RHS/bridge to production point RHS.

### Equilibrium, diagnostics, and test programs

- `helper/equilibrium_1pluslog.F90` — spherical stationary 1+log Schwarzschild reference solution.
- `helper/eq_schwarzschild_data.F90` — Schwarzschild stationary-slice BSSN data fixture.
- `helper/diagnose_stationary_ks_rhs.F90` — stationary Kerr–Schild RHS diagnostic program.
- `helper/probe_momentum_terms.F90` — momentum-term investigation utility.
- `helper/output.F90` — output module present in tree; check linked target before modifying.
- `helper/output_test.F90` — output implementation currently included in production source list.
- `helper/primitive_recovery.F90` — matter/primitive recovery utility, not used by vacuum driver.
- `helper/test_cartoon.F90` — cartoon reconstruction tests.
- `helper/test_constraint_conv.F90` — Hamiltonian constraint convergence test.
- `helper/test_momentum_conv.F90` — momentum convergence test; axis plateau remains open.
- `helper/test_eq_schwarzschild_data.F90` — Schwarzschild fixture tests.
- `helper/test_equilibrium_1pluslog.F90` — spherical trumpet anchor tests.
- `helper/test_excision_sat.F90` — reduced SAT behavior and non-exact label assertion.
- `helper/test_fo_complete_layout.F90`, `helper/test_fo_complete_state.F90`, `helper/test_fo_complete_rhs.F90` — expanded first-order layout/state/RHS checks.
- `helper/test_fo_layout.F90`, `helper/test_fo_rhs.F90` — compact first-order layout/RHS checks.
- `helper/test_fo_normal_system.F90` — reduced characteristic-system checks.
- `helper/test_fo_curved_sat.F90`, `helper/test_fo_sbp_sat.F90` — experimental SAT/SBP checks.
- `helper/test_fo_convergence.F90` — SBP convergence test.
- `helper/test_fo_validation.F90` — first-order validation cases.
- `helper/test_gamma_driver_rhs.F90` — manufactured gamma-driver RHS checks.
- `helper/test_horizon.F90`, `helper/test_horizon_drift.F90` — exact Kerr horizon and drift/finder tests.

## 12. Recommended reading order for a new maintainer

1. `parameter_files/vacuum_kerr.par`, `init/param.F90`, `init/grid.F90`.
2. `init/vars.F90`, `init/init_data.F90`, then `helper/cartoon.F90`.
3. `helper/excision.F90`, `helper/stencil.F90`, `helper/geometry.F90`.
4. `integrators/bssn_rhs.F90`, `integrators/time_step.F90`, `integrators/ko_diss.F90`, `integrators/sommerfeld.F90`.
5. `helper/constraints.F90`, `helper/horizon.F90`, output module actually linked in `Makefile`, then the driver.
6. Tests alongside each module; then read `helper/fo_normal_system.F90` and FO/SBP code as experimental boundary research rather than production assumptions.
7. Read this report’s failure/evidence sections before changing the gauge or excision parameters.

## 13. Implementation-level fix and Plan B

This section turns the repair programme into concrete work packages, including the information each package must produce and the decision to make if it fails. The aim is to end with a Kerr evolution that remains regular and convergent for a stated physical duration, not simply one that runs longer than the previous attempt.

### 13.1 First decide which coordinate representation is being solved

There are two different routes to long-term stability. Keep them separate in code and in the evidence:

**Route A: equilibrium trumpet initialization.** Construct Kerr initial data directly on a slice stationary under the intended lapse and shift conditions. This minimizes initial gauge transient and best matches the stated goal. It requires a genuinely rotating solution, not copying the spherical Schwarzschild lapse onto Kerr.

**Route B: evolve from Kerr–Schild and let the gauge settle.** Keep the current Kerr–Schild physical data and evolve with a standard moving-puncture gauge while the coordinates transition toward a trumpet. This can be a valid strategy, but only if the excision region tracks/remains safely inside the evolving horizon and the boundary treatment is stable throughout the transition. It is a fallback engineering route, not proof of equilibrium initial data.

Do not mix these routes in one result. For each run record the initial slice, lapse/shift equations, initial driver variables, excision surface, and whether the horizon is expected to move in coordinates.

### 13.2 Work package A — get a trusted stationary Kerr target

1. **Choose and document the exact target family.** Start with the stationary Kerr trumpet family of Dennison–Baumgarte–Montero or another explicitly derived stationary Kerr slicing. State the time coordinate, radial/angular coordinates, spin convention, lapse and shift, spatial metric, and extrinsic-curvature sign convention. Confirm the family’s slicing condition: a stationary trumpet in some time coordinate is not automatically the same slice as the code’s `1+log` gauge.
2. **Create a non-Fortran reference implementation.** Evaluate the four-metric and induced 3-metric at generic off-axis points. Extract lapse, shift, spatial metric, and extrinsic curvature by an independent 4-D decomposition. Separately evaluate the 3-D constraint equations. The two routes must agree at generic points and in the asymptotic, horizon, and trumpet limits. Avoid nested numerical differentiation and do not use a target lapse or radius as a fitted parameter.
3. **Verify the required physical invariants.** Check all components of the Hamiltonian and momentum constraints, not only one projected component. Compute horizon area, angular momentum, and mass from the data and compare with independent Kerr values. Confirm the spin-zero limit against the already verified Schwarzschild equilibrium module. For nonzero spin, include generic off-axis points; axis-only tests cannot validate angular terms.
4. **Transform the target into the production Cartesian cartoon representation.** Build all Cartesian tensor/vector components using a checked coordinate Jacobian. Verify axis regularity/parity and the y-component transformations. Test interpolation-independent pointwise values before using the grid interpolation.
5. **Populate all 24 fields consistently.** Derive `phi`, `gtilde`, `K`, `Atilde`, `Gammatilde`, `alpha`, `beta`, and `B` from the same slice. In particular, compute `Gammatilde` from the conformal metric and choose driver `B` from the *stationary driver equation*; do not set it to zero by habit if that violates the chosen shift equation. Verify determinant-one and trace-free conditions after conversion.
6. **Prove stationarity with the actual production RHS.** Evaluate every RHS component on the exact sampled data, separately reporting bulk, axis, horizon, trumpet/excision-ring, and outer-boundary regions. Initially disable KO, Sommerfeld, SAT, and projection so this measures the continuum RHS plus spatial discretization. Then turn each operator on individually and report its contribution. The maximum and RMS residuals must converge at the expected order as resolution increases. A small residual in only `alpha` or `beta` is not enough.
7. **Retain output targets only as outputs.** The supplied Schwarzschild anchors `alpha_AH=0.376179486830` and `R_c/M=1.540569415042` are regression outputs for their established Schwarzschild construction. Kerr quantities must emerge from the Kerr solve and be compared to independent geometry checks; they must not be imposed to make a shooting residual pass.

**Gate A:** The reference data satisfy all constraints and geometric invariants, and every production RHS component converges toward zero for the intended stationary gauge. If this fails, do not run a long evolution with the fixture. Identify whether the disagreement is in 4-D decomposition, coordinate transform, BSSN conversion, gauge equation, or discrete derivative by comparing intermediate tensors at the same point.

### 13.3 Work package B — make the inner boundary mathematically compatible

The present code combines second-order-in-space RHS evaluation with extrapolated values inside a curved excision surface. The boundary repair must be designed for this exact semidiscrete system.

1. **Write down the production principal part.** Enumerate evolved fields and derivative terms in `bssn_rhs.F90`, including lapse and shift-driver equations. Choose the boundary normal and orientation explicitly. At each boundary point use the normal to the actual Kerr-radius surface (or the actual trumpet excision surface), not a spherical approximation.
2. **Choose a consistent first-order reduction if characteristics are needed.** Define auxiliary derivative variables and their constraints, and make sure the reduction’s RHS is algebraically equivalent to the production second-order equations. A characteristic analysis from a different first-order BSSN formulation cannot be transplanted without this matching.
3. **Derive the normal matrix and its eigenstructure.** Include physical, gauge, and constraint-propagation sectors. State the convention for speed sign relative to the computational domain: which sign enters the retained exterior? Test the speeds in flat space with nonzero shift, in a curved diagonal metric, and at a generic spinning Kerr boundary point. Compare eigenvalues to an independent symbolic/numerical matrix calculation.
4. **Build manufactured characteristic tests with nonzero amplitudes.** For each incoming field, inject a known nonzero wave/amplitude and verify that the boundary treatment damps or prescribes it as designed. Also inject outgoing fields and verify they are not spuriously penalized. A zero-field pass is not evidence. Check the full characteristic vector and reconstruct all primitive fields.
5. **Implement a complete boundary condition.** Apply the penalty to every incoming field with coefficients consistent with the discrete energy estimate and the actual SBP/finite-difference norm. If the current grid/stencil has no SBP norm compatible with the penalty, either build a compatible SBP operator near the excision boundary or choose a boundary/excision strategy whose stability can be demonstrated. Document the difference between a physical excision condition (all relevant characteristics leave the exterior domain) and a numerical penalty used when incoming modes exist.
6. **Keep extrapolation only for the roles it can justify.** Interior extension may supply values required by stencils, but it cannot be described as a boundary condition for modes it does not control. Test fill accuracy, temporal refresh, and stencil footprint independently.
7. **Test moving geometry if Route B is used.** If the excision mask remains fixed while the apparent horizon moves, measure the minimum proper/coordinate separation and ensure the excision surface never crosses outside the horizon. Otherwise, implement a moving mask or a gauge/data design that bounds this separation; mask motion must preserve stencils and not create discontinuous state remapping.

**Gate B:** The complete production boundary map passes independent characteristic, manufactured-field, and energy/sign checks. The current reduced SAT remains off until this gate passes. A test of the reduced 3x3 blocks is not this gate.

### 13.4 Work package C — diagnose and stabilize the discrete bulk evolution

1. **Make a stationary semidiscrete test.** Use the trusted equilibrium data from Gate A on each grid. Report `max|RHS(field)|` and RMS by field and region. Turn off dissipation/boundaries first, then add each one. This distinguishes a wrong equilibrium equation from a boundary or operator imbalance.
2. **Check the metric algebra at every stage.** Record minimum eigenvalue and determinant of the physical and conformal metrics, trace of `Atilde`, `det(gtilde)-1`, lapse range, and first location/time at which each becomes invalid. A determinant-only printout is too late to localize growth.
3. **Instrument the first divergence, not just the final abort.** At a ring around excision, save per-variable values and RHS for all RK stages. Record first derivative/stencil selection, mask status, characteristic amplitudes, KO contribution, Sommerfeld contribution, and projection correction. Compare the bad point with neighbors and with the same continuum location at a finer grid.
4. **Establish spatial convergence before tuning dissipation.** Run at three or more resolutions with identical physical domain, excision radius in M, continuum data, and physical stop times. Compute all three momentum components separately and include axis/boundary subsets. The previously recorded momentum plateau must be investigated rather than averaged away.
5. **Check the time integrator separately.** Once spatial refinement is stable, halve `dt` at fixed grid and compare at equal physical times. If only the timestep refinement changes failure materially, identify a temporal stability limit. Smaller CFL is a stability diagnostic, not a replacement for spatial convergence.
6. **Vary one outer-boundary parameter at a time.** Compare two or more outer radii at fixed near-hole resolution. Then vary KO strength around the documented default. Track waves arriving back at the hole and boundary reflections. Do not change gauge, resolution, excision radius, and KO together.

**Gate C:** constraint and geometric diagnostics converge; the stationary data remain stationary to discretization error; the first-growing mode is attributable to a tested source or the run remains bounded. If a variable grows but no source is isolated, the cause remains unresolved.

### 13.5 Work package D — perform the actual long-time acceptance experiment

Use a table of experiment identifiers and immutable configuration files. Every row should specify spin, mass, gauge modes, initial-data family, grid, resolution, `r_excise`, stencil/excision order, boundary method, dissipation, `dt`, number of steps, and final physical time.

At each diagnostic time record:

- apparent-horizon found flag, area, irreducible mass, angular momentum, horizon mass, shape extrema, and coordinate location;
- exterior Hamiltonian max/RMS and each of `Mx`, `My`, `Mz` max/RMS, with the region definition fixed;
- minimum `det(gtilde)` and minimum metric eigenvalue, including index and coordinates;
- minimum lapse, maxima of `|K|`, `|Atilde|`, `|beta|`, and `|B|` in a fixed near-hole shell;
- each field’s RHS norm and the inner-boundary incoming characteristic amplitudes;
- whether any algebraic projection is making a large correction.

Use at least three resolutions and compare all runs at common physical times (for example `t/M`, not step number). Establish a refinement trend for horizon invariants and constraints. Run first to a moderate time, then extend the same validated configuration; do not change final time or suppress horizon checks to call a run successful. The eventual required duration should be explicitly agreed as a physical `T/M` acceptance target and kept fixed across resolutions.

**Gate D:** no determinant/eigenvalue loss; horizon is found throughout; constraints and horizon invariants are bounded and converge with resolution at equal time; the stationary case does not exhibit secular coordinate or geometric drift; and the result is reproducible from a clean build/configuration.

### 13.6 Plan B if equilibrium Kerr data do not produce a stable run

Plan B is a diagnostic branch, not permission to relax the acceptance criteria. Start it only after recording precisely which gate failed.

#### Plan B1 — isolate a production-RHS or gauge-equation defect

- If Gate A fails, compare the reference 4-D ADM quantities, transformed Cartesian fields, BSSN fields, and discrete RHS in that order. Use the same point and coordinate convention for every comparison. Do not change normalization until an independent route explains the discrepancy.
- Reduce spin continuously: test `a=0` against the established Schwarzschild fixture, then small nonzero spin, then the target spin. A discontinuity as `a -> 0` points to spin-dependent geometry, Cartesian transformation, or axis regularity. Smooth convergence does not by itself validate the target-spin case.
- Manufacture smooth axisymmetric fields with known derivatives and pass every scalar/vector/tensor component through `get_neighbor`, `compute_stencil`, geometry, and RHS. Require nonzero expected derivatives and all-component error reports.
- Compare the production RHS with an independent symbolic or high-precision reference on generic off-axis points. If the continuum RHS is nonzero for claimed stationary data, fix the equations/conventions before changing boundary treatment.

#### Plan B2 — replace or complete the production formulation

If the current second-order formulation does not admit a tractable, stable boundary treatment on the existing stencil, stop trying to label its reduced blocks “exact.” Port the production evolution to a single fully specified strongly hyperbolic first-order BSSN formulation with its matching variables, constraints, characteristic fields, and boundary conditions—or adopt a separately validated BSSN implementation as a reference. Do not evolve a mixture in which one module assumes one principal symbol and another evolves a different one.

For this route, stage the port module-by-module: state conversion; flat-space principal-symbol tests; curved-background RHS equivalence; constraint propagation; cartoon operator compatibility; outer-boundary SAT; inner-boundary SAT; then Kerr equilibrium evolution. The complete first-order path must be built into the production executable and covered by `make check-parity`. A passing standalone FO unit test is insufficient if `make run` still executes the old RHS.

#### Plan B3 — retain Kerr–Schild initialization but treat the gauge transition as a moving-boundary problem

If stationary Kerr trumpet data are too large a first milestone, return to valid Kerr–Schild data and implement a standard moving-puncture gauge transition as a separate route. First establish a controlled one-hole gauge evolution in a no-excision or safely excised test domain where the geometry remains resolved. Then monitor the apparent horizon and adjust the excision geometry consistently if it moves. This may require a moving excision surface, horizon-tracking coordinate map, or a sufficiently conservative fixed excision surface; each has to be tested for stencil integrity and characteristic flow.

This route is acceptable only if the boundary stays inside the horizon and the full boundary map remains stable during the transition. It does not meet a claim of “stationary from the initial slice” unless the data are actually stationary.

#### Plan B4 — replace the finite-difference boundary infrastructure

If tests show the current nonuniform centered/one-sided stencil and curved mask cannot support a stable estimate, move the near-boundary evolution to a compatible SBP-SAT discretization, or use a domain-decomposition/spectral method with explicit characteristic boundary conditions. Preserve the same continuum equations and physical data for comparisons. Revalidate cartoon axis regularity and tensor transformations; a new boundary discretization cannot be accepted with a scalar-only convergence result.

#### Plan B decision tree

| Failed gate / symptom | Next action | Do not do |
|---|---|---|
| Independent 4-D and 3-D data extraction disagree | Resolve metric decomposition, sign, or coordinate transform at a point | Tune a factor to match a published K or lapse |
| Constraints pass but stationary RHS does not | Audit BSSN conversion, gauge driver equilibrium, and RHS conventions field by field | Begin long production runs |
| Bulk RHS converges but near-boundary RHS does not | Isolate stencil/mask/normal/SAT; test compatible SBP or revised excision geometry | Increase KO until the mode disappears |
| Boundary map is incomplete or fails manufactured incoming fields | Complete matching first-order system or change boundary discretization | Call the current reduced SAT exact |
| Frozen-gauge Kerr–Schild still grows away from both boundaries | Investigate bulk RHS, projection, time integration, and formulation stability | Attribute all growth to gauge drift |
| Failure time improves only when `dt` shrinks, but spatial refinement does not converge | Analyze semidiscrete stability and eigenvalue/CFL limits | Claim a physics/gauge fix from smaller steps |
| Horizon invariants drift while constraints converge | Check gauge/data stationarity and horizon finder independently | Equate coordinate-radius motion with physical mass loss |
| Horizon finder fails but local metric remains regular | Validate finder resolution/initial guesses and compare an independent surface method | Declare physical collapse from finder failure alone |

Plan B ends with the same Gate D acceptance. If all implementation routes fail a gate, the correct deliverable is a localized, reproducible failure and the smallest failing test—not a “stable” run obtained by rolling simulation time backward, reducing the requested duration, hiding diagnostics, or fitting the target values.

### 13.7 Immediate next action from the new evidence

Do not proceed to a long trumpet evolution yet. This independent calculation has now been carried out in `/tmp/independent_ks_momentum.py`. It constructs the full 3-D Cartesian Kerr–Schild metric and shift, uses symbolic derivatives to form the Christoffels and `K_ij`, and then symbolically differentiates the physical ADM momentum constraint. At the near-axis point `(x,z)=(0.0628272251309,-1.535248)M` and two controls, all three `M^i` components are about `1e-41` in 40-digit arithmetic. This is an independent nonzero-expected comparison: the continuum vacuum momentum constraint vanishes, while the production finite-difference result at the nearby grid point has `M_z` around `1e-1`.

The next code/research task is therefore to compare the **sampled BSSN state** and **production stencil contraction** at exactly the symbolic point, and identify whether the discrepancy first appears in initial `K_ij`, the BSSN field conversion, cartoon derivatives, or the constraint assembly. Use a control point at the same Kerr radius away from the axis. Report all three components and each covariant-derivative contribution separately. The analytic script differentiates the base metric/shift symbolically before evaluation, so it does not differentiate an already finite-differenced `K_ij`.

If the independent ADM residual is small while the production `M_z` remains large, isolate the cartoon/stencil and BSSN constraint implementation. If both routes show the same residual, audit the Kerr–Schild initial `K_ij` construction and its sign/index conventions. If the residual decreases away from the axis but plateaus only at `x=dx`, test analytic parity fields and the axis limit directly. Only after this discrepancy is explained should the equilibrium Kerr RHS and boundary work proceed; otherwise a “stationary” data solver could inherit a faulty validation operator.

The production diagnostic now records the momentum constraint as seven additive
pieces: the x, y, and z derivatives of raised `Atilde`, the two connection
contractions, the `6 Atilde^{ij} d_j phi` term, and the `-(2/3) gtilde^{ij}
d_j K` term. At `Nx=192, Nz=384`, the near-band maximum is at
`(x,z)=(0.062827,-1.535248)M`, where `M_z=-1.2465e-1`. The three derivative
pieces are respectively `+1.4950e-1`, `+1.5062e-1`, and `-7.8881e-2`;
the remaining pieces are `-1.3177e-1`, `-1.9287e-7`, `-2.2404e-2`, and
`-1.9171e-1`. Their sum reproduces the reported residual. These figures narrow
the defect to the raised-`Atilde` divergence, but do not identify which
coordinate derivative is wrong: the y derivative is the cartoon reconstruction,
and the x/z pieces also include the near-axis stencil and metric raising. The
same-radius off-axis control has `M_z=-1.4589e-2`, so the large residual is
strongly localized to the axis-adjacent polar-horizon region. The full
independent Cartesian comparison of these seven pieces at this exact point is
still pending; no production stencil or evolution equation should be changed
until that comparison distinguishes data conversion from derivative assembly.

## 14. Bottom-line status

The code builds and the current test suite passes. The recorded short baseline is reproducible. The system still does not have a demonstrated long-term stable Kerr evolution. The preferred repair is a verified stationary Kerr data/gauge pair plus a boundary treatment derived for the exact production system. If that route stalls, Plan B isolates bulk equations, formulation, gauge transition, and boundary discretization one at a time. Both paths require equal-physical-time convergence and the same full acceptance gates. Until those gates pass, any apparent gain in lifetime is an experiment, not a total fix.
