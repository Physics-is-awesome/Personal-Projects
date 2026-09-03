# DG_C: well-balanced discontinuous Galerkin solver for self-gravitating Euler flows

This directory contains a Fortran implementation of a high-order, well-balanced, total-energy-conserving local discontinuous Galerkin (LDG) solver for compressible self-gravitating Euler equations in 2D and 3D.

The code follows the formulation described in the paper:

- L. Pan, W. Chen, J. Qiu, T. Xiong
- "High order well-balanced and total-energy-conserving local discontinuous Galerkin methods for compressible self-gravitating Euler equations"
- J. Comput. Phys. 556 (2026)

## Project layout

```text
DG_C/
├── init/
│   ├── kinds.F90      # real kinds, physical constants, global parameters
│   ├── mesh.F90       # Cartesian mesh construction and topology
│   ├── quad.F90       # quadrature rules and point tables
│   ├── basis.F90      # DG basis functions and derivative tables
│   ├── param.F90      # case setup, namelist input, equilibrium families
│   └── paramter_file.cfg  # sample parameter/config file
├── helper/
│   ├── EOS.F90        # equation of state, primitive/conservative conversion
│   ├── pois.F90       # Poisson solve and LDG coupling
│   ├── rem.F90        # Riemann solvers and flux routines
│   ├── output.F90     # output and diagnostics utilities
│   ├── driver.F90     # main program entry point / driver logic
│   └── rem.F90        # Riemann flux routines
├── integrator/
│   ├── dg.F90         # DG spatial operator assembly
│   ├── oss.F90        # OE damping + positivity limiter
│   └── RK.F90         # SSP-RK time stepping
├── sgeuler3d          # built executable (after compiling)
├── mod_*.mod          # compiled module files
└── README.md          # this documentation
```

## Core purpose

The code evolves a conservative state vector

- U = (rho, rho u, E)

for the Euler equations with a gravitational potential and a self-consistent Poisson solve. The solver supports both:

- standard LDG discretization
- structure-preserving (well-balanced) formulation

It includes:

- equilibrium state construction
- DG reconstruction and flux assembly
- Poisson solve for gravity
- SSP-RK time stepping
- optional oscillation-eliminating (OE) damping
- optional positivity-preserving (PP) limiter
- mass and total-energy diagnostics

## Build instructions

From the project directory:

```bash
cd /home/ajcason/Coding/Personal-Projects/Fortran/DG_C
```

Build for 3D:

```bash
gfortran -O2 -cpp -DNDIM=3 \
  init/kinds.F90 init/mesh.F90 init/quad.F90 init/basis.F90 \
  helper/EOS.F90 helper/pois.F90 helper/rem.F90 \
  integrator/dg.F90 integrator/oss.F90 integrator/RK.F90 \
  helper/output.F90 helper/driver.F90 \
  -o sgeuler3d
```

Build for 2D:

```bash
gfortran -O2 -cpp -DNDIM=2 \
  init/kinds.F90 init/mesh.F90 init/quad.F90 init/basis.F90 \
  helper/EOS.F90 helper/pois.F90 helper/rem.F90 \
  integrator/dg.F90 integrator/oss.F90 integrator/RK.F90 \
  helper/output.F90 helper/driver.F90 \
  -o sgeuler2d
```

The `-cpp` flag is required because the code uses preprocessor directives such as `#ifndef NDIM` and `#define NDIM`.

## Running the solver

Run the executable with an input file name, or use the default file if none is supplied:

```bash
./sgeuler3d input.nml
```

If no argument is passed, the driver tries to read a default file named `input.nml`.

## Input and configuration

The code uses a Fortran namelist block in `mod_params` (`control`) to configure the run. Parameters include:

- `case_name`: which test problem to run
- `scheme`: `'sp'` or `'std'`
- `basis_type`: `'P'` or `'Q'`
- `kdeg`: polynomial order
- `nxyz`: grid size in each coordinate direction
- `rk_order`: SSP-RK order
- `use_oe`: enable oscillation-eliminating damping
- `use_pp`: enable positivity limiter
- `use_ibp`: use integration-by-parts form for the discrete operator
- `cfl`: CFL number
- `tfinal`: final time
- `pois_tol`, `pois_maxit`: Poisson solver tolerance and max iterations
- `G_override`, `mu_override`: optional overrides
- `nout`: output frequency
- `verbose`: print diagnostics

The equilibrium families and examples are defined in `mod_params` using the constants:

- `EQ_PLANE`
- `EQ_BESSEL`
- `EQ_LANE1`
- `EQ_CONST`

The supported cases are defined in the same module and map to the paper's benchmark examples.

## Detailed worked example: 2D well-balanced Bessel equilibrium (`wb2d`)

A representative nontrivial test is the 2D equilibrium case `case_name = 'wb2d'`. In this configuration the code sets:

- `scheme = 'sp'` (structure-preserving, well-balanced mode)
- `ieq = EQ_BESSEL`
- `gam = 2`, `pnu = 2`, `Ggrav = 1`, `lam = 1`
- `xlo(1:2) = (-1/2, -1/2)`, `xhi(1:2) = (1/2, 1/2)`
- `bc_fluid = BC_EXACT`, `bc_pois = BC_EXACT`

This corresponds to a radial polytropic equilibrium of the form

- ρ^e(x) = λ θ(r/a)^n
- p^e(x) = κ (ρ^e)^ν
- φ^e(x) = -(κ ν/(ν-1)) (ρ^e)^{ν-1}

with `n = 1`, `ν = 2`, and `θ(r)` approximated by the Bessel profile

- θ(r) = (1/π) ∫_0^π cos(r sin α) dα = J_0(r)

The code implements this in `steady_state` as:

```fortran
case (EQ_BESSEL)
    r   = radius(x)/aLE
    th  = theta_bessel(r)
    rho = lam*th**pol_n
    p   = kap*rho**pnu
    phi = -(kap*pnu/(pnu - one))*rho**(pnu - one)
end select
```

This is the hydrostatic balance used by the solver:

- u = 0
- ∇p = -ρ ∇φ
- Δφ = 4πGρ

In other words, the code is not trying to evolve arbitrary data; it is first constructing a discrete equilibrium state and then solving the perturbed problem around that equilibrium. That is exactly the setting where a well-balanced scheme is most valuable, because the discrete operator should preserve the equilibrium up to truncation and solver error, instead of producing spurious gravitational accelerations from a marginally nonzero residual.

A second useful example is the perturbed case `case_name = 'pert2d'`, which begins from the equilibrium state and adds a small pressure bump:

```fortran
case (C_PERT2D, C_PERT3D)
    r2 = sum((x(1:nd) - pert_x0(1:nd))**2)
    p  = p + pert_mu*exp(-100.0_dp*r2)
end select
```

This is a compact perturbation with amplitude `pert_mu` around the radius-centred equilibrium. It is a clean test for the ability of the scheme to evolve a small disturbance without destroying the hydrostatic balance.

## Physics and mathematics check

The governing equations in the code correspond to the self-gravitating Euler system

- ∂_t ρ + ∇·(ρ u) = 0
- ∂_t (ρ u) + ∇·(ρ u ⊗ u + p I) = -ρ ∇φ
- ∂_t E + ∇·((E + p) u) = -ρ u · ∇φ
- Δφ = 4πGρ

with the ideal-gas closure

- p = (γ - 1)(E - 1/2 ρ |u|^2)

This is consistent with the code's `prim2cons`, `cons2prim`, and `phys_flux` routines in `helper/EOS.F90`, and with the Poisson solve in `helper/pois.F90`.

The equilibrium families in the code are physically sensible for the intended tests:

- `EQ_PLANE`: a sinusoidal plane-wave equilibrium for the accuracy tests
- `EQ_BESSEL`: a radial polytropic equilibrium matching the Lane-Emden/Bessel family
- `EQ_LANE1`: the 3D Lane-Emden `sin(αr)/(αr)` profile
- `EQ_CONST`: the constant-density Jeans background used as a special case with `φ = 0`

The sign convention is also internally consistent: the Poisson equation is solved as `Δφ = 4πGρ`, while the momentum source is `-ρ∇φ` and the total energy source is the work term `-ρ u · ∇φ`. In the code comments, this is exactly the structure used in the well-balanced formulation.

One important caveat is that the code is a research implementation meant to mirror the paper's formulation, so the final check of mathematical correctness is not just algebraic consistency but numerical validation against benchmark solutions. Based on the code structure and formulas, the physics is coherent and in line with standard self-gravitating Euler theory. I would still recommend validating with a small reference run for at least one equilibrium case to confirm the discrete balance is preserved to the expected tolerance.

## Detailed worked example: 3D Lane-Emden equilibrium (`wb3d`)

A good 3D benchmark is the case `case_name = 'wb3d'`. This is the spherical, structure-preserving equilibrium test used in the paper's 3D example set. In the code it is configured as:

- `scheme = 'sp'`
- `ieq = EQ_LANE1`
- `nd = 3`
- `Kpol = 1`, `rho0 = 1`, `Ggrav = 1/pi`
- `gam = 2`, `pnu = 2`
- `alph = sqrt(4*pi*Ggrav/(2*Kpol))`
- `aLE = 1/alph`
- `xlo(1:3) = (-1/2, -1/2, -1/2)`, `xhi(1:3) = (1/2, 1/2, 1/2)`
- `bc_fluid = BC_EXACT`, `bc_pois = BC_EXACT`

This corresponds to a radial 3D equilibrium of the familiar Lane-Emden type:

- ρ^e(r) = ρ0 sinc(α r)
- p^e(r) = K (ρ^e)^2
- φ^e(r) = -2 K ρ^e(r)

where

- `sinc(z) = sin(z) / z`
- `α = sqrt(4πG / (2K))`
- `K = ρ0 = 1` for the default setup

The code implements this in `steady_state` as:

```fortran
case (EQ_LANE1)
    r   = radius(x)
    rho = rho0*sinc(alph*r)
    p   = Kpol*rho*rho
    phi = -two*Kpol*rho
end select
```

This is a spherical equilibrium with zero velocity and radial force balance. The intended balance is the 3D analogue of the 1D hydrostatic equilibrium:

- u = 0
- dp/dr = -ρ dφ/dr
- d/dr (r^2 dφ/dr) = 4πG r^2 ρ

Equivalently, the gravitational acceleration is central and radial:

- g^e(r) = -∇φ^e(r)
- g^e(r) = (1/r) [ ... ]

with the radial dependence encoded using the `radius(x)` helper and the `sinc` profile. This is exactly the type of equilibrium for which a well-balanced method should remain close to machine precision away from numerical discretisation error, because the analytic solution is stationary in time.

The 3D perturbed case `case_name = 'pert3d'` is then a small compact pressure bump on top of the equilibrium:

```fortran
case (C_PERT3D)
    r2 = sum((x(1:nd) - pert_x0(1:nd))**2)
    p  = p + pert_mu*exp(-100.0_dp*r2)
end select
```

This is a smooth localized perturbation around the spherical background, designed to test the code's ability to propagate a small wave without destroying the equilibrium. The Poisson solve is still posed with exact boundary data while the fluid boundary condition is set to transmissive (`BC_TRANS`), so the perturbation can propagate outward while the equilibrium remains anchored in the gravity solver.

The 3D explosion case `case_name = 'expl3d'` is another useful example:

```fortran
case (C_EXPL3D)
    r = radius(x)
    if (r < blast_r0) p = expl_alpha*p
end select
```

This multiplies the pressure in a compact central sphere by a factor `expl_alpha`, creating a strong local pressure pulse in a gravitationally balanced sphere. It is more demanding than the equilibrium test and is a realistic check of the shock-capturing and limiter behaviour in 3D.

## Physics and mathematics check

The 3D case is mathematically consistent with the self-gravitating Euler system:

- ∂_t ρ + ∇·(ρ u) = 0
- ∂_t (ρ u) + ∇·(ρ u ⊗ u + p I) = -ρ ∇φ
- ∂_t E + ∇·((E + p) u) = -ρ u · ∇φ
- Δφ = 4πGρ

and with the ideal-gas relation

- p = (γ - 1)[E - 1/2 ρ |u|^2]

The 3D `wb3d` case is especially important because it is a true spherical equilibrium, not just a Cartesian or plane-wave construction. In that sense it is a stronger test of the well-balanced formulation than the 2D Bessel case, because any discretisation error in the radial gravity, source term, or boundary treatment becomes visible immediately as a spurious acceleration or drift.

Based on the code structure, the 3D case is physically sound: the equilibrium is built from a radial `sinc` profile, the gravitational potential is derived from the same radial density profile, and the code uses the exact same Euler-Poisson structure in the residual assembly and Poisson solve. This is the right kind of test to use for verifying that the 3D well-balanced discretisation preserves hydrostatic balance without artificial forcing.

## Solver workflow

The standard execution pipeline is:

1. Read parameters and case setup.
2. Build the mesh and DG basis.
3. Initialise the Poisson solver and DG scratch arrays.
4. Construct the discrete equilibrium state.
5. Project the initial condition onto the DG basis.
6. Reconstruct the conservative variables and gravity field.
7. Advance in time using SSP-RK.
8. Optionally apply OE damping and positivity limiters after each stage.
9. Compute diagnostics and write output.

The main control loop is in `helper/driver.F90`.

## Module responsibilities

### `init/kinds.F90`

Defines:

- `dp` real kind
- numerical constants (`zero`, `half`, `one`, `pi`, etc.)
- the global `mod_params` module, which contains the physical and discretisation settings

### `init/mesh.F90`

Creates the structured tensor-product mesh, neighbour connectivity, cell geometry, and mapping data used throughout the solver.

### `init/quad.F90`

Defines quadrature rules for volume and face integration.

### `init/basis.F90`

Builds the DG modal basis, derivative tables, and interpolation helpers used for evaluating field values and their derivatives.

### `helper/EOS.F90`

Contains the equation-of-state routines and conversion utilities between conservative and primitive variables.

### `helper/pois.F90`

Solves the Poisson problem associated with the gravitational field and computes the discrete potential / gradient pair.

### `helper/rem.F90`

Implements the Riemann flux logic used to couple adjacent cells, including the well-balanced modifications used in the structure-preserving scheme.

### `integrator/dg.F90`

This is the main spatial-discretisation module. It assembles the residual for both the standard and structure-preserving schemes, computes equilibrium states, reconstructs gravity terms, and handles the discrete operators.

### `integrator/oss.F90`

Implements:

- OE damping for oscillation suppression
- PP limiting for positivity preservation

### `integrator/RK.F90`

Contains the SSP-RK time-stepping logic and the stage/post-stage processing pipeline.

### `helper/output.F90`

Handles writing solution snapshots, diagnostics, and energy/mass logs.

## Numerical notes

- The code is implemented in a module-based Fortran style and relies on `use` statements to share data between subroutines.
- The dimension is set at compile time via the preprocessor flag `-DNDIM=2` or `-DNDIM=3`.
- The solver uses a highly structured, array-based implementation, so the code depends strongly on consistent indexing and dimension conventions.
- The equilibrium-preserving formulation is the defining feature of the structure-preserving (`scheme = 'sp'`) option.

## Typical debugging workflow

If compilation fails, the most common causes are:

- forgetting `-cpp`
- forgetting `-DNDIM=2` or `-DNDIM=3`
- mismatched module order between files
- stale `.mod` files from an earlier build configuration

A safe rebuild is:

```bash
rm -f *.mod *.o sgeuler3d
gfortran -O2 -cpp -DNDIM=3 \
  init/kinds.F90 init/mesh.F90 init/quad.F90 init/basis.F90 \
  helper/EOS.F90 helper/pois.F90 helper/rem.F90 \
  integrator/dg.F90 integrator/oss.F90 integrator/RK.F90 \
  helper/output.F90 helper/driver.F90 \
  -o sgeuler3d
```

## Summary

The code is a compact but fairly complete research-grade DG implementation for self-gravitating compressible Euler flows. The most important modules are:

- `init/kinds.F90` and `init/param.F90`: configuration and physics
- `init/basis.F90`: basis and discrete operators
- `integrator/dg.F90`: residual assembly
- `helper/pois.F90`: gravity/Poisson coupling
- `integrator/RK.F90`: time stepping
- `integrator/oss.F90`: limiter and damping

This project is most useful as a reference or template for a structured DG implementation that couples finite volume/finite element fluxes, Poisson gravity, and well-balanced source terms.
