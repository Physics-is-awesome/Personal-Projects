# Mathematical Framework: 3D Tetrahedral DG on a Spherical Domain

## Problem Setup

**Domain:** Ω = {x ∈ ℝ³ : |x| ≤ R} (3D ball)

**PDE:** Conservation law with gravity
```
∂u/∂t + ∇·F(u) = S(u, ∇Φ)
```

where:
- u = [ρ, ρv₁, ρv₂, ρv₃, E] (conserved variables)
- F(u) = Euler flux
- S(u, ∇Φ) = gravitational source term
- Φ satisfies LDG Poisson problem (gravity)

**Constraint:** Well-balanced method with total energy conservation (per the paper)

## Mesh Setup

### Tetrahedral Triangulation

Mesh Ωₕ is a partition into tetrahedra:
```
Ω ≈ Ωₕ = ⋃ Kₑ
       e=1..nelem

where each Kₑ is a tetrahedron with 4 vertices.
```

### Mesh Topology

Stored in `mesh_t`:
```
nnode : number of nodes
nelem : number of elements
x(i, 1:3) : coordinates of node i
conn(e, 1:4) : node indices of tetrahedron e
```

This is **minimal**—only topology, nothing derived.

## Reference Tetrahedron

All DG basis functions and quadrature are defined on a single **reference element**:

```
Kref = {ξ ∈ ℝ³ : ξ₁ ≥ 0, ξ₂ ≥ 0, ξ₃ ≥ 0, ξ₁ + ξ₂ + ξ₃ ≤ 1}
```

**Vertices:**
```
V₁ = (0, 0, 0)
V₂ = (1, 0, 0)
V₃ = (0, 1, 0)
V₄ = (0, 0, 1)
```

**Volume:** vol(Kref) = 1/6

**Barycentric coordinates** λ = (λ₁, λ₂, λ₃, λ₄):
```
λ₁ = 1 - ξ₁ - ξ₂ - ξ₃
λ₂ = ξ₁
λ₃ = ξ₂
λ₄ = ξ₃

Point in Kref ⟺ λᵢ ≥ 0 for all i ∧ Σλᵢ = 1
```

## Affine Reference-to-Physical Mapping

For element Kₑ with vertices {x₁, x₂, x₃, x₄}:

### Affine Map

```
Φₑ : Kref → Kₑ
ξ ↦ x(ξ) = x₁ + J·ξ
```

where **Jacobian matrix**:
```
J = [x₂ - x₁ | x₃ - x₁ | x₄ - x₁]  (3×3, columns are edge vectors)
```

### Volume Element

Physical volume measure:
```
dx = |det(J)| dξ
```

**Element volume:**
```
|Kₑ| = ∫_Kref |det(J)| dξ = |det(J)| · (1/6)
```

Since for the reference tet, ∫_Kref dξ = 1/6.

### Inverse and Gradient Map

**Inverse Jacobian:** J⁻¹ (computed in `geometry_mod`)

**Gradient transformation** (chain rule):
```
∇ₓ u = (J⁻ᵀ) ∇_ξ u
```

where J⁻ᵀ = (J⁻¹)ᵀ.

This is needed for flux computation:
```
∂u/∂xᵢ = Σⱼ (J⁻¹)ᵢⱼ ∂u/∂ξⱼ
```

## DG Discretization

### Local DG Space

On element Kₑ, discretize:
```
uₑ(x) ≈ Σᵢ uᵢₑ φᵢ(ξ(x))

where φᵢ are basis functions on Kref, ξ(x) = J⁻¹(x - x₁)
```

### Polynomial Basis

Typically use:
- Legendre polynomials on Kref
- Tensorized or nodal basis
- Order N gives (N+1)(N+2)(N+3)/6 basis functions

**Example (N=0, piecewise constant):**
```
φ₀(ξ) = 1 (constant)
⟹ uₑ(x) = u₀ₑ = constant per element
```

This basis is defined in `basis_mod` (to be implemented).

### Quadrature

Reference element quadrature:
```
∫_Kref f(ξ) dξ ≈ Σq wq f(ξq)
```

Implemented in `reference_element_mod`.

**Standard Gaussian rules** for tetrahedral elements:
- Order 1: 1 point (centroid)
- Order 2: 4 points
- Order 3: 5 points
- Order 4: 14 points
- ...

### Physical Domain Quadrature

Map reference quadrature to physical element:
```
Physical point: xq = x₁ + J·ξq
Physical weight: wq_phys = wq_ref · |det(J)|
```

This gives:
```
∫_Kₑ f(x) dx = Σq f(xq) · wq_ref · |det(J)|
```

where wq_ref are the reference quadrature weights.

## Weak Formulation

### Volume Integral (Interior)

For test function v (also in DG space):
```
∫_Kₑ v · ∂u/∂t dx + ∫_Kₑ v · ∇·F(u) dx = ∫_Kₑ v · S(u,∇Φ) dx
```

Using integration by parts on the flux term:
```
∫_Kₑ v · ∇·F(u) dx = ∫_∂Kₑ v F(u)·n ds - ∫_Kₑ ∇v · F(u) dx
```

### Surface Integral (Faces)

Replace flux F(u) with numerical flux F*(u⁻, u⁺, n):
```
∫_∂Kₑ v F*(u⁻, u⁺, n) ds
```

where u⁻ = trace of u from inside Kₑ, u⁺ = trace from neighbor.

### Final Weak Form (Local)

For each element and test function φᵢ:
```
d/dt ∫_Kₑ φᵢ uₑ dx + ∫_∂Kₑ φᵢ F* ds - ∫_Kₑ ∇φᵢ·F(uₑ) dx = ∫_Kₑ φᵢ S(uₑ) dx
```

This is assembled by looping over quadrature points.

## Computational Steps in DG Assembly

### Step 1: Reference Basis & Quadrature

(Precomputed in `reference_element_mod` and `basis_mod`)
```
φᵢ(ξq) : basis function i at reference quadrature point q
∇_ξ φᵢ(ξq) : basis gradient (in ξ-coordinates)
wq, ξq : quadrature point q and weight
```

### Step 2: Physical Quadrature (Per-Element)

(Computed in `quadrature_mod` or RHS assembly)
```
For element e and quadrature point q:
  xq = x₁ + J·ξq
  ∇_x φᵢ = (J⁻ᵀ) ∇_ξ φᵢ
  wq_phys = wq · |det J|
```

### Step 3: Solution Evaluation

(From `state_mod`)
```
For element e and quadrature point q:
  ue(xq) = Σᵢ uᵢₑ φᵢ(ξq)
  ∇ue(xq) = Σᵢ uᵢₑ ∇_x φᵢ(ξq)
  (primitive variables, if needed)
```

### Step 4: Flux and Source

```
Fₑ(ue(xq)) : Euler flux at quadrature point q
Sₑ(ue(xq), ∇Φ(xq)) : gravitational source at q
r(xq) = √(xq²) : radius for gravity/density stratification
```

### Step 5: Residual Assembly

```
For each element e and basis function φᵢ:

rᵢₑ = d/dt ∫ φᵢ uₑ dx
      + (boundary flux term from surface integrals)
      - ∫_Kₑ ∇φᵢ · F(uₑ) dx
      - ∫_Kₑ φᵢ S(uₑ) dx

Numerically:
rᵢₑ = M_mass · duₑ/dt + F_flux + F_source
```

where M_mass is the local mass matrix.

## Why This Design Works

### Separation: Mesh ≠ Geometry ≠ DG

- **mesh.F90:** Stores what's given (topology)
- **geometry.F90:** Computes what's derived (volumes, Jacobians) ← fast, one-time cost
- **reference_element.F90:** Defines abstract reference basis/quadrature (no mesh)
- **basis.F90:** Polynomial basis functions (no mesh, no quadrature weights)
- **state.F90:** Solution coefficients (depends on basis, not mesh)
- **rhs.F90:** Assembles residual by gluing together geometry + basis + quadrature + state

### Reusability

- Reference quadrature rule is the same for all elements
- Basis functions are the same for all elements
- Only the mapping x(ξ) and Jacobian change per element
- Code efficiency: loop over elements, quadrature points once

### Modularity

Each piece is testable independently:
- Test mesh reading ✓
- Test geometry (volume checks, Jacobian invertibility) ✓
- Test reference quadrature (sum of weights = volume?) ✓
- Test basis evaluation ✓
- Test quadrature mapping ✓
- Then test full residual assembly

## Implementation Checklist

- [x] mesh.F90 (topology)
- [x] geometry.F90 (volumes, Jacobians, determinants, inverses)
- [x] reference_element.F90 (reference tet, quadrature)
- [ ] basis.F90 (polynomial basis functions, gradients)
- [ ] quadrature.F90 (map reference → physical quadrature)
- [ ] state.F90 (solution coefficients, primitive/conserved variables)
- [ ] gravity.F90 (gravitational potential, LDG Poisson)
- [ ] flux.F90 (Euler fluxes, numerical flux)
- [ ] rhs.F90 (volume + face integral assembly)
- [ ] time_integrator.F90 (SSPRK or IMEX stepping)

## Example: Piecewise Constant DG (P0)

To get started, try N=0 (one coefficient per element):

```fortran
! basis.F90: P0 basis
subroutine basis_p0(xi, phi, dphi_dxi)
  real(dp), intent(in) :: xi(3)
  real(dp), intent(out) :: phi(1)
  real(dp), intent(out) :: dphi_dxi(1, 3)
  
  phi(1) = 1.0_dp                    ! constant basis
  dphi_dxi(1, :) = 0.0_dp            ! derivative is zero
end subroutine basis_p0

! state.F90: P0 solution
type state_t
  real(dp), allocatable :: u(:)      ! u(e) = solution in element e (one value)
end type
```

This is simple to test and verify conservation.

---

**Key insight:** The design separates the "where" (mesh/geometry) from the "what" (basis/DG) from the "how much" (state/solution). This makes debugging, testing, and extending the solver much cleaner.
