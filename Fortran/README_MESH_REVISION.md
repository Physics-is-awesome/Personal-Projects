# 3D Spherical Tetrahedral Mesh - Complete Revision Summary

## Files Created

This package includes a corrected mesh design for your 3D discontinuous Galerkin (DG) code on a spherical domain. The following modules have been created:

### Core Modules

1. **[mesh.F90](mesh.F90)** – Minimal topology
   - Defines `mesh_t` type with node coordinates and element connectivity
   - Provides `create_mesh_from_gmsh()` to read Gmsh .msh files
   - ~100 lines of essential code

2. **[geometry.F90](geometry.F90)** – Derived geometric quantities
   - Defines `geometry_t` type with volumes, Jacobians, inverse Jacobians
   - Provides `compute_geometry()` to calculate from mesh topology
   - Handles determinant and matrix inversion for 3×3 Jacobians
   - ~200 lines

3. **[reference_element.F90](reference_element.F90)** – Reference tetrahedron & quadrature
   - Defines reference tet vertices and barycentric coordinates
   - Provides `quadrature_ref_tet()` for 1st–4th order Gaussian quadrature
   - Utility functions for point-in-element tests
   - ~250 lines

4. **[test_mesh_geometry.F90](test_mesh_geometry.F90)** – Example usage
   - Creates a simple unit tetrahedron mesh
   - Demonstrates geometry computation and verification
   - Tests quadrature generation
   - Shows expected .msh file format
   - ~150 lines

### Documentation

5. **[MESH_ARCHITECTURE.md](MESH_ARCHITECTURE.md)** – Complete design guide
   - Detailed explanation of each module's responsibilities
   - Mathematical formulas for volumes, Jacobians, affine maps
   - DG data flow diagram
   - Usage examples and checklists

6. **[sphere.geo](sphere.geo)** – Gmsh script
   - Generates tetrahedral mesh of a 3D ball
   - Configurable radius and mesh size
   - Ready to use: `gmsh sphere.geo -format msh2 -o sphere.msh`

## The Main Problem You Had

**Issue:** You were trying to use an icosahedron as the mesh.

**Why this was wrong:**
- Icosahedron = 20 triangular faces on a sphere surface (S²)
- Does NOT fill the 3D volume (missing interior)
- DG requires volume integrals over tetrahedra, not just surface triangulations
- Cannot compute element volumes or Jacobians for a 2D surface

**Solution:** Use a tetrahedral volume mesh.

## The Corrected Design

### Separation of Concerns

```
┌─────────────────────────────────────┐
│  mesh_mod                           │
│  ├─ node coordinates                │
│  └─ element connectivity (4 nodes)  │
└─────────┬───────────────────────────┘
          ↓
┌─────────────────────────────────────┐
│  geometry_mod                       │
│  ├─ element volumes                 │
│  ├─ Jacobian matrices               │
│  ├─ determinants                    │
│  └─ inverse Jacobians               │
└─────────┬───────────────────────────┘
          ↓
┌─────────────────────────────────────┐
│  reference_element_mod              │
│  ├─ reference tet definition        │
│  ├─ barycentric coordinates         │
│  └─ quadrature points & weights     │
└─────────┬───────────────────────────┘
          ↓
┌─────────────────────────────────────┐
│  basis_mod (future)                 │
│  ├─ polynomial basis functions      │
│  ├─ basis gradients                 │
│  └─ orthonormalization              │
└─────────┬───────────────────────────┘
          ↓ (and so on...)
```

### Why This is Better

**Before (monolithic mesh):**
```fortran
type mesh_t
  ! geometry
  real(dp), allocatable :: vol(:)
  real(dp), allocatable :: J(:,:,:)
  real(dp), allocatable :: det_J(:)
  real(dp), allocatable :: inv_J(:,:,:)
  real(dp), allocatable :: element_radius(:)  ← WRONG!
  
  ! DG data
  real(dp), allocatable :: basis(:,:,:,:)
  real(dp), allocatable :: qp(:,:,:)
  real(dp), allocatable :: qw(:,:)
  
  ! state
  real(dp), allocatable :: u(:,:,:)
  real(dp), allocatable :: rho(:,:)
  
  ! ... 50+ fields mixed together
end type
```

**After (modular design):**
```fortran
! mesh.F90
type mesh_t
  integer :: nnode, nelem
  real(dp), allocatable :: x(:,:)      ! topology
  integer, allocatable :: conn(:,:)    ! only
end type

! geometry.F90
type geometry_t
  real(dp), allocatable :: vol(:)      ! derived from
  real(dp), allocatable :: J(:,:,:)    ! topology
  ! ...
end type

! reference_element.F90 → basis.F90 → state.F90
! Each module handles its own data
```

## Key Concepts Explained

### 1. Why Tetrahedra?

For a DG method on 3D, you need:
- **Volume integrals:** ∫_K f(u,∇u) dx
- **Surface integrals:** ∫_∂K F(u) · n ds (faces of elements)
- **Local basis support:** Polynomial basis on K must be easy to evaluate

Tetrahedra satisfy all of these:
- Partition 3D space without gaps
- Have well-defined volume and faces
- Admit affine coordinate transformations
- Support Gaussian quadrature

Icosahedra only give the boundary (S²), not the volume (B³).

### 2. The Affine Mapping

For a tetrahedron with vertices x₁, x₂, x₃, x₄:

**Reference tetrahedron** (ξ-coordinates):
```
ξ₁ = (0,0,0)  ← vertex 1
ξ₂ = (1,0,0)  ← vertex 2
ξ₃ = (0,1,0)  ← vertex 3
ξ₄ = (0,0,1)  ← vertex 4
```

**Affine mapping** (reference → physical):
```
x(ξ) = x₁ + J · ξ
```

where J is the Jacobian matrix:
```
J = [x₂-x₁  |  x₃-x₁  |  x₄-x₁]
    (3×3 matrix with edge vectors as columns)
```

**Volume calculation:**
```
V = |det(J)| / 6
(The factor of 6 comes from the reference tet volume = 1/6)
```

**Gradient transformation** (needed for flux terms):
```
∇_x u = J⁻ᵀ ∇_ξ u
```

### 3. Element Radius in 3D

**Problem with my earlier design:**
```fortran
real(dp), allocatable :: element_radius(:)  ! ← WRONG for 3D
```

**Why it's wrong:**
- A tetrahedron is a 3D object; it doesn't have a single "radius"
- Points inside the tet are at different distances from the origin
- r = √(x² + y² + z²) varies across the element

**Correct approach:**
- Evaluate r at quadrature points as needed: `r_q = sqrt(qp_x^2 + qp_y^2 + qp_z^2)`
- Store r in state variables, not mesh
- No per-element scalar "radius"

## Test Output

When you run the test program:

```
./test_mesh_geometry
```

You should see:

```
Mesh created with:
  Nodes:            4
  Elements:            1

Volume of element   1 =     0.166667
det(J) of element   1 =     1.000000
Total mesh volume:   0.16666666666666666
✓ Volume check PASSED

Quadrature order  1 has   1 points:
  qp(  1) =   0.25000000  0.25000000  0.25000000
Quadrature order  2 has   4 points:
  ...
Quadrature order  3 has   5 points:
  ...
Quadrature order  4 has  14 points:
  ...
```

This confirms:
- Mesh topology is correctly read
- Geometry is correctly computed (volume = 1/6 for unit tet ✓)
- Jacobian determinant is 1 (correct for unit tet ✓)
- Quadrature points are generated

## How to Generate Your Mesh

### Step 1: Create Gmsh geometry

Use the provided [sphere.geo](sphere.geo), or create your own:

```gmsh
// sphere.geo
R = 1.0;
lc = 0.2;  // mesh size

// ... define points on sphere ...
// ... define surface patches ...
// ... define volume ...

Surface Loop(1) = {...};
Volume(1) = {1};
Mesh.Algorithm3D = 4;
```

### Step 2: Mesh with Gmsh

```bash
gmsh sphere.geo -format msh2 -o sphere.msh -3
```

The `-3` option triggers 3D (volume) meshing. Output: `sphere.msh`

### Step 3: Read into your code

```fortran
program main
  use mesh_mod, only: mesh_t, create_mesh_from_gmsh
  use geometry_mod, only: geometry_t, compute_geometry
  
  type(mesh_t) :: mesh
  type(geometry_t) :: geom
  
  call create_mesh_from_gmsh(mesh, 'sphere.msh')
  call compute_geometry(mesh, geom)
  
  print *, 'Mesh has', mesh%nnode, 'nodes and', mesh%nelem, 'elements'
  print *, 'Total volume:', sum(geom%vol)
  print *, 'Expected (4πR³/3):', 4.0d0 * 3.14159265358979d0 / 3.0d0
  
end program main
```

## For Your DG Code: Next Steps

The data flow for a complete solver should be:

```
1. mesh_mod
   └─ Read topology from file

2. geometry_mod
   └─ Compute volumes, Jacobians

3. reference_element_mod
   └─ Define quadrature on reference tet

4. basis_mod (to implement)
   ├─ Define polynomial basis on reference tet
   ├─ Evaluate basis at quadrature points
   └─ Compute basis gradients

5. quadrature_mod (to implement)
   └─ Map reference quadrature to physical elements

6. state_mod (to implement)
   ├─ Store solution coefficients per element
   ├─ Compute primitive variables
   └─ Evaluate solution at quadrature points

7. gravity_mod (to implement)
   ├─ Compute gravitational potential
   └─ Solve LDG Poisson problem

8. flux_mod
   ├─ Euler flux F(u)
   └─ Numerical flux (Lax-Friedrichs, HLLC, etc.)

9. rhs_mod
   ├─ Assemble volume integrals
   ├─ Assemble face integrals
   └─ Return residual dU/dt

10. time_integrator_mod
    ├─ SSPRK or IMEX stepping
    └─ Update solution in time
```

## Compilation

```bash
gfortran -c mesh.F90 geometry.F90 reference_element.F90
gfortran -c test_mesh_geometry.F90
gfortran -o test_mesh_geometry mesh.o geometry.o reference_element.o test_mesh_geometry.o
./test_mesh_geometry
```

## What NOT to Do

❌ Don't put in mesh_t:
- Element volumes (→ geometry_mod)
- Jacobians (→ geometry_mod)
- Element radius (→ evaluate at quadrature points)
- Quadrature points (→ reference_element_mod)
- Basis functions (→ basis_mod)
- Solution coefficients (→ state_mod)
- Face connectivity (→ add later in geometry_mod)

❌ Don't use:
- Icosahedra for volume meshes
- Scalar radius for tetrahedra
- Monolithic mesh objects with everything

✅ Do use:
- External mesh generators (Gmsh, TetGen, Netgen)
- Modular design (one concept per module)
- Affine coordinate maps for efficiency

## References

- Paper: "High order well-balanced and total-energy-conserving local discontinuous Galerkin methods for compressible self-gravitating Euler equations"
- Gmsh: https://gmsh.info/
- TetGen: https://www.wias-berlin.de/software/tetgen/
- DG theory: Hesthaven & Warburton, "Nodal Discontinuous Galerkin Methods"

---

**Status:** ✓ Mesh design revised
**Status:** ✓ Three core modules created and tested
**Status:** ✓ Example Gmsh script provided
**Next:** Implement basis.F90 and quadrature mapping for physical elements
