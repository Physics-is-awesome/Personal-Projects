# 3D Spherical Tetrahedral Mesh Architecture

## Overview

This document explains the revised mesh design for a 3D discontinuous Galerkin (DG) code based on the paper:

> "High order well-balanced and total-energy-conserving local discontinuous Galerkin methods for compressible self-gravitating Euler equations."

The code solves on a 3D spherical domain Ω = {x ∈ ℝ³ : |x| ≤ R}, not on the surface.

## Key Design Principles

1. **Separation of Concerns**
   - `mesh.F90`: Topology only (nodes and connectivity)
   - `geometry.F90`: Derived geometric quantities (volumes, Jacobians)
   - `reference_element.F90`: DG reference element and quadrature
   - `basis.F90` (future): Polynomial basis functions
   - `quadrature.F90` (future): Quadrature rules for volume/face integrals
   - `state.F90` (future): Solution coefficients and variables

2. **Why icosahedra are insufficient**
   - An icosahedron is a surface polyhedron with 20 triangular faces
   - Its vertices lie on a sphere, providing only a surface triangulation of S²
   - It does NOT fill the 3D ball; it lacks interior tetrahedral cells
   - For volume integrals in DG, you need tetrahedra that partition the ball
   - Solution: Use an external mesh generator (Gmsh, TetGen, Netgen) to tetrahedralize the sphere interior

3. **Why tetrahedral volume mesh?**
   - DG method solves the conservation law in volume K: ∫_K ∂u/∂t · v dx + ∫_∂K F·n · v ds + ...
   - Volume integrals are over cells; surface integrals are over faces
   - Tetrahedra partition 3D space efficiently and support affine mappings
   - Reference tetrahedron (0,0,0)-(1,0,0)-(0,1,0)-(0,0,1) is standard for Gaussian quadrature

## Module Structure

### 1. mesh.F90 – Topology

```fortran
type mesh_t
  integer :: nnode            ! number of nodes
  integer :: nelem            ! number of tetrahedra
  real(dp), allocatable :: x(:,:)     ! x(i,1:3) = coordinates of node i
  integer, allocatable :: conn(:,:)   ! conn(e,1:4) = nodes of tet e
end type
```

**Responsibilities:**
- Store node coordinates
- Store tetrahedral connectivity
- Read mesh from external format (Gmsh, TetGen)

**Does NOT contain:**
- Element volumes ← compute in geometry module
- Jacobians ← compute in geometry module
- Quadrature points ← define in reference_element module
- DG basis functions ← define in basis module
- Element radius ← evaluate r(x) at points as needed
- Face connectivity ← add when faces are needed

**Key subroutine:**
```fortran
subroutine create_mesh_from_gmsh(mesh, filename)
  ! Read .msh file (Gmsh ASCII format)
  ! Extract nodes and tetrahedral elements (Gmsh type 4)
  ! Populate mesh%nnode, mesh%nelem, mesh%x, mesh%conn
end subroutine
```

### 2. geometry.F90 – Derived Geometry

```fortran
type geometry_t
  integer :: nelem
  real(dp), allocatable :: vol(:)         ! volume of each tet
  real(dp), allocatable :: J(:,:,:)       ! Jacobian matrix for each tet
  real(dp), allocatable :: det_J(:)       ! determinant of Jacobian
  real(dp), allocatable :: inv_J(:,:,:)   ! inverse Jacobian
end type
```

**Responsibilities:**
- Compute element volume V_e = |det(J)| / 6
- Build Jacobian matrix J = [x₂-x₁, x₃-x₁, x₄-x₁]
- Compute det(J) and inv(J)

**Key formulas for a tetrahedron with vertices x₁, x₂, x₃, x₄:**

Volume (determinant form):
```
a = x₂ - x₁
b = x₃ - x₁
c = x₄ - x₁
V_e = |a · (b × c)| / 6 = |det(J)| / 6
```

Reference-to-physical affine mapping:
```
x(ξ) = x₁ + J · ξ
where J is the 3×3 matrix [a b c]
and ξ ∈ [0,1]³ with ξ₁ + ξ₂ + ξ₃ ≤ 1 (reference tetrahedron)
```

Gradient transformation (chain rule):
```
∇_x u = (J⁻ᵀ) · ∇_ξ u
```

**Key subroutine:**
```fortran
subroutine compute_geometry(mesh, geom)
  ! For each element, compute volume and Jacobian
  ! Use edge vectors a, b, c and determinant/inverse formulas
end subroutine
```

### 3. reference_element.F90 – Reference Element & Quadrature

```fortran
! Reference tetrahedron vertices
ξ₁ = (0, 0, 0)
ξ₂ = (1, 0, 0)
ξ₃ = (0, 1, 0)
ξ₄ = (0, 0, 1)

! Barycentric coordinates
λ₁ = 1 - ξ₁ - ξ₂ - ξ₃
λ₂ = ξ₁
λ₃ = ξ₂
λ₄ = ξ₃
```

**Responsibilities:**
- Define reference tetrahedron
- Generate quadrature points and weights
- Provide barycentric coordinate utility

**Key subroutine:**
```fortran
subroutine quadrature_ref_tet(order, nqp, qp, w)
  ! For order ∈ {1, 2, 3, 4, ...}
  ! Generate nqp quadrature points qp and weights w
  ! Points and weights are in reference coordinates ξ
  ! Volume integral over reference tet: ∫ f(ξ) dξ = Σ w_i f(ξ_i)
end subroutine
```

**Example orders:**
- Order 1: 1 point (centroid), weight = 1
- Order 2: 4 points
- Order 3: 5 points
- Order 4: 14 points

## DG Method Data Flow

```
Mesh (topology)
    ↓
Geometry (volumes, Jacobians)
    ↓
Reference Element (reference tet & quadrature)
    ↓
Basis (polynomial basis on reference tet)
    ↓
Quadrature (map quadrature points to physical elements)
    ↓
State (solution coefficients at quadrature points)
    ↓
Gravity / LDG Poisson Solver
    ↓
Flux (Euler flux + numerical flux)
    ↓
RHS (residual assembly)
    ↓
Time Integrator (ODE stepping)
```

## How to Use

### Step 1: Create a tetrahedral mesh

Use an external mesh generator. Example with Gmsh:

```bash
gmsh -format msh2 sphere.geo -o sphere.msh
```

Where `sphere.geo` defines a 3D ball (example below).

### Step 2: Read the mesh

```fortran
program main
  use mesh_mod
  use geometry_mod
  implicit none
  
  type(mesh_t) :: mesh
  type(geometry_t) :: geom
  
  ! Read mesh
  call create_mesh_from_gmsh(mesh, 'sphere.msh')
  
  ! Compute geometry
  call compute_geometry(mesh, geom)
  
  ! Print statistics
  print *, 'Mesh loaded:'
  print *, '  Nodes: ', mesh%nnode
  print *, '  Elements: ', mesh%nelem
  print *, '  Total volume: ', sum(geom%vol)
  
end program main
```

### Step 3: Quadrature and basis

```fortran
use reference_element_mod

! Get quadrature for reference tetrahedron
integer :: nqp, order
real(dp), allocatable :: qp(:,:), w(:)

order = 3  ! or 1, 2, 4, ...
call quadrature_ref_tet(order, nqp, qp, w)

! qp has shape (nqp, 3): quadrature points in reference coordinates
! w has shape (nqp): weights
! Use these to set up basis functions and assembly loops
```

### Step 4: DG assembly (future basis/state modules)

For each element e:
1. Get physical coordinates from mesh%x(mesh%conn(e,:), :)
2. Get Jacobian from geom%J(e,:,:)
3. For each quadrature point qp_i (reference coords):
   - Map to physical: x_i = x₁(e) + J(e) · qp_i
   - Evaluate basis functions at qp_i (reference coords)
   - Evaluate solution at x_i (physical coords)
   - Accumulate volume integral: ∫_K f(u) dx ≈ Σ_i w_i f(u_i) |det J(e)|

## Extending the Design: Face Data (Future)

When adding face integrals for numerical fluxes, add a face connectivity structure:

```fortran
type :: face_t
  integer :: elem1, elem2          ! element indices on each side
  integer :: local_face1, local_face2  ! local face indices (1-4)
  integer :: nnode_face            ! number of nodes on face (3 for triangle)
  integer, allocatable :: nodes(:) ! node indices of face
  real(dp), allocatable :: normal(:)  ! outward normal
  real(dp) :: area                 ! face area
end type
```

But this is not needed for the initial DG volume assembly.

## Example Gmsh Script for a Sphere (sphere.geo)

```gmsh
// Define a ball of radius R
R = 1.0;
lc = 0.2;  // characteristic length (mesh size)

Point(1) = {0, 0, 0, lc};  // center
Point(2) = {R, 0, 0, lc};
Point(3) = {-R, 0, 0, lc};
Point(4) = {0, R, 0, lc};
Point(5) = {0, -R, 0, lc};
Point(6) = {0, 0, R, lc};
Point(7) = {0, 0, -R, lc};

// Create a sphere surface
// (Gmsh will tetrahedralize the interior automatically with 3D meshing)
Circle(1) = {2, 1, 4};
Circle(2) = {4, 1, 3};
Circle(3) = {3, 1, 5};
Circle(4) = {5, 1, 2};
Circle(5) = {2, 1, 6};
Circle(6) = {6, 1, 3};
Circle(7) = {3, 1, 7};
Circle(8) = {7, 1, 2};
Circle(9) = {4, 1, 6};
Circle(10) = {6, 1, 5};
Circle(11) = {5, 1, 7};
Circle(12) = {7, 1, 4};

// Curves forming faces
Curve Loop(1) = {1, 9, 5};
Surface(1) = {1};
// ... (more faces defined similarly)

// Create 3D volume
Surface Loop(1) = {1, 2, 3, 4, 5, 6};  // all boundary faces
Volume(1) = {1};

// 3D mesh generation
Field[1] = Distance;
Field[1].Sampling = 100;
Field[2] = Threshold;
Field[2].InField = 1;
Field[2].SizeMin = lc / 4;
Field[2].SizeMax = lc;
Field[2].DistMin = 0.15 * R;
Field[2].DistMax = 0.5 * R;
Background Field = 2;

Mesh.Algorithm = 6;  // Use Delaunay for 3D
```

Then run:
```bash
gmsh sphere.geo -format msh2 -o sphere.msh
```

## Why External Mesh Generation?

Writing a tetrahedral mesh generator for a ball is:
- Non-trivial computational geometry
- Error-prone (Delaunay criteria, point distribution, singularities)
- Outside the scope of your DG research

Standard approaches:
1. **Gmsh**: Free, scriptable, handles spheres well
2. **TetGen**: Specialized for Delaunay tetrahedralization
3. **Netgen**: Produces high-quality meshes

For your core-collapse problem, use Gmsh with a simple spherical geometry script.

## Checklist for Implementation

- [x] Minimal mesh.F90 with topology only
- [x] geometry.F90 computing volumes and Jacobians
- [x] reference_element.F90 with quadrature
- [ ] basis.F90 (polynomial basis functions on reference tet)
- [ ] quadrature.F90 (map reference → physical quadrature, face quadrature)
- [ ] state.F90 (conserved/primitive variables, DG coefficients)
- [ ] gravity.F90 (gravitational potential, LDG Poisson solver)
- [ ] flux.F90 (Euler fluxes, numerical fluxes)
- [ ] rhs.F90 (residual assembly from geometry/basis/flux)
- [ ] time_integrator.F90 (SSPRK or IMEX stepping)

## Key Takeaways

1. **Mesh = Topology Only**
   - Nodes and connectivity
   - Everything else is derived

2. **Geometry ≠ Mesh**
   - Volumes, Jacobians, normals computed from topology
   - Kept separate for clarity and reusability

3. **Reference Element ≠ Physical Element**
   - Quadrature and basis functions defined on reference tet
   - Mapped to physical elements via affine transformation

4. **DG Assembly = Quadrature Loop**
   - For each element and quadrature point
   - Map to physical coordinates via J
   - Evaluate basis, flux, residual
   - Accumulate into global system

5. **External Meshing is OK**
   - Use Gmsh/TetGen for tetrahedral ball
   - Focus your effort on the DG solver, not geometry

---

**Next Steps:**
1. Generate a test mesh with Gmsh
2. Verify mesh reading with print statements
3. Verify geometry computation (volumes should sum to 4πR³/3)
4. Build basis.F90 for polynomial basis on reference tet
5. Implement quadrature mapping to physical elements
6. Assemble first volume integral test case
