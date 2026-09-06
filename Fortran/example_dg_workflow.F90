! Example: Using mesh and geometry modules in a DG solver context
! This shows the basic workflow for setting up a DG calculation

program dg_solver_example
  use iso_fortran_env, only: real64
  use mesh_mod, only: mesh_t, dp, create_mesh_from_gmsh
  use geometry_mod, only: geometry_t, compute_geometry
  use reference_element_mod, only: quadrature_ref_tet
  implicit none

  type(mesh_t) :: mesh
  type(geometry_t) :: geom
  integer :: e, q, nqp, order
  real(dp), allocatable :: qp(:,:), w(:)
  real(dp) :: x_phys(3), r_phys, rho_init
  real(dp) :: J_inv(3,3)

  print *, "========================================="
  print *, "Example: DG Solver Workflow"
  print *, "========================================="

  ! STEP 1: Create mesh (synthetic for this example)
  print *, ""
  print *, "STEP 1: Mesh Setup"
  print *, "-"
  call create_simple_mesh(mesh)
  print *, "Mesh created: ", mesh%nelem, " elements"

  ! STEP 2: Compute geometry
  print *, ""
  print *, "STEP 2: Geometry Computation"
  print *, "-"
  call compute_geometry(mesh, geom)
  print *, "Total mesh volume: ", sum(geom%vol)
  print *, "Expected (unit tet volume): ", 1.0_dp / 6.0_dp

  ! STEP 3: Get reference quadrature
  print *, ""
  print *, "STEP 3: Quadrature Setup"
  print *, "-"
  order = 2
  call quadrature_ref_tet(order, nqp, qp, w)
  print *, "Quadrature order ", order, " with ", nqp, " points"

  ! STEP 4: Example DG assembly loop
  ! (This is a simplified version showing data flow only)
  print *, ""
  print *, "STEP 4: DG Assembly Example"
  print *, "Loop: For each element and quadrature point..."
  print *, "-"

  do e = 1, mesh%nelem
     print *, ""
     print *, "Element ", e, ":"

     ! Get Jacobian for this element
     J_inv = geom%inv_J(e, :, :)
     print *, "  det(J) = ", geom%det_J(e)
     print *, "  Volume = ", geom%vol(e)

     ! Loop over quadrature points
     do q = 1, nqp
        ! Map reference quadrature point to physical coordinates
        ! x(ξ) = x₁ + J · ξ
        x_phys = map_to_physical(mesh, e, qp(q, :), geom)

        ! Evaluate radius for gravity/source terms
        r_phys = sqrt(x_phys(1)**2 + x_phys(2)**2 + x_phys(3)**2)

        ! Example: Initial density (toy model)
        rho_init = 1.0_dp / (1.0_dp + r_phys**2)

        print '(A,I3,A,3F8.3,A,F8.3,A,F8.4)', &
             "    QP ", q, ": ξ=", qp(q,:), " x=", x_phys(1), " ρ=", rho_init

        ! In a full DG code, you would:
        ! - Evaluate basis functions φ_i(ξ_q), ∇φ_i(ξ_q)
        ! - Evaluate solution u(x_q) from coefficients
        ! - Compute flux F(u), source S(u)
        ! - Accumulate residual: ∫ φ_i(F + S) dV ≈ Σ_q w_q φ_i(ξ_q) F(u_q) |det J|
     end do
  end do

  print *, ""
  print *, "========================================="
  print *, "Key observations:"
  print *, "  - Mesh topology is minimal (nodes + connectivity)"
  print *, "  - Geometry is computed once per mesh (volumes, Jacobians)"
  print *, "  - Quadrature is reference-based (ξ coordinates)"
  print *, "  - Physical coordinates x(ξ) = x₁ + J·ξ map ref to phys"
  print *, "  - Gradients use J⁻ᵀ for chain rule"
  print *, "  - Volume/source terms evaluated at quadrature points"
  print *, "========================================="

  deallocate(qp, w)

contains

  subroutine create_simple_mesh(m)
    type(mesh_t), intent(inout) :: m
    m%nnode = 4
    m%nelem = 1
    allocate(m%x(4, 3))
    allocate(m%conn(1, 4))
    m%x(1, :) = [0.0_dp, 0.0_dp, 0.0_dp]
    m%x(2, :) = [1.0_dp, 0.0_dp, 0.0_dp]
    m%x(3, :) = [0.0_dp, 1.0_dp, 0.0_dp]
    m%x(4, :) = [0.0_dp, 0.0_dp, 1.0_dp]
    m%conn(1, :) = [1, 2, 3, 4]
  end subroutine

  function map_to_physical(m, elem, xi_ref, g) result(x_phys)
    type(mesh_t), intent(in) :: m
    type(geometry_t), intent(in) :: g
    integer, intent(in) :: elem
    real(dp), intent(in) :: xi_ref(3)
    real(dp) :: x_phys(3)
    integer :: n1
    real(dp) :: x1(3)

    ! Get first vertex
    n1 = m%conn(elem, 1)
    x1 = m%x(n1, :)

    ! Map: x(ξ) = x₁ + J·ξ
    x_phys = x1 + matmul(g%J(elem, :, :), xi_ref)
  end function

end program dg_solver_example
