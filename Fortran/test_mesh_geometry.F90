program test_mesh_geometry
  !
  ! Test program demonstrating mesh and geometry module usage.
  !
  ! This program:
  !   1. Creates a simple tetrahedral mesh (either from file or synthetic)
  !   2. Computes geometry (volumes, Jacobians)
  !   3. Verifies basic properties
  !   4. Generates reference element quadrature
  !
  use iso_fortran_env, only: real64
  use mesh_mod, only: mesh_t, dp
  use geometry_mod, only: geometry_t, compute_geometry
  use reference_element_mod, only: quadrature_ref_tet
  implicit none

  type(mesh_t) :: mesh
  type(geometry_t) :: geom
  integer :: i, e, nqp, order
  real(dp), allocatable :: qp(:,:), w(:)
  real(dp) :: total_vol, expected_vol, R

  print *, "========================================="
  print *, "Test: Mesh and Geometry Modules"
  print *, "========================================="

  ! Create a simple synthetic mesh: a single tetrahedron
  print *, ""
  print *, "Step 1: Create synthetic test mesh"
  print *, "---"
  call create_test_mesh(mesh)

  print *, "Mesh created with:"
  print *, "  Nodes: ", mesh%nnode
  print *, "  Elements: ", mesh%nelem
  print *, ""
  print *, "Node coordinates:"
  do i = 1, mesh%nnode
     print '(A,I3,A,3F12.6)', "  x(", i, ") = ", mesh%x(i, :)
  end do

  print *, ""
  print *, "Element connectivity:"
  do e = 1, mesh%nelem
     print '(A,I3,A,4I6)', "  conn(", e, ") = ", mesh%conn(e, :)
  end do

  ! Compute geometry
  print *, ""
  print *, "Step 2: Compute geometry"
  print *, "---"
  call compute_geometry(mesh, geom)

  print *, "Geometry computed:"
  do e = 1, geom%nelem
     print '(A,I3,A,F12.6)', "  Volume of element ", e, " = ", geom%vol(e)
     print '(A,I3,A,F12.6)', "  det(J) of element ", e, " = ", geom%det_J(e)
  end do

  ! Verify volume
  total_vol = sum(geom%vol)
  print *, ""
  print *, "Total mesh volume: ", total_vol
  print *, "Expected volume of unit tet: ", 1.0_dp/6.0_dp
  if (abs(total_vol - 1.0_dp/6.0_dp) < 1.0d-10) then
     print *, "✓ Volume check PASSED"
  else
     print *, "✗ Volume check FAILED"
  end if

  ! Test reference element quadrature
  print *, ""
  print *, "Step 3: Reference element quadrature"
  print *, "---"

  do order = 1, 4
     call quadrature_ref_tet(order, nqp, qp, w)
     print *, ""
     print '(A,I2,A,I3,A)', "Quadrature order ", order, " has ", nqp, " points:"
     print '(A,F12.8)', "  Total weight: ", sum(w)
     do i = 1, min(nqp, 5)
        print '(A,I3,A,3F12.8,A,F12.8)', "    qp(", i, ") = ", qp(i, :), " w = ", w(i)
     end do
     if (nqp > 5) then
        print *, "    ... (", nqp - 5, " more points)"
     end if
     deallocate(qp, w)
  end do

  ! Test with a mesh read from file (if file exists)
  print *, ""
  print *, "Step 4: Mesh I/O test"
  print *, "---"
  print *, "To read from a Gmsh file, call:"
  print *, "  call create_mesh_from_gmsh(mesh, 'sphere.msh')"
  print *, ""
  print *, "Expected .msh file format:"
  print *, "  $MeshFormat"
  print *, "  2.2 0 8"
  print *, "  $EndMeshFormat"
  print *, "  $Nodes"
  print *, "  <nnode>"
  print *, "  <node_id> <x> <y> <z>"
  print *, "  ..."
  print *, "  $EndNodes"
  print *, "  $Elements"
  print *, "  <nelem>"
  print *, "  <elem_id> <type> <ntags> <tag1> ... <node1> <node2> ..."
  print *, "  (type=4 for tetrahedra)"
  print *, "  ..."
  print *, "  $EndElements"

  print *, ""
  print *, "========================================="
  print *, "Test completed successfully"
  print *, "========================================="

contains

  subroutine create_test_mesh(m)
    !
    ! Create a simple synthetic mesh: unit tetrahedron
    ! with vertices at (0,0,0), (1,0,0), (0,1,0), (0,0,1)
    !
    type(mesh_t), intent(inout) :: m

    m%nnode = 4
    m%nelem = 1

    allocate(m%x(4, 3))
    allocate(m%conn(1, 4))

    ! Vertices of unit tetrahedron
    m%x(1, :) = [0.0_dp, 0.0_dp, 0.0_dp]
    m%x(2, :) = [1.0_dp, 0.0_dp, 0.0_dp]
    m%x(3, :) = [0.0_dp, 1.0_dp, 0.0_dp]
    m%x(4, :) = [0.0_dp, 0.0_dp, 1.0_dp]

    ! Connectivity
    m%conn(1, :) = [1, 2, 3, 4]

  end subroutine create_test_mesh

end program test_mesh_geometry
