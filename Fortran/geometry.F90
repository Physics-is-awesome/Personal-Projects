module geometry_mod
  !
  ! Geometry module for 3D tetrahedral elements.
  !
  ! Computes derived quantities from mesh topology:
  !   - element volumes
  !   - Jacobian matrices and determinants
  !   - reference-to-physical affine mappings
  !   - normal vectors (to be extended for face data)
  !
  ! These are all derived from node coordinates and connectivity.
  ! They do NOT belong in the mesh module.
  !
  use iso_fortran_env, only: real64
  implicit none
  private

  integer, parameter, public :: dp = real64
  real(dp), parameter :: TOL = 1.0d-15

  type, public :: geometry_t
     integer :: nelem = 0
     real(dp), allocatable :: vol(:)        ! vol(e): volume of element e
     real(dp), allocatable :: J(:,:,:)      ! J(e,1:3,1:3): Jacobian matrix for element e
     real(dp), allocatable :: det_J(:)      ! det_J(e): determinant of Jacobian
     real(dp), allocatable :: inv_J(:,:,:)  ! inv_J(e,1:3,1:3): inverse Jacobian
  end type geometry_t

  public :: compute_geometry

contains

  subroutine compute_geometry(mesh, geom)
    !
    ! Compute derived geometric quantities from mesh topology.
    !
    ! Input:
    !   mesh  : tetrahedral mesh (topology only)
    !
    ! Output:
    !   geom  : geometry structure with volumes, Jacobians
    !
    use mesh_mod, only: mesh_t

    type(mesh_t), intent(in) :: mesh
    type(geometry_t), intent(inout) :: geom

    integer :: e, n1, n2, n3, n4
    real(dp) :: x1(3), x2(3), x3(3), x4(3)
    real(dp) :: a(3), b(3), c(3)
    real(dp) :: denom

    geom%nelem = mesh%nelem
    allocate(geom%vol(mesh%nelem))
    allocate(geom%J(mesh%nelem, 3, 3))
    allocate(geom%det_J(mesh%nelem))
    allocate(geom%inv_J(mesh%nelem, 3, 3))

    do e = 1, mesh%nelem
       ! Get node indices for tetrahedron e
       n1 = mesh%conn(e, 1)
       n2 = mesh%conn(e, 2)
       n3 = mesh%conn(e, 3)
       n4 = mesh%conn(e, 4)

       ! Get coordinates
       x1(:) = mesh%x(n1, :)
       x2(:) = mesh%x(n2, :)
       x3(:) = mesh%x(n3, :)
       x4(:) = mesh%x(n4, :)

       ! Edge vectors from x1
       a(:) = x2(:) - x1(:)
       b(:) = x3(:) - x1(:)
       c(:) = x4(:) - x1(:)

       ! Jacobian matrix J = [a b c] (columns)
       geom%J(e, 1, 1) = a(1)
       geom%J(e, 2, 1) = a(2)
       geom%J(e, 3, 1) = a(3)

       geom%J(e, 1, 2) = b(1)
       geom%J(e, 2, 2) = b(2)
       geom%J(e, 3, 2) = b(3)

       geom%J(e, 1, 3) = c(1)
       geom%J(e, 2, 3) = c(2)
       geom%J(e, 3, 3) = c(3)

       ! Compute determinant
       geom%det_J(e) = det3(geom%J(e, :, :))

       ! Volume = |det J| / 6
       geom%vol(e) = abs(geom%det_J(e)) / 6.0_dp

       ! Check for degenerate element
       if (abs(geom%det_J(e)) < TOL) then
          print *, 'Warning: Element ', e, ' has zero or very small volume'
       end if

       ! Compute inverse Jacobian
       geom%inv_J(e, :, :) = inv3(geom%J(e, :, :))

    end do

  end subroutine compute_geometry

  function det3(A) result(d)
    !
    ! Compute determinant of 3x3 matrix A.
    ! det(A) = A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) + A(1,3)*A(2,1)*A(3,2)
    !        - A(1,3)*A(2,2)*A(3,1) - A(1,1)*A(2,3)*A(3,2) - A(1,2)*A(2,1)*A(3,3)
    !
    real(dp), intent(in) :: A(3, 3)
    real(dp) :: d

    d = A(1,1) * (A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
      - A(1,2) * (A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
      + A(1,3) * (A(2,1)*A(3,2) - A(2,2)*A(3,1))

  end function det3

  function inv3(A) result(Ainv)
    !
    ! Compute inverse of 3x3 matrix A.
    ! Uses Cramer's rule: Ainv = adj(A) / det(A)
    !
    real(dp), intent(in) :: A(3, 3)
    real(dp) :: Ainv(3, 3), d

    d = det3(A)
    if (abs(d) < TOL) then
       print *, 'Error: Singular matrix in inv3'
       stop
    end if

    ! Adjugate matrix (transposed cofactor matrix)
    Ainv(1,1) = (A(2,2)*A(3,3) - A(2,3)*A(3,2)) / d
    Ainv(2,1) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1)) / d
    Ainv(3,1) = (A(2,1)*A(3,2) - A(2,2)*A(3,1)) / d

    Ainv(1,2) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2)) / d
    Ainv(2,2) = (A(1,1)*A(3,3) - A(1,3)*A(3,1)) / d
    Ainv(3,2) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1)) / d

    Ainv(1,3) = (A(1,2)*A(2,3) - A(1,3)*A(2,2)) / d
    Ainv(2,3) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1)) / d
    Ainv(3,3) = (A(1,1)*A(2,2) - A(1,2)*A(2,1)) / d

  end function inv3

end module geometry_mod
