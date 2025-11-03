!===========================================================
! mass_matrix.f90
! Compute 1D L2 mass matrix for Lagrange basis on [-1,1]
!===========================================================
module quad_mod
  implicit none
contains
  subroutine gauss_legendre(n, x, w)
    ! Generate n-point Gauss-Legendre nodes (x) and weights (w)
    integer, intent(in) :: n
    real(8), intent(out) :: x(n), w(n)
    integer :: i, j, m
    real(8) :: z, z1, p1, p2, p3, pp, pi
    pi = 4.0d0 * atan(1.0d0)
    m = (n + 1)/2
    do i = 1, m
       z = cos(pi * (i - 0.25d0) / (n + 0.5d0))
       do
          p1 = 1.0d0
          p2 = 0.0d0
          do j = 1, n
             p3 = p2
             p2 = p1
             p1 = ((2.0d0*j - 1.0d0)*z*p2 - (j - 1.0d0)*p3)/j
          end do
          pp = n*(z*p1 - p2)/(z*z - 1.0d0)
          z1 = z
          z = z1 - p1/pp
          if (abs(z - z1) < 1.0d-14) exit
       end do
       x(i)     = -z
       x(n+1-i) =  z
       w(i)     = 2.0d0 / ((1.0d0 - z*z)*pp*pp)
       w(n+1-i) =  w(i)
    end do
  end subroutine gauss_legendre
end module quad_mod
!===========================================================

module basis_mod
  implicit none
contains
  subroutine lagrange_basis(nodes, xi, phi)
    ! Compute Lagrange basis functions at given xi
    real(8), intent(in) :: nodes(:), xi(:)
    real(8), intent(out) :: phi(size(nodes), size(xi))
    integer :: i, j, k, n
    real(8) :: num
    n = size(nodes)
    phi = 1.0d0
    do i = 1, n
       do k = 1, size(xi)
          do j = 1, n
             if (j /= i) then
                phi(i,k) = phi(i,k) * (xi(k) - nodes(j)) / (nodes(i) - nodes(j))
             end if
          end do
       end do
    end do
  end subroutine lagrange_basis
end module basis_mod
!===========================================================

module mass_matrix_mod
  use quad_mod
  use basis_mod
  implicit none
contains
  subroutine compute_mass_matrix(p, a, b, M)
    ! Compute element mass matrix for Lagrange basis of degree p on [a,b]
    integer, intent(in) :: p
    real(8), intent(in) :: a, b
    real(8), intent(out) :: M(p+1, p+1)
    integer :: nq, i, j, q
    real(8) :: J, xq(:), wq(:), nodes(:), phi(:,:)

    nq = p + 2
    allocate(xq(nq), wq(nq))
    call gauss_legendre(nq, xq, wq)

    ! Reference element [-1,1] nodes
    allocate(nodes(p+1))
    nodes = [( -1.0d0 + 2.0d0*(i-1)/p, i=1,p+1 )]

    allocate(phi(p+1, nq))
    call lagrange_basis(nodes, xq, phi)

    J = (b - a) / 2.0d0
    M = 0.0d0

    do i = 1, p+1
       do j = 1, p+1
          do q = 1, nq
             M(i,j) = M(i,j) + wq(q) * phi(i,q) * phi(j,q) * J
          end do
       end do
    end do

    deallocate(xq, wq, nodes, phi)
  end subroutine compute_mass_matrix
end module mass_matrix_mod
!===========================================================

program main
  use mass_matrix_mod
  implicit none
  integer, parameter :: p = 3
  real(8) :: M(p+1, p+1)
  real(8) :: a, b
  integer :: i, j

  a = 0.0d0
  b = 1.0d0

  call compute_mass_matrix(p, a, b, M)

  print *, "Mass matrix M for degree ", p, " on [", a, ",", b, "]:"
  do i = 1, p+1
     write(*,'(100(f10.6,1x))') (M(i,j), j=1,p+1)
  end do
end program main
!===========================================================
