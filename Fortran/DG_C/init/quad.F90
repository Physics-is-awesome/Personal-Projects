!=======================================================================
!  mod_quadrature : 1D Gauss-Legendre rules, normalised Legendre
!                   derivative tables, small dense LU.
!  Supplies {x_K^(q),w_K^(q)} and {x_E^(mu),w_E^(mu)} of (3.21),(3.23),
!  (3.26) and the modal basis of Sec. 3.1.
!=======================================================================
module mod_quadrature
  use mod_kinds
  implicit none
  private
  public :: gauss_legendre, leg_table, dense_lu, dense_lusolve

contains

  !---------------------------------------------------------------------
  !  n-point Gauss-Legendre rule on [-1,1]:  sum(w) = 2
  !---------------------------------------------------------------------
  subroutine gauss_legendre(n, x, w)
    integer,  intent(in)  :: n
    real(dp), intent(out) :: x(n), w(n)
    integer  :: i, j, it, m
    real(dp) :: z, z1, p1, p2, p3, pp
    m = (n+1)/2
    do i = 1, m
       z = cos(pi*(real(i,dp)-0.25_dp)/(real(n,dp)+0.5_dp))
       pp = one
       do it = 1, 200
          p1 = one ; p2 = zero
          do j = 1, n
             p3 = p2 ; p2 = p1
             p1 = ((two*real(j,dp)-one)*z*p2 - (real(j,dp)-one)*p3)/real(j,dp)
          end do
          pp = real(n,dp)*(z*p1-p2)/(z*z-one)
          z1 = z ; z = z1 - p1/pp
          if (abs(z-z1) <= 1.0e-16_dp) exit
       end do
       x(i)     = -z
       x(n+1-i) =  z
       w(i)     = two/((one-z*z)*pp*pp)
       w(n+1-i) = w(i)
    end do
  end subroutine gauss_legendre

  !---------------------------------------------------------------------
  !  tab(m,r) = d^r/dx^r [ sqrt(2m+1) P_m(x) ],  m=0..kmax, r=0..rmax
  !
  !  from the r-times differentiated Legendre recurrence
  !     (m+1) P^{(r)}_{m+1} = (2m+1)( x P^{(r)}_m + r P^{(r-1)}_m )
  !                           - m P^{(r)}_{m-1}
  !---------------------------------------------------------------------
  subroutine leg_table(kmax, rmax, x, tab)
    integer,  intent(in)  :: kmax, rmax
    real(dp), intent(in)  :: x
    real(dp), intent(out) :: tab(0:kmax,0:rmax)
    real(dp), allocatable :: p(:,:)
    real(dp) :: t
    integer  :: m, r
    allocate(p(0:kmax,0:rmax))
    p = zero
    p(0,0) = one
    if (kmax >= 1) then
       p(1,0) = x
       if (rmax >= 1) p(1,1) = one
    end if
    do m = 1, kmax-1
       do r = 0, rmax
          t = x*p(m,r)
          if (r >= 1) t = t + real(r,dp)*p(m,r-1)
          p(m+1,r) = ((two*real(m,dp)+one)*t - real(m,dp)*p(m-1,r))/(real(m,dp)+one)
       end do
    end do
    do m = 0, kmax
       tab(m,:) = sqrt(two*real(m,dp)+one)*p(m,:)
    end do
    deallocate(p)
  end subroutine leg_table

  !---------------------------------------------------------------------
  !  dense LU with partial pivoting (block preconditioner)
  !---------------------------------------------------------------------
  subroutine dense_lu(n, a, ipiv, info)
    integer,  intent(in)    :: n
    real(dp), intent(inout) :: a(n,n)
    integer,  intent(out)   :: ipiv(n), info
    integer  :: i, j, k, ip
    real(dp) :: amax, t
    info = 0
    do k = 1, n
       ip = k ; amax = abs(a(k,k))
       do i = k+1, n
          if (abs(a(i,k)) > amax) then ; amax = abs(a(i,k)) ; ip = i ; end if
       end do
       ipiv(k) = ip
       if (amax == zero) then ; info = k ; return ; end if
       if (ip /= k) then
          do j = 1, n ; t = a(k,j) ; a(k,j) = a(ip,j) ; a(ip,j) = t ; end do
       end if
       do i = k+1, n
          a(i,k) = a(i,k)/a(k,k)
          do j = k+1, n
             a(i,j) = a(i,j) - a(i,k)*a(k,j)
          end do
       end do
    end do
  end subroutine dense_lu

  subroutine dense_lusolve(n, a, ipiv, b)
    integer,  intent(in)    :: n, ipiv(n)
    real(dp), intent(in)    :: a(n,n)
    real(dp), intent(inout) :: b(n)
    integer  :: i, k
    real(dp) :: t
    do k = 1, n
       if (ipiv(k) /= k) then ; t = b(k) ; b(k) = b(ipiv(k)) ; b(ipiv(k)) = t ; end if
       do i = k+1, n
          b(i) = b(i) - a(i,k)*b(k)
       end do
    end do
    do k = n, 1, -1
       b(k) = b(k)/a(k,k)
       do i = 1, k-1
          b(i) = b(i) - a(i,k)*b(k)
       end do
    end do
  end subroutine dense_lusolve

end module mod_quadrature
