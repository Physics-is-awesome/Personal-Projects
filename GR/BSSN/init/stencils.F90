module deriv_stencils_mod
  use kinds_mod,      only: dp
  use params_mod,     only: Nx, Nz, r_excise, dx
  use grid_mod,       only: xg, zg
  use fd_weights_mod, only: fd_weights
  implicit none
  private
  public :: init_deriv_stencils, dx1, dx2, dz1, dz2, dxz

  integer, parameter :: N1 = 5   ! points for order-4 first derivative
  integer, parameter :: N2 = 6   ! points for order-4 second derivative (one-sided)

  type :: stencil_t
    integer,  allocatable :: off(:)
    real(dp), allocatable :: w(:)
  end type stencil_t

  type(stencil_t), allocatable :: sx1(:,:), sx2(:,:), sz1(:,:), sz2(:,:)
  logical,         allocatable :: valid(:,:)

contains

  ! Re-entrant: deallocate first so the module can be re-initialised after a
  ! resolution change (constraint-convergence and other diagnostic drivers rely
  ! on this).  Previously a second call aborted with "Attempting to allocate
  ! already allocated variable 'valid'".
  subroutine init_deriv_stencils()
    integer :: i, k
    if (allocated(sx1)) deallocate(sx1)
    if (allocated(sx2)) deallocate(sx2)
    if (allocated(sz1)) deallocate(sz1)
    if (allocated(sz2)) deallocate(sz2)
    if (allocated(valid)) deallocate(valid)
    allocate(valid(Nx,Nz))
    do k = 1, Nz
      do i = 1, Nx
        valid(i,k) = sqrt(xg(i)**2 + zg(k)**2) >= r_excise
      end do
    end do

    allocate(sx1(Nx,Nz), sx2(Nx,Nz), sz1(Nx,Nz), sz2(Nx,Nz))
    do k = 1, Nz
      do i = 1, Nx
        if (.not. valid(i,k)) cycle   ! RHS is never evaluated inside excision
        call build_stencil(i, k, .true.,  1, N1, sx1(i,k))
        call build_stencil(i, k, .true.,  2, N2, sx2(i,k))
        call build_stencil(i, k, .false., 1, N1, sz1(i,k))
        call build_stencil(i, k, .false., 2, N2, sz2(i,k))
      end do
    end do
  end subroutine init_deriv_stencils

  ! Tries centered, then fully-forward, then fully-backward windows of
  ! size npts, shrinking (and thus dropping order) only if none fit --
  ! which shouldn't happen given an adequately buffered r_excise.
  subroutine build_stencil(i, k, is_x, m, npts, st)
    integer, intent(in)  :: i, k, m, npts
    logical, intent(in)  :: is_x
    type(stencil_t), intent(out) :: st
    integer :: npts_try, half

    do npts_try = npts, m+1, -1
      half = (npts_try-1)/2
      if (try_window(i,k,is_x,-half, npts_try-1-half, m, st)) return
      if (try_window(i,k,is_x, 0,    npts_try-1,       m, st)) return
      if (try_window(i,k,is_x,-(npts_try-1), 0,        m, st)) return
    end do
    error stop "build_stencil: no valid window -- increase r_excise buffer"
  end subroutine build_stencil

  logical function try_window(i, k, is_x, lo, hi, m, st) result(ok)
    integer, intent(in)  :: i, k, lo, hi, m
    logical, intent(in)  :: is_x
    type(stencil_t), intent(out) :: st
    integer  :: n, j, idx
    real(dp), allocatable :: xnode(:), c(:,:)

    ok = .false.
    n = hi - lo
    if (n < m) return
    allocate(xnode(0:n))
    do j = 0, n
      idx = lo + j
      if (is_x) then
        if (i+idx < 1 .or. i+idx > Nx) return
        if (.not. valid(i+idx, k)) return
        xnode(j) = xg(i+idx) - xg(i)
      else
        if (k+idx < 1 .or. k+idx > Nz) return
        if (.not. valid(i, k+idx)) return
        xnode(j) = zg(k+idx) - zg(k)
      end if
    end do

    allocate(c(0:n,0:m))
    call fd_weights(0.0_dp, xnode, n, m, c)
    allocate(st%off(0:n), st%w(0:n))
    do j = 0, n
      st%off(j) = lo + j
      st%w(j)   = c(j,m)
    end do
    ok = .true.
  end function try_window

  real(dp) function dx1(u, i, k) result(r)
    real(dp), intent(in) :: u(:,:); integer, intent(in) :: i, k
    integer :: j
    r = 0.0_dp
    do j = 0, size(sx1(i,k)%off)-1
      r = r + sx1(i,k)%w(j) * u(i+sx1(i,k)%off(j), k)
    end do
  end function dx1

  real(dp) function dx2(u, i, k) result(r)
    real(dp), intent(in) :: u(:,:); integer, intent(in) :: i, k
    integer :: j
    r = 0.0_dp
    do j = 0, size(sx2(i,k)%off)-1
      r = r + sx2(i,k)%w(j) * u(i+sx2(i,k)%off(j), k)
    end do
  end function dx2

  real(dp) function dz1(u, i, k) result(r)
    real(dp), intent(in) :: u(:,:); integer, intent(in) :: i, k
    integer :: j
    r = 0.0_dp
    do j = 0, size(sz1(i,k)%off)-1
      r = r + sz1(i,k)%w(j) * u(i, k+sz1(i,k)%off(j))
    end do
  end function dz1

  real(dp) function dz2(u, i, k) result(r)
    real(dp), intent(in) :: u(:,:); integer, intent(in) :: i, k
    integer :: j
    r = 0.0_dp
    do j = 0, size(sz2(i,k)%off)-1
      r = r + sz2(i,k)%w(j) * u(i, k+sz2(i,k)%off(j))
    end do
  end function dz2

  ! Mixed partial via nested 1D application: d/dz[ d/dx u ].
  real(dp) function dxz(u, i, k) result(r)
    real(dp), intent(in) :: u(:,:); integer, intent(in) :: i, k
    integer :: j, kk
    r = 0.0_dp
    do j = 0, size(sz1(i,k)%off)-1
      kk = k + sz1(i,k)%off(j)
      r  = r + sz1(i,k)%w(j) * dx1(u, i, kk)
    end do
  end function dxz

end module deriv_stencils_mod
