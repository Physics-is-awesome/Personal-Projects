module stencil_mod
  use kinds_mod, only: dp
  use params_mod, only: dx, dy, dz
  use vars_mod, only: NVARS
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: stencil_t, compute_stencil

  type :: stencil_t
    real(dp) :: val(NVARS)
    real(dp) :: d1x(NVARS), d1y(NVARS), d1z(NVARS)
    real(dp) :: d2x(NVARS), d2y(NVARS), d2z(NVARS)
    real(dp) :: d2xy(NVARS), d2xz(NVARS), d2yz(NVARS)
  end type stencil_t

contains

  ! Fills S with the central value and all derivative arrays at grid point
  ! (i,k). Valid for i in [1, Nx-1] (i=1 is the axis, handled transparently
  ! by the Cartoon axis ghost) and k in [2, Nz-1] (k=1, Nz are the outer
  ! z-boundary and are updated separately, not via this interior stencil).
  subroutine compute_stencil(u, i, k, S)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: i, k
    type(stencil_t), intent(out) :: S

    real(dp) :: c0(NVARS)
    real(dp) :: xp(NVARS), xm(NVARS), zp(NVARS), zm(NVARS)
    real(dp) :: yp(NVARS), ym(NVARS)
    real(dp) :: xpyp(NVARS), xpym(NVARS), xmyp(NVARS), xmym(NVARS)
    real(dp) :: zpyp(NVARS), zpym(NVARS), zmyp(NVARS), zmym(NVARS)
    real(dp) :: xpzp(NVARS), xpzm(NVARS), xmzp(NVARS), xmzm(NVARS)

    c0   = u(i, k, :)

    xp   = get_neighbor(u, i, k,  1,  0, 0.0_dp)
    xm   = get_neighbor(u, i, k, -1,  0, 0.0_dp)
    zp   = get_neighbor(u, i, k,  0,  1, 0.0_dp)
    zm   = get_neighbor(u, i, k,  0, -1, 0.0_dp)
    yp   = get_neighbor(u, i, k,  0,  0,  dy)
    ym   = get_neighbor(u, i, k,  0,  0, -dy)

    xpyp = get_neighbor(u, i, k,  1,  0,  dy)
    xpym = get_neighbor(u, i, k,  1,  0, -dy)
    xmyp = get_neighbor(u, i, k, -1,  0,  dy)
    xmym = get_neighbor(u, i, k, -1,  0, -dy)

    zpyp = get_neighbor(u, i, k,  0,  1,  dy)
    zpym = get_neighbor(u, i, k,  0,  1, -dy)
    zmyp = get_neighbor(u, i, k,  0, -1,  dy)
    zmym = get_neighbor(u, i, k,  0, -1, -dy)

    xpzp = get_neighbor(u, i, k,  1,  1, 0.0_dp)
    xpzm = get_neighbor(u, i, k,  1, -1, 0.0_dp)
    xmzp = get_neighbor(u, i, k, -1,  1, 0.0_dp)
    xmzm = get_neighbor(u, i, k, -1, -1, 0.0_dp)

    S%val = c0

    S%d1x = (xp - xm) / (2.0_dp * dx)
    S%d1y = (yp - ym) / (2.0_dp * dy)
    S%d1z = (zp - zm) / (2.0_dp * dz)

    S%d2x = (xp - 2.0_dp * c0 + xm) / dx**2
    S%d2y = (yp - 2.0_dp * c0 + ym) / dy**2
    S%d2z = (zp - 2.0_dp * c0 + zm) / dz**2

    S%d2xy = (xpyp - xpym - xmyp + xmym) / (4.0_dp * dx * dy)
    S%d2xz = (xpzp - xpzm - xmzp + xmzm) / (4.0_dp * dx * dz)
    S%d2yz = (zpyp - zpym - zmyp + zmym) / (4.0_dp * dy * dz)

  end subroutine compute_stencil

end module stencil_mod
