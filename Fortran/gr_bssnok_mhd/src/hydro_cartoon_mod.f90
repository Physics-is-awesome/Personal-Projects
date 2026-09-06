module hydro_cartoon_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, dx
  use grid_mod, only: xg
  use hydro_vars_mod, only: NPRIM, IVX, IVY
  implicit none
  private
  public :: get_neighbor_prim

contains

  function get_neighbor_prim(u, i, k, ix_off, iz_off, y_val) result(v)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: i, k, ix_off, iz_off
    real(dp), intent(in) :: y_val
    real(dp) :: v(NPRIM)
    real(dp) :: Xtarget
    integer  :: itarget, ktarget

    Xtarget = xg(i) + real(ix_off, dp) * dx
    itarget = i + ix_off
    ktarget = k + iz_off

    if (abs(y_val) < 1.0e-14_dp .and. itarget >= 1 .and. itarget <= Nx) then
      v = u(itarget, ktarget, :)
    else
      v = cartoon_ghost_prim(u, ktarget, Xtarget, y_val)
    end if
  end function get_neighbor_prim

  function cartoon_ghost_prim(u, k, X, Y) result(vout)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: k
    real(dp), intent(in) :: X, Y
    real(dp) :: vout(NPRIM)
    real(dp) :: varpi, theta, c, s
    real(dp) :: f(NPRIM)

    varpi = sqrt(X*X + Y*Y)
    theta = atan2(Y, X)
    c = cos(theta); s = sin(theta)

    f = interp_radial_prim(u, k, varpi)
    vout = f
    vout(IVX) = c*f(IVX) - s*f(IVY)
    vout(IVY) = s*f(IVX) + c*f(IVY)
    ! IVZ, IRHO, IEPS are scalars under this rotation: unchanged
  end function cartoon_ghost_prim

  function interp_radial_prim(u, k, varpi) result(fvals)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: k
    real(dp), intent(in) :: varpi
    real(dp) :: fvals(NPRIM)
    integer  :: j
    real(dp) :: x0, x1, x2, L0, L1, L2

    j = floor(varpi / dx) + 1
    if (j < 1) j = 1
    if (j > Nx - 2) j = Nx - 2

    x0 = xg(j); x1 = xg(j+1); x2 = xg(j+2)
    L0 = (varpi - x1) * (varpi - x2) / ((x0 - x1) * (x0 - x2))
    L1 = (varpi - x0) * (varpi - x2) / ((x1 - x0) * (x1 - x2))
    L2 = (varpi - x0) * (varpi - x1) / ((x2 - x0) * (x2 - x1))

    fvals = L0 * u(j, k, :) + L1 * u(j+1, k, :) + L2 * u(j+2, k, :)
  end function interp_radial_prim

end module hydro_cartoon_mod
