module cartoon_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, dx
  use grid_mod, only: xg
  use vars_mod, only: NVARS, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                       IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                       IGTX, IGTY, IGTZ, IBETAX, IBETAY, IBETAZ, IBX, IBY, IBZ
  implicit none
  private
  public :: get_neighbor

contains

  ! Returns the NVARS field values at the "logical" neighbor of grid point
  ! (i,k), offset by ix_off grid spacings in x, iz_off grid spacings in z
  ! (z is never rotated - it's the symmetry axis - so this is a plain index
  ! shift; caller is responsible for k+iz_off staying in [1,Nz]), and y_val
  ! in the (physical, not gridded) y direction. Interior x-neighbors with
  ! y_val=0 are read directly (exact); everything else (y-ghosts, and the
  ! axis x<0 ghost) goes through the Cartoon radial-interpolation + rotation
  ! machinery, evaluated on row ktarget = k + iz_off.
  function get_neighbor(u, i, k, ix_off, iz_off, y_val) result(v)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: i, k, ix_off, iz_off
    real(dp), intent(in) :: y_val
    real(dp) :: v(NVARS)
    real(dp) :: Xtarget
    integer  :: itarget, ktarget

    Xtarget = xg(i) + real(ix_off, dp) * dx
    itarget = i + ix_off
    ktarget = k + iz_off

    if (abs(y_val) < 1.0e-14_dp .and. itarget >= 1 .and. itarget <= Nx) then
      v = u(itarget, ktarget, :)
    else
      v = cartoon_ghost(u, ktarget, Xtarget, y_val)
    end if
  end function get_neighbor

  ! Synthesizes the field state at physical point (X, Y, z(k)) from the
  ! stored axisymmetric half-plane u(:,k,:) (which lives at y=0, x>=0).
  function cartoon_ghost(u, k, X, Y) result(vout)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: k
    real(dp), intent(in) :: X, Y
    real(dp) :: vout(NVARS)
    real(dp) :: varpi, theta, c, s
    real(dp) :: f(NVARS)

    varpi = sqrt(X*X + Y*Y)
    theta = atan2(Y, X)
    c = cos(theta)
    s = sin(theta)

    f = interp_radial(u, k, varpi)

    ! Scalars: phi, K, alpha (indices not explicitly rotated) are copied as-is.
    vout = f

    ! Vectors (Gamma-tilde^i, beta^i, B^i): rotate (x,y) components.
    call rotate_vec(f(IGTX),   f(IGTY),   c, s, vout(IGTX),   vout(IGTY))
    call rotate_vec(f(IBETAX), f(IBETAY), c, s, vout(IBETAX), vout(IBETAY))
    call rotate_vec(f(IBX),    f(IBY),    c, s, vout(IBX),    vout(IBY))

    ! Tensors (gamma-tilde_ij, A-tilde_ij): full 2-index rotation.
    call rotate_tensor(f(IGXX), f(IGXY), f(IGXZ), f(IGYY), f(IGYZ), f(IGZZ), c, s, &
                        vout(IGXX), vout(IGXY), vout(IGXZ), vout(IGYY), vout(IGYZ), vout(IGZZ))
    call rotate_tensor(f(IAXX), f(IAXY), f(IAXZ), f(IAYY), f(IAYZ), f(IAZZ), c, s, &
                        vout(IAXX), vout(IAXY), vout(IAXZ), vout(IAYY), vout(IAYZ), vout(IAZZ))
  end function cartoon_ghost

  subroutine rotate_vec(Vx, Vy, c, s, Vxp, Vyp)
    real(dp), intent(in)  :: Vx, Vy, c, s
    real(dp), intent(out) :: Vxp, Vyp
    Vxp = c * Vx - s * Vy
    Vyp = s * Vx + c * Vy
  end subroutine rotate_vec

  subroutine rotate_tensor(Txx, Txy, Txz, Tyy, Tyz, Tzz, c, s, &
                            Txxp, Txyp, Txzp, Tyyp, Tyzp, Tzzp)
    real(dp), intent(in)  :: Txx, Txy, Txz, Tyy, Tyz, Tzz, c, s
    real(dp), intent(out) :: Txxp, Txyp, Txzp, Tyyp, Tyzp, Tzzp
    Txxp = c*c*Txx - 2.0_dp*c*s*Txy + s*s*Tyy
    Txyp = c*s*(Txx - Tyy) + (c*c - s*s)*Txy
    Txzp = c*Txz - s*Tyz
    Tyyp = s*s*Txx + 2.0_dp*c*s*Txy + c*c*Tyy
    Tyzp = s*Txz + c*Tyz
    Tzzp = Tzz
  end subroutine rotate_tensor

  ! Quadratic (3-point) Lagrange interpolation of all NVARS fields, at fixed
  ! row k, to target radius varpi along the stored x-grid.
  function interp_radial(u, k, varpi) result(fvals)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: k
    real(dp), intent(in) :: varpi
    real(dp) :: fvals(NVARS)
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
  end function interp_radial

end module cartoon_mod
