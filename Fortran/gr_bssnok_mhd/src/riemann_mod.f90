module riemann_mod
  use kinds_mod, only: dp
  use eos_mod, only: pressure, sound_speed_sq
  use hydro_vars_mod, only: NPRIM, NCONS, IRHO, IVX, IVY, IVZ, IEPS, ID, ISX, ISY, ISZ, ITAU
  implicit none
  private
  public :: prim_to_cons, physical_flux, char_speeds_v2, hlle_flux

contains

  subroutine prim_to_cons(prim, gamma_ij, cons)
    real(dp), intent(in)  :: prim(NPRIM), gamma_ij(3,3)
    real(dp), intent(out) :: cons(NCONS)
    real(dp) :: rho, vx, vy, vz, eps, p, v2, W, h
    real(dp) :: vlow(3), vup(3)

    rho = prim(IRHO); vx = prim(IVX); vy = prim(IVY); vz = prim(IVZ); eps = prim(IEPS)
    vup = (/ vx, vy, vz /)
    vlow = matmul(gamma_ij, vup)
    v2 = sum(vup*vlow)
    v2 = min(v2, 1.0_dp - 1.0e-12_dp)
    W = 1.0_dp/sqrt(max(1.0_dp - v2, 1.0e-14_dp))
    p = pressure(rho, eps)
    h = 1.0_dp + eps + p/max(rho,1.0e-300_dp)

    cons(ID)   = rho*W
    cons(ISX)  = rho*h*W*W*vlow(1)
    cons(ISY)  = rho*h*W*W*vlow(2)
    cons(ISZ)  = rho*h*W*W*vlow(3)
    cons(ITAU) = rho*h*W*W - p - rho*W
  end subroutine prim_to_cons

  ! Un-densitized physical flux F^dir (dir = 1,2,3 for x,y,z), given
  ! primitives and the local lapse/shift/metric.
  subroutine physical_flux(prim, alpha, beta_up, gamma_ij, dir, flux)
    real(dp), intent(in)  :: prim(NPRIM), alpha, beta_up(3), gamma_ij(3,3)
    integer,  intent(in)  :: dir
    real(dp), intent(out) :: flux(NCONS)
    real(dp) :: rho, vdir, eps, p
    real(dp) :: cons(NCONS), vhat

    rho = prim(IRHO); eps = prim(IEPS)
    vdir = prim(IVX-1+dir)
    p = pressure(rho, eps)
    call prim_to_cons(prim, gamma_ij, cons)
    vhat = alpha*vdir - beta_up(dir)

    flux(ID)   = cons(ID)*vhat
    flux(ISX)  = cons(ISX)*vhat
    flux(ISY)  = cons(ISY)*vhat
    flux(ISZ)  = cons(ISZ)*vhat
    flux(ISX-1+dir) = flux(ISX-1+dir) + alpha*p
    flux(ITAU) = cons(ITAU)*vhat + alpha*p*vdir
  end subroutine physical_flux

  ! Extremal characteristic speeds (coordinate-frame) along direction dir,
  subroutine char_speeds_v2(prim, alpha, beta_dir, gii, v2, dir, lmin, lmax)
    real(dp), intent(in)  :: prim(NPRIM), alpha, beta_dir, gii, v2
    integer,  intent(in)  :: dir
    real(dp), intent(out) :: lmin, lmax
    real(dp) :: rho, eps, a2, vdir, num, disc

    rho = prim(IRHO); eps = prim(IEPS)
    a2 = sound_speed_sq(rho, eps)
    vdir = prim(IVX-1+dir)

    num = vdir*(1.0_dp - a2)
    disc = a2*(1.0_dp-v2)*( gii*(1.0_dp - v2*a2) - vdir*vdir*(1.0_dp-a2) )
    disc = max(disc, 0.0_dp)
    lmax = alpha*( num + sqrt(disc) )/(1.0_dp - v2*a2) - beta_dir
    lmin = alpha*( num - sqrt(disc) )/(1.0_dp - v2*a2) - beta_dir
  end subroutine char_speeds_v2

  ! HLLE flux given left/right primitive states and the (shared, face)
  ! metric. v2L, v2R are the full 3-velocity-squared for each state
  ! (gamma_ij v^i v^j at the face metric), passed by the caller.
  subroutine hlle_flux(primL, primR, v2L, v2R, alpha, beta_up, gamma_ij, gii, dir, flux)
    real(dp), intent(in)  :: primL(NPRIM), primR(NPRIM), v2L, v2R
    real(dp), intent(in)  :: alpha, beta_up(3), gamma_ij(3,3), gii
    integer,  intent(in)  :: dir
    real(dp), intent(out) :: flux(NCONS)
    real(dp) :: lminL, lmaxL, lminR, lmaxR, lm, lp
    real(dp) :: FL(NCONS), FR(NCONS), UL(NCONS), UR(NCONS)

    call char_speeds_v2(primL, alpha, beta_up(dir), gii, v2L, dir, lminL, lmaxL)
    call char_speeds_v2(primR, alpha, beta_up(dir), gii, v2R, dir, lminR, lmaxR)

    lp = max(0.0_dp, lmaxL, lmaxR)
    lm = min(0.0_dp, lminL, lminR)

    call physical_flux(primL, alpha, beta_up, gamma_ij, dir, FL)
    call physical_flux(primR, alpha, beta_up, gamma_ij, dir, FR)
    call prim_to_cons(primL, gamma_ij, UL)
    call prim_to_cons(primR, gamma_ij, UR)

    if (lp - lm < 1.0e-14_dp) then
      flux = 0.5_dp*(FL+FR)
    else
      flux = (lp*FL - lm*FR + lp*lm*(UR-UL)) / (lp - lm)
    end if
  end subroutine hlle_flux

end module riemann_mod
