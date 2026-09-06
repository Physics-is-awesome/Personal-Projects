module tov_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: tov_profile_t, solve_tov, tov_lookup

  type :: tov_profile_t
    integer :: n
    real(dp), allocatable :: r(:)      ! Schwarzschild-like radius
    real(dp), allocatable :: m(:)      ! enclosed mass
    real(dp), allocatable :: p(:)      ! pressure
    real(dp), allocatable :: rho(:)    ! rest-mass density
    real(dp), allocatable :: Phi(:)    ! metric potential, alpha = exp(Phi)
    real(dp), allocatable :: riso(:)   ! isotropic radius (matched/rescaled)
    real(dp) :: R_star, M_star, riso_star
    real(dp) :: eos_K, eos_gamma
  end type tov_profile_t

contains

  ! rho_from_p: invert p = K rho^Gamma
  real(dp) function rho_from_p(p, K, Gamma) result(rho)
    real(dp), intent(in) :: p, K, Gamma
    if (p <= 0.0_dp) then
      rho = 0.0_dp
    else
      rho = (p/K)**(1.0_dp/Gamma)
    end if
  end function rho_from_p

  ! total energy density from p (polytrope: eps = p/((Gamma-1)rho), e = rho+rho*eps = rho+p/(Gamma-1))
  real(dp) function energy_from_p(p, K, Gamma) result(e)
    real(dp), intent(in) :: p, K, Gamma
    real(dp) :: rho
    rho = rho_from_p(p, K, Gamma)
    if (rho <= 0.0_dp) then
      e = 0.0_dp
    else
      e = rho + p/(Gamma - 1.0_dp)
    end if
  end function energy_from_p

  subroutine solve_tov(rho_c, K, Gamma, prof, rmax, npts)
    real(dp), intent(in) :: rho_c, K, Gamma
    type(tov_profile_t), intent(out) :: prof
    real(dp), intent(in) :: rmax
    integer,  intent(in) :: npts

    real(dp) :: dr, r, m, p, Phi, riso_raw
    real(dp) :: p_c
    integer :: i, isurf
    real(dp) :: k1m,k2m,k3m,k4m, k1p,k2p,k3p,k4p, k1f,k2f,k3f,k4f, k1r,k2r,k3r,k4r
    real(dp) :: p_floor_frac

    prof%eos_K = K; prof%eos_gamma = Gamma
    prof%n = npts
    allocate(prof%r(npts), prof%m(npts), prof%p(npts), prof%rho(npts), &
             prof%Phi(npts), prof%riso(npts))

    p_c = K*rho_c**Gamma
    p_floor_frac = 1.0e-10_dp

    dr = rmax/real(npts-1, dp)
    r = 0.0_dp; m = 0.0_dp; p = p_c; Phi = 0.0_dp; riso_raw = 1.0e-8_dp  ! tiny nonzero seed

    prof%r(1)=r; prof%m(1)=m; prof%p(1)=p; prof%rho(1)=rho_c; prof%Phi(1)=Phi; prof%riso(1)=riso_raw
    isurf = npts

    do i = 2, npts
      call derivs(r,       m,       p,       riso_raw,       k1m,k1p,k1f,k1r)
      call derivs(r+0.5_dp*dr, m+0.5_dp*dr*k1m, p+0.5_dp*dr*k1p, riso_raw+0.5_dp*dr*k1r, k2m,k2p,k2f,k2r)
      call derivs(r+0.5_dp*dr, m+0.5_dp*dr*k2m, p+0.5_dp*dr*k2p, riso_raw+0.5_dp*dr*k2r, k3m,k3p,k3f,k3r)
      call derivs(r+dr,       m+dr*k3m,       p+dr*k3p,       riso_raw+dr*k3r,       k4m,k4p,k4f,k4r)

      m = m + (dr/6.0_dp)*(k1m+2.0_dp*k2m+2.0_dp*k3m+k4m)
      p = p + (dr/6.0_dp)*(k1p+2.0_dp*k2p+2.0_dp*k3p+k4p)
      Phi = Phi + (dr/6.0_dp)*(k1f+2.0_dp*k2f+2.0_dp*k3f+k4f)
      riso_raw = riso_raw + (dr/6.0_dp)*(k1r+2.0_dp*k2r+2.0_dp*k3r+k4r)
      r = r + dr

      if (p <= p_floor_frac*p_c) then
        prof%r(i)=r; prof%m(i)=m; prof%p(i)=0.0_dp; prof%rho(i)=0.0_dp
        prof%Phi(i)=Phi; prof%riso(i)=riso_raw
        isurf = i
        exit
      end if

      prof%r(i)=r; prof%m(i)=m; prof%p(i)=p; prof%rho(i)=rho_from_p(p,K,Gamma)
      prof%Phi(i)=Phi; prof%riso(i)=riso_raw
      isurf = i
    end do

    prof%R_star = prof%r(isurf)
    prof%M_star = prof%m(isurf)
    prof%n = isurf

    ! Match Phi to exterior Schwarzschild: exp(2 Phi(R)) = 1 - 2M/R
    block
      real(dp) :: Phi_target, Phi_shift
      Phi_target = 0.5_dp*log(max(1.0_dp - 2.0_dp*prof%M_star/prof%R_star, 1.0e-12_dp))
      Phi_shift = Phi_target - prof%Phi(isurf)
      prof%Phi(1:isurf) = prof%Phi(1:isurf) + Phi_shift
    end block

    ! Match/rescale isotropic radius to the exterior isotropic relation.
    block
      real(dp) :: riso_matched, rescale
      riso_matched = 0.5_dp*( (prof%R_star - prof%M_star) &
                              + sqrt(max(prof%R_star**2 - 2.0_dp*prof%M_star*prof%R_star, 0.0_dp)) )
      rescale = riso_matched/prof%riso(isurf)
      prof%riso(1:isurf) = prof%riso(1:isurf)*rescale
      prof%riso_star = riso_matched
    end block

  contains

    subroutine derivs(rr, mm, pp, rr_iso, dmdr, dpdr, dPhidr, driso)
      real(dp), intent(in)  :: rr, mm, pp, rr_iso
      real(dp), intent(out) :: dmdr, dpdr, dPhidr, driso
      real(dp) :: ee, denom
      ee = energy_from_p(pp, K, Gamma)
      dmdr = 4.0_dp*3.14159265358979323846_dp*rr*rr*ee
      denom = rr*(rr - 2.0_dp*mm)
      if (rr < 1.0e-10_dp .or. denom <= 0.0_dp) then
        dpdr = 0.0_dp
        dPhidr = 0.0_dp
        driso = 1.0_dp  ! dr_iso/dr -> 1 as r->0 (regular center)
      else
        dpdr   = -(ee+pp)*(mm + 4.0_dp*3.14159265358979323846_dp*rr**3*pp)/denom
        dPhidr =  (mm + 4.0_dp*3.14159265358979323846_dp*rr**3*pp)/denom
        driso  = rr_iso/(rr*sqrt(max(1.0_dp - 2.0_dp*mm/rr, 1.0e-14_dp)))
      end if
    end subroutine derivs

  end subroutine solve_tov

  ! Interpolates the TOV profile at a given isotropic radius riso, returning
  ! rho, p, eps, alpha (=exp(Phi)), and the conformal factor psi (gamma_ij =
  ! psi^4 delta_ij in isotropic coordinates). For riso beyond the star,
  ! returns vacuum (rho=p=0) with the exact exterior Schwarzschild-isotropic
  ! alpha and psi.
  subroutine tov_lookup(prof, riso, rho, p, eps, alpha, psi)
    type(tov_profile_t), intent(in) :: prof
    real(dp), intent(in)  :: riso
    real(dp), intent(out) :: rho, p, eps, alpha, psi
    integer :: lo, hi, mid
    real(dp) :: frac, rsch, Phi_here

    if (riso >= prof%riso_star) then
      rho = 0.0_dp; p = 0.0_dp; eps = 0.0_dp
      psi = 1.0_dp + prof%M_star/(2.0_dp*riso)
      alpha = (1.0_dp - prof%M_star/(2.0_dp*riso)) / (1.0_dp + prof%M_star/(2.0_dp*riso))
      return
    end if

    ! binary search for the bracketing interior table interval in riso
    lo = 1; hi = prof%n
    do while (hi - lo > 1)
      mid = (lo+hi)/2
      if (prof%riso(mid) < riso) then
        lo = mid
      else
        hi = mid
      end if
    end do
    if (prof%riso(hi) > prof%riso(lo)) then
      frac = (riso - prof%riso(lo)) / (prof%riso(hi) - prof%riso(lo))
    else
      frac = 0.0_dp
    end if
    frac = min(max(frac, 0.0_dp), 1.0_dp)

    rsch     = prof%r(lo)   + frac*(prof%r(hi)   - prof%r(lo))
    p        = prof%p(lo)   + frac*(prof%p(hi)   - prof%p(lo))
    rho      = prof%rho(lo) + frac*(prof%rho(hi) - prof%rho(lo))
    Phi_here = prof%Phi(lo) + frac*(prof%Phi(hi) - prof%Phi(lo))

    if (p < 0.0_dp) p = 0.0_dp
    if (rho > 0.0_dp) then
      eps = p/((prof%eos_gamma - 1.0_dp)*rho)
    else
      eps = 0.0_dp
    end if
    alpha = exp(Phi_here)
    psi = sqrt(max(rsch,1.0e-300_dp)/max(riso,1.0e-300_dp))
  end subroutine tov_lookup

end module tov_mod
