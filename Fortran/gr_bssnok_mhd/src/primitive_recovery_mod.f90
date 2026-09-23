module primitive_recovery_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: con2prim, rho_floor, p_floor

  real(dp) :: rho_floor = 1.0e-10_dp
  real(dp) :: p_floor   = 1.0e-12_dp

contains

  ! Given conserved (D, Sx, Sy, Sz, tau) and the local physical inverse
  ! metric gammainv(3,3), recovers primitives (rho, vx, vy, vz, eps).
  ! Returns ok=.false. if recovery failed (caller should apply an
  ! atmosphere floor at that point).
  subroutine con2prim(D, Sx, Sy, Sz, tau, gammainv, rho, vx, vy, vz, eps, ok)
    real(dp), intent(in)  :: D, Sx, Sy, Sz, tau, gammainv(3,3)
    real(dp), intent(out) :: rho, vx, vy, vz, eps
    logical,  intent(out) :: ok

    real(dp) :: Slow(3), S2, Q
    real(dp) :: plo, phi, flo, fhi, pmid, fmid
    real(dp) :: v2, W, h, p
    integer :: it
    integer, parameter :: maxit = 60
    real(dp), parameter :: tol = 1.0e-12_dp

    Slow = (/ Sx, Sy, Sz /)
    S2 = 0.0_dp
    block
      integer :: a, b
      do a = 1, 3
        do b = 1, 3
          S2 = S2 + gammainv(a,b)*Slow(a)*Slow(b)
        end do
      end do
    end block

    Q = tau + D
    ok = .true.

    if (D <= 0.0_dp .or. Q <= 0.0_dp) then
      ok = .false.
    end if

    if (ok) then
      plo = 1.0e-15_dp
      phi = 10.0_dp*max(Q, 1.0_dp) + 10.0_dp

      flo = resid(plo)
      fhi = resid(phi)

      if (flo*fhi > 0.0_dp) then
        ! Bracket failed (can happen right at/near excised, unphysical, or
        ! super-extreme states) -- fall back to an atmosphere floor.
        ok = .false.
      end if
    end if

    if (.not. ok) then
      rho = rho_floor; eps = eps_from_rho_p(rho_floor, p_floor)
      vx = 0.0_dp; vy = 0.0_dp; vz = 0.0_dp
      return
    end if

    do it = 1, maxit
      pmid = 0.5_dp*(plo+phi)
      fmid = resid(pmid)
      if (abs(fmid) < tol .or. 0.5_dp*(phi-plo) < tol) exit
      if (flo*fmid <= 0.0_dp) then
        phi = pmid; fhi = fmid
      else
        plo = pmid; flo = fmid
      end if
    end do

    p = pmid
    v2 = min(S2/(Q+p)**2, 1.0_dp - 1.0e-12_dp)
    W = 1.0_dp/sqrt(max(1.0_dp - v2, 1.0e-14_dp))
    rho = max(D/W, rho_floor)
    h = (Q+p)/(D*W)
    eps = max(h - 1.0_dp - p/rho, 1.0e-14_dp)

    if (S2 > 0.0_dp) then
      vx = gammainv(1,1)*Slow(1)+gammainv(1,2)*Slow(2)+gammainv(1,3)*Slow(3)
      vy = gammainv(2,1)*Slow(1)+gammainv(2,2)*Slow(2)+gammainv(2,3)*Slow(3)
      vz = gammainv(3,1)*Slow(1)+gammainv(3,2)*Slow(2)+gammainv(3,3)*Slow(3)
      vx = vx / (Q+p); vy = vy / (Q+p); vz = vz / (Q+p)
    else
      vx = 0.0_dp; vy = 0.0_dp; vz = 0.0_dp
    end if

  contains

    real(dp) function resid(ptrial) result(f)
      real(dp), intent(in) :: ptrial
      real(dp) :: v2l, Wl, rhol, hl, epsl, ptry
      v2l = min(S2/(Q+ptrial)**2, 1.0_dp - 1.0e-12_dp)
      Wl = 1.0_dp/sqrt(max(1.0_dp - v2l, 1.0e-14_dp))
      rhol = max(D/Wl, 1.0e-300_dp)
      hl = (Q+ptrial)/(D*Wl)
      epsl = hl - 1.0_dp - ptrial/rhol
      if (epsl < 0.0_dp) epsl = 0.0_dp
      ptry = pressure(rhol, epsl)
      f = ptrial - ptry
    end function resid

  end subroutine con2prim

end module primitive_recovery_mod
