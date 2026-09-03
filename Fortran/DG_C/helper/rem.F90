!=======================================================================
!  mod_riemann : the multi-dimensional HLLC flux of Sec. 3.2 and the
!                modified (well-balanced) HLLC flux of Sec. 3.4.1
!
!  F^hllc(U_L,U_R;n) =  F(U_L)                     if 0 <= S_L
!                       F_*L = F_L + S_L(U_*L-U_L) if S_L <= 0 <= S_*
!                       F_*R = F_R + S_R(U_*R-U_R) if S_* <= 0 <= S_R
!                       F(U_R)                     if 0 >= S_R      (3.4)
!
!  with  S_L = min{uh_L - c_L, uh_R - c_R},  S_R = max{uh_L+c_L, uh_R+c_R},
!        S_* = (p_R-p_L+rho_L uh_L(S_L-uh_L)-rho_R uh_R(S_R-uh_R))
!              / (rho_L(S_L-uh_L) - rho_R(S_R-uh_R))                (3.5)
!
!        U_*i = rho_i (S_i-uh_i)/(S_i-S_*) *
!               ( 1, u_i (with the n-component replaced by S_*),
!                 E_i/rho_i + (S_*-uh_i)(S_* + p_i/(rho_i(S_i-uh_i))) )^T
!
!  Lemma 3.1:  for U_L = (rho_L,0,p/(g-1)), U_R = (rho_R,0,p/(g-1))
!              F^hllc = (0, p n^T, 0)^T.
!
!  Modified flux (3.20):
!     Fhat = F^hllc( (p^{e,*}/p^{e,int}) U^int , (p^{e,*}/p^{e,ext}) U^ext ; n )
!     p^{e,*} = 1/2 ( p^{e,int} + p^{e,ext} ).
!  At the discrete equilibrium both rescaled states carry the SAME
!  pressure p^{e,*} and zero velocity, so Lemma 3.1 gives
!     Fhat = (0, p^{e,*} n^T, 0)^T   and in particular fhat^[1] = 0,
!  which is exactly what the proof of Theorem 4.1 needs.
!
!  ---------------------------------------------------------------------
!  FLOATING-POINT NOTE (important for Theorem 4.1)
!
!  Theorem 4.1 requires fhat^[1] = 0 and fhat^[3] = 0 *identically* at
!  the discrete equilibrium, so that rho and E_tot do not drift at all.
!  In exact arithmetic that follows from Lemma 3.1.  In floating point it
!  depends on how the star state is associated.  At equilibrium uh = 0
!  and S_* = 0, so the factor is
!
!     rho_L (S_L - uh_L)/(S_L - S_*)  =  rho_L S_L / S_L .
!
!  Evaluated left to right as  (rho_L*S_L)/S_L  this is NOT bit-identical
!  to rho_L, leaving fhat^[1] = S_L*(fac - rho_L) = O(eps) ~ 1e-15.  We
!  therefore form the ratio FIRST,
!
!     rat = (S_L - uh_L)/(S_L - S_*)  ->  S_L/S_L = 1 exactly,
!     fac = rho_L * rat               ->  rho_L    exactly,
!
!  and likewise carry the energy as  E_L*rat + fac*(...)  so that the
!  first term returns E_L exactly when rat == 1.  Measured over 2x10^4
!  random equilibrium face states this changes
!
!        component        left-to-right      ratio-first
!        fhat^[1]  (2D)      6.4e-15            0.0
!        fhat^[3]  (2D)      3.4e-15            0.0
!        fhat^[1]  (3D)      8.9e-15            2.0e-16
!        fhat^[3]  (3D)      3.4e-15            1.0e-16
!
!  and leaves the flux unchanged for general states to 2e-14 relative.
!  The small 3D remainder comes from the rescaling p^{e,*}/p^{e,int}
!  itself, which can perturb the recovered pressure by one ulp so that
!  S_* is O(eps) rather than exactly zero; this is the same 1e-16 level
!  the paper reports in Table 5.9.
!=======================================================================
module mod_riemann
  use mod_kinds
  use mod_params
  use mod_physics
  implicit none
  private
  public :: hllc_flux, hllc_flux_wb, max_speed, spectral_radius_n

contains

  !---------------------------------------------------------------------
  pure real(dp) function max_speed(u, m) result(a)
    real(dp), intent(in) :: u(nvar)
    integer,  intent(in) :: m
    a = abs(u(IMX+m-1)/u(IRHO)) + soundspeed(u)
  end function max_speed

  !---------------------------------------------------------------------
  !  spectral radius of sum_i n_i dF_i/dU  =  |u.n| + c    (Sec. 3.6)
  !---------------------------------------------------------------------
  pure real(dp) function spectral_radius_n(u, n) result(b)
    real(dp), intent(in) :: u(nvar), n(3)
    real(dp) :: rho, vel(3), p, un
    call cons2prim(u, rho, vel, p)
    un = sum(vel(1:nd)*n(1:nd))
    b  = abs(un) + sqrt(max(gam*p/rho, zero))
  end function spectral_radius_n

  !=====================================================================
  !  the plain HLLC flux across a face whose unit normal is +e_m or -e_m.
  !  We pass the direction index m and the sign sgn = n.e_m = +-1;
  !  on a tensor mesh this is equivalent to the general normal form.
  !=====================================================================
  pure subroutine hllc_flux(ul, ur, m, sgn, f)
    real(dp), intent(in)  :: ul(nvar), ur(nvar)
    integer,  intent(in)  :: m
    real(dp), intent(in)  :: sgn
    real(dp), intent(out) :: f(nvar)
    real(dp) :: fl(nvar), fr(nvar), us(nvar)
    real(dp) :: rl, rr, pl, pr, vl(3), vr(3), cl, cr
    real(dp) :: uhl, uhr, sl, sr, sm, rat, fac, den
    integer  :: r

    call cons2prim(ul, rl, vl, pl)
    call cons2prim(ur, rr, vr, pr)

    ! normal velocities  uh = u.n  with n = sgn e_m
    uhl = sgn*vl(m)
    uhr = sgn*vr(m)
    cl  = sqrt(max(gam*pl/rl, zero))
    cr  = sqrt(max(gam*pr/rr, zero))

    sl = min(uhl - cl, uhr - cr)
    sr = max(uhl + cl, uhr + cr)

    ! F(U).n  =  sgn * F_m(U)
    call phys_flux(ul, m, fl) ; fl = sgn*fl
    call phys_flux(ur, m, fr) ; fr = sgn*fr

    if (zero <= sl) then
       f = fl
       return
    else if (zero >= sr) then
       f = fr
       return
    end if

    den = rl*(sl - uhl) - rr*(sr - uhr)
    sm  = (pr - pl + rl*uhl*(sl - uhl) - rr*uhr*(sr - uhr))/den

    if (zero <= sm) then
       !  ratio FIRST so that rat == 1 exactly when uhl == sm  (see header)
       rat = (sl - uhl)/(sl - sm)
       fac = rl*rat
       us(IRHO) = fac
       do r = 1, nd
          us(IMX+r-1) = fac*vl(r)
       end do
       ! replace the normal component by S_* :  u_* = u - (uh - S_*) n
       us(IMX+m-1) = us(IMX+m-1) - fac*(uhl - sm)*sgn
       ! E_L*rat returns E_L exactly when rat == 1
       us(IEN) = ul(IEN)*rat + fac*( (sm - uhl)*(sm + pl/(rl*(sl - uhl))) )
       f = fl + sl*(us - ul)
    else
       rat = (sr - uhr)/(sr - sm)
       fac = rr*rat
       us(IRHO) = fac
       do r = 1, nd
          us(IMX+r-1) = fac*vr(r)
       end do
       us(IMX+m-1) = us(IMX+m-1) - fac*(uhr - sm)*sgn
       us(IEN) = ur(IEN)*rat + fac*( (sm - uhr)*(sm + pr/(rr*(sr - uhr))) )
       f = fr + sr*(us - ur)
    end if
  end subroutine hllc_flux

  !=====================================================================
  !  the modified HLLC flux of (3.20)
  !=====================================================================
  pure subroutine hllc_flux_wb(ul, ur, pel, per, m, sgn, f)
    real(dp), intent(in)  :: ul(nvar), ur(nvar)   ! U^int , U^ext
    real(dp), intent(in)  :: pel, per             ! p^{e,int} , p^{e,ext}
    integer,  intent(in)  :: m
    real(dp), intent(in)  :: sgn
    real(dp), intent(out) :: f(nvar)
    real(dp) :: pstar, wl(nvar), wr(nvar)

    pstar = half*(pel + per)
    wl = (pstar/pel)*ul
    wr = (pstar/per)*ur
    call hllc_flux(wl, wr, m, sgn, f)
  end subroutine hllc_flux_wb

end module mod_riemann
