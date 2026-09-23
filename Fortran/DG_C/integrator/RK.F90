!=======================================================================
!  mod_timeint : SSP-RK time discretisations (Sec. 3.5) + diagnostics
!
!  Second order:
!     Y^(1) = Y^n + dt G(Y^n, t^n)
!     Y^n+1 = 1/2 Y^n + 1/2 ( Y^(1) + dt G(Y^(1), t^n + dt) )
!
!  Third order:
!     Y^(1) = Y^n + dt G(Y^n, t^n)
!     Y^(2) = 3/4 Y^n + 1/4 ( Y^(1) + dt G(Y^(1), t^n + dt) )
!     Y^n+1 = 1/3 Y^n + 2/3 ( Y^(2) + dt G(Y^(2), t^n + dt/2) )
!
!  with G = L, Y = U_h for the standard scheme and G = H, Y = W_h for
!  the structure-preserving scheme.
!
!  When the OE technique is on, every stage output is post-processed
!  as in Sec. 3.6:
!     W~^(l) = (stage combination),   W^(l) = W^e_h + F_dt( W~^(l) - W^e_h )
!
!  Time step (Sec. 5):
!     dt = CFL * h / max_K ( ||ubar_K||_inf + cbar_K )
!=======================================================================
module mod_timeint
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics
  use mod_poisson
  use mod_dg
  use mod_limiter
  implicit none
  private
  public :: ti_alloc, ti_free, ti_step, ti_dt, total_mass, total_energy, &
            errors, diagnostics

  real(dp), allocatable :: y1(:,:,:), y2(:,:,:), rhs(:,:,:), Uw(:,:,:)

contains

  subroutine ti_alloc()
    call ti_free()
    allocate(y1(nb,nvar,ncell), y2(nb,nvar,ncell), rhs(nb,nvar,ncell))
    allocate(Uw(nb,nvar,ncell))
  end subroutine ti_alloc

  subroutine ti_free()
    if (allocated(y1)) deallocate(y1, y2, rhs, Uw)
  end subroutine ti_free

  !---------------------------------------------------------------------
  real(dp) function ti_dt(Y) result(dt)
    real(dp), intent(in) :: Y(nb,nvar,ncell)
    real(dp) :: alp
    call dg_recover_U(Y, Uw)
    alp = dg_max_speed(Uw)
    if (alp <= zero) alp = 1.0e-12_dp
    dt = cfl*hglob/alp
  end function ti_dt

  !---------------------------------------------------------------------
  !  one SSP-RK step;  Y is U_h ('std') or W_h ('sp')
  !---------------------------------------------------------------------
  subroutine ti_step(Y, t, dt)
    real(dp), intent(inout) :: Y(nb,nvar,ncell)
    real(dp), intent(in)    :: t, dt

    select case (rk_order)

    case (1)
       call dg_residual(Y, t, rhs)
       Y = Y + dt*rhs
       call post_stage(Y, dt)

    case (2)
       call dg_residual(Y, t, rhs)
       y1 = Y + dt*rhs
       call post_stage(y1, dt)

       call dg_residual(y1, t + dt, rhs)
       Y = half*Y + half*(y1 + dt*rhs)
       call post_stage(Y, dt)

    case default
       call dg_residual(Y, t, rhs)
       y1 = Y + dt*rhs
       call post_stage(y1, dt)

       call dg_residual(y1, t + dt, rhs)
       y2 = 0.75_dp*Y + 0.25_dp*(y1 + dt*rhs)
       call post_stage(y2, dt)

       call dg_residual(y2, t + half*dt, rhs)
       Y = (one/three)*Y + (two/three)*(y2 + dt*rhs)
       call post_stage(Y, dt)

    end select
  end subroutine ti_step

  !---------------------------------------------------------------------
  !  OE damping about the equilibrium (3.39) followed by the PP limiter.
  !  The PP limiter must act on U, so for the SP scheme we convert,
  !  limit, and convert back with the same projector P.
  !---------------------------------------------------------------------
  subroutine post_stage(Y, dt)
    real(dp), intent(inout) :: Y(nb,nvar,ncell)
    real(dp), intent(in)    :: dt
    real(dp), allocatable   :: Wref(:,:,:)

    if (use_oe) then
       allocate(Wref(nb,nvar,ncell))
       if (trim(scheme) == 'sp') then
          Wref = Weq
       else
          Wref = zero
          Wref(:,IRHO,:) = rhoe
          Wref(:,IEN,:)  = pe/(gam - one)
       end if
       call oe_damping(Y, Wref, dt)
       deallocate(Wref)
    end if

    if (use_pp) then
       call dg_recover_U(Y, Uw)
       call pp_limiter(Uw)
       if (trim(scheme) == 'sp') then
          call dg_W_from_U(Uw, Uw(:,IRHO,:), phi_h, Y)
       else
          Y = Uw
       end if
     end if

  end subroutine post_stage

  !=====================================================================
  !                           diagnostics
  !=====================================================================

  real(dp) function total_mass(Y) result(mass)
    real(dp), intent(in) :: Y(nb,nvar,ncell)
    integer :: ic
    mass = zero
    do ic = 1, ncell
       mass = mass + Y(1,IRHO,ic)*cvol
    end do
  end function total_mass

  !---------------------------------------------------------------------
  !  int_Omega E_tot dx.  For the SP scheme E_tot is evolved directly;
  !  for the standard scheme it is reconstructed as E + 1/2 rho phi
  !  (or E + 1/2 (rho-rho0) phi for the Jeans problem, Example 5.5).
  !---------------------------------------------------------------------
  real(dp) function total_energy(Y) result(etot)
    real(dp), intent(in) :: Y(nb,nvar,ncell)
    integer  :: ic, q
    real(dp) :: e, r, ph, r0
    if (trim(scheme) == 'sp') then
       etot = zero
       do ic = 1, ncell
          etot = etot + Y(1,IEN,ic)*cvol
       end do
       return
    end if
    call dg_recover_U(Y, Uw)
    etot = zero
    do ic = 1, ncell
       do q = 1, nqv
          e  = eval_v(Uw(:,IEN,ic),  q)
          r  = eval_v(Uw(:,IRHO,ic), q)
          ph = eval_v(phi_h(:,ic),   q)
          r0 = zero
          if (jeans_delta) r0 = eval_v(rhoe(:,ic), q)
          etot = etot + cvol*wv(q)*(e + half*(r - r0)*ph)
          print *, " total energy"
          print *, etot
        end do
    end do
  end function total_energy

  !---------------------------------------------------------------------
  !  L1, L2, Linf errors of one component of U against the exact data
  !---------------------------------------------------------------------
  subroutine errors(Y, t, ivar, e1, e2, ei)
    real(dp), intent(in)  :: Y(nb,nvar,ncell)
    real(dp), intent(in)  :: t
    integer,  intent(in)  :: ivar
    real(dp), intent(out) :: e1, e2, ei
    integer  :: ic, q
    real(dp) :: x(3), ue(nvar), uh, d, phi
    call dg_recover_U(Y, Uw)
    e1 = zero ; e2 = zero ; ei = zero
    do ic = 1, ncell
       do q = 1, nqv
          call ref2phys(ic, xv(:,q), x)
          call exact_state(x, t, ue, phi)
          uh = eval_v(Uw(:,ivar,ic), q)
          d  = abs(uh - ue(ivar))
          e1 = e1 + cvol*wv(q)*d
          e2 = e2 + cvol*wv(q)*d*d
          ei = max(ei, d)
       end do
    end do
    e2 = sqrt(e2)
  end subroutine errors

  !---------------------------------------------------------------------
  !  error of the potential against phi_exact
  !---------------------------------------------------------------------
  subroutine errors_phi(t, e1, e2, ei)
    real(dp), intent(in)  :: t
    real(dp), intent(out) :: e1, e2, ei
    integer  :: ic, q
    real(dp) :: x(3), ue(nvar), d, phi, phh
    e1 = zero ; e2 = zero ; ei = zero
    do ic = 1, ncell
       do q = 1, nqv
          call ref2phys(ic, xv(:,q), x)
          call exact_state(x, t, ue, phi)
          phh = eval_v(phi_h(:,ic), q)
          d   = abs(phh - phi)
          e1 = e1 + cvol*wv(q)*d
          e2 = e2 + cvol*wv(q)*d*d
          ei = max(ei, d)
       end do
    end do
    e2 = sqrt(e2)
  end subroutine errors_phi

  !---------------------------------------------------------------------
  subroutine diagnostics(Y, t, m0, e0)
    real(dp), intent(in) :: Y(nb,nvar,ncell)
    real(dp), intent(in) :: t, m0, e0
    real(dp) :: m, e
    m = total_mass(Y)
    e = total_energy(Y)
    write(*,'(a,es12.5,a,es12.5,a,es12.5,a,es12.5)')                      &
         '   t = ', t,                                                    &
         '   M = ', m,                                                    &
         '   dM = ', m - m0,                                              &
         '   dE_tot = ', e - e0
  end subroutine diagnostics

end module mod_timeint
