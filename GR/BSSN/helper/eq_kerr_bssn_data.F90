!==============================================================================
! eq_kerr_bssn_data -- BSSN initial data for the stationary Kerr trumpet.
!
! This is the route-C data set: the analytic DBM vacuum Kerr family in
! helper/eq_kerr_trumpet.F90, converted to BSSN variables.  See the MEASURED
! RESULT block at the top of that module before changing anything -- in
! particular, this family is NOT 1+log and NOT maximal, and it is stationary
! only under a tabulated Bona-Masso gauge f(alpha) (slicing_mode 3).
!
! The spatial metric, lapse and shift come straight from the closed form at
! arbitrary Cartesian coordinates, so derivatives are taken with a FIXED step
! h (independent of dx).  That is deliberate:
!
!   * It removes the discretisation from the data, so the residual of the full
!     BSSN RHS on this data is a clean measure of whether the DATA and the GAUGE
!     are consistent, rather than a mix of the two.  That distinction is what
!     cracked the momentum-constraint plateau, where every per-term scan failed
!     but a comparison against an independent formulation succeeded.
!   * It costs 7 evaluations of the closed form per grid point instead of
!     stencil work, which is irrelevant at these sizes.
!
! K is built from the validated gauge relation
!
!     K_ij = (1 / (2 alpha)) [ beta^k d_k gamma_ij
!                            + gamma_jk d_i beta^k + gamma_ik d_j beta^k ]
!
! i.e. the LIE derivative of gamma, not the divergence form.  The divergence
! form silently gives the wrong answer here; helper/test_k_from_gauge.F90
! validates this one term by term.
!
! det(gamma-tilde) = 1 is imposed as in the rest of the code (conformal
! factor mode 0), so phi = ln det(gamma) / 12.
!==============================================================================
module eq_kerr_bssn_data_mod
  use kinds_mod, only: dp
  use vars_mod
  use params_mod, only: bh_mass, bh_spin
  use grid_mod, only: xg, zg
  use eq_kerr_trumpet_mod, only: ktr_kerr_xyz, ktr_trumpet_valid

  implicit none
  private
  public :: set_eq_kerr_trumpet_id, kerr_bssn_deriv_step, state_at

  ! Fixed derivative step, independent of the grid spacing.  Central
  ! differences have error ~ eps/h + h^2/6, so this is near the double-
  ! precision optimum and small h makes it WORSE, not better.
  real(dp), parameter :: HSTEP = 1.0e-4_dp

contains

  pure real(dp) function kerr_bssn_deriv_step() result(h)
    h = HSTEP
  end function kerr_bssn_deriv_step

  !--------------------------------------------------------------------------
  ! Evaluate the BSSN state at one Cartesian point, with exact-enough
  ! derivatives.  ok = .false. means the point is inside the trumpet, where the
  ! closed form is singular.
  !--------------------------------------------------------------------------
  ! reason: 0 = ok, 1 = closed form reports outside its domain (trumpet),
  !         2 = det(gamma) <= 0 (degenerate metric -- NOT expected anywhere).
  ! The two are separate failures and must not be collapsed into one flag.
  subroutine state_at(x, y, z, m, a, r0, phi, gt, at, Ktr, alpha, beta, ok, reason, det_out)
    real(dp), intent(in)  :: x, y, z, m, a, r0
    real(dp), intent(out) :: phi, gt(3,3), at(3,3), Ktr, alpha, beta(3)
    logical, intent(out)   :: ok
    integer, intent(out), optional :: reason
    real(dp), intent(out), optional :: det_out
    real(dp) :: g(3,3), ginv(3,3), detg, e4m
    real(dp) :: gp(3,3), gm(3,3), ap, am, bp(3), bm(3)
    real(dp) :: dg(3,3,3), dbeta(3,3), Lg(3,3), Kij(3,3)
    real(dp) :: xp, xm, yp, ym, zp, zm, det2
    logical :: ok2
    integer :: dir, p, q

    if (present(reason)) reason = 0
    if (present(det_out)) det_out = 0.0_dp     ! never leave it stale on failure
    call ktr_kerr_xyz(x, y, z, m, a, r0, alpha, beta, g, ok)
    if (.not. ok) then
      if (present(reason)) reason = 1
      return
    end if

    ! ---- derivatives by fixed-step central differences
    do dir = 1, 3
      xp = x; xm = x; yp = y; ym = y; zp = z; zm = z
      select case (dir)
      case (1); xp = x + HSTEP;  xm = x - HSTEP
      case (2); yp = y + HSTEP;  ym = y - HSTEP
      case (3); zp = z + HSTEP;  zm = z - HSTEP
      end select
      call ktr_kerr_xyz(xp, yp, zp, m, a, r0, ap, bp, gp, ok2)
      call ktr_kerr_xyz(xm, ym, zm, m, a, r0, am, bm, gm, ok2)
      do p = 1, 3
        do q = 1, 3
          dg(dir,p,q) = (gp(p,q) - gm(p,q))/(2.0_dp*HSTEP)
        end do
      end do
      dbeta(:,dir) = (bp(:) - bm(:))/(2.0_dp*HSTEP)
    end do

    ! ---- K_ij = L_beta gamma_ij / (2 alpha), LIE form
    Lg = 0.0_dp
    do p = 1, 3
      do q = 1, 3
        Lg(p,q) = sum(beta*dg(:,p,q)) &
                 + sum(g(q,:)*dbeta(p,:)) + sum(g(p,:)*dbeta(q,:))
      end do
    end do
    Kij = Lg/(2.0_dp*alpha)

    ! ---- conformally flatten, det(gamma-tilde) = 1
    detg = g(1,1)*(g(2,2)*g(3,3)-g(2,3)*g(3,2)) &
         - g(1,2)*(g(1,2)*g(3,3)-g(1,3)*g(2,3)) &
         + g(1,3)*(g(1,2)*g(2,3)-g(1,3)*g(2,2))
    if (detg <= 0.0_dp) then
      ok = .false.
      if (present(reason)) reason = 2
      return
    end if
    if (present(det_out)) det_out = detg
    phi = log(detg)/12.0_dp
    e4m = exp(-4.0_dp*phi)
    gt = e4m*g
    call inv3(g, ginv, det2)
    Ktr = sum(ginv*Kij)
    at = e4m*(Kij - g*Ktr/3.0_dp)
  end subroutine state_at

  !--------------------------------------------------------------------------
  ! Fill a BSSN state array on the grid.
  !--------------------------------------------------------------------------
  subroutine set_eq_kerr_trumpet_id(u, m, a, r0)
    real(dp), intent(out) :: u(:,:,:)
    real(dp), intent(in), optional :: m, a, r0
    real(dp) :: mm, aa, rr, phi, gt(3,3), at(3,3), Ktr, alpha, beta(3)
    logical :: ok
    integer :: i, k, nbad, nbad1, nbad2, rsn, iw, kw, nfar
    real(dp) :: rr2, rmin1, rmax1, rmin2, rmax2

    mm = bh_mass;  if (present(m)) mm = m
    aa = bh_spin;  if (present(a)) aa = a
    rr = mm;       if (present(r0)) rr = r0

    u = 0.0_dp
    nbad = 0; nbad1 = 0; nbad2 = 0; nfar = 0
    rmin1 = huge(1.0_dp); rmax1 = -huge(1.0_dp)
    rmin2 = huge(1.0_dp); rmax2 = -huge(1.0_dp)
    do k = 1, size(u,2)
      do i = 1, size(u,1)
        call state_at(xg(i), 0.0_dp, zg(k), mm, aa, rr, &
                      phi, gt, at, Ktr, alpha, beta, ok, rsn)
        if (.not. ok) then
          rr2 = sqrt(xg(i)**2 + zg(k)**2)
          if (rsn == 1) then
            nbad1 = nbad1 + 1
            rmin1 = min(rmin1, rr2);  rmax1 = max(rmax1, rr2)
          else
            nbad2 = nbad2 + 1
            rmin2 = min(rmin2, rr2);  rmax2 = max(rmax2, rr2)
          end if
          ! A rejection well outside the trumpet is a bug, not geometry.  Record
          ! the worst offender so it cannot hide inside an aggregate count.
          if (rr2 > 1.5_dp*rr) then
            nfar = nfar + 1
            iw = i;  kw = k
          end if
          ! Inside the trumpet: flat metric, unit lapse, zero shift.  This is a
          ! placeholder, NOT constraint-satisfying data; it is excised.
          u(i,k,IGXX) = 1.0_dp; u(i,k,IGYY) = 1.0_dp; u(i,k,IGZZ) = 1.0_dp
          u(i,k,IALPHA) = 1.0_dp
          nbad = nbad + 1
          cycle
        end if
        u(i,k,IPHI) = phi
        u(i,k,IGXX) = gt(1,1); u(i,k,IGXY) = gt(1,2); u(i,k,IGXZ) = gt(1,3)
        u(i,k,IGYY) = gt(2,2); u(i,k,IGYZ) = gt(2,3); u(i,k,IGZZ) = gt(3,3)
        u(i,k,IK)   = Ktr
        u(i,k,IAXX) = at(1,1); u(i,k,IAXY) = at(1,2); u(i,k,IAXZ) = at(1,3)
        u(i,k,IAYY) = at(2,2); u(i,k,IAYZ) = at(2,3); u(i,k,IAZZ) = at(3,3)
        u(i,k,IALPHA) = alpha
        u(i,k,IBETAX) = beta(1)
        u(i,k,IBETAY) = beta(2)
        u(i,k,IBETAZ) = beta(3)
      end do
    end do
    write(*,'(A,F7.4,A,F7.4,A,F7.4)') &
         'eq_kerr_bssn_data: m=', mm, '  a=', aa, '  R0=', rr
    write(*,'(A,I0,A,I0)') '  rejected: outside closed-form domain=', nbad1, &
         '   degenerate det(gamma)<=0=', nbad2
    if (nbad1 > 0) write(*,'(A,F9.4,A,F9.4)') '    domain rejections span r in [', rmin1, ' ,', rmax1, ']'
    if (nbad2 > 0) write(*,'(A,F9.4,A,F9.4)') '    degenerate  rejections span r in [', rmin2, ' ,', rmax2, ']'
    write(*,'(A,I0,A,I0)') '  total placeholder cells=', nbad, &
         '   FAR outside trumpet (r>1.5*R0)=', nfar
    if (nfar > 0) then
      write(*,'(A,F9.4,A,F9.4,A,I0,A,I0)') &
           '  WARNING: far-field rejections at (x,z)=(', xg(iw), ' ,', zg(kw), &
           ')  grid index i=', iw, '  k=', kw
    end if
  end subroutine set_eq_kerr_trumpet_id

  subroutine inv3(M, Minv, det)
    real(dp), intent(in)  :: M(3,3)
    real(dp), intent(out) :: Minv(3,3), det
    real(dp) :: c11, c12, c13, c22, c23, c33
    c11 =  M(2,2)*M(3,3)-M(2,3)*M(3,2)
    c12 = -(M(1,2)*M(3,3)-M(1,3)*M(2,3))
    c13 =  M(1,2)*M(2,3)-M(1,3)*M(2,2)
    c22 =  M(1,1)*M(3,3)-M(1,3)*M(3,1)
    c23 = -(M(1,1)*M(2,3)-M(1,2)*M(1,3))
    c33 =  M(1,1)*M(2,2)-M(1,2)*M(1,2)
    det = M(1,1)*c11 + M(1,2)*c12 + M(1,3)*c13
    Minv = 0.0_dp
    Minv(1,1)=c11; Minv(1,2)=c12; Minv(1,3)=c13
    Minv(2,1)=c12; Minv(2,2)=c22; Minv(2,3)=c23
    Minv(3,1)=c13; Minv(3,2)=c23; Minv(3,3)=c33
    Minv = Minv/det
  end subroutine inv3

end module eq_kerr_bssn_data_mod
