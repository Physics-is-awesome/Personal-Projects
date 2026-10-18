!===============================================================================
! diagnose_kerr_bssn -- does the stationary Kerr trumpet data (route C) leave
! a ZERO full-BSSN RHS?
!
! This is the acceptance test for route C.  The question it answers is NOT
! "do the constraints hold" (they are checked separately) but "are these data a
! fixed point of the evolution", which is what stops the horizon migrating.
!
! Slicing is mode 3, the tabulated f(alpha) from integrators/bssn_rhs.F90.
! The shift driver is mode 4, whose B = 0 fixed point is correct for a
! stationary shift: d_t beta = 3/4 B = 0 and d_t B = RHS_Gam - eta B, so the
! residual that matters is RHS_Gam = d_t Lambda-tilde.
!
! IMPORTANT -- read before trusting a number here.  The Kerr data is built from
! the CLOSED FORM with derivatives at a fixed step (helper/eq_kerr_bssn_data),
! while the RHS uses the grid stencils.  So the residual mixes a genuine
! data/gauge inconsistency with ordinary truncation, and the two are NOT
! separated by this program.  To separate them, run at several resolutions:
! if the residual falls, it is truncation; if it is flat, it is the data or the
! gauge.  Do NOT quote a single-resolution number as a verdict.
!
! The Schwarzschild equilibrium fixture is measured on the same stencils as a
! scale reference, so the two numbers are comparable.
!
! Build-only diagnostic.  Not a gate (yet).
!===============================================================================
program diagnose_kerr_bssn
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state, IPHI, IK, IALPHA, &
                       IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                       IBETAX, IBETAY, IBETAZ, IBX, IBY, IBZ, &
                       IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                       IGTX, IGTY, IGTZ
  use initial_data_mod, only: compute_conformal_connections
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id, state_at
  use eq_schwarzschild_data_mod, only: set_eq_schwarzschild_id
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: hamiltonian_constraint, momentum_constraint
  use bssn_rhs_mod, only: compute_rhs_point

  implicit none
  real(dp), allocatable :: u(:,:,:), rhs(:), H(:,:), M(:,:,:)
  real(dp) :: r, hmx, hrms, mmx, rphi, rgam, rktr, rbet, rB, rat, ralp
  real(dp) :: hmx_s, hrms_s, mmx_s, rgam_s, ralp_s
  integer :: i, k, ilo, ihi, klo, khi, hmi, hmk, nprobe, rsn_p
  real(dp) :: rp, ctp, phi_p, Ktr_p, alpha_p, det_p, gt_p(3,3), at_p(3,3), beta_p(3)
  logical :: ok_p
  real(dp) :: hmr, hmalpha, hmphi, hmgxx, hmgyy
  real(dp) :: npts

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin = 0.8_dp
  slicing_mode = 3
  gamma_driver_mode = 4
  ilo=2; ihi=Nx-1; klo=2; khi=Nz-1
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(NVARS), H(Nx,Nz), M(Nx,Nz,3))

  write(*,'(A)') '============================================================'
  write(*,'(A)') ' Kerr trumpet BSSN data, slicing_mode=3 (tabulated f(alpha)),'
  write(*,'(A)') ' gamma_driver_mode=4 (B=0 fixed point).  annulus r in [3,5].'
  write(*,'(A)') '------------------------------------------------------------'

  ! ---------------- Kerr trumpet ----------------
  call set_eq_kerr_trumpet_id(u, 1.0_dp, 0.8_dp, 1.0_dp)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call excision_reset_geometry()
  call extrapolate_excised(u)
  call hamiltonian_constraint(u, H, ilo, ihi, klo, khi)
  call momentum_constraint(u, M)

  ! radial probe of the raw closed form: is det(gamma) really negative in the
  ! far field, or is the guard misfiring?
  write(*,'(A)') ' radial probe of the closed form (equator, a=0.8, R0=1):'
  write(*,'(A,3ES13.5,I3)') '        r         alpha         det(gamma)  why'
  do nprobe = 1, 12
    rp = 1.0_dp + 0.4_dp*real(nprobe,dp)
    call state_at(rp, 0.0_dp, 0.0_dp, 1.0_dp, 0.8_dp, 1.0_dp, &
                  phi_p, gt_p, at_p, Ktr_p, alpha_p, beta_p, ok_p, rsn_p, det_p)
    write(*,'(A,3ES13.5,I3)') ' ', rp, alpha_p, det_p, rsn_p
  end do
  write(*,'(A)') '   why: 0=ok 1=outside closed-form domain 2=det(gamma)<=0'
  ! SHIFT probe.  The far-field Kerr shift must fall off like 2M/r in these
  ! units.  If |beta| is off by a factor that grows with r, the (r,th,ph)
  ! components are in the wrong basis.  Compare |beta| against 2M/r directly.
  write(*,'(A)') ' shift probe, equator: |beta_cart| must fall like 2M/r:'
  write(*,'(A,4ES13.5)') '        r          |beta|        2M/r        ratio'
  do nprobe = 1, 8
    rp = 2.0_dp + 2.0_dp*real(nprobe,dp)
    call state_at(rp, 0.0_dp, 0.0_dp, 1.0_dp, 0.8_dp, 1.0_dp, &
                  phi_p, gt_p, at_p, Ktr_p, alpha_p, beta_p, ok_p, rsn_p, det_p)
    write(*,'(A,4ES13.5)') ' ', rp, sqrt(sum(beta_p**2)), 2.0_dp/rp, &
         sqrt(sum(beta_p**2))/(2.0_dp/rp)
  end do
  ! metric probe: at large r the Cartesian spatial metric must approach delta.
  ! dev = max |gamma - delta| is the single number that would expose any
  ! remaining frame or normalisation error.
  write(*,'(A)') ' metric probe, equator: gamma_cart must -> delta as r grows:'
  write(*,'(A,5ES13.5)') '        r        gxx        gyy        gzz        gxy'
  do nprobe = 1, 8
    rp = 2.0_dp + 2.0_dp*real(nprobe,dp)
    call state_at(rp, 0.0_dp, 0.0_dp, 1.0_dp, 0.8_dp, 1.0_dp, &
                  phi_p, gt_p, at_p, Ktr_p, alpha_p, beta_p, ok_p, rsn_p, det_p)
    ! gt_p is CONFORMAL; recover gamma = e^{4 phi} gt_p
    write(*,'(A,5ES13.5)') ' ', rp, exp(4.0_dp*phi_p)*gt_p(1,1), &
         exp(4.0_dp*phi_p)*gt_p(2,2), exp(4.0_dp*phi_p)*gt_p(3,3), &
         exp(4.0_dp*phi_p)*gt_p(1,2)
  end do

  ! The degenerate rejections are ANGLE dependent, not radial: probe ct = z/r
  ! at fixed r to find where det(gamma) dies.
  write(*,'(A)') ' polar-angle probe at fixed r=4.0 (equator ct=0, pole ct=+-1):'
  write(*,'(A,4ES13.5,I3)') '        ct         alpha         det(gamma)   g_thth      why'
  do nprobe = 0, 10
    ctp = -1.0_dp + 0.2_dp*real(nprobe,dp)
    call state_at(4.0_dp*sqrt(max(1.0_dp-ctp*ctp,0.0_dp)), 0.0_dp, 4.0_dp*ctp, &
                  1.0_dp, 0.8_dp, 1.0_dp, &
                  phi_p, gt_p, at_p, Ktr_p, alpha_p, beta_p, ok_p, rsn_p, det_p)
    write(*,'(A,4ES13.5,I3)') ' ', ctp, alpha_p, det_p, gt_p(2,2), rsn_p
  end do

  ! sample-point dump, so a catastrophic residual can be traced to the data
  ! rather than blamed on the gauge
  write(*,'(A)') ' sample points (data as stored):'
  do k = Nz/2, Nz/2+Nz/12, Nz/12
    i = int(0.75_dp*Nx)
    write(*,'(A,F7.3,A,F7.3,A,4ES12.4)') '   x=',xg(i),' z=',zg(k), &
         '  alpha,phi,K,gtxx=', u(i,k,IALPHA), u(i,k,IPHI), u(i,k,IK), u(i,k,IGXX)
    write(*,'(A,3ES12.4)') '        betax,betay,betaz=', &
         u(i,k,IBETAX), u(i,k,IBETAY), u(i,k,IBETAZ)
    write(*,'(A,4ES12.4)') '        gtyy,gtzz,gtxy,Axxy=', &
         u(i,k,IGYY), u(i,k,IGZZ), u(i,k,IGXY), u(i,k,IAXX)
  end do

  hmx=0.0_dp; hrms=0.0_dp; mmx=0.0_dp; npts=0.0_dp
  hmi=0; hmk=0; hmr=0.0_dp; hmalpha=0.0_dp; hmphi=0.0_dp; hmgxx=0.0_dp; hmgyy=0.0_dp
  rgam=0.0_dp; rktr=0.0_dp; rbet=0.0_dp; rB=0.0_dp
  rat=0.0_dp; ralp=0.0_dp; rphi=0.0_dp
  do k=3,Nz-2
    do i=2,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<3.0_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      if (abs(H(i,k))>hmx) then
        hmx=abs(H(i,k)); hmi=i; hmk=k; hmr=r; hmalpha=u(i,k,IALPHA)
        hmphi=u(i,k,IPHI); hmgxx=u(i,k,IGXX); hmgyy=u(i,k,IGYY)
      end if
      hrms=hrms+H(i,k)**2;  npts=npts+1.0_dp
      mmx=max(mmx,sqrt(sum(M(i,k,:)**2)))
      call compute_rhs_point(u,i,k,rhs)
      rgam=max(rgam,maxval(abs(rhs(IGTX:IGTZ))))
      rktr=max(rktr,abs(rhs(IK)))
      rbet=max(rbet,maxval(abs(rhs(IBETAX:IBETAZ))))
      rB  =max(rB,  maxval(abs(rhs(IBX:IBZ))))
      rat =max(rat, maxval(abs(rhs(IAXX:IAZZ))))
      ralp=max(ralp,abs(rhs(IALPHA)))
      rphi=max(rphi,abs(rhs(IPHI)))
    end do
  end do
  hrms=sqrt(hrms/max(npts,1.0_dp))
  write(*,'(A,ES12.4,A,ES12.4)') ' Kerr   constraints: max|H|=', hmx, '  rms|H|=', hrms
  write(*,'(A,F8.3,A,2F8.3)') '   max|H| at r=', hmr, '  (x,z)=(', xg(hmi), zg(hmk), ')'
  write(*,'(A,4ES12.4)') '   there: alpha,phi,gtxx,gtyy=', hmalpha, hmphi, hmgxx, hmgyy
  write(*,'(A,ES12.4)') '                   max|M|=', mmx
  write(*,'(A,7ES12.4)') ' Kerr   RHS max [Lambda,K,beta,B,Atilde,alpha,phi]:', &
       rgam, rktr, rbet, rB, rat, ralp, rphi
  write(*,'(A,I0)') '         annulus cells =', int(npts)

  ! ---------------- Schwarzschild reference on the same stencils --------------
  bh_spin = 0.0_dp
  r_excise = 1.4_dp
  call set_eq_schwarzschild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call excision_reset_geometry()
  call extrapolate_excised(u)
  call hamiltonian_constraint(u, H, ilo, ihi, klo, khi)
  call momentum_constraint(u, M)
  hmx_s=0.0_dp; hrms_s=0.0_dp; mmx_s=0.0_dp
  rgam_s=0.0_dp; ralp_s=0.0_dp
  do k=3,Nz-2
    do i=2,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<3.0_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      hmx_s=max(hmx_s,abs(H(i,k)));  hrms_s=hrms_s+H(i,k)**2
      mmx_s=max(mmx_s,sqrt(sum(M(i,k,:)**2)))
      call compute_rhs_point(u,i,k,rhs)
      rgam_s=max(rgam_s,maxval(abs(rhs(IGTX:IGTZ))))
      ralp_s=max(ralp_s,abs(rhs(IALPHA)))
    end do
  end do
  hrms_s=sqrt(hrms_s/max(npts,1.0_dp))
  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A,ES12.4,A,ES12.4)') ' Schw    constraints: max|H|=', hmx_s, '  rms|H|=', hrms_s
  write(*,'(A,ES12.4)') '                   max|M|=', mmx_s
  write(*,'(A,2ES12.4)') ' Schw    RHS max [Lambda,alpha]:', rgam_s, ralp_s
  write(*,'(A)') '============================================================'
  write(*,'(A)') ' Compare only within a column, and only across resolutions.'

end program diagnose_kerr_bssn
