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
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id, state_at, kerr_bssn_deriv_step, &
                                   initialize_kerr_f_cache, get_kerr_f_cache
  use eq_kerr_trumpet_mod, only: ktr_kerr_xyz
  use eq_schwarzschild_data_mod, only: set_eq_schwarzschild_id
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: hamiltonian_constraint, momentum_constraint
  use bssn_rhs_mod, only: compute_rhs_point, bm_f_of_alpha

  implicit none
  real(dp), allocatable :: u(:,:,:), rhs(:), H(:,:), M(:,:,:)
  real(dp) :: r, hmx, hrms, mmx, rphi, rgam, rktr, rbet, rB, rat, ralp
  real(dp) :: hmx_s, hrms_s, mmx_s, rgam_s, ralp_s
  integer :: i, k, ilo, ihi, klo, khi, hmi, hmk, nprobe, rsn_p, nb, nbth
  integer, parameter :: NRBIN = 7, NTH_BIN = 10
  integer, parameter :: NGR = 4
  real(dp) :: cth, cth_cell, cth_lo, cth_hi, cth_mid, mmax, msum, mcnt
  real(dp) :: spin, r0in
  character(len=64) :: argbuf
  real(dp) :: rlo, rhi, bh, bs, bn
  real(dp) :: rp, ctp, phi_p, Ktr_p, alpha_p, det_p, gt_p(3,3), at_p(3,3), beta_p(3)
  logical :: ok_p
  real(dp) :: hmr, hmalpha, hmphi, hmgxx, hmgyy
  real(dp) :: npts
  real(dp) :: al0, alxp, alxm, alzp, alzm, b0(3), btmp(3), gtmp(3,3)
  real(dp) :: f_req, f_tab, f_cache, lapse_resid, lapse_resid_max, lapse_resid_sum
  real(dp) :: f_diff_max, beta_grad_alpha, hs
  real(dp) :: rhs_tab_max(NGR),rhs_spatial_max(NGR),rhs_tab_sum(NGR),rhs_spatial_sum(NGR)
  real(dp) :: f_req_min(NGR),f_req_max(NGR),rhs_spatial,rhs_tab
  integer :: rhs_count(NGR),ig
  logical :: ok0

  ! Optional argv: spin, then R0.  Comparing H across spin is the test that
  ! separates a radial fault (alpha, bR, g_rr) from a spin-dependent one
  ! (bP, the azimuthal shift, which carries 1/sin^2(th) behaviour near the axis).
  spin = 0.8_dp;  r0in = 1.0_dp
  argbuf = ''
  call get_command_argument(1, argbuf); if (len_trim(argbuf) > 0) read(argbuf,*) spin
  argbuf = ''
  call get_command_argument(2, argbuf); if (len_trim(argbuf) > 0) read(argbuf,*) r0in
  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin = spin
  gamma_driver_mode = 4
  ilo=2; ihi=Nx-1; klo=2; khi=Nz-1
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(NVARS), H(Nx,Nz), M(Nx,Nz,3))

  write(*,'(A)') '============================================================'
  write(*,'(A,I0,A)') ' Kerr trumpet BSSN data, slicing_mode=',slicing_mode,' (route-C lapse),'
  write(*,'(A)') ' gamma_driver_mode=4 (B=0 fixed point).  annulus r in [3,5].'
  write(*,'(A)') '------------------------------------------------------------'

  ! ---------------- Kerr trumpet ----------------
  call set_eq_kerr_trumpet_id(u, 1.0_dp, spin, r0in)
  if (slicing_mode==4) call initialize_kerr_f_cache()
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
  ! Evaluate the exact DBM lapse/shift/K at each sample independently of the
  ! evolution stencil. This isolates the gauge-law residual from grid RHS error.
  lapse_resid_max=0.0_dp; lapse_resid_sum=0.0_dp; f_diff_max=0.0_dp
  hs=kerr_bssn_deriv_step()
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
      call state_at(xg(i),0.0_dp,zg(k),1.0_dp,bh_spin,r0in,phi_p,gt_p,at_p, &
                    Ktr_p,alpha_p,beta_p,ok_p)
      al0=alpha_p; b0=beta_p; ok0=ok_p
      if (ok0) then
        call ktr_kerr_xyz(xg(i)+hs,0.0_dp,zg(k),1.0_dp,bh_spin,r0in,alxp,btmp,gtmp,ok0)
        call ktr_kerr_xyz(xg(i)-hs,0.0_dp,zg(k),1.0_dp,bh_spin,r0in,alxm,btmp,gtmp,ok0)
        call ktr_kerr_xyz(xg(i),0.0_dp,zg(k)+hs,1.0_dp,bh_spin,r0in,alzp,btmp,gtmp,ok0)
        call ktr_kerr_xyz(xg(i),0.0_dp,zg(k)-hs,1.0_dp,bh_spin,r0in,alzm,btmp,gtmp,ok0)
        beta_grad_alpha=b0(1)*(alxp-alxm)/(2.0_dp*hs) &
                       +b0(3)*(alzp-alzm)/(2.0_dp*hs)
        if (abs(al0*al0*Ktr_p)>1.0e-14_dp) then
          f_req=beta_grad_alpha/(al0*al0*Ktr_p)
          f_tab=bm_f_of_alpha(al0,bh_spin)
          lapse_resid=beta_grad_alpha-al0*al0*f_tab*Ktr_p
          lapse_resid_max=max(lapse_resid_max,abs(lapse_resid))
          lapse_resid_sum=lapse_resid_sum+lapse_resid*lapse_resid
          f_diff_max=max(f_diff_max,abs(f_tab-f_req))
        end if
      end if
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

  ! M vs r and theta.  The max|M| did NOT improve with the g_Rphi fix
  ! (6.29e-3 -> 7.49e-3), so it is probably a different mechanism.
  ! If it's concentrated on the axis, the cartoon/Jacobian axis is the
  ! suspect; if it's spread out, it's the K-from-gauge or something else.
  ! This is the single measurement that separates the remaining candidates.
  write(*,'(A)') ' polar-angle profile of |M| at r in [3,5]:'
  write(*,'(A,ES13.5)') '   ct         max|M|       rms|M|'
  do nbth = 0, 9
    cth_lo = real(nbth,dp)/10.0_dp
    cth_hi = real(nbth+1,dp)/10.0_dp
    cth_mid = 0.5_dp*(cth_lo+cth_hi)
    mmax = 0.0_dp; msum = 0.0_dp; mcnt = 0.0_dp
    do k=3,Nz-2
      do i=2,Nx-1
        r=sqrt(xg(i)**2+zg(k)**2)
        if (r<3.0_dp .or. r>5.0_dp) cycle
        if (excision_status(i,k)/=PHYSICAL) cycle
        cth_cell = abs(zg(k))/max(r,1.0e-30_dp)
        if (cth_cell < cth_lo .or. cth_cell >= cth_hi) cycle
        mmax = max(mmax, sqrt(sum(M(i,k,:)**2)))
        msum = msum + sum(M(i,k,:)**2)
        mcnt = mcnt + 1.0_dp
      end do
    end do
    write(*,'(A,F7.4,A,F7.4,2ES13.5)') ' ', cth_lo, '..', cth_hi, mmax, sqrt(msum/max(mcnt,1.0_dp))
  end do
  write(*,'(A)') '   If the peak is at |ct|~1 => axis.  If flat => spread.'
  ! H vs r.  This is the test that separates the two remaining candidates:
  ! if H -> 0 at large r and grows inward, the LAPSE is the suspect and there
  ! is something to fix; if H is FLAT in r, the family is not a vacuum solution
  ! as coded and route C is dead.  Nothing in the tree has ever checked
  ! R_ij = 0 for this family -- Part A checked structure and asymptotics only.
  write(*,'(A,F7.4,A,F7.4)') ' spin=', spin, '  R0=', r0in
  write(*,'(A)') ' radial profile of the Hamiltonian residual:'
  write(*,'(A,3ES13.4)') '   r-bin        max|H|        rms|H|'
  do nb = 1, 7
    rlo = 2.0_dp + (12.0_dp-2.0_dp)*real(nb-1,dp)/7.0_dp
    rhi = 2.0_dp + (12.0_dp-2.0_dp)*real(nb  ,dp)/7.0_dp
    bh = 0.0_dp; bs = 0.0_dp; bn = 0.0_dp
    do k=3,Nz-2
      do i=2,Nx-1
        r=sqrt(xg(i)**2+zg(k)**2)
        if (r<rlo .or. r>=rhi) cycle
        if (excision_status(i,k)/=PHYSICAL) cycle
        bh=max(bh,abs(H(i,k)));  bs=bs+H(i,k)**2;  bn=bn+1.0_dp
      end do
    end do
    write(*,'(A,F6.2,A,F6.2,2ES13.4)') '   ', rlo, '..', rhi, bh, &
         sqrt(bs/max(bn,1.0_dp))
  end do
  write(*,'(A)') '   NOTE: the innermost bins still sit close to the trumpet'
  write(*,'(A)') '   placeholder (160 flat cells, r<1.41); treat r<3 as suspect.'
  write(*,'(A)') '   H flat in r => the family is not a vacuum solution as coded.'

  write(*,'(A,ES12.4,A,ES12.4)') ' Kerr   constraints: max|H|=', hmx, '  rms|H|=', hrms
  write(*,'(A,F8.3,A,2F8.3)') '   max|H| at r=', hmr, '  (x,z)=(', xg(hmi), zg(hmk), ')'
  write(*,'(A,4ES12.4)') '   there: alpha,phi,gtxx,gtyy=', hmalpha, hmphi, hmgxx, hmgyy
  write(*,'(A,ES12.4)') '                   max|M|=', mmx
  write(*,'(A,7ES12.4)') ' Kerr   RHS max [Lambda,K,beta,B,Atilde,alpha,phi]:', &
       rgam, rktr, rbet, rB, rat, ralp, rphi
  write(*,'(A,3ES12.4)') ' analytic f-table residual max/rms, max|f-f_required|:', &
       lapse_resid_max, sqrt(lapse_resid_sum/max(npts,1.0_dp)), f_diff_max
  write(*,'(A,I0)') '         annulus cells =', int(npts)

  ! Compare the actual production finite-difference lapse RHS against a
  ! diagnostic position-dependent coefficient f_required(x,z).  This uses
  ! the same discrete beta.grad(alpha) already present in RHS_alpha and only
  ! replaces its Bona-Masso source term; it does not modify production state.
  rhs_tab_max=0.0_dp;rhs_spatial_max=0.0_dp
  rhs_tab_sum=0.0_dp;rhs_spatial_sum=0.0_dp;rhs_count=0
  f_req_min=huge(1.0_dp);f_req_max=-huge(1.0_dp)
  do k=3,Nz-2
    do i=2,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<1.55_dp .or. r>8.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      if (r<2.0_dp) then
        ig=1
      else if (r<3.0_dp) then
        ig=2
      else if (r<5.0_dp) then
        ig=3
      else
        ig=4
      end if
      call compute_rhs_point(u,i,k,rhs)
      call state_at(xg(i),0.0_dp,zg(k),1.0_dp,bh_spin,r0in,phi_p,gt_p,at_p, &
                    Ktr_p,alpha_p,beta_p,ok_p)
      if (.not.ok_p .or. abs(alpha_p*alpha_p*Ktr_p)<1.0e-14_dp) cycle
      call ktr_kerr_xyz(xg(i)+hs,0.0_dp,zg(k),1.0_dp,bh_spin,r0in,alxp,btmp,gtmp,ok_p)
      call ktr_kerr_xyz(xg(i)-hs,0.0_dp,zg(k),1.0_dp,bh_spin,r0in,alxm,btmp,gtmp,ok_p)
      call ktr_kerr_xyz(xg(i),0.0_dp,zg(k)+hs,1.0_dp,bh_spin,r0in,alzp,btmp,gtmp,ok_p)
      call ktr_kerr_xyz(xg(i),0.0_dp,zg(k)-hs,1.0_dp,bh_spin,r0in,alzm,btmp,gtmp,ok_p)
      if (.not.ok_p) cycle
      beta_grad_alpha=beta_p(1)*(alxp-alxm)/(2.0_dp*hs) &
                     +beta_p(3)*(alzp-alzm)/(2.0_dp*hs)
      f_req=beta_grad_alpha/(alpha_p*alpha_p*Ktr_p)
      f_tab=bm_f_of_alpha(alpha_p,bh_spin)
      call get_kerr_f_cache(i,k,f_cache,ok_p)
      if (slicing_mode==4 .and. ok_p) then
        rhs_tab=rhs(IALPHA)+alpha_p*alpha_p*(f_cache-f_tab)*Ktr_p
        rhs_spatial=rhs(IALPHA)
      else
        rhs_tab=rhs(IALPHA)
        rhs_spatial=rhs(IALPHA)+alpha_p*alpha_p*(f_tab-f_req)*Ktr_p
      end if
      rhs_tab_max(ig)=max(rhs_tab_max(ig),abs(rhs_tab))
      rhs_spatial_max(ig)=max(rhs_spatial_max(ig),abs(rhs_spatial))
      rhs_tab_sum(ig)=rhs_tab_sum(ig)+rhs_tab**2
      rhs_spatial_sum(ig)=rhs_spatial_sum(ig)+rhs_spatial**2
      f_req_min(ig)=min(f_req_min(ig),f_req);f_req_max(ig)=max(f_req_max(ig),f_req)
      rhs_count(ig)=rhs_count(ig)+1
    end do
  end do
  write(*,'(A)') ' exact-data lapse RHS: current scalar table vs spatial f_required'
  write(*,'(A)') ' radial band    max(table) rms(table) max(spatial) rms(spatial) f_required min/max'
  do ig=1,NGR
    if (rhs_count(ig)==0) cycle
    select case(ig)
    case(1);rlo=1.55_dp;rhi=2.0_dp
    case(2);rlo=2.0_dp;rhi=3.0_dp
    case(3);rlo=3.0_dp;rhi=5.0_dp
    case default;rlo=5.0_dp;rhi=8.0_dp
    end select
    write(*,'(F5.2,A,F4.1,4ES13.5,2ES13.5,I8)') rlo,'..',rhi, &
         rhs_tab_max(ig),sqrt(rhs_tab_sum(ig)/rhs_count(ig)), &
         rhs_spatial_max(ig),sqrt(rhs_spatial_sum(ig)/rhs_count(ig)), &
         f_req_min(ig),f_req_max(ig),rhs_count(ig)
  end do

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
