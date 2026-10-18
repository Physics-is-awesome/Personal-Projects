! Constraint and external-anchor gate for the full stationary 1+log
! Schwarzschild data reconstructed in areal Cartesian coordinates.
program test_eq_schwarzschild_data
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg,zg,setup_grid
  use vars_mod, only: NVARS,alloc_state,IPHI,IGXX,IGZZ,IK,IAXX,IAZZ, &
                      IALPHA,IBETAX,IBETAZ,IBX,IBZ
  use eq_schwarzschild_data_mod, only: set_eq_schwarzschild_id
  use equilibrium_1pluslog_mod, only: eq_lapse,eq_shift,eq_mean_curvature, &
                                      eq_lapse_ext,eq_shift_ext, &
                                      eq_dalpha_dr,eq_dbeta_dr
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background,extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: hamiltonian_constraint,momentum_constraint
  use bssn_rhs_mod, only: compute_rhs_point
  use excision_mod, only: excision_status,PHYSICAL
  implicit none
  real(dp),allocatable :: u(:,:,:),H(:,:),Mcon(:,:,:)
  real(dp) :: rhs(NVARS),alpha_p,q_p,beta_r,beta_r_p,bdriver,drhs(7),production_b_rhs
  real(dp) :: stationary_b_rhs,stationary_beta_rhs
  ! Both driver gates compare the production default against the demonstrated
  ! stationary candidate (mode 4) on identical data and stencil, so the
  ! comparison is resolution-independent and self-calibrating.
  real(dp),parameter :: gate_factor=10.0_dp
  real(dp) :: r,alpha,q,k_exact,radial,mtan,hmx,hrms,mrad,mphi,my
  real(dp) :: axis_hmax,axis_mrad,axis_mphi,axis_my
  real(dp) :: x,z,ux,uz,npts,expected_beta,measured_beta
  integer :: i,k,imin,imax,kmin,kmax,mri,mrk,production_mode

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin=0.0_dp
  r_excise=1.4_dp
  Nx=96; Nz=192
  dx=xmax/real(Nx-1,dp); dz=2.0_dp*zmax/real(Nz-1,dp); dy=dx
  ! Whatever init_params settled on IS the production default (module default,
  ! parameter file, or GR_GAMMA_DRIVER_MODE).  Gate that, not a hardcoded mode,
  ! so changing the production default to a stationary gauge passes this test.
  production_mode=gamma_driver_mode
  call setup_grid()
  call alloc_state(u,Nx,Nz)
  allocate(H(Nx,Nz),Mcon(Nx,Nz,3))

  call set_eq_schwarzschild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  ! Production default driver, with the production B=0 initialization.
  gamma_driver_mode=production_mode
  production_b_rhs=0.0_dp
  do k=2,Nz-1
    do i=5,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.6_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      call compute_rhs_point(u,i,k,rhs)
      production_b_rhs=max(production_b_rhs,maxval(abs(rhs(IBX:IBZ))))
    end do
  end do

  ! Candidate compatibility check: a stationary metric has d_t Lambda=0,
  ! so the non-advective driver should preserve B=0 and the coordinate shift.
  gamma_driver_mode=4
  stationary_b_rhs=0.0_dp; stationary_beta_rhs=0.0_dp
  do k=2,Nz-1
    do i=5,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.6_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      call compute_rhs_point(u,i,k,rhs)
      stationary_b_rhs=max(stationary_b_rhs,maxval(abs(rhs(IBX:IBZ))))
      stationary_beta_rhs=max(stationary_beta_rhs,maxval(abs(rhs(IBETAX:IBETAZ))))
    end do
  end do

  ! A stationary 1+log lapse does not imply a stationary Gamma-driver shift.
  ! Initialize B so that the advective shift equation has zero continuum RHS,
  ! then measure the remaining semidiscrete BSSN/gauge residual.
  slicing_mode=0
  gamma_driver_mode=3
  do k=1,Nz
    do i=1,Nx
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<=r_excise .or. r<1.35_dp*bh_mass) cycle
      alpha=eq_lapse_ext(bh_mass,r)
      q=eq_shift_ext(bh_mass,r)
      alpha_p=eq_dalpha_dr(bh_mass,r)
      q_p=eq_dbeta_dr(bh_mass,r)
      beta_r=alpha*q
      beta_r_p=alpha_p*q+alpha*q_p
      bdriver=-(4.0_dp/3.0_dp)*beta_r*beta_r_p
      u(i,k,IBX)=bdriver*xg(i)/r
      u(i,k,IBZ)=bdriver*zg(k)/r
    end do
  end do
  drhs=0.0_dp
  do k=2,Nz-1
    do i=5,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.6_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      call compute_rhs_point(u,i,k,rhs)
      drhs(1)=max(drhs(1),abs(rhs(IPHI)))
      drhs(2)=max(drhs(2),maxval(abs(rhs(IGXX:IGZZ))))
      drhs(3)=max(drhs(3),abs(rhs(IK)))
      drhs(4)=max(drhs(4),maxval(abs(rhs(IBETAX:IBETAZ))))
      drhs(5)=max(drhs(5),maxval(abs(rhs(IBX:IBZ))))
      drhs(6)=max(drhs(6),maxval(abs(rhs(IAXX:IAZZ))))
      drhs(7)=max(drhs(7),abs(rhs(IALPHA)))
    end do
  end do
  call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
  call momentum_constraint(u,Mcon)

  hmx=0.0_dp; hrms=0.0_dp; mrad=0.0_dp; mphi=0.0_dp; my=0.0_dp; npts=0.0_dp
  axis_hmax=0.0_dp; axis_mrad=0.0_dp; axis_mphi=0.0_dp; axis_my=0.0_dp
  mri=0; mrk=0
  do k=kmin,kmax
    do i=imin,imax
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.6_dp .or. r>5.0_dp) cycle
      x=xg(i); z=zg(k); ux=x/r; uz=z/r
      radial=ux*Mcon(i,k,1)+uz*Mcon(i,k,3)
      mtan=-uz*Mcon(i,k,1)+ux*Mcon(i,k,3)
      if (i<5) then
        axis_hmax=max(axis_hmax,abs(H(i,k)))
        axis_mrad=max(axis_mrad,abs(radial)); axis_mphi=max(axis_mphi,abs(mtan))
        axis_my=max(axis_my,abs(Mcon(i,k,2)))
        cycle
      end if
      hmx=max(hmx,abs(H(i,k))); hrms=hrms+H(i,k)*H(i,k)
      if (abs(radial)>mrad) then
        mrad=abs(radial); mri=i; mrk=k
      end if
      mphi=max(mphi,abs(mtan)); my=max(my,abs(Mcon(i,k,2)))
      npts=npts+1.0_dp
    end do
  end do
  hrms=sqrt(hrms/max(npts,1.0_dp))

  i=minloc(abs(xg-3.0_dp),dim=1); k=minloc(abs(zg),dim=1)
  r=sqrt(xg(i)**2+zg(k)**2)
  alpha=eq_lapse(bh_mass,r); q=eq_shift(bh_mass,r,alpha)
  k_exact=eq_mean_curvature(bh_mass,r)
  measured_beta=u(i,k,IBETAX)*(xg(i)/sqrt(xg(i)**2+zg(k)**2)) &
       +u(i,k,IBETAZ)*(zg(k)/sqrt(xg(i)**2+zg(k)**2))
  expected_beta=alpha*q
  write(*,'(A,F8.4,A,ES12.4,A,ES12.4)') 'Schwarzschild trumpet R=',r, &
       '  K exact=',k_exact,'  beta^R exact=',expected_beta
  write(*,'(A,ES11.3,A,ES11.3,A,ES11.3)') ' annulus (x>=4dx) max|H|=',hmx, &
       ' rms(H)=',hrms,' max|M_rad|=',mrad
  write(*,'(A,ES11.3,A,ES11.3,A,ES11.3)') ' annulus max|M_tan|=',mphi, &
       ' max|M_y|=',my,'  cells=',npts
  write(*,'(A,2I5,A,2F10.4)') ' max radial M index/point: ',mri,mrk,'  (x,z)=',xg(mri),zg(mrk)
  write(*,'(A,4ES11.3)') ' axis layer (x<4dx) max [H,Mrad,Mtan,My]: ', &
       axis_hmax,axis_mrad,axis_mphi,axis_my
  write(*,'(A,7ES11.3)') ' BSSN RHS max [phi,gtilde,K,beta,B,Atilde,alpha]: ',drhs
  write(*,'(A,I0,A,ES11.3)') ' production Gamma-driver (mode ',production_mode, &
       ') max|RHS_B| (B=0): ',production_b_rhs
  write(*,'(A,2ES11.3)') ' non-advective stationary candidate max|RHS_B,beta|: ', &
       stationary_b_rhs,stationary_beta_rhs
  if (abs(k_exact)<1.0e-5_dp .or. abs(expected_beta)<1.0e-5_dp) &
       error stop 'Schwarzschild gate must report nonzero K and shift'
  if (abs(u(i,k,IK)-k_exact)>1.0e-5_dp) &
       error stop 'Schwarzschild K disagrees with independent spherical anchor'
  if (abs(measured_beta-expected_beta)>1.0e-5_dp) &
       error stop 'Schwarzschild shift disagrees with areal-coordinate anchor'
  if (hmx>5.0e-2_dp .or. hrms>1.0e-2_dp .or. mrad>5.0e-2_dp .or. &
      mphi>5.0e-2_dp .or. my>5.0e-2_dp) error stop 'Schwarzschild constraint gate failed'
  ! ---- stationary-gauge gate -------------------------------------------------
  ! On exact equilibrium data with the production B=0 initialisation, a gauge
  ! that is a genuine fixed point of the evolution must leave only a
  ! discretisation-level residual in its driver variable.  The default
  ! (gamma_driver_mode 0) has no such fixed point: its driver residual is orders
  ! of magnitude above the physical fields, the shift is anchored to nothing,
  ! and the apparent horizon migrates inward until it reaches the fixed
  ! r_excise surface (observed: step 6848, det(gtilde) < 0 at the boundary).
  !
  ! Gate on the RATIO, not an absolute number: both residuals come from the
  ! same stencil on the same data, so the ratio isolates the gauge defect from
  ! truncation error.  Do NOT loosen gate_factor to make `make check` green --
  ! fix the gauge instead.  See the KNOWN LIMITATION note in the Makefile.
  !
  ! Note on candidate fixes -- READ BEFORE CHANGING THE DEFAULT:
  !
  ! * mode 1 (puncture-anchored) does NOT satisfy this gate as written.  With
  !   the production B=0 initialization its driver RHS is -eta*B_puncture,
  !   which measures 1.674 here -- far worse than mode 0.  It is only
  !   stationary if B is initialised to the puncture profile, an
  !   equilibrium-data question rather than a one-line default change.
  !
  ! * mode 4 DOES satisfy this gate today (ratio 1.0), because B=0 is its exact
  !   fixed point on Schwarzschild.  DO NOT conclude that switching the default
  !   to mode 4 fixes production.  Measured on parameter_files/vacuum_kerr.par
  !   at 48x96, equal physical time t=1.786, apparent-horizon r_min from 1.5999:
  !       mode 0: 1.4359   (drift 0.1640)
  !       mode 4: 1.3079   (drift 0.2920)
  !   That comparison is NOT conclusive: 48x96 is itself under-resolved (see
  !   below), so it understates how badly mode 0 drifts.  The mode-4 number has
  !   not been repeated at higher resolution.  Treat "mode 4 is worse" as
  !   unproven rather than established.
  !
  ! Why this gate cannot be the whole story: the fixture is SPHERICAL
  ! (bh_spin=0).  There is no Kerr equilibrium data in the tree, so production
  ! starts from Kerr-Schild, which is NOT a 1+log slice at any spin.  A gauge
  ! that is a fixed point of spherical equilibrium data can still be the wrong
  ! fixed point for Kerr-Schild data.  This gate is a NECESSARY condition only;
  ! passing it does not license changing the production default.
  !
  ! Resolution evidence (mode 0, r_excise=1.0, equal physical time):
  !       t       48x96 drift    96x192 drift
  !     0.4466       0.0429         0.0395
  !     0.8886       0.1277         0.1274
  !     1.3328       0.1558         0.2364
  !     1.7770       0.1640         0.3452
  ! The two resolutions agree early and diverge later, and 48x96 STALLS at
  ! r~1.44 while 96x192 keeps marching.  The stall is a discretization artifact:
  ! the coarse grid appears to survive only because it cannot follow the true
  ! horizon motion.  The production grid is therefore under-resolved, and the
  ! controlling blocker remains the absence of stationary Kerr data
  ! (LONG_TERM_GAUGE_DRIFT_REPORT.md section 9, Phase 3).
  if (stationary_b_rhs<=0.0_dp) &
       error stop 'stationary candidate residual must be > 0 to calibrate the gate'
  if (production_b_rhs>gate_factor*stationary_b_rhs) then
     write(*,'(A,I0,A,ES11.3,A,ES11.3,A,F7.2,A,ES11.3)') &
          'FAIL: production Gamma-driver (mode ',production_mode,') residual=', &
          production_b_rhs,'  stationary candidate=',stationary_b_rhs, &
          '  ratio=',production_b_rhs/stationary_b_rhs,'  (limit ',gate_factor,')'
     write(*,'(A)') ' The default gauge is not stationary on exact equilibrium data,'
     write(*,'(A)') ' so `make run` will migrate the horizon into the excision region.'
     error stop 'production default gauge is not stationary'
  end if
  if (stationary_b_rhs>gate_factor*production_b_rhs) then
     write(*,'(A,ES11.3,A,ES11.3)') &
          'FAIL: stationary candidate residual=',stationary_b_rhs, &
          '  is worse than production=',production_b_rhs
     error stop 'stationary candidate is not the better driver'
  end if
  write(*,'(A,ES11.3,A,ES11.3,A,F7.2)') &
       'PASS: production and candidate driver residuals within factor ', &
       gate_factor,'  (',production_b_rhs,' vs ',stationary_b_rhs,')'
  write(*,'(A)') 'PASS: nonzero Schwarzschild anchor and full constraint-vector checks.'
end program test_eq_schwarzschild_data
