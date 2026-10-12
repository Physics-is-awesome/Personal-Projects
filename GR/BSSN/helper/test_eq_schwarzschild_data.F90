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
  real(dp) :: rhs(NVARS),alpha_p,q_p,beta_r,beta_r_p,bdriver,drhs(7),legacy_b_rhs
  real(dp) :: stationary_b_rhs,stationary_beta_rhs
  real(dp) :: r,alpha,q,k_exact,radial,mtan,hmx,hrms,mrad,mphi,my
  real(dp) :: axis_hmax,axis_mrad,axis_mphi,axis_my
  real(dp) :: x,z,ux,uz,npts,expected_beta,measured_beta
  integer :: i,k,imin,imax,kmin,kmax,mri,mrk

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin=0.0_dp
  r_excise=1.4_dp
  Nx=96; Nz=192
  dx=xmax/real(Nx-1,dp); dz=2.0_dp*zmax/real(Nz-1,dp); dy=dx
  gamma_driver_mode=0
  call setup_grid()
  call alloc_state(u,Nx,Nz)
  allocate(H(Nx,Nz),Mcon(Nx,Nz,3))

  call set_eq_schwarzschild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  ! First check the legacy driver with its production B=0 initialization.
  gamma_driver_mode=0
  legacy_b_rhs=0.0_dp
  do k=2,Nz-1
    do i=5,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.6_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      call compute_rhs_point(u,i,k,rhs)
      legacy_b_rhs=max(legacy_b_rhs,maxval(abs(rhs(IBX:IBZ))))
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
  write(*,'(A,ES11.3)') ' legacy Gamma-driver max|RHS_B| (B=0): ',legacy_b_rhs
  write(*,'(A,2ES11.3)') ' non-advective stationary candidate max|RHS_B,beta|: ', &
       stationary_b_rhs,stationary_beta_rhs
  if (abs(k_exact)<1.0e-5_dp .or. abs(expected_beta)<1.0e-5_dp) &
       error stop 'Schwarzschild gate must report nonzero K and shift'
  if (abs(u(i,k,IK)-k_exact)>1.0e-5_dp) &
       error stop 'Schwarzschild K disagrees with independent spherical anchor'
  if (abs(measured_beta-expected_beta)>1.0e-5_dp) &
       error stop 'Schwarzschild shift disagrees with areal-coordinate anchor'
  if (legacy_b_rhs<1.0e-6_dp .or. stationary_b_rhs>=0.1_dp*legacy_b_rhs) &
       error stop 'non-advective Gamma-driver did not reduce the stationary B residual'
  if (hmx>5.0e-2_dp .or. hrms>1.0e-2_dp .or. mrad>5.0e-2_dp .or. &
      mphi>5.0e-2_dp .or. my>5.0e-2_dp) error stop 'Schwarzschild constraint gate failed'
  write(*,'(A)') 'PASS: nonzero Schwarzschild anchor and full constraint-vector checks.'
  write(*,'(A)') 'DIAGNOSTIC: the tested Gamma-driver RHS is not stationary; see residuals above.'
end program test_eq_schwarzschild_data
