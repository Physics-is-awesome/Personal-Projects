! Manufactured, nonzero check of the advective Gamma-driver equations.
! The expected values below are evaluated from flat-space polynomial fields,
! independently of the production RHS assembly.
program test_gamma_driver_rhs
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_point
  implicit none

  real(dp), allocatable :: u(:,:,:)
  real(dp) :: rhs(NVARS), x, z, rho2, beta(3), Bvec(3), Gt(3)
  real(dp) :: a, bcoef, ccoef, p, q, s, e, f, g, divb
  real(dp) :: bgrad_beta(3), bgrad_lambda(3), bgrad_driver(3), rhs_beta(3), rhs_driver(3)
  real(dp) :: gamma_rhs_z, expected_beta(3), expected_B(3), err
  real(dp) :: expected_standard_beta(3), expected_standard_B(3), standard_beta(3), standard_B(3)
  integer :: i, k

  call init_params('parameter_files/vacuum_kerr.par')
  gamma_driver_mode = 3
  slicing_mode = 2
  gauge_eta = 0.6_dp
  call setup_grid()
  call alloc_state(u, Nx, Nz)

  ! Axisymmetric quadratic manufactured fields. Values are nonzero at the
  ! check point and their derivatives are exactly represented by the stencil.
  a=0.20_dp; bcoef=-0.10_dp; ccoef=0.07_dp
  p=0.13_dp; q=-0.04_dp; s=0.05_dp
  e=0.11_dp; f=-0.09_dp; g=0.03_dp
  do k=1,Nz
    do i=1,Nx
      x=xg(i); z=zg(k); rho2=x*x
      u(i,k,IGXX)=1.0_dp; u(i,k,IGYY)=1.0_dp; u(i,k,IGZZ)=1.0_dp
      u(i,k,IALPHA)=1.0_dp
      u(i,k,IBETAX)=a*x*z
      u(i,k,IBETAZ)=bcoef*z*z+ccoef*rho2
      u(i,k,IBX)=e*x*z
      u(i,k,IBZ)=f*z*z+g*rho2
      u(i,k,IGTX)=p*x*z
      u(i,k,IGTZ)=q*z*z+s*rho2
    end do
  end do
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  i=10; k=70
  x=xg(i); z=zg(k); rho2=x*x
  beta=(/a*x*z, 0.0_dp, bcoef*z*z+ccoef*rho2/)
  Bvec=(/e*x*z, 0.0_dp, f*z*z+g*rho2/)
  Gt=(/p*x*z, 0.0_dp, q*z*z+s*rho2/)
  divb=2.0_dp*(a+bcoef)*z

  ! beta^j partial_j beta^i
  bgrad_beta(1)=a*x*((a+bcoef)*z*z+ccoef*rho2)
  bgrad_beta(2)=0.0_dp
  bgrad_beta(3)=2.0_dp*ccoef*x*beta(1)+2.0_dp*bcoef*z*beta(3)
  expected_beta=0.75_dp*Bvec+bgrad_beta

  ! beta^j partial_j B^i and beta^j partial_j Lambda^i.
  bgrad_driver(1)=e*x*((a+bcoef)*z*z+ccoef*rho2)
  bgrad_driver(2)=0.0_dp
  bgrad_driver(3)=2.0_dp*g*x*beta(1)+2.0_dp*f*z*beta(3)
  bgrad_lambda(1)=p*x*((a+bcoef)*z*z+ccoef*rho2)
  bgrad_lambda(2)=0.0_dp
  bgrad_lambda(3)=2.0_dp*s*x*beta(1)+2.0_dp*q*z*beta(3)

  ! Flat-space conformal-connection RHS: Delta beta^i + (1/3)partial^i div(beta),
  ! plus the known transport/index/density terms from the evolved Lambda field.
  gamma_rhs_z=2.0_dp*bcoef+4.0_dp*ccoef+(2.0_dp/3.0_dp)*(a+bcoef) &
       + bgrad_lambda(3)-Gt(1)*(2.0_dp*ccoef*x)-Gt(3)*(2.0_dp*bcoef*z) &
       +(2.0_dp/3.0_dp)*Gt(3)*divb
  expected_B(1)=-Gt(1)*(a*z)-Gt(3)*(a*x) &
       +(2.0_dp/3.0_dp)*Gt(1)*divb+bgrad_driver(1)-gauge_eta*Bvec(1)
  expected_B(2)=0.0_dp
  expected_B(3)=gamma_rhs_z-bgrad_lambda(3)+bgrad_driver(3)-gauge_eta*Bvec(3)

  call compute_rhs_point(u,i,k,rhs)
  rhs_beta=(/rhs(IBETAX),rhs(IBETAY),rhs(IBETAZ)/)
  rhs_driver=(/rhs(IBX),rhs(IBY),rhs(IBZ)/)
  err=maxval(abs(rhs_beta-expected_beta))
  write(*,'(A,3ES14.5)') ' Gamma-driver beta RHS computed/expected/error: ', &
       maxval(abs(rhs_beta)),maxval(abs(expected_beta)),err
  if (err>2.0e-10_dp) error stop 'advective Gamma-driver beta RHS mismatch'
  err=maxval(abs(rhs_driver-expected_B))
  write(*,'(A,3ES14.5)') ' Gamma-driver B RHS computed/expected/error:    ', &
       maxval(abs(rhs_driver)),maxval(abs(expected_B)),err
  write(*,'(A,3ES14.5)') '   computed B components: ',rhs_driver
  write(*,'(A,3ES14.5)') '   expected B components: ',expected_B
  if (err>2.0e-10_dp) error stop 'advective Gamma-driver B RHS mismatch'
  if (maxval(abs(expected_beta))<1.0e-6_dp .or. maxval(abs(expected_B))<1.0e-6_dp) then
    error stop 'manufactured Gamma-driver acceptance answer must be nonzero'
  end if

  ! Independent non-advective Gamma-driver contract. The only difference in
  ! the B equation is that it evolves the coordinate-time connection directly.
  gamma_driver_mode=4
  expected_standard_beta=0.75_dp*Bvec
  expected_standard_B=expected_B+bgrad_lambda-bgrad_driver
  call compute_rhs_point(u,i,k,rhs)
  standard_beta=(/rhs(IBETAX),rhs(IBETAY),rhs(IBETAZ)/)
  standard_B=(/rhs(IBX),rhs(IBY),rhs(IBZ)/)
  err=maxval(abs(standard_beta-expected_standard_beta))
  write(*,'(A,3ES14.5)') ' Nonadvective beta RHS computed/expected/error: ', &
       maxval(abs(standard_beta)),maxval(abs(expected_standard_beta)),err
  if (err>2.0e-10_dp) error stop 'non-advective Gamma-driver beta RHS mismatch'
  err=maxval(abs(standard_B-expected_standard_B))
  write(*,'(A,3ES14.5)') ' Nonadvective B RHS computed/expected/error:    ', &
       maxval(abs(standard_B)),maxval(abs(expected_standard_B)),err
  if (err>2.0e-10_dp) error stop 'non-advective Gamma-driver B RHS mismatch'
  if (maxval(abs(expected_standard_beta))<1.0e-6_dp .or. &
      maxval(abs(expected_standard_B))<1.0e-6_dp) then
    error stop 'manufactured non-advective Gamma-driver answer must be nonzero'
  end if
  write(*,'(A)') 'PASS: nonzero advective and non-advective Gamma-driver RHS checks.'
end program test_gamma_driver_rhs
