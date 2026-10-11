program test_fo_validation
  use kinds_mod, only: dp
  use fo_curved_sat_mod, only: kerr_schild_excision_normal, apply_curved_characteristic_sat
  use fo_normal_system_mod, only: fo_normal_state_eigensystem, FO_NSTATE, &
                                  FO_MODE_ADVECTION, fo_normal_mode_class
  implicit none
  real(dp) :: q(FO_NSTATE), normal(3), A(FO_NSTATE,FO_NSTATE)
  real(dp) :: lambda(FO_NSTATE), R(FO_NSTATE,FO_NSTATE), L(FO_NSTATE,FO_NSTATE)
  real(dp) :: nx, nz, gn, rhs(2), state(2), target(2), speed(2)
  character(len=32) :: labels(FO_NSTATE)
  logical :: supported(FO_NSTATE)
  integer :: j

  ! Stationary Kerr interface normal regression.
  call kerr_schild_excision_normal(0.76596_dp, -0.88421_dp, 0.8_dp, 1.0_dp, nx, nz, gn)
  if (abs(sqrt(nx*nx+nz*nz)-1.0_dp) > 1.0e-12_dp) &
    error stop 'Kerr-Schild normal is not unit length'

  ! The transport completion has an exact identity eigensystem and explicit
  ! support labels; unsupported physical BSSN modes must remain unclaimed.
  q = 0.0_dp
  q(2) = 1.0_dp; q(5) = 1.0_dp; q(7) = 1.0_dp; q(18) = 1.0_dp
  normal = [nx,0.0_dp,nz]
  call fo_normal_state_eigensystem(q, normal, A, lambda, R, L, labels, supported)
  if (maxval(abs(matmul(L,R)-identity(FO_NSTATE))) > 1.0e-13_dp) &
    error stop 'transport eigensystem identity failed'
  do j = 1, FO_NSTATE
    if (supported(j)) then
      if (fo_normal_mode_class(labels(j)) /= FO_MODE_ADVECTION) &
        error stop 'transport mode classification failed'
    end if
  end do

  ! Scalar-wave-style incoming penalty sanity check.
  state = [2.0_dp, 2.0_dp]; target = 1.0_dp; speed = [-1.0_dp, 1.0_dp]; rhs = 0.0_dp
  call apply_curved_characteristic_sat(state,target,speed,1.0_dp,0.25_dp,0.5_dp,nx,nz,rhs)
  if (rhs(1) <= 0.0_dp .or. abs(rhs(2)) > 1.0e-14_dp) &
    error stop 'incoming SAT validation failed'
  write(*,'(A,2(1X,ES12.4))') 'first-order validation building blocks passed:', nx, nz
contains
  function identity(n) result(a)
    integer, intent(in) :: n
    real(dp) :: a(n,n)
    integer :: i
    a = 0.0_dp
    do i = 1, n
      a(i,i) = 1.0_dp
    end do
  end function identity
end program test_fo_validation
