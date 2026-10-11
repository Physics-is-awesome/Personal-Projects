program test_fo_convergence
  use kinds_mod, only: dp
  use fo_sbp_mod, only: sbp_d1
  implicit none
  integer, parameter :: n1=41, n2=81
  real(dp) :: e1, e2
  e1 = derivative_error(n1)
  e2 = derivative_error(n2)
  if (e2 >= 0.35_dp*e1) error stop 'SBP refinement did not converge'
  write(*,'(A,2(ES12.4,1X),A,F8.4)') 'SBP convergence errors=',e1,e2,' ratio=',e1/e2
contains
  real(dp) function derivative_error(n)
    integer, intent(in) :: n
    real(dp) :: f(n), df(n), x, dx
    integer :: i
    dx = 1.0_dp/real(n-1,dp)
    do i=1,n
      x=real(i-1,dp)*dx
      f(i)=sin(2.0_dp*acos(-1.0_dp)*x)
    end do
    call sbp_d1(f,dx,df)
    derivative_error = maxval(abs(df-2.0_dp*acos(-1.0_dp)*cos(2.0_dp*acos(-1.0_dp)*[(real(i-1,dp)*dx,i=1,n)])))
  end function derivative_error
end program test_fo_convergence
