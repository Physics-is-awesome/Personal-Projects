program test_tov
  use kinds_mod, only: dp
  use tov_mod, only: tov_profile_t, solve_tov, tov_lookup
  implicit none

  type(tov_profile_t) :: prof
  real(dp) :: rho_c, K, Gamma
  real(dp) :: riso, rho, p, eps, alpha, psi
  integer :: i

  ! Standard test case: Gamma=2 polytrope, K=100 (common in NR literature,
  ! e.g. Baumgarte & Shapiro), central density in the typical NS-like range.
  Gamma = 2.0_dp
  K = 100.0_dp
  rho_c = 1.28e-3_dp   ! central rest-mass density (G=c=1 units)

  call solve_tov(rho_c, K, Gamma, prof, 50.0_dp, 200000)

  print *, "=== TOV solver test ==="
  print '(A,ES12.4)', " central density rho_c = ", rho_c
  print '(A,F10.5)', " Schwarzschild surface radius R = ", prof%R_star
  print '(A,F10.5)', " gravitational mass M           = ", prof%M_star
  print '(A,F10.5)', " compactness M/R                = ", prof%M_star/prof%R_star
  print '(A,F10.5)', " isotropic surface radius riso  = ", prof%riso_star
  print *

  print *, "Profile sanity check (rho, p, alpha, psi vs isotropic radius):"
  do i = 0, 10
    riso = real(i,dp)/10.0_dp * prof%riso_star * 1.3_dp
    call tov_lookup(prof, riso, rho, p, eps, alpha, psi)
    print '(A,F8.4,A,ES12.4,A,ES12.4,A,F10.6,A,F10.6)', &
      " riso=", riso, "  rho=", rho, "  p=", p, "  alpha=", alpha, "  psi=", psi
  end do

end program test_tov
