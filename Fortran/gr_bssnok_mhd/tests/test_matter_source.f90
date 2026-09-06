program test_matter_source
  use kinds_mod, only: dp
  use hydro_vars_mod, only: NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  use eos_mod, only: eos_gamma, pressure
  use matter_source_mod, only: matter_source
  implicit none

  real(dp) :: prim(NPRIM), gamma_ij(3,3)
  real(dp) :: E, Slow(3), Sij(3,3)
  real(dp) :: rho, eps, p, Strace
  logical :: ok

  eos_gamma = 2.0_dp
  rho = 1.0e-3_dp
  eps = 0.5_dp
  p = pressure(rho, eps)

  prim(IRHO) = rho; prim(IVX)=0.0_dp; prim(IVY)=0.0_dp; prim(IVZ)=0.0_dp; prim(IEPS)=eps
  gamma_ij = 0.0_dp
  gamma_ij(1,1)=1.2_dp; gamma_ij(2,2)=1.2_dp; gamma_ij(3,3)=1.2_dp  ! isotropic, not flat, to check generality

  call matter_source(prim, gamma_ij, E, Slow, Sij)

  print *, "=== matter_source_mod unit test (fluid at rest) ==="
  print '(A,ES14.6,A,ES14.6)', " rho=", rho, "  eps=", eps
  print '(A,ES14.6)', " pressure p = ", p
  print '(A,ES14.6,A,ES14.6)', " expected E = rho+rho*eps = ", rho+rho*eps, "   got E = ", E
  print '(A,3ES14.6)', " S_i (expect 0,0,0) = ", Slow
  print '(A,ES14.6,A,ES14.6)', " S_xx (expect p*gamma_xx=", p*gamma_ij(1,1), ")  got = ", Sij(1,1)

  Strace = Sij(1,1)/gamma_ij(1,1) + Sij(2,2)/gamma_ij(2,2) + Sij(3,3)/gamma_ij(3,3)
  ! (since gamma is diagonal here, gamma^ii = 1/gamma_ii)
  print '(A,ES14.6,A,ES14.6)', " trace S = gamma^ij S_ij (expect 3p=", 3.0_dp*p, ")  got = ", Strace

  ok = .true.
  if (abs(E - (rho+rho*eps)) > 1.0e-12_dp) ok = .false.
  if (maxval(abs(Slow)) > 1.0e-12_dp) ok = .false.
  if (abs(Sij(1,1) - p*gamma_ij(1,1)) > 1.0e-12_dp*max(1.0_dp,abs(p))) ok = .false.
  if (abs(Strace - 3.0_dp*p) > 1.0e-10_dp) ok = .false.

  print *
  if (ok) then
    print *, "PASSED: static-fluid matter source reduces to the expected E=rho(1+eps), S_i=0, S_ij=p*gamma_ij."
  else
    print *, "FAILED"
  end if

end program test_matter_source
