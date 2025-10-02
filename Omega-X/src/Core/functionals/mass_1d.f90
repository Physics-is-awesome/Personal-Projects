module mass
  use mesh
  use projection_matrix
  implicit none
  real(8), intent(in)  :: rho_h(N), u_h(N)
  real(8), intent(out) :: rho_rhs(N)
  real(8) :: flux(N)
  integer :: i

  ! Compute pointwise flux: rho * u
  do i = 1, N
    flux(i) = rho_h(i) * u_h(i)
  end do

  ! Apply Galerkin derivative projection to flux
  call apply_weak_derivative(flux, rho_rhs)

  ! Multiply by 1/dx to apply M^{-1} (lumped mass matrix)
  do i = 1, N
    rho_rhs(i) = -rho_rhs(i) / dx
  end do

  ! Optional: boundary conditions (e.g., zero mass flux)
  rho_rhs(1) = 0.0d0
  rho_rhs(N) = 0.0d0
end module mass
