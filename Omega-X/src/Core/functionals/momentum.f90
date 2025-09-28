module momentum
  use mesh
  use states
  implicit none
  real(8), intent(in)  :: u_h(N), m_h(N), d_dx(u_h, i), d_dx(eta_h, i), rho_h(i), sigma_h(i) d_dx(T_h, i), d_dx(u_h, i), Re, dx, dphi_dx
  real(8), intent(out) :: dmdt(N)
  real(8) :: momentum_RHS(N)
  integer :: i
  real(8), parameter :: Re = 100.0d0

do i = 2, N-1
    momentum_RHS(i) = &
   + m_h(i) * dphi_dx * u_h(i) * dx &                  ! (m u, ∂x φ)
   - m_h(i) * d_dx(u_h, i) * dx &                     ! (m ∂x u, φ)
   - rho_h(i) * d_dx(eta_h, i) * dx &                 ! (ρ ∂x η, φ)
   - sigma_h(i) * d_dx(T_h, i) * dx &                 ! (σ ∂x T, φ)
   - (1.0d0 / Re) * d_dx(u_h, i) * dphi_dx * dx       ! viscous term
end do

do i = 2, N-1
    dmdt(i) = - RHS(i) / Mii(i)
end do



end module momentum
