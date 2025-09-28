module momentum
  use mesh
  use states
  implicit none
  real(8), intent(in)  :: p_h(N), dx(u_h, i), dx, Mii
  real(8), intent(out) :: dpdt(N)
  real(8) :: mass_RHS(N)
  integer :: i

do i = 2, N-1
    mass_RHS(i) =  - p_h(i) * d_dx(u_h, i) * dx                     ! (p ∂x u, φ)

end do

do i = 2, N-1
    dpdt(i) = - mass_RHS(i) / Mii(i)
end do



end module momentum
