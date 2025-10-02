module entropy_1d
  implicit none
  contains 


  subroutine compute_entropy_rhs(N, sigma_h, u_h, T_h, dx, Re, Pr, gamma, rhs)
    use mesh_1d
    use projection_matrix_1d
    integer, intent(in) :: N
    real(8), intent(in)  :: sigma_h(N), u_h(N), T_h(N)
    real(8), intent(out) :: rhs(N)

    ! Temporary fields
    real(8) :: dx_u(N), dx_T(N)
    real(8) :: visc_prod(N), heat_grad(N), heat_prod(N)
    real(8) :: heat_term(N), flux(N)
    integer :: i

    ! Physical constants
    real(8), parameter :: Re = 100.0d0
    real(8), parameter :: Pr = 1.0d0
    real(8), parameter :: gamma = 1.4d0

    ! Step 1: compute flux = σ u
    do i = 1, N
      flux(i) = sigma_h(i) * u_h(i)
    end do

    ! Step 2: Project derivative of flux: ∂x(σ u)
    call apply_weak_derivative(flux, rhs)

    ! Step 3: Compute derivative fields
    call apply_weak_derivative(u_h, dx_u)
    call apply_weak_derivative(T_h, dx_T)

    ! Step 4: Entropy production terms
    do i = 1, N
      ! Viscous entropy production: (∂x u)^2 / T
      visc_prod(i) = (dx_u(i)**2) / T_h(i)

      ! Heat conduction entropy production
      heat_grad(i) = dx_T(i) / T_h(i)          ! (∂x T)/T
      heat_prod(i) = (dx_T(i)**2) / (T_h(i)**2) ! (∂x T)^2 / T^2

      heat_term(i) = (gamma / (gamma - 1.0d0)) * (heat_prod(i) - heat_grad(i) / dx)

      ! Add entropy production terms to RHS
      rhs(i) = rhs(i) - (1.0d0 / Re) * visc_prod(i) * dx
      rhs(i) = rhs(i) - (1.0d0 / (Re * Pr)) * heat_term(i) * dx
    end do

    ! Step 5: Apply mass matrix inverse (lumped)
    do i = 1, N
      rhs(i) = -rhs(i) / dx
    end do

    ! Optional: boundary conditions (e.g., no entropy flux)
    rhs(1) = 0.0d0
    rhs(N) = 0.0d0
  end subroutine compute_entropy_rhs
end module entropy_1d
