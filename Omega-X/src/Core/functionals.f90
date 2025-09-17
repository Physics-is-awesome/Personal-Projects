module functionals
    use state
    use mesh
    implicit none

    ! Functionals: Hamiltonian (H) and Entropy (S)
    real :: H_val, S_val
    real, dimension(nx) :: dH_drho, dH_du, dH_de
    real, dimension(nx) :: dS_drho, dS_du, dS_de

contains

!===================================================================
! Compute total Hamiltonian functional
! Paper definition:
!   H = ∫ dx (½ ρ u² + ρ e)
!===================================================================
subroutine compute_hamiltonian()
    integer :: i
    H_val = 0.0
    do i = 1, nx
        H_val = H_val + (0.5 * rho(i) * u(i)**2 + rho(i) * e(i)) * dx(i)
    end do
end subroutine compute_hamiltonian


!===================================================================
! Compute entropy functional
! Paper definition:
!   S = ∫ dx ρ s(ρ, e)
! with:
!   s(ρ, e) = log(e / ρ^γ)
!         => ρ s = ρ log(e) - γ ρ log(ρ)
!===================================================================
subroutine compute_entropy()
    integer :: i
    real :: s_i

    S_val = 0.0
    do i = 1, nx
        if (rho(i) > 1e-12 .and. e(i) > 1e-12) then
            s_i = log(e(i)) - gamma * log(rho(i))   ! specific entropy
        else
            s_i = 0.0   ! avoid log(0)
        end if
        S_val = S_val + rho(i) * s_i * dx(i)
    end do
end subroutine compute_entropy


!===================================================================
! Variational Derivatives of Hamiltonian
! From paper:
! δH/δρ = ½ u² + e
! δH/δu = ρ u
! δH/δe = ρ
!===================================================================
subroutine compute_dH()
    integer :: i

    do i = 1, nx
        dH_drho(i) = 0.5 * u(i)**2 + e(i)
        dH_du(i)   = rho(i) * u(i)
        dH_de(i)   = rho(i)
    end do
end subroutine compute_dH


!===================================================================
! Variational Derivatives of Entropy
! From paper:
! δS/δρ = s + ρ ∂s/∂ρ = log(e) - γ log(ρ) - γ
! δS/δu = 0
! δS/δe = ρ ∂s/∂e = ρ / e
!
! ✅ Note: This assumes s(ρ,e) = log(e / ρ^γ)
!===================================================================
subroutine compute_dS()
    integer :: i

    do i = 1, nx
        if (rho(i) > 1e-12 .and. e(i) > 1e-12) then
            dS_drho(i) = log(e(i)) - gamma * log(rho(i)) - gamma
            dS_du(i)   = 0.0
            dS_de(i)   = rho(i) / e(i)
        else
            dS_drho(i) = 0.0
            dS_du(i)   = 0.0
            dS_de(i)   = 0.0
        end if
    end do
end subroutine compute_dS

end module functionals
