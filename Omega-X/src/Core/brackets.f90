module brackets
    use state
    use functionals
    use mesh
    implicit none

contains

!=========================================================
! Compute total bracket updates:
!   ∂t z = {z, H} + {{{z, S, H, z}}}
!=========================================================
subroutine compute_brackets()
    call zero_derivatives()
    call compute_dH()
    call compute_dS()
    call poisson_bracket()
    call metric_bracket()
end subroutine compute_brackets


!=========================================================
! Antisymmetric Poisson bracket evolution
!
! Paper form (for ideal 1D compressible fluid):
!   ∂t ρ = -∂x (ρ u)
!   ∂t u = -∂x (u²/2 + h)    ; h = e + p/ρ
!   ∂t e = -∂x (e u + p u/ρ)
!
! Note: This is manually implemented — no formal PB used yet.
!=========================================================
subroutine poisson_bracket()
    integer :: i

    do i = 2, nx-1
        ! Upwinded fluxes (naive discretization)
        drho_dt(i) = -(rho(i+1)*u(i+1) - rho(i-1)*u(i-1)) / (x(i+1) - x(i-1))

        ! Compute pressure and enthalpy
        ! h = e + p/ρ = e + (γ - 1)e = γ e
        du_dt(i) = -((0.5 * u(i+1)**2 + gamma * e(i+1)) - (0.5 * u(i-1)**2 + gamma * e(i-1))) / (x(i+1) - x(i-1))

        ! Total energy flux
        de_dt(i) = -((e(i+1) * u(i+1) + pressure(i+1) * u(i+1)/rho(i+1)) - &
                     (e(i-1) * u(i-1) + pressure(i-1) * u(i-1)/rho(i-1))) / (x(i+1) - x(i-1))
    end do
end subroutine poisson_bracket


!=========================================================
! Metric 4-bracket evolution:
!   ∂t z = {{{z, S, H, z}}}
!
! Paper uses:
!   ∂t u = ∂x (μ ∂x δS/δu)
!   ∂t e = ∂x (κ ∂x δS/δe)
!
! Only affects u and e — not ρ
!
! ⚠️ μ (viscosity) and κ (conductivity) need to be set
!=========================================================
subroutine metric_bracket()
    integer :: i
    real :: mu, kappa
    real, dimension(nx) :: gradS_du, gradS_de

    mu = 0.01     ! ⚠️ Set based on physical regime
    kappa = 0.01  ! ⚠️ Set based on physical regime

    ! Compute gradients of δS/δu and δS/δe
    do i = 2, nx-1
        gradS_du(i) = (dS_du(i+1) - dS_du(i-1)) / (x(i+1) - x(i-1))
        gradS_de(i) = (dS_de(i+1) - dS_de(i-1)) / (x(i+1) - x(i-1))
    end do

    ! Apply metric bracket terms
    do i = 2, nx-1
        du_dt(i) = du_dt(i) + (mu * (gradS_du(i+1) - gradS_du(i-1))) / (x(i+1) - x(i-1))
        de_dt(i) = de_dt(i) + (kappa * (gradS_de(i+1) - gradS_de(i-1))) / (x(i+1) - x(i-1))
    end do
end subroutine metric_bracket

end module brackets
