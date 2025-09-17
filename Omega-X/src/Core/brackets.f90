
module brackets
    use state
    implicit none

contains

    subroutine compute_rhs()
        integer :: i

        ! Hamiltonian evolution (simplified 1D MHD)
        do i = 2, nx-1
            drho_dt(i) = - (rho(i+1)*v(i+1) - rho(i-1)*v(i-1)) / 2.0
            dv_dt(i)   = - (v(i+1)**2 - v(i-1)**2)/2.0 - (s(i+1)-s(i-1))/2.0 - (B(i+1)**2 - B(i-1)**2)/2.0
            dB_dt(i)   = - (v(i+1)*B(i+1) - v(i-1)*B(i-1)) / 2.0
        end do

        ! Metric bracket (dissipation of B, production of entropy)
        do i = 2, nx-1
            ds_dt(i) = 0.001 * ((s(i+1) - 2.0*s(i) + s(i-1))) ! diffusion
            dB_dt(i) = dB_dt(i) + 0.001 * (B(i+1) - 2.0*B(i) + B(i-1)) ! resistivity
        end do

    end subroutine compute_rhs

end module brackets
