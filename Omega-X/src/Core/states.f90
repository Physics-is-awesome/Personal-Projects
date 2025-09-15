module state
    use mesh
    implicit none

    ! ----------------------------------------------
    ! Field variables (at each node)
    ! ----------------------------------------------
    real, dimension(nx) :: rho      ! Mass density
    real, dimension(nx) :: u        ! Velocity
    real, dimension(nx) :: e        ! Specific internal energy

    ! ----------------------------------------------
    ! Time derivatives (for bracket updates)
    ! ----------------------------------------------
    real, dimension(nx) :: drho_dt
    real, dimension(nx) :: du_dt
    real, dimension(nx) :: de_dt

    ! ----------------------------------------------
    ! Parameters for thermodynamics
    ! ----------------------------------------------
    real, parameter :: gamma = 1.4   ! Ideal gas gamma

contains

    ! -------------------------------------------------
    ! Initialize fields with default profile
    ! -------------------------------------------------
    subroutine initialize_state()
        integer :: i

        do i = 1, nx
            ! Example: centered density bump, rest velocity, uniform internal energy
            rho(i) = 1.0 + 0.2 * exp( -100.0 * (x(i) - 0.5)**2 )
            u(i)   = 0.0
            e(i)   = 2.5   ! Adjust to give desired pressure
        end do
    end subroutine initialize_state

    ! -------------------------------------------------
    ! Reset time derivatives before each bracket update
    ! -------------------------------------------------
    subroutine zero_derivatives()
        drho_dt = 0.0
        du_dt   = 0.0
        de_dt   = 0.0
    end subroutine zero_derivatives

    ! -------------------------------------------------
    ! Compute pressure (ideal gas)
    ! -------------------------------------------------
    function pressure(i) result(p)
        integer, intent(in) :: i
        real :: p
        p = (gamma - 1.0) * rho(i) * e(i)
    end function pressure

    ! -------------------------------------------------
    ! Compute temperature (optional)
    ! -------------------------------------------------
    function temperature(i) result(T)
        integer, intent(in) :: i
        real :: T
        T = (gamma - 1.0) * e(i)
    end function temperature

end module state
