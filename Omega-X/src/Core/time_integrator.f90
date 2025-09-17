module time_integrator
    use state
    use brackets
    implicit none

    real :: time = 0.0

contains




!--------------------------------------------------------
! Midpoint method (RK2):
!   z* = z^n + 0.5*dt * dz/dt
!   dz/dt at midpoint → z^{n+1} = z^n + dt * dz/dt(z*)
!
! Note: This is better for preserving structure
!--------------------------------------------------------
subroutine time_step_midpoint(dt)
    real, intent(in) :: dt
    integer :: i
    real, dimension(nx) :: rho_tmp, u_tmp, e_tmp
    real, dimension(nx) :: drho_dt_tmp, du_dt_tmp, de_dt_tmp

    ! Stage 1
    call compute_brackets()

    do i = 1, nx
        rho_tmp(i) = rho(i) + 0.5 * dt * drho_dt(i)
        u_tmp(i)   = u(i)   + 0.5 * dt * du_dt(i)
        e_tmp(i)   = e(i)   + 0.5 * dt * de_dt(i)
    end do

    ! Replace state with midpoint state
    !call overwrite_state(rho_tmp, u_tmp, e_tmp)

    ! Stage 2
    call compute_brackets()

    ! Update original state
    do i = 1, nx
        rho(i) = rho(i) + dt * drho_dt(i)
        u(i)   = u(i)   + dt * du_dt(i)
        e(i)   = e(i)   + dt * de_dt(i)
        print*, 'e is', e
    end do

    time = time + dt
end subroutine time_step_midpoint


!--------------------------------------------------------
! Utility to overwrite current state
!--------------------------------------------------------
!subroutine overwrite_state(rho_new, u_new, e_new)
 !   real, intent(in) :: rho_new(nx), u_new(nx), e_new(nx)
  !  integer :: i

   ! do i = 1, nx
    !    rho = rho_new
     !   u = u_new
      !  e = e_new
    !end do
    

!end subroutine overwrite_state

end module time_integrator
