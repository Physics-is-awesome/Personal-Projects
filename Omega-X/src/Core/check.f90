module state_checker
    implicit none
    real, allocatable :: rho(:), u(:), e(:)
    integer :: nx

contains

    subroutine init_states(n)
        integer, intent(in) :: n
        nx = n
        allocate(rho(nx), u(nx), e(nx))
    end subroutine init_states

    ! Generic setter with NaN check
    subroutine set_rho(new_rho)
        real, intent(in) :: new_rho(:)
        rho = new_rho
        call check_nan(rho, "rho")
    end subroutine set_rho

    subroutine set_u(new_u)
        real, intent(in) :: new_u(:)
        u = new_u
        call check_nan(u, "u")
    end subroutine set_u

    subroutine set_e(new_e)
        real, intent(in) :: new_e(:)
        e = new_e
        call check_nan(e, "e")
    end subroutine set_e

    ! Reuse your previous check_nan subroutine here
    subroutine check_nan(arr, name)
        real, intent(in) :: arr(:)
        character(len=*), intent(in) :: name
        integer :: i
        do i = 1, size(arr)
            if (arr(i) /= arr(i)) then
                print *, "NaN detected in ", name, " at index ", i
                stop 1
            end if
        end do
    end subroutine check_nan

end module state_checker
