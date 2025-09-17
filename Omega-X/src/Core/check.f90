module runtime_checks
    use iso_fortran_env, only: real64
    implicit none
contains

    !------------------------------
    ! Generic NaN/Inf checker
    !------------------------------
    subroutine check_array(name, arr)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            if (.not. ieee_is_finite(arr(i))) then
                write(*,'(A, I5, A, F15.8)') 'Runtime NaN/Inf detected in ', trim(name), i, arr(i)
                stop 999
            end if
        end do
    end subroutine check_array

    !------------------------------
    ! Example setter for rho
    !------------------------------
    subroutine set_rho(rho, val)
        real(real64), intent(inout) :: rho(:)
        real(real64), intent(in) :: val(:)
        rho = val
        call check_array('rho', rho)
    end subroutine set_rho

    ! Similarly for u
    subroutine set_u(u, val)
        real(real64), intent(inout) :: u(:)
        real(real64), intent(in) :: val(:)
        u = val
        call check_array('u', u)
    end subroutine set_u

    ! Similarly for e
    subroutine set_e(e, val)
        real(real64), intent(inout) :: e(:)
        real(real64), intent(in) :: val(:)
        e = val
        call check_array('e', e)
    end subroutine set_e

    ! H and S
    subroutine set_H(H, val)
        real(real64), intent(inout) :: H
        real(real64), intent(in) :: val
        H = val
        if (.not. ieee_is_finite(H)) then
            write(*,'(A,F15.8)') 'Runtime NaN/Inf detected in H: ', H
            stop 999
        end if
    end subroutine set_H

    subroutine set_S(S, val)
        real(real64), intent(inout) :: S
        real(real64), intent(in) :: val
        S = val
        if (.not. ieee_is_finite(S)) then
            write(*,'(A,F15.8)') 'Runtime NaN/Inf detected in S: ', S
            stop 999
        end if
    end subroutine set_S

end module runtime_checks
