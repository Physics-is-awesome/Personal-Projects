module io
    use state
    use functionals
    use mesh
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer :: output_counter = 0

contains

!=========================================================
! Write full field output to file
!=========================================================
subroutine write_state()
    character(len=50) :: filename
    integer :: i
    real :: T, s

    call compute_hamiltonian()
    call compute_entropy()

    write(filename, '("output/fields_t", I4.4, ".dat")') output_counter
    open(unit=10, file=filename, status="replace")

    write(10, '(A)') "# x    rho     u      e     T     s"

    do i = 1, nx
        if (rho(i) > 1e-12 .and. e(i) > 1e-12) then
            T = temperature(i)
            s = log(e(i)) - gamma * log(rho(i))
        else
            T = 0.0
            s = 0.0
            print*, 'False number in(tempature or entropy ' x(i)
        end if
        write(10, '(F10.5, 5F10.5)') x(i), rho(i), u(i), e(i), T, s
        if (ieee_is_nan(rho(i))) then
            print*, "Rho is false in " x(i)
        end if
        if (ieee_is_nan(u(i))) then
            print*, 'u is false in ' x(i)
        end if
        if (ieee_is_nan(e(i))) then
            print*, 'e is false in ' x(i)
        end if
        
    end do

    close(10)


end subroutine write_state


!=========================================================
! Append conserved quantities H and S to file
!=========================================================
subroutine write_conserved(time)
    real, intent(in) :: time
    integer :: unit

    call compute_hamiltonian()
    call compute_entropy()

    unit = 20
    if (output_counter == 0) then
        open(unit=unit, file="output/conserved.dat", status="replace")
        write(unit, '(A)') "# time     H       S"
    else
        open(unit=unit, file="output/conserved.dat", status="old", position="append")
    end if

    write(unit, '(F10.5, 2F15.8)') time, H_val, S_val

    close(unit)
    output_counter = output_counter + 1
end subroutine write_conserved

end module io
