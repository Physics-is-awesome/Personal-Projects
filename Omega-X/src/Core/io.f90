module io
    implicit none
    private
    public :: write_fields, write_conserved

    integer :: output_counter = 0

contains



    subroutine write_fields(time)
        use mesh, only: nx, x, dx
        use state, only: rho, u, e
        implicit none
        real, intent(in) :: time
        character(len=100) :: filename
        integer :: unit, i

        ! Format file name
        write(filename, '("output/fields_t", I4.4, ".dat")') output_counter
        unit = 10

        ! Open file and write header
        open(unit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') "# x     rho     u     e     T"

        ! Write data
        do i = 1, nx
            write(unit, '(F10.5, 3F10.5)') x(i), rho(i), u(i), e(i), time
        end do

        close(unit)
        output_counter = output_counter + 1
    end subroutine write_fields

    subroutine write_conserved(time)
        use functionals, only: H_val, S_val, compute_hamiltonian, compute_entropy
        implicit none
        real, intent(in) :: time
        integer :: unit
        logical :: exists

        call compute_hamiltonian()
        call compute_entropy()

        unit = 20

        ! Create or append to conserved.dat
        inquire(file="output/conserved.dat", exist=exists)
        if (.not. exists .or. output_counter == 0) then
            open(unit=unit, file="output/conserved.dat", status="replace", action="write")
            write(unit, '(A)') "# time     H       S"
        else
            open(unit=unit, file="output/conserved.dat", status="old", position="append", action="write")
        end if

        write(unit, '(F10.5, 2F15.8)') time, H_val, S_val
        close(unit)
    end subroutine write_conserved

end module io
