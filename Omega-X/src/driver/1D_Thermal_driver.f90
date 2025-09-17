program main_driver
    use mesh
    use state
    use assembly
    use time_integrator
    use io
    implicit none

    !------------------------------------------
    ! Parameters
    !------------------------------------------
    integer, parameter :: nsteps = 1000
    real,    parameter :: dt     = 0.001
    integer, parameter :: output_interval = 20

    integer :: step


    !------------------------------------------
    ! Initialization
    !------------------------------------------
    print *, "Initializing mesh..."
    call initialize_mesh()

    print *, "Initializing state..."
    call initialize_state()

    print *, "Assembling FEM matrices..."
    call assemble_matrices()

    print *, "Beginning simulation..."
    call write_fields(time)
    call write_conserved(time)

    !------------------------------------------
    ! Time Loop
    !------------------------------------------
    do step = 1, nsteps
        call time_step_midpoint(dt)

        if (mod(step, output_interval) == 0) then
            print *, "Step:", step, "Time:", time
            call write_fields(time)
            call write_conserved(time)
        end if
    end do

    !------------------------------------------
    ! Final Output
    !------------------------------------------
    call write_fields(time)
    call write_conserved(time)

    print *, "Simulation complete."

end program main_driver
