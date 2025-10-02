program omega_x_driver
  use mesh
  use states
  use time_integrator
  use io
  implicit none

  real(8) :: dt, t, t_end
  integer :: step
  character(len=100) :: fname
  
  dt = 1.0d-3
  t_end = 1.0d0
  t = 0.0d0

  call initialize_mesh()
  call initialize_states()

  do while (t < t_end)
    call advance_one_step(dt)
    t = t + dt
    print *, 't = ', t
  end do



  step = step + 1
  if (mod(step, 100) == 0) then
    write(fname, '(A,I4.4,A)') "output/state_", step, ".csv"
    call write_state_to_csv(fname)
  end if

end program omega_x_driver
