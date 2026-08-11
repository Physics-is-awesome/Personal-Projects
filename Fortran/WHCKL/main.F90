PROGRAM WHCKL
  use init
  use kepler
  use init_val
  use corrector
  use percision
  use interaction

  IMPLICIT NONE
  integer :: total_t
  integer :: iterations

  total_t = 100

  iterations = total_t / dt

  call init(state, n)

  call initial_fundemental_values(state)

  call corrector_11(state, h)

  do i = 0, iterations
    call cartesian_to_jacobi(state)

    call kepler_step(state, dt * 0.5_real64)

    call jacobi_half(state, dt * 0.5_real64)

    call jacobi_to_cartesian(state)

    call cartesian_half(state, dt)

    call cartesian_to_jacobi(state)

    call jacobi_half(state, dt * 0.5_real64)

    call kepler_step(state, dt * 0.5_real64)


  end do

END PROGRAM WHCKL
