module interaction
  use precision
  use transform
  implicit none

contains
  subroutine jacobi_half(state, dt)
    type(nbody_state), intent(inout) :: state
    real(real64),      intent(in)    :: dt

    real(real64) :: qj2
    real(real64) :: qjmag
    real(real64) :: force(3)

    integer :: i

    do i =3, state%n

      qj2 = dot_product(state%qj(:,i), state%qj(:,i))

      qjmag = sqrt(qj2)

      force = state%G * state%mass(i) * state%M * state%qj(:,i) / (qj2*qjmag)

      state%pj(:,i) = state%pj(:,i) + dt * force
    end do
  end subroutine jacobi_half
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cartesian_half(state, dt)
    type(nbody_state), intent(inout) :: state
    real(real64),      intent(in)    :: dt

    real(real64) :: dr(3) ! displacement vector between objects
    real(real64) :: r2
    real(real64) :: inv_r3
    real(real64) :: force(3)

    integer :: i, j

    do i = 1, state%n-1
      do j = i+1, state%n

        ! Exclude interactions involving body 2
        if (i == 2 .or. j == 2) cycle

        dr = state%p(:,i) - state%p(:,j)

        r2 = dot_product(dr, dr)

        inv_r3 = 1.0 / (r2 * sqrt(r2))

        force = -state%G * state%mass(i) * state%mass(j) * inv_r3 * dr

        state%p(:,i) = state%p(:,i) + dt * force
        state%p(:,j) = state%p(:,j) - dt * force
      end do
    end do
  end subroutine cartesian_half


end module interaction
