module transform
  use precision
  use init

  implicit none

contains
  subroutine cartesian_to_jacobi(state)

    type(nbody_state), intent(inout) :: state


    real(real64) :: Rsum(3)
    real(real64) :: Psum(3)

    real(real64) :: M_total
    real(real64) :: m_current

    integer :: i


    M_total = state%mass(1)

    Rsum = state%mass(1) * state%q(:,1)
    Psum = state%p(:,1)


    do i = 2, state%n

      m_current = state%mass(i)


      ! Jacobi position

      state%qj(:,i) = state%q(:,i) - Rsum / M_total


      ! Jacobi momentum

      state%pj(:,i) = (M_total / (M_total + m_current)) * state%p(:,i) &
        - (m_current / (M_total + m_current)) * Psum


      ! Update interior system

      Rsum = Rsum + m_current * state%q(:,i)

      Psum = Psum + state%p(:,i)

      M_total = M_total + m_current

    end do


    ! Center of mass

    state%qj(:,1) = Rsum / M_total

    state%pj(:,1) = Psum

  end subroutine cartesian_to_jacobi

  subroutine jacobi_to_cartesian(state)

    type(nbody_state), intent(inout) :: state


    real(real64) :: Rsum(3)
    real(real64) :: Psum(3)

    real(real64) :: M_total
    real(real64) :: M_inner
    real(real64) :: m_current

    integer :: i


    !-----------------------------------------------------------
    ! Total mass
    !-----------------------------------------------------------

    M_total = sum(state%mass)


    !-----------------------------------------------------------
    ! Total center-of-mass position
    !-----------------------------------------------------------

    Rsum = M_total * state%qj(:,1)


    !-----------------------------------------------------------
    ! Total momentum
    !-----------------------------------------------------------

    Psum = state%pj(:,1)


    !-----------------------------------------------------------
    ! Work from the outside inward
    !
    ! Inverting cartesian_to_jacobi's recurrence gives, at each
    ! step (with M_total = M_i, M_inner = M_{i-1} before Rsum is
    ! reduced):
    !
    !   q_i = Q_i * (M_inner/M_total) + Rsum/M_total
    !   p_i = P_i + (m_i/M_total) * Psum
    !
    ! (derived by solving cartesian_to_jacobi's Q_i/P_i equations
    ! for q_i/p_i in terms of R_i, M_i instead of R_{i-1}, M_{i-1};
    ! verified by round-tripping cartesian_to_jacobi ->
    ! jacobi_to_cartesian on test cases). The previous version was
    ! missing the (M_inner/M_total) factor on the position term and
    ! divided the momentum term by M_inner instead of M_total.
    !-----------------------------------------------------------

    do i = state%n, 2, -1

      m_current = state%mass(i)

      M_inner = M_total - m_current


      !-------------------------------------------------------
      ! Cartesian position
      !-------------------------------------------------------

      state%q(:,i) = (M_inner / M_total) * state%qj(:,i) + Rsum / M_total


      !-------------------------------------------------------
      ! Cartesian momentum
      !-------------------------------------------------------

      state%p(:,i) = state%pj(:,i) &
        + (m_current / M_total) * Psum


      !-------------------------------------------------------
      ! Remove body i from the interior system
      !-------------------------------------------------------

      Rsum = Rsum - m_current * state%q(:,i)

      Psum = Psum - state%p(:,i)

      M_total = M_inner

    end do


    !-----------------------------------------------------------
    ! Central body
    !-----------------------------------------------------------

    state%q(:,1) = Rsum / M_total

    state%p(:,1) = Psum

  end subroutine jacobi_to_cartesian
end module transform
