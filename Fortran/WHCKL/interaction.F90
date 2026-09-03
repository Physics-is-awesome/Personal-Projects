module interaction
  use precision
  use transform
  implicit none

contains

  !==============================================================
  ! INTERACTION KICK — full pairwise Newtonian force, all bodies,
  ! applied directly to CARTESIAN momenta (which are canonically
  ! conjugate to Cartesian positions, so this is an exact kick
  ! for the interaction Hamiltonian). Kept for reference/testing;
  ! jacobi_interaction_kick below does the same physics with half
  ! the coordinate-transform work (see its header comment).
  !==============================================================

  subroutine cartesian_half(state, dt)
    type(nbody_state), intent(inout) :: state
    real(real64),      intent(in)    :: dt

    real(real64) :: dr(3)     ! displacement vector between objects
    real(real64) :: r2
    real(real64) :: inv_r3
    real(real64) :: force(3)

    integer :: i, j

    ! Only mutual planet-planet terms (i, j >= 2). Star-planet
    ! gravity is already handled exactly by kepler_step's Kepler
    ! drift; including the star here would apply that force a
    ! second time as a discrete kick, injecting spurious energy
    ! every step.
    do i = 2, state%n-1
      do j = i+1, state%n

        dr = state%q(:,i) - state%q(:,j)

        r2 = dot_product(dr, dr)

        inv_r3 = 1.0_real64 / (r2 * sqrt(r2))

        force = -state%G * state%mass(i) * state%mass(j) * inv_r3 * dr

        state%p(:,i) = state%p(:,i) + dt * force
        state%p(:,j) = state%p(:,j) - dt * force
      end do
    end do
  end subroutine cartesian_half


  !==============================================================
  ! JACOBI_INTERACTION_KICK — same physics as cartesian_half, but
  ! done directly in Jacobi phase space instead of round-tripping
  ! full position+momentum through Cartesian coordinates.
  !
  ! Since the interaction Hamiltonian only depends on position, the
  ! kick only changes momenta: dP_i/dt = -dH_int/dQ_i. Because the
  ! Jacobi<->Cartesian position map is linear (q = L(Q)) and the
  ! transform is canonical, the Jacobi momenta and Cartesian momenta
  ! are related by P = L^T p (verified numerically against
  ! cartesian_to_jacobi's momentum formula via finite differences,
  ! matching to ~1e-10, i.e. to the finite-difference step size).
  ! The same identity applies to forces: dP/dt = L^T F, where F is
  ! the ordinary Cartesian pairwise force. And L^T is *exactly* the
  ! linear combination cartesian_to_jacobi already uses to turn
  ! Cartesian momenta into Jacobi momenta -- so that formula is
  ! reused below verbatim with F substituted for p.
  !
  ! This needs only a POSITION-only Jacobi->Cartesian conversion
  ! (no momentum work) plus the momentum-shaped recursion applied to
  ! forces -- roughly half the transform work of converting full
  ! position+momentum state to Cartesian and back, and it never
  ! touches state%q/state%p at all.
  !==============================================================

  subroutine jacobi_interaction_kick(state, dt)
    type(nbody_state), intent(inout) :: state
    real(real64),      intent(in)    :: dt

    real(real64) :: q(3, state%n)
    real(real64) :: F(3, state%n)
    real(real64) :: dP(3, state%n)

    real(real64) :: Rsum(3), Fsum(3)
    real(real64) :: M_total, M_inner, m_current
    real(real64) :: dr(3), r2, inv_r3, force(3)

    integer :: i, j

    !----------------------------------------------------------
    ! Jacobi -> Cartesian, POSITIONS ONLY
    !----------------------------------------------------------
    M_total = sum(state%mass)
    Rsum = M_total * state%qj(:,1)

    do i = state%n, 2, -1
      m_current = state%mass(i)
      M_inner = M_total - m_current
      q(:,i) = (M_inner/M_total)*state%qj(:,i) + Rsum/M_total
      Rsum = Rsum - m_current*q(:,i)
      M_total = M_inner
    end do

    q(:,1) = Rsum / M_total


    !----------------------------------------------------------
    ! Pairwise Cartesian forces, mutual planet-planet only
    ! (i, j >= 2) -- star excluded, same reasoning as
    ! cartesian_half.
    !----------------------------------------------------------
    F = 0.0_real64

    do i = 2, state%n-1
      do j = i+1, state%n

        dr = q(:,i) - q(:,j)
        r2 = dot_product(dr, dr)
        inv_r3 = 1.0_real64 / (r2 * sqrt(r2))

        force = -state%G * state%mass(i) * state%mass(j) * inv_r3 * dr

        F(:,i) = F(:,i) + force
        F(:,j) = F(:,j) - force
      end do
    end do


    !----------------------------------------------------------
    ! dP_i/dt = L^T F, using the same recursion as
    ! cartesian_to_jacobi's momentum formula, with F in place
    ! of the Cartesian momenta p.
    !----------------------------------------------------------
    M_total = state%mass(1)
    Fsum = F(:,1)   ! star feels no direct force here; = 0

    do i = 2, state%n
      m_current = state%mass(i)

      dP(:,i) = (M_total / (M_total + m_current)) * F(:,i) &
              - (m_current / (M_total + m_current)) * Fsum

      Fsum = Fsum + F(:,i)
      M_total = M_total + m_current
    end do


    !----------------------------------------------------------
    ! Apply kick directly to Jacobi momenta. state%qj is
    ! untouched (interaction Hamiltonian doesn't depend on
    ! momentum), and state%q/state%p are untouched too -- no
    ! frame-switch needed before or after this call.
    !----------------------------------------------------------
    do i = 2, state%n
      state%pj(:,i) = state%pj(:,i) + dt * dP(:,i)
    end do

  end subroutine jacobi_interaction_kick

end module interaction
