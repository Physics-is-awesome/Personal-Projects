module kepler

    use iso_fortran_env, only : real64
    use init, only : nbody_state

    implicit none

    private

    public :: kepler_step


contains


    !============================================================
    !
    !  KEPLER STEP
    !
    !  Advances every Jacobi Kepler orbit by dt.
    !
    !  Q_i = Jacobi relative position
    !  P_i = canonical Jacobi momentum
    !
    !  mu_i = reduced mass
    !
    !  M_i-1 = total mass interior to body i
    !
    !============================================================

    subroutine kepler_step(state, dt)

        type(nbody_state), intent(inout) :: state
        real(real64),      intent(in)    :: dt

        integer :: i

        do i = 1, state%n - 1

            call kepler_drift( &
                state%qj(:,i), &
                state%pj(:,i), &
                state%mu(i), &
                state%M(i-1), &
                state%G, &
                dt )

        end do

    end subroutine kepler_step


    !============================================================
    !
    !  SINGLE KEPLER ORBIT
    !
    !  Solves
    !
    !       dQ/dt = P / mu
    !
    !       dP/dt = -G M mu Q / |Q|^3
    !
    !  using universal variables.
    !
    !============================================================

    subroutine kepler_drift(state, mu_red, Mcentral, G, dt)
        type(nbody_state), intent(inout) :: state

        real(real64), intent(in) :: mu_red
        real(real64), intent(in) :: Mcentral
        real(real64), intent(in) :: dt

        real(real64) :: r0
        real(real64) :: v2
        real(real64) :: rv

        real(real64) :: alpha
        real(real64) :: chi
        real(real64) :: chi_old

        real(real64) :: z
        real(real64) :: C
        real(real64) :: S

        real(real64) :: sqrt_mu

        real(real64) :: F
        real(real64) :: dF

        real(real64) :: f_cof
        real(real64) :: g_cof
        real(real64) :: fdot
        real(real64) :: gdot

        real(real64) :: Q0(3,state%n)
        real(real64) :: P0(3,state%n)
        real(real64) :: v0(3,state%n)

        real(real64) :: r

        integer :: iter


        !--------------------------------------------------------
        ! Save initial state
        !--------------------------------------------------------

        Q0 = state%qj
        P0 = state%pj


        !--------------------------------------------------------
        ! Convert canonical momentum to relative velocity
        !
        ! P = mu_red * v
        !
        ! Therefore
        !
        ! v = P / mu_red
        !--------------------------------------------------------

        v0 = P0 / mu_red


        !--------------------------------------------------------
        ! Initial radius and velocity quantities
        !--------------------------------------------------------

        r0 = sqrt(dot_product(Q0,Q0))

        v2 = dot_product(v0,v0)

        rv = dot_product(Q0,v0)


        !--------------------------------------------------------
        ! Gravitational parameter
        !
        ! lambda = G Mcentral
        !--------------------------------------------------------

        sqrt_mu = sqrt(G*Mcentral)


        !--------------------------------------------------------
        ! Universal-variable alpha
        !
        ! alpha = 2/r - v^2/(GM)
        !
        ! alpha > 0 : elliptic
        ! alpha = 0 : parabolic
        ! alpha < 0 : hyperbolic
        !--------------------------------------------------------

        alpha = 2.0_real64/r0 - v2/(G*Mcentral)


        !--------------------------------------------------------
        ! Initial guess for universal anomaly chi
        !--------------------------------------------------------

        if (abs(alpha) > 1.0e-12_real64) then

            chi = sqrt(G*Mcentral) * abs(alpha) * dt

        else

            chi = sqrt(G*Mcentral) * dt / r0

        end if


        !--------------------------------------------------------
        ! Newton iteration
        !
        ! Solve universal Kepler equation:
        !
        ! F(chi) = 0
        !--------------------------------------------------------

        do iter = 1, 100

            chi_old = chi

            z = alpha * chi * chi

            call stumpff_C(z, C)

            call stumpff_S(z, S)


            !----------------------------------------------------
            ! Universal Kepler equation
            !
            ! F =
            !
            ! (r0 v_r / sqrt(mu)) chi^2 C
            !
            ! + (1-alpha r0) chi^3 S
            !
            ! + r0 chi
            !
            ! - sqrt(mu) dt
            !----------------------------------------------------

            F = (rv/sqrt_mu) * chi*chi * C &
                + (1.0_real64-alpha*r0) * chi**3 * S &
                + r0*chi &
                - sqrt_mu*dt


            !----------------------------------------------------
            ! Derivative of universal Kepler equation
            !
            ! dF/dchi =
            !
            ! (r0 v_r / sqrt(mu)) chi(1-zS)
            !
            ! + (1-alpha r0) chi^2 C
            !
            ! + r0
            !----------------------------------------------------

            dF = (rv/sqrt_mu) * chi * (1.0_real64-z*S) &
                 + (1.0_real64-alpha*r0) * chi*chi*C &
                 + r0


            chi = chi - F/dF


            if (abs(chi-chi_old) < 1.0e-13_real64) exit

        end do


        !--------------------------------------------------------
        ! Lagrange f and g coefficients
        !
        ! Q(t+dt) = f Q0 + g v0
        !--------------------------------------------------------

        z = alpha * chi * chi

        call stumpff_C(z, C)

        call stumpff_S(z, S)


        f_cof = 1.0_real64 &
            - chi*chi/r0*C

        g_cof = dt &
            - chi**3/sqrt_mu*S


        !--------------------------------------------------------
        ! New position
        !--------------------------------------------------------

        state%qj = f*Q0 + g*v0


        !--------------------------------------------------------
        ! New radius
        !--------------------------------------------------------

        r = sqrt(dot_product(state%qj,state%qj))


        !--------------------------------------------------------
        ! Time derivatives of f and g
        !
        ! fdot = sqrt(mu)/(r*r0) (alpha chi^3 S - chi)
        !
        ! gdot = 1 - chi^2/r C
        !--------------------------------------------------------

        fdot = sqrt_mu/(r*r0) &
               * (alpha*chi**3*S - chi)

        gdot = 1.0_real64 &
               - chi*chi/r*C


        !--------------------------------------------------------
        ! New velocity
        !--------------------------------------------------------

        v0 = fdot*Q0 + gdot*v0


        !--------------------------------------------------------
        ! Convert velocity back to canonical momentum
        !
        ! P = mu_red * v
        !--------------------------------------------------------

        state%pj = mu_red*v0

    end subroutine kepler_drift


    !============================================================
    !
    !  STUMPFF C FUNCTION
    !
    !       C(z) = (1-cos(sqrt(z)))/z       z > 0
    !
    !       C(z) = (cosh(sqrt(-z))-1)/(-z) z < 0
    !
    !       C(0) = 1/2
    !
    !============================================================

    subroutine stumpff_C(z, C)

        real(real64), intent(in)  :: z
        real(real64), intent(out) :: C

        real(real64) :: x


        if (abs(z) < 1.0e-8_real64) then

            C = 0.5_real64 &
                - z/24.0_real64 &
                + z*z/720.0_real64 &
                - z*z*z/40320.0_real64

        else if (z > 0.0_real64) then

            x = sqrt(z)

            C = (1.0_real64-cos(x))/z

        else

            x = sqrt(-z)

            C = (cosh(x)-1.0_real64)/(-z)

        end if

    end subroutine stumpff_C


    !============================================================
    !
    !  STUMPFF S FUNCTION
    !
    !       S(z) = (sqrt(z)-sin(sqrt(z)))/sqrt(z)^3   z > 0
    !
    !       S(z) = (sinh(sqrt(-z))-sqrt(-z))/sqrt(-z)^3
    !                                                    z < 0
    !
    !       S(0) = 1/6
    !
    !============================================================

    subroutine stumpff_S(z, S)

        real(real64), intent(in)  :: z
        real(real64), intent(out) :: S

        real(real64) :: x


        if (abs(z) < 1.0e-8_real64) then

            S = 1.0_real64/6.0_real64 &
                - z/120.0_real64 &
                + z*z/5040.0_real64 &
                - z*z*z/362880.0_real64

        else if (z > 0.0_real64) then

            x = sqrt(z)

            S = (x-sin(x))/(x*x*x)

        else

            x = sqrt(-z)

            S = (sinh(x)-x)/(x*x*x)

        end if

    end subroutine stumpff_S


end module kepler
