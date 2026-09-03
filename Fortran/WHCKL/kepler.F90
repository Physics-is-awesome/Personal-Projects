module kepler

    use iso_fortran_env, only : real64
    use init, only : nbody_state

    implicit none

    private

    public :: kepler_step

contains

    !============================================================
    !  KEPLER STEP — advance every Jacobi orbit (i = 2..n) by dt
    !============================================================

    subroutine kepler_step(state, dt)

        type(nbody_state), intent(inout) :: state
        real(real64),      intent(in)    :: dt

        integer :: i

        do i = 2, state%n

            call kepler_drift( &
                state%qj(:,i), &
                state%pj(:,i), &
                state%mu(i-1), &   ! mu is allocated (n-1); orbit i uses slot i-1
                state%M(i),   &   ! interior mass THROUGH body i, not i-1
                state%G, &
                dt )

        end do

    end subroutine kepler_step


    !============================================================
    !  SINGLE KEPLER ORBIT (universal variables)
    !
    !  Operates on ONE Jacobi vector (Q, P), not the full state.
    !============================================================

    subroutine kepler_drift(Q, P, mu_red, Mcentral, G, dt)

        real(real64), intent(inout) :: Q(3)
        real(real64), intent(inout) :: P(3)

        real(real64), intent(in) :: mu_red
        real(real64), intent(in) :: Mcentral
        real(real64), intent(in) :: G
        real(real64), intent(in) :: dt

        real(real64) :: r0, v2, rv
        real(real64) :: alpha, chi, chi_old
        real(real64) :: z, C, S
        real(real64) :: sqrt_mu
        real(real64) :: F, dF
        real(real64) :: f_cof, g_cof, fdot, gdot
        real(real64) :: Q0(3), v0(3)
        real(real64) :: r

        integer :: iter
        logical :: converged
        real(real64) :: rv_over_sqrt_mu, one_minus_alpha_r0, sqrt_mu_dt
        real(real64) :: chi2, chi3

        Q0 = Q
        v0 = P / mu_red

        r0 = sqrt(dot_product(Q0,Q0))
        v2 = dot_product(v0,v0)
        rv = dot_product(Q0,v0)

        sqrt_mu = sqrt(G*Mcentral)

        alpha = 2.0_real64/r0 - v2/(G*Mcentral)

        if (abs(alpha) > 1.0e-12_real64) then
            chi = sqrt_mu * alpha * dt
        else
            chi = sqrt_mu * dt / r0
        end if

        ! Loop-invariant (don't depend on chi) -- hoisted out of the
        ! Newton iteration below. rv_over_sqrt_mu in particular
        ! replaces a division that was being repeated on every
        ! iteration (for both F and dF) with one computed up front.
        rv_over_sqrt_mu   = rv / sqrt_mu
        one_minus_alpha_r0 = 1.0_real64 - alpha*r0
        sqrt_mu_dt         = sqrt_mu * dt

        converged = .false.

        do iter = 1, 100

            chi_old = chi

            chi2 = chi*chi
            chi3 = chi2*chi

            z = alpha * chi2
            call stumpff_CS(z, C, S)

            F = rv_over_sqrt_mu * chi2 * C &
                + one_minus_alpha_r0 * chi3 * S &
                + r0*chi &
                - sqrt_mu_dt

            dF = rv_over_sqrt_mu * chi * (1.0_real64-z*S) &
                 + one_minus_alpha_r0 * chi2*C &
                 + r0

            chi = chi - F/dF

            if (abs(chi-chi_old) < 1.0e-13_real64) then
                converged = .true.
                exit
            end if

        end do

        if (.not. converged) then
            print *, "WARNING: kepler_drift did not converge, chi residual = ", &
                      abs(chi-chi_old)
        end if

        z = alpha * chi * chi
        call stumpff_CS(z, C, S)

        f_cof = 1.0_real64 - chi*chi/r0*C
        g_cof = dt - chi**3/sqrt_mu*S

        Q = f_cof*Q0 + g_cof*v0

        r = sqrt(dot_product(Q,Q))

        fdot = sqrt_mu/(r*r0) * (alpha*chi**3*S - chi)
        gdot = 1.0_real64 - chi*chi/r*C

        v0 = fdot*Q0 + gdot*v0

        P = mu_red*v0

    end subroutine kepler_drift


    !============================================================
    !  Combined Stumpff functions C(z), S(z), via argument
    !  reduction + duplication (not direct cos/cosh(sqrt(z))).
    !
    !  Same idea WHFast itself uses (see stumpff_cs in
    !  integrator_whfast.c): repeatedly halve the argument
    !  (z -> z/4) until it's small enough for a Taylor series to
    !  be accurate, then reconstruct C(z), S(z) at the original
    !  argument via the duplication identities
    !
    !    C(4w) = C(w) - (w/2)*C(w)^2
    !    S(4w) = [S(w) + C(w) - w*S(w)*C(w)] / 4
    !
    !  (derived from the half-angle trig/hyperbolic identities for
    !  C = c2, S = c3; verified numerically against the direct
    !  cos/cosh formulas for both signs of z before use here).
    !
    !  This replaces two separate transcendental-heavy subroutine
    !  calls (stumpff_C, stumpff_S — each with its own cos/cosh)
    !  with a single call that shares the reduction work and
    !  avoids the precision loss / premature overflow of evaluating
    !  cos(x)/cosh(x) directly at a large x.
    !============================================================

    subroutine stumpff_CS(z, C, S)

        real(real64), intent(in)  :: z
        real(real64), intent(out) :: C
        real(real64), intent(out) :: S

        real(real64) :: w, Cnew, Snew
        integer :: n, k

        w = z
        n = 0
        do while (abs(w) > 0.1_real64)
            w = w * 0.25_real64
            n = n + 1
        end do

        ! Taylor series, safe since |w| <= 0.1
        C = 0.5_real64 - w/24.0_real64 + w*w/720.0_real64 &
            - w**3/40320.0_real64 + w**4/3628800.0_real64
        S = 1.0_real64/6.0_real64 - w/120.0_real64 + w*w/5040.0_real64 &
            - w**3/362880.0_real64 + w**4/39916800.0_real64

        do k = 1, n
            Cnew = C - 0.5_real64*w*C*C
            Snew = (S + C - w*S*C) * 0.25_real64
            C = Cnew
            S = Snew
            w = w * 4.0_real64
        end do

    end subroutine stumpff_CS


end module kepler
