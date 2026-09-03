module symplectic_corrector

    use precision
    use init_val
    use kepler
    use interaction
    use transform

    implicit none

    private

    public :: corrector_11
    public :: inverse_corrector_11


    !===========================================================
    ! 11th-order corrector coefficients
    !===========================================================

    real(real64), parameter :: a1 = &
        0.41833001326703777398908601289259374469640768464934_real64

    real(real64), parameter :: a2 = &
        0.83666002653407554797817202578518748939281536929867_real64

    real(real64), parameter :: a3 = &
        1.2549900398011133219672580386777812340892230539480_real64

    real(real64), parameter :: a4 = &
        1.6733200530681510959563440515703749787856307385973_real64

    real(real64), parameter :: a5 = &
        2.0916500663351888699454300644629687234820384232467_real64


    real(real64), parameter :: b1 = &
        0.00020361579647854651301632818774633716473696537436847_real64

    real(real64), parameter :: b2 = &
       -0.0023487215292295354188307328851055489876255097419754_real64

    real(real64), parameter :: b3 = &
        0.012309078592019946317544564763237909911330686448336_real64

    real(real64), parameter :: b4 = &
       -0.038121613681288650508647613260247372125243616270670_real64

    real(real64), parameter :: b5 = &
        0.072593394748842738674253180742744961827622366521517_real64


contains


    !===========================================================
    ! Z(a,b) = K(a) I(-b) K(-2a) I(b) K(a)
    !
    ! K(x): Kepler drift by x. I(x): interaction kick by x. Both
    ! now operate purely on Jacobi coordinates (qj/pj) -- no
    ! coordinate-frame switch is needed between them (see
    ! jacobi_interaction_kick in interaction.F90).
    !===========================================================

    subroutine corrector_Z(state, a, b)

        type(nbody_state), intent(inout) :: state

        real(real64), intent(in) :: a
        real(real64), intent(in) :: b


        call kepler_step(state, a)
        call jacobi_interaction_kick(state, -b)
        call kepler_step(state, -2.0_real64*a)
        call jacobi_interaction_kick(state, b)
        call kepler_step(state, a)

    end subroutine corrector_Z


    !===========================================================
    ! Forward 11th-order corrector
    !
    ! C_11
    !===========================================================

    subroutine corrector_11(state, h)

        type(nbody_state), intent(inout) :: state

        real(real64), intent(in) :: h


        call corrector_Z(state, -a5*h, -b1*h)
        call corrector_Z(state, -a4*h, -b2*h)
        call corrector_Z(state, -a3*h, -b3*h)
        call corrector_Z(state, -a2*h, -b4*h)
        call corrector_Z(state, -a1*h, -b5*h)

        call corrector_Z(state,  a1*h,  b5*h)
        call corrector_Z(state,  a2*h,  b4*h)
        call corrector_Z(state,  a3*h,  b3*h)
        call corrector_Z(state,  a4*h,  b2*h)
        call corrector_Z(state,  a5*h,  b1*h)

    end subroutine corrector_11


    !===========================================================
    ! Inverse 11th-order corrector
    !
    ! C_11^{-1}
    !
    ! Reverse the order and negate the coefficients.
    !===========================================================

    subroutine inverse_corrector_11(state, h)

        type(nbody_state), intent(inout) :: state

        real(real64), intent(in) :: h


        call corrector_Z(state, -a5*h,  b1*h)
        call corrector_Z(state, -a4*h,  b2*h)
        call corrector_Z(state, -a3*h,  b3*h)
        call corrector_Z(state, -a2*h,  b4*h)
        call corrector_Z(state, -a1*h,  b5*h)

        call corrector_Z(state,  a1*h, -b5*h)
        call corrector_Z(state,  a2*h, -b4*h)
        call corrector_Z(state,  a3*h, -b3*h)
        call corrector_Z(state,  a4*h, -b2*h)
        call corrector_Z(state,  a5*h, -b1*h)

    end subroutine inverse_corrector_11


end module symplectic_corrector
