module init_val
  use precision
  use init

  implicit none

contains

  subroutine initial_fundemental_values(state, dt)

    type(nbody_state), intent(out) :: state
    ! put in variables here, don't forget!
    real(real_64), intent(out) :: dt = 0.25
    ! masses
    state%mass(1) = 1.0_real64    ! remember to change values later
    state%mass(2) = 1.0e-3_real64
    state%mass(3) = 1.0e-3_real64

    !==============================
    ! initial positions
    !==============================
    !sun
    state%q(:,1) = [ &
        0.0_real64, &
        0.0_real64, &
        0.0_real64 &
    ]


    ! Planet 1
    state%q(:,2) = [ &
        1.0_real64, &
        0.0_real64, &
        0.0_real64 &
    ]


    ! Planet 2
    state%q(:,3) = [ &
        0.0_real64, &
        2.0_real64, &
        0.0_real64 &
        ]

    !===========================================================
    ! Initial momenta
    !===========================================================

    state%p(:,1) = [ &
        0.0_real64, &
        0.0_real64, &
        0.0_real64 &
    ]


    state%p(:,2) = [ &
        0.0_real64, &
        3.16e-2_real64, &
        0.0_real64 &
    ]


    state%p(:,3) = [ &
        -2.24e-2_real64, &
        0.0_real64, &
        0.0_real64 &
        ]
    ! gravitational constant
    state%G = 1.16e-11
  end subroutine initial_fundemental_values



end module init_val
