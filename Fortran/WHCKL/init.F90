module init

  use precision

  implicit none

  private

  public :: initialize
  public :: destroy

  type :: nbody_state
    ! number of particles
    integer :: n = 3 ! change if needed

    ! position
    ! q(dim, particle number)
    !
    real(real64), allocatable :: q(:,:)

    !momenta
    real(real64), allocatable :: p(:,:)

    !Mass
    real(real64), allocatable :: mass(:)

    !Gravitational constant
    real(real64) :: G

    ! Jacobi canonical variables
    real(real64), allocatable :: qj(:,:)
    real(real64), allocatable :: pj(:,:)

    ! interactive acceleration
    real(real64), allocatable :: a_int(:,:)

    ! Cumulative masses
    real(real64), allocatable :: M(:)

    ! Jacobi reduced masses
    real(real64), allocatable :: mu(:)
  end type nbody_state

  public ::nbody_state
contains

  subroutine initialize(state, n)

        type(nbody_state), intent(out) :: state

        integer, intent(in) :: n




        state%n = n


        allocate(state%q(3,n))
        allocate(state%p(3,n))
        allocate(state%mass(n))
        allocate(state%qj(3,n))
        allocate(state%pj(3,n))
        allocate(state%a_int(3,n))
        allocate(state%M(n))
        allocate(state%mu(n-1))

        state%q = 0.0_real64
        state%p = 0.0_real64
        state%mass = 1.0_real64
        state%G = 1.0_real64
        state%qj = 0.0_real64
        state%pj = 0.0_real64
        state%mu = 0.0_real64
        state%M = 0.0_real64
        state%a_int = 0.0_real64
      end subroutine initialize

      subroutine destroy(state)

        type(nbody_state), intent(inout) :: state

        if (allocated(state%q)) then
            deallocate(state%q)
        end if

        if (allocated(state%p)) then
            deallocate(state%p)
        end if

        if (allocated(state%mass)) then
            deallocate(state%mass)
        end if

    end subroutine destroy
end module init
