module driver_init
  use read_config
  use states
  use calc_vals
  use types
  implicit none

  contains
  subroutine init()
    type(Omega-X), intent(inout) :: Omega
    call read_file(Omega)
    call init_state(Omega)
    call init_momentum(Omega)
    call init_entropy(Omega)
    call init_mass(Omega)
    call calculate_variables(Omega)

  end subroutine init
end module driver_init
