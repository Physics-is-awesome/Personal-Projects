module driver_init
  use read_config
  use states
  use calc_vals
  implicit none

  contains
  subroutine init()
  
    call read_file()
    call init_temp()
    call init_momentum()
    call init_entropy()
    call calculate_variables()

  end subroutine init
end module driver_init
