module driver_init
  use read_config
  use states
  implicit none

  contains
  subroutine init()
    calculate_variables()
    call read_file()
    call init_temp()
    call init_momentum()
    call init_entropy()
  end subroutine init
end module driver_init
