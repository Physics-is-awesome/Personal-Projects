program driver_init
  use read_config
  use states
  implicit none

  contains
  subroutine init()
    call read_file()
    call init_temp()
  end subroutine init
end program driver_init
