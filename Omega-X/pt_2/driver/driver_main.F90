program driver
  use driver_init
  use driver_evol
  implicit none
  call init()
  call driver_evolution()
end program driver
