program driver
  use driver_init
  use evol
  implicit none
  call init()
  call driver_evolution()
  print*, test
end program driver
