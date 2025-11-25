program driver
  use driver_init
  use evol
  use declare_2
  implicit none
  call init()
  call driver_evolution()
  print*, F_m_h(2)
end program driver
