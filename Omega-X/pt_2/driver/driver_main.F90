program driver
  use driver_init
  use evol
  use types
  implicit none

  type(Omega-X) :: Omega
  ! init driver
  call init(Omega)
  
  ! evolution driver
  call driver_evolution(Omega)

end program driver
