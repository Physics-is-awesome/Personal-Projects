module init
  implicit none



  ! -------------------------------------------------------------------------------------------------
  ! Read dimensions, form mesh, and ...
  !
  subroutine read_config
    open(10, file='config.cfg')

    read(10, *) 
