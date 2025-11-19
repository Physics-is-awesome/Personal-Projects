program test
  use use_test
  implicit none

  call math2()
contains

  subroutine math2()
    call math
    real(8) :: d
    d = c+2+a
  end subroutine math2
end program test
