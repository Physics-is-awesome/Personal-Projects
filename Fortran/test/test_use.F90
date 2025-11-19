program test
  use use_test
  implicit none
  call math()
  call math2()
contains

  subroutine math2()
    real(8) :: d
    d = c+2+a
  end subroutine math2
end program test
