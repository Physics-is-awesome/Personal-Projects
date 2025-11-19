program test
  use use_test
  implicit none

  call math2()
  print*, d
contains

  subroutine math2()
    real(8) :: d
    call math()
    d = c + 2 + a
  end subroutine math2
end program test
