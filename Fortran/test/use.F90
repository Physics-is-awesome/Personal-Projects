module use_test
  implicit none
  real(8) :: a = 8.0, b = 3.14, c
  print*, "Hi"
contains

  subroutine math()
    c = a ** b
  end subroutine math

end module use_test
