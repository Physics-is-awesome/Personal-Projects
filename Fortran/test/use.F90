module use_test
  implicit none

contains

  subroutine math()
    real(8) :: a = 8.0, b = 3.14, c

    c = a ** b
  end subroutine math

end module use_test
