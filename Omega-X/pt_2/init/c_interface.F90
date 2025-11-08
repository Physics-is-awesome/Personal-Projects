module c_interface
  use iso_c_binding

  interface
    subroutine call_cpp(str) bind(C, name="c_function")
      import :: C_CHAR
      character(kind=C_CHAR), dimension(*), intent(in) :: str
    end subroutine call_cpp
  end interface

end module c_interface
