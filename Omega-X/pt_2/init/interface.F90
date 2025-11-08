module c_interface

  interface
    subroutine pass_string(fstring, length) bind(C)
        use iso_c_binding
        character(kind=c_char), dimension(*) :: fstring
        integer(c_int), value :: length
    end subroutine
  end interface

end module c_interface
