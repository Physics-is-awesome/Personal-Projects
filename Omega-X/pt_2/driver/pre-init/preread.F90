module read_config
  use iso_c_binding
  use c_interface
  implicit none

  character(len=20), target :: fstring
contains
  ! -------------------------------------------------------------------------------------------------
  ! Read dimensions, sizes, dynamics, etc
  ! -------------------------------------------------------------------------------------------------
  subroutine read_file()

    integer, parameter :: max_entries = 100
    character(len=32) :: keys(max_entries)
    character(len=128) :: values(max_entries)

    character(len=256) :: line, key, eqsign, value

    count = 0
    open(unit=10, file="config/config.cfg", status="old", action="read")

    do
      !read(10,'(A)',iostat=ios) line
      !if (ios /= 0) exit
      !if (line(1:1) == "#" .or. trim(line) == "") cycle

      !read(line,*) key, eqsign, value
      !count = count + 1
      !keys(count)   = trim(key)
      !values(count) = trim(value)
      read(10,'(A)',iostat=ios) line
      if (ios < 0) exit        ! end of file
      if (ios > 0) then
        print *, "Read error"
        exit
      end if

      if (line(1:1) == "#" .or. trim(line) == "") cycle
      pos = index(line,"=")
      if (pos > 0) then
       key   = adjustl(trim(line(:pos-1)))
       value = adjustl(trim(line(pos+1:)))
       count = count + 1
       keys(count)   = key
       values(count) = value
end if
      
    end do
    close(10)

   
    ! test
    fstring = get_string("string", keys, values, count)
    call c_function(fstring, len(fstring))

    
  end subroutine read_file

  ! get string
  function get_string(search_key, keys, values, n) result(string)
    character(len=*), intent(in) :: search_key
    character(len=*), intent(in) :: keys(:), values(:)
    integer, intent(in) :: n
    integer :: i
    character(len=20) :: string
    string = ""
    do i = 1, n
       if (trim(keys(i)) == trim(search_key)) then
          read(values(i),*) string
          return
       end if
    end do
  end function get_string


end module read_config
