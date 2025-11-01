program init
  implicit none



  ! -------------------------------------------------------------------------------------------------
  ! Read dimensions, form mesh, and ...
  !
  subroutine read_config
    implicit none
    integer, parameter :: max_entries = 100
    character(len=32) :: keys(max_entries)
    character(len=128) :: values(max_entries)
    integer :: count, ios
    character(len=256) :: line, key, value

    count = 0
    open(unit=10, file="input.cfg", status="old", action="read")

    do
      read(10,'(A)',iostat=ios) line
      if (ios /= 0) exit
      if (line(1:1) == "#" .or. trim(line) == "") cycle

      read(line,*) key, value
      count = count + 1
      keys(count)   = trim(key)
      values(count) = trim(value)
    end do
    close(10)

    ! Now you can look up any key you want:
    print *, "ny =", get_int("ny", keys, values, count)
    print *, "nx =", get_int("nx", keys, values, count)
  end subroutine read_config
