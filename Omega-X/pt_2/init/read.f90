module read_config
  implicit none

contains
  ! -------------------------------------------------------------------------------------------------
  ! Read dimensions, sizes, dynamics, etc
  ! -------------------------------------------------------------------------------------------------
  subroutine read_file()

    integer, parameter :: max_entries = 100
    character(len=32) :: keys(max_entries)
    character(len=128) :: values(max_entries)
    integer :: count, ios, nx, ny, nz, dim
    logical :: mass, entropy, momentum
    character(len=256) :: line, key, eqsign, value

    count = 0
    open(unit=10, file="../config/config.cfg", status="old", action="read")

    do
      read(10,'(A)',iostat=ios) line
      if (ios /= 0) exit
      if (line(1:1) == "#" .or. trim(line) == "") cycle

      read(line,*) key, eqsign, value
      count = count + 1
      keys(count)   = trim(key)
      values(count) = trim(value)
    end do
    close(10)
:
    ! getting dimensions
    dim = get_int("dim", keys, values, count)
    nx = get_int("nx", keys, values, count)
    if (dim >= 2) then
      ny = get_int("ny", keys, values, count)
    end if
    if (dim >= 3) then
      nz = get_int("nz", keys, values, count)
    end if
    
    ! dynamics allowed
    mass = get_bol("mass", keys, values, count)
    momentum = get_bol("momentum", keys, values, count)
    entropy = get_bol("entropy", keys, values, count)
    
    ! temp
    temp_dis = get_string("temp_dist", keys, values, count)
    temp_mean = get_real("temp_mean", keys, values, count)
    temp_var = get_real("temp_var", keys, values, count)

    ! mass
    mass_dis = get_string("mass_dist", keys, values, count)
    mass_mean = get_real("mass_mean", keys, values, count)
    mass_var = get_real("mass_var", keys, values, count)

    ! momentum
    momentum_dis = get_string("momentum_dist", keys, values, count)
    momentum_mean = get_real("momentum_mean", keys, values, count)
    momentum_var = get_real("moemntum_var", keys, values, count)

    ! entropy
    entropy_dis = get_string("entropy_dist", keys, values, count)
    entropy_mean = get_real("entropy_mean", keys, values, count)
    entropy_var = get_real("entropy_var", keys, values, count)
    
  end subroutine read_file
  ! getting integers
  function get_int(search_key, keys, values, n) result(val)
    character(len=*), intent(in) :: search_key
    character(len=*), intent(in) :: keys(:), values(:)
    integer, intent(in) :: n
    integer :: val, i
    val = -1
    do i = 1, n
       if (trim(keys(i)) == trim(search_key)) then
          read(values(i),*) val
          return
       end if
    end do
  end function get_int
  ! Get bolean 
  function get_bol(search_key, keys, values, n) result(bol)
    character(len=*), intent(in) :: search_key
    character(len=*), intent(in) :: keys(:), values(:)
    integer, intent(in) :: n
    integer :: i
    logical :: bol
    bol = .false.
    do i = 1, n
       if (trim(keys(i)) == trim(search_key)) then
          read(values(i),*) bol
          return
       end if
    end do
  end function get_bol
  ! Get real
  function get_real(search_key, keys, values, n) result(reals)
    character(len=*), intent(in) :: search_key
    character(len=*), intent(in) :: keys(:), values(:)
    integer, intent(in) :: n
    integer :: i
    real(8) :: reals
    reals = 0.0
    do i = 1, n
       if (trim(keys(i)) == trim(search_key)) then
          read(values(i),*) reals
          return
       end if
    end do
  end function get_real
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
