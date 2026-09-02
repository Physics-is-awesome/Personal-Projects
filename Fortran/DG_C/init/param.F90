module param
  implicit none
  
  private
  public :: read_params, params_type
  
  type :: params_type
    character(len=256) :: mesh_file
    character(len=256) :: output_dir
    integer :: nelem
    integer :: nvar
    integer :: order
    real(8) :: cfl
    real(8) :: time_final
    real(8) :: dt
  end type params_type
  
contains

  subroutine read_params(param_file, params)
    character(len=*), intent(in) :: param_file
    type(params_type), intent(out) :: params
    integer :: unit, ios
    character(len=512) :: line, key, value
    integer :: eq_pos
    
    ! Initialize defaults
    params%mesh_file = ''
    params%output_dir = './'
    params%nelem = 100
    params%nvar = 5
    params%order = 1
    params%cfl = 0.5d0
    params%time_final = 1.0d0
    params%dt = 0.01d0
    
    ! Open parameter file
    open(newunit=unit, file=trim(param_file), status='old', &
         action='read', iostat=ios)
    
    if (ios /= 0) then
      print *, 'Error: Cannot open parameter file: ', trim(param_file)
      return
    end if
    
    ! Read and parse each line
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      ! Skip comments and empty lines
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#' .or. line(1:1) == '!') cycle
      
      ! Find '=' separator
      eq_pos = index(line, '=')
      if (eq_pos == 0) cycle
      
      ! Extract key and value
      key = adjustl(line(1:eq_pos-1))
      value = adjustl(line(eq_pos+1:))
      
      ! Remove trailing comments
      if (index(value, '#') > 0) then
        value = value(1:index(value, '#')-1)
      end if
      if (index(value, '!') > 0) then
        value = value(1:index(value, '!')-1)
      end if
      value = adjustl(value)
      
      ! Parse parameters
      select case (trim(key))
        case ('mesh_file')
          params%mesh_file = trim(value)
        case ('output_dir')
          params%output_dir = trim(value)
        case ('nelem')
          read(value, *, iostat=ios) params%nelem
        case ('nvar')
          read(value, *, iostat=ios) params%nvar
        case ('order')
          read(value, *, iostat=ios) params%order
        case ('cfl')
          read(value, *, iostat=ios) params%cfl
        case ('time_final')
          read(value, *, iostat=ios) params%time_final
        case ('dt')
          read(value, *, iostat=ios) params%dt
      end select
    end do
    
    close(unit)
    
    print *, 'Parameters read successfully from: ', trim(param_file)
    
  end subroutine read_params

end module param
