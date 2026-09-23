module params_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: Nx, Nz, xmax, zmax, dx, dz, dy
  public :: bh_mass, bh_spin, bh_center_x, bh_center_z
  public :: star_center_x, star_center_z
  public :: gauge_eta
  public :: cfl, nsteps, out_every
  public :: ko_eps
  public :: r_excise
  public :: init_params

  ! --- Grid ---
  integer  :: Nx = 48          ! points in x in [0, xmax], x(1) = 0
  integer  :: Nz = 96          ! points in z in [-zmax, zmax]
  real(dp) :: xmax = 12.0_dp   ! in units of M
  real(dp) :: zmax = 12.0_dp
  real(dp) :: dx, dz
  real(dp) :: dy               ! Cartoon "thin direction" spacing (= dx by default)

  ! --- Black hole (Kerr-Schild) ---
  real(dp) :: bh_mass = 1.0_dp
  real(dp) :: bh_spin = 0.0_dp  ! a, with 0 <= |a| < bh_mass
  real(dp) :: bh_center_x = 0.0_dp, bh_center_z = 0.0_dp
  real(dp) :: star_center_x = 0.0_dp, star_center_z = 0.0_dp

  ! --- Gauge ---
  real(dp) :: gauge_eta = 2.0_dp  ! Gamma-driver damping, ~ 2/M

  ! --- Time evolution ---
  real(dp) :: cfl = 0.25_dp
  integer  :: nsteps = 60
  integer  :: out_every = 10
  real(dp) :: ko_eps = 0.2_dp   ! Kreiss-Oliger dissipation strength, typical 0.1-0.5
  real(dp) :: r_excise = 1.5_dp ! excise inside this radius (horizon is at r=2M); frozen, not evolved

contains

!==============================================================
! Set up all run parameters:
!   1. start from the compiled-in defaults above
!   2. read a .par file, if present, overriding matched keys
!   3. apply any GR_* environment-variable overrides on top
!      (handy for one-off tweaks without editing the .par file)
!   4. derive the grid spacings
!
! par_file defaults to "params.par" if not given.
!==============================================================
  subroutine init_params(par_file)
    character(len=*), intent(in), optional :: par_file

    if (present(par_file)) then
      call read_params(par_file)
    else
      call read_params('params.par')
    end if

    call override_int("GR_NX", Nx)
    call override_int("GR_NZ", Nz)
    call override_real("GR_XMAX", xmax)
    call override_real("GR_ZMAX", zmax)
    call override_real("GR_BH_MASS", bh_mass)
    call override_real("GR_SPIN", bh_spin)
    call override_real("GR_BH_X", bh_center_x)
    call override_real("GR_BH_Z", bh_center_z)
    call override_real("GR_STAR_X", star_center_x)
    call override_real("GR_STAR_Z", star_center_z)
    call override_real("GR_GAUGE_ETA", gauge_eta)
    call override_int("GR_NSTEPS", nsteps)
    call override_int("GR_OUT_EVERY", out_every)
    call override_real("GR_CFL", cfl)
    call override_real("GR_KO_EPS", ko_eps)
    call override_real("GR_REXCISE", r_excise)

    dx = xmax / real(Nx - 1, dp)
    dz = 2.0_dp * zmax / real(Nz - 1, dp)
    dy = dx
  end subroutine init_params

!==============================================================
! Read a "key = value" .par file, one setting per line.
! '#' and '!' start comments (both inline and full-line).
! Blank lines are skipped. Keys are matched case-insensitively.
! A missing file is not an error: the compiled-in defaults
! (or earlier layers) are simply left in place.
!==============================================================
  subroutine read_params(par_file)
    character(len=*), intent(in) :: par_file
    integer :: unit, ios, eq_pos, hash_pos, bang_pos
    character(len=256) :: line
    character(len=64)  :: key
    character(len=192) :: value
    integer :: val_ios

    open(newunit=unit, file=trim(par_file), status='old', &
         action='read', iostat=ios)
    if (ios /= 0) then
      print *, 'Note: parameter file "', trim(par_file), &
               '" not found; using existing defaults.'
      return
    end if

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit

      line = adjustl(line)
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#' .or. line(1:1) == '!') cycle

      ! Strip inline comments
      hash_pos = index(line, '#')
      if (hash_pos > 0) line = line(1:hash_pos-1)
      bang_pos = index(line, '!')
      if (bang_pos > 0) line = line(1:bang_pos-1)

      eq_pos = index(line, '=')
      if (eq_pos == 0) cycle

      key   = adjustl(line(1:eq_pos-1))
      value = adjustl(line(eq_pos+1:))
      call to_lower(key)

      val_ios = 0
      select case (trim(key))
        case ('nx');            read(value, *, iostat=val_ios) Nx
        case ('nz');             read(value, *, iostat=val_ios) Nz
        case ('xmax');           read(value, *, iostat=val_ios) xmax
        case ('zmax');           read(value, *, iostat=val_ios) zmax
        case ('bh_mass');        read(value, *, iostat=val_ios) bh_mass
        case ('bh_spin');        read(value, *, iostat=val_ios) bh_spin
        case ('bh_center_x');    read(value, *, iostat=val_ios) bh_center_x
        case ('bh_center_z');    read(value, *, iostat=val_ios) bh_center_z
        case ('star_center_x');  read(value, *, iostat=val_ios) star_center_x
        case ('star_center_z');  read(value, *, iostat=val_ios) star_center_z
        case ('gauge_eta');      read(value, *, iostat=val_ios) gauge_eta
        case ('cfl');            read(value, *, iostat=val_ios) cfl
        case ('nsteps');         read(value, *, iostat=val_ios) nsteps
        case ('out_every');      read(value, *, iostat=val_ios) out_every
        case ('ko_eps');         read(value, *, iostat=val_ios) ko_eps
        case ('r_excise');       read(value, *, iostat=val_ios) r_excise
        case default
          print *, 'Warning: unknown parameter "', trim(key), &
                   '" in ', trim(par_file), ' (ignored)'
      end select

      if (val_ios /= 0) then
        print *, 'Warning: could not parse "', trim(key), ' = ', &
                 trim(value), '" in ', trim(par_file)
      end if
    end do

    close(unit)
  end subroutine read_params

!==============================================================
! Lowercase a string in place (for case-insensitive key matching)
!==============================================================
  subroutine to_lower(str)
    character(len=*), intent(inout) :: str
    integer :: i, c
    do i = 1, len_trim(str)
      c = iachar(str(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) then
        str(i:i) = achar(c + 32)
      end if
    end do
  end subroutine to_lower

!==============================================================
! Helpers: override a value with an environment variable,
! if that variable is set and the read succeeds.
!==============================================================
  subroutine override_int(env_name, var)
    character(len=*), intent(in) :: env_name
    integer, intent(inout) :: var
    character(len=64) :: val_str
    integer :: length, ios

    call get_environment_variable(env_name, val_str, length)
    if (length > 0) then
      read(val_str, *, iostat=ios) var
      if (ios /= 0) then
        print *, 'Warning: could not parse ', trim(env_name), ' = ', trim(val_str)
      end if
    end if
  end subroutine override_int

  subroutine override_real(env_name, var)
    character(len=*), intent(in) :: env_name
    real(dp), intent(inout) :: var
    character(len=64) :: val_str
    integer :: length, ios

    call get_environment_variable(env_name, val_str, length)
    if (length > 0) then
      read(val_str, *, iostat=ios) var
      if (ios /= 0) then
        print *, 'Warning: could not parse ', trim(env_name), ' = ', trim(val_str)
      end if
    end if
  end subroutine override_real

end module params_mod
