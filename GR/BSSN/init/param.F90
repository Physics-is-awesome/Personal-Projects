module params_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: Nx, Nz, xmax, zmax, dx, dz, dy
  public :: bh_mass, bh_spin, bh_center_x, bh_center_z
  public :: star_center_x, star_center_z
  public :: gauge_eta
  public :: puncture_sign
  public :: slicing_mode
  public :: gamma_driver_mode
  public :: puncture_lapse_mode
  public :: conformal_factor_mode
  public :: cfl, nsteps, out_every, hz_every_par
  public :: ko_eps
  public :: r_excise
  public :: excision_order
  public :: excision_sat
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
  ! Sign of the analytic puncture driver Gamma^a = puncture_sign * 2M x^a / r^3.
  ! UNVALIDATED.  Matching Schwarzschild asymptotically requires an INWARD
  ! Gbar^r ~ -M/(2r^2), but the normalisation and sign are not pinned down
  ! against a solved puncture solution, because that solve is not
  ! implemented.  Only consulted when gamma_driver_mode == 1 (off by default).
  real(dp) :: puncture_sign = -1.0_dp
  ! Bona-Masso slicing  d_perp alpha = -alpha^2 f K,  f = 2/alpha for
  ! 1+log (mode 0) or f = 1/alpha for maximal slicing (mode 1).
  ! mode 2 freezes the Kerr-Schild lapse; pair with gamma_driver_mode=2.
  ! The Kerr-Schild initial data is a static gauge, so evolving it with
  ! maximal slicing produces a much smaller gauge transient than 1+log.
  integer  :: slicing_mode = 0
  ! 0 = legacy: d_t Gbar = RHS_Gam - eta*Gbar   (Gbar is coupled to the
  !     Gtilde equations and is only damped towards ZERO -- no puncture)
  ! 1 = puncture: d_t Gbar = -eta (Gbar - puncture_sign*2M x^a/r^3)
  ! 2 = freeze Kerr-Schild shift and driver (stationary-background control)
  ! 3 = advective Gamma-driver, (d_t-beta.D)beta=3/4 B and
  !     (d_t-beta.D)B=(d_t-beta.D)Lambda-eta B (experimental)
  ! 4 = non-advective Gamma-driver, d_t beta=3/4 B and
  !     d_t B=d_t Lambda-eta B (stationary-shift compatibility candidate)
  integer  :: gamma_driver_mode = 0
  ! Puncture-lapse shaping for the INITIAL data:
  !   0 = off: lapse is whatever Kerr-Schild gives (maximal-slicing like,
  !       alpha ~ 0.707 at the horizon -- does NOT vanish anywhere)
  !   1 = puncture: rescale alpha by a factor vanishing at r_excise so the 1+log
  !       lapse reaches zero on the excision boundary.
  ! This is a diagnostic knob; it changes the solution and therefore injects a
  ! constraint violation.  Validated only by the horizon-drift guard.
  integer  :: puncture_lapse_mode = 0
  ! 0 = phi=(1/12)ln det gamma (det gamma-tilde = 1)
  ! 1 = phi=(1/4)ln(1+2H)    (standard Kerr-Schild, gamma-tilde -> delta)
  integer  :: conformal_factor_mode = 0

  ! --- Time evolution ---
  real(dp) :: cfl = 0.005_dp
  integer  :: nsteps = 60
  integer  :: out_every = 10
  integer  :: hz_every_par = 10
  real(dp) :: ko_eps = 0.2_dp   ! Kreiss-Oliger dissipation strength, typical 0.1-0.5
  real(dp) :: r_excise = 1.0_dp ! keep the excision boundary well inside the Kerr horizon
  logical  :: excision_sat = .false. ! requires a complete BSSN characteristic map
  integer  :: excision_order = 2     ! bounded ghost extrapolation near the inner boundary

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
    real(dp) :: r_horizon

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
    call override_real("GR_PUNCTURE_SIGN", puncture_sign)
    call override_int("GR_GAMMA_DRIVER_MODE", gamma_driver_mode)
    call override_int("GR_SLICING_MODE", slicing_mode)
    call override_int("GR_PUNCTURE_LAPSE_MODE", puncture_lapse_mode)
    call override_int("GR_CONFORMAL_FACTOR_MODE", conformal_factor_mode)
    call override_int("GR_NSTEPS", nsteps)
    call override_int("GR_OUT_EVERY", out_every)
    call override_int("GR_HORIZON_EVERY", hz_every_par)
    call override_real("GR_CFL", cfl)
    call override_real("GR_KO_EPS", ko_eps)
    call override_real("GR_REXCISE", r_excise)
    call override_logical("GR_EXCISION_SAT", excision_sat)
    call override_int("GR_EXCISION_ORDER", excision_order)

    if (bh_mass <= 0.0_dp) error stop 'Parameter error: bh_mass must be positive'
    if (abs(bh_spin) >= bh_mass) error stop 'Parameter error: |bh_spin| must be smaller than bh_mass'
    if (cfl <= 0.0_dp) error stop 'Parameter error: cfl must be positive'
    if (nsteps < 0) error stop 'Parameter error: nsteps must be non-negative'
    if (out_every <= 0) error stop 'Parameter error: out_every must be positive'
    if (hz_every_par < 0) error stop 'Parameter error: hz_every must be non-negative'
    if (r_excise <= 0.0_dp) error stop 'Parameter error: r_excise must be positive'
    if (excision_order < 1 .or. excision_order > 4) then
      error stop 'Parameter error: excision_order must be in [1,4]'
    end if
    r_horizon = bh_mass + sqrt(bh_mass**2 - bh_spin**2)
    if (r_excise >= r_horizon) error stop 'Parameter error: r_excise must lie inside the Kerr outer horizon'

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
        case ('puncture_sign');  read(value, *, iostat=val_ios) puncture_sign
        case ('gamma_driver_mode'); read(value, *, iostat=val_ios) gamma_driver_mode
        case ('slicing_mode'); read(value, *, iostat=val_ios) slicing_mode
        case ('puncture_lapse_mode'); read(value, *, iostat=val_ios) puncture_lapse_mode
        case ('conformal_factor_mode'); read(value, *, iostat=val_ios) conformal_factor_mode
        case ('cfl');            read(value, *, iostat=val_ios) cfl
        case ('nsteps');         read(value, *, iostat=val_ios) nsteps
        case ('out_every');      read(value, *, iostat=val_ios) out_every
        case ('ko_eps');         read(value, *, iostat=val_ios) ko_eps
        case ('r_excise');       read(value, *, iostat=val_ios) r_excise
        case ('excision_order'); read(value, *, iostat=val_ios) excision_order
        case ('excision_sat');    read(value, *, iostat=val_ios) excision_sat
        ! Both spellings accepted: parameter_files/vacuum_kerr.par says
        ! 'hz_every', the compiled-in variable is hz_every_par.
        case ('hz_every');       read(value, *, iostat=val_ios) hz_every_par
        case ('hz_every_par');   read(value, *, iostat=val_ios) hz_every_par
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

  subroutine override_logical(env_name, var)
    character(len=*), intent(in) :: env_name
    logical, intent(inout) :: var
    character(len=64) :: val_str
    integer :: length, ios

    call get_environment_variable(env_name, val_str, length)
    if (length > 0) then
      read(val_str, *, iostat=ios) var
      if (ios /= 0) then
        print *, 'Warning: could not parse ', trim(env_name), ' = ', trim(val_str)
      end if
    end if
  end subroutine override_logical

end module params_mod
