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

  subroutine init_params()
    call override_int("GR_NX", Nx)
    call override_int("GR_NZ", Nz)
    call override_real("GR_XMAX", xmax)
    call override_real("GR_ZMAX", zmax)
    call override_real("GR_SPIN", bh_spin)
    call override_real("GR_BH_X", bh_center_x)
    call override_real("GR_BH_Z", bh_center_z)
    call override_real("GR_STAR_X", star_center_x)
    call override_real("GR_STAR_Z", star_center_z)
    call override_int("GR_NSTEPS", nsteps)
    call override_int("GR_OUT_EVERY", out_every)
    call override_real("GR_CFL", cfl)
    call override_real("GR_REXCISE", r_excise)

    dx = xmax / real(Nx - 1, dp)
    dz = 2.0_dp * zmax / real(Nz - 1, dp)
    dy = dx
  end subroutine init_params

  subroutine override_int(name, val)
    character(len=*), intent(in) :: name
    integer, intent(inout) :: val
    character(len=64) :: buf
    integer :: stat
    call get_environment_variable(name, buf, status=stat)
    if (stat == 0) read(buf,*) val
  end subroutine override_int

  subroutine override_real(name, val)
    character(len=*), intent(in) :: name
    real(dp), intent(inout) :: val
    character(len=64) :: buf
    integer :: stat
    call get_environment_variable(name, buf, status=stat)
    if (stat == 0) read(buf,*) val
  end subroutine override_real

end module params_mod
