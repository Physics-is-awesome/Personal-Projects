! Geometry-aware SAT building blocks for a level-set excision interface.
!
! The nonlinear characteristic transform is intentionally supplied by the
! caller.  This module only handles the curved normal, local grid scale, and
! SBP-compatible point penalty.
module fo_curved_sat_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: interface_normal_2d
  public :: kerr_schild_excision_normal
  public :: apply_curved_characteristic_sat
  public :: kerr_radius

contains

  subroutine interface_normal_2d(phi_x, phi_z, nx, nz, grad_norm)
    real(dp), intent(in) :: phi_x, phi_z
    real(dp), intent(out) :: nx, nz, grad_norm

    grad_norm = sqrt(phi_x*phi_x + phi_z*phi_z)
    if (.not. (grad_norm > 0.0_dp) .or. grad_norm /= grad_norm) then
      error stop 'interface_normal_2d: degenerate level-set gradient'
    end if
    nx = phi_x/grad_norm
    nz = phi_z/grad_norm
  end subroutine interface_normal_2d

  subroutine kerr_schild_excision_normal(x, z, spin, r_excise, nx, nz, grad_norm)
    real(dp), intent(in) :: x, z, spin, r_excise
    real(dp), intent(out) :: nx, nz, grad_norm
    real(dp) :: h, rp, rm, zp, zm, phi_x, phi_z

    if (r_excise <= 0.0_dp .or. abs(spin) >= 1.0_dp) &
      error stop 'kerr_schild_excision_normal: invalid Kerr parameters'
    h = 1.0e-6_dp*max(1.0_dp, abs(x), abs(z), r_excise)
    rp = kerr_radius(x+h,z,spin)
    rm = kerr_radius(x-h,z,spin)
    zp = kerr_radius(x,z+h,spin)
    zm = kerr_radius(x,z-h,spin)
    phi_x = (rp-rm)/(2.0_dp*h)
    phi_z = (zp-zm)/(2.0_dp*h)
    call interface_normal_2d(phi_x, phi_z, nx, nz, grad_norm)
  end subroutine kerr_schild_excision_normal

  real(dp) function kerr_radius(x, z, spin) result(r)
    real(dp), intent(in) :: x, z, spin
    real(dp) :: rho2, a2, r2
    rho2 = x*x + z*z
    a2 = spin*spin
    r2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)**2 + 4.0_dp*a2*z*z))
    r = sqrt(max(r2,0.0_dp))
  end function kerr_radius

  subroutine apply_curved_characteristic_sat(state, target, speed, tau, dx, dz, nx, nz, rhs)
    real(dp), intent(in) :: state(:), target(:), speed(:), tau, dx, dz, nx, nz
    real(dp), intent(inout) :: rhs(:)
    real(dp) :: normal_norm, h_normal
    integer :: n

    n = size(state)
    if (size(target) /= n .or. size(speed) /= n .or. size(rhs) /= n) &
      error stop 'apply_curved_characteristic_sat: inconsistent mode sizes'
    if (tau < 0.0_dp .or. dx <= 0.0_dp .or. dz <= 0.0_dp) &
      error stop 'apply_curved_characteristic_sat: invalid penalty parameters'
    normal_norm = sqrt(nx*nx + nz*nz)
    if (abs(normal_norm - 1.0_dp) > 1.0e-12_dp) &
      error stop 'apply_curved_characteristic_sat: normal is not unit length'

    ! The inverse normal grid spacing is the metric used by the diagonal
    ! SBP norm for a Cartesian grid cut by a level-set interface.
    h_normal = 1.0_dp/sqrt((nx/dx)**2 + (nz/dz)**2)
    rhs = rhs - tau*min(speed, 0.0_dp)*(state-target)/h_normal
  end subroutine apply_curved_characteristic_sat

end module fo_curved_sat_mod
