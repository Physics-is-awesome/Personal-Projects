module sommerfeld_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz
  use grid_mod, only: xg, zg
  use vars_mod
  implicit none
  private
  public :: add_sommerfeld_bc

  real(dp), parameter :: char_speed = 1.0_dp

  ! DIAGNOSTIC: per-variable asymptotic falloff.  The legacy condition used
  ! - (f - f0)/r for EVERY field, i.e. it assumed u - f0 ~ 1/r for all 24
  ! variables.  For exact Kerr-Schild data the true powers are far steeper:
  !     gtilde -> 1                            (no 1/r term at all)
  !     phi, alpha-1, beta          ~ 1/r^3
  !     K, Atilde, Gammatilde, B     ~ 1/r^4
  ! Setting GR_SOMMERFELD_DECAY=1 switches the condition to the per-field
  ! power n, i.e.  - n (f - f0)/r.  Default 0 preserves the legacy behaviour.
  logical, save :: have_init = .false.
  logical, save :: use_decay = .false.

contains

  subroutine sommerfeld_read_env()
    character(len=64) :: buf
    integer :: ios, iv
    if (have_init) return
    have_init = .true.
    call get_environment_variable("GR_SOMMERFELD_DECAY", buf, status=ios)
    if (ios == 0) then
       read(buf,*) iv
       use_decay = (iv /= 0)
    end if
  end subroutine sommerfeld_read_env

  ! Asymptotic (Minkowski/flat, alpha=1, beta=0) value for each field.
  function asymptotic_value(ivar) result(f0)
    integer, intent(in) :: ivar
    real(dp) :: f0
    f0 = 0.0_dp
    if (ivar == IGXX .or. ivar == IGYY .or. ivar == IGZZ) f0 = 1.0_dp
    if (ivar == IALPHA) f0 = 1.0_dp
  end function asymptotic_value

  ! Asymptotic falloff power n such that (f - f0) ~ r^(-n), for exact
  ! Kerr-Schild data.  See the module header for the derivation.
  function asymptotic_power(ivar) result(n)
    integer, intent(in) :: ivar
    real(dp) :: n
    n = 3.0_dp
    select case (ivar)
    case (IGXX, IGYY, IGZZ)                 ! -> 1, no 1/r term
       n = 0.0_dp
    case (IGXY, IGXZ, IGYZ)                 ! -> 0 like 1/r^3
       n = 3.0_dp
    case (IK)
       n = 4.0_dp
    case (IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ)
       n = 4.0_dp
    case (IGTX, IGTY, IGTZ)
       n = 4.0_dp
    case (IPHI, IALPHA)
       n = 3.0_dp
    case default                            ! beta, B
       n = 3.0_dp
    end select
  end function asymptotic_power

  ! Fills the RHS at the outer boundary rows/columns (i=Nx; k=1; k=Nz) with
  ! a simple outgoing-wave (Sommerfeld) condition:
  !   d_t f = -c (d_r f) - (f - f0)/r
  ! approximating the outward radial derivative by a one-sided difference
  ! along whichever grid direction is normal to that particular boundary
  ! (z at the z-edges, x at the x-edge). This replaces the previous
  ! zeroth-order copy BC, which was likely feeding boundary noise back into
  ! the interior through the Gamma-driver shift's second-derivative terms.
  subroutine add_sommerfeld_bc(u, rhs)
    real(dp), intent(in)    :: u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    integer :: i, k, ivar
    real(dp) :: r, f0, dfdr, f1, f2, f3, pw

    call sommerfeld_read_env()

    ! --- z = -zmax boundary (k=1): outward direction is -z ---
    k = 1
    do i = 1, Nx
      r = sqrt(xg(i)**2 + zg(k)**2)
      if (r < 1.0e-6_dp) r = 1.0e-6_dp
      do ivar = 1, NVARS
        f0 = asymptotic_value(ivar)
        f1 = u(i,1,ivar); f2 = u(i,2,ivar); f3 = u(i,3,ivar)
        ! d_z f (one-sided forward, 2nd order), then d_r f = -d_z f here
        dfdr = -( -3.0_dp*f1 + 4.0_dp*f2 - f3 ) / (2.0_dp*dz)
        pw = merge(asymptotic_power(ivar), 1.0_dp, use_decay)
        rhs(i,k,ivar) = -char_speed*dfdr - pw*(f1 - f0)/r
      end do
    end do

    ! --- z = +zmax boundary (k=Nz): outward direction is +z ---
    k = Nz
    do i = 1, Nx
      r = sqrt(xg(i)**2 + zg(k)**2)
      if (r < 1.0e-6_dp) r = 1.0e-6_dp
      do ivar = 1, NVARS
        f0 = asymptotic_value(ivar)
        f1 = u(i,Nz,ivar); f2 = u(i,Nz-1,ivar); f3 = u(i,Nz-2,ivar)
        dfdr = ( 3.0_dp*f1 - 4.0_dp*f2 + f3 ) / (2.0_dp*dz)
        pw = merge(asymptotic_power(ivar), 1.0_dp, use_decay)
        rhs(i,k,ivar) = -char_speed*dfdr - pw*(f1 - f0)/r
      end do
    end do

    ! --- x = xmax boundary (i=Nx): outward direction is +x ---
    i = Nx
    do k = 2, Nz - 1   ! corners (k=1,Nz) already set above; leave as-is
      r = sqrt(xg(i)**2 + zg(k)**2)
      if (r < 1.0e-6_dp) r = 1.0e-6_dp
      do ivar = 1, NVARS
        f0 = asymptotic_value(ivar)
        f1 = u(Nx,k,ivar); f2 = u(Nx-1,k,ivar); f3 = u(Nx-2,k,ivar)
        dfdr = ( 3.0_dp*f1 - 4.0_dp*f2 + f3 ) / (2.0_dp*dx)
        pw = merge(asymptotic_power(ivar), 1.0_dp, use_decay)
        rhs(i,k,ivar) = -char_speed*dfdr - pw*(f1 - f0)/r
      end do
    end do

  end subroutine add_sommerfeld_bc

end module sommerfeld_mod
