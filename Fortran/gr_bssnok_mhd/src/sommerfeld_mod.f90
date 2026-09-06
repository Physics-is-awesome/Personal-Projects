module sommerfeld_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz
  use grid_mod, only: xg, zg
  use vars_mod
  implicit none
  private
  public :: add_sommerfeld_bc

  real(dp), parameter :: char_speed = 1.0_dp

contains

  ! Asymptotic (Minkowski/flat, alpha=1, beta=0) value for each field.
  function asymptotic_value(ivar) result(f0)
    integer, intent(in) :: ivar
    real(dp) :: f0
    f0 = 0.0_dp
    if (ivar == IGXX .or. ivar == IGYY .or. ivar == IGZZ) f0 = 1.0_dp
    if (ivar == IALPHA) f0 = 1.0_dp
  end function asymptotic_value

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
    real(dp) :: r, f0, dfdr, f1, f2, f3

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
        rhs(i,k,ivar) = -char_speed*dfdr - (f1 - f0)/r
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
        rhs(i,k,ivar) = -char_speed*dfdr - (f1 - f0)/r
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
        rhs(i,k,ivar) = -char_speed*dfdr - (f1 - f0)/r
      end do
    end do

  end subroutine add_sommerfeld_bc

end module sommerfeld_mod
