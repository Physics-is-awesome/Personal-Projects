module dissipation_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, ko_eps, r_excise
  use grid_mod, only: xg, zg
  use vars_mod, only: NVARS
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: add_ko_dissipation

contains

  ! Adds standard Kreiss-Oliger dissipation to rhs, for every field, summed
  ! over the three spatial directions:
  !
  !   (d_t u)_diss = -eps/(16 h) * ( u(-2) - 4 u(-1) + 6 u(0) - 4 u(+1) + u(+2) )
  !
  ! applied in x, y (via Cartoon ghosts), and z. Only applied on the subset
  ! of the interior grid where the 2-point-wide stencil stays in bounds in x
  ! and z (i in [1, Nx-2], k in [3, Nz-2]); points closer to the outer
  ! boundary than that are left undissipated, which is standard practice
  ! (the boundary itself is handled separately by boundary_mod).
  subroutine add_ko_dissipation(u, rhs)
    real(dp), intent(in)    :: u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    integer :: i, k
    real(dp) :: c0(NVARS)
    real(dp) :: xm1(NVARS), xp1(NVARS), xm2(NVARS), xp2(NVARS)
    real(dp) :: ym1(NVARS), yp1(NVARS), ym2(NVARS), yp2(NVARS)
    real(dp) :: zm1(NVARS), zp1(NVARS), zm2(NVARS), zp2(NVARS)
    real(dp) :: diss(NVARS)

    do k = 3, Nz - 2
      do i = 1, Nx - 2
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise) cycle  ! frozen inside excision radius
        c0  = u(i,k,:)

        xm1 = get_neighbor(u, i, k, -1, 0, 0.0_dp)
        xp1 = get_neighbor(u, i, k,  1, 0, 0.0_dp)
        xm2 = get_neighbor(u, i, k, -2, 0, 0.0_dp)
        xp2 = get_neighbor(u, i, k,  2, 0, 0.0_dp)

        ym1 = get_neighbor(u, i, k, 0, 0, -dy)
        yp1 = get_neighbor(u, i, k, 0, 0,  dy)
        ym2 = get_neighbor(u, i, k, 0, 0, -2.0_dp*dy)
        yp2 = get_neighbor(u, i, k, 0, 0,  2.0_dp*dy)

        zm1 = get_neighbor(u, i, k, 0, -1, 0.0_dp)
        zp1 = get_neighbor(u, i, k, 0,  1, 0.0_dp)
        zm2 = get_neighbor(u, i, k, 0, -2, 0.0_dp)
        zp2 = get_neighbor(u, i, k, 0,  2, 0.0_dp)

        diss = -ko_eps/(16.0_dp*dx) * (xm2 - 4.0_dp*xm1 + 6.0_dp*c0 - 4.0_dp*xp1 + xp2) &
               -ko_eps/(16.0_dp*dy) * (ym2 - 4.0_dp*ym1 + 6.0_dp*c0 - 4.0_dp*yp1 + yp2) &
               -ko_eps/(16.0_dp*dz) * (zm2 - 4.0_dp*zm1 + 6.0_dp*c0 - 4.0_dp*zp1 + zp2)

        rhs(i,k,:) = rhs(i,k,:) + diss
      end do
    end do
  end subroutine add_ko_dissipation

end module dissipation_mod
