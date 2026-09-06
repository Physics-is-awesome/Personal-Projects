module initial_data_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, bh_mass, bh_spin
  use grid_mod, only: xg, zg
  use vars_mod
  use geometry_mod, only: sym3_inverse
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: set_kerr_schild_id, compute_conformal_connections
  public :: kerr_schild_point, kerr_schild_adm

contains

  ! Cartesian Kerr-Schild ADM quantities at an arbitrary point (x,y,z).
  ! Spin along z. See docs/derivation.md section 5.
  subroutine kerr_schild_point(x, y, z, gij, alpha, beta_lower)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: gij(3,3), alpha, beta_lower(3)
    real(dp) :: M, a2, rho2, r2, r, denom, H
    real(dp) :: l(3)
    real(dp), parameter :: floor_ = 1.0e-8_dp

    M = bh_mass
    a2 = bh_spin*bh_spin
    rho2 = x*x + y*y + z*z
    r2 = 0.5_dp*( (rho2 - a2) + sqrt((rho2 - a2)**2 + 4.0_dp*a2*z*z) )
    if (r2 < floor_) r2 = floor_
    r = sqrt(r2)

    denom = r2*r2 + a2*z*z
    if (denom < floor_) denom = floor_
    H = M*r2*r / denom

    l(1) = (r*x + bh_spin*y) / (r2 + a2)
    l(2) = (r*y - bh_spin*x) / (r2 + a2)
    l(3) = z / r

    gij = 0.0_dp
    gij(1,1) = 1.0_dp; gij(2,2) = 1.0_dp; gij(3,3) = 1.0_dp
    gij = gij + 2.0_dp*H*outer(l, l)

    alpha = 1.0_dp / sqrt(1.0_dp + 2.0_dp*H)
    beta_lower = 2.0_dp*H*l
  end subroutine kerr_schild_point

  function outer(u, v) result(M)
    real(dp), intent(in) :: u(3), v(3)
    real(dp) :: M(3,3)
    integer :: p, q
    do p = 1, 3
      do q = 1, 3
        M(p,q) = u(p)*v(q)
      end do
    end do
  end function outer

  ! Full ADM data at (x,y,z), including K_ij obtained from the stationary
  ! relation K_ij = (D_i beta_j + D_j beta_i) / (2 alpha), with the
  ! covariant derivative built from Christoffels finite-differenced directly
  ! from the closed-form metric (independent evaluations at x +/- h, etc. --
  ! no Cartoon machinery needed here since we have an exact formula).
  subroutine kerr_schild_adm(x, y, z, gij, gijinv, alpha, beta_upper, Kij)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: gij(3,3), gijinv(3,3), alpha, beta_upper(3), Kij(3,3)
    real(dp) :: g0(3,3), bl0(3), a0
    real(dp) :: gxp(3,3), gxm(3,3), gyp(3,3), gym(3,3), gzp(3,3), gzm(3,3)
    real(dp) :: blxp(3), blxm(3), blyp(3), blym(3), blzp(3), blzm(3)
    real(dp) :: dum_a
    real(dp) :: dg(3,3,3)      ! dg(m,a,b) = d_m g_ab
    real(dp) :: dbl(3,3)       ! dbl(m,a)  = d_m beta_lower_a
    real(dp) :: Chr_low(3,3,3), Gam(3,3,3)
    real(dp) :: Dbeta(3,3), det
    real(dp), parameter :: h = 1.0e-4_dp
    integer :: p, q, kk

    call kerr_schild_point(x, y, z, g0, a0, bl0)
    call kerr_schild_point(x+h, y, z, gxp, dum_a, blxp)
    call kerr_schild_point(x-h, y, z, gxm, dum_a, blxm)
    call kerr_schild_point(x, y+h, z, gyp, dum_a, blyp)
    call kerr_schild_point(x, y-h, z, gym, dum_a, blym)
    call kerr_schild_point(x, y, z+h, gzp, dum_a, blzp)
    call kerr_schild_point(x, y, z-h, gzm, dum_a, blzm)

    gij = g0; alpha = a0
    call sym3_inverse(gij, gijinv, det)

    dg(1,:,:) = (gxp - gxm) / (2.0_dp*h)
    dg(2,:,:) = (gyp - gym) / (2.0_dp*h)
    dg(3,:,:) = (gzp - gzm) / (2.0_dp*h)

    dbl(1,:) = (blxp - blxm) / (2.0_dp*h)
    dbl(2,:) = (blyp - blym) / (2.0_dp*h)
    dbl(3,:) = (blzp - blzm) / (2.0_dp*h)

    do kk = 1, 3
      do p = 1, 3
        do q = 1, 3
          Chr_low(kk,p,q) = 0.5_dp*(dg(p,q,kk) + dg(q,p,kk) - dg(kk,p,q))
        end do
      end do
    end do
    do kk = 1, 3
      do p = 1, 3
        do q = 1, 3
          Gam(kk,p,q) = sum(gijinv(kk,:) * Chr_low(:,p,q))
        end do
      end do
    end do

    do p = 1, 3
      do q = 1, 3
        Dbeta(p,q) = dbl(p,q) - sum(Gam(:,p,q) * bl0(:))
      end do
    end do

    Kij = (Dbeta + transpose(Dbeta)) / (2.0_dp*alpha)
    beta_upper = matmul(gijinv, bl0)
  end subroutine kerr_schild_adm

  ! Fills the whole master half-plane (x>=0, y=0, all z) with BSSNOK
  ! conformally-decomposed Kerr-Schild data. Gamma-tilde^i is left at zero
  ! here and filled in by compute_conformal_connections below, once the
  ! conformal metric is on the grid.
  subroutine set_kerr_schild_id(u)
    real(dp), intent(inout) :: u(:,:,:)
    integer :: i, k
    real(dp) :: gij(3,3), gijinv(3,3), alpha, beta_upper(3), Kij(3,3)
    real(dp) :: phi, Ktrace
    real(dp) :: gtij(3,3), Aij(3,3)

    do k = 1, Nz
      do i = 1, Nx
        call kerr_schild_adm(xg(i), 0.0_dp, zg(k), gij, gijinv, alpha, beta_upper, Kij)

        phi = log(gij(1,1)*(gij(2,2)*gij(3,3)-gij(2,3)*gij(2,3)) &
                  - gij(1,2)*(gij(1,2)*gij(3,3)-gij(2,3)*gij(1,3)) &
                  + gij(1,3)*(gij(1,2)*gij(2,3)-gij(2,2)*gij(1,3))) / 12.0_dp

        gtij = exp(-4.0_dp*phi) * gij
        Ktrace = sum(gijinv * Kij)
        Aij = exp(-4.0_dp*phi) * (Kij - (gij*Ktrace)/3.0_dp)

        u(i,k,IPHI) = phi
        u(i,k,IGXX) = gtij(1,1); u(i,k,IGXY) = gtij(1,2); u(i,k,IGXZ) = gtij(1,3)
        u(i,k,IGYY) = gtij(2,2); u(i,k,IGYZ) = gtij(2,3); u(i,k,IGZZ) = gtij(3,3)
        u(i,k,IK)   = Ktrace
        u(i,k,IAXX) = Aij(1,1); u(i,k,IAXY) = Aij(1,2); u(i,k,IAXZ) = Aij(1,3)
        u(i,k,IAYY) = Aij(2,2); u(i,k,IAYZ) = Aij(2,3); u(i,k,IAZZ) = Aij(3,3)
        u(i,k,IALPHA) = alpha
        u(i,k,IBETAX) = beta_upper(1); u(i,k,IBETAY) = beta_upper(2); u(i,k,IBETAZ) = beta_upper(3)
        u(i,k,IGTX) = 0.0_dp; u(i,k,IGTY) = 0.0_dp; u(i,k,IGTZ) = 0.0_dp
        u(i,k,IBX) = 0.0_dp;  u(i,k,IBY) = 0.0_dp;  u(i,k,IBZ) = 0.0_dp
      end do
    end do
  end subroutine set_kerr_schild_id

  ! Gamma-tilde^i = -d_j gamma-tilde^{ij}, computed by finite-differencing
  ! the conformal metric inverse now that it is stored on the grid.
  subroutine compute_conformal_connections(u)
    real(dp), intent(inout) :: u(:,:,:)
    integer :: i, k
    real(dp) :: gxp(3,3), gxm(3,3), gyp(3,3), gym(3,3), gzp(3,3), gzm(3,3)
    real(dp) :: ginvxp(3,3), ginvxm(3,3), ginvyp(3,3), ginvym(3,3), ginvzp(3,3), ginvzm(3,3)
    real(dp) :: dginv(3,3,3), Gt(3)
    real(dp) :: det
    real(dp) :: vp(NVARS), vm(NVARS)

    do k = 2, Nz - 1
      do i = 1, Nx - 1
        vp = get_neighbor(u, i, k,  1, 0, 0.0_dp); gxp = pack_g(vp)
        vm = get_neighbor(u, i, k, -1, 0, 0.0_dp); gxm = pack_g(vm)
        vp = get_neighbor(u, i, k,  0, 1, 0.0_dp); gzp = pack_g(vp)
        vm = get_neighbor(u, i, k,  0,-1, 0.0_dp); gzm = pack_g(vm)
        vp = get_neighbor(u, i, k,  0, 0, dy);     gyp = pack_g(vp)
        vm = get_neighbor(u, i, k,  0, 0,-dy);     gym = pack_g(vm)

        call sym3_inverse(gxp, ginvxp, det); call sym3_inverse(gxm, ginvxm, det)
        call sym3_inverse(gyp, ginvyp, det); call sym3_inverse(gym, ginvym, det)
        call sym3_inverse(gzp, ginvzp, det); call sym3_inverse(gzm, ginvzm, det)

        dginv(1,:,:) = (ginvxp - ginvxm) / (2.0_dp*dx)
        dginv(2,:,:) = (ginvyp - ginvym) / (2.0_dp*dy)
        dginv(3,:,:) = (ginvzp - ginvzm) / (2.0_dp*dz)

        Gt(1) = -(dginv(1,1,1) + dginv(2,1,2) + dginv(3,1,3))
        Gt(2) = -(dginv(1,2,1) + dginv(2,2,2) + dginv(3,2,3))
        Gt(3) = -(dginv(1,3,1) + dginv(2,3,2) + dginv(3,3,3))

        u(i,k,IGTX) = Gt(1); u(i,k,IGTY) = Gt(2); u(i,k,IGTZ) = Gt(3)
      end do
    end do
  end subroutine compute_conformal_connections

  function pack_g(arr) result(M)
    real(dp), intent(in) :: arr(:)
    real(dp) :: M(3,3)
    M(1,1) = arr(IGXX); M(1,2) = arr(IGXY); M(1,3) = arr(IGXZ)
    M(2,1) = arr(IGXY); M(2,2) = arr(IGYY); M(2,3) = arr(IGYZ)
    M(3,1) = arr(IGXZ); M(3,2) = arr(IGYZ); M(3,3) = arr(IGZZ)
  end function pack_g

end module initial_data_mod
