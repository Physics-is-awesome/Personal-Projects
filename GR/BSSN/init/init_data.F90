module initial_data_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, bh_mass, bh_spin, &
                         bh_center_x, bh_center_z, r_excise, &
                         gamma_driver_mode, puncture_lapse_mode, conformal_factor_mode
  use grid_mod, only: xg, zg
  use vars_mod
  use geometry_mod, only: sym3_inverse
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: set_kerr_schild_id, compute_conformal_connections
  public :: kerr_schild_point, kerr_schild_adm

contains
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
  ! turn metrics into BSSN variables
  subroutine set_kerr_schild_id(u)
    real(dp), intent(inout) :: u(:,:,:)
    integer :: i, k
    real(dp) :: gij(3,3), gijinv(3,3), alpha, beta_upper(3), Kij(3,3)
    real(dp) :: phi, Ktrace, detg, twoH
    real(dp) :: gtij(3,3), Aij(3,3)

    do k = 1, Nz
      do i = 1, Nx
        call kerr_schild_adm(xg(i), 0.0_dp, zg(k), gij, gijinv, alpha, beta_upper, Kij)

        ! --- Conformal factor -------------------------------------------
        ! mode 0 (legacy): phi = (1/12) ln det gamma, so det gamma-tilde = 1.
        !   This is a legal BSSN gauge but it is NOT the conventional choice:
        !   gamma-tilde stays far from flat, and the standard 1+log puncture
        !   construction (in which gamma-tilde -> delta in the punctured
        !   coordinates) cannot be expressed in it.
        !
        ! mode 1 (standard): psi^4 = 1 + 2H, i.e. phi = (1/4) ln(1+2H), the
        !   conventional Kerr-Schild conformal factor used by puncture codes.
        !   gamma-tilde = gamma / psi^4.
        ! -----------------------------------------------------------------
        detg = gij(1,1)*(gij(2,2)*gij(3,3)-gij(2,3)*gij(2,3)) &
             - gij(1,2)*(gij(1,2)*gij(3,3)-gij(2,3)*gij(1,3)) &
             + gij(1,3)*(gij(1,2)*gij(2,3)-gij(2,2)*gij(1,3))
        if (conformal_factor_mode == 1) then
          twoH = detg - 1.0_dp
          if (twoH <= 0.0_dp) twoH = 0.0_dp
          phi = 0.25_dp*log(1.0_dp + twoH)
        else
          phi = log(max(detg, 1.0e-300_dp)) / 12.0_dp
        end if

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
        if (puncture_lapse_mode == 1) then
          u(i,k,IALPHA) = puncture_lapse_shape(xg(i), zg(k)) * alpha
        end if
        u(i,k,IBETAX) = beta_upper(1); u(i,k,IBETAY) = beta_upper(2); u(i,k,IBETAZ) = beta_upper(3)
        u(i,k,IGTX) = 0.0_dp; u(i,k,IGTY) = 0.0_dp; u(i,k,IGTZ) = 0.0_dp
        u(i,k,IBX) = 0.0_dp;  u(i,k,IBY) = 0.0_dp;  u(i,k,IBZ) = 0.0_dp
      end do
    end do
  end subroutine set_kerr_schild_id

  ! ---------------------------------------------------------------------------
  ! Puncture shaping factor for the initial lapse.
  !
  !   f(r) = (r^2 - r_excise^2) / (r^2 + r_excise^2)
  !
  ! f = 0 at r = r_excise (so the 1+log lapse reaches zero on the excision
  ! boundary), f -> 1 as r -> infinity, and f' = 0 at r = r_excise, so the lapse
  ! is smooth across the puncture to second order.  It uses the plain grid
  ! radius because the puncture is a property of the Cartesian grid, not of the
  ! Kerr-Schild radial coordinate.
  !
  ! NOTE: this CHANGES the solution.  It is a diagnostic for the gauge-drift
  ! hypothesis, not constraint-satisfying initial data -- expect an O(1)
  ! Hamiltonian-constraint violation at t=0 wherever f differs appreciably
  ! from 1.
  ! ---------------------------------------------------------------------------
  function puncture_lapse_shape(x, z) result(f)
    real(dp), intent(in) :: x, z
    real(dp) :: f, r2
    r2 = (x - bh_center_x)**2 + (z - bh_center_z)**2
    if (r2 <= r_excise*r_excise) then
      f = 0.0_dp
    else
      f = (r2 - r_excise*r_excise) / (r2 + r_excise*r_excise)
    end if
  end function puncture_lapse_shape

  ! compute conformal connections via contracting the derivatives of the inverse metric
  subroutine compute_conformal_connections(u)
    real(dp), intent(inout) :: u(:,:,:)
    integer :: i, k
    real(dp) :: gxp(3,3), gxm(3,3), gyp(3,3), gym(3,3), gzp(3,3), gzm(3,3)
    real(dp) :: ginvxp(3,3), ginvxm(3,3), ginvyp(3,3), ginvym(3,3), ginvzp(3,3), ginvzm(3,3)
    real(dp) :: dginv(3,3,3), Gt(3)
    real(dp) :: det, ginv0(3,3), dphix, dphiz, gbar(3)
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

        ! ---------------------------------------------------------------------
        ! Gamma-driver variable initialisation.
        !
        ! Legacy mode (gamma_driver_mode == 0): IBX/IBY/IBZ are left at zero,
        ! exactly as this code has always done.  That is inconsistent with
        ! d_t beta^a = (3/4) Gbar^a, but changing it on its own makes the run
        ! WORSE (inner-boundary failure at step 14055 instead of 18285):
        ! the legacy driver is d_t Gbar = RHS_Gam - eta*Gbar, so a correctly
        ! sized Gbar ~ 1.5 immediately gets damped at rate eta=2 and destroys
        ! the shift.  The two fixes have to go together.
        !
        ! Puncture mode (gamma_driver_mode == 1): initialise the BARRED
        ! connection
        !
        !     Gbar^a = Gtilde^a + gtilde^{ab} d_b phi
        !
        ! which is what the puncture driver actually acts on.  phi is even in
        ! the Cartesian y direction by construction, so d_y phi vanishes on the
        ! stored plane and only the x and z derivatives enter.
        ! ---------------------------------------------------------------------
        if (gamma_driver_mode == 1) then
          call sym3_inverse(pack_g(u(i,k,:)), ginv0, det)
          vp = get_neighbor(u, i, k,  1, 0, 0.0_dp)
          vm = get_neighbor(u, i, k, -1, 0, 0.0_dp)
          dphix = (vp(IPHI) - vm(IPHI)) / (2.0_dp*dx)
          vp = get_neighbor(u, i, k,  0, 1, 0.0_dp)
          vm = get_neighbor(u, i, k,  0,-1, 0.0_dp)
          dphiz = (vp(IPHI) - vm(IPHI)) / (2.0_dp*dz)
          gbar(1) = Gt(1) + ginv0(1,1)*dphix + ginv0(1,3)*dphiz
          gbar(2) = Gt(2) + ginv0(2,1)*dphix + ginv0(2,3)*dphiz
          gbar(3) = Gt(3) + ginv0(3,1)*dphix + ginv0(3,3)*dphiz
          u(i,k,IBX) = gbar(1); u(i,k,IBY) = gbar(2); u(i,k,IBZ) = gbar(3)
        end if
      end do
    end do
  end subroutine compute_conformal_connections

  ! outer product
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

  ! ADM formulation
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


  function pack_g(arr) result(M)
    real(dp), intent(in) :: arr(:)
    real(dp) :: M(3,3)
    M(1,1) = arr(IGXX); M(1,2) = arr(IGXY); M(1,3) = arr(IGXZ)
    M(2,1) = arr(IGXY); M(2,2) = arr(IGYY); M(2,3) = arr(IGYZ)
    M(3,1) = arr(IGXZ); M(3,2) = arr(IGYZ); M(3,3) = arr(IGZZ)
  end function pack_g




end module initial_data_mod
