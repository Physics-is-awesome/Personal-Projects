module excision_sat_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, bh_spin, r_excise
  use grid_mod, only: xg, zg
  use vars_mod, only: NVARS, IALPHA, IPHI, IK, &
                      IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                      IBETAX, IBETAY, IBETAZ, IBX, IBY, IBZ
  use excision_mod, only: is_excised, get_excision_background_point
  use fo_curved_sat_mod, only: kerr_schild_excision_normal, kerr_radius
  use fo_normal_system_mod, only: fo_normal_block_eigensystem, &
                                  FO_BLOCK_LAPSE, FO_BLOCK_GRAVITY, &
                                  FO_BLOCK_SHIFT, FO_MAX_BLOCK
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  implicit none
  private

  ! Incoming/outgoing convention for the level-set penalty below.
  !
  ! The interface normal handed in by fo_curved_sat_mod is grad(r)/|grad(r)|,
  ! i.e. it points towards INCREASING radius, i.e. from the excised interior
  ! into the evolved exterior.  A characteristic mode travels along sign(lambda)
  ! times that normal, so:
  !
  !   lambda > 0 : travels outwards, INTO the evolved exterior -> INCOMING
  !                data that must be damped at the excision boundary.
  !   lambda < 0 : travels into the excised interior  -> harmless.
  !
  ! This is the same convention as excision_characteristic_check(), which
  ! flags speed = -beta_n + alpha*sqrt(2 gamma_nn) >= 0 at the inner ring.
  logical, parameter, public :: exact_excision_characteristics = .true.

  public :: apply_legacy_excision_sat
  public :: apply_characteristic_sat
  public :: apply_kerr_excision_sat
  public :: apply_fo_complete_excision_sat

contains

  subroutine apply_legacy_excision_sat(u, rhs, h, enabled, tau)
    real(dp), intent(in) :: u(:,:,:), h
    real(dp), intent(inout) :: rhs(:,:,:)
    logical, intent(in), optional :: enabled
    real(dp), intent(in), optional :: tau
    logical :: requested
    real(dp) :: tau_used

    requested = .false.
    if (present(enabled)) requested = enabled
    if (.not. requested) return

    if (size(u,1) /= size(rhs,1) .or. size(u,2) /= size(rhs,2) .or. &
        size(u,3) /= size(rhs,3) .or. h <= 0.0_dp) then
      error stop 'apply_legacy_excision_sat: incompatible state or spacing'
    end if

    ! The optional `tau` used to be forwarded to apply_kerr_excision_sat while
    ! still undefined, so an enabled penalty was scaled by whatever happened
    ! to be on the stack.  Default it explicitly to the unit penalty.
    tau_used = 1.0_dp
    if (present(tau)) tau_used = tau

    call apply_kerr_excision_sat(u, rhs, tau_used, h, h)
  end subroutine apply_legacy_excision_sat

  subroutine apply_characteristic_sat(state, target, speed, tau, h, rhs)
    real(dp), intent(in) :: state(:), target(:), speed(:), tau, h
    real(dp), intent(inout) :: rhs(:)
    integer :: n

    n = size(state)
    if (size(target) /= n .or. size(speed) /= n .or. size(rhs) /= n) &
      error stop 'apply_characteristic_sat: inconsistent mode sizes'
    if (tau < 0.0_dp .or. h <= 0.0_dp) &
      error stop 'apply_characteristic_sat: invalid penalty parameters'

    rhs = rhs - tau*min(speed, 0.0_dp)*(state-target)/h
  end subroutine apply_characteristic_sat

  subroutine apply_kerr_excision_sat(u, rhs, tau_in, hx_in, hz_in)
    real(dp), intent(in) :: u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    real(dp), intent(in), optional :: tau_in, hx_in, hz_in
    real(dp) :: tau, hx, hz, r_ks, norm_x, norm_z, gn, h_normal
    real(dp) :: alpha, phi, Ktr, beta_n, gnn, det
    real(dp) :: gxx, gxy, gxz, gyy, gyz, gzz
    real(dp) :: gi11, gi13, gi33, gamma_nn, Ann
    real(dp) :: d_alpha_x, d_alpha_z, dn_alpha
    real(dp) :: d_gnn_x, d_gnn_z, dn_gnn
    real(dp) :: d_betan_x, d_betan_z, dn_betan
    real(dp) :: u_bg(NVARS), bg_p(NVARS), bg_m(NVARS)
    real(dp) :: alpha_t, Ktr_t, beta_n_t, gnn_t, Ann_t
    real(dp) :: dn_alpha_t, dn_gnn_t, dn_betan_t
    real(dp) :: v(3), vt(3), w(3), wt(3)
    real(dp) :: pen_w(3), pen_v(3)
    real(dp) :: Amat(FO_MAX_BLOCK,FO_MAX_BLOCK), lam(FO_MAX_BLOCK)
    real(dp) :: Rmat(FO_MAX_BLOCK,FO_MAX_BLOCK), Lmat(FO_MAX_BLOCK,FO_MAX_BLOCK)
    character(len=32) :: labels(FO_MAX_BLOCK)
    integer :: i, k, m, n_modes, ip, im, kp, km
    real(dp) :: gp, gm

    if (.not. allocated(xg) .or. .not. allocated(zg)) return
    if (r_excise <= 0.0_dp) return

    tau = 1.0_dp
    if (present(tau_in)) tau = tau_in
    hx = dx
    if (present(hx_in)) hx = hx_in
    hz = dz
    if (present(hz_in)) hz = hz_in
    if (tau < 0.0_dp) tau = 0.0_dp
    if (hx <= 0.0_dp .or. hz <= 0.0_dp) return
    if (tau == 0.0_dp) return

    do k = 2, Nz-1
      kp = min(k+1, Nz)
      km = max(k-1, 1)
      do i = 1, Nx-1
        if (is_excised(i,k)) cycle

        r_ks = kerr_radius(xg(i), zg(k), bh_spin)
        if (r_ks > r_excise + 2.0_dp*max(hx, hz)) cycle

        call kerr_schild_excision_normal(xg(i), zg(k), bh_spin, r_excise, &
                                         norm_x, norm_z, gn)
        h_normal = 1.0_dp / sqrt((norm_x/hx)**2 + (norm_z/hz)**2)

        alpha = u(i,k,IALPHA)
        phi   = u(i,k,IPHI)
        Ktr   = u(i,k,IK)
        beta_n = u(i,k,IBETAX)*norm_x + u(i,k,IBETAZ)*norm_z

        gxx = u(i,k,IGXX); gxy = u(i,k,IGXY); gxz = u(i,k,IGXZ)
        gyy = u(i,k,IGYY); gyz = u(i,k,IGYZ); gzz = u(i,k,IGZZ)
        gnn = gxx*norm_x*norm_x + 2.0_dp*gxz*norm_x*norm_z + gzz*norm_z*norm_z

        det = gxx*(gyy*gzz - gyz*gyz) - gxy*(gxy*gzz - gyz*gxz) + &
              gxz*(gxy*gyz - gyy*gxz)
        if (det <= 0.0_dp .or. gnn <= 0.0_dp .or. alpha <= 0.0_dp) cycle

        gi11 = (gyy*gzz - gyz*gyz) / det
        gi13 = (gxy*gyz - gxz*gyy) / det
        gi33 = (gxx*gyy - gxy*gxy) / det
        gamma_nn = exp(-4.0_dp*phi) * (gi11*norm_x*norm_x + &
                   2.0_dp*gi13*norm_x*norm_z + gi33*norm_z*norm_z)
        if (gamma_nn <= 0.0_dp) cycle

        Ann = u(i,k,IAXX)*norm_x*norm_x + 2.0_dp*u(i,k,IAXZ)*norm_x*norm_z + &
              u(i,k,IAZZ)*norm_z*norm_z

        ip = min(i+1, Nx)
        im = max(i-1, 1)

        if (i == 1) then
          d_alpha_x = 0.0_dp
          d_gnn_x   = 0.0_dp
          d_betan_x = 0.0_dp
        else
          d_alpha_x = (u(ip,k,IALPHA) - u(im,k,IALPHA)) / (2.0_dp*hx)
          gp = u(ip,k,IGXX)*norm_x*norm_x + 2.0_dp*u(ip,k,IGXZ)*norm_x*norm_z + &
               u(ip,k,IGZZ)*norm_z*norm_z
          gm = u(im,k,IGXX)*norm_x*norm_x + 2.0_dp*u(im,k,IGXZ)*norm_x*norm_z + &
               u(im,k,IGZZ)*norm_z*norm_z
          d_gnn_x = (gp - gm) / (2.0_dp*hx)
          d_betan_x = ((u(ip,k,IBETAX)*norm_x + u(ip,k,IBETAZ)*norm_z) - &
                       (u(im,k,IBETAX)*norm_x + u(im,k,IBETAZ)*norm_z)) / (2.0_dp*hx)
        end if

        d_alpha_z = (u(i,kp,IALPHA) - u(i,km,IALPHA)) / (2.0_dp*hz)
        gp = u(i,kp,IGXX)*norm_x*norm_x + 2.0_dp*u(i,kp,IGXZ)*norm_x*norm_z + &
             u(i,kp,IGZZ)*norm_z*norm_z
        gm = u(i,km,IGXX)*norm_x*norm_x + 2.0_dp*u(i,km,IGXZ)*norm_x*norm_z + &
             u(i,km,IGZZ)*norm_z*norm_z
        d_gnn_z = (gp - gm) / (2.0_dp*hz)
        d_betan_z = ((u(i,kp,IBETAX)*norm_x + u(i,kp,IBETAZ)*norm_z) - &
                     (u(i,km,IBETAX)*norm_x + u(i,km,IBETAZ)*norm_z)) / (2.0_dp*hz)

        dn_alpha = norm_x*d_alpha_x + norm_z*d_alpha_z
        dn_gnn   = norm_x*d_gnn_x   + norm_z*d_gnn_z
        dn_betan = norm_x*d_betan_x + norm_z*d_betan_z

        call get_excision_background_point(i, k, u_bg)
        if (maxval(abs(u_bg)) <= 0.0_dp) u_bg = u(i,k,:)

        alpha_t = u_bg(IALPHA)
        Ktr_t   = u_bg(IK)
        gnn_t   = u_bg(IGXX)*norm_x*norm_x + 2.0_dp*u_bg(IGXZ)*norm_x*norm_z + &
                  u_bg(IGZZ)*norm_z*norm_z
        Ann_t   = u_bg(IAXX)*norm_x*norm_x + 2.0_dp*u_bg(IAXZ)*norm_x*norm_z + &
                  u_bg(IAZZ)*norm_z*norm_z
        beta_n_t = u_bg(IBETAX)*norm_x + u_bg(IBETAZ)*norm_z

        call get_excision_background_point(ip, k, bg_p)
        call get_excision_background_point(im, k, bg_m)
        if (i == 1) then
          d_alpha_x = 0.0_dp; d_gnn_x = 0.0_dp; d_betan_x = 0.0_dp
        else
          d_alpha_x = (bg_p(IALPHA) - bg_m(IALPHA)) / (2.0_dp*hx)
          gp = bg_p(IGXX)*norm_x*norm_x + 2.0_dp*bg_p(IGXZ)*norm_x*norm_z + &
               bg_p(IGZZ)*norm_z*norm_z
          gm = bg_m(IGXX)*norm_x*norm_x + 2.0_dp*bg_m(IGXZ)*norm_x*norm_z + &
               bg_m(IGZZ)*norm_z*norm_z
          d_gnn_x = (gp - gm) / (2.0_dp*hx)
          d_betan_x = ((bg_p(IBETAX)*norm_x + bg_p(IBETAZ)*norm_z) - &
                       (bg_m(IBETAX)*norm_x + bg_m(IBETAZ)*norm_z)) / (2.0_dp*hx)
        end if

        call get_excision_background_point(i, kp, bg_p)
        call get_excision_background_point(i, km, bg_m)
        d_alpha_z = (bg_p(IALPHA) - bg_m(IALPHA)) / (2.0_dp*hz)
        gp = bg_p(IGXX)*norm_x*norm_x + 2.0_dp*bg_p(IGXZ)*norm_x*norm_z + &
             bg_p(IGZZ)*norm_z*norm_z
        gm = bg_m(IGXX)*norm_x*norm_x + 2.0_dp*bg_m(IGXZ)*norm_x*norm_z + &
             bg_m(IGZZ)*norm_z*norm_z
        d_gnn_z = (gp - gm) / (2.0_dp*hz)
        d_betan_z = ((bg_p(IBETAX)*norm_x + bg_p(IBETAZ)*norm_z) - &
                     (bg_m(IBETAX)*norm_x + bg_m(IBETAZ)*norm_z)) / (2.0_dp*hz)

        dn_alpha_t = norm_x*d_alpha_x + norm_z*d_alpha_z
        dn_gnn_t   = norm_x*d_gnn_x   + norm_z*d_gnn_z
        dn_betan_t = norm_x*d_betan_x + norm_z*d_betan_z

        ! Block 1: Lapse
        call fo_normal_block_eigensystem(FO_BLOCK_LAPSE, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        ! Published slot order: the propagating gauge mode is
        ! G^{(alpha,+-)} = K -+ alpha_n/(alpha sqrt(f)), so the +-speed pair is
        ! (K, alpha_n) and the lapse itself is the zero-speed slot.
        v  = [Ktr, dn_alpha, alpha]
        vt = [Ktr_t, dn_alpha_t, alpha_t]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IK)     = rhs(i,k,IK)     + pen_v(1)
        rhs(i,k,IALPHA) = rhs(i,k,IALPHA) + pen_v(3)

        ! Block 2: Gravity
        call fo_normal_block_eigensystem(FO_BLOCK_GRAVITY, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        v  = [gnn, dn_gnn, Ann]
        vt = [gnn_t, dn_gnn_t, Ann_t]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IGXX) = rhs(i,k,IGXX) + pen_v(1)*norm_x*norm_x
        rhs(i,k,IGXZ) = rhs(i,k,IGXZ) + pen_v(1)*norm_x*norm_z
        rhs(i,k,IGZZ) = rhs(i,k,IGZZ) + pen_v(1)*norm_z*norm_z
        rhs(i,k,IAXX) = rhs(i,k,IAXX) + pen_v(3)*(norm_x*norm_x - &
                        (1.0_dp/3.0_dp)*gxx*gamma_nn*exp(4.0_dp*phi))
        rhs(i,k,IAXZ) = rhs(i,k,IAXZ) + pen_v(3)*(norm_x*norm_z - &
                        (1.0_dp/3.0_dp)*gxz*gamma_nn*exp(4.0_dp*phi))
        rhs(i,k,IAZZ) = rhs(i,k,IAZZ) + pen_v(3)*(norm_z*norm_z - &
                        (1.0_dp/3.0_dp)*gzz*gamma_nn*exp(4.0_dp*phi))

        ! Block 3: Shift
        call fo_normal_block_eigensystem(FO_BLOCK_SHIFT, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        v  = [beta_n, dn_betan, u(i,k,IBX)*norm_x + u(i,k,IBZ)*norm_z]
        vt = [beta_n_t, dn_betan_t, u_bg(IBX)*norm_x + u_bg(IBZ)*norm_z]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IBETAX) = rhs(i,k,IBETAX) + pen_v(1)*norm_x
        rhs(i,k,IBETAZ) = rhs(i,k,IBETAZ) + pen_v(1)*norm_z
        rhs(i,k,IBX)    = rhs(i,k,IBX)    + pen_v(3)*norm_x
        rhs(i,k,IBZ)    = rhs(i,k,IBZ)    + pen_v(3)*norm_z
      end do
    end do
  end subroutine apply_kerr_excision_sat

  subroutine apply_fo_complete_excision_sat(q, rhs, h, enabled, tau)
    real(dp), intent(in) :: q(:,:,:), h
    real(dp), intent(inout) :: rhs(:,:,:)
    logical, intent(in), optional :: enabled
    real(dp), intent(in), optional :: tau
    real(dp) :: tau_val, r_ks, norm_x, norm_z, gn, h_normal
    real(dp) :: alpha, phi, Ktr, beta_n, gnn, det
    real(dp) :: gxx, gxy, gxz, gyy, gyz, gzz
    real(dp) :: gi11, gi13, gi33, gamma_nn, Ann
    real(dp) :: dn_alpha, dn_gnn, dn_betan
    real(dp) :: u_bg(NVARS)
    real(dp) :: alpha_t, Ktr_t, beta_n_t, gnn_t, Ann_t
    real(dp) :: dn_alpha_t, dn_gnn_t, dn_betan_t
    real(dp) :: v(3), vt(3), w(3), wt(3)
    real(dp) :: pen_w(3), pen_v(3)
    real(dp) :: Amat(FO_MAX_BLOCK,FO_MAX_BLOCK), lam(FO_MAX_BLOCK)
    real(dp) :: Rmat(FO_MAX_BLOCK,FO_MAX_BLOCK), Lmat(FO_MAX_BLOCK,FO_MAX_BLOCK)
    character(len=32) :: labels(FO_MAX_BLOCK)
    logical :: requested
    integer :: i, k, m, n_modes

    requested = .false.
    if (present(enabled)) requested = enabled
    if (.not. requested) return

    if (size(q,1) /= size(rhs,1) .or. size(q,2) /= size(rhs,2) .or. &
        size(q,3) < NFO_COMPLETE_VARS .or. size(rhs,3) < NFO_COMPLETE_VARS .or. &
        h <= 0.0_dp) return

    tau_val = 1.0_dp
    if (present(tau)) tau_val = tau

    do k = 2, Nz-1
      do i = 1, Nx-1
        if (is_excised(i,k)) cycle

        r_ks = kerr_radius(xg(i), zg(k), bh_spin)
        if (r_ks > r_excise + 2.0_dp*max(dx, dz)) cycle

        call kerr_schild_excision_normal(xg(i), zg(k), bh_spin, r_excise, &
                                         norm_x, norm_z, gn)
        h_normal = 1.0_dp / sqrt((norm_x/dx)**2 + (norm_z/dz)**2)

        alpha = q(i,k,IALPHA)
        phi   = q(i,k,IPHI)
        Ktr   = q(i,k,IK)
        beta_n = q(i,k,IBETAX)*norm_x + q(i,k,IBETAZ)*norm_z

        gxx = q(i,k,IGXX); gxy = q(i,k,IGXY); gxz = q(i,k,IGXZ)
        gyy = q(i,k,IGYY); gyz = q(i,k,IGYZ); gzz = q(i,k,IGZZ)
        gnn = gxx*norm_x*norm_x + 2.0_dp*gxz*norm_x*norm_z + gzz*norm_z*norm_z

        det = gxx*(gyy*gzz - gyz*gyz) - gxy*(gxy*gzz - gyz*gxz) + &
              gxz*(gxy*gyz - gyy*gxz)
        if (det <= 0.0_dp .or. gnn <= 0.0_dp .or. alpha <= 0.0_dp) cycle

        gi11 = (gyy*gzz - gyz*gyz) / det
        gi13 = (gxy*gyz - gxz*gyy) / det
        gi33 = (gxx*gyy - gxy*gxy) / det
        gamma_nn = exp(-4.0_dp*phi) * (gi11*norm_x*norm_x + &
                   2.0_dp*gi13*norm_x*norm_z + gi33*norm_z*norm_z)
        if (gamma_nn <= 0.0_dp) cycle

        Ann = q(i,k,IAXX)*norm_x*norm_x + 2.0_dp*q(i,k,IAXZ)*norm_x*norm_z + &
              q(i,k,IAZZ)*norm_z*norm_z

        dn_alpha = norm_x*q(i,k,fo_complete_index(IALPHA,1)) + &
                   norm_z*q(i,k,fo_complete_index(IALPHA,3))
        dn_gnn   = norm_x*(q(i,k,fo_complete_index(IGXX,1))*norm_x*norm_x + &
                       2.0_dp*q(i,k,fo_complete_index(IGXZ,1))*norm_x*norm_z + &
                       q(i,k,fo_complete_index(IGZZ,1))*norm_z*norm_z) + &
                   norm_z*(q(i,k,fo_complete_index(IGXX,3))*norm_x*norm_x + &
                       2.0_dp*q(i,k,fo_complete_index(IGXZ,3))*norm_x*norm_z + &
                       q(i,k,fo_complete_index(IGZZ,3))*norm_z*norm_z)
        dn_betan = norm_x*(q(i,k,fo_complete_index(IBETAX,1))*norm_x + &
                           q(i,k,fo_complete_index(IBETAZ,1))*norm_z) + &
                   norm_z*(q(i,k,fo_complete_index(IBETAX,3))*norm_x + &
                           q(i,k,fo_complete_index(IBETAZ,3))*norm_z)

        call get_excision_background_point(i, k, u_bg)
        if (maxval(abs(u_bg)) <= 0.0_dp) u_bg = q(i,k,1:NVARS)

        alpha_t = u_bg(IALPHA)
        Ktr_t   = u_bg(IK)
        gnn_t   = u_bg(IGXX)*norm_x*norm_x + 2.0_dp*u_bg(IGXZ)*norm_x*norm_z + &
                  u_bg(IGZZ)*norm_z*norm_z
        Ann_t   = u_bg(IAXX)*norm_x*norm_x + 2.0_dp*u_bg(IAXZ)*norm_x*norm_z + &
                  u_bg(IAZZ)*norm_z*norm_z
        beta_n_t = u_bg(IBETAX)*norm_x + u_bg(IBETAZ)*norm_z
        dn_alpha_t = 0.0_dp
        dn_gnn_t   = 0.0_dp
        dn_betan_t = 0.0_dp

        ! Block 1: Lapse
        call fo_normal_block_eigensystem(FO_BLOCK_LAPSE, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        ! Published slot order: the propagating gauge mode is
        ! G^{(alpha,+-)} = K -+ alpha_n/(alpha sqrt(f)), so the +-speed pair is
        ! (K, alpha_n) and the lapse itself is the zero-speed slot.
        v  = [Ktr, dn_alpha, alpha]
        vt = [Ktr_t, dn_alpha_t, alpha_t]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau_val * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IK)     = rhs(i,k,IK)     + pen_v(1)
        rhs(i,k,IALPHA) = rhs(i,k,IALPHA) + pen_v(3)
        rhs(i,k,fo_complete_index(IALPHA,1)) = rhs(i,k,fo_complete_index(IALPHA,1)) + &
                                               pen_v(2)*norm_x
        rhs(i,k,fo_complete_index(IALPHA,3)) = rhs(i,k,fo_complete_index(IALPHA,3)) + &
                                               pen_v(2)*norm_z

        ! Block 2: Gravity
        call fo_normal_block_eigensystem(FO_BLOCK_GRAVITY, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        v  = [gnn, dn_gnn, Ann]
        vt = [gnn_t, dn_gnn_t, Ann_t]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau_val * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IGXX) = rhs(i,k,IGXX) + pen_v(1)*norm_x*norm_x
        rhs(i,k,IGXZ) = rhs(i,k,IGXZ) + pen_v(1)*norm_x*norm_z
        rhs(i,k,IGZZ) = rhs(i,k,IGZZ) + pen_v(1)*norm_z*norm_z
        rhs(i,k,IAXX) = rhs(i,k,IAXX) + pen_v(3)*(norm_x*norm_x - &
                        (1.0_dp/3.0_dp)*gxx*gamma_nn*exp(4.0_dp*phi))
        rhs(i,k,IAXZ) = rhs(i,k,IAXZ) + pen_v(3)*(norm_x*norm_z - &
                        (1.0_dp/3.0_dp)*gxz*gamma_nn*exp(4.0_dp*phi))
        rhs(i,k,IAZZ) = rhs(i,k,IAZZ) + pen_v(3)*(norm_z*norm_z - &
                        (1.0_dp/3.0_dp)*gzz*gamma_nn*exp(4.0_dp*phi))

        ! Block 3: Shift
        call fo_normal_block_eigensystem(FO_BLOCK_SHIFT, alpha, phi, gamma_nn, beta_n, &
                                         n_modes, Amat, lam, Rmat, Lmat, labels)
        v  = [beta_n, dn_betan, q(i,k,IBX)*norm_x + q(i,k,IBZ)*norm_z]
        vt = [beta_n_t, dn_betan_t, u_bg(IBX)*norm_x + u_bg(IBZ)*norm_z]
        w  = matmul(Lmat(1:3,1:3), v)
        wt = matmul(Lmat(1:3,1:3), vt)
        pen_w = 0.0_dp
        do m = 1, 3
          if (lam(m) > 0.0_dp) pen_w(m) = -tau_val * lam(m) * (w(m) - wt(m)) / h_normal
        end do
        pen_v = matmul(Rmat(1:3,1:3), pen_w)
        rhs(i,k,IBETAX) = rhs(i,k,IBETAX) + pen_v(1)*norm_x
        rhs(i,k,IBETAZ) = rhs(i,k,IBETAZ) + pen_v(1)*norm_z
        rhs(i,k,IBX)    = rhs(i,k,IBX)    + pen_v(3)*norm_x
        rhs(i,k,IBZ)    = rhs(i,k,IBZ)    + pen_v(3)*norm_z
      end do
    end do
  end subroutine apply_fo_complete_excision_sat

end module excision_sat_mod
