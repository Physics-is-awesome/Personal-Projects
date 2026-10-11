! VERSION v3 (2026-10-01): vars tables ICONFMET/IATILDE + horizon_adm_from_state(u,...) adapter
!===============================================================================
! horizon_mod.f90  --  axisymmetric apparent-horizon finder + BH diagnostics
!                      for a Cartoon (x,z; y suppressed) BSSNOK code.
!
! WHAT IT DOES
!   * Finds the outermost marginally-trapped surface  r = h(mu),  mu = cos(theta),
!     measured from the point (x=0, z=zc), by Newton iteration on the Legendre
!     coefficients of h:   h(mu) = sum_l a_l P_l(mu)      (even l only if eq_sym)
!   * Solves   Theta = D_i s^i - K + K_ij s^i s^j = 0   (outgoing null expansion)
!   * Reports  area A, irreducible mass M_irr = sqrt(A/16pi), spin J (Killing
!     vector d/dphi), Christodoulou mass M_H, and h_min/h_max (excision check).
!
! CONVENTIONS / ASSUMPTIONS  (check these against your code!)
!   1. Axisymmetry about the z-axis; the Cartoon grid holds the y=0 plane with
!      x = cylindrical radius rho >= 0.  Tensors are CARTESIAN components on that
!      plane, packed (1..6) = (xx, xy, xz, yy, yz, zz).  xy and yz are generally
!      NONZERO for a spinning BH (Kerr-Schild has gamma_{rho phi} /= 0), so they
!      are carried, not dropped.
!   2. Arrays passed in are the *interior* grid only (no ghost zones), with
!         x(i) = x0 + (i-1)*dx ,   z(j) = z0 + (j-1)*dz
!      x0 must be 0 (vertex-centred) or dx/2 (cell-centred); same for z0 if
!      eq_sym = .true.   Values at rho<0 are obtained by the pi-rotation
!      parity (xz, yz odd); at z<0 (eq_sym only) by reflection (xz, yz odd).
!   3. ADM variables:  gamma_ij = e^{4phi} gt_ij ,
!                      K_ij     = e^{4phi} ( At_ij + gt_ij K/3 )
!      with the BSSN sign convention  K_ij = -(1/2 alpha)(d_t gamma_ij - L_beta gamma_ij).
!      If your K has the opposite sign, call horizon_set_k_sign(-1.0_hdp).
!   4. The BH must be on the axis (x=0); it may be offset in z (zc /= 0, then
!      use eq_sym = .false. and a grid covering both z hemispheres).
!
! TYPICAL USE
!     call horizon_init_grid(x0,z0,dx,dz,nx,nz,.true.)       ! once
!     call horizon_init(hz, 8, 0.0_hdp, .true.)               ! once
!     ...every N steps:
!     call horizon_adm_from_bssn(e4phi, gt, At, Ktr)          ! fill ADM copies
!     call find_horizon(hz, 2.0_hdp, ierr)                    ! guess used 1st call
!     if (ierr == 0) call horizon_log_line(unit, step, t, hz)
!===============================================================================
module horizon_mod
  implicit none
  private

  integer,   parameter, public :: hdp = kind(1.0d0)
  real(hdp), parameter :: pi = 3.141592653589793238462643383279502884_hdp
  integer,   parameter, public :: HZ_MAXL = 24            ! max # Legendre terms
  integer,   parameter :: LMAXP = 2*HZ_MAXL + 2
  real(hdp), parameter :: SGN(6) = [1._hdp, 1._hdp, -1._hdp, 1._hdp, -1._hdp, 1._hdp]

  type, public :: horizon_t
     integer   :: nl = 8
     logical   :: eq_sym = .true.
     real(hdp) :: zc = 0.0_hdp
     integer   :: lval(HZ_MAXL) = 0
     real(hdp) :: coef(HZ_MAXL) = 0.0_hdp
     logical   :: found = .false.
     integer   :: niter = 0
     real(hdp) :: resid = 0.0_hdp                       ! max |Theta| at collocation pts
     real(hdp) :: area = 0.0_hdp, m_irr = 0.0_hdp, j_spin = 0.0_hdp
     real(hdp) :: m_h = 0.0_hdp, hmin = 0.0_hdp, hmax = 0.0_hdp
  end type horizon_t

  ! ---- module state: grid + ADM copies ---------------------------------------
  integer   :: nx = 0, nz = 0, ioff_x = 0, ioff_z = 0
  real(hdp) :: x0 = 0, z0 = 0, dxg = 1, dzg = 1
  logical   :: eqsym = .true.
  real(hdp), allocatable :: gam(:,:,:), kex(:,:,:)
  real(hdp) :: ksign  = 1.0_hdp
  real(hdp) :: fd_frac = 0.5_hdp                         ! FD step = fd_frac*min(dx,dz)
  integer   :: n_quad = 96                               ! quadrature pts in theta

  public :: horizon_init_grid, horizon_load_adm, horizon_adm_from_bssn
  public :: horizon_adm_from_state
  public :: horizon_init, find_horizon, horizon_diagnostics
  public :: horizon_inside_excision, horizon_set_k_sign
  public :: horizon_log_header, horizon_log_line
  public :: interp_metric, expansion

contains

  !-----------------------------------------------------------------------------
  subroutine horizon_set_k_sign(s)
    real(hdp), intent(in) :: s
    ksign = s
  end subroutine horizon_set_k_sign

  !-----------------------------------------------------------------------------
  subroutine horizon_init_grid(x0_, z0_, dx_, dz_, nx_, nz_, eq_sym)
    real(hdp), intent(in) :: x0_, z0_, dx_, dz_
    integer,   intent(in) :: nx_, nz_
    logical,   intent(in) :: eq_sym
    if (x0_ < 0.0_hdp) error stop 'horizon_mod: x0 must be >= 0'
    ioff_x = nint(2.0_hdp*x0_/dx_)
    if (abs(2.0_hdp*x0_/dx_ - ioff_x) > 1.0e-8_hdp) &
         error stop 'horizon_mod: x0 must be 0 or a half-integer multiple of dx'
    ioff_z = 0
    if (eq_sym) then
       if (z0_ < 0.0_hdp) error stop 'horizon_mod: z0 must be >= 0 when eq_sym'
       ioff_z = nint(2.0_hdp*z0_/dz_)
       if (abs(2.0_hdp*z0_/dz_ - ioff_z) > 1.0e-8_hdp) &
            error stop 'horizon_mod: z0 must be 0 or a half-integer multiple of dz'
    end if
    x0 = x0_; z0 = z0_; dxg = dx_; dzg = dz_; nx = nx_; nz = nz_; eqsym = eq_sym
    if (allocated(gam)) deallocate(gam)
    if (allocated(kex)) deallocate(kex)
    allocate(gam(6,nx,nz), kex(6,nx,nz))
    gam = 0.0_hdp; kex = 0.0_hdp
  end subroutine horizon_init_grid

  !-----------------------------------------------------------------------------
  ! Adapter 1: you already have physical ADM arrays.
  subroutine horizon_load_adm(gam_in, k_in)
    real(hdp), intent(in) :: gam_in(6,nx,nz), k_in(6,nx,nz)
    gam = gam_in
    kex = k_in
  end subroutine horizon_load_adm

  ! Adapter 2: build ADM arrays from BSSN variables.
  !   e4phi(i,j) = e^{4 phi}   (if you evolve chi: e4phi = 1/chi; if W: 1/W**2)
  !   gt(6,..)   conformal metric, At(6,..) conformal traceless K, Ktr = trace K
  subroutine horizon_adm_from_bssn(e4phi, gt, At, Ktr)
    real(hdp), intent(in) :: e4phi(nx,nz), gt(6,nx,nz), At(6,nx,nz), Ktr(nx,nz)
    integer :: c
    do c = 1, 6
       gam(c,:,:) = e4phi*gt(c,:,:)
       kex(c,:,:) = e4phi*(At(c,:,:) + gt(c,:,:)*Ktr/3.0_hdp)
    end do
  end subroutine horizon_adm_from_bssn


  ! Adapter 3 (for drivers that keep the whole state in u(i,k,var)):
  !   gamma_ij = e^{4 phi} gt_ij ,  K_ij = e^{4 phi} ( At_ij + gt_ij K/3 )
  !   iphi, ik      : variable indices of phi and K
  !   igt(6),iat(6) : variable indices of gt and At, packed (xx,xy,xz,yy,yz,zz)
  !   ilo, klo      : array index of the FIRST interior point (x0,z0), i.e. skip
  !                   any ghost zones; nx,nz from horizon_init_grid are used.
  subroutine horizon_adm_from_state(u, iphi, ik, igt, iat, ilo, klo)
    real(hdp), intent(in) :: u(:,:,:)
    integer,   intent(in) :: iphi, ik, igt(6), iat(6), ilo, klo
    integer   :: c, i, k, ii, kk
    real(hdp) :: e4, kt
    do k = 1, nz
       kk = klo + k - 1
       do i = 1, nx
          ii = ilo + i - 1
          e4 = exp(4.0_hdp*u(ii,kk,iphi))
          kt = u(ii,kk,ik)
          do c = 1, 6
             gam(c,i,k) = e4*u(ii,kk,igt(c))
             kex(c,i,k) = e4*(u(ii,kk,iat(c)) + u(ii,kk,igt(c))*kt/3.0_hdp)
          end do
       end do
    end do
  end subroutine horizon_adm_from_state

  !-----------------------------------------------------------------------------
  subroutine horizon_init(hh, nl, zc, eq_sym)
    type(horizon_t), intent(inout) :: hh
    integer,         intent(in)    :: nl
    real(hdp),       intent(in)    :: zc
    logical,         intent(in)    :: eq_sym
    integer :: n
    if (nl < 1 .or. nl > HZ_MAXL) error stop 'horizon_mod: bad nl'
    hh%nl = nl; hh%zc = zc; hh%eq_sym = eq_sym
    hh%lval = 0; hh%coef = 0.0_hdp
    call clear_results(hh)
    do n = 1, nl
       if (eq_sym) then
          hh%lval(n) = 2*(n-1)
       else
          hh%lval(n) = n-1
       end if
    end do
    hh%found = .false.
  end subroutine horizon_init

  ! Zero all reported results so a failed solve never leaves stale numbers.
  subroutine clear_results(hh)
    type(horizon_t), intent(inout) :: hh
    hh%niter = 0;  hh%resid = 0.0_hdp
    hh%area = 0.0_hdp;  hh%m_irr = 0.0_hdp;  hh%j_spin = 0.0_hdp
    hh%m_h = 0.0_hdp;  hh%hmin = 0.0_hdp;  hh%hmax = 0.0_hdp
  end subroutine clear_results

  !-----------------------------------------------------------------------------
  ! Parity-aware access to a grid node (indices may lie outside 1..n).
  function node_val(arr, c, i, j, ok) result(v)
    real(hdp), intent(in)    :: arr(:,:,:)
    integer,   intent(in)    :: c, i, j
    logical,   intent(inout) :: ok
    real(hdp) :: v, s
    integer   :: ii, jj
    ii = i; jj = j; s = 1.0_hdp
    if (ii < 1) then
       ii = 2 - ii - ioff_x
       s  = s*SGN(c)
    end if
    if (eqsym .and. jj < 1) then
       jj = 2 - jj - ioff_z
       s  = s*SGN(c)
    end if
    if (ii < 1 .or. ii > nx .or. jj < 1 .or. jj > nz) then
       ok = .false.
       v = 0.0_hdp
       return
    end if
    v = s*arr(c,ii,jj)
  end function node_val

  subroutine lag4(t, w)                 ! cubic Lagrange weights, nodes -1,0,1,2
    real(hdp), intent(in)  :: t
    real(hdp), intent(out) :: w(-1:2)
    w(-1) = -t*(t-1._hdp)*(t-2._hdp)/6._hdp
    w( 0) =  (t+1._hdp)*(t-1._hdp)*(t-2._hdp)/2._hdp
    w( 1) = -(t+1._hdp)*t*(t-2._hdp)/2._hdp
    w( 2) =  (t+1._hdp)*t*(t-1._hdp)/6._hdp
  end subroutine lag4

  !-----------------------------------------------------------------------------
  ! Interpolate gamma_ij and K_ij to (rho, z) on the y=0 plane (4th-order).
  subroutine interp_metric(rho, z, g, k, ok)
    real(hdp), intent(in)    :: rho, z
    real(hdp), intent(out)   :: g(6), k(6)
    logical,   intent(inout) :: ok
    real(hdp) :: fx, fz, tx, tz, wx(-1:2), wz(-1:2), w
    integer   :: ifx, ifz, i0, j0, a, b, c
    fx = (rho - x0)/dxg;  ifx = floor(fx);  tx = fx - ifx;  i0 = ifx + 1
    fz = (z   - z0)/dzg;  ifz = floor(fz);  tz = fz - ifz;  j0 = ifz + 1
    call lag4(tx, wx);  call lag4(tz, wz)
    g = 0.0_hdp;  k = 0.0_hdp
    do b = -1, 2
       do a = -1, 2
          w = wx(a)*wz(b)
          do c = 1, 6
             g(c) = g(c) + w*node_val(gam, c, i0+a, j0+b, ok)
             k(c) = k(c) + w*node_val(kex, c, i0+a, j0+b, ok)
          end do
       end do
    end do
  end subroutine interp_metric

  !-----------------------------------------------------------------------------
  ! Inverse and determinant of a symmetric 3x3 packed (xx,xy,xz,yy,yz,zz).
  subroutine sym_inv(g, gi, det)
    real(hdp), intent(in)  :: g(6)
    real(hdp), intent(out) :: gi(6), det
    real(hdp) :: cxx, cxy, cxz, cyy, cyz, czz
    cxx = g(4)*g(6) - g(5)*g(5)
    cxy = g(3)*g(5) - g(2)*g(6)
    cxz = g(2)*g(5) - g(4)*g(3)
    cyy = g(1)*g(6) - g(3)*g(3)
    cyz = g(2)*g(3) - g(1)*g(5)
    czz = g(1)*g(4) - g(2)*g(2)
    det = g(1)*cxx + g(2)*cxy + g(3)*cxz
    if (abs(det) < tiny(1.0_hdp)) then
       gi = 0.0_hdp
       return
    end if
    gi = [cxx, cxy, cxz, cyy, cyz, czz]/det
  end subroutine sym_inv

  !-----------------------------------------------------------------------------
  subroutine legendre_all(lmax, mu, P, dP)
    integer,   intent(in)  :: lmax
    real(hdp), intent(in)  :: mu
    real(hdp), intent(out) :: P(0:LMAXP), dP(0:LMAXP)
    integer :: l
    P = 0.0_hdp; dP = 0.0_hdp
    P(0) = 1.0_hdp
    if (lmax >= 1) then
       P(1) = mu;  dP(1) = 1.0_hdp
    end if
    do l = 2, lmax
       P(l)  = ((2*l-1)*mu*P(l-1) - (l-1)*P(l-2))/real(l, hdp)
       dP(l) = dP(l-2) + (2*l-1)*P(l-1)
    end do
  end subroutine legendre_all

  subroutine surf_h(hh, mu, h, hmu)
    type(horizon_t), intent(in)  :: hh
    real(hdp),       intent(in)  :: mu
    real(hdp),       intent(out) :: h, hmu
    real(hdp) :: P(0:LMAXP), dP(0:LMAXP)
    integer :: n, lmax
    lmax = maxval(hh%lval(1:hh%nl))
    call legendre_all(lmax, max(-1.0_hdp, min(1.0_hdp, mu)), P, dP)
    h = 0.0_hdp; hmu = 0.0_hdp
    do n = 1, hh%nl
       h   = h   + hh%coef(n)*P(hh%lval(n))
       hmu = hmu + hh%coef(n)*dP(hh%lval(n))
    end do
  end subroutine surf_h

  !-----------------------------------------------------------------------------
  ! Vector density W^a = rho*sqrt(det gamma)*s^a  (a = x,z) of the unit normal to
  ! F = r - h(mu) = 0, extended off the surface.  Also returns s^i (3 comps),
  ! sqrt(det), inverse metric and K_ij at the point.
  subroutine w_vector(hh, rho, z, wx, wz, s, dens, gi, k, ok)
    type(horizon_t), intent(in)    :: hh
    real(hdp),       intent(in)    :: rho, z
    real(hdp),       intent(out)   :: wx, wz, s(3), dens, gi(6), k(6)
    logical,         intent(inout) :: ok
    real(hdp) :: g(6), det, zp, r, mu, h, hmu, fr, fz, nn2, nn
    wx = 0; wz = 0; s = 0; dens = 1; gi = 0
    call interp_metric(rho, z, g, k, ok)
    if (.not. ok) return
    call sym_inv(g, gi, det)
    if (det <= 0.0_hdp) then
       ok = .false.; return
    end if
    dens = sqrt(det)
    zp = z - hh%zc
    r  = sqrt(rho*rho + zp*zp)
    if (r < 1.0e-10_hdp) then
       ok = .false.; return
    end if
    mu = zp/r
    call surf_h(hh, mu, h, hmu)
    fr  = rho/r + hmu*zp*rho/r**3          ! dF/drho
    fz  = zp/r  - hmu*rho*rho/r**3         ! dF/dz
    nn2 = fr*(gi(1)*fr + gi(3)*fz) + fz*(gi(3)*fr + gi(6)*fz)
    if (nn2 <= 0.0_hdp) then
       ok = .false.; return
    end if
    nn   = sqrt(nn2)
    s(1) = (gi(1)*fr + gi(3)*fz)/nn
    s(2) = (gi(2)*fr + gi(5)*fz)/nn
    s(3) = (gi(3)*fr + gi(6)*fz)/nn
    wx = rho*dens*s(1)
    wz = rho*dens*s(3)
  end subroutine w_vector

  !-----------------------------------------------------------------------------
  ! Outgoing null expansion Theta at the surface point with polar cosine mu:
  !   Theta = div(s) - K + K_ij s^i s^j        (div via 4th-order FD of W^a)
  function expansion(hh, mu, ok) result(theta)
    type(horizon_t), intent(in)    :: hh
    real(hdp),       intent(in)    :: mu
    logical,         intent(inout) :: ok
    real(hdp) :: theta
    real(hdp) :: h, hmu, rho, z, eps, wx(-2:2), wz(-2:2), dum1, dum2, ddum
    real(hdp) :: s(3), dens, gi(6), k(6), sd(3), gd(6), kd(6)
    real(hdp) :: dwx, dwz, div, ktr, kss
    integer :: m
    call surf_h(hh, mu, h, hmu)
    if (h <= 0.0_hdp) then
       ok = .false.; theta = 1.0e30_hdp; return
    end if
    rho = h*sqrt(max(0.0_hdp, 1.0_hdp - mu*mu))
    z   = hh%zc + h*mu
    eps = fd_frac*min(dxg, dzg)
    call w_vector(hh, rho, z, wx(0), wz(0), s, dens, gi, k, ok)
    do m = -2, 2
       if (m == 0) cycle
       call w_vector(hh, rho + m*eps, z, wx(m), dum1, sd, ddum, gd, kd, ok)
       call w_vector(hh, rho, z + m*eps, dum2, wz(m), sd, ddum, gd, kd, ok)
    end do
    if (.not. ok .or. rho <= 1.0e-12_hdp) then
       ok = .false.; theta = 1.0e30_hdp; return
    end if
    dwx = (-wx(2) + 8*wx(1) - 8*wx(-1) + wx(-2))/(12*eps)
    dwz = (-wz(2) + 8*wz(1) - 8*wz(-1) + wz(-2))/(12*eps)
    div = (dwx + dwz)/(rho*dens)
    ktr = gi(1)*k(1) + gi(4)*k(4) + gi(6)*k(6) &
        + 2*(gi(2)*k(2) + gi(3)*k(3) + gi(5)*k(5))
    kss = k(1)*s(1)**2 + k(4)*s(2)**2 + k(6)*s(3)**2 &
        + 2*(k(2)*s(1)*s(2) + k(3)*s(1)*s(3) + k(5)*s(2)*s(3))
    theta = div + ksign*(kss - ktr)
  end function expansion

  !-----------------------------------------------------------------------------
  subroutine gauleg(n, x, w)             ! Gauss-Legendre nodes/weights on (-1,1)
    integer,   intent(in)  :: n
    real(hdp), intent(out) :: x(n), w(n)
    integer :: i, j, m
    real(hdp) :: z, z1, p1, p2, p3, pp
    m = (n+1)/2
    do i = 1, m
       z = cos(pi*(i-0.25_hdp)/(n+0.5_hdp))
       do
          p1 = 1.0_hdp; p2 = 0.0_hdp
          do j = 1, n
             p3 = p2; p2 = p1
             p1 = ((2*j-1)*z*p2 - (j-1)*p3)/real(j, hdp)
          end do
          pp = n*(z*p1 - p2)/(z*z - 1.0_hdp)
          z1 = z
          z  = z1 - p1/pp
          if (abs(z - z1) < 1.0e-15_hdp) exit
       end do
       x(i)     = -z;   x(n+1-i) = z
       w(i)     = 2.0_hdp/((1.0_hdp - z*z)*pp*pp);  w(n+1-i) = w(i)
    end do
  end subroutine gauleg

  subroutine colloc_nodes(hh, mu)
    type(horizon_t), intent(in)  :: hh
    real(hdp),       intent(out) :: mu(hh%nl)
    real(hdp), allocatable :: xg(:), wg(:)
    integer :: n
    n = hh%nl
    if (hh%eq_sym) then
       allocate(xg(2*n), wg(2*n))
       call gauleg(2*n, xg, wg)
       mu = xg(n+1:2*n)                 ! positive half
    else
       allocate(xg(n), wg(n))
       call gauleg(n, xg, wg)
       mu = xg
    end if
  end subroutine colloc_nodes

  subroutine residual(hh, mu, f, ok)
    type(horizon_t), intent(in)    :: hh
    real(hdp),       intent(in)    :: mu(:)
    real(hdp),       intent(out)   :: f(:)
    logical,         intent(inout) :: ok
    integer :: k
    do k = 1, size(mu)
       f(k) = expansion(hh, mu(k), ok)
    end do
  end subroutine residual

  subroutine solve_lin(n, a, b, ok)      ! Gaussian elimination, partial pivoting
    integer,   intent(in)    :: n
    real(hdp), intent(inout) :: a(n,n), b(n)
    logical,   intent(out)   :: ok
    integer :: i, j, p
    real(hdp) :: t, fac, row(n)
    ok = .true.
    do i = 1, n
       p = i - 1 + maxloc(abs(a(i:n,i)), 1)
       if (abs(a(p,i)) < 1.0e-300_hdp) then
          ok = .false.; return
       end if
       if (p /= i) then
          row = a(i,:); a(i,:) = a(p,:); a(p,:) = row
          t = b(i); b(i) = b(p); b(p) = t
       end if
       do j = i+1, n
          fac = a(j,i)/a(i,i)
          a(j,i:n) = a(j,i:n) - fac*a(i,i:n)
          b(j) = b(j) - fac*b(i)
       end do
    end do
    do i = n, 1, -1
       b(i) = (b(i) - sum(a(i,i+1:n)*b(i+1:n)))/a(i,i)
    end do
  end subroutine solve_lin

  !-----------------------------------------------------------------------------
  ! Newton solve for the Legendre coefficients.  `guess` (coordinate radius) is
  ! only used if no previous solution is stored in hh (hh%found == .false.).
  ! ierr: 0 ok, 1 not converged, 2 surface left the grid / bad metric, 3 singular.
  subroutine find_horizon(hh, guess, ierr)
    type(horizon_t), intent(inout) :: hh
    real(hdp),       intent(in)    :: guess
    integer,         intent(out)   :: ierr
    ! Ladder of starting radii, as multiples of the caller's guess.  A single
    ! Newton solve converges to whichever root happens to be nearest its
    ! starting point, so on evolved (noisy, non-Kerr) data the returned
    ! horizon could jump with the guess -- which silently corrupts the
    ! drift guard, whose entire job is to track ONE surface in time.  We
    ! therefore solve from several starts and keep the OUTERMOST root that
    ! actually satisfies the marginal condition.
        ! Seed list: bracketed sign changes first, then a DENSE geometric ladder
    ! about the caller's guess.
    !
    ! The scan alone is not sufficient.  It probes a SPHERICAL trial family,
    ! and once the horizon has become strongly non-spherical in evolved data
    ! that family can miss the crossing entirely (the scan then returns zero
    ! seeds).  The dense ladder covers the basins directly: with only a handful
    ! of widely spaced factors some calls landed exclusively in the basin of a
    ! spurious non-marginal root, and the answer then depended on the guess.
    integer, parameter :: NCAND = 24
    integer, parameter :: NLAD = 12
    real(hdp), parameter :: ladder_lo = 0.45_hdp, ladder_hi = 2.20_hdp
    real(hdp) :: cand(NCAND), seeds(NCAND), best_r, g, bc(HZ_MAXL), bl(HZ_MAXL)
    integer :: s, nscan, nseed, ntry, ierr1, biter
    real(hdp) :: cprev(HZ_MAXL)
    logical :: had_prev
    real(hdp) :: bresid

    ierr = 0
    best_r = -huge(1.0_hdp)
    had_prev = hh%found
    cprev = hh%coef
    nseed = 0

    ! Candidate 0: continue from the previous fit, if there was one.  This is
    ! the single most reliable start and is what the finder relied on before
    ! the multi-start logic was added.
    if (had_prev) then
       hh%coef = cprev
       call newton_from(hh, 0.0_hdp, .false., ierr1)
       if (ierr1 == 0 .and. marginal(hh)) then
          best_r = hh%hmin
          bc = hh%coef; bl = hh%lval; biter = hh%niter; bresid = hh%resid
       end if
    end if

    ! Bracketed roots: scan a family of spherical trial surfaces and collect
    ! every radius where Theta crosses from negative to positive.  These are
    ! genuine brackets, so Newton starts from them land on the intended root
    ! rather than on whichever one happens to be near the guess.
    call scan_seeds(hh, guess, seeds, nscan)
    ntry = nscan
    do s = 1, min(size(seeds), NCAND)
       cand(s) = seeds(s)
    end do
    do s = 1, NLAD
       if (ntry >= NCAND) exit
       ntry = ntry + 1
       cand(ntry) = guess*ladder_lo*(ladder_hi/ladder_lo)** &
                    (real(s-1, hdp)/real(NLAD-1, hdp))
    end do

    do s = 1, ntry
       g = cand(s)
       call newton_from(hh, g, .true., ierr1)
       if (ierr1 /= 0) cycle
       if (.not. marginal(hh)) cycle
       if (hh%hmin > best_r) then
          best_r = hh%hmin
          bc = hh%coef
          bl = hh%lval
          biter = hh%niter
          bresid = hh%resid
       end if
    end do

    if (best_r <= -huge(1.0_hdp)) then
       ! No candidate gave a valid marginal surface.  Fall back to the old
       ! behaviour -- continue from the previous fit, or start from the
       ! caller's guess -- so the failure mode and the reported diagnostics
       ! are unchanged from before the multi-start logic.
       if (had_prev) then
          hh%coef = cprev
          call newton_from(hh, 0.0_hdp, .false., ierr)
       else
          call newton_from(hh, guess, .true., ierr)
       end if
       return
    end if

    hh%coef = bc
    hh%lval = bl
    hh%niter = biter
    hh%resid = bresid
    hh%found = .true.
    call horizon_diagnostics(hh)
    if (.not. hh%found) ierr = 2
  end subroutine find_horizon

  !---------------------------------------------------------------------------
  ! Collect bracketed starting radii for Theta = 0.
  !
  ! Evaluate Theta on a family of spherical trial surfaces h(mu) = R and scan R
  ! outward on a logarithmic grid.  Every negative -> positive crossing of
  ! Theta is a genuine sign change of the residual, hence a genuine bracket, so
  ! a Newton iteration seeded there converges to the root the bracket belongs
  ! to.  This is what makes the returned horizon independent of the caller's
  ! starting guess.
  !
  ! Points that the grid cannot supply (inside the excised region, off the
  ! grid) report ok = .false. and are simply skipped, so the scan needs no
  ! knowledge of the excision radius.
  !---------------------------------------------------------------------------
  subroutine scan_seeds(hh, guess, seeds, nseed)
    type(horizon_t), intent(inout) :: hh
    real(hdp),       intent(in)    :: guess
    real(hdp),       intent(out)   :: seeds(:)
    integer,         intent(out)   :: nseed
    integer, parameter :: NSCAN = 300
    real(hdp) :: rlo, rhi, r, th, th_prev
    logical  :: ok, ok_prev
    integer  :: is

    nseed = 0
    rlo = 0.30_hdp*guess
    rhi = 4.00_hdp*guess
    ok_prev = .false.
    th_prev = 0.0_hdp

    do is = 0, NSCAN
       r = rlo*(rhi/rlo)**(real(is, hdp)/real(NSCAN, hdp))
       th = theta_of_sphere(hh, r, ok)
       ! Record EVERY sign change, in either direction.  For a spherically
       ! symmetric trial family Theta typically changes sign twice near the
       ! horizon; keeping only the -/+ crossing throws away the bracket that
       ! actually seeds the marginal root, and the + /- one can converge to a
       ! spurious surface.  All brackets are kept and the marginal test plus
       ! the largest-hmin rule below pick the right one.
       if (ok .and. ok_prev .and. (th_prev < 0.0_hdp) .neqv. (th < 0.0_hdp)) then
          if (nseed < size(seeds)) then
             nseed = nseed + 1
             seeds(nseed) = r
          end if
       end if
       if (ok) then
          th_prev = th
          ok_prev = .true.
       end if
    end do
  end subroutine scan_seeds

  ! Theta evaluated on the spherical trial surface h(mu) = R.
  function theta_of_sphere(hh, R, ok) result(th)
    type(horizon_t), intent(inout) :: hh
    real(hdp),       intent(in)    :: R
    logical,         intent(out)   :: ok
    real(hdp) :: th
    real(hdp) :: save(HZ_MAXL)
    save = hh%coef
    hh%coef = 0.0_hdp
    hh%coef(1) = R
    th = expansion(hh, 0.5_hdp, ok)
    hh%coef = save
  end function theta_of_sphere

  ! Is the current fit a genuine MARGINALLY trapped surface?
  ! Scale the WHOLE fitted surface (every Legendre coefficient) inward and
  ! outward and require Theta to be negative just inside and positive just
  ! outside.  A Newton fixed point failing this is a spurious root -- e.g. a
  ! tangent point of Theta -- and must not be reported as a horizon.
  !
  ! NOTE: scaling only the a_0 intercept (rather than every coefficient) does
  ! NOT work: it reshapes the surface instead of inflating it, and then the
  ! sign test rejects genuine horizons too.
  logical function marginal(hh)
    type(horizon_t), intent(in)    :: hh
    real(hdp), parameter :: eps = 2.0e-2_hdp
    type(horizon_t) :: s
    real(hdp) :: th_in, th_out
    logical :: ok

    marginal = .false.
    s = hh
    ok = .true.
    s%coef = (1.0_hdp - eps)*hh%coef
    th_in = expansion(s, 0.5_hdp, ok)
    if (.not. ok) return
    s%coef = (1.0_hdp + eps)*hh%coef
    th_out = expansion(s, 0.5_hdp, ok)
    if (.not. ok) return

    marginal = (th_in < 0.0_hdp .and. th_out > 0.0_hdp)
  end function marginal

  ! Newton solve of the Theta = 0 collocation system from a single start.
  !
  ! reset = .true.  -> discard any existing fit and start from a surface of
  !                   constant radius `guess` (used for the scan/ladder seeds).
  ! reset = .false. -> keep the existing fit and simply continue from it.
  !                   This continuation is what makes the finder reliable on
  !                   evolved data: the previous horizon is almost always in
  !                   the right basin, whereas the grid may not supply any
  !                   point inside the excised region, and a scan of a
  !                   SPHERICAL trial family can miss a strongly non-spherical
  !                   horizon entirely.
  subroutine newton_from(hh, guess, reset, ierr)
    type(horizon_t), intent(inout) :: hh
    real(hdp),       intent(in)    :: guess
    logical,         intent(in)    :: reset
    integer,         intent(out)   :: ierr
    integer,   parameter :: maxit = 60
    real(hdp), parameter :: tol_rel = 1.0e-10_hdp
    real(hdp), allocatable :: mu(:), f(:), fp(:), fm(:), ft(:), jac(:,:), da(:), c0(:)
    real(hdp) :: scale, delta, fn, fnt, lam, a_save, dmax
    integer :: n, it, l, nb
    logical :: ok, okj, conv, accepted

    ierr = 0
    n = hh%nl
    allocate(mu(n), f(n), fp(n), fm(n), ft(n), jac(n,n), da(n), c0(n))
    call colloc_nodes(hh, mu)
    if (reset) then
       hh%coef = 0.0_hdp
       hh%coef(1) = guess
    end if
    hh%found = .false.
    call clear_results(hh)
    conv = .false.

    do it = 1, maxit
       ok = .true.
       call residual(hh, mu, f, ok)
       if (.not. ok) then
          ierr = 2; return
       end if
       fn = maxval(abs(f))
       scale = max(abs(hh%coef(1)), 1.0e-3_hdp)
       delta = 1.0e-3_hdp*scale
       okj = .true.
       do l = 1, n
          a_save = hh%coef(l)
          hh%coef(l) = a_save + delta;  call residual(hh, mu, fp, okj)
          hh%coef(l) = a_save - delta;  call residual(hh, mu, fm, okj)
          hh%coef(l) = a_save
          jac(:,l) = (fp - fm)/(2*delta)
       end do
       if (.not. okj) then
          ierr = 2; return
       end if
       da = -f
       call solve_lin(n, jac, da, ok)
       if (.not. ok) then
          ierr = 3; return
       end if
       c0 = hh%coef(1:n)
       lam = 1.0_hdp
       accepted = .false.
       do nb = 1, 8
          hh%coef(1:n) = c0 + lam*da
          ok = .true.
          call residual(hh, mu, ft, ok)
          fnt = huge(1.0_hdp)
          if (ok) fnt = maxval(abs(ft))
          if (ok .and. (fnt < fn .or. fn < 1.0e-12_hdp)) then
             accepted = .true.; exit
          end if
          lam = 0.5_hdp*lam
       end do
       if (.not. accepted) then         ! stagnated at noise level: take the full step
          hh%coef(1:n) = c0 + da
          lam = 1.0_hdp
       end if
       dmax = lam*maxval(abs(da))
       hh%niter = it
       if (dmax < tol_rel*scale) then
          conv = .true.; exit
       end if
    end do

    ok = .true.
    call residual(hh, mu, f, ok)
    hh%resid = maxval(abs(f))
    if (.not. ok) then
       ierr = 2; return
    end if
    if (.not. conv) then
       ierr = 1; return
    end if
    hh%found = .true.
    call horizon_diagnostics(hh)
    if (.not. hh%found) ierr = 2
  end subroutine newton_from

  !-----------------------------------------------------------------------------
  ! Area, spin, masses, h_min/h_max from the current coefficients.
  subroutine horizon_diagnostics(hh)
    type(horizon_t), intent(inout) :: hh
    real(hdp), allocatable :: xq(:), wq(:)
    real(hdp) :: th, mu, h, hmu, hth, sth, cth, rho, z, rth, zth
    real(hdp) :: g(6), k(6), gi(6), det
    real(hdp) :: sig_tt, sig_tp, sig_pp, dA, fr, fz, nn, sgn_o, s(3)
    real(hdp) :: asum, jsum, wgt
    logical :: ok
    integer :: q

    allocate(xq(n_quad), wq(n_quad))
    call gauleg(n_quad, xq, wq)
    asum = 0.0_hdp; jsum = 0.0_hdp
    hh%hmin = huge(1.0_hdp); hh%hmax = 0.0_hdp
    ok = .true.
    do q = 1, n_quad
       th  = 0.5_hdp*pi*(1.0_hdp + xq(q));  wgt = 0.5_hdp*pi*wq(q)
       sth = sin(th); cth = cos(th); mu = cth
       call surf_h(hh, mu, h, hmu)
       hth = -sth*hmu
       rho = h*sth;  z = hh%zc + h*cth
       rth = hth*sth + h*cth
       zth = hth*cth - h*sth
       hh%hmin = min(hh%hmin, h);  hh%hmax = max(hh%hmax, h)
       call interp_metric(rho, z, g, k, ok)
       if (.not. ok) then
          hh%found = .false.; return
       end if
       call sym_inv(g, gi, det)
       sig_tt = g(1)*rth*rth + 2*g(3)*rth*zth + g(6)*zth*zth
       sig_tp = rho*(g(2)*rth + g(5)*zth)
       sig_pp = rho*rho*g(4)
       dA = sqrt(max(0.0_hdp, sig_tt*sig_pp - sig_tp*sig_tp))
       asum = asum + wgt*dA
       ! outward unit normal: covector ~ (zth, -rth), orient so that it increases r
       fr = zth;  fz = -rth
       sgn_o = sign(1.0_hdp, fr*rho + fz*(z - hh%zc))
       fr = sgn_o*fr;  fz = sgn_o*fz
       nn = sqrt(fr*(gi(1)*fr + gi(3)*fz) + fz*(gi(3)*fr + gi(6)*fz))
       s(1) = (gi(1)*fr + gi(3)*fz)/nn
       s(2) = (gi(2)*fr + gi(5)*fz)/nn
       s(3) = (gi(3)*fr + gi(6)*fz)/nn
       ! K_ij phi^i s^j  with phi^i = (0, rho, 0) on the y=0 plane
       jsum = jsum + wgt*dA*rho*(k(2)*s(1) + k(4)*s(2) + k(5)*s(3))
    end do
    hh%area   = 2*pi*asum
    hh%m_irr  = sqrt(hh%area/(16*pi))
    hh%j_spin = ksign*(2*pi*jsum)/(8*pi)
    hh%m_h    = sqrt(hh%m_irr**2 + hh%j_spin**2/(4*hh%m_irr**2))
  end subroutine horizon_diagnostics

  !-----------------------------------------------------------------------------
  ! Excision safety: is the horizon's closest point to the centre inside
  ! margin*r_excise ?  (.true. = PROBLEM: excision too close/large)
  function horizon_inside_excision(hh, r_excise, margin) result(bad)
    type(horizon_t), intent(in) :: hh
    real(hdp),       intent(in) :: r_excise
    real(hdp),       intent(in), optional :: margin
    logical :: bad
    real(hdp) :: m
    m = 1.2_hdp
    if (present(margin)) m = margin
    bad = (.not. hh%found) .or. (hh%hmin < m*r_excise)
  end function horizon_inside_excision

  !-----------------------------------------------------------------------------
  subroutine horizon_log_header(unit)
    integer, intent(in) :: unit
    write(unit,'(a)') '# step  time  found  niter  resid  area  m_irr  j_spin  m_H  hmin  hmax'
  end subroutine horizon_log_header

  subroutine horizon_log_line(unit, step, t, hh)
    integer,         intent(in) :: unit, step
    real(hdp),       intent(in) :: t
    type(horizon_t), intent(in) :: hh
    write(unit,'(i8,1x,es14.6,1x,l2,1x,i4,8(1x,es14.6))') step, t, hh%found, hh%niter, &
         hh%resid, hh%area, hh%m_irr, hh%j_spin, hh%m_h, hh%hmin, hh%hmax
  end subroutine horizon_log_line

end module horizon_mod
