!===============================================================================
! diagnose_momentum_exact -- is the momentum-constraint plateau a DISCRETISATION
! error or an ALGEBRAIC inconsistency?
!
! Established already (see the Makefile note and helper/diagnose_momentum_map):
!   * rms|M| is dx-INDEPENDENT: over dx = 0.0311 -> 0.0039 (8x) it goes
!     5.90e-4 -> 6.41e-4, i.e. order -0.06.  A truncation error cannot do this.
!   * every one of the seven terms is individually dx-independent.
!   * the Hamiltonian constraint converges at order 2 on the same data with the
!     same stencils, so the data is smooth and the stencils are sound.
! Therefore the residual is dx-independent BY CONSTRUCTION somewhere, and the
! only way to tell algebraic from numerical is to remove the discretisation.
!
! METHOD.  Evaluate the same algebraic expression with EXACT derivatives:
!   bssn_point() rebuilds (phi, gamma-tilde, A-tilde, K) from the Kerr-Schild
!   closed form at ARBITRARY coordinates, and derivatives are taken with a fixed
!   step h = 1e-5 that does not depend on dx at all.  Truncation ~ h^2/6 ~ 1e-11,
!   roundoff ~ eps/(2h) ~ 1e-12, so this is exact to ~1e-11.
!
!   If |M| is then ~1e-11  -> the expression is correct and the plateau is a
!                            numerical problem after all (contradicting the
!                            dx-scaling, which would then need re-examining).
!   If |M| is then ~5e-4   -> the expression does not vanish on Kerr-Schild
!                            data, i.e. a genuine algebraic/convention error,
!                            of the same family as the det(gamma-tilde)=1 issue
!                            recorded in baseline/MOMENTUM_FINDINGS.md.
!
! CONTROL.  The expression is replicated here, and a transcription error would
! invalidate everything.  So it is FIRST run with the code's own stencil
! derivatives and required to reproduce momentum_constraint() to ~1e-12.  Only
! then is it run with exact derivatives.
!
! Build-only diagnostic.  Not a gate.
!===============================================================================
program diagnose_momentum_exact
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state, IPHI, IK, &
                       IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                       IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections, &
                               kerr_schild_adm
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry, excision_status
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry
  use constraints_mod, only: momentum_constraint

  implicit none
  real(dp), allocatable :: u(:,:,:), M(:,:,:), Mt(:,:,:,:)
  real(dp), parameter :: HREF = 1.0e-5_dp
  real(dp) :: xat, zat, r, e_ctrl, e_exact, e_adm, ms, me, ma
  real(dp) :: M_ctrl(3), M_exact(3), M_adm(3)
  real(dp) :: P_ctrl(3,7), P_nodab(3,7)
  real(dp) :: P_ex(3,7), divS(3), gamS(3), divS_c(3), gamS_c(3)
  integer :: jjj
  real(dp) :: S_c(3,3), S_a(3,3), G_c(3,3,3), G_a(3,3,3)
  real(dp) :: gdiff
  real(dp) :: gt0_dbg(3,3), gtstored_dbg(3,3), Abup_dbg(3), dAbup_dbg(3), e4m_dbg
  integer :: ii
  real(dp) :: S_d1x_IA(6), dA_dbg(6,3)
  integer :: i, k, n, nfail
    integer, parameter :: IA(6) = [1,1,1,2,2,3]
    integer, parameter :: IB(6) = [1,2,3,2,3,3]
    integer, parameter :: IV(6) = [IGXX,IGXY,IGXZ,IGYY,IGYZ,IGZZ]
    integer, parameter :: AV(6) = [IAXX,IAXY,IAXZ,IAYY,IAYZ,IAZZ]

  call init_params('parameter_files/vacuum_kerr.par')
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(M(Nx,Nz,3))
  allocate(Mt(Nx,Nz,3,7))

  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call excision_reset_geometry()
  call extrapolate_excised(u)
  call momentum_constraint(u, M, Mt)

  nfail = 0; e_ctrl = 0.0_dp; e_exact = 0.0_dp
  write(*,'(A)') '============================================================'
  write(*,'(A,F9.2,A)') ' exact-derivative step h = ', HREF, '  (dx-independent)'
  write(*,'(A)') '  r       |M| stencil    |M| coded     |M| ADM      control'
  write(*,'(A)') '                 (constraint)  (exact deriv)  (exact deriv)  err'
  write(*,'(A)') '------------------------------------------------------------'

  n = 0
  do k = 20, Nz-20, 37
    do i = 12, Nx-12, 23
      xat = xg(i); zat = zg(k); r = sqrt(xat*xat + zat*zat)
      if (r < 3.0_dp .or. r > 5.0_dp) cycle
      n = n + 1
      ! ---- control: replicate the expression on the CODE's own derivatives
      call via_stencil(u, i, k, M_ctrl, P_ctrl)
      ! ---- exact: same expression, derivatives from the closed form
      call via_exact(xat, 0.0_dp, zat, M_exact, P_ex, S_c, G_c)
      call via_exact_adm(xat, 0.0_dp, zat, M_adm, divS, gamS, S_a)
      if (n <= 1) then
        write(*,'(A,3ES15.6)') '   M_ctrl (mine)   =', M_ctrl(1), M_ctrl(2), M_ctrl(3)
        write(*,'(A,3ES15.6)') '   M (constraint) =', M(i,k,1), M(i,k,2), M(i,k,3)
        write(*,'(A)') ' piece-by-piece, free index i=1 (mine vs constraint):'
        do ii = 1, 7
          write(*,'(A,I2,2ES14.6)') '   piece ', ii, P_ctrl(1,ii), Mt(i,k,1,ii)
        end do
        write(*,'(A)') ' piece 1 with dAtilde derivatives set to ZERO:'
        do ii = 1, 1
          write(*,'(A,I2,2ES14.6)') '   piece ', ii, P_nodab(1,ii), Mt(i,k,1,ii)
        end do
        write(*,'(A,3ES14.6)') '   gt0 vs stored metric, max diff = ', &
          maxval(abs(gt0_dbg - gtstored_dbg))
      end if
      ms = sqrt(sum(M(i,k,:)**2))
      me = sqrt(sum(M_exact**2))
      ma = sqrt(sum(M_adm**2))
      e_ctrl = max(e_ctrl, sqrt(sum((M_ctrl - M(i,k,:))**2)))
      e_exact = max(e_exact, me)
      e_adm = max(e_adm, ma)
      do jjj = 1, 3
        divS_c(jjj) = sum(P_ex(jjj,1:5))
        gamS_c(jjj) = P_ex(jjj,6) + P_ex(jjj,7)
      end do
      write(*,'(A,F6.2,3ES13.5,ES12.4)') ' ', r, ms, me, ma, &
           sqrt(sum((M_ctrl - M(i,k,:))**2))
      if (n >= 12) exit
    end do
    if (n >= 12) exit
  end do

  if (n >= 1) then
    write(*,'(A)') '------------------------------------------------------------'
    write(*,'(A)') ' coded form vs ADM form, split the same way, exact derivs:'
    write(*,'(A,3ES14.6)') '   d_jS  coded(1..5) =', divS_c
    write(*,'(A,3ES14.6)') '   d_jS  ADM         =', divS
    write(*,'(A,3ES14.6)') '   diff             =', divS_c-divS
    write(*,'(A,3ES14.6)') '   GamS coded(6+7)  =', gamS_c
    write(*,'(A,3ES14.6)') '   GamS ADM         =', gamS
    write(*,'(A,3ES14.6)') '   diff             =', gamS_c-gamS
    write(*,'(A)') ' S^{ij}  coded  e^{-4phi}(Atilde^{ij} - (2/3) gtilde^{ij} K):'
    write(*,'(A,6ES13.5)') '   xx xy xz yy yz zz =', S_c(1,1), S_c(1,2), S_c(1,3), &
         S_c(2,2), S_c(2,3), S_c(3,3)
    write(*,'(A)') ' S^{ij}  ADM    K^{ij} - gamma^{ij} K          :'
    write(*,'(A,6ES13.5)') '   xx xy xz yy yz zz =', S_a(1,1), S_a(1,2), S_a(1,3), &
         S_a(2,2), S_a(2,3), S_a(3,3)
    gdiff = maxval(abs(G_c - G_a))
    write(*,'(A,ES13.5)') ' max|Gam_coded - Gam_ADM| over all 27 =', gdiff
    write(*,'(A)') ' coded Gp(1,1,1),(1,2,1),(1,3,1) =', G_c(1,1,1), G_c(1,2,1), G_c(1,3,1)
    write(*,'(A,3ES13.5)') ' ADM   Gam same              =', G_a(1,1,1), G_a(1,2,1), G_a(1,3,1)
    write(*,'(A)') ' diff (coded - ADM):'
    write(*,'(A,6ES13.5)') '   xx xy xz yy yz zz =', S_c(1,1)-S_a(1,1), &
         S_c(1,2)-S_a(1,2), S_c(1,3)-S_a(1,3), S_c(2,2)-S_a(2,2), &
         S_c(2,3)-S_a(2,3), S_c(3,3)-S_a(3,3)
  end if
  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A,ES12.4)') ' max control error (replication vs constraint) = ', e_ctrl
  write(*,'(A,ES12.4)') ' max |M| coded form, EXACT derivatives       = ', e_exact
  write(*,'(A,ES12.4)') ' max |M| ADM form,   EXACT derivatives       = ', e_adm
  write(*,'(A)') '============================================================'

  if (e_ctrl > 1.0e-10_dp) then
    write(*,'(A,ES12.4)') ' FAIL: the replicated expression does not reproduce'
    write(*,'(A,ES12.4)') '       momentum_constraint (error ', e_ctrl, ') --'
    write(*,'(A)') '       transcription error, the exact-derivative result'
    write(*,'(A)') '       below is meaningless.  Fix the copy first.'
    stop 2
  end if
  write(*,'(A)') 'PASS: replication validated against momentum_constraint.'

contains

  ! ---------------------------------------------------------------------------
  ! Rebuild the BSSN state at an ARBITRARY point from the Kerr-Schild closed
  ! form.  Mirrors set_kerr_schild_id exactly, including conformal_factor_mode
  ! 0 (phi = ln det g / 12, hence det gamma-tilde = 1).
  ! ---------------------------------------------------------------------------
  subroutine bssn_point(x, y, z, phi, gt, Aij, Ktr)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: phi, gt(3,3), Aij(3,3), Ktr
    real(dp) :: gij(3,3), gijinv(3,3), alpha, beta(3), Kij(3,3), detg, e4m
    call kerr_schild_adm(x, y, z, gij, gijinv, alpha, beta, Kij)
    detg = det3(gij)
    phi = log(max(detg, 1.0e-300_dp))/12.0_dp
    e4m = exp(-4.0_dp*phi)
    gt = e4m*gij
    Ktr = sum(gijinv*Kij)
    Aij = e4m*(Kij - gij*Ktr/3.0_dp)
  end subroutine bssn_point

  ! Exact derivatives: fixed step HREF, independent of the grid.
  subroutine via_exact(x, y, z, Mout, Pout, Sc, Gc)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: Mout(3), Pout(3,7), Sc(3,3), Gc(3,3,3)
    real(dp) :: phi0, Ktr0, php, phm, Ktp, Ktm, gt0(3,3), A0(3,3)
    real(dp) :: gtp(3,3), gtm(3,3), Ap(3,3), Am(3,3)
    real(dp) :: dphi(3), dKtr(3), dgt(3,3,3), dA(3,3,3)
    real(dp) :: xp, xm, yp, ym, zp, zm
    integer :: a, b, dir, a_, b_, c_, d_
    real(dp) :: gi_(3,3), det_, q_
    call bssn_point(x, y, z, phi0, gt0, A0, Ktr0)
    do dir = 1, 3
      xp = x; xm = x; yp = y; ym = y; zp = z; zm = z
      select case (dir)
      case (1); xp = x + HREF;  xm = x - HREF
      case (2); yp = y + HREF;  ym = y - HREF
      case (3); zp = z + HREF;  zm = z - HREF
      end select
      call bssn_point(xp, yp, zp, php, gtp, Ap, Ktp)
      call bssn_point(xm, ym, zm, phm, gtm, Am, Ktm)
      dphi(dir) = (php - phm)/(2.0_dp*HREF)
      dKtr(dir) = (Ktp - Ktm)/(2.0_dp*HREF)
      do a = 1, 3
        do b = 1, 3
          dgt(dir,a,b) = (gtp(a,b) - gtm(a,b))/(2.0_dp*HREF)
          dA(dir,a,b)  = (Ap(a,b)  - Am(a,b)) /(2.0_dp*HREF)
        end do
      end do
    end do
    call mom_expr(phi0, gt0, A0, Ktr0, dphi, dgt, dA, dKtr, Mout, Pout, Gc)
    ! the coded definition:  S^ij = e^{-4phi} (Atilde^{ij} - (2/3) gtilde^{ij} K)
    call sym3inv(gt0, gi_, det_)
    do a_ = 1, 3
      do b_ = 1, 3
        q_ = 0.0_dp
        do c_ = 1, 3
          do d_ = 1, 3
            q_ = q_ + gi_(a_,c_)*gi_(b_,d_)*A0(c_,d_)
          end do
        end do
        Sc(a_,b_) = exp(-4.0_dp*phi0)*(q_ - (2.0_dp/3.0_dp)*gi_(a_,b_)*Ktr0)
      end do
    end do
  end subroutine via_exact


  ! Same expression, but fed the code's own stencil derivatives.
  subroutine via_stencil(u, i, k, Mout, Pout)
    real(dp), intent(in)  :: u(:,:,:)
    integer, intent(in)   :: i, k
    real(dp), intent(out) :: Mout(3), Pout(3,7)
    type(stencil_t) :: S
    type(conformal_geometry_t) :: G
    real(dp) :: phi0, Ktr0, gt0(3,3), A0(3,3), dphi(3), dKtr(3)
    real(dp) :: dgt(3,3,3), dA(3,3,3), dA_save(3,3,3)
    real(dp) :: Mout_nod(3)
    real(dp) :: t1(NVARS), t2(NVARS), t3(NVARS)
    call compute_stencil(u, i, k, excision_status, S)
    call compute_conformal_geometry(S, G)
    gt0 = G%gt
    phi0 = S%val(IPHI);  Ktr0 = S%val(IK)
    call unpack6(S%val(IAXX), S%val(IAXY), S%val(IAXZ), &
                 S%val(IAYY), S%val(IAYZ), S%val(IAZZ), A0)
    t1 = S%d1x; t2 = S%d1y; t3 = S%d1z
    dphi(1) = t1(IPHI); dphi(2) = t2(IPHI); dphi(3) = t3(IPHI)
    dKtr(1) = t1(IK);   dKtr(2) = t2(IK);   dKtr(3) = t3(IK)
    call fill_ders(t1, t2, t3, dgt, dA)
    gt0_dbg = gt0
    gtstored_dbg = 0.0_dp
    gtstored_dbg(1,1)=S%val(IGXX); gtstored_dbg(1,2)=S%val(IGXY)
    gtstored_dbg(1,3)=S%val(IGXZ); gtstored_dbg(2,2)=S%val(IGYY)
    gtstored_dbg(2,3)=S%val(IGYZ); gtstored_dbg(3,3)=S%val(IGZZ)
    gtstored_dbg(2,1)=S%val(IGXY); gtstored_dbg(3,1)=S%val(IGXZ)
    gtstored_dbg(3,2)=S%val(IGYZ)
    call mom_expr(phi0, gt0, A0, Ktr0, dphi, dgt, dA, dKtr, Mout, Pout)
    ! Does piece 1 match the constraint if the Atilde DERIVATIVES are dropped?
    ! pieces 2..7 already match exactly, so if this also matches, dAb is the
    ! single differing input and everything else in the copy is faithful.
    dA_save = dA
    dA = 0.0_dp
    call mom_expr(phi0, gt0, A0, Ktr0, dphi, dgt, dA, dKtr, Mout_nod, P_nodab)
    dA = dA_save
  end subroutine via_stencil

  subroutine fill_ders(t1, t2, t3, dgt, dA)
    real(dp), intent(in)  :: t1(NVARS), t2(NVARS), t3(NVARS)
    real(dp), intent(out) :: dgt(3,3,3), dA(3,3,3)
    integer :: dir
    dgt = 0.0_dp; dA = 0.0_dp
    do dir = 1, 3
      select case (dir)
      case (1); call fill_one(t1, dgt(dir,:,:), dA(dir,:,:))
      case (2); call fill_one(t2, dgt(dir,:,:), dA(dir,:,:))
      case (3); call fill_one(t3, dgt(dir,:,:), dA(dir,:,:))
      end select
    end do
  end subroutine fill_ders

  subroutine fill_one(t, dg, dA_)
    real(dp), intent(in)  :: t(NVARS)
    real(dp), intent(out) :: dg(3,3), dA_(3,3)
    integer :: m, a, b
    do m = 1, 6
      a = IA(m); b = IB(m)
      dg(a,b)  = t(IV(m));  dg(b,a)  = t(IV(m))
      dA_(a,b) = t(AV(m)); dA_(b,a) = t(AV(m))
    end do
  end subroutine fill_one

  ! ---------------------------------------------------------------------------
  !  M^i = d_j S^{ij} + Gamma^i_{jk} S^{kj} + Gamma^j_{jk} S^{ik},
  !  S^{ij} = e^{-4phi} (Atilde^{ij} - (2/3) gamma-tilde^{ij} K).
  !  Transcribed from helper/constraints.F90:momentum_constraint; the control at
  !  the top of the program is what makes this copy trustworthy.
  ! ---------------------------------------------------------------------------
  subroutine mom_expr(phi0, gt, Ab, Ktr, dphi, dgt, dAb, dKtr, Mout, Pout, Gpo, Smo)
    real(dp), intent(in) :: phi0, gt(3,3), Ab(3,3), Ktr
    real(dp), intent(in) :: dphi(3), dgt(3,3,3), dAb(3,3,3), dKtr(3)
    real(dp), intent(out) :: Mout(3)
    real(dp), intent(out), optional :: Pout(3,7), Gpo(3,3,3), Smo(3,3)
    real(dp) :: gtinv(3,3), det, e4, e4m
    real(dp) :: Abup(3,3), dAbup(3,3,3), dgtinv(3,3,3)
    real(dp) :: Qmat(3,3), Smat(3,3), dgamma(3,3,3), Gp(3,3,3)
    real(dp) :: pieces(7), acc, q
    integer :: a, b, c, d, m, jf, j2, dir

    call sym3inv(gt, gtinv, det)
    e4 = exp(4.0_dp*phi0); e4m = 1.0_dp/e4

    Abup = 0.0_dp
    do a = 1, 3
      do b = 1, 3
        q = 0.0_dp
        do c = 1, 3
          do d = 1, 3
            q = q + gtinv(a,c)*gtinv(b,d)*Ab(c,d)
          end do
        end do
        Abup(a,b) = q
      end do
    end do

    do dir = 1, 3
      do a = 1, 3
        do b = 1, 3
          q = 0.0_dp
          do c = 1, 3
            do d = 1, 3
              q = q + gtinv(a,c)*gtinv(b,d)*dgt(dir,c,d)
            end do
          end do
          dgtinv(dir,a,b) = -q
        end do
      end do
      do a = 1, 3
        do b = 1, 3
          acc = 0.0_dp
          do c = 1, 3
            do d = 1, 3
              acc = acc + dgtinv(dir,a,c)*gtinv(b,d)*Ab(c,d) &
                        + gtinv(a,c)*dgtinv(dir,b,d)*Ab(c,d) &
                        + gtinv(a,c)*gtinv(b,d)*dAb(dir,c,d)
            end do
          end do
          dAbup(dir,a,b) = acc
        end do
      end do
    end do
    do a = 1, 3
      do b = 1, 3
        Qmat(a,b) = Abup(a,b) - (2.0_dp/3.0_dp)*gtinv(a,b)*Ktr
        Smat(a,b) = e4m*Qmat(a,b)
      end do
    end do

    do dir = 1, 3
      do a = 1, 3
        do b = 1, 3
          dgamma(dir,a,b) = e4*( 4.0_dp*dphi(dir)*gt(a,b) + dgt(dir,a,b) )
        end do
      end do
    end do
    do a = 1, 3
      do b = 1, 3
        do c = 1, 3
          acc = 0.0_dp
          do m = 1, 3
            acc = acc + gtinv(a,m)*0.5_dp*( dgamma(b,m,c) &
                                         + dgamma(c,m,b) &
                                         - dgamma(m,b,c) )
          end do
          Gp(a,b,c) = e4m*acc
        end do
      end do
    end do

    do jf = 1, 3
      pieces = 0.0_dp
      do j2 = 1, 3
        pieces(1) = pieces(1) + e4m*dAbup(j2,jf,j2)
        pieces(2) = pieces(2) -(2.0_dp/3.0_dp)*e4m*Ktr*dgtinv(j2,jf,j2)
        pieces(3) = pieces(3) -(2.0_dp/3.0_dp)*e4m*gtinv(j2,jf)*dKtr(j2)
        pieces(4) = pieces(4) -4.0_dp*e4m*dphi(j2)*Abup(j2,jf)
        pieces(5) = pieces(5) +(8.0_dp/3.0_dp)*e4m*dphi(j2)*gtinv(j2,jf)*Ktr
        do m = 1, 3
          pieces(6) = pieces(6) + Gp(jf,j2,m)*Smat(m,j2)
          pieces(7) = pieces(7) + Gp(j2,j2,m)*Smat(jf,m)
        end do
      end do
      Mout(jf) = sum(pieces)
      if (present(Pout)) Pout(jf,:) = pieces
      if (present(Gpo)) Gpo = Gp
      if (present(Smo)) Smo = Smat
    end do
  end subroutine mom_expr

  subroutine unpack6(xx, xy, xz, yy, yz, zz, M6)
    real(dp), intent(in)  :: xx, xy, xz, yy, yz, zz
    real(dp), intent(out) :: M6(3,3)
    M6 = 0.0_dp
    M6(1,1)=xx; M6(1,2)=xy; M6(1,3)=xz
    M6(2,1)=xy; M6(2,2)=yy; M6(2,3)=yz
    M6(3,1)=xz; M6(3,2)=yz; M6(3,3)=zz
  end subroutine unpack6

  function det3(M) result(d)
    real(dp), intent(in) :: M(3,3)
    real(dp) :: d
    d = M(1,1)*(M(2,2)*M(3,3)-M(2,3)*M(3,2)) &
      - M(1,2)*(M(1,2)*M(3,3)-M(1,3)*M(2,3)) &
      + M(1,3)*(M(1,2)*M(2,3)-M(1,3)*M(2,2))
  end function det3

  subroutine sym3inv(M, Minv, det)
    real(dp), intent(in)  :: M(3,3)
    real(dp), intent(out) :: Minv(3,3), det
    real(dp) :: c11, c12, c13, c22, c23, c33
    c11 =  M(2,2)*M(3,3)-M(2,3)*M(3,2)
    c12 = -(M(1,2)*M(3,3)-M(1,3)*M(2,3))
    c13 =  M(1,2)*M(2,3)-M(1,3)*M(2,2)
    c22 =  M(1,1)*M(3,3)-M(1,3)*M(3,1)
    c23 = -(M(1,1)*M(2,3)-M(1,2)*M(1,3))
    c33 =  M(1,1)*M(2,2)-M(1,2)*M(1,2)
    det = M(1,1)*c11 + M(1,2)*c12 + M(1,3)*c13
    Minv = 0.0_dp
    Minv(1,1)=c11; Minv(1,2)=c12; Minv(1,3)=c13
    Minv(2,1)=c12; Minv(2,2)=c22; Minv(2,3)=c23
    Minv(3,1)=c13; Minv(3,2)=c23; Minv(3,3)=c33
    Minv = Minv/det
  end subroutine sym3inv


  ! ---------------------------------------------------------------------------
  !  INDEPENDENT REFERENCE: the textbook ADM form, evaluated directly on the
  !  exact Kerr-Schild data.
  !
  !      M^i = D_j ( K^{ij} - gamma^{ij} K ),   D = Levi-Civita of gamma
  !
  !  This deliberately shares NO algebra with the seven-piece conformal form in
  !  helper/constraints.F90 -- no e^{+-4phi}, no gtilde, no Atilde, no split
  !  into d_j S and Gamma S terms.  If the two agree to ~1e-10 the coded form is
  !  right and any residual is numerical; if they disagree, the coded form has
  !  an algebraic error and the size of the gap is its size.
  ! ---------------------------------------------------------------------------
  subroutine adm_point(x, y, z, g, Kij)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: g(3,3), Kij(3,3)
    real(dp) :: ginv(3,3), alpha, beta(3), det
    call kerr_schild_adm(x, y, z, g, ginv, alpha, beta, Kij)
  end subroutine adm_point

  subroutine via_exact_adm(x, y, z, Mout, DivS, GamS, Sa)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: Mout(3), DivS(3), GamS(3), Sa(3,3)
    real(dp) :: g0(3,3), K0(3,3), gp(3,3), gm(3,3), Kp(3,3), Km(3,3)
    real(dp) :: dg(3,3,3), dK(3,3,3)
    real(dp) :: gi(3,3), dgi(3,3,3), Kup(3,3), dKup(3,3,3), Sm(3,3), dSm(3,3,3)
    real(dp) :: Gam(3,3,3), Ktr, dKtr(3), det, xp, xm, yp, ym, zp, zm, q
    integer :: a, b, c, d, m, dir, jf, j2

    call adm_point(x, y, z, g0, K0)
    do dir = 1, 3
      xp = x; xm = x; yp = y; ym = y; zp = z; zm = z
      select case (dir)
      case (1); xp = x + HREF;  xm = x - HREF
      case (2); yp = y + HREF;  ym = y - HREF
      case (3); zp = z + HREF;  zm = z - HREF
      end select
      call adm_point(xp, yp, zp, gp, Kp)
      call adm_point(xm, ym, zm, gm, Km)
      do a = 1, 3
        do b = 1, 3
          dg(dir,a,b) = (gp(a,b) - gm(a,b))/(2.0_dp*HREF)
          dK(dir,a,b) = (Kp(a,b) - Km(a,b))/(2.0_dp*HREF)
        end do
      end do
    end do

    call sym3inv(g0, gi, det)
    do dir = 1, 3
      do a = 1, 3
        do b = 1, 3
          q = 0.0_dp
          do c = 1, 3
            do d = 1, 3
              q = q - gi(a,c)*dg(dir,c,d)*gi(d,b)
            end do
          end do
          dgi(dir,a,b) = q
        end do
      end do
    end do

    Ktr = 0.0_dp
    do a = 1, 3
      do b = 1, 3
        Ktr = Ktr + gi(a,b)*K0(a,b)
      end do
    end do
    do dir = 1, 3
      q = 0.0_dp
      do a = 1, 3
        do b = 1, 3
          q = q + dgi(dir,a,b)*K0(a,b) + gi(a,b)*dK(dir,a,b)
        end do
      end do
      dKtr(dir) = q
    end do

    do dir = 1, 3
      do a = 1, 3
        do b = 1, 3
          q = 0.0_dp
          do c = 1, 3
            do d = 1, 3
              q = q + dgi(dir,a,c)*gi(b,d)*K0(c,d) &
                    + gi(a,c)*dgi(dir,b,d)*K0(c,d) &
                    + gi(a,c)*gi(b,d)*dK(dir,c,d)
            end do
          end do
          dKup(dir,a,b) = q
        end do
      end do
    end do
    do a = 1, 3
      do b = 1, 3
        q = 0.0_dp
        do c = 1, 3
          do d = 1, 3
            q = q + gi(a,c)*gi(b,d)*K0(c,d)
          end do
        end do
        Kup(a,b) = q
      end do
    end do

    do a = 1, 3
      do b = 1, 3
        Sm(a,b) = Kup(a,b) - gi(a,b)*Ktr
      end do
    end do
    do dir = 1, 3
      do a = 1, 3
        do b = 1, 3
          dSm(dir,a,b) = dKup(dir,a,b) - dgi(dir,a,b)*Ktr - gi(a,b)*dKtr(dir)
        end do
      end do
    end do

    do a = 1, 3
      do b = 1, 3
        do c = 1, 3
          q = 0.0_dp
          do m = 1, 3
            q = q + gi(a,m)*0.5_dp*( dg(b,m,c) + dg(c,m,b) - dg(m,b,c) )
          end do
          Gam(a,b,c) = q
        end do
      end do
    end do

    do jf = 1, 3
      q = 0.0_dp
      do j2 = 1, 3
        q = q + dSm(j2,jf,j2)
      end do
      DivS(jf) = q
      q = 0.0_dp
      do j2 = 1, 3
        do m = 1, 3
          q = q + Gam(jf,j2,m)*Sm(m,j2) + Gam(j2,j2,m)*Sm(jf,m)
        end do
      end do
      GamS(jf) = q
      Mout(jf) = DivS(jf) + GamS(jf)
      Sa(jf,:) = Sm(jf,:)
      G_a = Gam
    end do
  end subroutine via_exact_adm

end program diagnose_momentum_exact
