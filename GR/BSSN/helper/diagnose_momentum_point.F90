!===============================================================================
! diagnose_momentum_point -- localize the axis-adjacent momentum-constraint
! residual reported in LONG_TERM_GAUGE_DRIFT_REPORT.md section 13.7.
!
! PURPOSE
!   An independent symbolic evaluation (see /tmp/independent_ks_momentum.py)
!   gives |M^i| ~ 1e-41 for exact Kerr-Schild data at the near-axis point
!   (x,z) = (0.0628272251309, -1.535248)M, i.e. the continuum vacuum momentum
!   constraint is exactly satisfied there.  The production finite-difference
!   code reports M_z ~ -1.25e-1 at the neighbouring grid point.  The report
!   requires that this discrepancy be localized BEFORE any production change.
!
! WHAT THIS PROGRAM DOES
!   At one chosen grid point it prints, side by side:
!
!     (A) the STORED BSSN state  u(i,k,:)  -- what the evolution actually uses;
!     (B) the ANALYTIC BSSN state built from kerr_schild_adm() at exactly the
!         same (x, 0, z) -- what the stored state *should* be.
!
!   Comparing (A) with (B) isolates the DATA stage: set_kerr_schild_id's
!   conversion of (gamma,K) -> (phi, gtilde, K, Atilde).  If these agree to
!   round-off, the defect is NOT in the initial data, and the remaining
!   suspects are the discrete derivatives (cartoon/stencil) or the constraint
!   assembly itself.
!
!   It also prints the seven additive pieces of the momentum constraint at that
!   point, and at a same-Kerr-radius control point away from the axis, so the
!   axis-localized nature of the residual can be checked directly.
!
!   This is a DIAGNOSTIC.  It changes no production state and asserts no
!   convergence; it only reports numbers.
!
! USAGE
!   diagnose_momentum_point [parfile] [Nx] [Nz] [target_z]
!===============================================================================
program diagnose_momentum_point
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, xmax, r_excise, bh_spin, &
                         bh_mass, slicing_mode, gamma_driver_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod
  use geometry_mod, only: sym3_inverse
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections, &
                              kerr_schild_adm
  use excision_mod, only: set_excision_background, extrapolate_excised, &
                          excision_reset_geometry, excision_status
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint
  use cartoon_mod, only: get_neighbor

  implicit none

  real(dp), allocatable :: u(:,:,:), Mcon(:,:,:), Mterms(:,:,:,:)
  integer :: i, k, ios
  character(len=256) :: par_file, buf
  real(dp) :: target_z, target_x, target_r
  real(dp) :: best_dr, r1
  integer :: ictl, kctl, ic, kc
  integer :: j
  logical :: ex

  par_file = 'parameter_files/vacuum_kerr.par'
  target_x = 0.0628272251309_dp
  target_z = -1.535248_dp

  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) par_file = buf
  buf = ''
  call get_command_argument(2, buf); if (len_trim(buf) > 0) read(buf,*) target_x
  buf = ''
  call get_command_argument(3, buf); if (len_trim(buf) > 0) read(buf,*) target_z

  ! A silently-unreadable parameter file would run on the built-in defaults and
  ! produce plausible-looking but meaningless numbers.  Refuse to continue.
  inquire(file=trim(par_file), exist=ex)
  if (.not. ex) then
     write(*,'(A)') 'ERROR: parameter file "'//trim(par_file)//'" not found.'
     write(*,'(A)') 'Run from the project root, or pass a readable path.'
     error stop 'diagnose_momentum_point: missing parameter file'
  end if

  call init_params(trim(par_file))
  slicing_mode    = 2        ! frozen lapse
  gamma_driver_mode = 2      ! frozen shift  (diagnostic only; no RHS here)
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call excision_reset_geometry()
  call extrapolate_excised(u)
  call init_deriv_stencils()

  allocate(Mcon(Nx,Nz,3), Mterms(Nx,Nz,3,7))
  call momentum_constraint(u, Mcon, Mterms)

  ! ---- locate the grid point nearest the requested coordinate --------------
  i = 1
  do while (i < Nx)
     if (abs(xg(i) - target_x) <= abs(xg(i+1) - target_x)) exit
     i = i + 1
  end do
  k = 1
  do while (k < Nz)
     if (abs(zg(k) - target_z) <= abs(zg(k+1) - target_z)) exit
     k = k + 1
  end do
  target_r = rks_of(xg(i), zg(k))

  write(*,'(A)') '========================================================================'
  write(*,'(A,I0,A,I0,A,F10.6,A,F12.8)') &
       ' Nx=',Nx,' Nz=',Nz,' dx=',dx,' dz=',dz
  write(*,'(A,2(1X,I0),A)') ' target grid index (i,k)=', i, k, &
       '  (requested x=dx, z=-1.535248)'
  write(*,'(A,F12.8,A,F12.8)') ' grid x= ',xg(i),'  z= ',zg(k)
  write(*,'(A,F10.6,A,F12.8)') ' Kerr radius r_KS=',target_r,'   a=',bh_spin
  write(*,'(A,I0)') ' excision mask at this point = ',excision_status(i,k)
  write(*,'(A)') '========================================================================'

  ! ---- (A) stored vs (B) analytic BSSN state --------------------------------
  call compare_state(u, i, k)

  ! ---- momentum constraint and its seven pieces ----------------------------
  write(*,'(A)') ''
  write(*,'(A)') '--- production momentum constraint at this point ---------------'
  write(*,'(A,3(1X,ES14.6))') ' Mx,My,Mz =', (Mcon(i,k,j), j=1,3)
  write(*,'(A)') ' pieces [1]=dAup [2]=-(2/3)K.dgt [3]=-(2/3)gt.dK [4]=-4dphi.A'//&
               ' [5]=(8/3)dphi.gtK [6]=Gam^i S [7]=Gam^j S'
  do j = 1, 3
     write(*,'(A,I0,A,7(1X,ES13.5))') ' M',j,' pieces=', Mterms(i,k,j,:)
     write(*,'(A,I0,A,ES13.5)') ' M',j,'   sum  =', sum(Mterms(i,k,j,:))
  end do

  ! ---- same-radius control away from the axis ------------------------------
  ictl = 1; kctl = 1
  best_dr = huge(1.0_dp)
  do kc = 2, Nz-1
    do ic = 2, Nx-1
       if (xg(ic) < 5.0_dp*max(dx,dz)) cycle
       r1 = rks_of(xg(ic), zg(kc))
       if (abs(r1 - target_r) < best_dr) then
          best_dr = abs(r1 - target_r); ictl = ic; kctl = kc
       end if
    end do
  end do
  write(*,'(A)') ''
  write(*,'(A)') '--- same-Kerr-radius control away from the axis ----------------'
  write(*,'(A,2(1X,I0),A,F10.6,A,F12.8)') &
       ' control grid index (i,k)=', ictl, kctl, '  x=',xg(ictl),'  r_KS=',rks_of(xg(ictl),zg(kctl))
  write(*,'(A,F10.3)') '   |dr| =',best_dr
  write(*,'(A,3(1X,ES14.6))') ' Mx,My,Mz =', (Mcon(ictl,kctl,j), j=1,3)
  do j = 1, 3
     write(*,'(A,I0,A,7(1X,ES13.5))') ' M',j,' pieces=', Mterms(ictl,kctl,j,:)
  end do

  ! ---- independent ADM-form momentum constraint on the SAME state -----------
  write(*,'(A)') ''
  call adm_momentum(u, i, k)
  call adm_momentum(u, ictl, kctl)

  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' If "stored" and "analytic" agree above, the initial-data'
  write(*,'(A)') ' conversion is NOT the defect; the residual then lies in the'
  write(*,'(A)') ' discrete derivatives or the constraint assembly.'
  write(*,'(A)') '========================================================================'

contains

  ! Kerr radius used for the radial band / control matching
  function rks_of(xq, zq) result(rq)
    real(dp), intent(in) :: xq, zq
    real(dp) :: rq, rho2, a2, r2
    a2 = bh_spin*bh_spin
    rho2 = xq*xq + zq*zq
    r2 = 0.5_dp*( (rho2 - a2) + sqrt((rho2 - a2)**2 + 4.0_dp*a2*zq*zq) )
    rq = sqrt(max(0.0_dp, r2))
  end function rks_of

  ! Compare stored BSSN state against analytic Kerr-Schild BSSN state at the
  ! same physical point.  Uses the production conversion formula exactly.
  subroutine compare_state(st, ii, kk)
    real(dp), intent(in) :: st(:,:,:)
    integer, intent(in) :: ii, kk
    real(dp) :: gij(3,3), gijinv(3,3), alpha, beta_up(3), Kij(3,3)
    real(dp) :: phi_an, Ktr_an, detg, twoH
    real(dp) :: gt_an(3,3), A_an(3,3), e4
    real(dp) :: dmax, dphi, dg_t, dK_t, dA_t, dgt_t

    call kerr_schild_adm(xg(ii), 0.0_dp, zg(kk), gij, gijinv, alpha, beta_up, Kij)

    detg = gij(1,1)*(gij(2,2)*gij(3,3)-gij(2,3)*gij(2,3)) &
         - gij(1,2)*(gij(1,2)*gij(3,3)-gij(2,3)*gij(1,3)) &
         + gij(1,3)*(gij(1,2)*gij(2,3)-gij(2,2)*gij(1,3))
    twoH = detg - 1.0_dp
    if (twoH <= 0.0_dp) twoH = 0.0_dp
    phi_an = log(max(detg,1.0e-300_dp))/12.0_dp        ! conformal_factor_mode 0
    gt_an  = exp(-4.0_dp*phi_an)*gij
    Ktr_an = sum(gijinv*Kij)
    e4     = exp(4.0_dp*phi_an)
    A_an   = (Kij - (gij*Ktr_an)/3.0_dp)/e4

    dphi = st(ii,kk,IPHI) - phi_an
    dK_t = st(ii,kk,IK)   - Ktr_an
    dgt_t = max(max(abs(st(ii,kk,IGXX)-gt_an(1,1)), abs(st(ii,kk,IGYY)-gt_an(2,2))), &
                max(abs(st(ii,kk,IGZZ)-gt_an(3,3)), &
                    max(abs(st(ii,kk,IGXY)-gt_an(1,2)), &
                        max(abs(st(ii,kk,IGXZ)-gt_an(1,3)), &
                            abs(st(ii,kk,IGYZ)-gt_an(2,3))))))
    dA_t = max(max(abs(st(ii,kk,IAXX)-A_an(1,1)), abs(st(ii,kk,IAYY)-A_an(2,2))), &
                max(abs(st(ii,kk,IAZZ)-A_an(3,3)), &
                    max(abs(st(ii,kk,IAXY)-A_an(1,2)), &
                        max(abs(st(ii,kk,IAXZ)-A_an(1,3)), &
                            abs(st(ii,kk,IAYZ)-A_an(2,3))))))
    dmax = max(abs(st(ii,kk,IALPHA)-alpha), &
               max(abs(st(ii,kk,IBETAX)-beta_up(1)), &
                   max(abs(st(ii,kk,IBETAZ)-beta_up(3)), abs(dgt_t))))
    dg_t = dgt_t

    write(*,'(A)') ''
    write(*,'(A)') '--- (A) STORED  vs  (B) ANALYTIC BSSN state at this point -----'
    write(*,'(A)') ' field            stored (A)                 analytic (B)        A-B'
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' phi    ', st(ii,kk,IPHI),  phi_an,  dphi
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' K      ', st(ii,kk,IK),    Ktr_an,  dK_t
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' alpha  ', st(ii,kk,IALPHA), alpha,  &
         st(ii,kk,IALPHA)-alpha
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' beta^x ', st(ii,kk,IBETAX), beta_up(1), &
         st(ii,kk,IBETAX)-beta_up(1)
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' beta^z ', st(ii,kk,IBETAZ), beta_up(3), &
         st(ii,kk,IBETAZ)-beta_up(3)
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' gt_xx  ', st(ii,kk,IGXX), gt_an(1,1), &
         st(ii,kk,IGXX)-gt_an(1,1)
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' gt_zz  ', st(ii,kk,IGZZ), gt_an(3,3), &
         st(ii,kk,IGZZ)-gt_an(3,3)
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' A_xx   ', st(ii,kk,IAXX), A_an(1,1), &
         st(ii,kk,IAXX)-A_an(1,1)
    write(*,'(A,2(1X,ES23.15),1X,ES12.4)') ' A_xz   ', st(ii,kk,IAXZ), A_an(1,3), &
         st(ii,kk,IAXZ)-A_an(1,3)
    write(*,'(A)') ' max |A-B| over all gt and A components = '//trim(fmt(dA_t))
    write(*,'(A)') ' max |A-B| over all gt components         = '//trim(fmt(dg_t))
    write(*,'(A)') ' max |A-B| over lapse/shift/gtilde        = '//trim(fmt(dmax))
    write(*,'(A)') ''
    write(*,'(A)') ' NOTE: kerr_schild_adm differentiates the analytic metric with'
    write(*,'(A)') ' its own h=1e-4 stencil, so Kij agrees only to that'
    write(*,'(A)') ' stencil''s truncation error (~1e-8), NOT to machine precision.'
  end subroutine compare_state

  function fmt(v) result(s)
    real(dp), intent(in) :: v
    character(len=24) :: s
    write(s,'(ES23.15)') v
  end function fmt

  ! -------------------------------------------------------------------------
  ! Independent ADM-form momentum constraint on the SAME evolved/sampled state:
  !
  !   M^i = D_j ( K^{ij} - gamma^{ij} K )
  !       = d_j S^{ij} + Gamma^i_{jk} S^{kj} + Gamma^j_{jk} S^{ik}
  !
  ! with gamma_ij = e^{4phi} gtilde_ij, K_ij = e^{4phi}(Atilde_ij + gtilde_ij K/3),
  ! and Gamma the PHYSICAL connection built from finite differences of the
  ! PHYSICAL metric through the same production cartoon stencil.
  !
  ! This is an assembly route completely separate from constraints.F90's
  ! seven-piece BSSN form.  On exact Kerr-Schild data it must vanish (up to
  ! discretisation error) for ANY correct implementation.  Comparing it with
  ! the seven pieces decides whether constraints.F90's algebra is at fault,
  ! independent of any Python re-derivation.
  ! -------------------------------------------------------------------------
  subroutine adm_momentum(st, ii, kk)
    real(dp), intent(in) :: st(:,:,:)
    integer, intent(in) :: ii, kk
    real(dp) :: gphys(3,3), ginv(3,3), detg
    real(dp) :: e4, Ktr, Kij(3,3), S(3,3), Sx(3,3), Sy(3,3), Sz(3,3)
    real(dp) :: Gconn(3,3,3)
    real(dp) :: vp(NVARS), vm(NVARS)
    real(dp) :: mi
    integer :: d, p, q, m, ig

    ! This routine takes centred differences in every direction, so it needs a
    ! full one-cell margin in x and z.  Skip rather than read out of bounds.
    if (ii < 2 .or. ii > Nx-1 .or. kk < 2 .or. kk > Nz-1) then
       write(*,'(A,2(1X,I0))') '   ADM: skipped, no stencil margin at (i,k)=', ii, kk
       return
    end if

    ! physical metric and its inverse at the point
    e4  = exp(4.0_dp*st(ii,kk,IPHI))
    gphys = e4*unpack_gt(st(ii,kk,:))
    call sym3_inverse(gphys, ginv, detg)
    Ktr = st(ii,kk,IK)
    ! physical extrinsic curvature
    do ig = 1, 6
       select case (ig)
       case (1); p = 1; q = 1
       case (2); p = 1; q = 2
       case (3); p = 1; q = 3
       case (4); p = 2; q = 2
       case (5); p = 2; q = 3
       case (6); p = 3; q = 3
       end select
       Kij(p,q) = e4*(st(ii,kk,8+ig) + gphys(p,q)*Ktr/3.0_dp)
       Kij(q,p) = Kij(p,q)
    end do
    ! S^{ij} = K^{ij} - gamma^{ij} K
    do p = 1, 3
       do q = 1, 3
          S(p,q) = sum(ginv(p,:)*matmul(Kij,ginv(q,:))) - ginv(p,q)*Ktr
       end do
    end do

    ! central differences of S^{ij} along each direction (production stencil)
    call fd_phys(st, ii, kk, 1, 0, 0.0_dp, Sx)
    call fd_phys(st, ii, kk, 0, 0, +dy,  Sy)
    call fd_phys(st, ii, kk, 0, 1, 0.0_dp, Sz)
    Sx = Sx/(2.0_dp*dx); Sy = Sy/(2.0_dp*dy); Sz = Sz/(2.0_dp*dz)
    call phys_connection(st, ii, kk, Gconn)

    do p = 1, 3
       mi = Sx(p,1) + Sy(p,2) + Sz(p,3)
       do q = 1, 3
          do m = 1, 3
             mi = mi + Gconn(p,q,m)*S(m,q) + Gconn(q,q,m)*S(p,m)
          end do
       end do
       write(*,'(A,I0,A,ES14.6)') '   ADM M',p,' = ', mi
    end do
  end subroutine adm_momentum

  ! central second-order difference of S^{ij} along one direction, using the
  ! production cartoon neighbour machinery on the sampled state
  subroutine fd_phys(st, ii, kk, dxo, dzo, yval, Sout)
    real(dp), intent(in) :: st(:,:,:)
    integer, intent(in) :: ii, kk, dxo, dzo
    real(dp), intent(in) :: yval
    real(dp), intent(out) :: Sout(3,3)
    real(dp) :: vp(NVARS), vm(NVARS), e4p, e4m, gtp(3,3), gtm(3,3)
    real(dp) :: gp(3,3), gm(3,3), gip(3,3), gim(3,3), dt, dp_
    integer :: p, q, r, c, ig
    vp = get_neighbor(st, ii, kk, +dxo, +dzo, yval)
    vm = get_neighbor(st, ii, kk, -dxo, -dzo, -yval)
    e4p = exp(4.0_dp*vp(IPHI)); e4m = exp(4.0_dp*vm(IPHI))
    gtp = unpack_gt(vp); gtm = unpack_gt(vm)
    gp = e4p*gtp; gm = e4m*gtm
    call sym3_inverse(gp, gip, dt); call sym3_inverse(gm, gim, dp_)
    do p = 1, 3
       do q = 1, 3
          Sout(p,q) = 0.0_dp
       end do
    end do
    ! S = K^{ij} - gamma^{ij} K with K from Atilde+trace, all in terms of the
    ! sampled physical metric; recompute K for each staggered point
    block
      real(dp) :: Kp(3,3), Km(3,3), Sp(3,3), Sm(3,3)
      real(dp) :: Kp_tr, Km_tr
      Kp = 0.0_dp; Km = 0.0_dp
      Kp_tr = vp(IK); Km_tr = vm(IK)
      do ig = 1, 6
         select case (ig)
         case (1); r = 1; c = 1
         case (2); r = 1; c = 2
         case (3); r = 1; c = 3
         case (4); r = 2; c = 2
         case (5); r = 2; c = 3
         case (6); r = 3; c = 3
         end select
         Kp(r,c) = e4p*(vp(8+ig) + gtp(r,c)*Kp_tr/3.0_dp); Kp(c,r) = Kp(r,c)
         Km(r,c) = e4m*(vm(8+ig) + gtm(r,c)*Km_tr/3.0_dp); Km(c,r) = Km(r,c)
      end do
      do p = 1, 3
         do q = 1, 3
            Sp(p,q) = sum(gip(p,:)*matmul(Kp,gip(q,:))) - gip(p,q)*Kp_tr
            Sm(p,q) = sum(gim(p,:)*matmul(Km,gim(q,:))) - gim(p,q)*Km_tr
         end do
      end do
      do p = 1, 3
         do q = 1, 3
            Sout(p,q) = Sp(p,q) - Sm(p,q)
         end do
      end do
    end block
  end subroutine fd_phys

  ! physical connection Gamma^k_{ij} from finite-differenced physical gamma.
  ! dgm(d,p,q,kk) = d_d gamma_pq ; index order matches constraints.F90 usage.
  subroutine phys_connection(st, ii, kk, GamOut)
    real(dp), intent(in) :: st(:,:,:)
    integer, intent(in) :: ii, kk
    real(dp), intent(out) :: GamOut(3,3,3)
    real(dp) :: vp(NVARS), vm(NVARS), ginv(3,3), det
    real(dp) :: dgm(3,3,3), low(3,3,3)
    integer :: d, kk2, pp, qq
    GamOut = 0.0_dp
    do d = 1, 3
       select case (d)
       case (1)
          vp = get_neighbor(st, ii, kk, +1, 0, 0.0_dp)
          vm = get_neighbor(st, ii, kk, -1, 0, 0.0_dp)
          dgm(d,:,:) = (unpack_phys(vp) - unpack_phys(vm))/(2.0_dp*dx)
       case (2)
          vp = get_neighbor(st, ii, kk, 0, 0, +dy)
          vm = get_neighbor(st, ii, kk, 0, 0, -dy)
          dgm(d,:,:) = (unpack_phys(vp) - unpack_phys(vm))/(2.0_dp*dy)
       case (3)
          vp = get_neighbor(st, ii, kk, 0, +1, 0.0_dp)
          vm = get_neighbor(st, ii, kk, 0, -1, 0.0_dp)
          dgm(d,:,:) = (unpack_phys(vp) - unpack_phys(vm))/(2.0_dp*dz)
       end select
    end do
    call sym3_inverse(unpack_phys(st(ii,kk,:)), ginv, det)
    do kk2 = 1, 3
       do pp = 1, 3
          do qq = 1, 3
             low(kk2,pp,qq) = 0.5_dp*(dgm(pp,qq,kk2) + dgm(qq,pp,kk2) - dgm(kk2,pp,qq))
          end do
       end do
    end do
    do kk2 = 1, 3
       do pp = 1, 3
          do qq = 1, 3
             GamOut(kk2,pp,qq) = sum(ginv(kk2,:)*low(:,pp,qq))
          end do
       end do
    end do
  end subroutine phys_connection

  ! unpack gtilde from an NVARS state vector
  function unpack_gt(v) result(M)
    real(dp), intent(in) :: v(NVARS)
    real(dp) :: M(3,3)
    M(1,1) = v(IGXX); M(1,2) = v(IGXY); M(1,3) = v(IGXZ)
    M(2,1) = v(IGXY); M(2,2) = v(IGYY); M(2,3) = v(IGYZ)
    M(3,1) = v(IGXZ); M(3,2) = v(IGYZ); M(3,3) = v(IGZZ)
  end function unpack_gt

  ! physical gamma = e^{4phi} gtilde at a sampled point
  function unpack_phys(v) result(M)
    real(dp), intent(in) :: v(NVARS)
    real(dp) :: M(3,3)
    M = exp(4.0_dp*v(IPHI))*unpack_gt(v)
  end function unpack_phys

end program diagnose_momentum_point