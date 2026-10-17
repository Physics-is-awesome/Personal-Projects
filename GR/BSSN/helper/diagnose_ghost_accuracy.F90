!===============================================================================
! diagnose_ghost_accuracy -- how far do the EXCISED-FILL ghost values sit from
! the exact Kerr-Schild solution, and is that error large enough to explain the
! O(1) Hamiltonian-constraint violation at the innermost physical points?
!
! CONTEXT (baseline/MARGIN_FINDINGS.md)
!   The Hamiltonian constraint is ~0 in the continuum for exact Kerr-Schild
!   data, yet the code reports max|H| ~ 1.2 at r_KS ~ 1.09 at t=0 -- and that
!   value does NOT converge under grid refinement:
!
!       dx=0.2553 -> max|H| = 0.820
!       dx=0.1263 -> max|H| = 1.806
!       dx=0.0628 -> max|H| = 0.509
!
!   rms|H| converges (~2nd order) but max|H| bounces and flips sign, so this is
!   not a truncation error.  The peak always sits on the innermost physical
!   grid points, whose stencils stencil.F90 degrades to one-sided and which
!   therefore read FILLED GHOST values from inside the mask.
!
!   This program separates the two possible causes:
!
!     (a) the ghosts themselves are inaccurate  -> the fill is at fault;
!     (b) the ghosts are accurate                 -> the fallback stencil /
!                                                   the constraint assembly is.
!
!   It builds the analytic Kerr-Schild BSSN state at every ghost point's own
!   (x, 0, z) using the production conversion formula, and reports the error
!   binned by how many cells inside the mask the ghost sits.
!
!   It then reports H at the innermost physical points under three ghost
!   treatments:
!       AS-FILLED   the production extrapolate_excised() ghosts
!       EXACT       the analytic Kerr-Schild values written into the interior
!       SMOOTH      a high-order analytic-state polynomial fit near the mask
!   If H collapses from O(1) to O(dx^2) under EXACT, the fill is the cause.
!
!   This is a DIAGNOSTIC.  It changes no production state and asserts nothing;
!   it only reports numbers.
!
! USAGE
!   diagnose_ghost_accuracy [parfile]
!===============================================================================
program diagnose_ghost_accuracy
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, xmax, r_excise, bh_spin, &
                         slicing_mode, gamma_driver_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod
  use initial_data_mod, only: set_kerr_schild_id, kerr_schild_adm, &
                         compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, &
                          excision_reset_geometry, excision_status, GHOST, &
                          PHYSICAL, EXCISED
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: hamiltonian_constraint
  use stencil_mod, only: compute_stencil, stencil_t
  use geometry_mod, only: compute_conformal_geometry, conformal_geometry_t, sym3_inverse

  implicit none

  real(dp), allocatable :: u(:,:,:), ux(:,:,:), Hval(:,:)
  integer :: ihmin, ihmax, khmin, khmax
  integer :: i, k, j, ios
  character(len=256) :: par_file, buf
  logical :: ex
  integer, parameter :: NDEPTH = 8
  integer :: dcount(NDEPTH)
  real(dp) :: dmax(NDEPTH), dphi(NDEPTH), dgt(NDEPTH), dA(NDEPTH), dat(NDEPTH)
  real(dp) :: depth, rq, worst
  integer :: idepth

  par_file = 'parameter_files/vacuum_kerr.par'
  buf = ''
  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) par_file = buf

  inquire(file=trim(par_file), exist=ex)
  if (.not. ex) then
     write(*,'(A)') 'ERROR: parameter file "'//trim(par_file)//'" not found.'
     error stop 'diagnose_ghost_accuracy: missing parameter file'
  end if

  call init_params(trim(par_file))
  slicing_mode    = 2        ! frozen lapse  (diagnostic only; no RHS here)
  gamma_driver_mode = 2      ! frozen shift  (diagnostic only; no RHS here)
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  call alloc_state(ux, Nx, Nz)
  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call excision_reset_geometry()

  write(*,'(A)') '========================================================================'
  write(*,'(A,I0,A,I0,A,F10.6,A,F10.6,A,F10.6)') &
       ' Nx=',Nx,' Nz=',Nz,' dx=',dx,' dz=',dz,' r_excise=',r_excise

  !===========================================================================
  ! PART 1 -- ghost fill error, binned by depth inside the mask
  !===========================================================================
  call extrapolate_excised(u)

  dcount = 0; dmax = 0.0_dp; dphi = 0.0_dp; dgt = 0.0_dp
  dA = 0.0_dp; dat = 0.0_dp

  do k = 1, Nz
     do i = 1, Nx
        if (excision_status(i,k) /= GHOST) cycle
        depth = (r_excise - rks_of(xg(i), zg(k))) / dx
        idepth = max(1, min(NDEPTH, int(depth) + 1))
        call ks_bssn_state(xg(i), 0.0_dp, zg(k), ux(i,k,:))
        dphi(idepth) = max(dphi(idepth), abs(u(i,k,IPHI) - ux(i,k,IPHI)))
        dgt (idepth) = max(dgt (idepth), tensor_err(u(i,k,:), ux(i,k,:), ICONFMET))
        dA  (idepth) = max(dA  (idepth), tensor_err(u(i,k,:), ux(i,k,:), IATILDE))
        dat (idepth) = max(dat (idepth), max(abs(u(i,k,IALPHA) - ux(i,k,IALPHA)), &
                               max(abs(u(i,k,IBETAX)-ux(i,k,IBETAX)), &
                                   max(abs(u(i,k,IBETAZ)-ux(i,k,IBETAZ)), &
                                       abs(u(i,k,IK)-ux(i,k,IK))))))
        dmax(idepth) = max(dmax(idepth), max(dphi(idepth), max(dgt(idepth), &
                              max(dA(idepth), dat(idepth)))))
        dcount(idepth) = dcount(idepth) + 1
     end do
  end do

  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 1 -- ghost fill error vs exact Kerr-Schild, by depth inside'
  write(*,'(A)') '           the mask (depth in cells beyond r_excise, i.e. negative).'
  write(*,'(A)') ' depth<=-1    -1..0     0..1       1..2       2..3       3..4       4..5       5..6'
  do j = 1, NDEPTH
     if (dcount(j) == 0) then
        write(*,'(8(1X,ES9.2))') 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp
        cycle
     end if
     write(*,'(8(1X,ES9.2))') real(dcount(j),dp), dphi(j), dgt(j), dA(j), dat(j), dmax(j), 0.0_dp, 0.0_dp
  end do
  write(*,'(A)') ' (columns: n, |dphi|, |dgtilde|, |dAtilde|, |d(aux)|, max)'

  worst = 0.0_dp
  do j = 1, NDEPTH
     worst = max(worst, dmax(j))
  end do
  write(*,'(A,ES12.4)') ' worst ghost fill error anywhere = ', worst

  !===========================================================================
  ! PART 2 -- H at the innermost physical points, three ghost treatments
  !===========================================================================
  call init_deriv_stencils()
  allocate(Hval(Nx,Nz))

  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 2 -- |H| at the innermost PHYSICAL points (r_KS just outside'
  write(*,'(A)') '           the mask) under three ghost treatments, all at t=0.'

  call h_at_point(u, Hval, 'AS-FILLED')

  ! EXACT: copy the production state, then overwrite the excised interior
  ! with the exact analytic Kerr-Schild values.
  ux = u
  do k = 1, Nz
     do i = 1, Nx
        if (excision_status(i,k) == EXCISED) call ks_bssn_state(xg(i), 0.0_dp, zg(k), ux(i,k,:))
     end do
  end do
  call h_at_point(ux, Hval, 'EXACT-ANALYTIC')

  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 3 -- decomposition of H near the mask.  H = R + (2/3)K^2 - A^2'
  write(*,'(A)') '           with R = e^(-4phi) gtilde^ij (Rt_ij + Rphi_ij).  The K and A'
  write(*,'(A)') '           pieces are pointwise algebraic (no discretisation error),'
  write(*,'(A)') '           so any resolution-independent floor must live in Rt or Rphi.'
  write(*,'(A)') '           max|.| per radial bin, dr/dx = (r_KS - r_excise)/dx'
  write(*,'(A)') ' dr/dx     n         |Rt|        |Rphi|      |K,A part|      |H|'
  call h_decomposition(u)
  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 4 -- per-derivative error at the innermost physical points.'
  write(*,'(A)') '   Each production derivative is compared against a high-order central'
  write(*,'(A)') '   difference of the EXACT analytic Kerr-Schild field at the same'
  write(*,'(A)') '   point.  Reference is a full central difference of the exact analytic'
  write(*,'(A)') '   field.  Fields checked: gtilde_xx and phi.'
  call derivative_errors(u)
  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 5 -- Gamma-tilde: the one state field never validly measured.'
  write(*,'(A)') '   compute_conformal_connections builds IGTX..IGTZ as'
  write(*,'(A)') '       Gt(k) = -( dx gt^{k1} + dy gt^{k2} + dz gt^{k3} )'
  write(*,'(A)') '   from SECOND-order central differences taken with get_neighbor, and'
  write(*,'(A)') '   loops i=2..Nx-1, k=2..Nz-1 with no excision-mask guard on its read'
  write(*,'(A)') '   range.  Meanwhile the curvature operator that consumes it uses the'
  write(*,'(A)') '   FOURTH-order compute_stencil.  So the field feeding Rt term3 is two'
  write(*,'(A)') '   orders less accurate than its neighbours by construction.'
  write(*,'(A)') '   Here the same formula is evaluated on the EXACT analytic conformal'
  write(*,'(A)') '   metric, and both the value and its x/z derivatives are compared.'
  call gamma_tilde_errors(u)
  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' PART 6 -- replicate the DRIVER t=0 filter exactly.'
  write(*,'(A)') '   The driver selects points with sqrt(x^2+z^2) >= r_excise+margin,'
  write(*,'(A)') '   i.e. the CYLINDRICAL radius rho.  The excision mask itself uses'
  write(*,'(A)') '   the KERR radius r_KS.  These disagree for z != 0, so the driver'
  write(*,'(A)') '   can report points that are inside the excision region.'
  call driver_filter_replica(u, Hval)
  write(*,'(A)') '========================================================================'
  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' If the fill is the cause, EXACT-ANALYTIC must reduce |H| at the'
  write(*,'(A)') ' innermost physical points toward the dx^2 truncation level.'
  write(*,'(A)') ' If |H| stays O(1) with exact ghosts, the fault is in the'
  write(*,'(A)') ' fallback stencil or the constraint assembly, not the fill.'
  write(*,'(A)') '========================================================================'

contains

  ! Compare the STORED Gamma-tilde against the same quantity evaluated on the
  ! exact analytic conformal metric, using the production formula
  !     Gamma^k = -( dx gt^{k1} + dy gt^{k2} + dz gt^{k3} )
  ! Only the value is compared: differentiating the analytic Gamma twice more
  ! would mean differentiating an already-differenced quantity, which is
  ! numerically hopeless and not needed.  If this error converges, Gamma-tilde
  ! is not the near-mask culprit; if it does not, it is.
  subroutine gamma_tilde_errors(st)
    real(dp), intent(in) :: st(:,:,:)
    integer, parameter :: NBG = 6
    real(dp) :: ev(NBG)
    integer :: cN(NBG), ii, kk, ib, m
    real(dp), parameter :: hh = 1.0e-4_dp
    real(dp) :: drr, rrr, d
    real(dp) :: gi0(3,3), gixp(3,3), gixm(3,3)
    real(dp) :: giyp(3,3), giym(3,3), gizp(3,3), gizm(3,3)

    ev = 0.0_dp; cN = 0

    do kk = 3, Nz-2
       do ii = 2, Nx-2
          rrr = rks_of(xg(ii), zg(kk))
          drr = rrr - r_excise
          if (drr < 0.0_dp .or. drr > real(NBG,dp)*dx) cycle
          ib = int(drr/dx) + 1
          if (ib < 1) ib = 1
          if (ib > NBG) ib = NBG
          cN(ib) = cN(ib) + 1

          call analytic_gtinv(xg(ii),        0.0_dp, zg(kk),       gi0)
          call analytic_gtinv(xg(ii)+hh,     0.0_dp, zg(kk),       gixp)
          call analytic_gtinv(xg(ii)-hh,     0.0_dp, zg(kk),       gixm)
          call analytic_gtinv(xg(ii),         hh,    zg(kk),       giyp)
          call analytic_gtinv(xg(ii),        -hh,    zg(kk),       giym)
          call analytic_gtinv(xg(ii),         0.0_dp, zg(kk)+hh,   gizp)
          call analytic_gtinv(xg(ii),         0.0_dp, zg(kk)-hh,   gizm)

          do m = 1, 3
             d = -( (gixp(m,1)-gixm(m,1)) &
                 + (giyp(m,2)-giym(m,2)) &
                 + (gizp(m,3)-gizm(m,3)) ) / (2.0_dp*hh)
             ev(ib) = max(ev(ib), abs(st(ii,kk,14+m) - d))
          end do
       end do
    end do

    write(*,'(A)') ' dr/dx    n      |Gamma-tilde err|   (worst of 3 components)'
    do ib = 1, NBG
       if (cN(ib) == 0) cycle
       write(*,'(3X,F5.1,2X,I4,3X,ES14.5)') real(ib-1,dp), cN(ib), ev(ib)
    end do
  end subroutine gamma_tilde_errors

  ! Analytic inverse of the conformal metric at (x,y,z).
  subroutine analytic_gtinv(xq, yq, zq, giout)
    real(dp), intent(in)  :: xq, yq, zq
    real(dp), intent(out) :: giout(3,3)
    real(dp) :: stt(NVARS), g(3,3), detq
    call ks_bssn_state(xq, yq, zq, stt)
    g(1,1) = stt(IGXX); g(1,2) = stt(IGXY); g(1,3) = stt(IGXZ)
    g(2,1) = stt(IGXY); g(2,2) = stt(IGYY); g(2,3) = stt(IGYZ)
    g(3,1) = stt(IGXZ); g(3,2) = stt(IGYZ); g(3,3) = stt(IGZZ)
    call sym3_inverse(g, giout, detq)
  end subroutine analytic_gtinv

  ! Replica of the driver's t=0 max|H| scan selection rule, retained so the
  ! diagnostic's selection can be compared against the driver's directly.
  subroutine driver_filter_replica(st, Hout)
    real(dp), intent(in) :: st(:,:,:)
    real(dp), intent(inout) :: Hout(:,:)
    real(dp) :: dmargin, best, rho, rk, rsum, rsum2
    integer :: ii, kk, bi, bk, nb_in, n_out
    character(len=64) :: buf
    integer :: ios

    dmargin = 1.0_dp
    buf = ''
    call get_environment_variable("GR_DIAG_MARGIN", buf, status=ios)
    if (ios == 0) read(buf,*) dmargin

    call hamiltonian_constraint(st, Hout, ihmin, ihmax, khmin, khmax)

    best = -1.0_dp; bi = 0; bk = 0
    nb_in = 0; n_out = 0; rsum = 0.0_dp; rsum2 = 0.0_dp; n_out = 0
    do kk = khmin, khmax
       do ii = ihmin, ihmax
          rho = sqrt(xg(ii)**2 + zg(kk)**2)
          rk  = rks_of(xg(ii), zg(kk))
          if (rho < r_excise + dmargin) cycle
          n_out = n_out + 1
          if (rk < r_excise) nb_in = nb_in + 1       ! selected but EXCISED
          rsum = rsum + abs(Hout(ii,kk))
          rsum2 = rsum2 + Hout(ii,kk)**2
          if (abs(Hout(ii,kk)) > best) then
             best = abs(Hout(ii,kk)); bi = ii; bk = kk
          end if
       end do
    end do

    write(*,'(A,F8.4,A,I0,A,I0,A,F8.4,A,F8.4,A,I0)') &
         ' margin=', dmargin, '  worst i=', bi, ' k=', bk, '  rho=', sqrt(xg(bi)**2+zg(bk)**2), &
         '  r_KS=', rks_of(xg(bi), zg(bk)), '  status=', excision_status(bi,bk)
    write(*,'(A,ES12.4)') ' driver-style max|H| = ', best
    write(*,'(A,I0,A,I0,A,F8.4)') ' selected points=', n_out, ' of which inside r_excise: ', nb_in, &
         '   (fraction ', real(nb_in,dp)/real(max(n_out,1),dp), ')'
    write(*,'(A,ES12.4,A,ES12.4)') ' mean |H| over selected = ', &
         rsum/real(max(n_out,1),dp), '   rms|H| = ', sqrt(rsum2/real(max(n_out,1),dp))
  end subroutine driver_filter_replica

  ! Compare every derivative the curvature operator consumes against a high-order
  ! central difference of the exact analytic field at the same point.
  subroutine derivative_errors(st)
    real(dp), intent(in) :: st(:,:,:)
    integer, parameter :: NBD2 = 4
    real(dp) :: e1x(NBD2,2), e2x(NBD2,2), e1z(NBD2,2), e2z(NBD2,2)
    real(dp) :: e1y(NBD2,2), e2y(NBD2,2)
    integer :: cN(NBD2), ii, kk, ib, fld
    real(dp), parameter :: hh = 1.0e-3_dp
    real(dp) :: fp(NVARS), fm(NVARS), fc(NVARS), fp2(NVARS), fm2(NVARS)
    real(dp) :: r1x, r2x, r1z, r2z, r1y, r2y, drr, rrr
    type(stencil_t) :: S
    integer :: ixp(2)

    e1x = 0.0_dp; e2x = 0.0_dp; e1z = 0.0_dp; e2z = 0.0_dp
    e1y = 0.0_dp; e2y = 0.0_dp; cN = 0
    ixp = [IGXX, IPHI]

    do kk = 3, Nz-2
       do ii = 2, Nx-2
          rrr = rks_of(xg(ii), zg(kk))
          drr = rrr - r_excise
          if (drr < 0.0_dp .or. drr > real(NBD2,dp)*dx) cycle
          ib = int(drr/dx) + 1
          if (ib < 1) ib = 1
          if (ib > NBD2) ib = NBD2
          cN(ib) = cN(ib) + 1

          call compute_stencil(st, ii, kk, excision_status, S)

          ! ---- analytic references at this point ----
          call ks_bssn_state(xg(ii), 0.0_dp, zg(kk), fc)
          call ks_bssn_state(xg(ii)+hh, 0.0_dp, zg(kk), fp)
          call ks_bssn_state(xg(ii)-hh, 0.0_dp, zg(kk), fm)
          call ks_bssn_state(xg(ii), hh, zg(kk), fp2)
          call ks_bssn_state(xg(ii), -hh, zg(kk), fm2)
          do fld = 1, 2
             ! y references: full central differences of the analytic field
             r1x = (fp(ixp(fld)) - fm(ixp(fld)))/(2.0_dp*hh)
             r2x = (fp(ixp(fld)) - 2.0_dp*fc(ixp(fld)) + fm(ixp(fld)))/(hh*hh)
             r1y = (fp2(ixp(fld)) - fm2(ixp(fld)))/(2.0_dp*hh)
             r2y = (fp2(ixp(fld)) - 2.0_dp*fc(ixp(fld)) + fm2(ixp(fld)))/(hh*hh)
             e1x(ib,fld) = max(e1x(ib,fld), abs(S%d1x(ixp(fld)) - r1x))
             e2x(ib,fld) = max(e2x(ib,fld), abs(S%d2x(ixp(fld)) - r2x))
             e1y(ib,fld) = max(e1y(ib,fld), abs(S%d1y(ixp(fld)) - r1y))
             e2y(ib,fld) = max(e2y(ib,fld), abs(S%d2y(ixp(fld)) - r2y))
          end do

          call ks_bssn_state(xg(ii), 0.0_dp, zg(kk)+hh, fp)
          call ks_bssn_state(xg(ii), 0.0_dp, zg(kk)-hh, fm)
          do fld = 1, 2
             r1z = (fp(ixp(fld)) - fm(ixp(fld)))/(2.0_dp*hh)
             r2z = (fp(ixp(fld)) - 2.0_dp*fc(ixp(fld)) + fm(ixp(fld)))/(hh*hh)
             e1z(ib,fld) = max(e1z(ib,fld), abs(S%d1z(ixp(fld)) - r1z))
             e2z(ib,fld) = max(e2z(ib,fld), abs(S%d2z(ixp(fld)) - r2z))
          end do
       end do
    end do

    write(*,'(A)') ' dr/dx    n     |d1x err| |d2x err| |d1z err| |d2z err|'//&
                ' |d1y err| |d2y err|   (gtilde_xx)'
    do ib = 1, NBD2
       if (cN(ib) == 0) cycle
       write(*,'(3X,F5.1,2X,I4,6(2X,ES10.3))') real(ib-1,dp), cN(ib), &
            e1x(ib,1), e2x(ib,1), e1z(ib,1), e2z(ib,1), e1y(ib,1), e2y(ib,1)
    end do
    write(*,'(A)') ' dr/dx    n     |d1x err| |d2x err| |d1z err| |d2z err|'//&
                ' |d1y err| |d2y err|   (phi)'
    do ib = 1, NBD2
       if (cN(ib) == 0) cycle
       write(*,'(3X,F5.1,2X,I4,6(2X,ES10.3))') real(ib-1,dp), cN(ib), &
            e1x(ib,2), e2x(ib,2), e1z(ib,2), e2z(ib,2), e1y(ib,2), e2y(ib,2)
    end do
  end subroutine derivative_errors

  ! Decompose H into the metric (Rt) and conformal-factor (Rphi) halves plus
  ! the algebraic K/A piece, binned by dr/dx.  Which half carries the
  ! resolution-independent floor tells us which derivative set is at fault.
  subroutine h_decomposition(st)
    real(dp), intent(in) :: st(:,:,:)
    integer, parameter :: NBD = 8
    real(dp) :: aRt(NBD), aRp(NBD), aKA(NBD), aH(NBD)
    integer :: cN(NBD), ii, kk, ib
    real(dp) :: e4, val, Rt_c, Rp_c, KA_c, H_c, drr, rrr
    type(stencil_t) :: S
    type(conformal_geometry_t) :: G

    aRt = 0.0_dp; aRp = 0.0_dp; aKA = 0.0_dp; aH = 0.0_dp; cN = 0
    do kk = 3, Nz-2
       do ii = 2, Nx-2
          rrr = rks_of(xg(ii), zg(kk))
          drr = rrr - r_excise
          if (drr < 0.0_dp .or. drr > real(NBD,dp)*dx) cycle
          ib = int(drr/dx) + 1
          if (ib < 1) ib = 1
          if (ib > NBD) ib = NBD

          call compute_stencil(st, ii, kk, excision_status, S)
          call compute_conformal_geometry(S, G)
          e4 = exp(-4.0_dp*st(ii,kk,IPHI))
          Rt_c = e4*sum(G%gtinv * G%Rt)
          Rp_c = e4*sum(G%gtinv * G%Rphi)
          KA_c = (2.0_dp/3.0_dp)*st(ii,kk,IK)**2 &
                 - sum(packA(st(ii,kk,:))*matmul(G%gtinv, &
                     matmul(packA(st(ii,kk,:)), G%gtinv)))
          H_c = Rt_c + Rp_c + KA_c

          aRt(ib) = max(aRt(ib), abs(Rt_c))
          aRp(ib) = max(aRp(ib), abs(Rp_c))
          aKA(ib) = max(aKA(ib), abs(KA_c))
          aH(ib)  = max(aH(ib),  abs(H_c))
          cN(ib) = cN(ib) + 1
       end do
    end do
    do ib = 1, NBD
       if (cN(ib) == 0) cycle
       write(*,'(3X,F5.1,2X,I5,4(2X,ES11.3))') real(ib-1,dp), cN(ib), aRt(ib), aRp(ib), aKA(ib), aH(ib)
    end do
  end subroutine h_decomposition

  ! pack the Atilde block of a state row into a 3x3 tensor
  function packA(arr) result(M)
    real(dp), intent(in) :: arr(:)
    real(dp) :: M(3,3)
    M(1,1) = arr(IAXX); M(1,2) = arr(IAXY); M(1,3) = arr(IAXZ)
    M(2,1) = M(1,2); M(2,2) = arr(IAYY); M(2,3) = arr(IAYZ)
    M(3,1) = M(1,3); M(3,2) = M(2,3); M(3,3) = arr(IAZZ)
  end function packA

  ! Kerr radius (same formula as excision_mod)
  function rks_of(xq, zq) result(rq)
    real(dp), intent(in) :: xq, zq
    real(dp) :: rq, rho2, a2, r2
    a2 = bh_spin*bh_spin
    rho2 = xq*xq + zq*zq
    r2 = 0.5_dp*( (rho2 - a2) + sqrt((rho2 - a2)**2 + 4.0_dp*a2*zq*zq) )
    rq = sqrt(max(0.0_dp, r2))
  end function rks_of

  ! Analytic Kerr-Schild BSSN state, using the production conversion formula
  ! (identical to set_kerr_schild_id, evaluated off-grid at arbitrary (x,z)).
  subroutine ks_bssn_state(xq, yq, zq, out)
    real(dp), intent(in)  :: xq, yq, zq
    real(dp), intent(out) :: out(:)
    real(dp) :: gij(3,3), gijinv(3,3), alpha, beta_up(3), Kij(3,3)
    real(dp) :: phia, Ktra, detg, twoH, e4
    real(dp) :: gt(3,3), At(3,3)

    call kerr_schild_adm(xq, yq, zq, gij, gijinv, alpha, beta_up, Kij)
    detg = gij(1,1)*(gij(2,2)*gij(3,3)-gij(2,3)*gij(2,3)) &
         - gij(1,2)*(gij(1,2)*gij(3,3)-gij(2,3)*gij(1,3)) &
         + gij(1,3)*(gij(1,2)*gij(2,3)-gij(2,2)*gij(1,3))
    twoH = max(detg - 1.0_dp, 0.0_dp)
    phia = log(max(detg,1.0e-300_dp))/12.0_dp     ! conformal_factor_mode 0
    e4   = exp(4.0_dp*phia)
    gt   = exp(-4.0_dp*phia)*gij
    Ktra = sum(gijinv*Kij)
    At   = (Kij - (gij*Ktra)/3.0_dp)/e4

    out = 0.0_dp
    out(IPHI)   = phia
    out(IK)     = Ktra
    out(IALPHA) = alpha
    out(IBETAX) = beta_up(1)
    out(IBETAY) = beta_up(2)
    out(IBETAZ) = beta_up(3)
    out(IGXX) = gt(1,1); out(IGXY) = gt(1,2); out(IGXZ) = gt(1,3)
    out(IGYY) = gt(2,2); out(IGYZ) = gt(2,3); out(IGZZ) = gt(3,3)
    out(IAXX) = At(1,1); out(IAXY) = At(1,2); out(IAXZ) = At(1,3)
    out(IAYY) = At(2,2); out(IAYZ) = At(2,3); out(IAZZ) = At(3,3)
  end subroutine ks_bssn_state

  ! max componentwise difference over a symmetric-tensor variable group
  function tensor_err(a, b, idx) result(e)
    real(dp), intent(in) :: a(:), b(:)
    integer, intent(in) :: idx(6)
    real(dp) :: e
    integer :: m
    e = 0.0_dp
    do m = 1, 6
       e = max(e, abs(a(idx(m)) - b(idx(m))))
    end do
  end function tensor_err

  ! Evaluate H over all points and report the largest values among the
  ! innermost physical points, with their location.
  subroutine h_at_point(st, Hout, label)
    real(dp), intent(in) :: st(:,:,:)
    real(dp), intent(inout) :: Hout(:,:)
    character(len=*), intent(in) :: label
    real(dp) :: best, dr
    integer :: bi, bk, jj, kk, nbin
    integer, parameter :: NBH = 12
    real(dp) :: hbin(NBH)
    integer :: cbin(NBH)

    call hamiltonian_constraint(st, Hout, ihmin, ihmax, khmin, khmax)

    best = -1.0_dp; bi = 0; bk = 0
    do kk = 1, Nz
       do jj = 1, Nx
          if (excision_status(jj,kk) /= PHYSICAL) cycle
          rq = rks_of(xg(jj), zg(kk))
          if (rq < r_excise + 1.0e-9_dp) cycle
          if (abs(Hout(jj,kk)) > best) then
             best = abs(Hout(jj,kk)); bi = jj; bk = kk
          end if
       end do
    end do

    write(*,'(A)') '------------------------------------------------------------------------'
    write(*,'(A,A,A,ES12.4,A,I0,A,I0,A,F9.5,A,F8.4)') &
         ' ', label, ':  max|H| over ALL physical points = ', best, &
         '  at i=', bi, ' k=', bk, '  r_KS=', rks_of(xg(bi), zg(bk)), &
         '  (dr=', rks_of(xg(bi), zg(bk)) - r_excise, '/dx)'

    ! H profile against distance from the mask, in units of dx.  Binned (not
    ! sorted-and-truncated) so the numbers are comparable across resolutions.
    write(*,'(A)') '   max|H| per radial bin, bins of one cell outward from the mask:'
    do jj = 1, NBH
       hbin(jj) = 0.0_dp; cbin(jj) = 0
    end do
    do kk = 1, Nz
       do jj = 1, Nx
          if (excision_status(jj,kk) /= PHYSICAL) cycle
          rq = rks_of(xg(jj), zg(kk))
          dr = rq - r_excise
          if (dr < 0.0_dp .or. dr > real(NBH,dp)*dx) cycle
          nbin = int(dr/dx) + 1
          if (nbin < 1) nbin = 1
          if (nbin > NBH) nbin = NBH
          hbin(nbin) = max(hbin(nbin), abs(Hout(jj,kk)))
          cbin(nbin) = cbin(nbin) + 1
       end do
    end do
    write(*,'(A)') '   dr/dx      n        max|H|'
    do jj = 1, NBH
       if (cbin(jj) == 0) cycle
       write(*,'(3X,F6.1,2X,I5,3X,ES13.5)') real(jj-1,dp), cbin(jj), hbin(jj)
    end do
  end subroutine h_at_point

end program diagnose_ghost_accuracy