!===============================================================================
! diagnose_inner_boundary -- Work Package B instrumentation.
!
! PURPOSE
!   Theory triage (baseline/THEORY_TRIAGE.md) localised the long-time
!   conformal-determinant failure to the excision boundary: max|H| grows
!   LINEARLY in time from t=0, max|K| peaks ~1 cell outside r_excise, and the
!   growth gets ~5x worse when r_excise is moved inward.  The question this
!   program answers is the one the report's Work Package C step 3 asks for:
!   is the error INJECTED by the interior extension (extrapolation), by the
!   stencil-order drop at the mask edge, or by a genuine incoming mode?
!
! METHOD (static, at t = 0 -- no evolution required, so this is cheap and
!        unambiguous)
!   Build the exact Kerr-Schild state twice from the same initial data:
!
!     u_prod : production path -- analytic exterior, then extrapolate_excised()
!              overwrites the interior/ghost region with the bounded extension.
!     u_ana  : analytic everywhere -- no extrapolation at all, so the interior
!              holds the true Kerr-Schild values.
!
!   Then evaluate the SAME point RHS (compute_rhs_point) at the same exterior
!   ring points with both states:
!
!     err = rhs(u_prod) - rhs(u_ana)
!
!   err is exactly the error the extrapolation injects into the evolution of
!   that point.  The interior values are a pure input to the point RHS, so no
!   other difference exists.
!
!   This cleanly separates the two candidate sources:
!     * if err is large and converges slowly with dx -> the extension is the
!       dominant error source (Theory B),
!     * if err is small but rhs(u_ana) itself converges slowly -> the stencil
!       order drop at the mask edge dominates (Theory C).
!
!   Also reports, per ring point, which derivative stencil branch the production
!   stencil actually selects (4th order centred / 2nd order centred / one
!   sided), since the branch drops wherever a 2-away neighbour is excised.
!
!   The whole thing is repeated over a resolution ladder inside one run so the
!   convergence order of each contribution is measured directly.
!
! USAGE
!   diagnose_inner_boundary [parfile]
!===============================================================================
program diagnose_inner_boundary
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, &
                          excision_reset_geometry, is_excised, EXCISED, &
                          excision_status, FD_RADIUS, GHOST, &
                          get_excision_background_point
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_point
  use timestep_mod, only: rk4_step

  implicit none

  real(dp), allocatable :: up(:,:,:), ua(:,:,:), rp(:,:,:), ra(:,:,:), err(:,:,:)
  integer :: nx0, nz0, nxr, nzr, ir, i, k, v
  real(dp) :: rks, r1, r2
  character(len=256) :: par_file, buf
  real(dp) :: fill_changed
real(dp), parameter :: unused = 0.0_dp
integer, parameter :: FD_R = 2
real(dp) :: pt1(NVARS), pt2(NVARS)
real(dp) :: ering(NVARS), ering_ana(NVARS)
real(dp) :: ana_smallx(NVARS), ana_bigx(NVARS)
integer  :: n_exc, n_touch
  real(dp) :: ebulk(NVARS)
  integer  :: i_ring, k_ring, i_bulk, k_bulk
  integer  :: bx(NVARS), bz(NVARS)
  integer, parameter :: NREF = 3
  integer  :: hist_nx(NREF)
  real(dp) :: hist_fill(NREF), hist_ana(NREF), hist_bulk(NREF)
  logical  :: ex

  hist_nx = 0; hist_fill = 0.0_dp; hist_ana = 0.0_dp; hist_bulk = 0.0_dp

  par_file = 'parameter_files/vacuum_kerr.par'
  buf = ''
  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) par_file = buf
  inquire(file=trim(par_file), exist=ex)
  if (.not. ex) then
     write(*,'(A)') 'ERROR: parameter file "'//trim(par_file)//'" not found.'
     error stop 'diagnose_inner_boundary: missing parameter file'
  end if

  call init_params(trim(par_file))
  nx0 = Nx; nz0 = Nz

  allocate(up(nx0,nz0,NVARS), ua(nx0,nz0,NVARS))
  allocate(rp(nx0,nz0,NVARS), ra(nx0,nz0,NVARS), err(nx0,nz0,NVARS))

  write(*,'(A)') '========================================================================'
  write(*,'(A)') ' Inner-boundary RHS instrumentation (exact Kerr-Schild, t=0)'
  write(*,'(A,F8.4,A,F8.4)') ' a=',bh_spin,'  r_excise=',r_excise
  write(*,'(A)') ' err = rhs(production fill) - rhs(analytic interior)'
  write(*,'(A)') '========================================================================'

  do ir = 1, NREF
     call init_params(trim(par_file))
     if (ir > 1) then
        Nx = nx0 * 2**(ir-1)
        Nz = nz0 * 2**(ir-1)
        dx = xmax/real(Nx-1,dp)
        dz = 2.0_dp*zmax/real(Nz-1,dp)
        dy = dx
     end if
     if (allocated(xg)) deallocate(xg)
     if (allocated(zg)) deallocate(zg)
     call setup_grid()
     if (allocated(up)) deallocate(up, ua, rp, ra, err)
     call alloc_state(up, Nx, Nz)
     call alloc_state(ua, Nx, Nz)
     allocate(rp(Nx,Nz,NVARS), ra(Nx,Nz,NVARS), err(Nx,Nz,NVARS))

     ! --- production state: analytic exterior + extrapolated interior
     ! Freeze the gauge.  Kerr-Schild data are NOT stationary under 1+log, so
     ! with the default gauge the lapse/shift RHS is legitimately O(1) and
     ! would swamp the discretisation residual we are trying to measure.
     slicing_mode = 2
     gamma_driver_mode = 2
     call set_kerr_schild_id(up)
     call compute_conformal_connections(up)
     call set_excision_background(up)
     call excision_reset_geometry()
     call extrapolate_excised(up)
     call init_deriv_stencils()

     ! --- analytic state: identical exterior, TRUE interior (no extension)
     call set_kerr_schild_id(ua)
     call compute_conformal_connections(ua)

     ! --- sanity: the fill must actually change the interior, otherwise the
     !     comparison below is vacuous
     fill_changed = 0.0_dp
     n_exc = 0
     do k = 1, Nz
        do i = 1, Nx
           if (excision_status(i,k) /= GHOST) cycle
           n_exc = n_exc + 1
           fill_changed = max(fill_changed, maxval(abs(up(i,k,:) - ua(i,k,:))))
        end do
     end do
     write(*,'(A,I0,A,ES12.4)') &
          ' sanity: excised cells=', n_exc, '   max|up-ua| inside mask=', fill_changed

     ! RHS at every exterior point for both
     do k = 2, Nz-1
        do i = 2, Nx-1
           call compute_rhs_point(up, i, k, pt1)
           call compute_rhs_point(ua, i, k, pt2)
           rp(i,k,:) = pt1
           ra(i,k,:) = pt2
           err(i,k,:) = pt1 - pt2
        end do
     end do

     ! --- accumulate ring (r_excise .. r_excise+3h) and bulk (2.3..4M) maxima
     ering = 0.0_dp; ering_ana = 0.0_dp; ebulk = 0.0_dp
     ana_smallx = 0.0_dp; ana_bigx = 0.0_dp
     bx = 0; bz = 0
     i_ring = 0; k_ring = 0; i_bulk = 0; k_bulk = 0
     n_touch = 0
     do k = 2, Nz-1
        do i = 2, Nx-1
           rks = rks_radius(xg(i), zg(k))
           if (rks < r_excise .or. rks > 4.0_dp*max(bh_mass,1.0_dp)) cycle
           if (rks < r_excise + 3.0_dp*max(dx,dz)) then
              ! Only points whose derivative stencil actually REACHES an
              ! excised cell can be influenced by the fill.  A point further
              ! out reads only physical values, so the comparison there is
              ! vacuous (err would be identically zero and would dilute the
              ! maximum).
              if (.not. stencil_touches_mask(i,k)) cycle
              n_touch = n_touch + 1
              do v = 1, NVARS
                 if (abs(err(i,k,v)) > ering(v)) then
                    ering(v) = abs(err(i,k,v)); bx(v) = i; bz(v) = k
                 end if
                 if (abs(ra(i,k,v)) > ering_ana(v)) ering_ana(v) = abs(ra(i,k,v))
                 if (xg(i) < 4.0_dp*dx) then
                    if (abs(ra(i,k,v)) > ana_smallx(v)) ana_smallx(v) = abs(ra(i,k,v))
                 else
                    if (abs(ra(i,k,v)) > ana_bigx(v)) ana_bigx(v) = abs(ra(i,k,v))
                 end if
              end do
              if (i_ring == 0) then
                 i_ring = i; k_ring = k
              end if
           else if (rks > 2.3_dp) then
              do v = 1, NVARS
                 if (abs(err(i,k,v)) > ebulk(v)) ebulk(v) = abs(err(i,k,v))
              end do
              if (i_bulk == 0) then
                 i_bulk = i; k_bulk = k
              end if
           end if
        end do
     end do

     write(*,'(A)')
     write(*,'(A,I0,A,I0,A,F10.6,A,F10.6)') &
          ' Nx=',Nx,' Nz=',Nz,' dx=',dx,' dz=',dz
     write(*,'(A,2(1X,I0),A,F9.5,A,I0)') &
          ' first ring point touching mask (i,k)=', i_ring, k_ring, ' r=', &
          rks_radius(xg(max(1,i_ring)), zg(max(1,k_ring))), &
          '   ring pts with masked stencil=', n_touch
     call branch_report(i_ring, k_ring)
     write(*,'(A,2(1X,I0),A,F9.5)') &
          ' first bulk point (i,k)=', i_bulk, k_bulk, ' r=', &
          rks_radius(xg(max(1,i_bulk)), zg(max(1,k_bulk)))
     write(*,'(A)') ' analytic-state RHS in the ring, split by distance from the axis'
     write(*,'(A)') ' (the y-derivative is a cartoon reconstruction; its error is'
     write(*,'(A)') '  geometric, ~ x/sqrt(x^2+dy^2), so it does NOT converge at fixed x/dx)'
     write(*,'(A)') ' var   |rhs|ring|   |rhs| x<4dx   |rhs| x>4dx'
     do v = 1, NVARS
        write(*,'(I4,3(1X,ES12.4))') v, ering_ana(v), ana_smallx(v), ana_bigx(v)
     end do
     write(*,'(A)') ' var  err|ring| (fill-injected)   |rhs_ana|ring|   err|bulk|'
     do v = 1, NVARS
        write(*,'(I4,3(1X,ES12.4))') v, ering(v), ering_ana(v), ebulk(v)
     end do
     write(*,'(A,I0,A,2(1X,I0),A,ES11.4)') &
          ' worst fill-injected var=', maxloc(ering,1), ' at (i,k)=', &
          bx(maxloc(ering,1)), bz(maxloc(ering,1)), &
          '  err=', maxval(ering)
     write(*,'(A)') '========================================================================'

     call record(ir, Nx, maxval(ering), maxval(ering_ana), maxval(ebulk))
     deallocate(up, ua, rp, ra, err)
  end do

  ! ---------------------------------------------------------------------------
  ! TIME-DEPENDENT PART.
  !
  ! The static comparison above cannot see the real problem: the fill is
  !   u = background + extrapolate(u - background)
  ! so at t=0 the perturbation is identically zero and the fill reproduces the
  ! analytic interior EXACTLY (err identically zero).  The interesting quantity
  ! is therefore how the extrapolated PERTURBATION grows with depth into the
  ! mask once the exterior has evolved.
  !
  ! excised.F90's own comment warns that each link in a fill chain reapplies a
  ! [4,-6,4,-1] polynomial (L1 norm 15) and that a few links deep can
  ! amplify an O(1) boundary value to O(1e4).  This measures whether that is
  ! happening: |perturbation|/|background| per depth layer.
  ! ---------------------------------------------------------------------------
  call perturbation_depth_scan()

  call convergence_report()

contains

  ! Does this point's derivative stencil read any excised cell?  Mirrors the
  ! stencil footprint of stencil.F90: +-1 and +-2 in x and z (FD_RADIUS = 2),
  ! plus the cartoon y ghosts, which are radial interpolations that can also
  ! land inside the mask when x is small.
  function stencil_touches_mask(ii, kk) result(touch)
    integer, intent(in) :: ii, kk
    logical :: touch
    integer :: di, dk
    touch = .false.
    if (ii < 2 .or. ii > Nx-1 .or. kk < 2 .or. kk > Nz-1) return
    do di = -FD_R, FD_R
       if (ii+di < 1 .or. ii+di > Nx) cycle
       do dk = -FD_R, FD_R
          if (kk+dk < 1 .or. kk+dk > Nz) cycle
          if (excision_status(ii+di, kk+dk) == GHOST) then
             touch = .true.
             return
          end if
       end do
    end do
  end function stencil_touches_mask

  ! Evolve the frozen-gauge system, then report how the extrapolated
  ! PERTURBATION compares with the frozen background inside the mask.
  !
  ! excised.F90's own comment warns that each link in a fill chain reapplies a
  ! [4,-6,4,-1] polynomial (L1 norm 15), so a few links deep can amplify an
  ! O(1) boundary value to O(1e4).  If |pert| >> |background| the interior is
  ! perturbation-dominated and the fill no longer represents the background
  ! solution at all.
  subroutine perturbation_depth_scan()
    real(dp), allocatable :: us(:,:,:)
    real(dp) :: bgp(NVARS), pert, pmax, bmax, dtr, dt
    integer  :: nstep, nloop, i, k, v, nb

    write(*,'(A)') '========================================================================'
    write(*,'(A)') ' PERTURBATION DEPTH SCAN (frozen gauge, background frozen at t=0)'
    write(*,'(A)') ' interior = background(t=0) + extrapolated (u - background)'

    nstep = 0
    buf = ''
    call get_command_argument(2, buf)
    if (len_trim(buf) > 0) read(buf,*) nstep

    call init_params(trim(par_file))
    slicing_mode = 2
    gamma_driver_mode = 2
    if (allocated(xg)) deallocate(xg)
    if (allocated(zg)) deallocate(zg)
    call setup_grid()
    if (allocated(us)) deallocate(us)
    call alloc_state(us, Nx, Nz)
    call set_kerr_schild_id(us)
    call compute_conformal_connections(us)
    call set_excision_background(us)
    call excision_reset_geometry()
    call extrapolate_excised(us)
    call init_deriv_stencils()

    dt = 0.005_dp*min(dx,dz)/sqrt(2.0_dp)
    write(*,'(A,I0,A,I0,A,F8.3,A,ES10.3)') &
         ' Nx=',Nx,' steps=',nstep,' r_excise=',r_excise,' dt=',dt
    write(*,'(A)') '  step      t        n_mask    max|pert|    max|background|   ratio'

    do nloop = 0, nstep
       if (nloop > 0) call rk4_step(us, dt, nloop)
       if (mod(nloop, max(1,nstep/4)) /= 0 .and. nloop /= nstep) cycle
       call extrapolate_excised(us)
       pmax = 0.0_dp; bmax = 0.0_dp; nb = 0
       do k = 1, Nz
          do i = 1, Nx
             if (excision_status(i,k) /= GHOST) cycle
             nb = nb + 1
             call get_excision_background_point(i,k,bgp)
             do v = 1, NVARS
                pert = abs(us(i,k,v) - bgp(v))
                if (pert > pmax) pmax = pert
             end do
             do v = 1, NVARS
                dtr = abs(bgp(v))
                if (dtr > bmax) bmax = dtr
             end do
          end do
       end do
       write(*,'(I6,2X,F8.4,2X,I7,2X,ES11.4,2X,ES14.4,2X,F8.2)') &
            nloop, real(nloop,dp)*dt, nb, pmax, bmax, &
            merge(pmax/merge(bmax,1.0_dp, bmax>0.0_dp), 0.0_dp, bmax>0.0_dp)
    end do
    write(*,'(A)') '========================================================================'
  end subroutine perturbation_depth_scan

  ! Kerr radius, matching the diagnostic band used by the driver
  function rks_radius(xq, zq) result(rq)
    real(dp), intent(in) :: xq, zq
    real(dp) :: rq, rho2, a2
    a2 = bh_spin*bh_spin
    rho2 = xq*xq + zq*zq
    rq = sqrt(max(0.0_dp, 0.5_dp*( (rho2-a2) + sqrt((rho2-a2)**2 + 4.0_dp*a2*zq*zq) )))
  end function rks_radius

  ! Which derivative stencil branch does compute_stencil select here?
  ! Mirrors stencil.F90: 4th order needs +-1 and +-2 all usable; else 2nd
  ! order centred; else one-sided.  (The y direction always uses cartoon
  ! ghosts and has no branch.)
  subroutine branch_report(ii, kk)
    integer, intent(in) :: ii, kk
    integer :: i1, i2, j1, j2
    logical :: x4, z4
    character(len=8) :: bx4, bz4
    i1 = min(max(ii-1,1), Nx); i2 = min(max(ii-2,1), Nx)
    j1 = min(max(kk-1,1), Nz); j2 = min(max(kk-2,1), Nz)
    x4 = (ii-1 >= 1 .and. ii-2 >= 1 .and. ii+2 <= Nx) .and. &
         (excision_status(i1,kk) /= EXCISED) .and. (excision_status(i2,kk) /= EXCISED) .and. &
         (excision_status(ii+1,kk) /= EXCISED) .and. (excision_status(ii+2,kk) /= EXCISED)
    z4 = (kk-1 >= 1 .and. kk-2 >= 1 .and. kk+2 <= Nz) .and. &
         (excision_status(ii,j1) /= EXCISED) .and. (excision_status(ii,j2) /= EXCISED) .and. &
         (excision_status(ii,kk+1) /= EXCISED) .and. (excision_status(ii,kk+2) /= EXCISED)
    bx4 = 'reduced '
    bz4 = 'reduced '
    if (x4) bx4 = '4th-ord '
    if (z4) bz4 = '4th-ord '
    write(*,'(A,2A,2A)') &
         '   stencil branch at that point: x=', bx4, '  z=', bz4, &
         '   (y uses cartoon ghosts always)'
  end subroutine branch_report

  subroutine record(ir_, nx_, e1, e2, e3)
    integer, intent(in) :: ir_, nx_
    real(dp), intent(in) :: e1, e2, e3
    if (ir_ <= size(hist_nx)) then
       hist_nx(ir_) = nx_
       hist_fill(ir_) = e1
       hist_ana(ir_)  = e2
       hist_bulk(ir_) = e3
    end if
  end subroutine record

  subroutine convergence_report()
    integer :: q
    write(*,'(A)') ' convergence of the two contributions (max over ring):'
    write(*,'(A)') '   Nx        err_fill        err_ana_analytic'
    do q = 1, NREF
       if (hist_nx(q) > 0) write(*,'(I6,2(1X,ES13.5))') hist_nx(q), hist_fill(q), hist_ana(q)
    end do
    do q = 2, NREF
       if (hist_nx(q) > 0 .and. hist_fill(q) > 0.0_dp) &
            write(*,'(A,I0,A,F6.2)') '   order of err_fill at Nx=', hist_nx(q), ' : ', &
                 log(hist_fill(q-1)/hist_fill(q))/log(2.0_dp)
       if (hist_nx(q) > 0 .and. hist_ana(q) > 0.0_dp) &
            write(*,'(A,I0,A,F6.2)') '   order of err_ana  at Nx=', hist_nx(q), ' : ', &
                 log(hist_ana(q-1)/hist_ana(q))/log(2.0_dp)
    end do
    write(*,'(A)') '========================================================================'
  end subroutine convergence_report

end program diagnose_inner_boundary