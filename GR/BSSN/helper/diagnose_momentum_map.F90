!===============================================================================
! diagnose_momentum_map -- WHERE does the momentum-constraint residual live?
!
! test_momentum_conv reports that rms|M| does not converge:
!     5.73e-4 -> 5.44e-4 -> 5.85e-4   (observed order -0.11)
! A 2nd-order stencil converges, so this plateau is NOT the y-stencil order
! (see the MEASURED ACCURACY block in helper/cartoon.F90).  This program finds
! out which cells are responsible, instead of guessing again.
!
! For each refinement it bins |M|^2 by polar angle, and re-measures the rms
! with the near-axis strip removed.  The two hypotheses that have been floated:
!   (H-polar) residual concentrated near the polar axis, |z|/r -> 1.  Then the
!             polar bins fail to converge while equatorial ones do.
!   (H-axis)  residual concentrated at small x, where the cartoon rotation
!             divides by x.  Then dropping x < 4 dx restores convergence.
! If neither shows up, the residual is spread evenly and it is neither.
!
! Build-only diagnostic.  Not a gate.
!
! Usage: diagnose_momentum_map [parfile] [rin] [rout] [refinements]
!===============================================================================
program diagnose_momentum_map
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint

  implicit none
  real(dp), allocatable :: u(:,:,:), M(:,:,:), Mt(:,:,:,:), us(:,:,:)
  real(dp), allocatable :: gth(:,:), ncnt(:,:), grb(:,:), ncnt_r(:,:)
  real(dp), allocatable :: psum(:,:)
  character(len=7), allocatable :: pnames(:)
  real(dp), allocatable :: npts_tot(:)
  real(dp), allocatable :: r_edge(:)
  character(len=256) :: par_file, buf
  real(dp) :: rin, rout, rms_all, rms_ax, xat, zat, rat, cthat
  real(dp) :: m2v, rr, ctv, ord
  integer :: nx0, nz0, nref, ir, i, kz, jb, jf
  logical :: do_ext
  integer, parameter :: NTH = 4, NRB = 12, NTM = 7
  real(dp), parameter :: th_edge(NTH+1) = [0.0_dp, 0.2_dp, 0.5_dp, 0.8_dp, 1.0_dp]

  par_file = 'parameter_files/vacuum_kerr.par'
  rin = 3.0_dp; rout = 5.0_dp; nref = 3
  buf = ''
  call get_command_argument(1, buf); if (len_trim(buf) > 0) par_file = buf
  buf = ''
  call get_command_argument(2, buf); if (len_trim(buf) > 0) read(buf,*) rin
  buf = ''
  call get_command_argument(3, buf); if (len_trim(buf) > 0) read(buf,*) rout
  buf = ''
  call get_command_argument(4, buf); if (len_trim(buf) > 0) read(buf,*) nref
  buf = ''
  call get_command_argument(5, buf); do_ext = (len_trim(buf) == 0)
  if (len_trim(buf) > 0) write(*,'(A,A)') ' NOTE: skipping the excision pass (', trim(buf), ')'

  allocate(gth(NTH,nref), ncnt(NTH,nref))
  allocate(grb(NRB,nref), ncnt_r(NRB,nref))
  allocate(psum(NTM,nref), npts_tot(nref))
  pnames = [character(len=7) :: 'd_jA^ij', '-2/3Kdg', '-2/3gt dK', &
                        '-4dphiAu', '+8/3dphi', 'Gam^i_jk', 'Gam^j_jk']
  allocate(r_edge(NRB+1))
  do jb = 1, NRB+1
    r_edge(jb) = rin + (rout-rin)*real(jb-1,dp)/real(NRB,dp)
  end do
  call init_params(trim(par_file))
  nx0 = Nx; nz0 = Nz

  write(*,'(A)') '============================================================'
  write(*,'(A,F8.3,A,F8.3,A)') ' annulus r in [', rin, ',', rout, ']'
  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' global rms vs rms with the near-axis strip (x < 4 dx) removed'

  do ir = 1, nref
    call init_params(trim(par_file))
    Nx = nx0*(2**(ir-1));  Nz = nz0*(2**(ir-1))
    dx = xmax/real(Nx-1, dp)
    dz = 2.0_dp*zmax/real(Nz-1, dp)
    dy = dx
    if (allocated(xg))  deallocate(xg)
    if (allocated(zg))  deallocate(zg)
    call setup_grid()
    if (allocated(u))  deallocate(u)
    if (allocated(M))  deallocate(M)
    call alloc_state(u, Nx, Nz)
    allocate(M(Nx,Nz,3))
    if (allocated(Mt)) deallocate(Mt)
    allocate(Mt(Nx,Nz,3,NTM))

    call set_kerr_schild_id(u)
    call compute_conformal_connections(u)
    if (do_ext) call set_excision_background(u)
    call init_deriv_stencils()
    call excision_reset_geometry()
    if (do_ext) then
      if (.not. allocated(us)) allocate(us(Nx,Nz,NVARS))
      us = u
      call extrapolate_excised(u)
      call report_excision_damage(us, u, rin, rout)
    end if
    call momentum_constraint(u, M, Mt)

    gth(:,ir) = 0.0_dp; ncnt(:,ir) = 0.0_dp
    grb(:,ir) = 0.0_dp; ncnt_r(:,ir) = 0.0_dp
    psum(:,ir) = 0.0_dp
    do kz = 3, Nz-2
      do i = 2, Nx-1
        call cell_m2(i, kz, rin, rout, m2v, rr, ctv, jf)
        if (m2v < 0.0_dp) cycle
        do jb = 1, NTH
          if (ctv >= th_edge(jb) .and. ctv < th_edge(jb+1)) then
            gth(jb,ir) = gth(jb,ir) + m2v
            ncnt(jb,ir) = ncnt(jb,ir) + 1.0_dp
          end if
        end do
        do jb = 1, NRB
          if (rr >= r_edge(jb) .and. rr < r_edge(jb+1)) then
            grb(jb,ir) = grb(jb,ir) + m2v
            ncnt_r(jb,ir) = ncnt_r(jb,ir) + 1.0_dp
          end if
        end do
        do jb = 1, NTM
          psum(jb,ir) = psum(jb,ir) + Mt(i,kz,jf,jb)**2
        end do
      end do
    end do
    gth(:,ir) = sqrt(gth(:,ir)/(3.0_dp*max(ncnt(:,ir),1.0_dp)))

    call scan(rin, rout, 4.0_dp*dx, rms_all, rms_ax, xat, zat, rat, cthat)
    npts_tot(ir) = sum(ncnt(:,ir))
    write(*,'(A,I2,A,I5,A,F9.4,3(A,ES13.4))') ' ref ', ir, &
         '  Nx=', Nx, '  dx=', dx, &
         '  rms_all=', rms_all, &
         '  rms_away=', rms_ax, &
         '  ratio=', rms_ax/max(rms_all,1.0e-30_dp)
    write(*,'(A,3(A,F9.4),A,F8.4)') '        max|M| at', '  x=', xat, &
         '  z=', zat, '  r=', rat, '  |z|/r=', cthat
  end do

  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' per polar-angle bin (cth = |z|/r):'
  write(*,'(A)') '   bin |z|/r      rms(ref1)     rms(ref2)     rms(ref3)    ord23'
  do jb = 1, NTH
    ord = 0.0_dp
    if (nref >= 3 .and. gth(jb,2) > 0.0_dp .and. gth(jb,3) > 0.0_dp) &
      ord = log(sqrt(gth(jb,2)/ncnt(jb,2))/sqrt(gth(jb,3)/ncnt(jb,3)))/log(2.0_dp)
    write(*,'(2A,F5.2,A,F5.2,3(A,ES13.4),A,F7.2)') '   ', '', &
         th_edge(jb), '..', th_edge(jb+1), &
         '  ', gth(jb,1), '  ', gth(jb,2), '  ', gth(jb,3), '  ord=', ord
  end do
  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' (ord is log2(rms2/rms3); 0 means dx-INDEPENDENT, not converged)'
  write(*,'(A)') ' per radial bin:'
  write(*,'(A)') '   bin  r         rms(ref1)     rms(ref2)     rms(ref3)    ord23'
  do jb = 1, NRB
    ord = 0.0_dp
    if (nref >= 3 .and. grb(jb,2) > 0.0_dp .and. grb(jb,3) > 0.0_dp) &
      ord = log(sqrt(grb(jb,2)/ncnt_r(jb,2))/sqrt(grb(jb,3)/ncnt_r(jb,3)))/log(2.0_dp)
    write(*,'(2A,F6.2,A,F6.2,3(A,ES13.4),A,F7.2)') '   ', '', &
         r_edge(jb), '..', r_edge(jb+1), &
         '  ', sqrt(grb(jb,1)/(3.0_dp*max(ncnt_r(jb,1),1.0_dp))), &
         '  ', sqrt(grb(jb,2)/(3.0_dp*max(ncnt_r(jb,2),1.0_dp))), &
         '  ', sqrt(grb(jb,3)/(3.0_dp*max(ncnt_r(jb,3),1.0_dp))), '  ord=', ord
  end do
  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' per-TERM decomposition of M, rms over the annulus.'
  write(*,'(A)') ' For exact Kerr-Schild data every term vanishes on its own.'
  write(*,'(A)') ' ord = log2(rms2/rms3); ~0 means that term does not converge.'
  write(*,'(A)') '   term             rms(ref1)     rms(ref2)     rms(ref3)    ord23'
  do jb = 1, NTM
    ord = 0.0_dp
    if (nref >= 3 .and. psum(jb,2) > 0.0_dp .and. psum(jb,3) > 0.0_dp) &
      ord = log(sqrt(psum(jb,2)/max(npts_tot(2),1.0_dp))/ &
                sqrt(psum(jb,3)/max(npts_tot(3),1.0_dp)))/log(2.0_dp)
    write(*,'(2A,A,3(A,ES13.4),A,F7.2)') '   ', '', pnames(jb), &
         '  ', sqrt(psum(jb,1)/max(npts_tot(1),1.0_dp)), &
         '  ', sqrt(psum(jb,2)/max(npts_tot(2),1.0_dp)), &
         '  ', sqrt(psum(jb,3)/max(npts_tot(3),1.0_dp)), '  ord=', ord
  end do
  write(*,'(A)') '============================================================'

contains

  ! Does extrapolate_excised actually change anything inside the annulus?
  ! A dx-independent perturbation injected here would explain a residual that
  ! is dx-independent in every term of M.
  subroutine report_excision_damage(us, u, rin, rout)
    real(dp), intent(in) :: us(:,:,:), u(:,:,:), rin, rout
    integer :: i, kz, iv
    real(dp) :: dd, mx, rmsd
    integer :: nch
    mx = 0.0_dp; rmsd = 0.0_dp; nch = 0
    do kz = 1, size(u,2)
      do i = 1, size(u,1)
        if (sqrt(xg(i)**2+zg(kz)**2) < rin .or. &
            sqrt(xg(i)**2+zg(kz)**2) > rout) cycle
        dd = maxval(abs(u(i,kz,:) - us(i,kz,:)))
        if (dd > 0.0_dp) nch = nch + 1
        mx = max(mx, dd);  rmsd = rmsd + dd**2
      end do
    end do
    write(*,'(A,I6,3A,2ES12.4)') '   excision pass touched ', nch, &
         ' cells in annulus,  max|du|=', mx, '  rms|du|=', &
         sqrt(rmsd/max(1,size(u,1)*size(u,2)))
  end subroutine report_excision_damage

  ! Returns |M|^2 at (i,kz), or -1 if outside the annulus.  Also rr, ctv.
  subroutine cell_m2(i, kz, rin, rout, q, rr, ctv, jf)
    integer, intent(in) :: i, kz
    integer, intent(out) :: jf
    real(dp), intent(in) :: rin, rout
    real(dp), intent(out) :: q, rr, ctv
    rr  = sqrt(xg(i)**2 + zg(kz)**2)
    ctv = abs(zg(kz))/max(rr, 1.0e-30_dp)
    jf  = 1
    if (rr < rin .or. rr > rout) then
      q = -1.0_dp
    else
      q = M(i,kz,1)**2 + M(i,kz,2)**2 + M(i,kz,3)**2
    end if
  end subroutine cell_m2

  subroutine scan(rin, rout, xcut, ra, rax, xm, zm, rm, cm)
    real(dp), intent(in) :: rin, rout, xcut
    real(dp), intent(out) :: ra, rax, xm, zm, rm, cm
    integer :: i, kz, jf
    real(dp) :: sa, sax, n1, n2, q, rr, ctv, c
    sa = 0.0_dp; sax = 0.0_dp; n1 = 0.0_dp; n2 = 0.0_dp
    xm = 0.0_dp; zm = 0.0_dp; rm = 0.0_dp; cm = 0.0_dp
    do kz = 3, size(M,2)-2
      do i = 2, size(M,1)-1
        call cell_m2(i, kz, rin, rout, q, rr, ctv, jf)
        if (q < 0.0_dp) cycle
        sa = sa + q;  n1 = n1 + 1.0_dp
        if (xg(i) > xcut) then
          sax = sax + q;  n2 = n2 + 1.0_dp
        end if
        c = sqrt(q)
        if (c > cm) then
          cm = c;  xm = xg(i);  zm = zg(kz);  rm = rr
        end if
      end do
    end do
    ra  = sqrt(sa /(3.0_dp*max(n1,1.0_dp)))
    rax = sqrt(sax/(3.0_dp*max(n2,1.0_dp)))
  end subroutine scan

end program diagnose_momentum_map
