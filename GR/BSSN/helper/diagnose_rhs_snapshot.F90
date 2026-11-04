!===============================================================================
! diagnose_rhs_snapshot -- evaluate the production BSSN RHS on a saved state
!
! Usage: diagnose_rhs_snapshot <parfile> <state.h5>
!
! This is the frozen-state counterpart of diagnose_kerr_bssn.  It reloads any
! state_*.h5 written by the production driver, rebuilds the excision background
! from that same state, and evaluates exactly the production RHS.  The point is
! to separate a t=0 data/gauge defect from an error that grows during the
! evolution, and to show which radial band carries it.
!
! The boundary rows (i=Nx, k=1, k=Nz) are excluded: in the production time
! step their RHS is replaced by the outer Sommerfeld closure, so the interior
! operator is not defined there.  Bands that touch those rows are marked with
! an asterisk in the count column.
!
! This is a diagnostic only.  It does not advance the state or change any
! production parameter.
!===============================================================================
program diagnose_rhs_snapshot
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, xmax, zmax, r_excise, &
                        slicing_mode, init_params
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use excision_mod, only: set_excision_background, excision_reset_geometry, &
                          extrapolate_excised, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  use stencil_mod, only: stencil_t, compute_stencil
  use eq_kerr_bssn_data_mod, only: initialize_kerr_f_cache
  use initial_data_mod, only: fill_conformal_connection_boundary
  use hdf5
  implicit none

  character(len=512) :: par_file, snapshot
  character(len=8), parameter :: field_name(NVARS) = [character(len=8) :: &
       'phi', 'gxx', 'gxy', 'gxz', 'gyy', 'gyz', 'gzz', 'K', &
       'Axx', 'Axy', 'Axz', 'Ayy', 'Ayz', 'Azz', &
       'Gamx', 'Gamy', 'Gamz', 'alpha', 'betax', 'betay', 'betaz', 'Bx', 'By', 'Bz']
  real(dp), allocatable :: u(:,:,:), rhs(:,:,:)
  real(dp) :: redges(10), bandmax(9,NVARS), bandrms(9,NVARS)
  real(dp) :: r, q
  integer :: bandcount(9), band_i(9,NVARS), band_k(9,NVARS)
  integer :: i, k, ib, iv, nb
  type(stencil_t) :: S

  call get_command_argument(1, par_file)
  call get_command_argument(2, snapshot)
  if (len_trim(par_file) == 0 .or. len_trim(snapshot) == 0) then
    write(*,'(A)') 'usage: diagnose_rhs_snapshot <parfile> <state.h5>'
    stop
  end if

  call init_params(trim(par_file))
  call setup_grid()
  allocate(u(Nx,Nz,NVARS), rhs(Nx,Nz,NVARS))
  call read_snapshot(trim(snapshot), u)

  call excision_reset_geometry()
  call set_excision_background(u)
  call extrapolate_excised(u)
  call init_deriv_stencils()
  if (slicing_mode == 4) call initialize_kerr_f_cache()
  call maybe_fill_gt_bc(u)

  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)

  redges = [r_excise, r_excise + 0.10_dp, r_excise + 0.30_dp, &
            r_excise + 0.50_dp, r_excise + 1.50_dp, r_excise + 3.50_dp, &
            r_excise + 6.50_dp, r_excise + 10.50_dp, 20.0_dp, huge(1.0_dp)]
  bandmax = 0.0_dp; bandrms = 0.0_dp; bandcount = 0
  band_i = 0; band_k = 0

  do k = 2, Nz - 1
    do i = 2, Nx - 1
      if (excision_status(i,k) /= PHYSICAL) cycle
      r = sqrt(xg(i)**2 + zg(k)**2)
      ib = 0
      do nb = 1, 9
        if (r >= redges(nb) .and. r < redges(nb+1)) then
          ib = nb
          exit
        end if
      end do
      if (ib == 0) cycle
      bandcount(ib) = bandcount(ib) + 1
      do iv = 1, NVARS
        q = abs(rhs(i,k,iv))
        if (q > bandmax(ib,iv)) then
          bandmax(ib,iv) = q
          band_i(ib,iv) = i
          band_k(ib,iv) = k
        end if
        bandrms(ib,iv) = bandrms(ib,iv) + rhs(i,k,iv)**2
      end do
    end do
  end do
  do ib = 1, 9
    if (bandcount(ib) > 0) bandrms(ib,:) = sqrt(bandrms(ib,:)/bandcount(ib))
  end do

  write(*,'(A,A)') 'snapshot: ', trim(snapshot)
  write(*,'(A,I0,A,I0)') 'grid: ', Nx, ' x ', Nz
  write(*,'(A,ES12.4,A,ES12.4,A,ES12.4,A,ES12.4)') 'domain: xmax=', xmax, &
       ' zmax=', zmax, ' dx=', dx, ' dz=', dz
  write(*,'(A,I0)') 'slicing_mode = ', slicing_mode
  write(*,'(A)') 'Exterior bands (physical points only; boundary rows excluded).'
  write(*,'(A)') 'Each entry is max|RHS_field| in the band; RMS is in the CSV-esque block below.'
  write(*,'(A)') ''

  ! Header, two lines so it stays printable.
  write(*,'(A,24(1X,A8))') 'band r-lo   r-hi   n    ', &
       (field_name(iv), iv=1,NVARS)
  do ib = 1, 9
    if (bandcount(ib) == 0) cycle
    write(*,'(I4,2F7.3,I6,2X,24ES9.2)') ib, redges(ib), redges(ib+1), &
         bandcount(ib), (bandmax(ib,iv), iv=1,NVARS)
  end do

  write(*,'(A)') ''
  write(*,'(A)') 'Global maxima and locations:'
  do iv = 1, NVARS
    q = 0.0_dp; ib = 0
    do nb = 1, 9
      if (bandmax(nb,iv) > q) then
        q = bandmax(nb,iv); ib = nb
      end if
    end do
    if (ib == 0) cycle
    write(*,'(A12,A,ES12.4,A,I0,A,I0,A,F7.3)') trim(field_name(iv)), &
         ' max=', q, ' at (', band_i(ib,iv), ',', band_k(ib,iv), &
         ') r=', sqrt(xg(band_i(ib,iv))**2 + zg(band_k(ib,iv))**2)
  end do

  write(*,'(A)') ''
  write(*,'(A)') 'RMS by band (columns are the same fields as above):'
  do ib = 1, 9
    if (bandcount(ib) == 0) cycle
    write(*,'(I4,2F7.3,I6,2X,24ES9.2)') ib, redges(ib), redges(ib+1), &
         bandcount(ib), (bandrms(ib,iv), iv=1,NVARS)
  end do

  write(*,'(A)') ''
  write(*,'(A)') 'Interpretation: a component that is flat in the band table and'
  write(*,'(A)') 'does not fall when the same snapshot is re-run at a finer grid is'
  write(*,'(A)') 'an operator/data defect, not ordinary truncation.  A component'
  write(*,'(A)') 'confined to the outermost band is a boundary effect.'

  ! The outer band is often set by the last one or two interior columns.
  ! Print them explicitly so a boundary-stencil artefact cannot be mistaken
  ! for a bulk residual.
  write(*,'(A)') ''
  write(*,'(A)') 'RHS at the last interior x-column, equatorial row:'
  write(*,'(A)') '   i    r        phi        gxx        Axx        Ayy        Azz'
  do i = max(2,Nx-4), Nx-1
    k = Nz/2
    if (excision_status(i,k) /= PHYSICAL) cycle
    write(*,'(I4,F8.3,5ES11.3)') i, sqrt(xg(i)**2+zg(k)**2), &
         rhs(i,k,IPHI), rhs(i,k,IGXX), rhs(i,k,IAXX), rhs(i,k,IAYY), rhs(i,k,IAZZ)
  end do

  ! Why the last interior columns can differ: print the raw 4th-order x
  ! difference on the same row.  A large derivative here with smooth stored
  ! values means the boundary column is inconsistent, not that the interior
  ! operator is wrong.
  write(*,'(A)') ''
  write(*,'(A)') 'x-stencil audit on the equatorial row (i=Nx-1 and Nx-2):'
  write(*,'(A)') '   i    d1x(phi)    d1x(gxx)    d1x(Axx)    d1x(gxz)    d1x(Axz)'
  do i = max(2,Nx-2), Nx-1
    k = Nz/2
    if (excision_status(i,k) /= PHYSICAL) cycle
    call compute_stencil(u, i, k, excision_status, S)
    write(*,'(I4,5ES12.3)') i, S%d1x(IPHI), S%d1x(IGXX), S%d1x(IAXX), &
         S%d1x(IGXZ), S%d1x(IAXZ)
  end do
  write(*,'(A)') ''
  write(*,'(A)') 'mixed/y audit on the equatorial row:'
  write(*,'(A)') '   i    d1y(Axx)   d2y(Axx)   d2xy(Axx)  d2xy(gxx)  d2xy(gxz)'
  do i = max(2,Nx-2), Nx-1
    k = Nz/2
    if (excision_status(i,k) /= PHYSICAL) cycle
    call compute_stencil(u, i, k, excision_status, S)
    write(*,'(I4,5ES12.3)') i, S%d1y(IAXX), S%d2y(IAXX), S%d2xy(IAXX), &
         S%d2xy(IGXX), S%d2xy(IGXZ)
  end do

contains

  ! Optional experiment: make the Gtilde boundary consistent with the
  ! interior before evaluating the RHS.  This calls the same interim closure
  ! now used by the production driver; it is not a characteristic condition.
  subroutine maybe_fill_gt_bc(state)
    real(dp), intent(inout) :: state(:,:,:)
    character(len=32) :: buf
    integer :: ios, fill

    fill = 0
    call get_environment_variable("GR_FILL_GT_BC", buf, status=ios)
    if (ios == 0) read(buf,*,iostat=ios) fill
    if (fill == 0) return

    call fill_conformal_connection_boundary(state)
    write(*,'(A)') ' NOTE: linearly extrapolated Gtilde boundary cells.'
  end subroutine maybe_fill_gt_bc

  subroutine read_snapshot(path, state)
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: state(:,:,:)
    integer(hid_t) :: fid, did
    integer(hsize_t), dimension(2) :: shape
    integer :: v, hstat
    shape = [int(Nx,hsize_t), int(Nz,hsize_t)]
    call h5open_f(hstat)
    call h5fopen_f(trim(path), H5F_ACC_RDONLY_F, fid, hstat)
    if (hstat /= 0) error stop 'could not open snapshot'
    do v = 1, NVARS
      call h5dopen_f(fid, trim(field_name(v)), did, hstat)
      if (hstat /= 0) error stop 'snapshot is missing a BSSN field'
      call h5dread_f(did, H5T_NATIVE_DOUBLE, state(:,:,v), shape, hstat)
      if (hstat /= 0) error stop 'could not read snapshot field'
      call h5dclose_f(did, hstat)
    end do
    call h5fclose_f(fid, hstat)
    call h5close_f(hstat)
  end subroutine read_snapshot

end program diagnose_rhs_snapshot
