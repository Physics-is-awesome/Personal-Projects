! Re-solve a saved evolution snapshot with several independent horizon-fit
! orders. This separates surface-fit truncation from changes in the evolved
! geometry without rerunning the evolution.
program diagnose_horizon_snapshot
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state, IPHI, IK, ICONFMET, IATILDE
  use horizon_mod, only: horizon_t, horizon_init_grid, horizon_set_excision, &
                         horizon_adm_from_state, horizon_init, find_horizon
  use hdf5
  implicit none

  character(len=512) :: par_file, snapshot
  character(len=8), parameter :: field_name(NVARS) = [character(len=8) :: &
       'phi', 'gxx', 'gxy', 'gxz', 'gyy', 'gyz', 'gzz', 'K', &
       'Axx', 'Axy', 'Axz', 'Ayy', 'Ayz', 'Azz', &
       'Gamx', 'Gamy', 'Gamz', 'alpha', 'betax', 'betay', 'betaz', 'Bx', 'By', 'Bz']
  integer, parameter :: norders=6
  integer, parameter :: orders(norders) = [8, 10, 12, 16, 20, 24]
  real(dp), allocatable :: u(:,:,:)
  type(horizon_t) :: hz
  integer(HID_T) :: file_id, dset_id
  integer(HSIZE_T) :: dims(2)
  integer :: ierr, i, nl
  real(dp) :: rplus, mask_r

  call get_command_argument(1, par_file)
  call get_command_argument(2, snapshot)
  if (len_trim(par_file) == 0 .or. len_trim(snapshot) == 0) then
    write(*,'(A)') 'usage: diagnose_horizon_snapshot <parameter-file> <state.h5>'
    error stop 2
  end if

  call init_params(trim(par_file))
  call setup_grid()
  allocate(u(Nx,Nz,NVARS))
  dims = [int(Nx,HSIZE_T), int(Nz,HSIZE_T)]

  call h5open_f(ierr)
  call h5fopen_f(trim(snapshot), H5F_ACC_RDONLY_F, file_id, ierr)
  if (ierr /= 0) error stop 'could not open snapshot'
  do i = 1, NVARS
    call h5dopen_f(file_id, trim(field_name(i)), dset_id, ierr)
    if (ierr /= 0) then
      write(*,'(A,A)') 'missing snapshot field: ', trim(field_name(i))
      error stop 3
    end if
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, u(:,:,i), dims, ierr)
    if (ierr /= 0) error stop 'could not read snapshot field'
    call h5dclose_f(dset_id, ierr)
  end do
  call h5fclose_f(file_id, ierr)
  call h5close_f(ierr)

  call horizon_init_grid(xg(1), zg(1), dx, dz, Nx, Nz, zg(1) >= -1.0e-12_dp)
  mask_r = max(0.0_dp, r_excise - 2.0_dp*max(dx,dz))
  call horizon_set_excision(mask_r, bh_spin)
  call horizon_adm_from_state(u, IPHI, IK, ICONFMET, IATILDE, 1, 1)
  rplus = bh_mass + sqrt(max(0.0_dp,bh_mass**2-bh_spin**2))

  write(*,'(A)') '# fit-order  ierr  resid  area  dA/A_Kerr  M_irr  J  M_H  hmin  hmax'
  do i = 1, norders
    nl = orders(i)
    call horizon_init(hz, nl, 0.0_dp, .true.)
    call find_horizon(hz, rplus, ierr)
    if (ierr == 0) then
      write(*,'(I5,1X,I4,1X,8(ES16.8,1X))') nl, ierr, hz%resid, hz%area, &
           hz%area/(8.0_dp*acos(-1.0_dp)*bh_mass*rplus)-1.0_dp, &
           hz%m_irr, hz%j_spin, hz%m_h, hz%hmin, hz%hmax
    else
      write(*,'(I5,1X,I4,1X,A,ES16.8)') nl, ierr, 'rejected_rmin_ks=', hz%rmin_ks
    end if
  end do
end program diagnose_horizon_snapshot
