module output_mod
  !---------------------------------------------------------------------------
  ! output_mod.F90  (HDF5 version)
  !
  ! Output routines for the gr_bssnok_mhd code family. Works for BOTH:
  !   - the vacuum driver (main.f90)            : metric only, no matter
  !   - the TOV / Kerr-TOV drivers (main_tov.f90, main_kerr_tov.f90): metric
  !     + GRHD conservatives + primitives
  ! via optional arguments -- pass hydro_cons/hydro_prim when you have them,
  ! omit them for the vacuum driver.
  !
  ! Provides (same public interface as the .dat version -- drop-in swap,
  ! no changes needed at call sites):
  !   - output_init             : open output dir + diagnostics.h5
  !   - output_write_snapshot   : dump the full grid state to state_NNNNNN.h5
  !   - output_write_diagnostics: append one row of scalar health metrics
  !   - output_finalize         : close the diagnostics file + HDF5 library
  !
  ! FILE LAYOUT
  !   <out_dir>/diagnostics.h5
  !     dataset "diagnostics", shape (ncols, nrows), nrows extendible.
  !     attribute "columns" on that dataset: space-separated column names,
  !     in the same order as the rows (step is column 1, time column 2, ...).
  !
  !   <out_dir>/state_NNNNNN.h5   (one file per snapshot, NNNNNN = step)
  !     file attributes: step (int), time (double), Nx, Nz, have_hydro
  !     dataset "x" (Nx), "z" (Nz)                 -- grid coordinates
  !     datasets "phi","gxx",...,"Bz"  (Nx,Nz)     -- 24 BSSN fields
  !     datasets "D","Sx","Sy","Sz","tau" (Nx,Nz)  -- GRHD conservatives (matter runs only)
  !     datasets "rho","vx","vy","vz","eps" (Nx,Nz)-- GRHD primitives    (matter runs only)
  !
  !   Quick read in Python (h5py):
  !     import h5py
  !     f = h5py.File('output/diagnostics.h5','r')
  !     data = f['diagnostics'][:]                       # shape (ncols, nrows)
  !     cols = f['diagnostics'].attrs['columns'].split()
  !     time = data[cols.index('time'), :]
  !
  !     g = h5py.File('output/state_000010.h5','r')
  !     phi = g['phi'][:]                                # shape (Nz,Nx) --
  !                                                       # see the note below
  !     x, z = g['x'][:], g['z'][:]
  !
  !   Note on array shape: the HDF5 Fortran API stores multi-dimensional
  !   arrays with the dimension order reversed relative to how you declare
  !   them in Fortran (this is documented, standard HDF5 Fortran behavior,
  !   not a bug here) -- an arr(Nx,Nz) written from Fortran ends up with
  !   on-disk/h5py-visible shape (Nz,Nx). The XDMF Dimensions below already
  !   account for this by listing Nz before Nx.
  !
  !   <out_dir>/simulation.xmf
  !     XDMF/XML time-series description tying every state_NNNNNN.h5 file
  !     together into one animatable dataset. One <Grid> block is appended
  !     per output_write_snapshot call, inside a temporal <Grid> collection;
  !     output_finalize writes the closing tags. To animate in ParaView:
  !     File > Open > simulation.xmf, click Apply, then use the time
  !     controls (Play / the frame slider) to step or animate through
  !     snapshots. simulation.xmf references the state_*.h5 files by
  !     relative path, so keep it in the same directory as they are (it
  !     already is, by construction -- both go in out_dir).
  !
  ! BUILD NOTES
  !   Needs the HDF5 Fortran bindings (module "hdf5"). Easiest is to compile
  !   with the HDF5 wrapper compiler, e.g.:
  !     h5fc -O2 -c output_mod.F90
  !     h5fc -O2 your_other_files.o output_mod.o -o your_program
  !   or, with plain gfortran, add the include dir and link both libraries
  !   (paths vary by distro/install, e.g. Ubuntu's libhdf5-fortran-dev):
  !     gfortran -I/usr/include/hdf5/serial -c output_mod.F90
  !     gfortran your_other_files.o output_mod.o \
  !       -L/usr/lib/x86_64-linux-gnu/hdf5/serial -lhdf5_fortran -lhdf5 -o your_program
  !
  ! DESIGN NOTE: self-contained on purpose, same as the .dat version. It does
  ! not "use" vars_mod / grid_mod / constraints_mod -- it takes the documented
  ! arrays as plain arguments, using the field order from README.md's "State
  ! layout and data ownership" section (also consistent with vars_mod's
  ! IALPHA=18, the one named index actually visible in main.f90). If your
  ! vars_mod exports the other 23 index names too, you can swap the local I_*
  ! parameters below for "use vars_mod, only: ..." -- nothing else needs to
  ! change as long as the field order matches.
  !---------------------------------------------------------------------------
  use hdf5
  implicit none
  private

  integer, parameter :: dp = kind(1.0d0)

  ! --- Metric/gauge array indices (matches README + vars_mod's IALPHA=18) --
  integer, parameter :: I_PHI   = 1
  integer, parameter :: I_GXX   = 2, I_GXY = 3, I_GXZ = 4
  integer, parameter :: I_GYY   = 5, I_GYZ = 6, I_GZZ = 7
  integer, parameter :: I_K     = 8
  integer, parameter :: I_AXX   = 9,  I_AXY = 10, I_AXZ = 11
  integer, parameter :: I_AYY   = 12, I_AYZ = 13, I_AZZ = 14
  integer, parameter :: I_GAMX  = 15, I_GAMY = 16, I_GAMZ = 17
  integer, parameter :: I_ALPHA = 18   ! matches vars_mod::IALPHA
  integer, parameter :: I_BETAX = 19, I_BETAY = 20, I_BETAZ = 21
  integer, parameter :: I_BX    = 22, I_BY = 23, I_BZ = 24
  integer, parameter :: NVARS   = 24

  ! --- GRHD conservative array indices (shape (Nx,Nz,5), matter runs only) -
  integer, parameter :: C_ID  = 1, C_ISX = 2, C_ISY = 3, C_ISZ = 4, C_TAU = 5
  integer, parameter :: NCONS = 5

  ! --- Primitive array indices (matter runs only) ---------------------------
  integer, parameter :: P_RHO = 1, P_VX = 2, P_VY = 3, P_VZ = 4, P_EPS = 5
  integer, parameter :: NPRIM = 5

  ! Dataset names for each field, in the same order as the I_*/C_*/P_*
  ! indices above -- used when looping to write snapshot datasets.
  character(len=8), parameter :: METRIC_NAMES(NVARS) = (/ &
       'phi     ', 'gxx     ', 'gxy     ', 'gxz     ', &
       'gyy     ', 'gyz     ', 'gzz     ', 'K       ', &
       'Axx     ', 'Axy     ', 'Axz     ', 'Ayy     ', &
       'Ayz     ', 'Azz     ', 'Gamx    ', 'Gamy    ', &
       'Gamz    ', 'alpha   ', 'betax   ', 'betay   ', &
       'betaz   ', 'Bx      ', 'By      ', 'Bz      ' /)
  character(len=8), parameter :: CONS_NAMES(NCONS) = (/ &
       'D       ', 'Sx      ', 'Sy      ', 'Sz      ', 'tau     ' /)
  character(len=8), parameter :: PRIM_NAMES(NPRIM) = (/ &
       'rho     ', 'vx      ', 'vy      ', 'vz      ', 'eps     ' /)

  integer(HSIZE_T), parameter :: DIAG_CHUNK_ROWS = 64_HSIZE_T

  logical :: hdf5_lib_open = .false.

  ! --- diagnostics HDF5 state (mirrors the old diag_unit/diag_open) --------
  integer(HID_T)   :: diag_file_id, diag_dset_id, diag_dcpl_id
  integer          :: diag_ncols = 0
  integer(HSIZE_T) :: diag_nrows = 0
  logical          :: diag_open = .false.
  logical          :: diag_has_matter = .false.

  ! --- XDMF time-series description (for ParaView animation) --------------
  integer :: xdmf_unit = -1
  logical :: xdmf_open = .false.

  public :: output_init, output_write_snapshot, output_write_diagnostics, &
            output_finalize

contains

  !---------------------------------------------------------------------------
  subroutine output_init(out_dir, has_matter)
    ! Create the output directory and open diagnostics.h5 with an
    ! extendible "diagnostics" dataset matching the run type. has_matter
    ! =.false. (default) is for the vacuum driver (main.f90); pass .true.
    ! for main_tov/main_kerr_tov.
    character(len=*), intent(in)           :: out_dir
    logical,          intent(in), optional :: has_matter

    integer            :: ios, hdferr
    integer(HSIZE_T)   :: dims(2), maxdims(2), chunk(2)
    integer(HID_T)     :: dspace_id
    character(len=512) :: colnames

    diag_has_matter = .false.
    if (present(has_matter)) diag_has_matter = has_matter

    call execute_command_line('mkdir -p '//trim(out_dir), exitstat=ios)
    if (ios /= 0) then
      write(*,*) 'output_mod: warning - mkdir -p ', trim(out_dir), &
                  ' returned nonzero status ', ios
    end if

    if (.not. hdf5_lib_open) then
      call h5open_f(hdferr)
      hdf5_lib_open = .true.
    end if

    if (diag_has_matter) then
      diag_ncols = 14
      colnames = 'step time dt rho_min rho_max alpha_min alpha_max '// &
                 'phi_min phi_max ham_max ham_rms n_recovery_fail '// &
                 'n_nan_metric n_nan_hydro'
    else
      diag_ncols = 10
      colnames = 'step time dt alpha_min alpha_max phi_min phi_max '// &
                 'ham_max ham_rms n_nan_metric'
    end if

    call h5fcreate_f(trim(out_dir)//'/diagnostics.h5', H5F_ACC_TRUNC_F, &
                      diag_file_id, hdferr)

    ! Extendible 2D dataset: (ncols, nrows), nrows unlimited so we can
    ! append one column-vector (one row of diagnostics) per step.
    dims    = (/ int(diag_ncols, HSIZE_T), 0_HSIZE_T /)
    maxdims = (/ int(diag_ncols, HSIZE_T), H5S_UNLIMITED_F /)
    chunk   = (/ int(diag_ncols, HSIZE_T), DIAG_CHUNK_ROWS /)

    call h5screate_simple_f(2, dims, dspace_id, hdferr, maxdims)
    call h5pcreate_f(H5P_DATASET_CREATE_F, diag_dcpl_id, hdferr)
    call h5pset_chunk_f(diag_dcpl_id, 2, chunk, hdferr)

    call h5dcreate_f(diag_file_id, 'diagnostics', H5T_NATIVE_DOUBLE, &
                      dspace_id, diag_dset_id, hdferr, diag_dcpl_id)
    call h5sclose_f(dspace_id, hdferr)

    call write_str_attr(diag_dset_id, 'columns', trim(colnames))
    call write_str_attr(diag_dset_id, 'layout', &
         'shape is (ncols, nrows); each row index is a time series, '// &
         'column order matches the columns attribute')

    diag_nrows = 0
    diag_open  = .true.

    ! -- XDMF time-series header (plain text/XML, not HDF5) -----------------
    open(newunit=xdmf_unit, file=trim(out_dir)//'/simulation.xmf', &
         status='replace', action='write', form='formatted')
    write(xdmf_unit,'(A)') '<?xml version="1.0" ?>'
    write(xdmf_unit,'(A)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
    write(xdmf_unit,'(A)') '<Xdmf Version="2.0">'
    write(xdmf_unit,'(A)') '  <Domain>'
    write(xdmf_unit,'(A)') '    <Grid Name="TimeSeries" GridType="Collection" '// &
                            'CollectionType="Temporal">'
    xdmf_open = .true.
  end subroutine output_init

  !---------------------------------------------------------------------------
  subroutine output_write_diagnostics(step, time, dt, Nx, Nz, metric, &
                                       ham_max, ham_rms, hydro_prim, &
                                       n_recovery_fail)
    ! Append one row of scalar run-health diagnostics. hydro_prim and
    ! n_recovery_fail are optional -- omit both for the vacuum driver.
    integer,  intent(in) :: step, Nx, Nz
    real(dp), intent(in) :: time, dt, ham_max, ham_rms
    real(dp), intent(in) :: metric(Nx,Nz,NVARS)
    real(dp), intent(in), optional :: hydro_prim(Nx,Nz,NPRIM)
    integer,  intent(in), optional :: n_recovery_fail

    integer          :: n_nan_metric, n_nan_hydro, recov, hdferr
    real(dp)         :: row(diag_ncols)
    integer(HSIZE_T) :: newdims(2), start(2), cnt(2)
    integer(HID_T)   :: fspace_id, mspace_id

    if (.not. diag_open) then
      write(*,*) 'output_mod: output_write_diagnostics called before ', &
                  'output_init -- skipping this line'
      return
    end if

    n_nan_metric = count_nan_3d(metric)

    if (diag_has_matter .and. present(hydro_prim)) then
      n_nan_hydro = count_nan_3d(hydro_prim)
      recov = 0
      if (present(n_recovery_fail)) recov = n_recovery_fail
      row = (/ real(step,dp), time, dt, &
               minval(hydro_prim(:,:,P_RHO)), maxval(hydro_prim(:,:,P_RHO)), &
               minval(metric(:,:,I_ALPHA)),   maxval(metric(:,:,I_ALPHA)), &
               minval(metric(:,:,I_PHI)),     maxval(metric(:,:,I_PHI)), &
               ham_max, ham_rms, real(recov,dp), real(n_nan_metric,dp), &
               real(n_nan_hydro,dp) /)
    else
      row = (/ real(step,dp), time, dt, &
               minval(metric(:,:,I_ALPHA)), maxval(metric(:,:,I_ALPHA)), &
               minval(metric(:,:,I_PHI)),   maxval(metric(:,:,I_PHI)), &
               ham_max, ham_rms, real(n_nan_metric,dp) /)
    end if

    ! Extend the dataset by one row, select that row, write it.
    diag_nrows = diag_nrows + 1
    newdims = (/ int(diag_ncols, HSIZE_T), diag_nrows /)
    call h5dset_extent_f(diag_dset_id, newdims, hdferr)

    call h5dget_space_f(diag_dset_id, fspace_id, hdferr)
    start = (/ 0_HSIZE_T, diag_nrows - 1_HSIZE_T /)
    cnt   = (/ int(diag_ncols, HSIZE_T), 1_HSIZE_T /)
    call h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, cnt, hdferr)

    call h5screate_simple_f(1, (/ int(diag_ncols, HSIZE_T) /), mspace_id, hdferr)
    call h5dwrite_f(diag_dset_id, H5T_NATIVE_DOUBLE, row, &
                    (/ int(diag_ncols, HSIZE_T) /), hdferr, mspace_id, fspace_id)

    call h5sclose_f(mspace_id, hdferr)
    call h5sclose_f(fspace_id, hdferr)

    call h5fflush_f(diag_file_id, H5F_SCOPE_LOCAL_F, hdferr)
  end subroutine output_write_diagnostics

  !---------------------------------------------------------------------------
  subroutine output_write_snapshot(step, time, out_dir, Nx, Nz, xg, zg, &
                                    metric, hydro_cons, hydro_prim)
    ! Write the full state at this step to <out_dir>/state_NNNNNN.h5.
    ! hydro_cons/hydro_prim are optional -- omit both for the vacuum
    ! driver (metric-only datasets).
    integer,          intent(in) :: step, Nx, Nz
    real(dp),         intent(in) :: time
    character(len=*), intent(in) :: out_dir
    real(dp),         intent(in) :: xg(Nx), zg(Nz)
    real(dp),         intent(in) :: metric(Nx,Nz,NVARS)
    real(dp),         intent(in), optional :: hydro_cons(Nx,Nz,NCONS)
    real(dp),         intent(in), optional :: hydro_prim(Nx,Nz,NPRIM)

    character(len=256) :: fname
    character(len=32)  :: state_basename
    logical             :: have_hydro
    integer(HID_T)      :: file_id
    integer              :: hdferr, k

    have_hydro = present(hydro_cons) .and. present(hydro_prim)

    if (.not. hdf5_lib_open) then
      call h5open_f(hdferr)
      hdf5_lib_open = .true.
    end if

    write(state_basename,'("state_",I6.6,".h5")') step
    fname = trim(out_dir)//'/'//trim(state_basename)
    call h5fcreate_f(trim(fname), H5F_ACC_TRUNC_F, file_id, hdferr)

    call write_int_attr(file_id, 'step', step)
    call write_dbl_attr(file_id, 'time', time)
    call write_int_attr(file_id, 'Nx', Nx)
    call write_int_attr(file_id, 'Nz', Nz)
    call write_int_attr(file_id, 'have_hydro', merge(1, 0, have_hydro))

    call write_dataset_1d(file_id, 'x', xg, Nx)
    call write_dataset_1d(file_id, 'z', zg, Nz)

    do k = 1, NVARS
      call write_dataset_2d(file_id, trim(METRIC_NAMES(k)), &
                             metric(:,:,k), Nx, Nz)
    end do

    if (have_hydro) then
      do k = 1, NCONS
        call write_dataset_2d(file_id, trim(CONS_NAMES(k)), &
                               hydro_cons(:,:,k), Nx, Nz)
      end do
      do k = 1, NPRIM
        call write_dataset_2d(file_id, trim(PRIM_NAMES(k)), &
                               hydro_prim(:,:,k), Nx, Nz)
      end do
    end if

    call h5fclose_f(file_id, hdferr)

    ! -- append this step's <Grid> to simulation.xmf, referencing the -------
    ! -- datasets we just wrote (relative path -- .xmf lives in out_dir) ----
    if (xdmf_open) call append_xdmf_grid(state_basename, time, Nx, Nz, have_hydro)
  end subroutine output_write_snapshot

  !---------------------------------------------------------------------------
  ! -- XDMF helpers: append one time-step's <Grid> block to simulation.xmf --

  subroutine append_xdmf_grid(basename, time, Nx, Nz, have_hydro)
    character(len=*), intent(in) :: basename
    real(dp),         intent(in) :: time
    integer,          intent(in) :: Nx, Nz
    logical,          intent(in) :: have_hydro

    character(len=16) :: nx_str, nz_str, time_str
    integer :: k

    write(nx_str,'(I0)')       Nx
    write(nz_str,'(I0)')       Nz
    write(time_str,'(ES16.8)') time
    time_str = adjustl(time_str)

    write(xdmf_unit,'(A)') '      <Grid Name="step" GridType="Uniform">'
    write(xdmf_unit,'(A)') '        <Time Value="'//trim(time_str)//'"/>'
    ! Dimensions="Nz Nx" (not "Nx Nz") -- see the array-shape note near the
    ! top of this file re: the HDF5 Fortran dimension-order reversal.
    write(xdmf_unit,'(A)') '        <Topology TopologyType="2DRectMesh" '// &
         'Dimensions="'//trim(nz_str)//' '//trim(nx_str)//'"/>'
    write(xdmf_unit,'(A)') '        <Geometry GeometryType="VXVY">'
    write(xdmf_unit,'(A)') '          <DataItem Dimensions="'//trim(nx_str)// &
         '" NumberType="Float" Precision="8" Format="HDF">'// &
         trim(basename)//':/x</DataItem>'
    write(xdmf_unit,'(A)') '          <DataItem Dimensions="'//trim(nz_str)// &
         '" NumberType="Float" Precision="8" Format="HDF">'// &
         trim(basename)//':/z</DataItem>'
    write(xdmf_unit,'(A)') '        </Geometry>'

    do k = 1, NVARS
      call write_xdmf_attribute(trim(METRIC_NAMES(k)), basename, nx_str, nz_str)
    end do
    if (have_hydro) then
      do k = 1, NCONS
        call write_xdmf_attribute(trim(CONS_NAMES(k)), basename, nx_str, nz_str)
      end do
      do k = 1, NPRIM
        call write_xdmf_attribute(trim(PRIM_NAMES(k)), basename, nx_str, nz_str)
      end do
    end if

    write(xdmf_unit,'(A)') '      </Grid>'
    flush(xdmf_unit)
  end subroutine append_xdmf_grid

  subroutine write_xdmf_attribute(name, basename, nx_str, nz_str)
    ! One <Attribute> block for a single field, e.g. name='phi'. nx_str/
    ! nz_str are pre-formatted (trimmed) grid-size strings from the caller.
    character(len=*), intent(in) :: name, basename, nx_str, nz_str

    write(xdmf_unit,'(A)') '        <Attribute Name="'//trim(name)// &
         '" AttributeType="Scalar" Center="Node">'
    write(xdmf_unit,'(A)') '          <DataItem Dimensions="'//trim(nz_str)// &
         ' '//trim(nx_str)//'" NumberType="Float" Precision="8" '// &
         'Format="HDF">'//trim(basename)//':/'//trim(name)//'</DataItem>'
    write(xdmf_unit,'(A)') '        </Attribute>'
  end subroutine write_xdmf_attribute

  !---------------------------------------------------------------------------
  subroutine output_finalize()
    integer :: hdferr
    if (diag_open) then
      call h5pclose_f(diag_dcpl_id, hdferr)
      call h5dclose_f(diag_dset_id, hdferr)
      call h5fclose_f(diag_file_id, hdferr)
      diag_open = .false.
    end if
    if (hdf5_lib_open) then
      call h5close_f(hdferr)
      hdf5_lib_open = .false.
    end if
    if (xdmf_open) then
      write(xdmf_unit,'(A)') '    </Grid>'
      write(xdmf_unit,'(A)') '  </Domain>'
      write(xdmf_unit,'(A)') '</Xdmf>'
      close(xdmf_unit)
      xdmf_open = .false.
    end if
  end subroutine output_finalize

  !---------------------------------------------------------------------------
  integer function count_nan_3d(arr) result(n)
    real(dp), intent(in) :: arr(:,:,:)
    n = count(arr /= arr)   ! IEEE NaN is the only value not equal to itself
  end function count_nan_3d

  !---------------------------------------------------------------------------
  ! -- small HDF5 helpers, used by output_write_snapshot / output_init ------

  subroutine write_dataset_1d(loc_id, name, arr, N)
    integer(HID_T),   intent(in) :: loc_id
    character(len=*), intent(in) :: name
    integer,          intent(in) :: N
    real(dp),         intent(in) :: arr(N)

    integer(HID_T)   :: dspace_id, dset_id
    integer(HSIZE_T) :: dims(1)
    integer          :: hdferr

    dims = (/ int(N, HSIZE_T) /)
    call h5screate_simple_f(1, dims, dspace_id, hdferr)
    call h5dcreate_f(loc_id, name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferr)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine write_dataset_1d

  subroutine write_dataset_2d(loc_id, name, arr, Nx, Nz)
    integer(HID_T),   intent(in) :: loc_id
    character(len=*), intent(in) :: name
    integer,          intent(in) :: Nx, Nz
    real(dp),         intent(in) :: arr(Nx,Nz)

    integer(HID_T)   :: dspace_id, dset_id
    integer(HSIZE_T) :: dims(2)
    integer          :: hdferr

    dims = (/ int(Nx, HSIZE_T), int(Nz, HSIZE_T) /)
    call h5screate_simple_f(2, dims, dspace_id, hdferr)
    call h5dcreate_f(loc_id, name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferr)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine write_dataset_2d

  subroutine write_int_attr(loc_id, name, val)
    integer(HID_T),   intent(in) :: loc_id
    character(len=*), intent(in) :: name
    integer,          intent(in) :: val

    integer(HID_T)   :: aspace_id, attr_id
    integer(HSIZE_T) :: dims(1)
    integer          :: hdferr, buf(1)

    dims(1) = 1_HSIZE_T
    buf(1)  = val
    call h5screate_f(H5S_SCALAR_F, aspace_id, hdferr)
    call h5acreate_f(loc_id, name, H5T_NATIVE_INTEGER, aspace_id, attr_id, hdferr)
    call h5awrite_f(attr_id, H5T_NATIVE_INTEGER, buf, dims, hdferr)
    call h5aclose_f(attr_id, hdferr)
    call h5sclose_f(aspace_id, hdferr)
  end subroutine write_int_attr

  subroutine write_dbl_attr(loc_id, name, val)
    integer(HID_T),   intent(in) :: loc_id
    character(len=*), intent(in) :: name
    real(dp),         intent(in) :: val

    integer(HID_T)   :: aspace_id, attr_id
    integer(HSIZE_T) :: dims(1)
    integer          :: hdferr
    real(dp)         :: buf(1)

    dims(1) = 1_HSIZE_T
    buf(1)  = val
    call h5screate_f(H5S_SCALAR_F, aspace_id, hdferr)
    call h5acreate_f(loc_id, name, H5T_NATIVE_DOUBLE, aspace_id, attr_id, hdferr)
    call h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, buf, dims, hdferr)
    call h5aclose_f(attr_id, hdferr)
    call h5sclose_f(aspace_id, hdferr)
  end subroutine write_dbl_attr

  subroutine write_str_attr(loc_id, name, str)
    integer(HID_T),   intent(in) :: loc_id
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: str

    integer(HID_T)   :: aspace_id, attr_id, atype_id
    integer(HSIZE_T) :: dims(1)
    integer(SIZE_T)  :: str_len
    integer          :: hdferr

    dims(1) = 1_HSIZE_T
    str_len = len(str)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, hdferr)
    call h5tset_size_f(atype_id, str_len, hdferr)
    call h5screate_f(H5S_SCALAR_F, aspace_id, hdferr)
    call h5acreate_f(loc_id, name, atype_id, aspace_id, attr_id, hdferr)
    call h5awrite_f(attr_id, atype_id, str, dims, hdferr)
    call h5aclose_f(attr_id, hdferr)
    call h5sclose_f(aspace_id, hdferr)
    call h5tclose_f(atype_id, hdferr)
  end subroutine write_str_attr

end module output_mod
