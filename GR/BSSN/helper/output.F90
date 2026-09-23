module output_mod
  !---------------------------------------------------------------------------
  ! output_mod.f90
  !
  ! Output routines for the gr_bssnok_mhd code family. Works for BOTH:
  !   - the vacuum driver (main.f90)            : metric only, no matter
  !   - the TOV / Kerr-TOV drivers (main_tov.f90, main_kerr_tov.f90): metric
  !     + GRHD conservatives + primitives
  ! via optional arguments -- pass hydro_cons/hydro_prim when you have them,
  ! omit them for the vacuum driver.
  !
  ! Provides:
  !   - output_init             : open output dir + diagnostics log
  !   - output_write_snapshot   : dump the full grid state to an ASCII file
  !   - output_write_diagnostics: append one line of scalar health metrics
  !   - output_finalize         : close the diagnostics log
  !
  ! DESIGN NOTE: self-contained on purpose. It does not "use" vars_mod /
  ! grid_mod / constraints_mod -- it takes the documented arrays as plain
  ! arguments, using the field order from README.md's "State layout and
  ! data ownership" section (also consistent with vars_mod's IALPHA=18,
  ! the one named index actually visible in main.f90). If your vars_mod
  ! exports the other 23 index names too, you can swap the local I_*
  ! parameters below for "use vars_mod, only: ..." -- nothing else needs
  ! to change as long as the field order matches.
  !---------------------------------------------------------------------------
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

  integer :: diag_unit = -1
  logical :: diag_open = .false.
  logical :: diag_has_matter = .false.

  public :: output_init, output_write_snapshot, output_write_diagnostics, &
            output_finalize

contains

  !---------------------------------------------------------------------------
  subroutine output_init(out_dir, has_matter)
    ! Create the output directory and open the diagnostics log with a
    ! header matching the run type. has_matter=.false. (default) is for
    ! the vacuum driver (main.f90); pass .true. for main_tov/main_kerr_tov.
    character(len=*), intent(in)           :: out_dir
    logical,          intent(in), optional :: has_matter
    integer :: ios

    diag_has_matter = .false.
    if (present(has_matter)) diag_has_matter = has_matter

    call execute_command_line('mkdir -p '//trim(out_dir), exitstat=ios)
    if (ios /= 0) then
      write(*,*) 'output_mod: warning - mkdir -p ', trim(out_dir), &
                  ' returned nonzero status ', ios
    end if

    open(newunit=diag_unit, file=trim(out_dir)//'/diagnostics.dat', &
         status='replace', action='write', form='formatted')

    if (diag_has_matter) then
      write(diag_unit,'(A)') '# step  time  dt  rho_min  rho_max  '// &
           'alpha_min  alpha_max  phi_min  phi_max  ham_max  ham_rms  '// &
           'n_recovery_fail  n_nan_metric  n_nan_hydro'
    else
      write(diag_unit,'(A)') '# step  time  dt  '// &
           'alpha_min  alpha_max  phi_min  phi_max  ham_max  ham_rms  '// &
           'n_nan_metric'
    end if
    diag_open = .true.
  end subroutine output_init

  !---------------------------------------------------------------------------
  subroutine output_write_diagnostics(step, time, dt, Nx, Nz, metric, &
                                       ham_max, ham_rms, hydro_prim, &
                                       n_recovery_fail)
    ! Append one line of scalar run-health diagnostics. hydro_prim and
    ! n_recovery_fail are optional -- omit both for the vacuum driver.
    integer,  intent(in) :: step, Nx, Nz
    real(dp), intent(in) :: time, dt, ham_max, ham_rms
    real(dp), intent(in) :: metric(Nx,Nz,NVARS)
    real(dp), intent(in), optional :: hydro_prim(Nx,Nz,NPRIM)
    integer,  intent(in), optional :: n_recovery_fail

    integer :: n_nan_metric, n_nan_hydro, recov

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
      write(diag_unit, &
        '(I8,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,'// &
        '1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,I8,1X,I8,1X,I8)') &
        step, time, dt, &
        minval(hydro_prim(:,:,P_RHO)), maxval(hydro_prim(:,:,P_RHO)), &
        minval(metric(:,:,I_ALPHA)),   maxval(metric(:,:,I_ALPHA)), &
        minval(metric(:,:,I_PHI)),     maxval(metric(:,:,I_PHI)), &
        ham_max, ham_rms, recov, n_nan_metric, n_nan_hydro
    else
      write(diag_unit, &
        '(I8,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,1X,ES14.6,'// &
        '1X,ES14.6,1X,ES14.6,1X,I8)') &
        step, time, dt, &
        minval(metric(:,:,I_ALPHA)), maxval(metric(:,:,I_ALPHA)), &
        minval(metric(:,:,I_PHI)),   maxval(metric(:,:,I_PHI)), &
        ham_max, ham_rms, n_nan_metric
    end if

    flush(diag_unit)
  end subroutine output_write_diagnostics

  !---------------------------------------------------------------------------
  subroutine output_write_snapshot(step, time, out_dir, Nx, Nz, xg, zg, &
                                    metric, hydro_cons, hydro_prim)
    ! Write the full state at this step. hydro_cons/hydro_prim are
    ! optional -- omit both for the vacuum driver (metric-only columns).
    integer,          intent(in) :: step, Nx, Nz
    real(dp),         intent(in) :: time
    character(len=*), intent(in) :: out_dir
    real(dp),         intent(in) :: xg(Nx), zg(Nz)
    real(dp),         intent(in) :: metric(Nx,Nz,NVARS)
    real(dp),         intent(in), optional :: hydro_cons(Nx,Nz,NCONS)
    real(dp),         intent(in), optional :: hydro_prim(Nx,Nz,NPRIM)

    character(len=256) :: fname
    logical :: have_hydro
    integer :: unit, i, j

    have_hydro = present(hydro_cons) .and. present(hydro_prim)

    write(fname,'(A,"/state_",I6.6,".dat")') trim(out_dir), step
    open(newunit=unit, file=trim(fname), status='replace', &
         action='write', form='formatted')

    write(unit,'(A,I8,A,ES16.8)') '# step = ', step, '   time = ', time
    write(unit,'(A)') '# columns:'
    if (have_hydro) then
      write(unit,'(A)') '# i j x z '// &
        'phi gxx gxy gxz gyy gyz gzz K '// &
        'Axx Axy Axz Ayy Ayz Azz Gamx Gamy Gamz '// &
        'alpha betax betay betaz Bx By Bz '// &
        'D Sx Sy Sz tau '// &
        'rho vx vy vz eps'
    else
      write(unit,'(A)') '# i j x z '// &
        'phi gxx gxy gxz gyy gyz gzz K '// &
        'Axx Axy Axz Ayy Ayz Azz Gamx Gamy Gamz '// &
        'alpha betax betay betaz Bx By Bz'
    end if

    do j = 1, Nz
      do i = 1, Nx
        if (have_hydro) then
          write(unit,'(I5,1X,I5,1X,ES16.8,1X,ES16.8,36(1X,ES16.8))') &
            i, j, xg(i), zg(j), &
            metric(i,j,I_PHI), &
            metric(i,j,I_GXX), metric(i,j,I_GXY), metric(i,j,I_GXZ), &
            metric(i,j,I_GYY), metric(i,j,I_GYZ), metric(i,j,I_GZZ), &
            metric(i,j,I_K), &
            metric(i,j,I_AXX), metric(i,j,I_AXY), metric(i,j,I_AXZ), &
            metric(i,j,I_AYY), metric(i,j,I_AYZ), metric(i,j,I_AZZ), &
            metric(i,j,I_GAMX), metric(i,j,I_GAMY), metric(i,j,I_GAMZ), &
            metric(i,j,I_ALPHA), &
            metric(i,j,I_BETAX), metric(i,j,I_BETAY), metric(i,j,I_BETAZ), &
            metric(i,j,I_BX), metric(i,j,I_BY), metric(i,j,I_BZ), &
            hydro_cons(i,j,C_ID), hydro_cons(i,j,C_ISX), &
            hydro_cons(i,j,C_ISY), hydro_cons(i,j,C_ISZ), &
            hydro_cons(i,j,C_TAU), &
            hydro_prim(i,j,P_RHO), hydro_prim(i,j,P_VX), &
            hydro_prim(i,j,P_VY), hydro_prim(i,j,P_VZ), hydro_prim(i,j,P_EPS)
        else
          write(unit,'(I5,1X,I5,1X,ES16.8,1X,ES16.8,24(1X,ES16.8))') &
            i, j, xg(i), zg(j), &
            metric(i,j,I_PHI), &
            metric(i,j,I_GXX), metric(i,j,I_GXY), metric(i,j,I_GXZ), &
            metric(i,j,I_GYY), metric(i,j,I_GYZ), metric(i,j,I_GZZ), &
            metric(i,j,I_K), &
            metric(i,j,I_AXX), metric(i,j,I_AXY), metric(i,j,I_AXZ), &
            metric(i,j,I_AYY), metric(i,j,I_AYZ), metric(i,j,I_AZZ), &
            metric(i,j,I_GAMX), metric(i,j,I_GAMY), metric(i,j,I_GAMZ), &
            metric(i,j,I_ALPHA), &
            metric(i,j,I_BETAX), metric(i,j,I_BETAY), metric(i,j,I_BETAZ), &
            metric(i,j,I_BX), metric(i,j,I_BY), metric(i,j,I_BZ)
        end if
      end do
      write(unit,*)   ! blank line between z-rows (gnuplot pm3d convention)
    end do

    close(unit)
  end subroutine output_write_snapshot

  !---------------------------------------------------------------------------
  subroutine output_finalize()
    if (diag_open) then
      close(diag_unit)
      diag_open = .false.
    end if
  end subroutine output_finalize

  !---------------------------------------------------------------------------
  integer function count_nan_3d(arr) result(n)
    real(dp), intent(in) :: arr(:,:,:)
    n = count(arr /= arr)   ! IEEE NaN is the only value not equal to itself
  end function count_nan_3d

end module output_mod
