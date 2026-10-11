!===============================================================================
! test_constraint_conv -- does the Hamiltonian constraint converge on EXACT
! Kerr-Schild initial data?
!
! Kerr-Schild is an exact vacuum solution, so R + (2/3)K^2 - A.A must vanish
! analytically.  Any residual is therefore pure numerical: second derivatives of
! an O(1) quantity at spacings comparable to M.  Measuring the observed order
! distinguishes a benign truncation error from a genuine bug in the geometry,
! the constraint assembly, or the initial data.
!
! Measured in a smooth annulus (default r in [3,5]) so the result reflects the
! exterior far from both the hole and the axis.
!
! Usage: test_constraint_conv [parfile] [rin] [rout] [refinements]
!===============================================================================
program test_constraint_conv
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: hamiltonian_constraint

  implicit none
  real(dp), allocatable :: u(:,:,:), H(:,:)
  character(len=256) :: par_file, buf
  real(dp) :: rin, rout, hmax, hrms, npts
  real(dp), allocatable :: hmaxes(:), rmses(:), dxs(:)
  integer :: nx0, nz0, nref, ir, imin, imax, kmin, kmax, i, k
  integer :: ios

  par_file = 'parameter_files/vacuum_kerr.par'
  rin = 3.0_dp; rout = 5.0_dp; nref = 4
  buf = ''
  call get_command_argument(1, par_file)
  call get_command_argument(2, buf); ios = 0; if (len_trim(buf) > 0) read(buf,*) rin
  call get_command_argument(3, buf); ios = 0; if (len_trim(buf) > 0) read(buf,*) rout
  call get_command_argument(4, buf); ios = 0; if (len_trim(buf) > 0) read(buf,*) nref

  allocate(hmaxes(nref), rmses(nref), dxs(nref))

  ! remember the coarsest requested resolution, then refine Nx,Nz together
  call init_params(trim(par_file))
  nx0 = Nx; nz0 = Nz

  write(*,'(A)') '============================================================'
  write(*,'(A,ES10.4,A,ES10.4)') ' Kerr M=', bh_mass, '  a=', bh_spin
  write(*,'(A,F8.3,A,F8.3,A,I0,A,I0)') ' annulus r in [', rin, ',', rout, &
       ']  base grid ', nx0, ' x ', nz0
  write(*,'(A)') '------------------------------------------------------------'

  do ir = 1, nref
    call init_params(trim(par_file))
    Nx = nx0*(2**(ir-1));  Nz = nz0*(2**(ir-1))
    dx = xmax/real(Nx-1, dp)
    dz = 2.0_dp*zmax/real(Nz-1, dp)
    dy = dx

    if (allocated(xg))  deallocate(xg)
    if (allocated(zg))  deallocate(zg)
    call setup_grid()
    call alloc_state(u, Nx, Nz)
    allocate(H(Nx,Nz))

    call set_kerr_schild_id(u)
    call compute_conformal_connections(u)
    call set_excision_background(u)
    call init_deriv_stencils()
    call excision_reset_geometry()
    call extrapolate_excised(u)
    call hamiltonian_constraint(u, H, imin, imax, kmin, kmax)

    hmax = 0.0_dp; hrms = 0.0_dp; npts = 0.0_dp
    do k = kmin, kmax
      do i = imin, imax
        if (sqrt(xg(i)**2 + zg(k)**2) < rin) cycle
        if (sqrt(xg(i)**2 + zg(k)**2) > rout) cycle
        hmax = max(hmax, abs(H(i,k)))
        hrms = hrms + H(i,k)**2
        npts = npts + 1.0_dp
      end do
    end do
    hrms = sqrt(hrms/max(npts,1.0_dp))

    dxs(ir)   = dx
    hmaxes(ir) = hmax
    rmses(ir)  = hrms
    write(*,'(A,I3,A,I5,A,I6,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3)') &
         ' ref ', ir, '  Nx=', Nx, '  Nz=', Nz, '  dx=', dx, &
         '  max|H|=', hmax, '  rms|H|=', hrms, '  pts=', npts

    deallocate(u, H)
  end do

  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' observed order (log2 of successive error ratios):'
  do ir = 2, nref
    if (hmaxes(ir-1) > 0.0_dp .and. hmaxes(ir) > 0.0_dp) then
      write(*,'(A,I2,A,F7.2,A,F7.2)') '  ', ir, '  max|H|: ', &
           log(hmaxes(ir-1)/hmaxes(ir))/log(2.0_dp), '   rms|H|: ', &
           log(rmses(ir-1)/rmses(ir))/log(2.0_dp)
    end if
  end do
  write(*,'(A)') '============================================================'

contains
end program test_constraint_conv