!===============================================================================
! test_momentum_conv -- validates constraints_mod::momentum_constraint against
! EXACT Kerr-Schild data under 2x refinement, exactly as test_constraint_conv
! does for the Hamiltonian constraint.
!
! Kerr-Schild is an exact vacuum solution, so the momentum constraint vanishes
! analytically and any residual is pure truncation error.  A flat (order ~0)
! result means the constraint expression is wrong, not under-resolved.
!
! Formulation: Brown et al., Phys. Rev. D 85, 084004 (2012), arXiv:1202.1038,
! eq. (2.19b).
!
! Usage: test_momentum_conv [parfile] [rin] [rout] [refinements]
!===============================================================================
program test_momentum_conv
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
  real(dp), allocatable :: u(:,:,:), M(:,:,:)
  real(dp), allocatable :: mmax(:), mrms(:)
  character(len=256) :: par_file, buf
  real(dp) :: rin, rout
  integer :: nx0, nz0, nref, ir, i, k
  real(dp) :: mx, rms, npts, rr

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

  allocate(mmax(nref), mrms(nref))
  call init_params(trim(par_file))
  nx0 = Nx; nz0 = Nz

  write(*,'(A)') '============================================================'
  write(*,'(A,ES10.4,A,ES10.4)') ' Kerr M=', bh_mass, '  a=', bh_spin
  write(*,'(A,F8.3,A,F8.3)') ' annulus r in [', rin, ',', rout, ']'
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
    if (allocated(u))  deallocate(u)
    if (allocated(M))  deallocate(M)
    call alloc_state(u, Nx, Nz)
    allocate(M(Nx,Nz,3))

    call set_kerr_schild_id(u)
    call compute_conformal_connections(u)
    call set_excision_background(u)
    call init_deriv_stencils()
    call excision_reset_geometry()
    call extrapolate_excised(u)
    call momentum_constraint(u, M)

    mx = 0.0_dp; rms = 0.0_dp; npts = 0.0_dp
    do k = 3, Nz-2
      do i = 2, Nx-1
        rr = sqrt(xg(i)**2 + zg(k)**2)
        if (rr < rin .or. rr > rout) cycle
        mx = max(mx, sqrt(M(i,k,1)**2 + M(i,k,2)**2 + M(i,k,3)**2))
        rms = rms + M(i,k,1)**2 + M(i,k,2)**2 + M(i,k,3)**2
        npts = npts + 1.0_dp
      end do
    end do
    rms = sqrt(rms/(3.0_dp*max(npts,1.0_dp)))
    mmax(ir) = mx;  mrms(ir) = rms
    write(*,'(A,I3,A,I5,A,I6,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3)') ' ref ', ir, &
         '  Nx=', Nx, '  Nz=', Nz, '  dx=', dx, '  max|M|=', mx, &
         '  rms|M|=', rms, '  pts=', npts
  end do

  write(*,'(A)') '------------------------------------------------------------'
  write(*,'(A)') ' observed order (log2 of successive error ratios):'
  do ir = 2, nref
    if (mrms(ir-1) > 0.0_dp .and. mrms(ir) > 0.0_dp) then
      write(*,'(A,I2,A,F7.2,A,F7.2)') '  ', ir, '  max|M|: ', &
           log(mmax(ir-1)/mmax(ir))/log(2.0_dp), '   rms|M|: ', &
           log(mrms(ir-1)/mrms(ir))/log(2.0_dp)
    end if
  end do
  write(*,'(A)') '============================================================'

contains
end program test_momentum_conv