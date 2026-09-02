!=======================================================================
!  main_sgeuler : driver for the well-balanced total-energy-conserving
!                 LDG scheme of Pan, Chen, Qiu & Xiong (2026).
!
!  Build (2D):
!     gfortran -O2 -cpp -DNDIM=2 mod_params.F90 mod_quadrature.f90 \
!         mod_mesh.f90 mod_basis.f90 mod_physics.f90 mod_riemann.f90 \
!         mod_poisson.f90 mod_dg.f90 mod_limiter.f90 mod_timeint.f90 \
!         mod_output.f90 main_sgeuler.F90 -o sgeuler2d
!
!  Build (3D): replace -DNDIM=2 by -DNDIM=3.
!  Run:   ./sgeuler2d input.nml
!=======================================================================
program main_sgeuler
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics
  use mod_riemann
  use mod_poisson
  use mod_dg
  use mod_limiter
  use mod_timeint
  use mod_output
  implicit none

  real(dp), allocatable :: Y(:,:,:), U(:,:,:)
  real(dp) :: t, dt, m0, e0, tdump, dtd, e
  real(dp) :: e1, e2, ei
  integer  :: istep, idump, nargs, iv
  character(len=256) :: fname

  !--- 1. input ------------------------------------------------------
  nargs = command_argument_count()
  if (nargs >= 1) then
     call get_command_argument(1, fname)
  else
     fname = 'input.nml'
  end if
  call read_input(trim(fname))
  call setup_case()

  !--- 2. discretisation ---------------------------------------------
  call build_mesh()
  call build_basis()
  call poisson_init()
  call dg_init()
  call ti_alloc()

  allocate(Y(nb,nvar,ncell), U(nb,nvar,ncell))

  call write_header()
  write(*,'(a,i0,a,i0,a,i0)') '  modes/cell = ', nb,                     &
       '   volume q-points = ', nqv, '   face q-points = ', nqf
  write(*,'(a)') '----------------------------------------------------------------------'

  !--- 3. frozen discrete equilibrium (Sec. 3.4) ---------------------
  call dg_equilibrium()

  !--- 4. initial data: (3.27) and, for 'sp', (3.28) -----------------
  call dg_project_ic(Y)

  call dg_recover_U(Y, U)
  m0 = total_mass(Y)
  e0 = total_energy(Y)
  write(*,'(a,es24.16)') '  initial mass         = ', m0
  write(*,'(a,es24.16)') '  initial total energy = ', e0
  write(*,'(a)') '----------------------------------------------------------------------'

  call open_energy()
  call log_energy(zero, e0, e0)

  !--- 5. time loop --------------------------------------------------
  t     = zero
  istep = 0
  idump = 0
  dtd   = tfinal
  if (nout > 0) dtd = tfinal/real(nout,dp)
  tdump = dtd

  do
     if (t >= tfinal*(one - 1.0e-14_dp)) exit
     dt = ti_dt(Y)
     if (t + dt > tfinal) dt = tfinal - t
     if (nout > 0 .and. t + dt > tdump) dt = tdump - t

     call ti_step(Y, t, dt)
     t     = t + dt
     istep = istep + 1

     e = total_energy(Y)
     call log_energy(t, e, e0)

     if (verbose .and. mod(istep,20) == 0) then
        write(*,'(a,i7,a,es12.5,a,es10.3,a,i6,a,es9.2,a,es11.4)')         &
             '  step ', istep, '  t = ', t, '  dt = ', dt,                &
             '  CG it = ', poisson_iters, '  res = ', poisson_resid,      &
             '  dE = ', e - e0
     end if

     if (nout > 0 .and. t >= tdump*(one - 1.0e-14_dp)) then
        idump = idump + 1
        call dg_recover_U(Y, U)
        call write_solution(U, t, itoa(idump))
        tdump = tdump + dtd
     end if
  end do

  !--- 6. final diagnostics ------------------------------------------
  write(*,'(a)') '----------------------------------------------------------------------'
  write(*,'(a,i0,a,es16.8)') '  steps = ', istep, '   final time = ', t
  call diagnostics(Y, t, m0, e0)

  call dg_recover_U(Y, U)

  write(*,'(a)') '  errors of U against the exact / equilibrium data:'
  call errors(Y, t, IRHO, e1, e2, ei)
  write(*,'(a,3es15.6)') '    rho    : L1,L2,Linf = ', e1, e2, ei
  do iv = 1, nd
     call errors(Y, t, IMX+iv-1, e1, e2, ei)
     write(*,'(a,i1,a,3es15.6)') '    mom',iv,'   : L1,L2,Linf = ', e1, e2, ei
  end do
  call errors(Y, t, IEN, e1, e2, ei)
  write(*,'(a,3es15.6)') '    E      : L1,L2,Linf = ', e1, e2, ei

  call write_solution(U, t, 'final')
  call write_line_axis(U, t)
  if (nd >= 2) call write_line_diag(U, t)
  call close_energy()

  !--- 7. clean up ---------------------------------------------------
  deallocate(Y, U)
  call ti_free()
  call dg_destroy()
  call poisson_destroy()
  call destroy_basis()
  call destroy_mesh()

contains

  character(len=8) function itoa(i)
    integer, intent(in) :: i
    write(itoa,'(i0)') i
  end function itoa

end program main_sgeuler
