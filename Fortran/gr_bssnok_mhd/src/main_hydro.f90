program main_hydro
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use hydro_vars_mod, only: NCONS, NPRIM, ID, ISX, ISY, ISZ, ITAU, IRHO, IVX, IVY, IVZ, IEPS, alloc_cons
  use background_mod, only: setup_background, NBG
  use hydro_initial_data_mod, only: set_atmosphere_id
  use hydro_rhs_mod, only: recover_all_primitives
  use hydro_timestep_mod, only: hydro_rk4_step
  use excision_mod, only: extrapolate_excised
  implicit none

  real(dp), allocatable :: cons(:,:,:), bg(:,:,:), prim(:,:,:)
  real(dp) :: rho0, eps0, dt, t
  integer :: n, i, k, probe_i, probe_k
  character(len=64) :: buf
  integer :: envstat

  call init_params()
  call setup_grid()

  rho0 = 1.0e-4_dp
  eps0 = 1.0e-2_dp
  call get_environment_variable("GR_RHO0", buf, status=envstat); if (envstat==0) read(buf,*) rho0
  call get_environment_variable("GR_EPS0", buf, status=envstat); if (envstat==0) read(buf,*) eps0

  print *, "=== GRHD on fixed Kerr-Schild background - Milestone 2 sanity test ==="
  print '(A,I0,A,I0)', " Grid: Nx=", Nx, "  Nz=", Nz
  print '(A,F6.3,A,F6.3)', " BH mass M=", bh_mass, "   spin a=", bh_spin
  print '(A,ES10.3,A,ES10.3)', " atmosphere rho0=", rho0, "   eps0=", eps0
  print *

  call alloc_cons(cons, Nx, Nz)
  allocate(prim(Nx,Nz,NPRIM))
  call setup_background(bg)
  call set_atmosphere_id(cons, bg, rho0, eps0)

  probe_i = 1
  do k = 1, Nz
    if (sqrt(xg(1)**2 + zg(k)**2) >= r_excise + 2.0_dp) then
      probe_k = k
      exit
    end if
  end do
  print '(A,I0,A,F8.4)', " probe point: i=1, k=", probe_k, "  z=", zg(probe_k)

  call recover_all_primitives(cons, bg, prim)
  print '(A,ES12.4,A,ES12.4,A,ES12.4)', " t=0  rho=", prim(1,probe_k,IRHO), &
    "  vz=", prim(1,probe_k,IVZ), "  eps=", prim(1,probe_k,IEPS)

  dt = cfl * min(dx, dz)
  t = 0.0_dp
  print '(A,F8.5)', " Time step dt = ", dt
  print *, "Evolving..."

  do n = 1, nsteps
    call hydro_rk4_step(cons, bg, dt)
    call extrapolate_excised(cons)
    t = t + dt

    if (mod(n, out_every) == 0 .or. n == nsteps) then
      call recover_all_primitives(cons, bg, prim)

      block
        logical :: found
        real(dp) :: maxrho, minrho
        found = .false.
        maxrho = -1.0e300_dp; minrho = 1.0e300_dp
        do k = 2, Nz-1
          do i = 1, Nx-1
            if (sqrt(xg(i)**2+zg(k)**2) < r_excise) cycle
            if (prim(i,k,IRHO) /= prim(i,k,IRHO)) found = .true.  ! NaN check
            maxrho = max(maxrho, prim(i,k,IRHO))
            minrho = min(minrho, prim(i,k,IRHO))
          end do
        end do
        print '(A,I5,A,F8.4,A,ES12.4,A,ES12.4,A,ES12.4,A,L1)', &
          " step ", n, "  t=", t, "  rho(probe)=", prim(1,probe_k,IRHO), &
          "  vz(probe)=", prim(1,probe_k,IVZ), "  eps(probe)=", prim(1,probe_k,IEPS), &
          "  NaN found=", found
        print '(A,ES12.4,A,ES12.4)', "     rho range (ext): min=", minrho, "  max=", maxrho
      end block
    end if
  end do

end program main_hydro
