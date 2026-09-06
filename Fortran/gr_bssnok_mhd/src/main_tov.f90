program main_tov
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IPHI, IALPHA, alloc_state
  use hydro_vars_mod, only: NCONS, NPRIM, IRHO, IEPS, alloc_cons
  use tov_mod, only: tov_profile_t, solve_tov
  use tov_id_mod, only: set_tov_id
  use coupled_timestep_mod, only: coupled_rk4_step
  use hydro_rhs_dynamic_mod, only: recover_all_primitives_dynamic, max_characteristic_speed
  use eos_mod, only: set_eos_gamma
  implicit none

  type(tov_profile_t) :: prof
  real(dp), allocatable :: bssn_u(:,:,:), hydro_cons(:,:,:), prim(:,:,:)
  real(dp) :: rho_c, tov_K, tov_Gamma
  real(dp) :: dt, t
  integer :: n, probe_i, probe_k
  real(dp) :: rho0_c, alpha0_c, phi0_c
  character(len=64) :: buf
  integer :: envstat

  ! No excision needed for a smooth stellar interior.
  r_excise = 0.0_dp
  call get_environment_variable("GR_REXCISE", buf, status=envstat); if (envstat==0) read(buf,*) r_excise

  call init_params()
  call setup_grid()

  tov_Gamma = 2.0_dp
  tov_K = 100.0_dp
  rho_c = 1.28e-3_dp
  call get_environment_variable("GR_RHOC", buf, status=envstat); if (envstat==0) read(buf,*) rho_c

  call set_eos_gamma(tov_Gamma)
  call solve_tov(rho_c, tov_K, tov_Gamma, prof, 50.0_dp, 200000)

  print *, "=== Milestone 3: TOV star, coupled BSSN+GRHD evolution ==="
  print '(A,I0,A,I0)', " Grid: Nx=", Nx, "  Nz=", Nz
  print '(A,F6.3,A,F6.3)', " domain: xmax=", xmax, "  zmax=", zmax
  print '(A,F8.4,A,F8.4,A,F8.4)', " TOV star: M=", prof%M_star, "  R=", prof%R_star, &
    "  riso_star=", prof%riso_star
  print *

  call alloc_state(bssn_u, Nx, Nz)
  call alloc_cons(hydro_cons, Nx, Nz)
  allocate(prim(Nx,Nz,NPRIM))

  call set_tov_id(bssn_u, hydro_cons, prof)

  probe_i = 1; probe_k = 1
  do while (sqrt(xg(1)**2 + zg(probe_k)**2) > 0.3_dp*prof%riso_star .and. probe_k < Nz)
    probe_k = probe_k + 1
  end do

  call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
  rho0_c = prim(probe_i,probe_k,IRHO)
  alpha0_c = bssn_u(probe_i,probe_k,IALPHA)
  phi0_c = bssn_u(probe_i,probe_k,IPHI)
  print '(A,I0,A,F8.4,A,ES12.4,A,F10.6,A,F10.6)', " probe: k=", probe_k, "  riso=", zg(probe_k), &
    "  rho0=", rho0_c, "  alpha0=", alpha0_c, "  phi0=", phi0_c
  print *

  t = 0.0_dp
  dt = cfl * min(dx, dz) / max_characteristic_speed(bssn_u, hydro_cons)
  print '(A,F8.5,A,F8.5)', " Initial time step dt = ", dt, "  max characteristic speed = ", &
    cfl * min(dx, dz) / dt
  print *, "Evolving (coupled BSSN + GRHD, no excision, TOV equilibrium)..."

  do n = 1, nsteps
    dt = cfl * min(dx, dz) / max_characteristic_speed(bssn_u, hydro_cons)
    call coupled_rk4_step(bssn_u, hydro_cons, dt)
    t = t + dt

    if (mod(n, out_every) == 0 .or. n == nsteps) then
      call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
      block
        logical :: found
        integer :: i, k
        found = .false.
        do k = 2, Nz-1
          do i = 1, Nx-1
            if (bssn_u(i,k,IALPHA) /= bssn_u(i,k,IALPHA)) found = .true.
          end do
        end do
        print '(A,I5,A,F8.4,A,ES12.4,A,F10.6,A,F10.6,A,L1)', &
          " step ", n, "  t=", t, "  rho(probe)=", prim(probe_i,probe_k,IRHO), &
          "  alpha(probe)=", bssn_u(probe_i,probe_k,IALPHA), &
          "  phi(probe)=", bssn_u(probe_i,probe_k,IPHI), "  NaN found=", found
      end block
    end if
  end do

end program main_tov
