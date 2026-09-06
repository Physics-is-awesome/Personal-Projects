program test_tov_hydro_only
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IALPHA, alloc_state
  use hydro_vars_mod, only: NCONS, NPRIM, IRHO, alloc_cons
  use tov_mod, only: tov_profile_t, solve_tov
  use tov_id_mod, only: set_tov_id
  use eos_mod, only: set_eos_gamma
  use hydro_rhs_dynamic_mod, only: compute_hydro_rhs_dynamic, recover_all_primitives_dynamic
  use hydro_boundary_mod, only: apply_hydro_boundary
  implicit none

  type(tov_profile_t) :: prof
  real(dp), allocatable :: bssn_u(:,:,:), hydro_cons(:,:,:), prim(:,:,:)
  real(dp), allocatable :: k1(:,:,:), k2(:,:,:), k3(:,:,:), k4(:,:,:), utmp(:,:,:)
  real(dp) :: rho_c, tov_K, tov_Gamma, dt, t
  integer :: n, probe_i, probe_k

  r_excise = 0.0_dp
  call init_params()
  call setup_grid()

  tov_Gamma = 2.0_dp; tov_K = 100.0_dp; rho_c = 1.28e-3_dp
  call set_eos_gamma(tov_Gamma)
  call solve_tov(rho_c, tov_K, tov_Gamma, prof, 50.0_dp, 200000)

  print *, "=== Hydro-only isolation test: frozen exact TOV metric ==="
  print '(A,F8.4,A,F8.4)', " TOV star: M=", prof%M_star, "  R=", prof%R_star
  print *

  call alloc_state(bssn_u, Nx, Nz)
  call alloc_cons(hydro_cons, Nx, Nz)
  allocate(prim(Nx,Nz,NPRIM))
  call set_tov_id(bssn_u, hydro_cons, prof)
  ! bssn_u is now NEVER touched again -- frozen exact metric.

  probe_i = 1; probe_k = 1
  do while (sqrt(xg(1)**2 + zg(probe_k)**2) > 0.3_dp*prof%riso_star .and. probe_k < Nz)
    probe_k = probe_k + 1
  end do

  call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
  print '(A,ES12.4)', " t=0 rho(probe) = ", prim(probe_i,probe_k,IRHO)

  block
    real(dp), allocatable :: rhs0(:,:,:)
    integer :: kk, kprobe_list(5), jj
    allocate(rhs0(Nx,Nz,NCONS))
    rhs0 = 0.0_dp
    call compute_hydro_rhs_dynamic(hydro_cons, bssn_u, rhs0)
    print *, "t=0 raw RHS (i=1, several z), should be ~0 at equilibrium:"
    kprobe_list = (/ Nz/2-20, Nz/2-10, Nz/2, Nz/2+10, Nz/2+20 /)
    do jj = 1, 5
      kk = kprobe_list(jj)
      print '(A,F8.4,A,ES12.4,A,ES12.4,A,ES12.4)', "   z=", zg(kk), &
        "  RHS(D)=", rhs0(1,kk,1), "  RHS(Sz)=", rhs0(1,kk,4), "  RHS(tau)=", rhs0(1,kk,5)
    end do
    deallocate(rhs0)
  end block

  dt = cfl * min(dx, dz)
  t = 0.0_dp
  allocate(k1(Nx,Nz,NCONS),k2(Nx,Nz,NCONS),k3(Nx,Nz,NCONS),k4(Nx,Nz,NCONS),utmp(Nx,Nz,NCONS))

  do n = 1, nsteps
    k1=0.0_dp; call compute_hydro_rhs_dynamic(hydro_cons, bssn_u, k1)
    utmp = hydro_cons + 0.5_dp*dt*k1; call apply_hydro_boundary(utmp)
    k2=0.0_dp; call compute_hydro_rhs_dynamic(utmp, bssn_u, k2)
    utmp = hydro_cons + 0.5_dp*dt*k2; call apply_hydro_boundary(utmp)
    k3=0.0_dp; call compute_hydro_rhs_dynamic(utmp, bssn_u, k3)
    utmp = hydro_cons + dt*k3; call apply_hydro_boundary(utmp)
    k4=0.0_dp; call compute_hydro_rhs_dynamic(utmp, bssn_u, k4)
    hydro_cons = hydro_cons + (dt/6.0_dp)*(k1+2.0_dp*k2+2.0_dp*k3+k4)
    call apply_hydro_boundary(hydro_cons)
    t = t + dt

    if (mod(n, out_every) == 0 .or. n == nsteps) then
      call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
      print '(A,I5,A,F8.4,A,ES12.4)', " step ", n, "  t=", t, "  rho(probe)=", prim(probe_i,probe_k,IRHO)
    end if
  end do

end program test_tov_hydro_only
