program main_kerr_tov
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: setup_grid
  use vars_mod, only: NVARS, alloc_state
  use hydro_vars_mod, only: NCONS, NPRIM, alloc_cons
  use tov_mod, only: tov_profile_t, solve_tov
  use kerr_tov_id_mod, only: set_kerr_tov_id
  use initial_data_mod, only: compute_conformal_connections
  use constraints_mod, only: hamiltonian_constraint, elliptic_hamiltonian_correction
  use coupled_timestep_mod, only: coupled_rk4_step
  use hydro_rhs_dynamic_mod, only: max_characteristic_speed
  use eos_mod, only: set_eos_gamma
  implicit none
  type(tov_profile_t) :: prof
  real(dp), allocatable :: u(:,:,:), cons(:,:,:), H(:,:)
  real(dp) :: rho_c, Kpoly, gamma, dt, t, hmax, hrms, hbefore, hafter
  integer :: imin, imax, kmin, kmax, n, ham_iter
  real(dp) :: ham_omega
  character(len=64) :: buf
  integer :: stat

  call init_params(); call setup_grid()
  gamma=2.0_dp; Kpoly=100.0_dp; rho_c=1.28e-3_dp
  call get_environment_variable("GR_RHOC",buf,status=stat); if(stat==0) read(buf,*) rho_c
  call set_eos_gamma(gamma)
  call solve_tov(rho_c,Kpoly,gamma,prof,50.0_dp,200000)
  call alloc_state(u,Nx,Nz); call alloc_cons(cons,Nx,Nz); allocate(H(Nx,Nz))
  call set_kerr_tov_id(u,cons,prof)
  call compute_conformal_connections(u)
  call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,cons)
  hmax=maxval(abs(H(imin:imax,kmin:kmax)))
  hrms=sqrt(sum(H(imin:imax,kmin:kmax)**2)/real((imax-imin+1)*(kmax-kmin+1),dp))
  print *, "=== Approximate dynamical Kerr + TOV initial data ==="
  print '(A,I0,A,I0)', " Grid: Nx=",Nx," Nz=",Nz
  print '(A,F8.4,A,F8.4,A)', " BH center (x,z)=(",bh_center_x,",",bh_center_z,")"
  print '(A,F8.4,A,F8.4,A)', " star center (x,z)=(",star_center_x,",",star_center_z,")"
  print '(A,ES12.4,A,ES12.4)', " before correction: max|H|=",hmax," rms|H|=",hrms
  ham_iter=3; ham_omega=0.7_dp
  call get_environment_variable("GR_HAM_ITER",buf,status=stat); if(stat==0) read(buf,*) ham_iter
  call get_environment_variable("GR_HAM_OMEGA",buf,status=stat); if(stat==0) read(buf,*) ham_omega
  call elliptic_hamiltonian_correction(u,cons,ham_iter,ham_omega,hbefore,hafter)
  call compute_conformal_connections(u)
  call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,cons)
  hmax=maxval(abs(H(imin:imax,kmin:kmax)))
  hrms=sqrt(sum(H(imin:imax,kmin:kmax)**2)/real((imax-imin+1)*(kmax-kmin+1),dp))
  print '(A,I0,A,F5.2)', " elliptic prototype iterations=",ham_iter," omega=",ham_omega
  print '(A,ES12.4,A,ES12.4)', " after correction:  max|H|=",hmax," rms|H|=",hrms
  print *, " NOTE: momentum constraints and physical boundaries are not solved."
  t=0.0_dp
  do n=1,nsteps
    dt=cfl*min(dx,dz)/max_characteristic_speed(u,cons)
    call coupled_rk4_step(u,cons,dt)
    t=t+dt
    if(mod(n,out_every)==0 .or. n==nsteps) print '(A,I0,A,F8.4,A,F8.5)'," step ",n," t=",t," dt=",dt
  end do
end program main_kerr_tov
