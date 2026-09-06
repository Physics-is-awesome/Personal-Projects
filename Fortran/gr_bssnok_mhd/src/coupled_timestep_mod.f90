module coupled_timestep_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use vars_mod, only: NVARS
  use hydro_vars_mod, only: NCONS, NPRIM
  use bssn_rhs_mod, only: compute_rhs_interior
  use dissipation_mod, only: add_ko_dissipation
  use sommerfeld_mod, only: add_sommerfeld_bc
  use hydro_rhs_dynamic_mod, only: compute_hydro_rhs_dynamic, recover_all_primitives_dynamic
  use hydro_boundary_mod, only: apply_hydro_boundary
  use matter_source_grid_mod, only: build_matter_source_grid
  implicit none
  private
  public :: coupled_rk4_step

contains

  subroutine coupled_stage_rhs(bssn_u, hydro_cons, k_bssn, k_hydro)
    real(dp), intent(in)    :: bssn_u(:,:,:), hydro_cons(:,:,:)
    real(dp), intent(inout) :: k_bssn(:,:,:), k_hydro(:,:,:)
    real(dp), allocatable :: prim(:,:,:), msrc(:,:,:)

    allocate(prim(Nx,Nz,NPRIM), msrc(Nx,Nz,10))

    call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
    call build_matter_source_grid(prim, bssn_u, msrc)

    k_bssn = 0.0_dp
    call compute_rhs_interior(bssn_u, k_bssn, msrc)
    call add_ko_dissipation(bssn_u, k_bssn)
    call add_sommerfeld_bc(bssn_u, k_bssn)

    k_hydro = 0.0_dp
    call compute_hydro_rhs_dynamic(hydro_cons, bssn_u, k_hydro)

    deallocate(prim, msrc)
  end subroutine coupled_stage_rhs

  subroutine coupled_rk4_step(bssn_u, hydro_cons, dt)
    real(dp), intent(inout) :: bssn_u(:,:,:), hydro_cons(:,:,:)
    real(dp), intent(in)    :: dt

    real(dp), allocatable :: k1b(:,:,:), k2b(:,:,:), k3b(:,:,:), k4b(:,:,:), utmpb(:,:,:)
    real(dp), allocatable :: k1h(:,:,:), k2h(:,:,:), k3h(:,:,:), k4h(:,:,:), utmph(:,:,:)

    allocate(k1b(Nx,Nz,NVARS), k2b(Nx,Nz,NVARS), k3b(Nx,Nz,NVARS), k4b(Nx,Nz,NVARS), utmpb(Nx,Nz,NVARS))
    allocate(k1h(Nx,Nz,NCONS), k2h(Nx,Nz,NCONS), k3h(Nx,Nz,NCONS), k4h(Nx,Nz,NCONS), utmph(Nx,Nz,NCONS))

    call coupled_stage_rhs(bssn_u, hydro_cons, k1b, k1h)

    utmpb = bssn_u + 0.5_dp*dt*k1b
    utmph = hydro_cons + 0.5_dp*dt*k1h
    call apply_hydro_boundary(utmph)
    call coupled_stage_rhs(utmpb, utmph, k2b, k2h)

    utmpb = bssn_u + 0.5_dp*dt*k2b
    utmph = hydro_cons + 0.5_dp*dt*k2h
    call apply_hydro_boundary(utmph)
    call coupled_stage_rhs(utmpb, utmph, k3b, k3h)

    utmpb = bssn_u + dt*k3b
    utmph = hydro_cons + dt*k3h
    call apply_hydro_boundary(utmph)
    call coupled_stage_rhs(utmpb, utmph, k4b, k4h)

    bssn_u = bssn_u + (dt/6.0_dp)*(k1b + 2.0_dp*k2b + 2.0_dp*k3b + k4b)
    hydro_cons = hydro_cons + (dt/6.0_dp)*(k1h + 2.0_dp*k2h + 2.0_dp*k3h + k4h)
    call apply_hydro_boundary(hydro_cons)

    deallocate(k1b,k2b,k3b,k4b,utmpb, k1h,k2h,k3h,k4h,utmph)
  end subroutine coupled_rk4_step

end module coupled_timestep_mod
