module hydro_timestep_mod
  use kinds_mod, only: dp
  use hydro_vars_mod, only: NCONS
  use hydro_rhs_mod, only: compute_hydro_rhs
  use hydro_boundary_mod, only: apply_hydro_boundary
  implicit none
  private
  public :: hydro_rk4_step

contains

  subroutine hydro_rk4_step(cons, bg, dt)
    real(dp), intent(inout) :: cons(:,:,:)
    real(dp), intent(in)    :: bg(:,:,:)
    real(dp), intent(in)    :: dt
    real(dp), allocatable :: k1(:,:,:), k2(:,:,:), k3(:,:,:), k4(:,:,:), utmp(:,:,:)
    integer :: n1, n2, n3

    n1 = size(cons,1); n2 = size(cons,2); n3 = size(cons,3)
    allocate(k1(n1,n2,n3), k2(n1,n2,n3), k3(n1,n2,n3), k4(n1,n2,n3), utmp(n1,n2,n3))

    k1 = 0.0_dp; call compute_hydro_rhs(cons, bg, k1)

    utmp = cons + 0.5_dp*dt*k1
    call apply_hydro_boundary(utmp)
    k2 = 0.0_dp; call compute_hydro_rhs(utmp, bg, k2)

    utmp = cons + 0.5_dp*dt*k2
    call apply_hydro_boundary(utmp)
    k3 = 0.0_dp; call compute_hydro_rhs(utmp, bg, k3)

    utmp = cons + dt*k3
    call apply_hydro_boundary(utmp)
    k4 = 0.0_dp; call compute_hydro_rhs(utmp, bg, k4)

    cons = cons + (dt/6.0_dp)*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
    call apply_hydro_boundary(cons)

    deallocate(k1, k2, k3, k4, utmp)
  end subroutine hydro_rk4_step

end module hydro_timestep_mod
