module hydro_initial_data_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use hydro_vars_mod, only: NCONS, NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  use riemann_mod, only: prim_to_cons
  use background_mod, only: IBGGXX, IBGGXY, IBGGXZ, IBGGYY, IBGGYZ, IBGGZZ
  implicit none
  private
  public :: set_atmosphere_id

contains

  subroutine set_atmosphere_id(cons, bg, rho0, eps0)
    real(dp), intent(inout) :: cons(:,:,:)
    real(dp), intent(in)    :: bg(:,:,:)
    real(dp), intent(in)    :: rho0, eps0
    integer :: i, k
    real(dp) :: prim(NPRIM), gamma_ij(3,3)

    prim(IRHO) = rho0; prim(IVX) = 0.0_dp; prim(IVY) = 0.0_dp; prim(IVZ) = 0.0_dp; prim(IEPS) = eps0

    do k = 1, Nz
      do i = 1, Nx
        gamma_ij(1,1)=bg(i,k,IBGGXX); gamma_ij(1,2)=bg(i,k,IBGGXY); gamma_ij(1,3)=bg(i,k,IBGGXZ)
        gamma_ij(2,1)=bg(i,k,IBGGXY); gamma_ij(2,2)=bg(i,k,IBGGYY); gamma_ij(2,3)=bg(i,k,IBGGYZ)
        gamma_ij(3,1)=bg(i,k,IBGGXZ); gamma_ij(3,2)=bg(i,k,IBGGYZ); gamma_ij(3,3)=bg(i,k,IBGGZZ)
        call prim_to_cons(prim, gamma_ij, cons(i,k,:))
      end do
    end do
  end subroutine set_atmosphere_id

end module hydro_initial_data_mod
