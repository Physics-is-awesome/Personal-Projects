module hydro_boundary_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  implicit none
  private
  public :: apply_hydro_boundary

contains

  subroutine apply_hydro_boundary(u)
    real(dp), intent(inout) :: u(:,:,:)
    u(Nx,:,:) = u(Nx-1,:,:)
    u(:,1,:)  = u(:,2,:)
    u(:,Nz,:) = u(:,Nz-1,:)
  end subroutine apply_hydro_boundary

end module hydro_boundary_mod
