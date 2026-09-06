module boundary_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  implicit none
  private
  public :: apply_boundary

contains

  ! v0.1: simple zeroth-order (copy) outer boundary. Not radiative/Sommerfeld;
  ! adequate only for short test evolutions before any outgoing wave content
  ! reaches the edge. Flagged in docs/derivation.md as a follow-up item.
  subroutine apply_boundary(u)
    real(dp), intent(inout) :: u(:,:,:)
    u(Nx,:,:) = u(Nx-1,:,:)
    u(:,1,:)  = u(:,2,:)
    u(:,Nz,:) = u(:,Nz-1,:)
  end subroutine apply_boundary

end module boundary_mod
