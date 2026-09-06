module matter_source_mod
  use kinds_mod, only: dp
  use eos_mod, only: pressure
  use hydro_vars_mod, only: NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  implicit none
  private
  public :: matter_source

contains

  ! Given primitives (rho, v^i, eps) and the local physical spatial metric,
  ! returns:
  !   E    = tau + D = rho h W^2 - p          (Eulerian energy density)
  !   Slow(3) = S_i = rho h W^2 v_i            (Eulerian momentum density)
  !   Sij(3,3) = rho h W^2 v_i v_j + p gamma_ij (Eulerian spatial stress)
  subroutine matter_source(prim, gamma_ij, E, Slow, Sij)
    real(dp), intent(in)  :: prim(NPRIM), gamma_ij(3,3)
    real(dp), intent(out) :: E, Slow(3), Sij(3,3)
    real(dp) :: rho, eps, p, h, W, v2
    real(dp) :: vup(3), vlow(3)
    integer :: a, b

    rho = prim(IRHO); eps = prim(IEPS)
    vup = (/ prim(IVX), prim(IVY), prim(IVZ) /)
    vlow = matmul(gamma_ij, vup)
    v2 = min(sum(vup*vlow), 1.0_dp - 1.0e-12_dp)
    W = 1.0_dp/sqrt(max(1.0_dp - v2, 1.0e-14_dp))
    p = pressure(rho, eps)
    h = 1.0_dp + eps + p/max(rho, 1.0e-300_dp)

    E = rho*h*W*W - p
    Slow = rho*h*W*W*vlow

    do a = 1, 3
      do b = 1, 3
        Sij(a,b) = rho*h*W*W*vlow(a)*vlow(b) + p*gamma_ij(a,b)
      end do
    end do
  end subroutine matter_source

end module matter_source_mod
