module live_metric_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IPHI, IK, IALPHA, IBETAX, IBETAY, IBETAZ
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry, pack6, pack6A
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: live_metric_at_cell, live_metric_at_ghost, live_metric_value

contains

  ! Lightweight value-only ADM extraction at a real grid point (i,k) -- no
  ! stencil or Ricci computation needed, just algebra on u(i,k,:) itself.
  subroutine live_metric_value(u, i, k, alpha, beta_up, gamma_ij, gamma_inv)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3)
    real(dp) :: phi, e4phi, gt(3,3)
    real(dp) :: det, c11,c12,c13,c22,c23,c33

    phi = u(i,k,IPHI); e4phi = exp(4.0_dp*phi)
    alpha = u(i,k,IALPHA)
    beta_up = (/ u(i,k,IBETAX), u(i,k,IBETAY), u(i,k,IBETAZ) /)
    gt = pack6(u(i,k,:))
    gamma_ij = e4phi*gt

    c11 =  gt(2,2)*gt(3,3) - gt(2,3)*gt(2,3)
    c12 = -(gt(1,2)*gt(3,3) - gt(1,3)*gt(2,3))
    c13 =  gt(1,2)*gt(2,3) - gt(1,3)*gt(2,2)
    c22 =  gt(1,1)*gt(3,3) - gt(1,3)*gt(1,3)
    c23 = -(gt(1,1)*gt(2,3) - gt(1,3)*gt(1,2))
    c33 =  gt(1,1)*gt(2,2) - gt(1,2)*gt(1,2)
    det = gt(1,1)*c11 + gt(1,2)*c12 + gt(1,3)*c13
    gamma_inv(1,1)=c11/det; gamma_inv(1,2)=c12/det; gamma_inv(1,3)=c13/det
    gamma_inv(2,1)=c12/det; gamma_inv(2,2)=c22/det; gamma_inv(2,3)=c23/det
    gamma_inv(3,1)=c13/det; gamma_inv(3,2)=c23/det; gamma_inv(3,3)=c33/det
    gamma_inv = gamma_inv/e4phi
  end subroutine live_metric_value

  ! Cell-centered physical ADM quantities and derivatives, at BSSN grid
  ! point (i,k), built from the conformal state via the same stencil +
  ! geometry machinery bssn_rhs_mod itself uses.
  subroutine live_metric_at_cell(u, i, k, alpha, beta_up, gamma_ij, gamma_inv, Kij, &
                                  dalpha, dbeta, dgamma)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3), Kij(3,3)
    real(dp), intent(out) :: dalpha(3), dbeta(3,3), dgamma(3,3,3)

    type(stencil_t) :: S
    type(conformal_geometry_t) :: G
    real(dp) :: phi, Ktr, e4phi
    real(dp) :: Amat(3,3), dg(3,3,3)
    integer :: m

    call compute_stencil(u, i, k, S)
    call compute_conformal_geometry(S, G)

    phi = S%val(IPHI); Ktr = S%val(IK)
    e4phi = exp(4.0_dp*phi)
    Amat = pack6A(S%val)

    alpha = S%val(IALPHA)
    beta_up = (/ S%val(IBETAX), S%val(IBETAY), S%val(IBETAZ) /)

    gamma_ij  = e4phi*G%gt
    gamma_inv = G%gtinv/e4phi
    Kij = e4phi*(Amat + G%gt*Ktr/3.0_dp)

    dalpha = G%dalpha

    dbeta(1,:) = (/ S%d1x(IBETAX), S%d1x(IBETAY), S%d1x(IBETAZ) /)
    dbeta(2,:) = (/ S%d1y(IBETAX), S%d1y(IBETAY), S%d1y(IBETAZ) /)
    dbeta(3,:) = (/ S%d1z(IBETAX), S%d1z(IBETAY), S%d1z(IBETAZ) /)

    dg(1,:,:) = pack6(S%d1x); dg(2,:,:) = pack6(S%d1y); dg(3,:,:) = pack6(S%d1z)
    do m = 1, 3
      dgamma(:,:,m) = e4phi*( 4.0_dp*G%dphi(m)*G%gt + dg(m,:,:) )
    end do
  end subroutine live_metric_at_cell

  ! Physical ADM quantities (value only, no derivatives) at an off-grid
  ! point reached via a Cartoon ghost offset from (i,k) -- used for y-ghosts
  ! and the axis ghost when averaging to a face value.
  subroutine live_metric_at_ghost(u, i, k, ix_off, iz_off, y_val, alpha, beta_up, gamma_ij, gamma_inv)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k, ix_off, iz_off
    real(dp), intent(in)  :: y_val
    real(dp), intent(out) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3)
    real(dp) :: v(NVARS), phi, e4phi
    real(dp) :: gt(3,3)

    v = get_neighbor(u, i, k, ix_off, iz_off, y_val)
    phi = v(IPHI)
    e4phi = exp(4.0_dp*phi)
    alpha = v(IALPHA)
    beta_up = (/ v(IBETAX), v(IBETAY), v(IBETAZ) /)
    gt = pack6(v)
    gamma_ij = e4phi*gt
    block
      real(dp) :: det
      real(dp) :: c11,c12,c13,c22,c23,c33
      c11 =  gt(2,2)*gt(3,3) - gt(2,3)*gt(2,3)
      c12 = -(gt(1,2)*gt(3,3) - gt(1,3)*gt(2,3))
      c13 =  gt(1,2)*gt(2,3) - gt(1,3)*gt(2,2)
      c22 =  gt(1,1)*gt(3,3) - gt(1,3)*gt(1,3)
      c23 = -(gt(1,1)*gt(2,3) - gt(1,3)*gt(1,2))
      c33 =  gt(1,1)*gt(2,2) - gt(1,2)*gt(1,2)
      det = gt(1,1)*c11 + gt(1,2)*c12 + gt(1,3)*c13
      gamma_inv(1,1)=c11/det; gamma_inv(1,2)=c12/det; gamma_inv(1,3)=c13/det
      gamma_inv(2,1)=c12/det; gamma_inv(2,2)=c22/det; gamma_inv(2,3)=c23/det
      gamma_inv(3,1)=c13/det; gamma_inv(3,2)=c23/det; gamma_inv(3,3)=c33/det
      gamma_inv = gamma_inv/e4phi
    end block
  end subroutine live_metric_at_ghost

end module live_metric_mod
