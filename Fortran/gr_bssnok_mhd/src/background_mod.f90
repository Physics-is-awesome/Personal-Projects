module background_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use grid_mod, only: xg, zg
  use geometry_mod, only: sym3_inverse
  use initial_data_mod, only: kerr_schild_point
  implicit none
  private
  public :: NBG
  public :: IBGA, IBGBX, IBGBY, IBGBZ
  public :: IBGGXX, IBGGXY, IBGGXZ, IBGGYY, IBGGYZ, IBGGZZ
  public :: IBGIXX, IBGIXY, IBGIXZ, IBGIYY, IBGIYZ, IBGIZZ
  public :: IBGSQRTG
  public :: IBGKXX, IBGKXY, IBGKXZ, IBGKYY, IBGKYZ, IBGKZZ
  public :: IBGDAX, IBGDAY, IBGDAZ
  public :: IBGDB   ! base index; dbeta(m,c) at IBGDB + (m-1)*3 + c
  public :: IBGDG   ! base index; d_m gamma_(ab) at IBGDG + (comp6-1)*3 + m, comp6 in 1..6 (xx,xy,xz,yy,yz,zz)
  public :: setup_background, metric_at

  integer, parameter :: IBGA    = 1
  integer, parameter :: IBGBX   = 2
  integer, parameter :: IBGBY   = 3
  integer, parameter :: IBGBZ   = 4
  integer, parameter :: IBGGXX  = 5
  integer, parameter :: IBGGXY  = 6
  integer, parameter :: IBGGXZ  = 7
  integer, parameter :: IBGGYY  = 8
  integer, parameter :: IBGGYZ  = 9
  integer, parameter :: IBGGZZ  = 10
  integer, parameter :: IBGIXX  = 11
  integer, parameter :: IBGIXY  = 12
  integer, parameter :: IBGIXZ  = 13
  integer, parameter :: IBGIYY  = 14
  integer, parameter :: IBGIYZ  = 15
  integer, parameter :: IBGIZZ  = 16
  integer, parameter :: IBGSQRTG = 17
  integer, parameter :: IBGKXX  = 18
  integer, parameter :: IBGKXY  = 19
  integer, parameter :: IBGKXZ  = 20
  integer, parameter :: IBGKYY  = 21
  integer, parameter :: IBGKYZ  = 22
  integer, parameter :: IBGKZZ  = 23
  integer, parameter :: IBGDAX  = 24
  integer, parameter :: IBGDAY  = 25
  integer, parameter :: IBGDAZ  = 26
  integer, parameter :: IBGDB   = 26   ! dbeta(m,c) -> IBGDB + (m-1)*3 + c   (27..35)
  integer, parameter :: IBGDG   = 35   ! d_m g_(ab) -> IBGDG + (comp6-1)*3 + m  (36..53)
  integer, parameter :: NBG = 53

contains

  subroutine setup_background(bg)
    real(dp), allocatable, intent(inout) :: bg(:,:,:)
    integer :: i, k, p, q, m
    real(dp) :: g0(3,3), gxp(3,3), gxm(3,3), gyp(3,3), gym(3,3), gzp(3,3), gzm(3,3)
    real(dp) :: a0, axp, axm, ayp, aym, azp, azm
    real(dp) :: bl0(3), blxp(3), blxm(3), blyp(3), blym(3), blzp(3), blzm(3)
    real(dp) :: ginv(3,3), det
    real(dp) :: dg(3,3,3)        ! dg(m,a,b) = d_m g_ab
    real(dp) :: Chr_low(3,3,3), Gam(3,3,3)
    real(dp) :: dbl(3,3)         ! d_m beta_lower_a
    real(dp) :: Dbeta(3,3), Kij(3,3)
    real(dp) :: bux(3)           ! beta_upper
    real(dp), parameter :: h = 1.0e-4_dp
    integer :: comp6

    allocate(bg(Nx, Nz, NBG))
    bg = 0.0_dp

    do k = 1, Nz
      do i = 1, Nx
        call kerr_schild_point(xg(i),   0.0_dp, zg(k),   g0,  a0,  bl0)
        call kerr_schild_point(xg(i)+h, 0.0_dp, zg(k),   gxp, axp, blxp)
        call kerr_schild_point(xg(i)-h, 0.0_dp, zg(k),   gxm, axm, blxm)
        call kerr_schild_point(xg(i),   h,      zg(k),   gyp, ayp, blyp)
        call kerr_schild_point(xg(i),  -h,      zg(k),   gym, aym, blym)
        call kerr_schild_point(xg(i),   0.0_dp, zg(k)+h, gzp, azp, blzp)
        call kerr_schild_point(xg(i),   0.0_dp, zg(k)-h, gzm, azm, blzm)

        call sym3_inverse(g0, ginv, det)

        dg(1,:,:) = (gxp - gxm) / (2.0_dp*h)
        dg(2,:,:) = (gyp - gym) / (2.0_dp*h)
        dg(3,:,:) = (gzp - gzm) / (2.0_dp*h)

        dbl(1,:) = (blxp - blxm) / (2.0_dp*h)
        dbl(2,:) = (blyp - blym) / (2.0_dp*h)
        dbl(3,:) = (blzp - blzm) / (2.0_dp*h)

        do m = 1, 3
          do p = 1, 3
            do q = 1, 3
              Chr_low(m,p,q) = 0.5_dp*(dg(p,q,m) + dg(q,p,m) - dg(m,p,q))
            end do
          end do
        end do
        do m = 1, 3
          do p = 1, 3
            do q = 1, 3
              Gam(m,p,q) = sum(ginv(m,:) * Chr_low(:,p,q))
            end do
          end do
        end do

        do p = 1, 3
          do q = 1, 3
            Dbeta(p,q) = dbl(p,q) - sum(Gam(:,p,q) * bl0(:))
          end do
        end do
        Kij = (Dbeta + transpose(Dbeta)) / (2.0_dp*a0)
        bux = matmul(ginv, bl0)

        bg(i,k,IBGA)  = a0
        bg(i,k,IBGBX) = bux(1); bg(i,k,IBGBY) = bux(2); bg(i,k,IBGBZ) = bux(3)

        bg(i,k,IBGGXX) = g0(1,1); bg(i,k,IBGGXY) = g0(1,2); bg(i,k,IBGGXZ) = g0(1,3)
        bg(i,k,IBGGYY) = g0(2,2); bg(i,k,IBGGYZ) = g0(2,3); bg(i,k,IBGGZZ) = g0(3,3)

        bg(i,k,IBGIXX) = ginv(1,1); bg(i,k,IBGIXY) = ginv(1,2); bg(i,k,IBGIXZ) = ginv(1,3)
        bg(i,k,IBGIYY) = ginv(2,2); bg(i,k,IBGIYZ) = ginv(2,3); bg(i,k,IBGIZZ) = ginv(3,3)

        bg(i,k,IBGSQRTG) = sqrt(max(det, 1.0e-300_dp))

        bg(i,k,IBGKXX) = Kij(1,1); bg(i,k,IBGKXY) = Kij(1,2); bg(i,k,IBGKXZ) = Kij(1,3)
        bg(i,k,IBGKYY) = Kij(2,2); bg(i,k,IBGKYZ) = Kij(2,3); bg(i,k,IBGKZZ) = Kij(3,3)

        bg(i,k,IBGDAX) = (axp-axm)/(2.0_dp*h)
        bg(i,k,IBGDAY) = (ayp-aym)/(2.0_dp*h)
        bg(i,k,IBGDAZ) = (azp-azm)/(2.0_dp*h)

        ! dbeta_upper(m,c): finite-difference beta_upper itself (raise bl at
        ! each neighbor with that neighbor's own inverse metric) for
        ! consistency; cheap since this all runs once at setup.
        block
          real(dp) :: ginvxp(3,3), ginvxm(3,3), ginvyp(3,3), ginvym(3,3), ginvzp(3,3), ginvzm(3,3)
          real(dp) :: buxp(3), buxm(3), buyp(3), buym(3), buzp(3), buzm(3), detn
          call sym3_inverse(gxp, ginvxp, detn); call sym3_inverse(gxm, ginvxm, detn)
          call sym3_inverse(gyp, ginvyp, detn); call sym3_inverse(gym, ginvym, detn)
          call sym3_inverse(gzp, ginvzp, detn); call sym3_inverse(gzm, ginvzm, detn)
          buxp = matmul(ginvxp, blxp); buxm = matmul(ginvxm, blxm)
          buyp = matmul(ginvyp, blyp); buym = matmul(ginvym, blym)
          buzp = matmul(ginvzp, blzp); buzm = matmul(ginvzm, blzm)
          do q = 1, 3
            bg(i,k, IBGDB + (1-1)*3 + q) = (buxp(q)-buxm(q))/(2.0_dp*h)
            bg(i,k, IBGDB + (2-1)*3 + q) = (buyp(q)-buym(q))/(2.0_dp*h)
            bg(i,k, IBGDB + (3-1)*3 + q) = (buzp(q)-buzm(q))/(2.0_dp*h)
          end do
        end block

        ! d_m gamma_(ab), 6 independent components x 3 directions
        do comp6 = 1, 6
          select case (comp6)
          case (1); p=1; q=1
          case (2); p=1; q=2
          case (3); p=1; q=3
          case (4); p=2; q=2
          case (5); p=2; q=3
          case (6); p=3; q=3
          end select
          bg(i,k, IBGDG + (comp6-1)*3 + 1) = dg(1,p,q)
          bg(i,k, IBGDG + (comp6-1)*3 + 2) = dg(2,p,q)
          bg(i,k, IBGDG + (comp6-1)*3 + 3) = dg(3,p,q)
        end do

      end do
    end do
  end subroutine setup_background

  ! Lightweight evaluation (no derivatives) of the ADM metric at an
  ! arbitrary Cartesian point, for Godunov face fluxes and y-ghost flux
  ! evaluation where only the metric value itself (not its derivatives) is
  ! needed.
  subroutine metric_at(x, y, z, alpha, beta_up, gamma_ij, gamma_inv, sqrtg)
    real(dp), intent(in)  :: x, y, z
    real(dp), intent(out) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3), sqrtg
    real(dp) :: beta_low(3), det
    call kerr_schild_point(x, y, z, gamma_ij, alpha, beta_low)
    call sym3_inverse(gamma_ij, gamma_inv, det)
    beta_up = matmul(gamma_inv, beta_low)
    sqrtg = sqrt(max(det, 1.0e-300_dp))
  end subroutine metric_at

end module background_mod
