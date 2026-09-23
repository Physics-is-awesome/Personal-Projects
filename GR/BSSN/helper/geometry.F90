module geometry_mod
  use kinds_mod, only: dp
  use vars_mod, only: IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                       IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                       IALPHA, IGTX, IGTY, IGTZ
  use stencil_mod, only: stencil_t
  implicit none
  private
  public :: conformal_geometry_t, compute_conformal_geometry
  public :: sym3_inverse, pack6, pack6A

  type :: conformal_geometry_t
    real(dp) :: gt(3,3)         ! conformal metric
    real(dp) :: gtinv(3,3)      ! its inverse
    real(dp) :: Gam(3,3,3)      ! Gam(k,i,j) = Gamma-tilde^k_ij
    real(dp) :: Rt(3,3)         ! conformal Ricci R-tilde_ij
    real(dp) :: Rphi(3,3)       ! phi contribution to Ricci
    real(dp) :: DDa(3,3)        ! D_i D_j alpha (physical)
    real(dp) :: dphi(3)         ! d_i phi
    real(dp) :: dalpha(3)       ! d_i alpha
  end type conformal_geometry_t

contains

  ! Packs the 6 independent components of a symmetric-tensor-shaped slice of
  ! an NVARS-length array (using the gamma-tilde index layout: xx,xy,xz,yy,yz,zz)
  ! into a full 3x3 symmetric matrix.
  function pack6(arr) result(M)
    real(dp), intent(in) :: arr(:)
    real(dp) :: M(3,3)
    M(1,1) = arr(IGXX); M(1,2) = arr(IGXY); M(1,3) = arr(IGXZ)
    M(2,1) = arr(IGXY); M(2,2) = arr(IGYY); M(2,3) = arr(IGYZ)
    M(3,1) = arr(IGXZ); M(3,2) = arr(IGYZ); M(3,3) = arr(IGZZ)
  end function pack6

  ! Same layout, for the A-tilde_ij (conformal trace-free extrinsic curvature)
  ! block of an NVARS-length array.
  function pack6A(arr) result(M)
    real(dp), intent(in) :: arr(:)
    real(dp) :: M(3,3)
    M(1,1) = arr(IAXX); M(1,2) = arr(IAXY); M(1,3) = arr(IAXZ)
    M(2,1) = arr(IAXY); M(2,2) = arr(IAYY); M(2,3) = arr(IAYZ)
    M(3,1) = arr(IAXZ); M(3,2) = arr(IAYZ); M(3,3) = arr(IAZZ)
  end function pack6A

  subroutine sym3_inverse(M, Minv, det)
    real(dp), intent(in)  :: M(3,3)
    real(dp), intent(out) :: Minv(3,3)
    real(dp), intent(out) :: det
    real(dp) :: c11, c12, c13, c22, c23, c33

    ! Cofactors of the symmetric matrix M.
    c11 =  M(2,2)*M(3,3) - M(2,3)*M(2,3)
    c12 = -(M(1,2)*M(3,3) - M(1,3)*M(2,3))
    c13 =  M(1,2)*M(2,3) - M(1,3)*M(2,2)
    c22 =  M(1,1)*M(3,3) - M(1,3)*M(1,3)
    c23 = -(M(1,1)*M(2,3) - M(1,3)*M(1,2))
    c33 =  M(1,1)*M(2,2) - M(1,2)*M(1,2)

    det = M(1,1)*c11 + M(1,2)*c12 + M(1,3)*c13

    Minv(1,1) = c11/det; Minv(1,2) = c12/det; Minv(1,3) = c13/det
    Minv(2,1) = c12/det; Minv(2,2) = c22/det; Minv(2,3) = c23/det
    Minv(3,1) = c13/det; Minv(3,2) = c23/det; Minv(3,3) = c33/det
  end subroutine sym3_inverse

  subroutine compute_conformal_geometry(S, G)
    type(stencil_t), intent(in) :: S
    type(conformal_geometry_t), intent(out) :: G

    real(dp) :: dg(3,3,3)          ! dg(m,a,b) = d_m gt_ab
    real(dp) :: Chr_low(3,3,3)     ! Chr_low(k,i,j) = Gamma-tilde_{k,ij} = gt_kl Gam^l_ij
    real(dp) :: M_d2x(3,3), M_d2y(3,3), M_d2z(3,3)
    real(dp) :: M_d2xy(3,3), M_d2xz(3,3), M_d2yz(3,3)
    real(dp) :: lap_g(3,3)
    real(dp) :: Gtvec(3), dGt(3,3)        ! Gtvec(k), dGt(m,k) = d_m Gamma-tilde^k
    real(dp) :: term2(3,3), term3(3,3), term4(3,3)
    real(dp) :: d2phi(3,3), DDphi(3,3), lap_phi, gdphi2
    real(dp) :: d2a(3,3), DDa_t(3,3), det, gdphi_da
    integer  :: i, j, k, l, m
    real(dp) :: dphi_v(3), da_v(3)

    G%gt = pack6(S%val)
    call sym3_inverse(G%gt, G%gtinv, det)

    dg(1,:,:) = pack6(S%d1x)
    dg(2,:,:) = pack6(S%d1y)
    dg(3,:,:) = pack6(S%d1z)

    do k = 1, 3
      do i = 1, 3
        do j = 1, 3
          Chr_low(k,i,j) = 0.5_dp*(dg(i,j,k) + dg(j,i,k) - dg(k,i,j))
        end do
      end do
    end do

    do k = 1, 3
      do i = 1, 3
        do j = 1, 3
          G%Gam(k,i,j) = sum(G%gtinv(k,:) * Chr_low(:,i,j))
        end do
      end do
    end do

    ! --- R-tilde_ij, connection-function form ---
    M_d2x  = pack6(S%d2x);  M_d2y  = pack6(S%d2y);  M_d2z  = pack6(S%d2z)
    M_d2xy = pack6(S%d2xy); M_d2xz = pack6(S%d2xz); M_d2yz = pack6(S%d2yz)

    lap_g = G%gtinv(1,1)*M_d2x + G%gtinv(2,2)*M_d2y + G%gtinv(3,3)*M_d2z &
          + 2.0_dp*G%gtinv(1,2)*M_d2xy + 2.0_dp*G%gtinv(1,3)*M_d2xz &
          + 2.0_dp*G%gtinv(2,3)*M_d2yz

    Gtvec = (/ S%val(IGTX), S%val(IGTY), S%val(IGTZ) /)
    dGt(1,:) = (/ S%d1x(IGTX), S%d1x(IGTY), S%d1x(IGTZ) /)
    dGt(2,:) = (/ S%d1y(IGTX), S%d1y(IGTY), S%d1y(IGTZ) /)
    dGt(3,:) = (/ S%d1z(IGTX), S%d1z(IGTY), S%d1z(IGTZ) /)

    do i = 1, 3
      do j = 1, 3
        term2(i,j) = 0.5_dp*( sum(G%gt(:,i)*dGt(j,:)) + sum(G%gt(:,j)*dGt(i,:)) )
        term3(i,j) = sum(Gtvec(:) * Chr_low(:,i,j))
      end do
    end do

    term4 = 0.0_dp
    do i = 1, 3
      do j = 1, 3
        do l = 1, 3
          do m = 1, 3
            do k = 1, 3
              term4(i,j) = term4(i,j) + G%gtinv(l,m) * ( &
                   G%Gam(k,l,i)*Chr_low(m,j,k) + G%Gam(k,l,j)*Chr_low(m,i,k) &
                   + G%Gam(k,i,m)*Chr_low(j,k,l) )
            end do
          end do
        end do
      end do
    end do

    G%Rt = -0.5_dp*lap_g + term2 + term3 + term4

    ! --- R^phi_ij ---
    dphi_v = (/ S%d1x(IPHI), S%d1y(IPHI), S%d1z(IPHI) /)
    G%dphi = dphi_v
    d2phi(1,1) = S%d2x(IPHI);  d2phi(2,2) = S%d2y(IPHI);  d2phi(3,3) = S%d2z(IPHI)
    d2phi(1,2) = S%d2xy(IPHI); d2phi(2,1) = d2phi(1,2)
    d2phi(1,3) = S%d2xz(IPHI); d2phi(3,1) = d2phi(1,3)
    d2phi(2,3) = S%d2yz(IPHI); d2phi(3,2) = d2phi(2,3)

    do i = 1, 3
      do j = 1, 3
        DDphi(i,j) = d2phi(i,j) - sum(G%Gam(:,i,j) * dphi_v(:))
      end do
    end do

    lap_phi = sum(G%gtinv * DDphi)
    gdphi2  = 0.0_dp
    do l = 1, 3
      do m = 1, 3
        gdphi2 = gdphi2 + G%gtinv(l,m)*dphi_v(l)*dphi_v(m)
      end do
    end do

    do i = 1, 3
      do j = 1, 3
        G%Rphi(i,j) = -2.0_dp*DDphi(i,j) - 2.0_dp*G%gt(i,j)*lap_phi &
                      + 4.0_dp*dphi_v(i)*dphi_v(j) - 4.0_dp*G%gt(i,j)*gdphi2
      end do
    end do

    ! --- D_i D_j alpha (physical, from conformal quantities) ---
    da_v = (/ S%d1x(IALPHA), S%d1y(IALPHA), S%d1z(IALPHA) /)
    G%dalpha = da_v
    d2a(1,1) = S%d2x(IALPHA);  d2a(2,2) = S%d2y(IALPHA);  d2a(3,3) = S%d2z(IALPHA)
    d2a(1,2) = S%d2xy(IALPHA); d2a(2,1) = d2a(1,2)
    d2a(1,3) = S%d2xz(IALPHA); d2a(3,1) = d2a(1,3)
    d2a(2,3) = S%d2yz(IALPHA); d2a(3,2) = d2a(2,3)

    do i = 1, 3
      do j = 1, 3
        DDa_t(i,j) = d2a(i,j) - sum(G%Gam(:,i,j) * da_v(:))
      end do
    end do

    ! gt^{kl} d_l phi d_k alpha (scalar contraction, same convention as gdphi2)
    gdphi_da = 0.0_dp
    do k = 1, 3
      do l = 1, 3
        gdphi_da = gdphi_da + G%gtinv(k,l)*dphi_v(l)*da_v(k)
      end do
    end do

    do i = 1, 3
      do j = 1, 3
        G%DDa(i,j) = DDa_t(i,j) - 2.0_dp*(dphi_v(i)*da_v(j) + dphi_v(j)*da_v(i)) &
                     + 2.0_dp*G%gt(i,j)*gdphi_da
      end do
    end do

  end subroutine compute_conformal_geometry

end module geometry_mod
