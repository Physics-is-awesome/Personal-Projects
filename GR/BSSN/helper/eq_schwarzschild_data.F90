! Full BSSN initial data for the spherical stationary 1+log Schwarzschild slice.
! Coordinates are Cartesian labels built from areal radius R; the spatial
! metric is generally non-conformally-flat in this radial coordinate.
module eq_schwarzschild_data_mod
  use kinds_mod, only: dp
  use vars_mod
  use params_mod, only: bh_mass, r_excise
  use grid_mod, only: xg, zg
  use equilibrium_1pluslog_mod, only: eq_lapse_ext, eq_shift_ext, eq_dalpha_dr, &
                                      eq_lapse_ext_valid
  implicit none
  private
  public :: set_eq_schwarzschild_id
contains
  subroutine set_eq_schwarzschild_id(u)
    real(dp), intent(out) :: u(:,:,:)
    integer :: i,k
    real(dp) :: x,z,r,alpha,q,ap,qp,krad,ktan,ktr,phi,conf
    real(dp) :: n(3),g(3,3),kij(3,3),at(3,3),gt(3,3)

    u=0.0_dp
    do k=1,size(u,2)
      do i=1,size(u,1)
        x=xg(i); z=zg(k); r=sqrt(x*x+z*z)
        if (r <= r_excise .or. r < 1.35_dp*bh_mass) then
          u(i,k,IGXX)=1.0_dp; u(i,k,IGYY)=1.0_dp; u(i,k,IGZZ)=1.0_dp
          u(i,k,IALPHA)=1.0_dp
          cycle
        end if
        if (.not. eq_lapse_ext_valid(bh_mass,r)) error stop &
          'set_eq_schwarzschild_id: lapse continuation is not positive'
        alpha=eq_lapse_ext(bh_mass,r)
        q=eq_shift_ext(bh_mass,r)
        if (r >= 2.0_dp*bh_mass) then
          ap=(2.0_dp*bh_mass/r**2-4.0_dp*q*q/r)/(2.0_dp*alpha-q*q)
        else
          ap=eq_dalpha_dr(bh_mass,r)
        end if
        qp=q*(0.5_dp*ap-2.0_dp/r)
        krad=qp/(alpha*alpha)
        ktan=q/r
        ktr=qp+2.0_dp*q/r
        phi=-log(alpha)/6.0_dp
        conf=exp(-4.0_dp*phi)
        n=(/x/r,0.0_dp,z/r/)
        g=0.0_dp
        g(1,1)=1.0_dp; g(2,2)=1.0_dp; g(3,3)=1.0_dp
        g=g+(1.0_dp/(alpha*alpha)-1.0_dp)*outer3(n,n)
        kij=ktan*identity3()+(krad-ktan)*outer3(n,n)
        at=conf*(kij-g*ktr/3.0_dp)
        gt=exp(-4.0_dp*phi)*g

        u(i,k,IPHI)=phi
        u(i,k,IGXX)=gt(1,1); u(i,k,IGXY)=gt(1,2); u(i,k,IGXZ)=gt(1,3)
        u(i,k,IGYY)=gt(2,2); u(i,k,IGYZ)=gt(2,3); u(i,k,IGZZ)=gt(3,3)
        u(i,k,IK)=ktr
        u(i,k,IAXX)=at(1,1); u(i,k,IAXY)=at(1,2); u(i,k,IAXZ)=at(1,3)
        u(i,k,IAYY)=at(2,2); u(i,k,IAYZ)=at(2,3); u(i,k,IAZZ)=at(3,3)
        u(i,k,IALPHA)=alpha
        u(i,k,IBETAX)=alpha*q*n(1)
        u(i,k,IBETAZ)=alpha*q*n(3)
      end do
    end do
  end subroutine set_eq_schwarzschild_id

  pure function outer3(a,b) result(m)
    real(dp), intent(in) :: a(3),b(3)
    real(dp) :: m(3,3)
    integer :: i,j
    do j=1,3
      do i=1,3
        m(i,j)=a(i)*b(j)
      end do
    end do
  end function outer3

  pure function identity3() result(m)
    real(dp) :: m(3,3)
    m=0.0_dp
    m(1,1)=1.0_dp; m(2,2)=1.0_dp; m(3,3)=1.0_dp
  end function identity3
end module eq_schwarzschild_data_mod
