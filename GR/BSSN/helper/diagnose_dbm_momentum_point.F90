! Compare the production DBM state and seven-piece momentum constraint at a
! near-axis point with an off-axis point at the same Euclidean radius.
! Usage: make diagnose-dbm-momentum-point \
!          MOMENTUM_PAR=parameter_files/vacuum_kerr_192.par \
!          MOMENTUM_Z=-1.535248
program diagnose_dbm_momentum_point
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, bh_mass, bh_spin, init_params
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id, state_at
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, &
                          excision_reset_geometry, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint
  use geometry_mod, only: sym3_inverse
  implicit none

  real(dp), allocatable :: u(:,:,:), M(:,:,:), Mterms(:,:,:,:)
  character(len=256) :: par_file, arg
  real(dp) :: target_z, phi, gt(3,3), at(3,3), Ktr, alpha, beta(3)
  real(dp) :: dmax, dgt, dat, r_target, dr, best_dr, r_here
  integer :: i, k, ic, kc, j, ios, reason
  logical :: ok

  par_file = 'parameter_files/vacuum_kerr_192.par'
  target_z = -1.535248_dp
  call get_command_argument(1, par_file)
  if (len_trim(par_file) == 0) par_file = 'parameter_files/vacuum_kerr_192.par'
  arg = ''; call get_command_argument(2, arg)
  if (len_trim(arg) > 0) read(arg,*,iostat=ios) target_z

  call init_params(trim(par_file))
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(M(Nx,Nz,3), Mterms(Nx,Nz,3,7))
  call set_eq_kerr_trumpet_id(u, bh_mass, bh_spin, bh_mass)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call excision_reset_geometry()
  call extrapolate_excised(u)
  call init_deriv_stencils()
  call momentum_constraint(u, M, Mterms)

  ! First positive x point, nearest z to the documented axis-adjacent point.
  i = 2; k = minloc(abs(zg-target_z), dim=1)
  r_target = sqrt(xg(i)**2 + zg(k)**2)
  write(*,'(A,I0,A,I0,A,2ES15.7,A,I0)') 'grid=',Nx,'x',Nz,' point=', &
       xg(i),zg(k),' mask=',excision_status(i,k)
  call report_point('near-axis', i, k)
  call state_at(xg(i),0.0_dp,zg(k),bh_mass,bh_spin,bh_mass, &
       phi,gt,at,Ktr,alpha,beta,ok,reason)
  if (.not. ok) then
    write(*,'(A,I0)') 'analytic state_at rejected near-axis sample, reason=',reason
  else
    call compare_point(i,k)
  end if

  ! Same-radius control with x at least five grid spacings from the axis.
  ic=0; kc=0; best_dr=huge(1.0_dp)
  do j=2,Nz-1
    block
      integer :: ii
      do ii=2,Nx-1
        if (xg(ii) < 5.0_dp*max(dx,dz)) cycle
        if (excision_status(ii,j) /= PHYSICAL) cycle
        r_here=sqrt(xg(ii)**2+zg(j)**2)
        dr=abs(r_here-r_target)
        if (dr < best_dr) then
          best_dr=dr; ic=ii; kc=j
        end if
      end do
    end block
  end do
  if (ic > 0) then
    write(*,'(A,2ES15.7,A,ES12.4)') 'control x,z=',xg(ic),zg(kc),' |dr|=',best_dr
    call report_point('off-axis',ic,kc)
    call compare_point(ic,kc)
  else
    write(*,'(A)') 'No valid off-axis control point found.'
  end if

contains

  subroutine report_point(label,ii,kk)
    character(len=*), intent(in) :: label
    integer, intent(in) :: ii,kk
    write(*,'(A,A,A,3ES14.5)') '--- ',trim(label),' Mx,My,Mz=',M(ii,kk,:)
    do j=1,3
      write(*,'(A,I0,A,7(1X,ES13.5))') '  M',j,' pieces=',Mterms(ii,kk,j,:)
      write(*,'(A,I0,A,ES13.5,A,ES13.5)') '  M',j,' sum=', &
           sum(Mterms(ii,kk,j,:)),' stored=',M(ii,kk,j)
    end do
  end subroutine report_point

  subroutine compare_point(ii,kk)
    integer, intent(in) :: ii,kk
    real(dp) :: p0,g0(3,3),a0(3,3),k0,al0,b0(3),qgt,qat
    logical :: good
    integer :: why
    call state_at(xg(ii),0.0_dp,zg(kk),bh_mass,bh_spin,bh_mass, &
         p0,g0,a0,k0,al0,b0,good,why)
    if (.not.good) then
      write(*,'(A,I0)') '  analytic comparison unavailable, reason=',why
      return
    end if
    qgt=maxval(abs(reshape([u(ii,kk,IGXX),u(ii,kk,IGXY),u(ii,kk,IGXZ), &
         u(ii,kk,IGXY),u(ii,kk,IGYY),u(ii,kk,IGYZ),u(ii,kk,IGXZ), &
         u(ii,kk,IGYZ),u(ii,kk,IGZZ)],[3,3])-g0))
    qat=maxval(abs(reshape([u(ii,kk,IAXX),u(ii,kk,IAXY),u(ii,kk,IAXZ), &
         u(ii,kk,IAXY),u(ii,kk,IAYY),u(ii,kk,IAYZ),u(ii,kk,IAXZ), &
         u(ii,kk,IAYZ),u(ii,kk,IAZZ)],[3,3])-a0))
    dmax=max(abs(u(ii,kk,IPHI)-p0),abs(u(ii,kk,IK)-k0), &
         abs(u(ii,kk,IALPHA)-al0),maxval(abs(u(ii,kk,IBETAX:IBETAZ)-b0)),qgt,qat)
    dgt=qgt; dat=qat
    write(*,'(A,3ES13.5)') '  state max errors (all, gtilde, Atilde)=',dmax,dgt,dat
    block
      real(dp) :: Madm(3)
      call adm_momentum_exact(xg(ii),0.0_dp,zg(kk),Madm)
      write(*,'(A,3ES14.6)') '  independent ADM-form M=',Madm
    end block
  end subroutine compare_point

  subroutine adm_data_at(x,y,z,gam,Kcov,Ktrace,good)
    real(dp), intent(in) :: x,y,z
    real(dp), intent(out) :: gam(3,3),Kcov(3,3),Ktrace
    logical, intent(out) :: good
    real(dp) :: ph0,gt0(3,3),a0(3,3),al0,b0(3),fac
    call state_at(x,y,z,bh_mass,bh_spin,bh_mass,ph0,gt0,a0,Ktrace,al0,b0,good)
    if (.not.good) return
    fac=exp(4.0_dp*ph0)
    gam=fac*gt0
    Kcov=fac*(a0+gt0*Ktrace/3.0_dp)
  end subroutine adm_data_at

  subroutine adm_momentum_exact(x,y,z,Mout)
    real(dp), intent(in) :: x,y,z
    real(dp), intent(out) :: Mout(3)
    real(dp), parameter :: h=1.0e-4_dp
    real(dp) :: gam0(3,3),K0(3,3),Ktr0,gi(3,3),detg
    real(dp) :: Kup(3,3),P(3,3),dPder(3,3,3),dg(3,3,3),G(3,3,3)
    real(dp) :: gp(3,3),gm(3,3),kp(3,3),km(3,3),ktp,ktm
    real(dp) :: gip(3,3),gim(3,3),detp,detm,Kup_p(3,3),Kup_m(3,3)
    real(dp) :: Pp(3,3),Pm(3,3),xp,yp,zp,xm,ym,zm
    integer :: d,a,b,c,l
    logical :: goodp,goodm
    call adm_data_at(x,y,z,gam0,K0,Ktr0,goodp)
    if (.not.goodp) then
      Mout=huge(1.0_dp); return
    end if
    call sym3_inverse(gam0,gi,detg)
    call raise_k(K0,gi,Kup)
    P=Kup-gi*Ktr0
    do d=1,3
      xp=x;yp=y;zp=z;xm=x;ym=y;zm=z
      select case(d)
      case(1);xp=x+h;xm=x-h
      case(2);yp=y+h;ym=y-h
      case(3);zp=z+h;zm=z-h
      end select
      call adm_data_at(xp,yp,zp,gp,kp,ktp,goodp)
      call adm_data_at(xm,ym,zm,gm,km,ktm,goodm)
      if (.not.(goodp.and.goodm)) then
        Mout=huge(1.0_dp); return
      end if
      dg(d,:,:)=(gp-gm)/(2.0_dp*h)
      call sym3_inverse(gp,gip,detp); call sym3_inverse(gm,gim,detm)
      call raise_k(kp,gip,Kup_p); call raise_k(km,gim,Kup_m)
      Pp=Kup_p-gip*ktp; Pm=Kup_m-gim*ktm
      dPder(d,:,:)=(Pp-Pm)/(2.0_dp*h)
    end do
    do a=1,3
      do b=1,3
        do c=1,3
          G(a,b,c)=0.0_dp
          do l=1,3
            G(a,b,c)=G(a,b,c)+0.5_dp*gi(a,l)*(dg(b,l,c)+dg(c,l,b)-dg(l,b,c))
          end do
        end do
      end do
    end do
    Mout=0.0_dp
    do a=1,3
      do b=1,3
        Mout(a)=Mout(a)+dPder(b,a,b)
        do c=1,3
          Mout(a)=Mout(a)+G(a,b,c)*P(c,b)+G(b,b,c)*P(a,c)
        end do
      end do
    end do
  end subroutine adm_momentum_exact

  subroutine raise_k(Kcov,ginv,Kup)
    real(dp), intent(in) :: Kcov(3,3),ginv(3,3)
    real(dp), intent(out) :: Kup(3,3)
    integer :: a,b,c,d
    Kup=0.0_dp
    do a=1,3
      do b=1,3
        do c=1,3
          do d=1,3
            Kup(a,b)=Kup(a,b)+ginv(a,c)*ginv(b,d)*Kcov(c,d)
          end do
        end do
      end do
    end do
  end subroutine raise_k

end program diagnose_dbm_momentum_point
