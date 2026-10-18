! ===========================================================================
!  Per-operator convergence test for the production stencil.
!
!  Why this exists
!  ---------------
!  test_k_from_gauge reconstructs K_ij from (gamma, alpha, beta) and finds that
!  only the components carrying a y index are wrong, with an error that does
!  NOT fall under refinement.  That localises the defect to the cartoon
!  y-derivative but not to a specific operator.  This test does that.
!
!  The probe field
!  --------------
!  A spherically symmetric rank-2 tensor -- exactly the structure of the areal
!  Schwarzschild/Kerr spatial metric:
!
!      T_ij = fr P_ij + ft (delta_ij - P_ij),   P_ij = x_i x_j / D
!
!  with D = x^2+y^2+z^2+1 so everything is analytic everywhere, plus a scalar
!  lapse and a radial shift.  We fill the y = 0 plane with exact analytic
!  values, run the PRODUCTION compute_stencil, and compare every returned
!  derivative against the analytic derivative at the same point.
!
!  Why this is a fair test
!  -----------------------
!  The cartoon method rebuilds off-plane values by interpolating the stored
!  x-row at cylindrical radius varpi = sqrt(x^2+y^2) and rotating.  For a
!  spherically symmetric tensor that is the physically correct operation, so a
!  correct stencil should recover every derivative to truncation error.  Any
!  component or direction that fails to converge under refinement is a defect
!  in that operator, not an artefact of the symmetry.
!
!  The parity that matters
!  -----------------------
!  T_xy and T_yz are ODD in y and vanish on the plane, so their d_y is
!  genuinely nonzero (e.g. d_y T_yz = fr z/D).  The d_y of every even component
!  is exactly zero.  A stencil that returns zero for the odd ones, or that
!  fails to converge for either parity, is the bug.
!
!  Layout note: everything is at program scope on purpose.  The analytic
!  evaluation is done once per resolution into Tp/dTp and then reused.
! ===========================================================================
program test_stencil_derivs
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: alloc_state, IALPHA, IBETAY, &
                      IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ
  use excision_mod, only: initialize_excision, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  implicit none


  integer, parameter :: CA(6) = [1, 1, 1, 2, 2, 3]
  integer, parameter :: CB(6) = [1, 2, 3, 2, 3, 3]
  integer, parameter :: CV(6) = [IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ]
  real(dp), parameter :: ONE=1.0_dp, TWO=2.0_dp, THREE=3.0_dp

  real(dp), allocatable :: u(:,:,:), Tp(:,:,:), dTp(:,:,:,:)
  type(stencil_t) :: S
  real(dp) :: err(8,3), got(3), r
  real(dp) :: uu(3), D, sD, fr, ft, dfr(3), dft(3), P(3,3), dPm(3,3,3)
  real(dp) :: dl, dfrc
  integer :: i, k, c, dir, a, b, kk, ix, kz
  character(len=32) :: buf

  character(len=8), parameter :: cstr(6) = &
       [character(len=8) :: 'T_xx','T_xy','T_xz','T_yy','T_yz','T_zz']

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin=0.0_dp
  r_excise=0.30_dp              ! keep the probe annulus clear of the mask
  err=0.0_dp


  ! One resolution per process: setup_grid allocates xg/zg without freeing, so
  ! it cannot be called twice in one run.  Drive it from outside:
  !   for n in 48 96 192; do GR_NX=$n GR_NZ=$((2*n)) ./bin/test_stencil_derivs; done
  Nx=96; Nz=192
  call get_environment_variable('GR_NX',buf)
  if (buf/=' ') read(buf,*) Nx
  call get_environment_variable('GR_NZ',buf)
  if (buf/=' ') read(buf,*) Nz

  write(*,'(A,I0,A,I0)') 'Stencil operator probe at Nx=',Nx,'  Nz=',Nz
  write(*,'(A)') 'max |stencil - analytic| over 2.0 <= r <= 5.0 on the y=0 plane.'
  write(*,'(A,3ES13.3)') '  comp          d_x          d_y          d_z'

  if (.true.) then
    dx=xmax/real(Nx-1,dp); dz=2.0_dp*zmax/real(Nz-1,dp); dy=dx
    call setup_grid()
    call alloc_state(u,Nx,Nz)
    allocate(Tp(Nx,Nz,8), dTp(Nx,Nz,8,3))
    if (.not. allocated(excision_status)) allocate(excision_status(Nx,Nz))
    call initialize_excision(excision_status)
    call init_deriv_stencils()

    ! ---- analytic field and gradient on the plane y = 0 ----
    do kz=1,Nz
      do ix=1,Nx
        uu=[xg(ix), 0.0_dp, zg(kz)]
        D=uu(1)*uu(1)+uu(2)*uu(2)+uu(3)*uu(3)+ONE
        sD=sqrt(D)
        fr=ONE/sD
        ft=ONE+TWO/sD
        do kk=1,3
          dfr(kk)=-uu(kk)/D**1.5_dp
          dft(kk)=-TWO*uu(kk)/D**1.5_dp
        end do
        do a=1,3
          do b=1,3
            P(a,b)=uu(a)*uu(b)/D
            do kk=1,3
              dPm(a,b,kk)=(merge(ONE,0.0_dp,a==kk)*uu(b) &
                          + merge(ONE,0.0_dp,b==kk)*uu(a))/D &
                         - TWO*uu(a)*uu(b)*uu(kk)/D**2
            end do
          end do
        end do
        do c=1,6
          a=CA(c); b=CB(c)
          dl=merge(ONE,0.0_dp,a==b)
          Tp(ix,kz,c)=fr*P(a,b)+ft*(dl-P(a,b))
          do kk=1,3
            dTp(ix,kz,c,kk)=dfr(kk)*P(a,b)+fr*dPm(a,b,kk) &
                           +dft(kk)*(dl-P(a,b))-ft*dPm(a,b,kk)
          end do
        end do
        ! 7 = lapse = fr ; 8 = shift_y = y / D^{3/2}
        Tp(ix,kz,7)=fr
        do kk=1,3
          dTp(ix,kz,7,kk)=dfr(kk)
        end do
        Tp(ix,kz,8)=uu(2)/D**1.5_dp
        dTp(ix,kz,8,:)=0.0_dp
        dTp(ix,kz,8,1)=-THREE*uu(2)*uu(1)/D**2.5_dp
        dTp(ix,kz,8,2)=ONE/D**1.5_dp-THREE*uu(2)*uu(2)/D**2.5_dp
        dTp(ix,kz,8,3)=-THREE*uu(2)*uu(3)/D**2.5_dp
      end do
    end do

    ! ---- fill the stored plane ----
    do kz=1,Nz
      do ix=1,Nx
        do c=1,6
          u(ix,kz,CV(c))=Tp(ix,kz,c)
        end do
        u(ix,kz,IALPHA)=Tp(ix,kz,7)
        u(ix,kz,IBETAY)=Tp(ix,kz,8)
      end do
    end do

    ! ---- compare every derivative ----
    do k=2,Nz-1
      do i=5,Nx-1
        r=sqrt(xg(i)**2+zg(k)**2)
        if (r<2.0_dp .or. r>5.0_dp) cycle
        if (excision_status(i,k)/=PHYSICAL) cycle
        call compute_stencil(u,i,k,excision_status,S)
        do c=1,6
          got=[S%d1x(CV(c)),S%d1y(CV(c)),S%d1z(CV(c))]
          do dir=1,3
            err(c,dir)=max(err(c,dir),abs(got(dir)-dTp(i,k,c,dir)))
          end do
        end do
        got=[S%d1x(IALPHA),S%d1y(IALPHA),S%d1z(IALPHA)]
        do dir=1,3
          err(7,dir)=max(err(7,dir),abs(got(dir)-dTp(i,k,7,dir)))
        end do
        got=[S%d1x(IBETAY),S%d1y(IBETAY),S%d1z(IBETAY)]
        do dir=1,3
          err(8,dir)=max(err(8,dir),abs(got(dir)-dTp(i,k,8,dir)))
        end do
      end do
    end do

  do c=1,6
      write(*,'(A,3ES13.3)') '  '//cstr(c),err(c,1),err(c,2),err(c,3)
    end do
    write(*,'(A,3ES13.3)') '  lapse  ',err(7,1),err(7,2),err(7,3)
    write(*,'(A,3ES13.3)') '  beta_y ',err(8,1),err(8,2),err(8,3)
  end if

end program test_stencil_derivs