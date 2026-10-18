! ===========================================================================
!  Per-operator convergence probe for the production stencil.
!
!  Why this exists
!  ---------------
!  test_k_from_gauge reconstructs K_ij from (gamma, alpha, beta) and finds that
!  only the components carrying a y index are wrong, with an error that does
!  NOT fall under refinement.  A first pass over the first derivatives found
!  that d_y of an odd-parity VECTOR component returns essentially zero and
!  has observed order ~0, while d_x/d_z are order ~4 and d_y of the metric
!  components is order ~2.  This program maps the full blast radius: all
!  first derivatives, all second derivatives, and all mixed derivatives, for
!  every tensor component plus all three shift components.
!
!  The probe field
!  --------------
!  A spherically symmetric rank-2 tensor -- exactly the structure of the areal
!  Schwarzschild/Kerr spatial metric:
!
!      T_ij = fr P_ij + ft (delta_ij - P_ij),   P_ij = x_i x_j / D
!
!  with D = x^2+y^2+z^2+1 so everything is analytic everywhere, plus a scalar
!  lapse and a radial shift vector b_i = x_i / D^{3/2}.  The metric components
!  and b_x, b_z are EVEN in y; T_xy, T_yz and b_y are ODD in y and vanish on
!  the plane.  That parity split is what separates a parity bug from a
!  general y-derivative bug.
!
!  Why this is a fair test
!  -----------------------
!  The cartoon method rebuilds off-plane values by interpolating the stored
!  x-row at cylindrical radius varpi = sqrt(x^2+y^2) and rotating.  For a
!  spherically symmetric field that is the physically correct operation, so a
!  correct stencil must recover every derivative to truncation error.
!
!  Reference derivatives
!  --------------------
!  The "analytic" values are obtained by central differences of the closed
!  form in three dimensions with a deliberately small step (h1 for first,
!  h2 for second derivatives).  That reference is accurate to ~1e-10, i.e.
!  six orders below the smallest stencil error being measured, so any
!  disagreement is attributable to the stencil rather than to the reference.
!  Hand-derived second derivatives were rejected here precisely because they
!  are easy to get subtly wrong.
!
!  One resolution per process: setup_grid allocates xg/zg without freeing, so
!  it cannot be called twice in a single run.  Drive it from outside:
!     for n in 48 96 192; do GR_NX=$n GR_NZ=$((2*n)) ./bin/test_stencil_derivs; done
! ===========================================================================
program test_stencil_derivs
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: alloc_state, NVARS, IALPHA, IBETAX, IBETAY, IBETAZ, &
                      IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ
  use excision_mod, only: initialize_excision, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  use cartoon_mod, only: get_neighbor
  implicit none

  integer, parameter :: NC = 10                      ! probe components
  integer, parameter :: CA(6) = [1, 1, 1, 2, 2, 3]
  integer, parameter :: CB(6) = [1, 2, 3, 2, 3, 3]
  integer, parameter :: CV(6) = [IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ]
  real(dp), parameter :: H1 = 1.0e-4_dp
  real(dp), parameter :: H2 = 1.0e-3_dp
  real(dp), parameter :: ONE=1.0_dp, TWO=2.0_dp, THREE=3.0_dp

  real(dp), allocatable :: u(:,:,:)
  type(stencil_t) :: S
  real(dp) :: err(NC,9), got(9), got9(9), r, xp, zp
  integer :: i, k, c, ix, kz
  character(len=32) :: buf

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin=0.0_dp
  r_excise=0.30_dp              ! keep the probe annulus clear of the mask
  err=0.0_dp

  Nx=96; Nz=192
  call get_environment_variable('GR_NX',buf)
  if (buf/=' ') read(buf,*) Nx
  call get_environment_variable('GR_NZ',buf)
  if (buf/=' ') read(buf,*) Nz

  dx=xmax/real(Nx-1,dp); dz=2.0_dp*zmax/real(Nz-1,dp); dy=dx
  call setup_grid()
  call alloc_state(u,Nx,Nz)
  if (.not. allocated(excision_status)) allocate(excision_status(Nx,Nz))
  call initialize_excision(excision_status)
  call init_deriv_stencils()

  ! ---- store exact analytic values on the y = 0 plane ----
  do kz=1,Nz
    do ix=1,Nx
      do c=1,6
        u(ix,kz,CV(c))=pfield(xg(ix),0.0_dp,zg(kz),c)
      end do
      u(ix,kz,IALPHA)=pfield(xg(ix),0.0_dp,zg(kz),7)
      u(ix,kz,IBETAX)=pfield(xg(ix),0.0_dp,zg(kz),8)
      u(ix,kz,IBETAY)=pfield(xg(ix),0.0_dp,zg(kz),9)
      u(ix,kz,IBETAZ)=pfield(xg(ix),0.0_dp,zg(kz),10)
    end do
  end do

  ! ---- compare every operator ----
  do k=2,Nz-1
    do i=5,Nx-1
      r=sqrt(xg(i)**2+zg(k)**2)
      if (r<2.0_dp .or. r>5.0_dp) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      call compute_stencil(u,i,k,excision_status,S)
      xp=xg(i); zp=zg(k)
      do c=1,6
        got=[S%d1x(CV(c)),S%d1y(CV(c)),S%d1z(CV(c)), &
             S%d2x(CV(c)),S%d2y(CV(c)),S%d2z(CV(c)), &
             S%d2xy(CV(c)),S%d2xz(CV(c)),S%d2yz(CV(c))]
        call accumulate(c, got, xp, zp)
      end do
      got=[S%d1x(IALPHA),S%d1y(IALPHA),S%d1z(IALPHA), &
           S%d2x(IALPHA),S%d2y(IALPHA),S%d2z(IALPHA), &
           S%d2xy(IALPHA),S%d2xz(IALPHA),S%d2yz(IALPHA)]
      call accumulate(7, got, xp, zp)
      got=[S%d1x(IBETAX),S%d1y(IBETAX),S%d1z(IBETAX), &
           S%d2x(IBETAX),S%d2y(IBETAX),S%d2z(IBETAX), &
           S%d2xy(IBETAX),S%d2xz(IBETAX),S%d2yz(IBETAX)]
      call accumulate(8, got, xp, zp)
      got9=0.0_dp
      got9(2)=S%d1y(IBETAY); got9(3)=S%d1z(IBETAY)
      got9(5)=S%d2y(IBETAY); got9(6)=S%d2z(IBETAY)
      got9(7)=S%d2xy(IBETAY); got9(8)=S%d2xz(IBETAY); got9(9)=S%d2yz(IBETAY)
      call accumulate(9, got9, xp, zp)
      got=[S%d1x(IBETAZ),S%d1y(IBETAZ),S%d1z(IBETAZ), &
           S%d2x(IBETAZ),S%d2y(IBETAZ),S%d2z(IBETAZ), &
           S%d2xy(IBETAZ),S%d2xz(IBETAZ),S%d2yz(IBETAZ)]
      call accumulate(10, got, xp, zp)
    end do
  end do

  write(*,'(A,I0,A,I0)') 'Stencil operator probe at Nx=',Nx,'  Nz=',Nz
  write(*,'(A)') 'max |stencil - reference| over 2.0 <= r <= 5.0 on y=0.'
  write(*,'(A)') ''
  write(*,'(A,3ES12.3)') '  first derivatives        d_x          d_y          d_z'
  do c=1,NC
    write(*,'(A,3ES12.3)') '  '//pname(c),err(c,1),err(c,2),err(c,3)
  end do
  write(*,'(A)')
  write(*,'(A,3ES12.3)') '  pure second derivs      d2x         d2y         d2z'
  do c=1,NC
    write(*,'(A,3ES12.3)') '  '//pname(c),err(c,4),err(c,5),err(c,6)
  end do
  write(*,'(A)')
  write(*,'(A,3ES12.3)') '  mixed derivatives      d2xy        d2xz        d2yz'
  do c=1,NC
    write(*,'(A,3ES12.3)') '  '//pname(c),err(c,7),err(c,8),err(c,9)
  end do

  call xy_scaling

  call decompose

  call recon_range

contains

  pure function pname(c) result(s)
    integer,intent(in) :: c
    character(len=8) :: s
    select case (c)
      case(1); s='T_xx'; case(2); s='T_xy'; case(3); s='T_xz'
      case(4); s='T_yy'; case(5); s='T_yz'; case(6); s='T_zz'
      case(7); s='lapse'; case(8); s='b_x';   case(9); s='b_y'
      case default; s='b_z'
    end select
  end function pname

  ! Closed-form probe field, component c, at arbitrary (x,y,z).
  !   1..6 T_xx T_xy T_xz T_yy T_yz T_zz   (spherically symmetric tensor)
  !   7    lapse = 1/sqrt(D)
  !   8..10 shift b_i = x_i / D^{3/2}
  function pfield(x,y,z,c) result(v)
    real(dp),intent(in) :: x,y,z
    integer,intent(in) :: c
    real(dp) :: v
    real(dp) :: uu(3), D, fr, ft, P, dl
    integer :: a,b
    uu=[x,y,z]
    D=uu(1)*uu(1)+uu(2)*uu(2)+uu(3)*uu(3)+ONE
    fr=ONE/sqrt(D)
    ft=ONE+TWO/sqrt(D)
    if (c>=1 .and. c<=6) then
      a=CA(c); b=CB(c)
      P=uu(a)*uu(b)/D
      dl=merge(ONE,0.0_dp,a==b)
      v=fr*P+ft*(dl-P)
    else if (c==7) then
      v=fr
    else
      v=uu(c-7)/D**1.5_dp
    end if
  end function pfield

  subroutine accumulate(c, got, xp, zp)
    integer,intent(in) :: c
    real(dp),intent(in) :: got(9), xp, zp
    real(dp) :: ref(9)
    call refders(xp, zp, c, ref)
    err(c,:)=max(err(c,:),abs(got(1:9)-ref(1:9)))
  end subroutine accumulate

  ! beta_y is odd in y and vanishes on the plane, so its d_x, d2x and d2xz
  ! are identically zero there; those stencil entries carry no information.
  ! Compare only the seven operators that are non-trivial on the plane.
  subroutine accumulate_vec_y(c, got, xp, zp)
    integer,intent(in) :: c
    real(dp),intent(in) :: got(7), xp, zp
    real(dp) :: ref(9), e(9)
    call refders(xp, zp, c, ref)
    e=0.0_dp
    e(2)=abs(got(1)-ref(2))     ! d_y
    e(3)=abs(got(2)-ref(3))     ! d_z
    e(5)=abs(got(3)-ref(5))     ! d2y
    e(6)=abs(got(4)-ref(6))     ! d2z
    e(7)=abs(got(5)-ref(7))     ! d2xy
    e(8)=abs(got(6)-ref(8))     ! d2xz
    e(9)=abs(got(7)-ref(9))     ! d2yz
    err(c,:)=max(err(c,:),e)
  end subroutine accumulate_vec_y

  ! Reference derivatives by central differences of pfield in three dimensions.
  subroutine refders(xp, zp, c, ref)
    real(dp),intent(in) :: xp, zp
    integer,intent(in) :: c
    real(dp),intent(out) :: ref(9)
    real(dp) :: pm(3), pp(3), a, b
    integer :: kk
    ! first derivatives, h = H1
    do kk=1,3
      pm=shift(xp,zp,-H1,kk); pp=shift(xp,zp,H1,kk)
      ref(kk)=(pfield(pp(1),pp(2),pp(3),c)-pfield(pm(1),pm(2),pm(3),c))/(2.0_dp*H1)
    end do
    ! pure second derivatives, h = H2
    do kk=4,6
      pm=shift(xp,zp,-H2,kk-3); pp=shift(xp,zp,H2,kk-3)
      ref(kk)=(pfield(pp(1),pp(2),pp(3),c)-2.0_dp*pfield(xp,0.0_dp,zp,c) &
              +pfield(pm(1),pm(2),pm(3),c))/H2**2
    end do
    ! mixed derivatives, h = H2, one axis at a time
    a=H2; b=H2
    ! d2xy : vary x and y
    pm=[xp-a,0.0_dp-a,zp]; pp=[xp+a,0.0_dp+a,zp]
    ref(7)=(pfield(xp+a,a,zp,c)-pfield(xp-a,a,zp,c) &
           -pfield(xp+a,-a,zp,c)+pfield(xp-a,-a,zp,c))/(4.0_dp*a*b)
    ! d2xz : vary x and z
    pm=[xp-a,0.0_dp,zp-a]; pp=[xp+a,0.0_dp,zp+a]
    ref(8)=(pfield(xp+a,0.0_dp,zp+a,c)-pfield(xp-a,0.0_dp,zp+a,c) &
           -pfield(xp+a,0.0_dp,zp-a,c)+pfield(xp-a,0.0_dp,zp-a,c))/(4.0_dp*a*b)
    ! d2yz : vary y and z
    ref(9)=(pfield(xp,a,zp+a,c)-pfield(xp,a,zp-a,c) &
           -pfield(xp,-a,zp+a,c)+pfield(xp,-a,zp-a,c))/(4.0_dp*a*b)
  end subroutine refders

  function shift(xp, zp, h, dir) result(p)
    real(dp),intent(in) :: xp, zp, h
    integer,intent(in) :: dir
    real(dp) :: p(3)
    p=[xp,0.0_dp,zp]
    p(dir)=p(dir)+h
  end function shift


  ! Does the y-derivative error scale as 1/x, as the rotation analysis predicts?
  ! The y difference is  2 sin(phi) * Vx_interp / (2 dy)  with phi ~ dy/x, so
  ! the interpolation error (Vx_interp - Vx_true), which is O(dx^2), enters
  ! DIVIDED BY x.  Near the axis that amplification dominates and it does not
  ! vanish under refinement -- exactly the non-convergence we see.
  subroutine xy_scaling
    integer :: ic, kc, cc, ii
    real(dp) :: rr, zz2, xx2, e3(3), gg8, rf(9)
    write(*,'(A)') '=== y-derivative error vs x at fixed r = 3.5 (expect ~1/x) ==='
    write(*,'(A,3ES13.4,A,ES13.4)') '          x          |d_y| err      x*|d_y| err'
    rr = 3.5_dp
    zz2 = 0.7_dp
    kc = minloc(abs(zg-zz2), dim=1)
    do ii=1,8
      xx2 = 0.6_dp + 0.4_dp*real(ii,dp)
      zz2 = sqrt(max(rr*rr - xx2*xx2, 0.0_dp))
      ic = minloc(abs(xg-xx2), dim=1)
      kc = minloc(abs(zg-zz2), dim=1)
      do cc=1,6
        u(ic,kc,CV(cc)) = pfield(xx2,0.0_dp,zz2,cc)
      end do
      u(ic,kc,IALPHA) = pfield(xx2,0.0_dp,zz2,7)
      u(ic,kc,IBETAY) = pfield(xx2,0.0_dp,zz2,9)
      call compute_stencil(u,ic,kc,excision_status,S)
      call refders(xx2, zz2, 2, rf);  e3(1)=abs(S%d1y(IGXY)-rf(2))
      call refders(xx2, zz2, 5, rf);  e3(2)=abs(S%d1y(IGYZ)-rf(2))
      call refders(xx2, zz2, 9, rf);  e3(3)=abs(S%d1y(IBETAY)-rf(2))
      gg8=maxval(e3)
      write(*,'(A,2ES13.4,A,ES13.4)') ' ', xx2, gg8, '   ', xx2*gg8
    end do
    write(*,'(A)') '  If x*|err| is roughly constant, the error is O(dx^2 / x) and'
    write(*,'(A)') '  refining alone cannot remove it.'
  end subroutine xy_scaling


  ! ------------------------------------------------------------------------
  !  DECOMPOSITION of the y-derivative error.
  !
  !  The stencil forms  d_y = (ghost(x,+dy) - ghost(x,-dy)) / (2 dy),  where
  !  ghost() is the cartoon reconstruction: interpolate the stored x-row at
  !  varpi = sqrt(x^2+y^2), then rotate.  There are two candidate error
  !  sources, and they have very different fixes, so separate them by MEASUREMENT
  !  rather than by argument:
  !
  !    A) interpolation: the radially interpolated value is not the true value
  !       at that varpi.  Measured as |ghost - analytic| for a single ghost
  !       point, with no differencing at all.
  !    B) assembly: the difference/rotation logic turns an accurate ghost into
  !       an inaccurate derivative.  Measured by putting the ANALYTIC
  !       off-plane values into the same difference and seeing what is left.
  !
  !  If A dominates -> fix interp_radial.  If B dominates -> fix the formula.
  ! ------------------------------------------------------------------------
  subroutine decompose
    integer :: ic, kc, cc, ii
    real(dp) :: xx2, zz2, gp(NVARS), gm(NVARS), ap(NVARS), am(NVARS)
    real(dp) :: einterp, eassem, etotal, gd_y, gd_y_ref, pfr(9)
    real(dp) :: yy
    write(*,'(A)') '=== DECOMPOSITION: interpolation vs assembly in d_y ==='
    write(*,'(A)') '  field compared: T_xy (an odd tensor component, d_y nonzero)'
    write(*,'(A)') ''
    write(*,'(A,5ES13.4)') '         x      interp_err   ghost_dy_err  analyticCD_err   ref_dy'
    do ii=1,6
      xx2 = 0.9_dp + 0.55_dp*real(ii,dp)
      zz2 = sqrt(max(3.5_dp**2 - xx2*xx2, 0.0_dp))
      ic = minloc(abs(xg-xx2), dim=1)
      kc = minloc(abs(zg-zz2), dim=1)
      do cc=1,6
        u(ic,kc,CV(cc)) = pfield(xg(ic),0.0_dp,zg(kc),cc)
      end do
      u(ic,kc,IALPHA) = pfield(xg(ic),0.0_dp,zg(kc),7)
      u(ic,kc,IBETAY) = pfield(xg(ic),0.0_dp,zg(kc),9)
      yy = dy
      gp = get_neighbor(u, ic, kc, 0, 0,  yy)
      gm = get_neighbor(u, ic, kc, 0, 0, -yy)
      do cc=1,10
        ap(cc) = pfield(xg(ic),  yy, zg(kc), cc)
        am(cc) = pfield(xg(ic), -yy, zg(kc), cc)
      end do
      ! A) interpolation only, component 2 = T_xy
      einterp = abs(gp(IGXY) - ap(2))
      ! the stencil's own result, same component
      gd_y = (gp(IGXY) - gm(IGXY))/(2.0_dp*dy)
      ! B) same difference, but from ANALYTIC off-plane values
      gd_y_ref = (ap(2) - am(2))/(2.0_dp*dy)
      call refders(xg(ic), zg(kc), 2, pfr)
      etotal = abs(gd_y - pfr(2))
      ! assembly error = total minus what the analytic difference itself misses
      eassem = abs(gd_y - gd_y_ref)
      write(*,'(A,5ES13.4)') ' ', xx2, einterp, etotal, abs(gd_y_ref-pfr(2)), gd_y_ref
    end do
    write(*,'(A)') ''
    write(*,'(A)') '  col 2 = pure interpolation error in one ghost value'
    write(*,'(A)') '  col 3 = error of the stencil d_y vs the converged reference'
    write(*,'(A)') '  col 4 = the true d_y (for scale)'
    write(*,'(A)') ''
  write(*,'(A)') '  col2 = pure interpolation error in ONE ghost value'
  write(*,'(A)') '  col3 = stencil d_y (from ghosts) vs converged reference'
  write(*,'(A)') '  col4 = ANALYTIC central difference (exact off-plane values)'
  write(*,'(A)') '       vs the same converged reference'
  write(*,'(A)') '  col5 = the true d_y, for scale'
  write(*,'(A)') ''
  write(*,'(A)') '  CONTROL: if col4 ~= col3, the error is the O(dy^2) central-'
  write(*,'(A)') '  difference truncation error itself, not anything the cartoon'
  write(*,'(A)') '  does wrong.  Then the y-path cannot be made 4th order by'
  write(*,'(A)') '  better interpolation -- it is limited by the difference.'
  write(*,'(A)') '  If col4 << col3, the cartoon IS adding error on top.'
  end subroutine decompose


  ! ------------------------------------------------------------------------
  !  Can the y-derivative be raised above 2nd order?
  !
  !  The decomposition showed d1y = (g(+dy)-g(-dy))/(2dy) is limited purely by
  !  the O(dy^2) truncation of that difference -- the cartoon reconstruction is
  !  accurate to ~1e-7 at y=+-dy.  A 4th-order stencil needs ghosts at +-2dy as
  !  well.  So the question is: does the reconstruction stay accurate OUT TO
  !  those larger |y|, or does it break down there?
  !
  !  Reconstruction error vs |y|/dy, for an ODD component (T_xy, whose
  !  off-plane value is built entirely by the rotation) and an EVEN one (T_xx).
  !  The 4th-order column is what the stencil would have to achieve.
  ! ------------------------------------------------------------------------
  subroutine recon_range
    integer :: ic, kc, cc, im, ii
    real(dp) :: xx2, zz2, gp(NVARS), ap(NVARS), yy, fac
    write(*,'(A)') ''
    write(*,'(A)') '=== reconstruction accuracy vs |y| (needed for higher order) ==='
    write(*,'(A,2ES13.4)') '       x      err_odd(T_xy)   err_even(T_xx)'
    do ii=1,3
      xx2 = 1.6_dp + 0.8_dp*real(ii,dp)
      zz2 = sqrt(max(3.5_dp**2 - xx2*xx2, 0.0_dp))
      ic = minloc(abs(xg-xx2), dim=1)
      kc = minloc(abs(zg-zz2), dim=1)
      do cc=1,6
        u(ic,kc,CV(cc)) = pfield(xg(ic),0.0_dp,zg(kc),cc)
      end do
      u(ic,kc,IALPHA) = pfield(xg(ic),0.0_dp,zg(kc),7)
      u(ic,kc,IBETAY) = pfield(xg(ic),0.0_dp,zg(kc),9)
      call compute_stencil(u,ic,kc,excision_status,S)
      write(*,'(A,3ES13.4)') '   y/dx = 1 :  ', &
        recon_err(ic,kc,xg(ic),zg(kc), 1.0_dp,IGXY,2,ap), &
        recon_err(ic,kc,xg(ic),zg(kc), 1.0_dp,IGXX,1,ap)
      write(*,'(A,3ES13.4)') '   y/dx = 2 :  ', &
        recon_err(ic,kc,xg(ic),zg(kc), 2.0_dp,IGXY,2,ap), &
        recon_err(ic,kc,xg(ic),zg(kc), 2.0_dp,IGXX,1,ap)
      write(*,'(A,3ES13.4)') '   y/dx = 4 :  ', &
        recon_err(ic,kc,xg(ic),zg(kc), 4.0_dp,IGXY,2,ap), &
        recon_err(ic,kc,xg(ic),zg(kc), 4.0_dp,IGXX,1,ap)
      write(*,'(A)') ''
    end do
    write(*,'(A)') '  If these stay ~1e-7 out to y/dx=4, a 4th-order y stencil is'
    write(*,'(A)') '  available and the 2nd-order limit is removable.  If they'
    write(*,'(A)') '  blow up like y^3, the reconstruction itself caps the order.'
  end subroutine recon_range

  ! comp  = index into the cartoon state array (e.g. IGXY)
  ! pcomp = index into pfield's component list (T_xy is pcomp 2, T_xx is 1)
  function recon_err(ic, kc, xx, zz, yfrac, comp, pcomp, scratch) result(e)
    integer, intent(in) :: ic, kc, comp, pcomp
    real(dp), intent(in) :: xx, zz, yfrac
    real(dp), intent(inout) :: scratch(NVARS)
    real(dp) :: yy, e
    yy = yfrac*dy
    scratch = get_neighbor(u, ic, kc, 0, 0, yy)
    e = abs(scratch(comp) - pfield(xx, yy, zz, pcomp))
  end function recon_err

end program test_stencil_derivs
