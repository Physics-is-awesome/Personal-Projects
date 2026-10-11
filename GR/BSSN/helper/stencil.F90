module stencil_mod

  use kinds_mod,   only: dp
  use params_mod,  only: dx, dy, dz, Nx, Nz
  use vars_mod,    only: NVARS
  use cartoon_mod, only: get_neighbor
  use excision_mod, only: EXCISED

  implicit none

  private
  public :: stencil_t
  public :: compute_stencil

  !=======================================================================
  ! BSSN finite-difference stencil
  !=======================================================================

  type :: stencil_t

    real(dp) :: val(NVARS)

    real(dp) :: d1x(NVARS)
    real(dp) :: d1y(NVARS)
    real(dp) :: d1z(NVARS)

    real(dp) :: d2x(NVARS)
    real(dp) :: d2y(NVARS)
    real(dp) :: d2z(NVARS)

    real(dp) :: d2xy(NVARS)
    real(dp) :: d2xz(NVARS)
    real(dp) :: d2yz(NVARS)

  end type stencil_t


contains


  !=======================================================================
  ! COMPUTE STENCIL
  !
  ! The x and z derivatives are excision-aware.
  !
  ! The y derivatives are still supplied by the Cartoon method.
  !
  ! mask(i,k) =
  !
  !     EXCISED  -> point is inside the excision region
  !     anything else -> usable exterior/ghost point
  !
  !=======================================================================

  subroutine compute_stencil(u, i, k, mask, S)

    real(dp), intent(in)    :: u(:,:,:)
    integer,  intent(in)    :: i, k
    integer,  intent(in)    :: mask(:,:)
    type(stencil_t), intent(out) :: S

    real(dp) :: c0(NVARS)

    ! Cartesian neighbors
    real(dp) :: xp(NVARS), xm(NVARS)
    real(dp) :: zp(NVARS), zm(NVARS)

    ! Second neighbors
    real(dp) :: xp2(NVARS), xm2(NVARS)
    real(dp) :: zp2(NVARS), zm2(NVARS)

    ! Cartoon y neighbors
    real(dp) :: yp(NVARS), ym(NVARS)

    ! Cartoon mixed neighbors
    real(dp) :: xpyp(NVARS), xpym(NVARS)
    real(dp) :: xmyp(NVARS), xmym(NVARS)

    real(dp) :: zpyp(NVARS), zpym(NVARS)
    real(dp) :: zmyp(NVARS), zmym(NVARS)

    ! x-z diagonal neighbors
    real(dp) :: xpzp(NVARS), xpzm(NVARS)
    real(dp) :: xmzp(NVARS), xmzm(NVARS)

    logical :: xp_ok, xm_ok
    logical :: xp2_ok, xm2_ok
    logical :: zp_ok, zm_ok
    logical :: zp2_ok, zm2_ok

    !-------------------------------------------------------------------
    ! Central value
    !-------------------------------------------------------------------

    c0 = u(i,k,:)

    S%val = c0


    !===================================================================
    ! Determine which x/z stencil directions are available.
    !===================================================================

    xp_ok  = valid_x(i,k,+1,mask)
    xm_ok  = valid_x(i,k,-1,mask)

    xp2_ok = valid_x2(i,k,+1,mask)
    xm2_ok = valid_x2(i,k,-1,mask)

    zp_ok  = valid_z(i,k,+1,mask)
    zm_ok  = valid_z(i,k,-1,mask)

    zp2_ok = valid_z2(i,k,+1,mask)
    zm2_ok = valid_z2(i,k,-1,mask)


    !===================================================================
    ! First x derivative
    !
    ! Centered:
    !
    !       f(i+1)-f(i-1)
    !       ----------------
    !             2 dx
    !
    ! Forward:
    !
    !       -3f(i)+4f(i+1)-f(i+2)
    !       -----------------------
    !                2 dx
    !
    ! Backward:
    !
    !        3f(i)-4f(i-1)+f(i-2)
    !       -----------------------
    !                2 dx
    !===================================================================

    if (xp_ok .and. xm_ok .and. xp2_ok .and. xm2_ok) then

      xp2 = get_neighbor(u,i,k, 2,0,0.0_dp)
      xm2 = get_neighbor(u,i,k,-2,0,0.0_dp)
      xp  = get_neighbor(u,i,k, 1,0,0.0_dp)
      xm  = get_neighbor(u,i,k,-1,0,0.0_dp)

      S%d1x = (xm2 - 8.0_dp*xm + 8.0_dp*xp - xp2)/(12.0_dp*dx)

    else if (xp_ok .and. xm_ok) then

      xp = get_neighbor(u,i,k, 1,0,0.0_dp)
      xm = get_neighbor(u,i,k,-1,0,0.0_dp)

      S%d1x = (xp-xm)/(2.0_dp*dx)

    else if (xp2_ok) then

      xp  = get_neighbor(u,i,k,1,0,0.0_dp)
      xp2 = get_neighbor(u,i,k,2,0,0.0_dp)

      S%d1x = (-3.0_dp*c0 + 4.0_dp*xp - xp2) &
              /(2.0_dp*dx)

    else if (xm2_ok) then

      xm  = get_neighbor(u,i,k,-1,0,0.0_dp)
      xm2 = get_neighbor(u,i,k,-2,0,0.0_dp)

      S%d1x = (3.0_dp*c0 - 4.0_dp*xm + xm2) &
              /(2.0_dp*dx)

    else

      write(*,*) 'ERROR: no valid x derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d1x'

    end if


    !===================================================================
    ! First z derivative
    !===================================================================

    if (zp_ok .and. zm_ok .and. zp2_ok .and. zm2_ok) then

      zp2 = get_neighbor(u,i,k,0, 2,0.0_dp)
      zm2 = get_neighbor(u,i,k,0,-2,0.0_dp)
      zp  = get_neighbor(u,i,k,0, 1,0.0_dp)
      zm  = get_neighbor(u,i,k,0,-1,0.0_dp)

      S%d1z = (zm2 - 8.0_dp*zm + 8.0_dp*zp - zp2)/(12.0_dp*dz)

    else if (zp_ok .and. zm_ok) then

      zp = get_neighbor(u,i,k,0, 1,0.0_dp)
      zm = get_neighbor(u,i,k,0,-1,0.0_dp)

      S%d1z = (zp-zm)/(2.0_dp*dz)

    else if (zp2_ok) then

      zp  = get_neighbor(u,i,k,0,1,0.0_dp)
      zp2 = get_neighbor(u,i,k,0,2,0.0_dp)

      S%d1z = (-3.0_dp*c0 + 4.0_dp*zp - zp2) &
              /(2.0_dp*dz)

    else if (zm2_ok) then

      zm  = get_neighbor(u,i,k,0,-1,0.0_dp)
      zm2 = get_neighbor(u,i,k,0,-2,0.0_dp)

      S%d1z = (3.0_dp*c0 - 4.0_dp*zm + zm2) &
              /(2.0_dp*dz)

    else

      write(*,*) 'ERROR: no valid z derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d1z'

    end if


    !===================================================================
    ! Cartoon y derivatives
    !
    ! These are NOT ordinary grid derivatives. Keep using get_neighbor().
    !===================================================================

    yp = get_neighbor(u,i,k,0,0, dy)
    ym = get_neighbor(u,i,k,0,0,-dy)

    S%d1y = (yp-ym)/(2.0_dp*dy)

    S%d2y = (yp-2.0_dp*c0+ym)/(dy**2)


    !===================================================================
    ! Second x derivative
    !
    ! Centered:
    !
    ! f(i+1)-2f(i)+f(i-1)
    ! --------------------
    !        dx^2
    !
    ! One-sided:
    !
    ! f(i)-2f(i+1)+f(i+2)
    ! --------------------
    !        dx^2
    !===================================================================

    if (xp_ok .and. xm_ok .and. xp2_ok .and. xm2_ok) then

      xp2 = get_neighbor(u,i,k, 2,0,0.0_dp)
      xm2 = get_neighbor(u,i,k,-2,0,0.0_dp)
      xp  = get_neighbor(u,i,k, 1,0,0.0_dp)
      xm  = get_neighbor(u,i,k,-1,0,0.0_dp)

      S%d2x = (-xp2 + 16.0_dp*xp - 30.0_dp*c0 + 16.0_dp*xm - xm2) &
              /(12.0_dp*dx**2)

    else if (xp_ok .and. xm_ok) then

      ! xp and xm were already loaded in the d1x branch
      !
      ! They are guaranteed to exist here.

      S%d2x = (xp - 2.0_dp*c0 + xm)/(dx**2)

    else if (xp2_ok) then

      xp  = get_neighbor(u,i,k,1,0,0.0_dp)
      xp2 = get_neighbor(u,i,k,2,0,0.0_dp)

      S%d2x = (c0 - 2.0_dp*xp + xp2)/(dx**2)

    else if (xm2_ok) then

      xm  = get_neighbor(u,i,k,-1,0,0.0_dp)
      xm2 = get_neighbor(u,i,k,-2,0,0.0_dp)

      S%d2x = (c0 - 2.0_dp*xm + xm2)/(dx**2)

    else

      write(*,*) 'ERROR: no valid x second derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d2x'

    end if


    !===================================================================
    ! Second z derivative
    !===================================================================

    if (zp_ok .and. zm_ok .and. zp2_ok .and. zm2_ok) then

      zp2 = get_neighbor(u,i,k,0, 2,0.0_dp)
      zm2 = get_neighbor(u,i,k,0,-2,0.0_dp)
      zp  = get_neighbor(u,i,k,0, 1,0.0_dp)
      zm  = get_neighbor(u,i,k,0,-1,0.0_dp)

      S%d2z = (-zp2 + 16.0_dp*zp - 30.0_dp*c0 + 16.0_dp*zm - zm2) &
              /(12.0_dp*dz**2)

    else if (zp_ok .and. zm_ok) then

      S%d2z = (zp - 2.0_dp*c0 + zm)/(dz**2)

    else if (zp2_ok) then

      zp  = get_neighbor(u,i,k,0,1,0.0_dp)
      zp2 = get_neighbor(u,i,k,0,2,0.0_dp)

      S%d2z = (c0 - 2.0_dp*zp + zp2)/(dz**2)

    else if (zm2_ok) then

      zm  = get_neighbor(u,i,k,0,-1,0.0_dp)
      zm2 = get_neighbor(u,i,k,0,-2,0.0_dp)

      S%d2z = (c0 - 2.0_dp*zm + zm2)/(dz**2)

    else

      write(*,*) 'ERROR: no valid z second derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d2z'

    end if


    !===================================================================
    ! Mixed x-z derivative
    !
    ! If all four diagonal points are available, use the ordinary
    ! centered mixed derivative:
    !
    !   f(+,+)-f(+,-)-f(-,+)+f(-,-)
    !   -----------------------------
    !              4 dx dz
    !
    ! Otherwise calculate it by differentiating the boundary-aware
    ! z derivative in x.
    !===================================================================

    if (xp_ok .and. xm_ok .and. zp_ok .and. zm_ok) then

      xpzp = get_neighbor(u,i,k, 1, 1,0.0_dp)
      xpzm = get_neighbor(u,i,k, 1,-1,0.0_dp)
      xmzp = get_neighbor(u,i,k,-1, 1,0.0_dp)
      xmzm = get_neighbor(u,i,k,-1,-1,0.0_dp)

      S%d2xz = (xpzp - xpzm - xmzp + xmzm) &
               /(4.0_dp*dx*dz)

    else

      call mixed_xz_one_sided(u,i,k,mask,S%d2xz)

    end if


    !===================================================================
    ! Mixed x-y derivative
    !
    ! Cartoon y derivative remains responsible for the y direction.
    ! We therefore differentiate the Cartoon d/dy quantity in x.
    !===================================================================

    xpyp = get_neighbor(u,i,k, 1,0, dy)
    xpym = get_neighbor(u,i,k, 1,0,-dy)

    xmyp = get_neighbor(u,i,k,-1,0, dy)
    xmym = get_neighbor(u,i,k,-1,0,-dy)

    if (xp_ok .and. xm_ok) then

      S%d2xy = (xpyp - xpym - xmyp + xmym) &
               /(4.0_dp*dx*dy)

    else if (xp2_ok) then

      ! Forward x difference of the Cartoon y derivative.
      !
      ! [D_y f(i+1) - D_y f(i)] / dx
      !
      ! This is only first-order at the excision boundary.

      S%d2xy = ( &
          (xpyp-xpym)/(2.0_dp*dy) &
        - (yp-ym)/(2.0_dp*dy) ) / dx

    else if (xm2_ok) then

      S%d2xy = ( &
          (yp-ym)/(2.0_dp*dy) &
        - (xmyp-xmym)/(2.0_dp*dy) ) / dx

    else

      write(*,*) 'ERROR: no valid xy derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d2xy'

    end if


    !===================================================================
    ! Mixed y-z derivative
    !===================================================================

    zpyp = get_neighbor(u,i,k,0, 1, dy)
    zpym = get_neighbor(u,i,k,0, 1,-dy)

    zmyp = get_neighbor(u,i,k,0,-1, dy)
    zmym = get_neighbor(u,i,k,0,-1,-dy)

    if (zp_ok .and. zm_ok) then

      S%d2yz = (zpyp - zpym - zmyp + zmym) &
               /(4.0_dp*dz*dy)

    else if (zp2_ok) then

      S%d2yz = ( &
          (zpyp-zpym)/(2.0_dp*dy) &
        - (yp-ym)/(2.0_dp*dy) ) / dz

    else if (zm2_ok) then

      S%d2yz = ( &
          (yp-ym)/(2.0_dp*dy) &
        - (zmyp-zmym)/(2.0_dp*dy) ) / dz

    else

      write(*,*) 'ERROR: no valid yz derivative stencil'
      write(*,*) 'i,k = ',i,k
      error stop 'compute_stencil: d2yz'

    end if

  end subroutine compute_stencil


  !=======================================================================
  ! Is the immediate x neighbor usable?
  !=======================================================================

  logical function valid_x(i,k,sgn,mask)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: mask(:,:)

    integer :: ii

    ii = i + sgn

    valid_x = .false.

    if (ii < 1) then

      ! Past the symmetry axis (x<0). There is no stored grid point here,
      ! but get_neighbor() synthesizes it via the Cartoon axis mirror
      ! (radial interpolation + rotation by pi) -- the same mechanism
      ! already used for the Cartoon y-derivatives, the mixed xy/yz
      ! derivatives below, and the KO dissipation stencils. It is a
      ! legitimate, usable neighbor, not a missing one. Treating it as
      ! invalid (the previous behavior) silently forced compute_stencil
      ! to fall back to a one-sided stencil at i=1, which is only
      ! first-order accurate right on the axis, instead of the centered,
      ! second-order stencil available via the mirror.

      valid_x = .true.
      return

    end if

    if (ii > Nx) return

    if (mask(ii,k) == EXCISED) return

    valid_x = .true.

  end function valid_x


  !=======================================================================
  ! Are two x neighbors usable?
  !=======================================================================

  logical function valid_x2(i,k,sgn,mask)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: mask(:,:)

    integer :: ii1,ii2

    ii1 = i + sgn
    ii2 = i + 2*sgn

    valid_x2 = .false.

    if (ii1 < 1 .or. ii1 > Nx) return
    if (ii2 < 1 .or. ii2 > Nx) return

    if (mask(ii1,k) == EXCISED) return
    if (mask(ii2,k) == EXCISED) return

    valid_x2 = .true.

  end function valid_x2


  !=======================================================================
  ! Is the immediate z neighbor usable?
  !=======================================================================

  logical function valid_z(i,k,sgn,mask)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: mask(:,:)

    integer :: kk

    kk = k + sgn

    valid_z = .false.

    if (kk < 1 .or. kk > Nz) return

    if (mask(i,kk) == EXCISED) return

    valid_z = .true.

  end function valid_z


  !=======================================================================
  ! Are two z neighbors usable?
  !=======================================================================

  logical function valid_z2(i,k,sgn,mask)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: mask(:,:)

    integer :: kk1,kk2

    kk1 = k + sgn
    kk2 = k + 2*sgn

    valid_z2 = .false.

    if (kk1 < 1 .or. kk1 > Nz) return
    if (kk2 < 1 .or. kk2 > Nz) return

    if (mask(i,kk1) == EXCISED) return
    if (mask(i,kk2) == EXCISED) return

    valid_z2 = .true.

  end function valid_z2


  !=======================================================================
  ! Mixed x-z derivative fallback.
  !
  ! We calculate:
  !
  !       d2 f
  !       ------
  !       dx dz
  !
  ! by differentiating a boundary-aware z derivative in x.
  !
  ! At an excision boundary this is intentionally lower order than the
  ! interior centered stencil. Stability is more important at this stage.
  !=======================================================================

  subroutine mixed_xz_one_sided(u,i,k,mask,d2xz)

    real(dp), intent(in) :: u(:,:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: mask(:,:)
    real(dp), intent(out) :: d2xz(NVARS)

    real(dp) :: z_here(NVARS)
    real(dp) :: z_plus(NVARS)
    real(dp) :: z_minus(NVARS)

    real(dp) :: xz_plus(NVARS)
    real(dp) :: xz_minus(NVARS)

    logical :: xp_ok
    logical :: xm_ok

    !-------------------------------------------------------------------
    ! Cartoon z derivative at the current point.
    !-------------------------------------------------------------------

    call local_z_derivative(u,i,k,mask,z_here)

    xp_ok = valid_x(i,k,+1,mask)
    xm_ok = valid_x(i,k,-1,mask)

    !-------------------------------------------------------------------
    ! Forward x direction.
    !-------------------------------------------------------------------

    if (xp_ok .and. valid_z_for_point(u,i+1,k,mask)) then

      call local_z_derivative(u,i+1,k,mask,z_plus)

      d2xz = (z_plus-z_here)/dx

      return

    end if

    !-------------------------------------------------------------------
    ! Backward x direction.
    !-------------------------------------------------------------------

    if (xm_ok .and. valid_z_for_point(u,i-1,k,mask)) then

      call local_z_derivative(u,i-1,k,mask,z_minus)

      d2xz = (z_here-z_minus)/dx

      return

    end if

    write(*,*) 'ERROR: no valid mixed xz derivative'
    write(*,*) 'i,k = ',i,k

    error stop 'compute_stencil: d2xz'

  end subroutine mixed_xz_one_sided


  !=======================================================================
  ! Determine whether a local z derivative can be constructed.
  !=======================================================================

  logical function valid_z_for_point(u,i,k,mask)

    real(dp), intent(in) :: u(:,:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: mask(:,:)

    valid_z_for_point = .false.

    if (i < 1 .or. i > Nx) return
    if (k < 1 .or. k > Nz) return

    if (mask(i,k) == EXCISED) return

    if (valid_z(i,k,+1,mask) .and. valid_z(i,k,-1,mask)) then
      valid_z_for_point = .true.
      return
    end if

    if (valid_z2(i,k,+1,mask)) then
      valid_z_for_point = .true.
      return
    end if

    if (valid_z2(i,k,-1,mask)) then
      valid_z_for_point = .true.
      return
    end if

  end function valid_z_for_point


  !=======================================================================
  ! Boundary-aware local z derivative.
  !=======================================================================

  subroutine local_z_derivative(u,i,k,mask,dzv)

    real(dp), intent(in) :: u(:,:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: mask(:,:)
    real(dp), intent(out) :: dzv(NVARS)

    real(dp) :: c(NVARS)
    real(dp) :: zp(NVARS), zm(NVARS)
    real(dp) :: zp2(NVARS), zm2(NVARS)

    c = u(i,k,:)

    if (valid_z(i,k,+1,mask) .and. &
        valid_z(i,k,-1,mask)) then

      zp = get_neighbor(u,i,k,0,1,0.0_dp)
      zm = get_neighbor(u,i,k,0,-1,0.0_dp)

      dzv = (zp-zm)/(2.0_dp*dz)

    else if (valid_z2(i,k,+1,mask)) then

      zp  = get_neighbor(u,i,k,0,1,0.0_dp)
      zp2 = get_neighbor(u,i,k,0,2,0.0_dp)

      dzv = (-3.0_dp*c+4.0_dp*zp-zp2)/(2.0_dp*dz)

    else if (valid_z2(i,k,-1,mask)) then

      zm  = get_neighbor(u,i,k,0,-1,0.0_dp)
      zm2 = get_neighbor(u,i,k,0,-2,0.0_dp)

      dzv = (3.0_dp*c-4.0_dp*zm+zm2)/(2.0_dp*dz)

    else

      write(*,*) 'ERROR: local_z_derivative has no valid stencil'
      write(*,*) 'i,k = ',i,k

      error stop 'local_z_derivative'

    end if

  end subroutine local_z_derivative

end module stencil_mod
