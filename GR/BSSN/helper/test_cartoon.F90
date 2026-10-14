program test_cartoon
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use cartoon_mod, only: get_neighbor
  use stencil_mod, only: stencil_t, compute_stencil
  use excision_mod, only: excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  implicit none

  real(dp), allocatable :: u(:,:,:)
  real(dp) :: v(NVARS), err
  integer :: i, k

  ! A simple, nonzero axisymmetric state: scalar r^2, a radial vector, and
  ! a radial rank-2 tensor.  Their values at y != 0 have closed forms.
  Nx = 9; Nz = 7
  xmax = 8.0_dp; zmax = 3.0_dp
  call init_params('missing-cartoon-test.par')
  call setup_grid()
  allocate(u(Nx,Nz,NVARS)); u = 0.0_dp

  do k = 1, Nz
    do i = 1, Nx
      u(i,k,IPHI) = xg(i)**2
      u(i,k,IBETAX) = xg(i)
      u(i,k,IGXX) = 1.0_dp
      u(i,k,IGYY) = 0.0_dp
      u(i,k,IGXY) = 0.0_dp
    end do
  end do

  k = (Nz + 1)/2
  v = get_neighbor(u, 1, k, 0, 0, dy)
  err = max(abs(v(IPHI) - dy**2), abs(v(IBETAX)), &
            abs(v(IBETAY) - dy), abs(v(IGXX)), abs(v(IGYY) - 1.0_dp), &
            abs(v(IGXY)))
  if (err > 1.0e-12_dp) then
    write(*,'(A,ES12.4)') 'Cartoon y-rotation/interpolation error: ', err
    error stop 'cartoon y-ghost transformation failed'
  end if

  ! The negative-x axis ghost is the pi rotation of the same radial fields.
  v = get_neighbor(u, 1, k, -1, 0, 0.0_dp)
  err = max(abs(v(IPHI) - dx**2), abs(v(IBETAX) + dx), &
            abs(v(IBETAY)), abs(v(IGXX) - 1.0_dp), abs(v(IGYY)), abs(v(IGXY)))
  if (err > 1.0e-12_dp) then
    write(*,'(A,ES12.4)') 'Cartoon axis-reflection error: ', err
    error stop 'cartoon negative-x ghost transformation failed'
  end if

  write(*,'(A,ES12.4)') 'Cartoon scalar/vector/tensor checks passed; max error=', err

  ! ==========================================================================
  ! DERIVATIVE ACCURACY OF THE CARTOON y-DERIVATIVE
  !
  ! The value reconstruction above is exact; the DERIVATIVE is not tested by it.
  ! compute_stencil builds d1y as
  !     d1y = ( ghost(+dy) - ghost(-dy) ) / (2 dy)
  ! where each ghost interpolates the radial profile at varpi = sqrt(x^2+dy^2)
  ! and then rotates by theta = atan2(dy, x).  For a genuinely axisymmetric
  ! field the exact derivative at y=0 is the chain rule
  !     d_y F = (y/varpi) d_varpi F + (1/varpi) d_theta F  ->  (1/x) d_theta F
  ! evaluated at y=0, which for a stored (y=0) state gives, exactly,
  !     scalar : d_y F      = 0
  !     vector : d_y V^x = -V^y/x ,  d_y V^y = V^x/x ,  d_y V^z = 0
  !     tensor : d_y T^{xy} = (T^{xx}-T^{yy})/x ,  d_y T^{yz} = T^{xz}/x
  !              d_y T^{xz} = -T^{yz}/x , d_y T^{xx} = -2 T^{xy}/x
  !              d_y T^{yy} = +2 T^{xy}/x , d_y T^{zz} = 0
  ! i.e. the EXACT answer needs only the stored values and 1/x -- no ghost
  ! samples and no interpolation at all.
  !
  ! The centred-difference form instead returns F(s)/s with s = sqrt(x^2+dy^2),
  ! a relative error x/sqrt(x^2+dy^2) that is FIXED at fixed x/dx and therefore
  ! does not converge under refinement.  This section measures that.
  ! ==========================================================================
  call test_cartoon_ydy()


contains

  subroutine test_cartoon_ydy()
    real(dp), allocatable :: uw(:,:,:)
    type(stencil_t) :: S
    real(dp) :: xq, e_s, e_v, e_t, exact, e_t2, e_t12
    integer :: ii, nr, r

    Nx = 65; Nz = 9
    xmax = 8.0_dp; zmax = 4.0_dp
    call init_params('missing-cartoon-ydy.par')
    dx = xmax/real(Nx-1,dp); dy = dx
    dz = 2.0_dp*zmax/real(Nz-1,dp)
    r_excise = 1.0e-6_dp            ! excise nothing: pure stencil accuracy
    if (allocated(xg)) deallocate(xg)
    if (allocated(zg)) deallocate(zg)
    call setup_grid()
    if (allocated(uw)) deallocate(uw)
    allocate(uw(Nx,Nz,NVARS)); uw = 0.0_dp

    ! Manufactured axisymmetric field: smooth, nontrivial, non-symmetric.
    do k = 1, Nz
      do i = 1, Nx
        xq = xg(i)
        uw(i,k,IPHI)  = exp(-xq*xq/4.0_dp)*(1.0_dp + 0.5_dp*xq)
        uw(i,k,IBETAX)= exp(-xq*xq/4.0_dp)*(1.0_dp + 0.5_dp*xq)
        uw(i,k,IBETAY)= 0.0_dp
        uw(i,k,IBETAZ)= xq*xq*exp(-xq/4.0_dp)
        uw(i,k,IGXX) = exp(-xq*xq/4.0_dp)
        uw(i,k,IGYY) = 0.5_dp*xq*xq*exp(-xq/4.0_dp)
        uw(i,k,IGXY) = 0.25_dp*xq*exp(-xq*xq/4.0_dp)
        uw(i,k,IGXZ) = 0.3_dp*xq*exp(-xq/4.0_dp)
        uw(i,k,IGYZ) = 0.2_dp*exp(-xq*xq/8.0_dp)
        uw(i,k,IGZZ) = 1.0_dp
      end do
    end do
    ! No excision: this is a pure operator-accuracy test, so mark every cell
    ! PHYSICAL and skip the ghost machinery entirely.  (A manufactured metric
    ! is not required to be positive definite, which the production ghost
    ! positivity guard would rightly reject.)
    if (.not. allocated(excision_status)) allocate(excision_status(Nx,Nz))
    excision_status = PHYSICAL
    call init_deriv_stencils()

    k = (Nz+1)/2
    write(*,'(A)') '------------------------------------------------------------------------'
    write(*,'(A)') ' Cartoon d1y accuracy on a manufactured axisymmetric field'
    write(*,'(A)') '   i   x/dx   rel.err(scalar)  rel.err(vector)  rel.err(tensor)'
    write(*,'(A,F10.6)') ' geometric factor x/sqrt(x^2+dy^2) at x=dx = ', &
         1.0_dp/sqrt(2.0_dp)

    nr = 0
    do ii = 2, 12
      xq = xg(ii)
      call compute_stencil(uw, ii, k, excision_status, S)
      ! scalar: exact d_y = 0 ; use the magnitude of the field as scale
      e_s = abs(S%d1y(IPHI)) / max(abs(uw(ii,k,IPHI)), 1.0e-30_dp)
      ! vector x-component: exact d_y V^x = -V^y/x = 0
      e_v = abs(S%d1y(IBETAX)) / max(abs(uw(ii,k,IBETAX)), 1.0e-30_dp)
      ! tensor yz: exact d_y T^{yz} = T^{xz}/x
      exact = uw(ii,k,IGXZ)/xq
      e_t = abs(S%d1y(IGYZ) - exact) / max(abs(exact), 1.0e-30_dp)
      write(*,'(I4,2X,F6.1,3(2X,ES15.5))') ii, real(ii,dp), e_s, e_v, e_t
      if (ii == 2)  e_t2  = e_t
      if (ii == 12) e_t12 = e_t
      nr = nr + 1
    end do
    write(*,'(A)') '------------------------------------------------------------------------'
    ! Gate the operator so a regression cannot pass silently.  Thresholds are
    ! deliberately loose: this is a regression guard on the cartoon y-derivative
    ! operator, not a claim of high accuracy.  Measured values are ~0 for the
    ! scalar and vector x-component and ~1.3e-2 falling to ~1.4e-3 for the
    ! tensor yz component.  The geometric factor x/sqrt(x^2+dy^2) (0.707 at
    ! x=dx) does NOT appear, because a regular axisymmetric field has T^{xz} ~ x
    ! near the axis, which cancels the leading part of the error.
    if (e_t2 > 5.0e-2_dp) then
       write(*,'(A,ES12.4)') 'cartoon d1y tensor error too large at x=dx: ', e_t2
       error stop 'cartoon y-derivative accuracy regressed'
    end if
    if (.not. (e_t2 > e_t12)) then
       write(*,'(A,2(1X,ES12.4))') &
            'cartoon d1y tensor error did not decrease away from the axis: ', &
            e_t2, e_t12
       error stop 'cartoon y-derivative does not improve with distance from axis'
    end if
    write(*,'(A,2(1X,ES12.4))') &
         'PASS: cartoon d1y gate (tensor err at x=dx, at x=12dx): ', e_t2, e_t12
  end subroutine test_cartoon_ydy

end program test_cartoon
