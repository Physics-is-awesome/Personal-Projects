program test_cartoon
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use cartoon_mod, only: get_neighbor
  implicit none

  real(dp), allocatable :: u(:,:,:)
  integer :: i, k
  real(dp) :: worst_err
  logical :: all_ok

  call init_params()
  call setup_grid()
  call alloc_state(u, Nx, Nz)

  ! Fill the master (y=0, x>=0) plane with a synthetic axisymmetric field.
  ! All master functions are degree <= 2 in the cylindrical radius (varpi),
  ! so the quadratic (3-point) radial interpolation in cartoon_mod should
  ! reproduce them to machine precision -- any larger discrepancy signals a
  ! real bug in the indexing/rotation logic, not interpolation truncation.
  do k = 1, Nz
    do i = 1, Nx
      u(i,k,IPHI)   = master_scalar(xg(i), zg(k))
      call master_vector(xg(i), zg(k), u(i,k,IGTX), u(i,k,IGTY), u(i,k,IGTZ))
      call master_tensor(xg(i), zg(k), u(i,k,IGXX), u(i,k,IGXY), u(i,k,IGXZ), &
                                        u(i,k,IGYY), u(i,k,IGYZ), u(i,k,IGZZ))
    end do
  end do

  all_ok = .true.
  worst_err = 0.0_dp

  print *, "=== cartoon_mod unit test ==="
  print *, "(all master functions are degree<=2 in varpi -> expect exact agreement)"
  print *

  ! Test 1: plain y-ghost at a generic interior point.
  call check("y-ghost, interior i=10,k=30, y=+dy", u, 10, 30,  0, 0,  dy)
  call check("y-ghost, interior i=10,k=30, y=-dy", u, 10, 30,  0, 0, -dy)

  ! Test 2: plain axis ghost (x<0, y=0) - the simple rotate-by-pi case.
  call check("axis ghost, i=1,k=30, x-1,y=0", u, 1, 30, -1, 0, 0.0_dp)

  ! Test 3: the suspect case - axis, COMBINED x<0 and y=+/-dy, as used by
  ! the mixed d2xy derivative in stencil_mod at i=1.
  call check("axis+y combo, i=1,k=30, x-1,y=+dy", u, 1, 30, -1, 0,  dy)
  call check("axis+y combo, i=1,k=30, x-1,y=-dy", u, 1, 30, -1, 0, -dy)

  ! Test 4: same combo one step off axis (i=2), and with x+1 instead of x-1.
  call check("i=2 combo, x-1,y=+dy", u, 2, 30, -1, 0,  dy)
  call check("i=2 combo, x+1,y=+dy", u, 2, 30,  1, 0,  dy)

  ! Test 5: mixed y-z combo (used by d2yz), interior point.
  call check("y-z combo, i=10,k=30, z+1,y=+dy", u, 10, 30, 0, 1,  dy)

  ! Test 6: mixed x-z combo at the axis (used by d2xz at i=1).
  call check("axis x-z combo, i=1,k=30, x-1,z+1,y=0", u, 1, 30, -1, 1, 0.0_dp)

  print *
  if (all_ok) then
    print '(A,ES10.2)', "ALL TESTS PASSED. worst error = ", worst_err
  else
    print '(A,ES10.2)', "AT LEAST ONE TEST FAILED. worst error = ", worst_err
  end if

contains

  real(dp) function master_scalar(varpi, z) result(f)
    real(dp), intent(in) :: varpi, z
    f = varpi*varpi*z - 0.1_dp*z**3 + 2.0_dp
  end function master_scalar

  subroutine master_vector(varpi, z, vx, vy, vz)
    real(dp), intent(in)  :: varpi, z
    real(dp), intent(out) :: vx, vy, vz
    vx = varpi*z
    vy = 0.3_dp*varpi*varpi
    vz = z*z - varpi
  end subroutine master_vector

  subroutine master_tensor(varpi, z, txx, txy, txz, tyy, tyz, tzz)
    real(dp), intent(in)  :: varpi, z
    real(dp), intent(out) :: txx, txy, txz, tyy, tyz, tzz
    txx = 1.0_dp + 0.05_dp*varpi*varpi
    txy = 0.02_dp*varpi*z
    txz = 0.03_dp*varpi + 0.01_dp*z
    tyy = 1.0_dp + 0.03_dp*z*z
    tyz = 0.015_dp*varpi*z
    tzz = 1.0_dp + 0.04_dp*varpi*varpi
  end subroutine master_tensor

  ! Independent ground truth at an arbitrary (X,Y,z): evaluate the master
  ! functions at (varpi,z) and rotate by theta=atan2(Y,X), using the same
  ! (hand-verified) rotation matrices as cartoon_mod, but coded fresh here.
  subroutine ground_truth(X, Y, z, vals)
    real(dp), intent(in)  :: X, Y, z
    real(dp), intent(out) :: vals(NVARS)
    real(dp) :: varpi, theta, c, s
    real(dp) :: vx0, vy0, vz0, txx0,txy0,txz0,tyy0,tyz0,tzz0

    varpi = sqrt(X*X + Y*Y)
    theta = atan2(Y, X)
    c = cos(theta); s = sin(theta)

    vals = 0.0_dp
    vals(IPHI) = master_scalar(varpi, z)

    call master_vector(varpi, z, vx0, vy0, vz0)
    vals(IGTX) = c*vx0 - s*vy0
    vals(IGTY) = s*vx0 + c*vy0
    vals(IGTZ) = vz0

    call master_tensor(varpi, z, txx0,txy0,txz0,tyy0,tyz0,tzz0)
    vals(IGXX) = c*c*txx0 - 2.0_dp*c*s*txy0 + s*s*tyy0
    vals(IGXY) = c*s*(txx0-tyy0) + (c*c-s*s)*txy0
    vals(IGXZ) = c*txz0 - s*tyz0
    vals(IGYY) = s*s*txx0 + 2.0_dp*c*s*txy0 + c*c*tyy0
    vals(IGYZ) = s*txz0 + c*tyz0
    vals(IGZZ) = tzz0
  end subroutine ground_truth

  subroutine check(label, u, i, k, ix_off, iz_off, y_val)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: u(:,:,:)
    integer, intent(in) :: i, k, ix_off, iz_off
    real(dp), intent(in) :: y_val
    real(dp) :: got(NVARS), truth(NVARS), err
    real(dp) :: Xt, Yt, Zt

    got = get_neighbor(u, i, k, ix_off, iz_off, y_val)
    Xt = xg(i) + real(ix_off,dp)*dx
    Yt = y_val
    Zt = zg(k + iz_off)
    call ground_truth(Xt, Yt, Zt, truth)

    err = maxval(abs(got - truth))
    worst_err = max(worst_err, err)
    if (err > 1.0e-9_dp) all_ok = .false.

    print '(A,A,ES12.4)', label, repeat(" ", max(1,40-len(label))) // "max|err| = ", err
    if (err > 1.0e-9_dp) then
      print '(A,6ES13.4)', "     got:   phi,Gtx,Gty,Gtz,gxx,gxz = ", &
        got(IPHI), got(IGTX), got(IGTY), got(IGTZ), got(IGXX), got(IGXZ)
      print '(A,6ES13.4)', "     truth: phi,Gtx,Gty,Gtz,gxx,gxz = ", &
        truth(IPHI), truth(IGTX), truth(IGTY), truth(IGTZ), truth(IGXX), truth(IGXZ)
    end if
  end subroutine check

end program test_cartoon
