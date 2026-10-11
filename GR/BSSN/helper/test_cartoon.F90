program test_cartoon
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, xmax, zmax, dx, dy, init_params
  use grid_mod, only: xg, setup_grid
  use vars_mod, only: NVARS, IPHI, IGXX, IGXY, IGYY, IBETAX, IBETAY
  use cartoon_mod, only: get_neighbor
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
end program test_cartoon
