program mhd_thruster_3d
  implicit none
  integer, parameter :: nx = 32, ny = 32, nz = 32
  real(8), parameter :: Lx = 0.1, Ly = 0.02, Lz = 0.02
  real(8), parameter :: dx = Lx/nx, dy = Ly/ny, dz = Lz/nz
  real(8), parameter :: dt = 0.0001, t_max = 0.2
  real(8), parameter :: rho = 1000.0
  real(8), parameter :: B0 = 0.5
  real(8), parameter :: J0 = 1.0e5
  real(8), parameter :: nu = 1.0e-10, eta = 1.0e-10
  integer :: i, j, k, n, n_steps, output_interval = 30
  real(8), dimension(nx,ny,nz) :: vx, vy, vz, Bx, By, Bz
  real(8), dimension(nx,ny,nz) :: vx_tmp, vy_tmp, vz_tmp, Bx_tmp, By_tmp, Bz_tmp
  real(8) :: t, energy
  character(len=20) :: filename

  call initialize_fields(vx, vy, vz, Bx, By, Bz)
  n_steps = int(t_max / dt)
  open(unit=10, file='energy.dat', status='replace')
  write(filename, '("fields_", i5.5, ".dat")') 0
  call write_fields(vx, vy, vz, Bx, By, Bz, filename)
  t = 0.0
  do n = 1, n_steps
    energy = compute_energy(vx, vy, vz, Bx, By, Bz)
    write(10, *) t, energy
    if (mod(n, output_interval) == 0) then
      write(filename, '("fields_", i5.5, ".dat")') n
      call write_fields(vx, vy, vz, Bx, By, Bz, filename)
    end if
    call leapfrog_step(vx, vy, vz, Bx, By, Bz, &
                       vx_tmp, vy_tmp, vz_tmp, Bx_tmp, By_tmp, Bz_tmp)
    vx = vx_tmp; vy = vy_tmp; vz = vz_tmp
    Bx = Bx_tmp; By = By_tmp; Bz = Bz_tmp
    call apply_boundary_conditions(vx, vy, vz, Bx, By, Bz)
    t = t + dt
  end do
  close(10)
  print *, 'Simulation complete. Energy in energy.dat, fields in fields_*.dat'

contains

  subroutine initialize_fields(vx, vy, vz, Bx, By, Bz)
    real(8), dimension(nx,ny,nz), intent(out) :: vx, vy, vz, Bx, By, Bz
    integer :: i, j, k
    real(8) :: y, z
    vx = 0.0; vy = 0.0; vz = 0.0
    Bx = 0.0; By = 0.0; Bz = 0.0
    do k = 1, nz
      z = (k-1)*dz
      do j = 1, ny
        y = (j-1)*dy
        do i = 1, nx
          vx(i,j,k) = 0.05 * (1.0 - ((y - Ly/2)/Ly)**2) * (1.0 - ((z - Lz/2)/Lz)**2)
          Bx(i,j,k) = 1.0e-2 * sin(2.0 * 3.14159 * i / nx)
          Bz(i,j,k) = B0
        end do
      end do
    end do
    print *, 'Initialization: Max vx =', maxval(abs(vx)), 'Bz =', Bz(1,1,1)
  end subroutine initialize_fields

  subroutine apply_boundary_conditions(vx, vy, vz, Bx, By, Bz)
    real(8), dimension(nx,ny,nz), intent(inout) :: vx, vy, vz, Bx, By, Bz
    integer :: i, k, j
    do k = 1, nz
      do j = 1, ny
        vx(1,j,k) = 0.05
      end do
    end do
    do k = 1, nz
      do i = 1, nx
        vx(i,1,k) = 0.0; vy(i,1,k) = vy(i,2,k); vz(i,1,k) = vz(i,2,k)
        vx(i,ny,k) = 0.0; vy(i,ny,k) = vy(i,ny-1,k); vz(i,ny,k) = vz(i,ny-1,k)
      end do
    end do
    do j = 1, ny
      do i = 1, nx
        vx(i,j,1) = 0.0; vy(i,j,1) = vy(i,j,2); vz(i,j,1) = vz(i,j,2)
        vx(i,j,nz) = 0.0; vy(i,j,nz) = vy(i,j,nz-1); vz(i,j,nz) = vz(i,j,nz-1)
      end do
    end do
    do k = 1, nz
      do i = 1, nx
        Bx(i,1,k) = Bx(i,2,k); Bx(i,ny,k) = Bx(i,ny-1,k)
        By(i,1,k) = By(i,2,k); By(i,ny,k) = By(i,ny-1,k)
        Bz(i,1,k) = Bz(i,2,k); Bz(i,ny,k) = Bz(i,ny-1,k)
      end do
    end do
    do j = 1, ny
      do i = 1, nx
        Bx(i,j,1) = Bx(i,j,2); Bx(i,j,nz) = Bx(i,j,nz-1)
        By(i,j,1) = By(i,j,2); By(i,j,nz) = By(i,j,nz-1)
        Bz(i,j,1) = Bz(i,j,2); Bz(i,j,nz) = Bz(i,j,nz-1)
      end do
    end do
  end subroutine apply_boundary_conditions

  function ddx(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k, ip, im
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          ip = mod(i,nx)+1
          im = mod(i-2+nx,nx)+1
          df(i,j,k) = (f(ip,j,k) - f(im,j,k)) / (2.0*dx)
        end do
      end do
    end do
  end function ddx

  function ddy(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k
    do k = 1, nz
      do j = 2, ny-1
        do i = 1, nx
          df(i,j,k) = (f(i,j+1,k) - f(i,j-1,k)) / (2.0*dy)
        end do
      end do
      do i = 1, nx
        df(i,1,k) = 0.0
        df(i,ny,k) = 0.0
      end do
    end do
  end function ddy

  function ddz(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k
    do k = 2, nz-1
      do j = 1, ny
        do i = 1, nx
          df(i,j,k) = (f(i,j,k+1) - f(i,j,k-1)) / (2.0*dz)
        end do
      end do
    end do
    do j = 1, ny
      do i = 1, nx
        df(i,j,1) = 0.0
        df(i,j,nz) = 0.0
      end do
    end do
  end function ddz

  function laplacian(f) result(lap)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: lap
    integer :: i, j, k, ip, im
    do k = 2, nz-1
      do j = 2, ny-1
        do i = 1, nx
          ip = mod(i,nx)+1
          im = mod(i-2+nx,nx)+1
          lap(i,j,k) = (f(ip,j,k) - 2.0*f(i,j,k) + f(im,j,k)) / (dx*dx) + &
                       (f(i,j+1,k) - 2.0*f(i,j,k) + f(i,j-1,k)) / (dy*dy) + &
                       (f(i,j,k+1) - 2.0*f(i,j,k) + f(i,j,k-1)) / (dz*dz)
        end do
      end do
    end do
    do k = 1, nz
      do i = 1, nx
        lap(i,1,k) = 0.0
        lap(i,ny,k) = 0.0
      end do
    end do
    do j = 1, ny
      do i = 1, nx
        lap(i,j,1) = 0.0
        lap(i,j,nz) = 0.0
      end do
    end do
  end function laplacian

  subroutine compute_derivatives(vx, vy, vz, Bx, By, Bz, &
                                 dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8), dimension(nx,ny,nz), intent(out) :: dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt
    real(8), dimension(nx,ny,nz) :: curlB_x, curlB_y, curlB_z, tmp_x, tmp_y, tmp_z
    curlB_x = ddy(Bz) - ddz(By)
    curlB_y = ddz(Bx) - ddx(Bz)
    curlB_z = ddx(By) - ddy(Bx)
    dvx_dt = -vx*ddx(vx) - vy*ddy(vx) - vz*ddz(vx) + &
             (curlB_x*(Bz+B0) + curlB_y*By + curlB_z*Bx)/rho + &
             J0*B0/rho + nu*laplacian(vx)
    dvy_dt = -vx*ddx(vy) - vy*ddy(vy) - vz*ddz(vy) + &
             (curlB_x*By + curlB_y*(Bz+B0) + curlB_z*By)/rho + &
             nu*laplacian(vy)
    dvz_dt = -vx*ddx(vz) - vy*ddy(vz) - vz*ddz(vz) + &
             (curlB_x*Bz + curlB_y*By + curlB_z*(Bz+B0))/rho + &
             nu*laplacian(vz)
    tmp_x = vz*By - vy*(Bz+B0)
    tmp_y = vx*(Bz+B0) - vz*Bx
    tmp_z = vy*Bx - vx*By
    dBx_dt = ddy(tmp_x) - ddz(tmp_y) + eta*laplacian(Bx)
    dBy_dt = ddz(tmp_y) - ddx(tmp_z) + eta*laplacian(By)
    dBz_dt = ddx(tmp_z) - ddy(tmp_x) + eta*laplacian(Bz)
  end subroutine compute_derivatives

  subroutine leapfrog_step(vx, vy, vz, Bx, By, Bz, &
                           vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8), dimension(nx,ny,nz), intent(out) :: vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new
    real(8), dimension(nx,ny,nz) :: dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt
    call compute_derivatives(vx, vy, vz, Bx, By, Bz, dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    vx_new = vx + 0.5*dt*dvx_dt
    vy_new = vy + 0.5*dt*dvy_dt
    vz_new = vz + 0.5*dt*dvz_dt
    Bx_new = Bx + dt*dBx_dt
    By_new = By + dt*dBy_dt
    Bz_new = Bz + dt*dBz_dt
    call apply_boundary_conditions(vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new)
    call compute_derivatives(vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new, &
                             dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    vx_new = vx + dt*dvx_dt
    vy_new = vy + dt*dvy_dt
    vz_new = vz + dt*dvz_dt
  end subroutine leapfrog_step

  function compute_energy(vx, vy, vz, Bx, By, Bz) result(energy)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8) :: energy
    energy = sum(0.5*rho*(vx**2 + vy**2 + vz**2) + 0.5*(Bx**2 + By**2 + Bz**2)) * dx*dy*dz
  end function compute_energy

  subroutine write_fields(vx, vy, vz, Bx, By, Bz, filename)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    character(len=*), intent(in) :: filename
    integer :: i, j, k
    open(unit=11, file=filename, status='replace')
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          write(11, *) (i-1)*dx, (j-1)*dy, (k-1)*dz, &
                       vx(i,j,k), vy(i,j,k), vz(i,j,k), &
                       Bx(i,j,k), By(i,j,k), Bz(i,j,k)
        end do
        write(11, *)
      end do
      write(11, *)
    end do
    close(11)
  end subroutine write_fields

end program mhd_thruster_3d
