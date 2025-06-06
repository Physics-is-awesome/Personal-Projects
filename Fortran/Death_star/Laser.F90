program sph_planet_formation_laser
  implicit none
  integer, parameter :: n = 100  ! Number of particles
  real, parameter :: G = 1.0     ! Gravitational constant
  real, parameter :: dt = 0.003  ! Time step
  real, parameter :: t_max = 15.0 ! Extended simulation time to observe impact
  real, parameter :: h = 0.25    ! Smoothing length
  real, parameter :: eps = 0.05  ! Softening length
  real, parameter :: m_central = 100.0 ! Central body mass
  real, parameter :: coll_dist = 0.1 ! Distance for repulsive force
  real, parameter :: k_coll = 20.0  ! Repulsive force constant
  real, parameter :: viscosity = 3.0 ! Artificial viscosity
  real, parameter :: k_press = 1.0  ! Pressure constant
  real, parameter :: gamma = 5.0/3.0 ! Adiabatic index
  real, parameter :: damp_close = 0.9 ! Damping for close encounters
  real, parameter :: damp_global = 0.999 ! Global damping
  real, parameter :: central_dist = 0.5 ! Distance to central body for damping
  real, parameter :: laser_time = 5.0 ! Time of laser impact
  real, parameter :: laser_width = 0.1 ! Width of laser beam (y-range)
  real, parameter :: v_laser = 10.0 ! Velocity kick from laser
  integer, parameter :: n_steps = int(t_max / dt)
  real :: x(n), y(n), vx(n), vy(n), mass(n), ax(n), ay(n), rho(n), P(n)
  real :: r, w, dwdr, force, fx, fy, r_disk, v_circ
  real :: mu, v_dot_r, pi_term, visc_term
  integer :: i, j, step
  real :: t
  logical :: laser_applied = .false. ! Flag to apply laser once
  logical :: close_encounter

  ! Random number seed
  call random_seed()

  ! Initialize particles in a disk
  do i = 1, n
    call random_number(r_disk)
    call random_number(t)
    r_disk = r_disk * 5.0
    t = t * 2.0 * 3.141592653589793
    x(i) = r_disk * cos(t)
    y(i) = r_disk * sin(t)
    v_circ = sqrt(G * m_central / r_disk) * 0.8
    vx(i) = -v_circ * sin(t) - 0.02 * x(i) / r_disk
    vy(i) = v_circ * cos(t) - 0.02 * y(i) / r_disk
    mass(i) = 1.0
  end do

  ! Open file for output
  open(unit=10, file='particle_positions.dat', status='replace')

  ! Main simulation loop
  do step = 1, n_steps
    t = step * dt

    ! Apply laser impact at t = laser_time
    if (.not. laser_applied .and. t >= laser_time) then
      do i = 1, n
        ! Check if particle is in laser beam (right side, near x-axis)
        if (x(i) > 0.0 .and. abs(y(i)) < laser_width) then
          vx(i) = vx(i) + v_laser ! Apply velocity kick along +x
        end if
      end do
      laser_applied = .true.
      print *, 'Laser applied at t = ', t
    end if

    ! Compute density and pressure
    do i = 1, n
      rho(i) = 0.0
      do j = 1, n
        r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
        w = kernel(r, h)
        rho(i) = rho(i) + mass(j) * w
      end do
      P(i) = k_press * rho(i)**gamma
    end do

    ! Write particle positions and density
    write(10, *) t, (x(i), y(i), rho(i), i=1, n)

    ! Calculate accelerations
    ax = 0.0
    ay = 0.0
    do i = 1, n
      do j = 1, n
        if (i /= j) then
          r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
          force = -G * mass(i) * mass(j) / (r**2 + eps**2)**1.5
          fx = force * (x(i) - x(j))
          fy = force * (y(i) - y(j))
          if (r < 2.0 * h) then
            dwdr = kernel_deriv(r, h)
            force = -(mass(j) * (P(i)/rho(i)**2 + P(j)/rho(j)**2)) * dwdr
            fx = fx + force * (x(i) - x(j))
            fy = fy + force * (y(i) - y(j))
            v_dot_r = (vx(i) - vx(j)) * (x(i) - x(j)) + (vy(i) - vy(j)) * (y(i) - y(j))
            if (v_dot_r < 0.0) then
              mu = h * v_dot_r / (r**2 + 0.01 * h**2)
              pi_term = (-viscosity * mu) / (rho(i) * rho(j))
              visc_term = mass(j) * pi_term * dwdr
              fx = fx + visc_term * (x(i) - x(j))
              fy = fy + visc_term * (y(i) - y(j))
            end if
          end if
          if (r < coll_dist) then
            force = k_coll * (coll_dist - r)
            fx = fx + force * (x(i) - x(j)) / r
            fy = fy + force * (y(i) - y(j)) / r
          end if
          ax(i) = ax(i) + fx / mass(i)
          ay(i) = ay(i) + fy / mass(i)
        end if
      end do
      r = sqrt(x(i)**2 + y(i)**2)
      force = -G * mass(i) * m_central / (r**2 + eps**2)**1.5
      ax(i) = ax(i) + force * x(i) / mass(i)
      ay(i) = ay(i) + force * y(i) / mass(i)
    end do

    ! Update velocities and positions with damping
    do i = 1, n
      close_encounter = .false.
      do j = 1, n
        if (i /= j) then
          r = sqrt((x(i) - x(j))**2 + (y(i) - x(j))**2)
          if (r < coll_dist) then
            close_encounter = .true.
            exit
          end if
        end if
      end do
      r = sqrt(x(i)**2 + y(i)**2)
      if (r < central_dist) then
        close_encounter = .true.
      end if
      if (close_encounter) then
        vx(i) = vx(i) * damp_close * damp_global + ax(i) * dt
        vy(i) = vy(i) * damp_close * damp_global + ay(i) * dt
      else
        vx(i) = vx(i) * damp_global + ax(i) * dt
        vy(i) = vy(i) * damp_global + ay(i) * dt
      end if
      x(i) = x(i) + vx(i) * dt
      y(i) = y(i) + vy(i) * dt
    end do
  end do

  ! Close output file
  close(10)

  print *, 'Simulation complete. Positions written to particle_positions.dat'

contains
  real function kernel(r, h)
    real, intent(in) :: r, h
    real :: q, sigma
    sigma = 10.0 / (7.0 * 3.141592653589793 * h**2)
    q = r / h
    if (q <= 1.0) then
      kernel = sigma * (1.0 - 1.5 * q**2 + 0.75 * q**3)
    else if (q <= 2.0) then
      kernel = sigma * 0.25 * (2.0 - q)**3
    else
      kernel = 0.0
    end if
  end function kernel

  real function kernel_deriv(r, h)
    real, intent(in) :: r, h
    real :: q, sigma
    sigma = 10.0 / (7.0 * 3.141592653589793 * h**2)
    q = r / h
    if (q <= 1.0) then
      kernel_deriv = sigma * (-3.0 * q + 2.25 * q**2) / h
    else if (q <= 2.0) then
      kernel_deriv = sigma * (-0.75 * (2.0 - q)**2) / h
    else
      kernel_deriv = 0.0
    end if
  end function kernel_deriv
end program sph_planet_formation_laser
