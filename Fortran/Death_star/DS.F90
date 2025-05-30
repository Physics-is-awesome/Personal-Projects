program planet_destruction
  implicit none
  integer, parameter :: nr = 200, nt = 100
  real, parameter :: r_planet = 6.371e6
  real, parameter :: t_max = 10.0
  real, parameter :: rho_0 = 2700.0  ! Granite-like density (kg/m^3)
  real, parameter :: beam_energy = 1.0e18  ! Reduced for stability (J)
  real, parameter :: sigma = 1.0e5  ! Beam width (m)
  real, parameter :: beam_duration = 0.1  ! Beam pulse duration (s)
  real, parameter :: G = 6.67430e-11
  real, parameter :: M_planet = 5.972e24
  integer, parameter :: output_freq = 10
  real, parameter :: cfl = 0.5
  real, parameter :: cs_max_limit = 1.0e5
  real, parameter :: rho_min = 1.0e-3
  real, parameter :: dt_min = 1.0e-5
  ! Tillotson EOS parameters (granite-like)
  real, parameter :: eos_alpha = 0.5
  real, parameter :: eos_beta = 1.3
  real, parameter :: eos_A = 1.8e11
  real, parameter :: eos_B = 1.8e11
  real, parameter :: eos_E0 = 1.0e7
  real, parameter :: mu_0 = 1.0e10  ! Shear modulus (Pa)
  real, parameter :: Y_0 = 1.0e9   ! Yield strength (Pa)
  real, parameter :: q_visc = 1.0  ! Artificial viscosity coefficient
  real, parameter :: e_max = 1.0e10  ! Max internal energy (J/kg)
  real, parameter :: div_v_max = 1.0e3  ! Limit velocity divergence (1/s)

  real :: r(nr), theta(nt), dr, dtheta
  real :: rho(nr, nt), u(nr, nt), v(nr, nt), e(nr, nt)
  real :: p(nr, nt), cs(nr, nt)
  real :: grav_acc(nr), shear_stress(nr, nt)
  real :: div_v, q_art
  integer :: i, j, step_count, ierr
  real :: t, dt, r2, q, cs_max, dt_cfl, beam_factor, mu, penetration
  character(len=20) :: filename
  logical :: isnan

  ! Initialize grid
  dr = r_planet / real(nr - 1)
  dtheta = 3.14159 / real(nt - 1)
  do i = 1, nr
    r(i) = (i - 1) * dr
    grav_acc(i) = -G * M_planet / max(r(i)**2, 1.0e3**2)
    do j = 1, nt
      theta(j) = (j - 1) * dtheta
      rho(i, j) = rho_0
      u(i, j) = 0.0
      v(i, j) = 0.0
      e(i, j) = 1.0e6
      p(i, j) = 0.0
      cs(i, j) = 0.0
      shear_stress(i, j) = 0.0
    end do
  end do

  ! Initialize time and step counter
  t = 0.0
  step_count = 0
  dt = 1.0e-3

  ! Time loop
  do while (t < t_max)
    step_count = step_count + 1

    ! Compute Tillotson EOS and sound speed
    cs_max = 0.0
    do i = 1, nr
      do j = 1, nt
        rho(i, j) = max(rho(i, j), rho_min)  ! Enforce rho_min
        mu = rho(i, j) / rho_0 - 1.0
        e(i, j) = min(max(e(i, j), 0.0), e_max)  ! Bound energy
        p(i, j) = (eos_alpha + eos_beta / (1.0 + e(i, j) / eos_E0)) * rho(i, j) * e(i, j) + &
                  eos_A * mu + eos_B * mu**2
        ! Ensure non-negative sound speed argument
        cs(i, j) = sqrt(max(eos_A + 2.0 * eos_B * mu, 1.0e-10) / rho(i, j))
        cs(i, j) = min(cs(i, j), cs_max_limit)
        if (isnan(cs(i, j))) then
          print *, 'NaN in cs at step ', step_count, ' i=', i, ' j=', j, &
                   ' rho=', rho(i, j), ' e=', e(i, j), ' mu=', mu
          stop
        end if
        cs_max = max(cs_max, cs(i, j))
      end do
    end do
    dt_cfl = cfl * min(dr, r_planet * dtheta) / max(cs_max, 1.0e-10)
    dt = max(min(dt, dt_cfl), dt_min)

    ! Apply time-dependent plasma beam
    beam_factor = max(0.0, 1.0 - t / beam_duration)
    do i = 1, nr
      do j = 1, nt
        rho(i, j) = max(rho(i, j), rho_min)  ! Enforce rho_min
        r2 = (r(i) - r_planet)**2 + (theta(j) * r_planet)**2
        penetration = max(exp(-rho_0 * r_planet / 1.0e9), 1.0e-30)
        q = beam_factor * (beam_energy / (2.0 * 3.14159 * sigma**2)) * &
            exp(-r2 / (2.0 * sigma**2)) * penetration
        q = min(q, 1.0e15)  ! Cap beam energy input
        e(i, j) = min(e(i, j) + q * dt / rho(i, j), e_max)
        if (isnan(e(i, j))) then
          print *, 'NaN in e at step ', step_count, ' i=', i, ' j=', j, &
                   ' q=', q, ' rho=', rho(i, j)
          stop
        end if
      end do
    end do

    ! Compute shear stress
    do i = 2, nr - 1
      do j = 2, nt - 1
        shear_stress(i, j) = mu_0 * sqrt(((u(i+1, j) - u(i-1, j)) / (2.0 * dr))**2 + &
                                        ((v(i, j+1) - v(i, j-1)) / (2.0 * dtheta))**2)
        if (shear_stress(i, j) > Y_0) rho(i, j) = max(0.5 * rho(i, j), rho_min)
      end do
    end do

    ! Update hydrodynamics with viscosity
    do i = 2, nr - 1
      do j = 2, nt - 1
        div_v = (u(i+1, j) - u(i-1, j)) / (2.0 * dr) + (v(i, j+1) - v(i, j-1)) / (2.0 * dtheta)
        div_v = max(min(div_v, div_v_max), -div_v_max)  ! Limit divergence
        q_art = q_visc * rho(i, j) * max(-div_v, 0.0) * min(dr, r_planet * dtheta)**2
        p(i, j) = p(i, j) + q_art
        u(i, j) = u(i, j) - dt * (p(i+1, j) - p(i-1, j)) / (2.0 * dr * rho(i, j)) + dt * grav_acc(i)
        v(i, j) = v(i, j) - dt * (p(i, j+1) - p(i, j-1)) / (2.0 * dtheta * rho(i, j))
        rho(i, j) = rho(i, j) - dt * rho(i, j) * div_v
        rho(i, j) = max(rho(i, j), rho_min)  ! Enforce rho_min
        if (isnan(rho(i, j))) then
          print *, 'NaN in rho at step ', step_count, ' i=', i, ' j=', j, &
                   ' div_v=', div_v, ' p=', p(i, j)
          stop
        end if
      end do
    end do

    ! Outflow boundary conditions
    do j = 1, nt
      rho(1, j) = max(rho(2, j), rho_min)
      u(1, j) = u(2, j)
      v(1, j) = v(2, j)
      e(1, j) = max(e(2, j), 0.0)
      p(1, j) = p(2, j)
      rho(nr, j) = max(rho(nr-1, j), rho_min)
      u(nr, j) = u(nr-1, j)
      v(nr, j) = v(nr-1, j)
      e(nr, j) = max(e(nr-1, j), 0.0)
      p(nr, j) = p(nr-1, j)
    end do
    do i = 1, nr
      rho(i, 1) = max(rho(i, 2), rho_min)
      u(i, 1) = u(i, 2)
      v(i, 1) = v(i, 2)
      e(i, 1) = max(e(i, 2), 0.0)
      p(i, 1) = p(i, 2)
      rho(i, nt) = max(rho(i, nt-1), rho_min)
      u(i, nt) = u(i, nt-1)
      v(i, nt) = v(i, nt-1)
      e(i, nt) = max(e(i, nt-1), 0.0)
      p(i, nt) = p(i, nt-1)
    end do

    ! Output data
    if (mod(step_count, output_freq) == 0) then
      write(filename, '(A, I4.4, A)') 'output_t', step_count / output_freq, '.dat'
      open(unit=10, file=filename, status='replace', iostat=ierr)
      if (ierr /= 0) then
        print *, 'Error opening file: ', trim(filename), ' Error code: ', ierr
        stop
      end if
      write(10, *) t
      do i = 1, nr
        do j = 1, nt
          write(10, *) r(i), theta(j), rho(i, j), e(i, j)
        end do
      end do
      close(10)
      write(*, '(A,A,A,F10.6,A,F10.2,A,F10.2,A,F10.2)') 'Wrote file: ', trim(filename), &
           ' dt = ', dt, ' cs_max = ', cs_max, ' max rho = ', maxval(rho), &
           ' min rho = ', minval(rho, mask=rho > 0.0)
    end if

    ! Update time
    t = t + dt
  end do

end program planet_destruction
