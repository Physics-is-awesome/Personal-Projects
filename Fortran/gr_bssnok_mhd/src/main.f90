program main
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IALPHA, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use constraints_mod, only: hamiltonian_constraint
  use timestep_mod, only: rk4_step
  use excision_mod, only: extrapolate_excised
  implicit none

  real(dp), allocatable :: u(:,:,:), Hgrid(:,:)
  integer :: imin, imax, kmin, kmax
  real(dp) :: Hmax, Hrms
  integer :: n, npts
  real(dp) :: dt, t
  integer :: axis_k, i, k
  real(dp) :: diag_margin
  character(len=64) :: buf
  integer :: envstat

  call init_params()
  call setup_grid()
  call alloc_state(u, Nx, Nz)

  diag_margin = 1.0_dp
  call get_environment_variable("GR_DIAG_MARGIN", buf, status=envstat)
  if (envstat == 0) read(buf,*) diag_margin

  print *, "=== GR BSSNOK (vacuum, Cartoon axisymmetric) - Milestone 1 ==="
  print '(A,I0,A,I0)', " Grid: Nx=", Nx, "  Nz=", Nz
  print '(A,F6.3,A,F6.3,A,F6.3)', " dx=", dx, "  dy=", dy, "  dz=", dz
  print '(A,F6.3,A,F6.3)', " BH mass M=", bh_mass, "   spin a=", bh_spin
  print '(A,F6.3,A)', " Excision radius r_excise=", r_excise, "  (horizon at r=2M; interior frozen)"
  print *

  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)

  allocate(Hgrid(Nx,Nz))
  call hamiltonian_constraint(u, Hgrid, imin, imax, kmin, kmax)
  Hmax = maxval(abs(Hgrid(imin:imax,kmin:kmax)))
  Hrms = sqrt(sum(Hgrid(imin:imax,kmin:kmax)**2) / real((imax-imin+1)*(kmax-kmin+1), dp))
  print '(A,ES12.4,A,ES12.4)', " t=0 Hamiltonian constraint (whole domain): max|H| = ", Hmax, &
    "   rms|H| = ", Hrms

  block
    integer :: ii, kk0, loc(2)
    real(dp) :: rr
    loc = maxloc(abs(Hgrid(imin:imax,kmin:kmax)))
    ii = loc(1) + imin - 1; kk0 = loc(2) + kmin - 1
    rr = sqrt(xg(ii)**2 + zg(kk0)**2)
    print '(A,I0,A,I0,A,F8.4,A,ES12.4)', " worst point: i=", ii, " k=", kk0, &
      "  varpi~r=", rr, "  H=", Hgrid(ii,kk0)
    print *, " H at a few sample radii along the axis (x=0):"
    do kk0 = kmin, kmax, max(1,(kmax-kmin)/8)
      print '(A,F8.4,A,ES12.4)', "   z=", zg(kk0), "   H=", Hgrid(imin,kk0)
    end do
  end block

  ! Representative axis point comfortably outside the excision radius, used
  ! for the lapse trace below (should evolve smoothly, unlike the frozen
  ! interior).
  axis_k = 1
  do k = kmin, kmax
    if (sqrt(xg(1)**2 + zg(k)**2) >= r_excise + 1.0_dp) then
      axis_k = k
      exit
    end if
  end do
  print '(A,F10.6,A,F8.4)', " Lapse alpha(x=0, outside excision) at t=0: ", u(1,axis_k,IALPHA), &
    "  at z=", zg(axis_k)
  print *

  dt = cfl * min(dx, dz)
  t = 0.0_dp
  print '(A,F8.5)', " Time step dt = ", dt
  print *, "Evolving (vacuum, 1+log/Gamma-driver puncture gauge, excised interior)..."

  do n = 1, nsteps
    call rk4_step(u, dt, n)
    call extrapolate_excised(u)
    t = t + dt

    ! lightweight safety net: report (but do not spam) any NaN outside excision
    block
      integer :: ii, kk0
      logical :: found
      found = .false.
      do kk0 = 1, Nz
        do ii = 1, Nx
          if (sqrt(xg(ii)**2 + zg(kk0)**2) < r_excise) cycle
          if (u(ii,kk0,IALPHA) /= u(ii,kk0,IALPHA)) then
            if (.not. found) then
              print '(A,I0,A,F8.4,A,F8.4)', " NaN outside excision at step ", n, &
                "  x=", xg(ii), "  z=", zg(kk0)
              found = .true.
            end if
          end if
        end do
      end do
    end block

    if (mod(n, out_every) == 0 .or. n == nsteps) then
      call hamiltonian_constraint(u, Hgrid, imin, imax, kmin, kmax)

      ! Stats over the exterior (evolved) region only.
      Hmax = 0.0_dp; Hrms = 0.0_dp; npts = 0
      do k = kmin, kmax
        do i = imin, imax
          if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
          Hmax = max(Hmax, abs(Hgrid(i,k)))
          Hrms = Hrms + Hgrid(i,k)**2
          npts = npts + 1
        end do
      end do
      Hrms = sqrt(Hrms / real(max(npts,1), dp))

      print '(A,I5,A,F8.4,A,ES12.4,A,ES12.4,A,F10.6)', &
        " step ", n, "  t=", t, "  max|H|(ext)=", Hmax, "  rms|H|(ext)=", Hrms, &
        "  alpha(probe)=", u(1,axis_k,IALPHA)

      block
        integer :: ii, kk0, bi, bk
        real(dp) :: bval, rr
        bval = 0.0_dp; bi = -1; bk = -1
        do k = kmin, kmax
          do i = imin, imax
            if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
            if (abs(Hgrid(i,k)) > bval) then
              bval = abs(Hgrid(i,k)); bi = i; bk = k
            end if
          end do
        end do
        if (bi > 0) then
          rr = sqrt(xg(bi)**2 + zg(bk)**2)
          print '(A,F8.4,A,F8.4)', "    (worst point beyond margin at r=", rr, &
            "  vs r_excise+margin=", r_excise + diag_margin
        end if
      end block
    end if
  end do

end program main
