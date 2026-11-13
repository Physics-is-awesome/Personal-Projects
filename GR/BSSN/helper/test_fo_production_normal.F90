program test_fo_production_normal
  ! Gate the analytic normal principal blocks of the production BSSN RHS.
  ! Measured A1/A2 are extracted by linear and quadratic Frechet probes of
  ! compute_rhs_point; A2 (second-normal) is compared with the full analytic
  ! matrix, and A1 (first-normal) is compared for the rows whose principal
  ! part is derived without background Christoffel/connection perturbations
  ! (phi, conformal metric, lapse, shift).  The remaining A1 rows (K, Atilde,
  ! Gamma, B) and the full characteristic transform are still open P2.2 work.
  ! No SAT or penalty is enabled.
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, bh_spin, &
                        initial_data_mode, gamma_driver_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, alloc_state
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, is_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_point
  use fo_production_normal_mod, only: production_normal_second_matrix, &
                                      production_normal_first_matrix, &
                                      production_normal_var_labels
  use fo_curved_sat_mod, only: kerr_schild_excision_normal, kerr_radius
  implicit none

  real(dp), allocatable :: u(:,:,:), ubase(:,:,:)
  real(dp) :: rhs0(NVARS), qpoint(NVARS)
  real(dp) :: c1a(NVARS,NVARS), c1anal(NVARS,NVARS)
  real(dp) :: c2(NVARS,NVARS), c2a(NVARS,NVARS)
  character(len=32) :: labels(NVARS), labels1(NVARS), varlabels(NVARS)
  logical :: supported(NVARS), supported1(NVARS)
  real(dp) :: normal(3), x0, z0, r_target, rbest, nrm
  real(dp) :: amp, err2, errall, err1, errall1, scale
  integer :: i, k, ii, kk, v, nbad, nbad1
  integer :: idir, out_unit
  character(len=256) :: out_file
  logical :: have_out

  call init_params('parameter_files/vacuum_kerr.par')
  call production_normal_var_labels(varlabels)
  out_file = ''
  call get_command_argument(1, out_file)
  have_out = len_trim(out_file) > 0
  if (have_out) then
    open(newunit=out_unit, file=trim(out_file), status='replace', action='write')
    write(out_unit,'(A)') '# production BSSN normal matrix: measured from rhs_point'
    write(out_unit,'(A)') '# row <- column : A1 (first normal), A2 (second normal)'
  end if
  if (initial_data_mode /= 1) error stop 'test_fo_production_normal: route-C required'
  if (gamma_driver_mode /= 4) error stop 'test_fo_production_normal: driver mode 4 required'
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(ubase(Nx,Nz,NVARS))

  call set_eq_kerr_trumpet_id(u, 1.0_dp, bh_spin, 1.0_dp)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)
  ubase = u

  ! Pick a central-stencil point a few cells outside the level set.  Staying
  ! away from the one-sided stencil band is essential: this test is about the
  ! continuum normal symbol, not about the boundary closure.
  r_target = r_excise + 4.0_dp*max(dx,dz)
  rbest = huge(1.0_dp)
  ii = -1; kk = -1
  do k = 5, Nz - 4
    do i = 5, Nx - 4
      if (is_excised(i,k)) cycle
      if (abs(xg(i)) < 2.0_dp*dx) cycle
      if (abs(kerr_radius(xg(i), zg(k), bh_spin) - r_target) < rbest) then
        rbest = abs(kerr_radius(xg(i), zg(k), bh_spin) - r_target)
        ii = i; kk = k
      end if
    end do
  end do
  if (ii < 0) error stop 'test_fo_production_normal: no sample point'
  x0 = xg(ii); z0 = zg(kk)
  if (is_excised(ii,kk)) error stop 'test_fo_production_normal: sample point excised'
  if (ii < 5 .or. ii > Nx - 4 .or. kk < 5 .or. kk > Nz - 4) &
    error stop 'test_fo_production_normal: sample point not central'

  call compute_rhs_point(ubase, ii, kk, rhs0)
  if (any(rhs0 /= rhs0)) error stop 'test_fo_production_normal: non-finite base RHS'

  if (have_out) then
    write(out_unit,'(A,3ES20.12)') '# point x,z,r = ', x0, z0, &
      kerr_radius(x0,z0,bh_spin)
  end if

  amp = 1.0e-6_dp
  errall = 0.0_dp
  errall1 = 0.0_dp
  nbad = 0
  nbad1 = 0

  do idir = 1, 3
    select case (idir)
    case (1)
      normal = [1.0_dp, 0.0_dp, 0.0_dp]
    case (2)
      normal = [0.0_dp, 0.0_dp, 1.0_dp]
    case default
      call kerr_schild_excision_normal(x0, z0, bh_spin, r_excise, &
                                       normal(1), normal(3), nrm)
      normal(2) = 0.0_dp
      normal = normal/sqrt(sum(normal*normal))
    end select

    c1a = 0.0_dp; c2 = 0.0_dp
    do v = 1, NVARS
      call measure_column(ubase, v, ii, kk, normal, x0, z0, amp, &
                          rhs0, c1a(:,v), c2(:,v))
    end do

    qpoint = ubase(ii,kk,:)
    call production_normal_second_matrix(qpoint, normal, c2a, labels, supported)
    call production_normal_first_matrix(qpoint, normal, c1anal, labels1, supported1)
    err2 = 0.0_dp
    err1 = 0.0_dp
    do v = 1, NVARS
      do k = 1, NVARS
        scale = max(1.0_dp, abs(c2a(v,k)))
        err2 = max(err2, abs(c2(v,k)-c2a(v,k))/scale)
        if (idir /= 1) then
          if (abs(c2(v,k)-c2a(v,k)) > 3.0e-3_dp*scale) then
            nbad = nbad + 1
            if (nbad <= 40) &
              write(*,'(A,A,A,A,3ES13.5)') '  FAIL C2 ', trim(varlabels(v)), &
                ' <- ', trim(varlabels(k)), c2(v,k), c2a(v,k), &
                c2(v,k)-c2a(v,k)
          end if
        end if
        if (supported1(v)) then
          scale = max(1.0_dp, abs(c1anal(v,k)))
          err1 = max(err1, abs(c1a(v,k)-c1anal(v,k))/scale)
          ! A1 linear probe is contaminated by Cartoon y reconstruction for
          ! normals with n_x != 0 in the axisymmetric code; only z-normal is
          ! an exact m=0 check for first derivatives.
          if (idir == 2) then
            if (abs(c1a(v,k)-c1anal(v,k)) > 3.0e-3_dp*scale) then
              nbad1 = nbad1 + 1
              if (nbad1 <= 40) &
                write(*,'(A,A,A,A,3ES13.5)') '  FAIL A1 ', trim(varlabels(v)), &
                  ' <- ', trim(varlabels(k)), c1a(v,k), c1anal(v,k), &
                  c1a(v,k)-c1anal(v,k)
            end if
          end if
        end if
      end do
    end do
    if (idir /= 1) errall = max(errall, err2)
    if (idir == 2) errall1 = max(errall1, err1)

    write(*,'(A,I1,A,2F10.5,A,A,ES12.4)') 'normal ', idir, &
      ' n=(', normal(1), normal(3), ')', ' C2 err=', err2
    if (idir == 1) write(*,'(A)') &
      '  diagnostic only: x-normal is not an m=0 plane wave in the Cartoon code'
    write(*,'(A,ES12.4)') '  A1 err (derived rows)=', err1
    write(*,'(A,ES12.4)') '  max measured A1 magnitude=', maxval(abs(c1a))
    if (have_out) call write_matrix(out_unit, idir, normal, c1a, c2)
    write(*,'(A,3ES13.5)') '  point x,z,r=', x0, z0, &
      kerr_radius(x0,z0,bh_spin)
  end do

  write(*,'(A,ES12.4,A,I0)') 'max relative C2 error=', errall, &
    '  entries outside tolerance=', nbad
  write(*,'(A,ES12.4,A,I0)') 'max relative A1 error (derived rows, z normal)=', errall1, &
    '  entries outside tolerance=', nbad1
  ! Print one compact row norm per field so the archived log identifies which
  ! production blocks were exercised.
  do v = 1, NVARS
    write(*,'(A24,1X,A24,1X,ES12.4)') trim(labels1(v)), trim(labels(v)), maxval(abs(c2a(v,:)))
  end do

  if (have_out) close(out_unit)
  if (nbad /= 0) error stop 'test_fo_production_normal: C2 mismatch'
  if (errall > 5.0e-3_dp) error stop 'test_fo_production_normal: C2 error too large'
  if (nbad1 /= 0) error stop 'test_fo_production_normal: A1 mismatch in derived rows'
  if (errall1 > 5.0e-3_dp) error stop 'test_fo_production_normal: A1 error too large in derived rows'
  write(*,'(A)') 'production normal principal matrices: PASS'
  write(*,'(A)') '  C2: all 24 rows (z/radial gate)'
  write(*,'(A)') '  A1 derived rows (z-normal gate): phi, gt, alpha, beta'
  write(*,'(A)') '  x/radial A1 and x C2: diagnostic (Cartoon m=0 contamination)'
  write(*,'(A)') '  A1 open rows (K,Atilde,Gamma,B), characteristic transform: NOT TESTED'
contains

  subroutine measure_column(u0, v, ip, kp, n, xp, zp, a, r0, a1, a2)
    real(dp), intent(in) :: u0(:,:,:), n(3), xp, zp, a
    real(dp), intent(in) :: r0(NVARS)
    integer, intent(in) :: v, ip, kp
    real(dp), intent(out) :: a1(NVARS), a2(NVARS)
    real(dp) :: r1(NVARS)

    ! Linear probe: value 0, gradient a*n, second derivative 0 at x0.
    u = u0
    call add_poly(u, v, a, n, xp, zp, 1)
    call compute_rhs_point(u, ip, kp, r1)
    a1 = (r1-r0)/a

    ! Quadratic probe: value 0, gradient 0, Hessian a*n n at x0.
    u = u0
    call add_poly(u, v, 0.5_dp*a, n, xp, zp, 2)
    call compute_rhs_point(u, ip, kp, r1)
    a2 = (r1-r0)/a

    u = u0
  end subroutine measure_column

  subroutine write_matrix(unit, tag, nn, a1, a2)
    integer, intent(in) :: unit, tag
    real(dp), intent(in) :: nn(3), a1(NVARS,NVARS), a2(NVARS,NVARS)
    integer :: iv, jv
    write(unit,'(A,I1,A,2F12.6,A)') '# normal ', tag, ' n=(', nn(1), nn(3), ')'
    do iv = 1, NVARS
      do jv = 1, NVARS
        write(unit,'(A,A,A,2ES18.9)') trim(varlabels(iv)), ' <- ', &
          trim(varlabels(jv)), a1(iv,jv), a2(iv,jv)
      end do
    end do
  end subroutine write_matrix

  subroutine add_poly(uu, v, a, nn, xp, zp, order)
    real(dp), intent(inout) :: uu(:,:,:)
    integer, intent(in) :: v, order
    real(dp), intent(in) :: a, nn(3), xp, zp
    real(dp) :: s
    integer :: i, k
    do k = 1, Nz
      do i = 1, Nx
        s = nn(1)*(xg(i)-xp) + nn(3)*(zg(k)-zp)
        if (order == 1) then
          uu(i,k,v) = uu(i,k,v) + a*s
        else
          uu(i,k,v) = uu(i,k,v) + a*s*s
        end if
      end do
    end do
  end subroutine add_poly

end program test_fo_production_normal
