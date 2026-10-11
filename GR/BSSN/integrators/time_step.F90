module timestep_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, IALPHA
  use params_mod, only: Nx, Nz, dx
  use grid_mod, only: xg, zg
  use excision_mod, only: is_excised, extrapolate_excised
  use bssn_rhs_mod, only: compute_rhs_interior
  use dissipation_mod, only: add_ko_dissipation
  use sommerfeld_mod, only: add_sommerfeld_bc
  use excision_sat_mod, only: apply_legacy_excision_sat
  use params_mod, only: excision_sat
  implicit none
  private
  public :: rk4_step
  real(dp), parameter :: lapse_floor = 1.0e-6_dp

contains

  subroutine rk4_step(u, dt, stepnum)
    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(in)    :: dt
    integer,  intent(in), optional :: stepnum
    real(dp), allocatable :: k1(:,:,:), k2(:,:,:), k3(:,:,:), k4(:,:,:), utmp(:,:,:)
    integer :: n1, n2, n3

    n1 = size(u,1); n2 = size(u,2); n3 = size(u,3)
    allocate(k1(n1,n2,n3), k2(n1,n2,n3), k3(n1,n2,n3), k4(n1,n2,n3), utmp(n1,n2,n3))

    ! Refresh the current state before the first RK stage.  The final state
    ! from the previous step has new exterior values, so its ghost layer
    ! cannot be reused for k1.
    call extrapolate_excised(u)
    k1 = 0.0_dp
    call compute_rhs_interior(u, k1);      call report_bad("k1 after interior", k1, stepnum)
    call apply_legacy_excision_sat(u, k1, dx, excision_sat)
    call add_ko_dissipation(u, k1);        call report_bad("k1 after KO",       k1, stepnum)
    call add_sommerfeld_bc(u, k1);         call report_bad("k1 after Sommerfeld", k1, stepnum)

    utmp = u + 0.5_dp*dt*k1
    call extrapolate_excised(utmp)
    call project_bssn_constraints(utmp, stepnum)
    k2 = 0.0_dp
    call compute_rhs_interior(utmp, k2);   call report_bad("k2 after interior", k2, stepnum)
    call apply_legacy_excision_sat(utmp, k2, dx, excision_sat)
    call add_ko_dissipation(utmp, k2);     call report_bad("k2 after KO",       k2, stepnum)
    call add_sommerfeld_bc(utmp, k2);      call report_bad("k2 after Sommerfeld", k2, stepnum)

    utmp = u + 0.5_dp*dt*k2
    call extrapolate_excised(utmp)
    call project_bssn_constraints(utmp, stepnum)
    k3 = 0.0_dp
    call compute_rhs_interior(utmp, k3);   call report_bad("k3 after interior", k3, stepnum)
    call apply_legacy_excision_sat(utmp, k3, dx, excision_sat)
    call add_ko_dissipation(utmp, k3);     call report_bad("k3 after KO",       k3, stepnum)
    call add_sommerfeld_bc(utmp, k3);      call report_bad("k3 after Sommerfeld", k3, stepnum)

    utmp = u + dt*k3
    call extrapolate_excised(utmp)
    call project_bssn_constraints(utmp, stepnum)
    k4 = 0.0_dp
    call compute_rhs_interior(utmp, k4);   call report_bad("k4 after interior", k4, stepnum)
    call apply_legacy_excision_sat(utmp, k4, dx, excision_sat)
    call add_ko_dissipation(utmp, k4);     call report_bad("k4 after KO",       k4, stepnum)
    call add_sommerfeld_bc(utmp, k4);      call report_bad("k4 after Sommerfeld", k4, stepnum)

    u = u + (dt/6.0_dp)*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
    call project_bssn_constraints(u, stepnum)

    deallocate(k1, k2, k3, k4, utmp)
  end subroutine rk4_step

  subroutine project_bssn_constraints(u, stepnum)
    real(dp), intent(inout) :: u(:,:,:)
    integer, intent(in), optional :: stepnum
    integer :: i, k
    real(dp) :: gxx, gxy, gxz, gyy, gyz, gzz, det, scale
    real(dp) :: ginv(3,3), A(3,3), trace_A

    do k = 1, Nz
      do i = 1, Nx
        if (is_excised(i,k)) cycle
        if (u(i,k,IALPHA) /= u(i,k,IALPHA)) then
          call invalid_state('non-finite lapse', i, k, stepnum)
        else if (u(i,k,IALPHA) < lapse_floor) then
          u(i,k,IALPHA) = lapse_floor
        end if

        gxx = u(i,k,IGXX); gxy = u(i,k,IGXY); gxz = u(i,k,IGXZ)
        gyy = u(i,k,IGYY); gyz = u(i,k,IGYZ); gzz = u(i,k,IGZZ)
        det = gxx*(gyy*gzz-gyz*gyz) - gxy*(gxy*gzz-gyz*gxz) &
            + gxz*(gxy*gyz-gyy*gxz)
        if (det /= det .or. det <= 0.0_dp) then
          write(*,'(A,ES14.6,A,6(1X,ES14.6))') &
            'Conformal metric failure det=', det, ' components=', &
            gxx, gxy, gxz, gyy, gyz, gzz
          call invalid_state('non-positive or non-finite conformal determinant', i, k, stepnum)
        end if

        scale = det**(-1.0_dp/3.0_dp)
        u(i,k,IGXX) = scale*gxx; u(i,k,IGXY) = scale*gxy
        u(i,k,IGXZ) = scale*gxz; u(i,k,IGYY) = scale*gyy
        u(i,k,IGYZ) = scale*gyz; u(i,k,IGZZ) = scale*gzz

        ginv(1,1) = u(i,k,IGYY)*u(i,k,IGZZ)-u(i,k,IGYZ)**2
        ginv(1,2) = u(i,k,IGXZ)*u(i,k,IGYZ)-u(i,k,IGXY)*u(i,k,IGZZ)
        ginv(1,3) = u(i,k,IGXY)*u(i,k,IGYZ)-u(i,k,IGXZ)*u(i,k,IGYY)
        ginv(2,2) = u(i,k,IGXX)*u(i,k,IGZZ)-u(i,k,IGXZ)**2
        ginv(2,3) = u(i,k,IGXY)*u(i,k,IGXZ)-u(i,k,IGXX)*u(i,k,IGYZ)
        ginv(3,3) = u(i,k,IGXX)*u(i,k,IGYY)-u(i,k,IGXY)**2
        ginv(2,1) = ginv(1,2); ginv(3,1) = ginv(1,3); ginv(3,2) = ginv(2,3)

        A = 0.0_dp
        A(1,1) = u(i,k,IAXX); A(1,2) = u(i,k,IAXY); A(1,3) = u(i,k,IAXZ)
        A(2,1) = A(1,2); A(2,2) = u(i,k,IAYY); A(2,3) = u(i,k,IAYZ)
        A(3,1) = A(1,3); A(3,2) = A(2,3); A(3,3) = u(i,k,IAZZ)
        trace_A = sum(ginv*A)
        A = A - (trace_A/3.0_dp) * reshape([ &
          u(i,k,IGXX), u(i,k,IGXY), u(i,k,IGXZ), &
          u(i,k,IGXY), u(i,k,IGYY), u(i,k,IGYZ), &
          u(i,k,IGXZ), u(i,k,IGYZ), u(i,k,IGZZ) ], [3,3])
        u(i,k,IAXX) = A(1,1); u(i,k,IAXY) = A(1,2); u(i,k,IAXZ) = A(1,3)
        u(i,k,IAYY) = A(2,2); u(i,k,IAYZ) = A(2,3); u(i,k,IAZZ) = A(3,3)
      end do
    end do
  end subroutine project_bssn_constraints

  subroutine invalid_state(reason, i, k, stepnum)
    character(len=*), intent(in) :: reason
    integer, intent(in) :: i, k
    integer, intent(in), optional :: stepnum
    if (present(stepnum)) then
      write(*,'(A,I0,A,I0,A,I0,A,F10.5,A,F10.5)') &
        'Invalid state at step ', stepnum, ' i=', i, ' k=', k, &
        ' x=', xg(i), ' z=', zg(k)
    else
      write(*,'(A,I0,A,I0,A,F10.5,A,F10.5)') &
        'Invalid state at i=', i, ' k=', k, ' x=', xg(i), ' z=', zg(k)
    end if
    error stop reason
  end subroutine invalid_state

  subroutine report_bad(label, arr, stepnum)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: arr(:,:,:)
    integer, intent(in), optional :: stepnum
    integer :: i, k, v, n1, n2, n3
    logical :: bad

    n1 = size(arr,1); n2 = size(arr,2); n3 = size(arr,3)
    do k = 1, n2
      do i = 1, n1
        do v = 1, n3
          bad = (arr(i,k,v) /= arr(i,k,v))                 ! NaN
          if (.not. bad) bad = (abs(arr(i,k,v)) > 1.0e30_dp)  ! blown-up/Inf-ish
          if (bad) then
            if (present(stepnum)) then
              print '(A,I0,A,A,A,I0,A,I0,A,I0)', " [step ", stepnum, "] first bad value at ", &
                label, ": i=", i, " k=", k, " field=", v
            else
              print '(A,A,A,I0,A,I0,A,I0)', " first bad value at ", label, ": i=", i, " k=", k, &
                " field=", v
            end if
            return
          end if
        end do
      end do
    end do
  end subroutine report_bad

end module timestep_mod
