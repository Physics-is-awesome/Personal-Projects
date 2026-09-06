module timestep_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS
  use bssn_rhs_mod, only: compute_rhs_interior
  use dissipation_mod, only: add_ko_dissipation
  use sommerfeld_mod, only: add_sommerfeld_bc
  implicit none
  private
  public :: rk4_step

contains

  subroutine rk4_step(u, dt, stepnum)
    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(in)    :: dt
    integer,  intent(in), optional :: stepnum
    real(dp), allocatable :: k1(:,:,:), k2(:,:,:), k3(:,:,:), k4(:,:,:), utmp(:,:,:)
    integer :: n1, n2, n3

    n1 = size(u,1); n2 = size(u,2); n3 = size(u,3)
    allocate(k1(n1,n2,n3), k2(n1,n2,n3), k3(n1,n2,n3), k4(n1,n2,n3), utmp(n1,n2,n3))

    k1 = 0.0_dp
    call compute_rhs_interior(u, k1);      call report_bad("k1 after interior", k1, stepnum)
    call add_ko_dissipation(u, k1);        call report_bad("k1 after KO",       k1, stepnum)
    call add_sommerfeld_bc(u, k1);         call report_bad("k1 after Sommerfeld", k1, stepnum)

    utmp = u + 0.5_dp*dt*k1
    k2 = 0.0_dp
    call compute_rhs_interior(utmp, k2);   call report_bad("k2 after interior", k2, stepnum)
    call add_ko_dissipation(utmp, k2);     call report_bad("k2 after KO",       k2, stepnum)
    call add_sommerfeld_bc(utmp, k2);      call report_bad("k2 after Sommerfeld", k2, stepnum)

    utmp = u + 0.5_dp*dt*k2
    k3 = 0.0_dp
    call compute_rhs_interior(utmp, k3);   call report_bad("k3 after interior", k3, stepnum)
    call add_ko_dissipation(utmp, k3);     call report_bad("k3 after KO",       k3, stepnum)
    call add_sommerfeld_bc(utmp, k3);      call report_bad("k3 after Sommerfeld", k3, stepnum)

    utmp = u + dt*k3
    k4 = 0.0_dp
    call compute_rhs_interior(utmp, k4);   call report_bad("k4 after interior", k4, stepnum)
    call add_ko_dissipation(utmp, k4);     call report_bad("k4 after KO",       k4, stepnum)
    call add_sommerfeld_bc(utmp, k4);      call report_bad("k4 after Sommerfeld", k4, stepnum)

    u = u + (dt/6.0_dp)*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)

    deallocate(k1, k2, k3, k4, utmp)
  end subroutine rk4_step

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
