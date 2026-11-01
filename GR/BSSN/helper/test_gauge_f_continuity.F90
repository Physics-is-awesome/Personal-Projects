! Continuity and monotonicity of the Bona-Masso slicing function used by
! slicing_mode = 3 (route C stationary Kerr).
!
! bm_f_of_alpha blends a tabulated a = 0.8 measurement with the a = 0 closed
! form f = (1 - alpha)/alpha.  The two branches must MEET.  A jump in f is a
! jump in d_t alpha, i.e. a first-order-in-space source that no amount of
! refinement removes.
!
! A plain finite-difference difference quotient cannot tell a jump from a steep
! but smooth region, because near alpha -> 0 the smooth |f'| is ~1/alpha^2.
! So this test uses the fact that a jump is STRIDE-INDEPENDENT:
!
!   d(h) = |f(alpha+h) - f(alpha-h)|
!
!   smooth f  =>  d(h) ~ 2h|f'|      so d(h/4)/d(h) ~ 0.25
!   jump      =>  d(h) = jump        so d(h/4)/d(h) ~ 1.00
!
! A sample is flagged only when BOTH the refined difference is above an
! absolute tolerance and the ratio is well above the smooth value.
program test_gauge_f_continuity
  use kinds_mod, only: dp
  use bssn_rhs_mod, only: bm_f_of_alpha
  implicit none

  real(dp), parameter :: alpha_lo = 0.02_dp
  real(dp), parameter :: alpha_hi = 0.95_dp
  integer,  parameter :: nsamp    = 20000
  real(dp), parameter :: jump_tol = 1.0e-2_dp   ! 1% of f
  real(dp), parameter :: ratio_max = 0.50_dp    ! smooth is 0.25

  ! First tabulated abscissa of the a = 0.8 table (Part H of
  ! helper/test_eq_kerr_data.F90), duplicated here on purpose: this is the
  ! branch node the test exists to police.
  real(dp), parameter :: tab_a1 = 0.03013707_dp

  ! The node that used to be the bottom of the table (R = 1.8).  The evolved
  ! lapse reaches alpha = 0.363, so this is inside the operating range and is
  ! kept as an explicit regression anchor.
  real(dp), parameter :: old_node = 0.37628835_dp

  real(dp) :: spins(3) = [0.0_dp, 0.4_dp, 0.8_dp]
  real(dp) :: a, alph, f, prev, d1, d2, ratio, delta
  real(dp) :: worst_d, worst_at, worst_ratio
  integer  :: is, js, nmono, nflag
  logical  :: failed

  failed = .false.
  delta  = 2.0_dp*(alpha_hi - alpha_lo)/real(nsamp, dp)

  write(*,'(A)') ' Bona-Masso slicing function: jump scan'
  write(*,'(A,ES9.2,A,F6.3,A,F6.3,A,I0)') '   stride = ', delta, &
    '  alpha in [', alpha_lo, ',', alpha_hi, ']  samples = ', nsamp
  write(*,'(A,F6.3,A,F6.3,A)') '   flag when |df| > ', jump_tol, &
    '  and d(h/4)/d(h) > ', ratio_max, ''

  do is = 1, size(spins)
    a = spins(is)
    worst_d = 0.0_dp
    worst_at = 0.0_dp
    worst_ratio = 0.0_dp
    nmono = 0
    nflag = 0
    prev = bm_f_of_alpha(alpha_lo, a)

    do js = 0, nsamp
      alph = alpha_lo + (alpha_hi - alpha_lo)*real(js, dp)/real(nsamp, dp)
      d1 = abs(bm_f_of_alpha(alph + delta, a) - bm_f_of_alpha(alph - delta, a))
      d2 = abs(bm_f_of_alpha(alph + 0.25_dp*delta, a) &
             - bm_f_of_alpha(alph - 0.25_dp*delta, a))
      ratio = d2/max(d1, 1.0e-300_dp)
      if (d2 > jump_tol .and. ratio > ratio_max) then
        nflag = nflag + 1
        if (d2 > worst_d) then
          worst_d = d2
          worst_at = alph
          worst_ratio = ratio
        end if
      end if

      f = bm_f_of_alpha(alph, a)
      if (f > prev + jump_tol) nmono = nmono + 1
      prev = f
      if (.not. (f > 0.0_dp) .or. f /= f) then
        write(*,'(A,F5.2,A,ES12.4,A,ES12.4)') '   a=', a, &
          '  non-positive or NaN f at alpha=', alph, '  f=', f
        failed = .true.
      end if
    end do

    ! Probe both branch nodes explicitly: a jump between two samples must not
    ! depend on whether a sample happened to land next to it.
    call probe_node(tab_a1)
    call probe_node(old_node)

    write(*,'(A,F5.2,A,I0,A,ES12.4,A,ES12.4,A,F6.3)') '   a=', a, &
      '  flagged samples = ', nflag, '  max |df| = ', worst_d, &
      '  at alpha = ', worst_at, '  ratio = ', worst_ratio
    if (nmono > 0) write(*,'(A,F5.2,A,I0)') '   a=', a, &
      '  monotonicity violations = ', nmono
    if (nflag > 0 .or. nmono > 0) failed = .true.
  end do

  write(*,'(A)') ''
  write(*,'(A)') ' Spin continuity at fixed alpha (the blend must meet the table)'
  alph = 0.363_dp; call spin_scan(alph)
  alph = 0.60_dp;  call spin_scan(alph)
  alph = 0.83_dp;  call spin_scan(alph)

  write(*,'(A)') ''
  write(*,'(A)') ' f(alpha) at the nodes that matter:'
  do is = 1, 6
    select case (is)
    case (1); alph = 0.363_dp          ! measured minimum lapse of the run
    case (2); alph = old_node          ! old table bottom
    case (3); alph = 0.50_dp
    case (4); alph = 0.70_dp
    case (5); alph = 0.82968850_dp     ! top of the table
    case (6); alph = 0.99_dp           ! far field: f must still fall
    end select
    write(*,'(A,F10.8,A,ES12.4,A,ES12.4,A,ES12.4)') '   alpha=', alph, &
      '  f(a=0)=', bm_f_of_alpha(alph, 0.0_dp), &
      '  f(a=0.4)=', bm_f_of_alpha(alph, 0.4_dp), &
      '  f(a=0.8)=', bm_f_of_alpha(alph, 0.8_dp)
  end do

  if (failed) then
    write(*,'(A)') ''
    write(*,'(A)') ' FAIL: bm_f_of_alpha is discontinuous or non-monotone.'
    write(*,'(A)') '   A jump in f is a jump in d_t alpha: an O(1) source no'
    write(*,'(A)') '   resolution can remove.  Check that the a = 0.8 table'
    write(*,'(A)') '   covers the alpha the run reaches and that the below-'
    write(*,'(A)') '   table continuation is the table''s own 1/alpha tail,'
    write(*,'(A)') '   not the a = 0 closed form.'
    error stop 'bm_f_of_alpha has a branch discontinuity'
  end if

  write(*,'(A)') ''
  write(*,'(A)') ' PASS: bm_f_of_alpha is continuous and monotone on the scan range.'

contains

  subroutine probe_node(alpha0)
    real(dp), intent(in) :: alpha0
    real(dp) :: dd1, dd2, rr
    dd1 = abs(bm_f_of_alpha(alpha0 + delta, a) - bm_f_of_alpha(alpha0 - delta, a))
    dd2 = abs(bm_f_of_alpha(alpha0 + 0.25_dp*delta, a) &
            - bm_f_of_alpha(alpha0 - 0.25_dp*delta, a))
    rr = dd2/max(dd1, 1.0e-300_dp)
    write(*,'(A,F10.8,A,ES12.4,A,ES12.4,A,F6.3)') '     node alpha=', alpha0, &
      '  d(h)=', dd1, '  d(h/4)=', dd2, '  ratio=', rr
    if (dd2 > jump_tol .and. rr > ratio_max) then
      nflag = nflag + 1
      if (dd2 > worst_d) then
        worst_d = dd2
        worst_at = alpha0
        worst_ratio = rr
      end if
    end if
  end subroutine probe_node

  ! The spin blend is f = (1-w)*closed + w*table with w = a/0.8.  It must meet
  ! the closed form at a = 0 and the table at a = 0.8.  The original code had
  ! these weights the other way round, which made f jump by 14% in the spin
  ! direction exactly at a = 0.8.
  subroutine spin_scan(alpha0)
    real(dp), intent(in) :: alpha0
    real(dp), parameter :: a_hi = 0.8_dp
    integer,  parameter :: na = 4000
    real(dp) :: da, aa, ds1, ds2, rs, wd, wa
    integer :: ja, nbad

    da = 2.0_dp*a_hi/real(na, dp)
    nbad = 0
    wd = 0.0_dp; wa = 0.0_dp
    do ja = 0, na
      aa = a_hi*real(ja, dp)/real(na, dp)
      ds1 = abs(bm_f_of_alpha(alpha0, aa + da) - bm_f_of_alpha(alpha0, aa - da))
      ds2 = abs(bm_f_of_alpha(alpha0, aa + 0.25_dp*da) &
              - bm_f_of_alpha(alpha0, aa - 0.25_dp*da))
      rs = ds2/max(ds1, 1.0e-300_dp)
      if (ds2 > jump_tol .and. rs > ratio_max) then
        nbad = nbad + 1
        if (ds2 > wd) then
          wd = ds2; wa = aa
        end if
      end if
    end do
    ! Probe the joint explicitly.
    ds1 = abs(bm_f_of_alpha(alpha0, a_hi + da) - bm_f_of_alpha(alpha0, a_hi - da))
    ds2 = abs(bm_f_of_alpha(alpha0, a_hi + 0.25_dp*da) &
            - bm_f_of_alpha(alpha0, a_hi - 0.25_dp*da))
    rs = ds2/max(ds1, 1.0e-300_dp)
    if (ds2 > jump_tol .and. rs > ratio_max) then
      nbad = nbad + 1
      if (ds2 > wd) then
        wd = ds2; wa = a_hi
      end if
    end if
    write(*,'(A,F8.5,A,I0,A,ES12.4,A,F7.4,A,F6.3)') '   alpha=', alpha0, &
      '  flagged = ', nbad, '  max |df| = ', wd, '  at a = ', wa, '  ratio = ', rs
    if (nbad > 0) failed = .true.
  end subroutine spin_scan

end program test_gauge_f_continuity
