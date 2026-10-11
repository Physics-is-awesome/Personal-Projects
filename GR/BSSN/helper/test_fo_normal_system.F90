program test_fo_normal_system
  use kinds_mod, only: dp
  use fo_normal_system_mod
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  use vars_mod, only: IGXX, IGYY, IGZZ, IALPHA, IK, IPHI
  implicit none
  integer :: b, n
  real(dp) :: A(FO_MAX_BLOCK,FO_MAX_BLOCK), lam(FO_MAX_BLOCK)
  real(dp) :: R(FO_MAX_BLOCK,FO_MAX_BLOCK), L(FO_MAX_BLOCK,FO_MAX_BLOCK), Ierr, Derr
  character(len=32) :: labels(FO_MAX_BLOCK)
  real(dp) :: q(FO_NSTATE), normal(3), Af(FO_NSTATE,FO_NSTATE)
  real(dp) :: lambda_full(FO_NSTATE), Rfull(FO_NSTATE,FO_NSTATE), Lfull(FO_NSTATE,FO_NSTATE)
  character(len=32) :: full_labels(FO_NSTATE)
  logical :: supported(FO_NSTATE)
  real(dp) :: qc(NFO_COMPLETE_VARS), Ac(NFO_COMPLETE_VARS,NFO_COMPLETE_VARS)
  real(dp) :: lc(NFO_COMPLETE_VARS)
  character(len=32) :: clabels(NFO_COMPLETE_VARS)
  logical :: csupported(NFO_COMPLETE_VARS)
  do b=FO_BLOCK_LAPSE,FO_BLOCK_SHIFT
    call fo_normal_block_eigensystem(b,1.3_dp,0.07_dp,1.7_dp,0.21_dp,n,A,lam,R,L,labels)
    Ierr=maxval(abs(matmul(L(1:n,1:n),R(1:n,1:n))-identity(n)))
    Derr=maxval(abs(matmul(matmul(L(1:n,1:n),A(1:n,1:n)),R(1:n,1:n))-diag(lam(1:n))))
    if (Ierr > 1.0e-13_dp .or. Derr > 1.0e-13_dp) error stop 'normal eigensystem test failed'
    if (len_trim(labels(1)) == 0) error stop 'missing mode label'
    write(*,'(A,A,A,2(ES12.4,1X))') trim(fo_normal_block_name(b)), ' modes: ', trim(labels(1)), Ierr, Derr
  end do

  ! The full API intentionally exposes only the state-dependent transport
  ! completion; the legacy gauge/gravity couplings remain classified as
  ! unsupported rather than being silently invented.
  q = 0.0_dp
  q(2) = 1.0_dp; q(5) = 1.0_dp; q(7) = 1.0_dp
  q(18) = 1.3_dp; q(19:21) = [0.2_dp, -0.1_dp, 0.05_dp]
  normal = [1.0_dp, 2.0_dp, -1.0_dp]
  call fo_normal_state_eigensystem(q,normal,Af,lambda_full,Rfull,Lfull,full_labels,supported)
  Ierr=maxval(abs(matmul(Lfull,Rfull)-identity(FO_NSTATE)))
  Derr=maxval(abs(matmul(matmul(Lfull,Af),Rfull)-diag(lambda_full)))
  if (Ierr > 1.0e-13_dp .or. Derr > 1.0e-13_dp) error stop 'full normal eigensystem test failed'
  if (supported(1) .or. .not. supported(FO_NSTATE)) error stop 'mode support classification failed'
  if (fo_normal_mode_class(full_labels(1)) /= FO_MODE_UNSUPPORTED .or. &
      fo_normal_mode_class(full_labels(FO_NSTATE)) /= FO_MODE_ADVECTION) &
      error stop 'mode class test failed'
  write(*,'(A,2(ES12.4,1X))') 'state transport symbol: ', Ierr, Derr

  qc = 0.0_dp
  qc(IGXX) = 1.0_dp; qc(IGYY) = 1.0_dp; qc(IGZZ) = 1.0_dp
  qc(IALPHA) = 1.2_dp
  call fo_complete_normal_matrix(qc, [0.6_dp,0.0_dp,0.8_dp], Ac, lc, clabels, csupported)
  if (maxval(abs(Ac - transpose(Ac))) <= 0.0_dp) then
    error stop 'complete normal matrix was not populated'
  end if
  if (csupported(IK)) error stop 'unsupported K gauge coupling was misclassified'
  if (.not. csupported(fo_complete_index(IPHI,1))) &
    error stop 'reduction transport mode was not classified'
  write(*,'(A,ES12.4)') 'complete normal matrix gauge coupling: ', &
    abs(Ac(IK,fo_complete_index(IALPHA,1)))

  ! -------------------------------------------------------------------------
  ! Published characteristic spectrum: Brown et al. PRD 85 084004 (2012),
  ! Sec. III.C, eqs. (3.22)-(3.30), for f = 2/alpha (1+log) and
  ! G = 3/(4 alpha^2), H = e^{4 phi} (Gamma-driver).
  ! -------------------------------------------------------------------------
  call check_published()

contains

  subroutine check_published()
    real(dp) :: alpha, phi, gnn, bn, gn
    real(dp) :: g(FO_NG_NGAUGE), ng(FO_NN_NGAUGE)
    real(dp) :: c_gh, c_f, c_lam
    integer :: i, nbad

    nbad = 0
    alpha = 0.8_dp; phi = -0.15_dp; gnn = 1.3_dp; bn = 0.21_dp
    gn = sqrt(gnn)
    c_gh  = (sqrt(3.0_dp)/2.0_dp)*exp(2.0_dp*phi)   ! alpha sqrt(G H)
    c_f   = sqrt(2.0_dp*alpha)                    ! alpha sqrt(f)
    c_lam = exp(2.0_dp*phi)                        ! alpha lambda

    ! The paper lists 20 independent gauge variables but only 19 characteristic
    ! fields: the trace of beta_i^j is not an independent field, so the two
    ! counts differ by one.  Check the FIELD multiplicities against the paper's
    ! list rather than equating them with the variable count.
    if (FO_NG_ZERO /= 11 .or. FO_NG_GH /= 4 .or. FO_NG_F /= 2 .or. FO_NG_LAM /= 2) then
      write(*,'(A,4(1X,I0))') ' FAIL: gauge multiplicities', &
           FO_NG_ZERO, FO_NG_GH, FO_NG_F, FO_NG_LAM
      nbad = nbad + 1
    end if
    if (FO_NN_ZERO /= 27 .or. FO_NN_PM /= 3) then
      write(*,'(A,2(1X,I0))') ' FAIL: non-gauge multiplicities', FO_NN_ZERO, FO_NN_PM
      nbad = nbad + 1
    end if
    if (FO_N_GAUGE /= 20 .or. FO_N_NONGAGE /= 32) then
      write(*,'(A,2(1X,I0))') ' FAIL: published variable counts', FO_N_GAUGE, FO_N_NONGAGE
      nbad = nbad + 1
    end if

    call fo_published_speeds(alpha, phi, gnn, bn, g, ng)

    ! zero-speed sector
    do i = 1, FO_NG_ZERO
      if (abs(g(i) - bn) > 1.0e-13_dp) then
        write(*,'(A,I0,ES12.4,A,ES12.4)') ' FAIL: gauge zero mode ', i, g(i), ' vs ', bn
        nbad = nbad + 1
        exit
      end if
    end do

    ! alpha sqrt(G H) family, then alpha sqrt(f), then alpha lambda
    call pair_check('G H', g(FO_NG_ZERO+1), g(FO_NG_ZERO+2), bn + c_gh*gn, nbad)
    call pair_check('G H', g(FO_NG_ZERO+3), g(FO_NG_ZERO+4), bn + c_gh*gn, nbad)
    call pair_check('f  ', g(FO_NG_ZERO+FO_NG_GH+1), g(FO_NG_ZERO+FO_NG_GH+2), &
                    bn + c_f*gn, nbad)
    call pair_check('lam', g(FO_NG_ZERO+FO_NG_GH+FO_NG_F+1), &
                    g(FO_NG_ZERO+FO_NG_GH+FO_NG_F+2), bn + c_lam*gn, nbad)

    ! non-gauge: 27 zero, then 3 pairs at beta_n +- alpha
    do i = 1, FO_NN_ZERO
      if (abs(ng(i) - bn) > 1.0e-13_dp) then
        write(*,'(A,I0,ES12.4)') ' FAIL: non-gauge zero mode ', i, ng(i)
        nbad = nbad + 1
        exit
      end if
    end do
    do i = 1, FO_NN_PM
      if (abs(ng(FO_NN_ZERO+2*i-1) - (bn + alpha*gn)) > 1.0e-13_dp) &
        call nbad_inc(nbad)
      if (abs(ng(FO_NN_ZERO+2*i)   - (bn - alpha*gn)) > 1.0e-13_dp) &
        call nbad_inc(nbad)
    end do

    ! strong-hyperbolicity condition 4GH != 3f.
    ! 4GH - 3f = 3 e^{4phi}/alpha^2 - 6/alpha = (3 e^{4phi} - 6 alpha)/alpha^2
    if (abs(fo_gauge_condition_residual(alpha, phi) &
            - (3.0_dp*exp(4.0_dp*phi) - 6.0_dp*alpha)/(alpha*alpha)) > 1.0e-10_dp) then
      write(*,'(A)') ' FAIL: gauge-condition residual mismatch'
      nbad = nbad + 1
    end if

    write(*,'(A,4(1X,ES11.3))') ' published gauge  +-GH, +-f, +-lam:', &
         g(FO_NG_ZERO+1)-bn, g(FO_NG_ZERO+FO_NG_GH+1)-bn, &
         g(FO_NG_ZERO+FO_NG_GH+FO_NG_F+1)-bn, g(FO_NG_NGAUGE)-bn
    write(*,'(A,ES11.3,A,I0,A,I0)') ' published non-gauge +-alpha:', &
         ng(FO_NN_ZERO+1)-bn, '   modes gauge=', FO_NG_NGAUGE, ' nongauge=', FO_NN_NGAUGE
    write(*,'(A,ES11.4)') ' 4GH-3f (hyperbolicity, must be nonzero) = ', &
         fo_gauge_condition_residual(alpha, phi)

    if (nbad > 0) error stop 'published characteristic spectrum mismatch'
  end subroutine check_published

  subroutine pair_check(tag, vplus, vminus, want_plus, nbad)
    character(len=3), intent(in) :: tag
    real(dp), intent(in) :: vplus, vminus, want_plus
    integer, intent(inout) :: nbad
    if (abs(vplus - want_plus) > 1.0e-13_dp) then
      write(*,'(A,A,2(1X,ES12.4))') ' FAIL: ', tag, ' plus', vplus, ' vs ', want_plus
      nbad = nbad + 1
    end if
    if (abs(vminus - (2.0_dp*0.21_dp - want_plus)) > 1.0e-13_dp) then
      write(*,'(A,A,2(1X,ES12.4))') ' FAIL: ', tag, ' minus', vminus
      nbad = nbad + 1
    end if
  end subroutine pair_check

  subroutine nbad_inc(nbad)
    integer, intent(inout) :: nbad
    nbad = nbad + 1
  end subroutine nbad_inc

  function identity(n) result(a)
    integer,intent(in)::n; real(dp)::a(n,n); integer::j
    a=0.0_dp; do j=1,n; a(j,j)=1.0_dp; end do
  end function identity
  function diag(x) result(a)
    real(dp),intent(in)::x(:); real(dp)::a(size(x),size(x)); integer::j
    a=0.0_dp; do j=1,size(x); a(j,j)=x(j); end do
  end function diag
end program test_fo_normal_system
