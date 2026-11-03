! Compare the production f(alpha) table with a dense equatorial DBM reference,
! and independently measure angular variation of the pointwise required f.
program diagnose_f_alpha_relation
  use kinds_mod, only: dp
  use params_mod, only: bh_spin, init_params
  use eq_kerr_bssn_data_mod, only: state_at
  use eq_kerr_trumpet_mod, only: ktr_kerr_xyz
  use bssn_rhs_mod, only: bm_f_of_alpha
  implicit none
  integer, parameter :: NB=96, NR=121, NC=121
  real(dp), parameter :: RAD_LO=1.55_dp, RAD_HI=8.0_dp
  real(dp), parameter :: ALO=0.25_dp, AHI=0.85_dp
  real(dp) :: amin(NB), amax(NB), tabmax(NB), angmax(NB), tabsum(NB), angsum(NB)
  real(dp) :: fit_num(NB),fit_den(NB),fit_val(NB)
  integer, parameter :: NRAD=3
  real(dp) :: fit_num_r(NB,NRAD),fit_den_r(NB,NRAD),fit_val_r(NB,NRAD)
  real(dp) :: tab_sse_r(NRAD),global_sse_r(NRAD),local_sse_r(NRAD),weight_r(NRAD)
  integer :: count_r(NRAD)
  real(dp) :: R, ct, x, z, a, freq, ftab, feq, ae, rmid, rlo, rhi, fm, q, q_sample, wt
  real(dp) :: max_tab, max_ang, sum_tab, sum_ang, nall, fit_sse, tab_wsse, fit_w
  integer :: ir, ic, ib, it, jb, cnt(NB)
  character(len=256) :: parfile
  logical :: ok

  call get_command_argument(1,parfile)
  if (len_trim(parfile)==0) parfile='parameter_files/vacuum_kerr.par'
  call init_params(trim(parfile))
  amin=huge(1.0_dp); amax=-huge(1.0_dp); tabmax=0.0_dp; angmax=0.0_dp
  tabsum=0.0_dp; angsum=0.0_dp; cnt=0
  fit_num=0.0_dp;fit_den=0.0_dp;fit_val=0.0_dp
  fit_num_r=0.0_dp;fit_den_r=0.0_dp;fit_val_r=0.0_dp
  tab_sse_r=0.0_dp;global_sse_r=0.0_dp;local_sse_r=0.0_dp
  weight_r=0.0_dp;count_r=0
  max_tab=0.0_dp; max_ang=0.0_dp; sum_tab=0.0_dp; sum_ang=0.0_dp; nall=0.0_dp
  fit_sse=0.0_dp;tab_wsse=0.0_dp;fit_w=0.0_dp

  write(*,'(A,F6.3,A)') 'Pointwise DBM slicing relation, spin=',bh_spin, &
       ' (required f vs dense equatorial reference and production table)'
  do ir=0,NR-1
    R=RAD_LO+(RAD_HI-RAD_LO)*real(ir,dp)/real(NR-1,dp)
    do ic=0,NC-1
      ct=real(ic,dp)/real(NC-1,dp)
      x=R*sqrt(max(0.0_dp,1.0_dp-ct*ct)); z=R*ct
      call required_f(x,z,a,freq,q,ok)
      if (.not.ok .or. a<=ALO .or. a>=AHI) cycle
      q_sample=q

      ! Invert alpha(R,theta=pi/2) with a tight bracket. This constructs a
      ! dense equatorial reference at exactly the sample's lapse, without
      ! conflating its discretization with the production table nodes.
      rlo=1.05_dp; rhi=6.20_dp
      do it=1,48
        rmid=0.5_dp*(rlo+rhi)
        call required_f(rmid,0.0_dp,ae,feq,q,ok)
        if (.not.ok) exit
        if (ae<a) then
          rlo=rmid
        else
          rhi=rmid
        end if
      end do
      if (.not.ok) cycle
      ftab=bm_f_of_alpha(a,bh_spin)
      fm=abs(ftab-feq)       ! interpolation error against dense equatorial curve
      if (ir==3 .and. ic==0) write(*,'(A,5ES16.7)') &
           'probe R=3 equator: alpha, f_required, f_equator, f_table, R_eq=', &
           a,freq,feq,ftab,rmid
      ib=min(NB,max(1,int((a-ALO)/(AHI-ALO)*NB)+1))
      ! Minimize the continuum lapse residual R_alpha=q*(f_required-f),
      ! with q=alpha^2 K, using spherical-volume quadrature weights.
      wt=q_sample*q_sample*R*R
      if (R<3.0_dp) then
        jb=1
      else if (R<5.0_dp) then
        jb=2
      else
        jb=3
      end if
      amin(ib)=min(amin(ib),a); amax(ib)=max(amax(ib),a); cnt(ib)=cnt(ib)+1
      tabmax(ib)=max(tabmax(ib),fm); tabsum(ib)=tabsum(ib)+fm*fm
      angmax(ib)=max(angmax(ib),abs(freq-feq)); angsum(ib)=angsum(ib)+(freq-feq)**2
      fit_num(ib)=fit_num(ib)+wt*freq;fit_den(ib)=fit_den(ib)+wt
      fit_num_r(ib,jb)=fit_num_r(ib,jb)+wt*freq
      fit_den_r(ib,jb)=fit_den_r(ib,jb)+wt
      max_tab=max(max_tab,fm); max_ang=max(max_ang,abs(freq-feq))
      sum_tab=sum_tab+fm*fm; sum_ang=sum_ang+(freq-feq)**2; nall=nall+1.0_dp
    end do
  end do
  write(*,'(A)') ' alpha bin        n      max table error     rms table error'// &
       '      max angular delta      rms angular delta'
  do ib=1,NB
    if (cnt(ib)==0) cycle
    write(*,'(F6.3,A,F6.3,I7,4ES22.10)') amin(ib),'..',amax(ib),cnt(ib), &
         tabmax(ib),sqrt(tabsum(ib)/cnt(ib)),angmax(ib),sqrt(angsum(ib)/cnt(ib))
  end do
  write(*,'(A,4ES18.8)') 'All samples: max table, RMS table, max angular, RMS angular =', &
       max_tab,sqrt(sum_tab/max(nall,1.0_dp)),max_ang,sqrt(sum_ang/max(nall,1.0_dp))
  do ib=1,NB
    if (fit_den(ib)<=0.0_dp) cycle
    fit_val(ib)=fit_num(ib)/fit_den(ib)
    do jb=1,NRAD
      if (fit_den_r(ib,jb)>0.0_dp) fit_val_r(ib,jb)=fit_num_r(ib,jb)/fit_den_r(ib,jb)
    end do
  end do
  ! Revisit the samples to evaluate the piecewise fit directly, avoiding
  ! cancellation in sum(w*f^2)-sum(w*f)^2/sum(w).
  do ir=0,NR-1
    R=RAD_LO+(RAD_HI-RAD_LO)*real(ir,dp)/real(NR-1,dp)
    do ic=0,NC-1
      ct=real(ic,dp)/real(NC-1,dp)
      x=R*sqrt(max(0.0_dp,1.0_dp-ct*ct));z=R*ct
      call required_f(x,z,a,freq,q,ok)
      if (.not.ok .or. a<=ALO .or. a>=AHI) cycle
      q_sample=q
      rlo=1.05_dp;rhi=6.20_dp
      do it=1,48
        rmid=0.5_dp*(rlo+rhi)
        call required_f(rmid,0.0_dp,ae,feq,q,ok)
        if (.not.ok) exit
        if (ae<a) then
          rlo=rmid
        else
          rhi=rmid
        end if
      end do
      if (.not.ok) cycle
      ib=min(NB,max(1,int((a-ALO)/(AHI-ALO)*NB)+1))
      wt=q_sample*q_sample*R*R
      if (R<3.0_dp) then
        jb=1
      else if (R<5.0_dp) then
        jb=2
      else
        jb=3
      end if
      ftab=bm_f_of_alpha(a,bh_spin)
      tab_wsse=tab_wsse+wt*(freq-ftab)**2
      fit_w=fit_w+wt
      fit_sse=fit_sse+wt*(freq-fit_val(ib))**2
      tab_sse_r(jb)=tab_sse_r(jb)+wt*(freq-ftab)**2
      global_sse_r(jb)=global_sse_r(jb)+wt*(freq-fit_val(ib))**2
      local_sse_r(jb)=local_sse_r(jb)+wt*(freq-fit_val_r(ib,jb))**2
      weight_r(jb)=weight_r(jb)+wt
      count_r(jb)=count_r(jb)+1
    end do
  end do
  write(*,'(A,2ES18.8)') 'q^2 volume-weighted lapse residual RMS: table, best binned f(alpha) =', &
       sqrt(tab_wsse/max(fit_w,1.0e-300_dp)),sqrt(fit_sse/max(fit_w,1.0e-300_dp))
  write(*,'(A)') 'radial band    table RMS   global-fit RMS   local-fit RMS    samples'
  write(*,'(A,3ES14.5,I10)') ' 1.55..3.00 ',sqrt(tab_sse_r(1)/max(weight_r(1),1.0e-300_dp)), &
       sqrt(global_sse_r(1)/max(weight_r(1),1.0e-300_dp)), &
       sqrt(local_sse_r(1)/max(weight_r(1),1.0e-300_dp)),count_r(1)
  write(*,'(A,3ES14.5,I10)') ' 3.00..5.00 ',sqrt(tab_sse_r(2)/max(weight_r(2),1.0e-300_dp)), &
       sqrt(global_sse_r(2)/max(weight_r(2),1.0e-300_dp)), &
       sqrt(local_sse_r(2)/max(weight_r(2),1.0e-300_dp)),count_r(2)
  write(*,'(A,3ES14.5,I10)') ' 5.00..8.00 ',sqrt(tab_sse_r(3)/max(weight_r(3),1.0e-300_dp)), &
       sqrt(global_sse_r(3)/max(weight_r(3),1.0e-300_dp)), &
       sqrt(local_sse_r(3)/max(weight_r(3),1.0e-300_dp)),count_r(3)
  write(*,'(A)') 'alpha bin    best-fit f(alpha)   table f(alpha)   samples'
  do ib=1,NB
    if (fit_den(ib)>0.0_dp) write(*,'(F6.3,A,F6.3,2ES18.8,I8)') amin(ib),'..',amax(ib), &
         fit_num(ib)/fit_den(ib),bm_f_of_alpha(0.5_dp*(amin(ib)+amax(ib)),bh_spin),cnt(ib)
  end do

contains
  subroutine required_f(x,y,alpha,f,q,good)
    real(dp), intent(in) :: x,y
    real(dp), intent(out) :: alpha,f,q
    logical, intent(out) :: good
    real(dp), parameter :: h=1.0e-4_dp
    real(dp) :: phi,gt(3,3),at(3,3),K,beta(3),b1(3),g(3,3),ap,am,grad
    call state_at(x,0.0_dp,y,1.0_dp,bh_spin,1.0_dp,phi,gt,at,K,alpha,beta,good)
    q=alpha*alpha*K
    if (.not.good .or. abs(q)<1.0e-14_dp) then
      good=.false.; f=0.0_dp; q=0.0_dp; return
    end if
    call ktr_kerr_xyz(x+h,0.0_dp,y,1.0_dp,bh_spin,1.0_dp,ap,b1,g,good)
    call ktr_kerr_xyz(x-h,0.0_dp,y,1.0_dp,bh_spin,1.0_dp,am,b1,g,good)
    grad=beta(1)*(ap-am)/(2.0_dp*h)
    call ktr_kerr_xyz(x,0.0_dp,y+h,1.0_dp,bh_spin,1.0_dp,ap,b1,g,good)
    call ktr_kerr_xyz(x,0.0_dp,y-h,1.0_dp,bh_spin,1.0_dp,am,b1,g,good)
    grad=grad+beta(3)*(ap-am)/(2.0_dp*h)
    f=grad/q
  end subroutine required_f
end program diagnose_f_alpha_relation
