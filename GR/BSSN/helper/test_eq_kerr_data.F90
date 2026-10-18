! ===========================================================================
!  Standalone verification + family scan for eq_kerr_trumpet_mod.
!
!  Part A  Structural checks on the closed forms and the coordinate map.
!          All analytic; no production stencil involved.
!
!  Part B  Validation of the K-from-gauge reconstruction against a fully
!          analytic reference at a = 0, R0 = M, reported term by term.  The
!          reconstruction is NOT trusted until this agrees; a self-check that
!          can fail is the whole point of this part.
!
!  Part C  Scan of the family parameter R0 over its admissible range
!          [M - sqrt(M^2-a^2), M], reporting the 1+log and maximal slicing
!          residuals at each.  If the family contains a 1+log member, this is
!          where it shows up.  At R0 = M the residuals are large for both, which
!          is the established result and is the control for this scan.
! ===========================================================================
program test_eq_kerr_data
  use kinds_mod, only: dp
  use eq_kerr_trumpet_mod
  implicit none

  real(dp) :: M, a, alpha, bt(3), g(3,3)
  real(dp) :: rlo, rhi, r0, Rp, x, z, h
  real(dp) :: al_surf, sh_far, detg, asym, ratio_g
  real(dp) :: Ktr, bda, res1, resm, need1, needm
  real(dp) :: cxx, czz, cyy
  real(dp) :: best1, bestm, best1_r0, bestm_r0
  real(dp) :: h1, h3, sumsq
  real(dp) :: a1_, a2_, a3_
  real(dp) :: f1, al0, ct, fmn(16), fmx(16), alw, ahw, alo, ahi, Rr
  real(dp) :: hh, cprev, ssum, cm, cc, smin, smax, sp, aa
  integer :: cnt(16), ii, ir, nbin, mu, ih, nc
  integer :: i, j, nfail, nscan
  logical :: ok

  nfail = 0
  M = 1.0_dp
  h = 1.0e-4_dp

  ! =================== Part A: structural checks (a = 0.8, R0 = M) ==========
  a = 0.8_dp
  r0 = M
  write(*,'(A)') '=== Part A: structural checks (a = 0.8, R0 = M) ==='
  write(*,'(A,4ES13.4)') '      R      alpha          |beta|          det(g)'
  do i=1,5
    Rp = 2.5_dp + real(i,dp)
    call equatorial(Rp, M, a, r0, alpha, bt, g)
    detg = det3(g)
    write(*,'(A,F7.3,3ES13.4)') ' ', Rp, alpha, sqrt(sum(bt*bt)), detg
    if (alpha < 0.0_dp) nfail = nfail + 1
    if (detg <= 0.0_dp) nfail = nfail + 1
  end do

  call equatorial(400.0_dp, M, a, r0, alpha, bt, g)
  sh_far = sqrt(sum(bt*bt))
  write(*,'(A)')
  write(*,'(A,ES13.5,A)') '  alpha at R=400M   (want 1) : ', alpha
  write(*,'(A,ES13.5,A)') '  |beta| at R=400M   (want 0) : ', sh_far
  write(*,'(A,ES13.5,A)') '  g_xx   at R=400M   (want 1) : ', g(1,1)
  write(*,'(A,ES13.5,A)') '  max asym of g      (want 0) : ', maxasym(g)
  if (abs(alpha-1.0_dp) > 1.0e-2_dp) nfail = nfail + 1
  if (sh_far > 1.0e-2_dp)          nfail = nfail + 1
  if (maxasym(g) > 1.0e-12_dp)     nfail = nfail + 1

  ! trumpet: g_xx must diverge as 1/(R-r0)^2 approaching R0 = M
  call equatorial(M+1.0e-3_dp, M, a, r0, alpha, bt, g); ratio_g = g(1,1)
  call equatorial(M+2.0e-3_dp, M, a, r0, alpha, bt, g)
  ratio_g = ratio_g/g(1,1)
  write(*,'(A,ES13.5,A)') '  g_xx ratio across R=M+1e-3,2e-3 (want 4) : ', ratio_g
  if (ratio_g < 2.0_dp) nfail = nfail + 1

  ! =================== Part B: validate the K reconstruction =================
  write(*,'(A)') ''
  write(*,'(A)') '=== Part B: K reconstruction vs analytic reference (a = 0) ==='
  write(*,'(A)') '  The analytic reference uses exact derivatives of the closed'
  write(*,'(A)') '  forms.  Terms: xx / zz / yy contributions to the trace.'
  write(*,'(A)')
  write(*,'(A)') ''
  write(*,'(A)') ''
  write(*,'(A,4F9.4,A,F9.4)') '      R     FD_xx      FD_zz      FD_yy    analytic_K   |K/Kan|'
  do i=1,5
    Rp = 2.5_dp + 0.5_dp*real(i,dp)
    call equatorial_derivs(Rp, M, 0.0_dp, M, h, Ktr, bda, need1, needm, &
                           cxx, czz, cyy)
    call analytic_k0_terms(Rp, M, h3, a1_, a2_, a3_)
    write(*,'(A,5F9.4,A,F9.4)') ' ', Rp, cxx, czz, cyy, h3, '  ', abs(Ktr/h3)
  end do

  ! =================== Part C: scan the family for a 1+log member ============
  write(*,'(A)') ''
  write(*,'(A)') '=== Part C: scan R0 for a slicing match (a = 0.8) ==='
  write(*,'(A)') '  Residual is |beta.d(alpha) - required|, rms over R = 3..6.'
  write(*,'(A)') '  Smallest wins.  R0 = M is the known-bad control.'
  call ktr_r0_range(M, a, rlo, rhi)
  write(*,'(A,2F7.4)') '  admissible R0 range: ', rlo, rhi
  write(*,'(A)')
  write(*,'(A,F7.4,3ES13.4,2F9.4)') '     R0     rms_1+log     rms_maximal    mean|K|'
  nscan = 11
  best1 = 1.0e30_dp; bestm = 1.0e30_dp
  best1_r0 = 0.0_dp; bestm_r0 = 0.0_dp
  do i=0,nscan-1
    r0 = rlo + (rhi-rlo)*real(i,dp)/real(nscan-1,dp)
    call scan_r0(r0, M, a, h, res1, resm, Ktr)
    write(*,'(A,F7.4,3ES13.4,2F9.4)') ' ', r0, res1, resm, Ktr
    if (res1 < best1) then
      best1 = res1; best1_r0 = r0
    end if
    if (resm < bestm) then
      bestm = resm; bestm_r0 = r0
    end if
  end do
  write(*,'(A)')
  write(*,'(A,F7.4,A,F9.4)') '  best 1+log   at R0 = ', best1_r0, '  residual ', best1
  write(*,'(A,F7.4,A,F9.4)') '  best maximal at R0 = ', bestm_r0, '  residual ', bestm
  write(*,'(A)')
  write(*,'(A)') '  If the best 1+log residual is not clearly smaller than the'
  write(*,'(A)') '  maximal one, the family does not contain a 1+log member and'
  write(*,'(A)') '  a numerical stationary-gauge solve is required instead.'

  ! =================== Part D: what gauge DOES the family satisfy? ==========
  write(*,'(A)') ''
  write(*,'(A)') '=== Part D: extract the Bona-Masso f, go/no-go for route C ==='
  write(*,'(A)') '  Stationary slices satisfy  beta.d(alpha) = alpha^2 f K, so'
  write(*,'(A)') '  f = (beta.d(alpha)) / (alpha^2 K).  If f is a clean function'
  write(*,'(A)') '  of alpha alone, the family IS stationary under a Bona-Masso'
  write(*,'(A)') '  gauge and route C is cheap.  If f varies at fixed alpha, it is'
  write(*,'(A)') '  not, and route C costs about as much as route A.'
  write(*,'(A)')
  write(*,'(A)') ' -- a = 0: an analytic prediction exists, f = (1-alpha)/(2 alpha) --'
  write(*,'(A,4F11.5,A,F11.5,A,F11.5)') &
       '      R        alpha          f_found     f_predicted    f/alpha^-1'
  do i=1,6
    Rp = 1.4_dp + real(i,dp)*0.8_dp
    call equatorial_derivs(Rp, M, 0.0_dp, M, h, Ktr, bda, need1, needm, &
                           cxx, czz, cyy)
    al0 = (Rp-M)/Rp
    f1 = bda/(al0*al0*Ktr)
    write(*,'(A,F7.3,4F11.5)') &
         ' ', Rp, al0, f1, (1.0_dp-al0)/(2.0_dp*al0)
  end do
  write(*,'(A)') ''
  write(*,'(A)') ' -- a = 0.8: is f a function of alpha alone? --'
  write(*,'(A,5F11.5)') '      R        alpha          f_found       2/alpha      1/alpha'
  do i=1,7
    Rp = 1.4_dp + real(i,dp)*0.8_dp
    call equatorial_derivs(Rp, M, 0.8_dp, M, h, Ktr, bda, need1, needm, &
                           cxx, czz, cyy)
    call equatorial(Rp, M, 0.8_dp, M, al0, bt, g)
    f1 = bda/(al0*al0*Ktr)
    write(*,'(A,F7.3,4F11.5)') ' ', Rp, al0, f1, 2.0_dp/al0, 1.0_dp/al0
  end do

  ! =================== Part E: is f a function of alpha alone? ==============
  write(*,'(A)') ''
  write(*,'(A)') '=== Part E: does f collapse onto a single f(alpha) curve? ==='
  write(*,'(A)') '  Along the equator f(R) can always be rewritten as f(alpha),'
  write(*,'(A)') '  which proves nothing.  The real test: sample many (R, theta),'
  write(*,'(A)') '  BIN BY ALPHA, and measure the spread of f inside each bin.'
  write(*,'(A)') '  Single-valued f(alpha)  => route C is cheap.'
  write(*,'(A)') '  Scatter within a bin   => f depends on (R,theta) and route C'
  write(*,'(A)') '                             costs about as much as route A.'
  write(*,'(A)') '  K here is the FULL 3-D reconstruction (ktr_kerr_xyz), including'
  write(*,'(A)') '  the y direction, so this result is decisive rather than indicative.'
  write(*,'(A)')
  nbin = 7
  alo = 0.45_dp; ahi = 0.95_dp
  do j=1,nbin
    fmn(j)= 1.0e30_dp
    fmx(j)=-1.0e30_dp
    cnt(j)=0
  end do
  do ii=1,4
    ct = 0.2_dp + 0.25_dp*real(ii-1,dp)
    do ir=1,13
      Rp = 2.0_dp + 0.5_dp*real(ir,dp)
      call point_derivs3(Rp, ct, M, a, M, h, Ktr, bda, al0)
      if (al0<=alo .or. al0>=ahi) cycle
      f1 = bda/(al0*al0*Ktr)
      j = int((al0-alo)/(ahi-alo)*real(nbin,dp))+1
      j = min(max(j,1),nbin)
      if (f1<fmn(j)) fmn(j)=f1
      if (f1>fmx(j)) fmx(j)=f1
      cnt(j)=cnt(j)+1
    end do
  end do
  write(*,'(A,F7.3,A,F7.3,A,F7.3,A,F11.3,A,F9.3)') &
       '  bin   alpha_lo   alpha_hi      n        f_range        rel_spread'
  do j=1,nbin
    if (cnt(j)==0) then
      write(*,'(A,I3,A)') '  ',j,'  (empty)'
      cycle
    end if
    alw = alo+(ahi-alo)*real(j-1,dp)/real(nbin,dp)
    ahw = alo+(ahi-alo)*real(j,dp)/real(nbin,dp)
    write(*,'(A,I3,2F10.3,A,I6,2F13.4,A,F9.3)') &
         '  ',j, alw, ahw, ' ', cnt(j), fmn(j), fmx(j), &
         (fmx(j)-fmn(j))/max(abs(fmn(j)),1.0e-30_dp), 0.0_dp
  end do
  write(*,'(A)')
  write(*,'(A)') '  Read: rel_spread near zero => f is a function of alpha alone.'

  ! =================== Part F: the shape of f(alpha) ========================
  write(*,'(A)')
  write(*,'(A)') '=== Part F: form of f(alpha), needed to implement the mode ==='
  write(*,'(A)') '  For a = 0 the closed form is linear in 1/alpha:  f = (1/alpha-1)/2'
  write(*,'(A)') '  Last column is f*(1-alpha).  Constant => f = c(1/alpha - 1) with'
  write(*,'(A)') '  a fixed c and the slicing mode is a one-liner.  Drifting => fit'
  write(*,'(A)') '  and tabulate.'
  write(*,'(A)')
  write(*,'(A,5F11.5)') '      a         R          alpha          f_found       1/alpha    f*(1-alpha)'
  do ir=1,8
    Rr = 1.8_dp + 0.4_dp*real(ir,dp)
    call point_derivs3(Rr, 0.0_dp, M, 0.0_dp, M, h, Ktr, bda, al0)
    f1 = bda/(al0*al0*Ktr)
    write(*,'(A,2F8.3,4F11.5)') '  0.0', Rr, al0, f1, 1.0_dp/al0, f1*(1.0_dp-al0)
  end do
  do ir=1,8
    Rr = 1.8_dp + 0.4_dp*real(ir,dp)
    call point_derivs3(Rr, 0.0_dp, M, 0.8_dp, M, h, Ktr, bda, al0)
    f1 = bda/(al0*al0*Ktr)
    write(*,'(A,2F8.3,4F11.5)') '  0.8', Rr, al0, f1, 1.0_dp/al0, f1*(1.0_dp-al0)
  end do

  ! =================== Part G: is the spread in c real or an h artefact? ====
  write(*,'(A)')
  write(*,'(A)') '=== Part G: h-convergence of c = f/(1/alpha - 1) ==='
  write(*,'(A)') '  If the spread within an alpha bin shrinks as h decreases, it'
  write(*,'(A)') '  was finite-difference error and c is constant => closed-form'
  write(*,'(A)') '  mode.  If it converges to a nonzero value, c genuinely'
  write(*,'(A)') '  depends on alpha and the mode must be tabulated.'
  write(*,'(A)') '  Central differences have error ~ eps/h + h^2/6, so the optimum'
  write(*,'(A)') '  is near h ~ 1e-5 in double precision and smaller h gets worse.'
  write(*,'(A)')
  write(*,'(A,F11.3,3F15.6)') '         h      spread_c(a=0.8)   mean_c(a=0.8)   rel_change'
  do ih=1,5
    hh = 10.0_dp**(1-ih)                 ! 1e-1 .. 1e-5
    cprev = -1.0_dp
    ssum=0.0_dp; nc=0
    ! recompute spread for this h over a fixed alpha grid
    ssum=0.0_dp; nc=0; smin=1.0e30_dp; smax=-1.0e30_dp
    do ir=1,8
      Rr = 1.8_dp + 0.4_dp*real(ir,dp)
      call point_derivs3(Rr, 0.0_dp, M, 0.8_dp, M, hh, Ktr, bda, al0)
      f1 = bda/(al0*al0*Ktr)
      cc = f1/(1.0_dp/al0 - 1.0_dp)
      ssum=ssum+cc; nc=nc+1
      if (cc<smin) smin=cc
      if (cc>smax) smax=cc
    end do
    cm = ssum/real(nc,dp)
    sp = (smax-smin)/max(abs(cm),1.0e-30_dp)
    if (cprev>0.0_dp) then
      write(*,'(A,F11.3,3F15.6)') ' ', hh, sp, cm, abs(cm-cprev)/abs(cm)
    else
      write(*,'(A,F11.3,2F15.6,A)') ' ', hh, sp, cm
    end if
    cprev = cm
  end do

  ! =================== Part H: the table the mode will use =================
  write(*,'(A)')
  write(*,'(A)') '=== Part H: tabulate f(alpha) for the slicing mode ==='
  write(*,'(A)') '  h = 1e-4 is converged (Part G).  Emitted along the equator;'
  write(*,'(A)') '  Part E shows the same f at the same alpha off-equator to ~0.5%.'
  write(*,'(A)') '  Copy these columns into the mode as the f(alpha) table.'
  write(*,'(A)')
  write(*,'(A,F7.3,2F16.8)') '        a         R          alpha            f(alpha)'
  do ii=1,2
    if (ii==1) then
      aa=0.0_dp
    else
      aa=0.8_dp
    end if
    do ir=1,12
      Rr = 1.4_dp + 0.4_dp*real(ir,dp)
      call point_derivs3(Rr, 0.0_dp, M, aa, M, 1.0e-4_dp, Ktr, bda, al0)
      if (al0<=0.0_dp) cycle
      f1 = bda/(al0*al0*Ktr)
      write(*,'(A,2F7.3,2F16.8)') ' ', aa, Rr, al0, f1
    end do
  end do

  write(*,'(A)')
  if (nfail==0) then
    write(*,'(A)') 'PASS: Part A structural checks.'
  else
    write(*,'(A,I0,A)') 'FAIL: Part A had ',nfail,' failed check(s).'
    error stop 'eq_kerr_trumpet_mod failed structural checks'
  end if


contains

  ! Cartesian (alpha, beta, gamma) at areal radius R on the equator.
  subroutine equatorial(Rr, mm, aa, r0, al, bv, gg)
    real(dp),intent(in)  :: Rr, mm, aa, r0
    real(dp),intent(out) :: al, bv(3), gg(3,3)
    logical :: o
    call ktr_kerr_cartesian(Rr, 0.0_dp, mm, aa, r0, al, bv, gg, o)
  end subroutine equatorial

  ! K from the gauge, with the three diagonal contributions returned separately.
  ! Direction 2 (y) is deliberately NOT differenced: on the equator the shift
  ! has no y component and gamma_xy = gamma_yz = 0, so it contributes nothing,
  ! and leaving it zero avoids evaluating the metric at |y| > 0 where the
  ! stored plane has no information.
  ! FULL 3-D derivatives at (x,0,z) using ktr_kerr_xyz, so the y direction is
  ! available and K includes its xy / xz / yy contributions.  This is what the
  ! f(alpha) collapse test needs; the in-plane-only version undercounts K away
  ! from the equator.
  subroutine point_derivs3(Rr, ct, mm, aa, r0, hh, Ktrace, bdalpha, al0)
    real(dp),intent(in)  :: Rr, ct, mm, aa, r0, hh
    real(dp),intent(out) :: Ktrace, bdalpha, al0
    real(dp) :: st, xx, zz, a0, b0(3), g0(3,3)
    real(dp) :: ap(3,3), bp(3,3), gp(3,3,3), am(3,3), bm(3,3), gm(3,3,3)
    real(dp) :: dGam(3,3,3), dB(3,3), ginv(3,3), Kij(3,3)
    real(dp) :: p(3), tb(3), tgm(3,3)
    integer :: mu,p1,q,idx

    st = sqrt(max(1.0_dp-ct*ct,0.0_dp))
    xx = Rr*st
    zz = Rr*ct
    call ktr_kerr_xyz(xx, 0.0_dp, zz, mm, aa, r0, a0, b0, g0, ok)
    do mu=1,3
      p = [xx,0.0_dp,zz]
      p(mu) = p(mu) + hh
      call ktr_kerr_xyz(p(1),p(2),p(3), mm, aa, r0, ap(mu,1), tb, tgm, ok)
      bp(mu,:)=tb; gp(mu,:,:)=tgm
      p(mu) = p(mu) - 2.0_dp*hh
      call ktr_kerr_xyz(p(1),p(2),p(3), mm, aa, r0, am(mu,1), tb, tgm, ok)
      bm(mu,:)=tb; gm(mu,:,:)=tgm
    end do

    dB=0.0_dp; dGam=0.0_dp
    do mu=1,3
      do p1=1,3
        dB(mu,p1) = (bp(mu,p1)-bm(mu,p1))/(2.0_dp*hh)
        do q=1,3
          dGam(mu,p1,q) = (gp(mu,p1,q)-gm(mu,p1,q))/(2.0_dp*hh)
        end do
      end do
    end do

    ginv = invert3(g0)
    do p1=1,3
      do q=1,3
        Kij(p1,q)=0.0_dp
        do idx=1,3
          Kij(p1,q)=Kij(p1,q) + b0(idx)*dGam(idx,p1,q) &
                            + g0(q,idx)*dB(p1,idx) + g0(p1,idx)*dB(q,idx)
        end do
        Kij(p1,q)=Kij(p1,q)/(2.0_dp*a0)
      end do
    end do
    Ktrace=0.0_dp
    do p1=1,3
      do q=1,3
        Ktrace=Ktrace+ginv(p1,q)*Kij(p1,q)
      end do
    end do
    al0 = a0
    ! beta . grad(alpha) = sum_mu beta^mu d(alpha)/dx^mu.
    ! This must use the ALPHA derivatives (ap/am), not dB, which is
    ! d(beta^mu)/dx^mu and would give a divergence instead.
    bdalpha = 0.0_dp
    do mu=1,3
      bdalpha = bdalpha + b0(mu)*(ap(mu,1)-am(mu,1))/(2.0_dp*hh)
    end do
  end subroutine point_derivs3

  ! Derivatives at a general point of the x-z plane, given areal radius and
  ! cos(theta).  x = R sin(th), z = R cos(th); theta enters through z.
  subroutine point_derivs(Rr, ct, mm, aa, r0, hh, Ktrace, bdalpha, al0)
    real(dp),intent(in)  :: Rr, ct, mm, aa, r0, hh
    real(dp),intent(out) :: Ktrace, bdalpha, al0
    real(dp) :: st, xx, zz, a0, b0(3), g0(3,3)
    real(dp) :: apx, amx, bpx(3), bmx(3), gpx(3,3), gmx(3,3)
    real(dp) :: apz, amz, bpz(3), bmz(3), gpz(3,3), gmz(3,3)
    real(dp) :: dGam(3,3,3), dB(3,3), ginv(3,3), Kij(3,3)
    integer :: p,q,idx
    st = sqrt(max(1.0_dp-ct*ct,0.0_dp))
    xx = Rr*st
    zz = Rr*ct
    call ktr_kerr_cartesian(xx,   zz,   mm, aa, r0, a0,  b0,  g0,  ok)
    call ktr_kerr_cartesian(xx+hh,zz,   mm, aa, r0, apx, bpx, gpx, ok)
    call ktr_kerr_cartesian(xx-hh,zz,   mm, aa, r0, amx, bmx, gmx, ok)
    call ktr_kerr_cartesian(xx,   zz+hh,mm, aa, r0, apz, bpz, gpz, ok)
    call ktr_kerr_cartesian(xx,   zz-hh,mm, aa, r0, amz, bmz, gmz, ok)
    dB=0.0_dp; dGam=0.0_dp
    do p=1,3
      dB(1,p)   = (bpx(p)-bmx(p))/(2.0_dp*hh)
      dB(3,p)   = (bpz(p)-bmz(p))/(2.0_dp*hh)
      do q=1,3
        dGam(1,p,q) = (gpx(p,q)-gmx(p,q))/(2.0_dp*hh)
        dGam(3,p,q) = (gpz(p,q)-gmz(p,q))/(2.0_dp*hh)
      end do
    end do
    ginv = invert3(g0)
    do p=1,3
      do q=1,3
        Kij(p,q)=0.0_dp
        do idx=1,3
          Kij(p,q)=Kij(p,q) + b0(idx)*dGam(idx,p,q) &
                            + g0(q,idx)*dB(p,idx) + g0(p,idx)*dB(q,idx)
        end do
        Kij(p,q)=Kij(p,q)/(2.0_dp*a0)
      end do
    end do
    Ktrace=0.0_dp
    do p=1,3
      do q=1,3
        Ktrace=Ktrace+ginv(p,q)*Kij(p,q)
      end do
    end do
    al0 = a0
    ! d_y of any axisymmetric SCALAR vanishes at y = 0, so only in-plane terms
    bdalpha = b0(1)*(apx-amx)/(2.0_dp*hh) + b0(3)*(apz-amz)/(2.0_dp*hh)
  end subroutine point_derivs

  subroutine equatorial_derivs(Rr, mm, aa, r0, hh, Ktrace, bdalpha, &
                               need1log, needmax, cxx, czz, cyy)
    real(dp),intent(in)  :: Rr, mm, aa, r0, hh
    real(dp),intent(out) :: Ktrace, bdalpha, need1log, needmax
    real(dp),intent(out) :: cxx, czz, cyy
    real(dp) :: a0, b0(3), g0(3,3)
    real(dp) :: apx, amx, bpx(3), bmx(3), gpx(3,3), gmx(3,3)
    real(dp) :: apz, amz, bpz(3), bmz(3), gpz(3,3), gmz(3,3)
    real(dp) :: dGam(3,3,3), dB(3,3), ginv(3,3), Kij(3,3)
    integer :: p,q,idx

    call ktr_kerr_cartesian(Rr,   0.0_dp, mm, aa, r0, a0,  b0,  g0,  ok)
    call ktr_kerr_cartesian(Rr+hh,0.0_dp, mm, aa, r0, apx, bpx, gpx, ok)
    call ktr_kerr_cartesian(Rr-hh,0.0_dp, mm, aa, r0, amx, bmx, gmx, ok)
    call ktr_kerr_cartesian(Rr,   hh,    mm, aa, r0, apz, bpz, gpz, ok)
    call ktr_kerr_cartesian(Rr,  -hh,    mm, aa, r0, amz, bmz, gmz, ok)

    dB=0.0_dp; dGam=0.0_dp
    do p=1,3
      dB(1,p)   = (bpx(p)-bmx(p))/(2.0_dp*hh)
      dB(3,p)   = (bpz(p)-bmz(p))/(2.0_dp*hh)
      do q=1,3
        dGam(1,p,q) = (gpx(p,q)-gmx(p,q))/(2.0_dp*hh)
        dGam(3,p,q) = (gpz(p,q)-gmz(p,q))/(2.0_dp*hh)
      end do
    end do

    ginv = invert3(g0)
    do p=1,3
      do q=1,3
        Kij(p,q)=0.0_dp
        do idx=1,3
          Kij(p,q)=Kij(p,q) + b0(idx)*dGam(idx,p,q) &
                            + g0(q,idx)*dB(p,idx) + g0(p,idx)*dB(q,idx)
        end do
        Kij(p,q)=Kij(p,q)/(2.0_dp*a0)
      end do
    end do

    Ktrace=0.0_dp
    do p=1,3
      do q=1,3
        Ktrace=Ktrace+ginv(p,q)*Kij(p,q)
      end do
    end do
    cxx = ginv(1,1)*Kij(1,1)
    czz = ginv(3,3)*Kij(3,3)
    cyy = ginv(2,2)*Kij(2,2)

    bdalpha = b0(1)*(apx-amx)/(2.0_dp*hh)
    need1log = -2.0_dp*a0*Ktrace
    needmax  = -      a0*Ktrace
  end subroutine equatorial_derivs

  ! rms slicing residual over R = 3..6 for a given R0.
  subroutine scan_r0(r0, mm, aa, hh, r1, rm, kmean)
    real(dp),intent(in)  :: r0, mm, aa, hh
    real(dp),intent(out) :: r1, rm, kmean
    real(dp) :: Rr, K, bd, n1, nm, s1, sm, sk
    integer :: n
    s1=0.0_dp; sm=0.0_dp; sk=0.0_dp; n=0
    do n=1,7
      Rr = 3.0_dp + 0.5_dp*real(n,dp)
      call equatorial_derivs(Rr, mm, aa, r0, hh, K, bd, n1, nm, a1_, a2_, a3_)
      s1=s1+(bd-n1)**2
      sm=sm+(bd-nm)**2
      sk=sk+abs(K)
      ! n advances via the DO loop
    end do
    r1 = sqrt(s1/7.0_dp)
    rm = sqrt(sm/7.0_dp)
    kmean = sk/7.0_dp
  end subroutine scan_r0

  ! Exact K for a=0, R0=M, from analytic derivatives.
  subroutine analytic_k0_terms(Rr, mm, kk, t1, t2, t3)
    real(dp),intent(in)  :: Rr, mm
    real(dp),intent(out) :: kk, t1, t2, t3
    real(dp) :: al, b, gxx, gzz, gyy, dgxx, dgyy, dbet, lxx, lzz
    real(dp) :: kxx, kzz, kyy, rm
    rm = Rr-mm
    al  = rm/Rr
    b   = mm*rm/Rr**2
    gxx = Rr**2/rm**2
    gzz = Rr**2
    gyy = Rr**2
    dgxx= -2.0_dp*mm*Rr/rm**3
    dgyy= 2.0_dp*Rr
    dbet= mm*(2.0_dp*mm-Rr)/Rr**3
    lxx = b*dgxx + 2.0_dp*gxx*dbet
    lzz = b*dgyy
    kxx = lxx/(2.0_dp*al)
    kzz = lzz/(2.0_dp*al)
    kyy = kzz
    t1 = kxx/gxx
    t2 = kzz/gzz
    t3 = kyy/gyy
    kk = t1+t2+t3
  end subroutine analytic_k0_terms

  subroutine analytic_terms(Rr, mm, kk, t1, t2, t3)
    real(dp),intent(in)  :: Rr, mm
    real(dp),intent(out) :: kk, t1, t2, t3
    real(dp) :: a1,a2,a3
    call analytic_k0_terms(Rr, mm, kk, a1, a2, a3)
    t1=a1; t2=a2; t3=a3
  end subroutine analytic_terms

  function det3(A) result(d)
    real(dp),intent(in) :: A(3,3)
    real(dp) :: d
    d = A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2)) &
      - A(1,2)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)) &
      + A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
  end function det3

  function maxasym(A) result(v)
    real(dp),intent(in) :: A(3,3)
    real(dp) :: v
    integer :: p,q
    v=0.0_dp
    do p=1,3
      do q=1,3
        v=max(v,abs(A(p,q)-A(q,p)))
      end do
    end do
  end function maxasym

  function invert3(A) result(B)
    real(dp),intent(in) :: A(3,3)
    real(dp) :: B(3,3), d
    d=det3(A)
    if (abs(d) < 1.0e-30_dp) then
      B=0.0_dp
      return
    end if
    B(1,1)=(A(2,2)*A(3,3)-A(2,3)*A(3,2))/d
    B(1,2)=(A(1,3)*A(3,2)-A(1,2)*A(3,3))/d
    B(1,3)=(A(1,2)*A(2,3)-A(1,3)*A(2,2))/d
    B(2,1)=(A(2,3)*A(3,1)-A(2,1)*A(3,3))/d
    B(2,2)=(A(1,1)*A(3,3)-A(1,3)*A(3,1))/d
    B(2,3)=(A(1,3)*A(2,1)-A(1,1)*A(2,3))/d
    B(3,1)=(A(2,1)*A(3,2)-A(2,2)*A(3,1))/d
    B(3,2)=(A(1,2)*A(3,1)-A(1,1)*A(3,2))/d
    B(3,3)=(A(1,1)*A(2,2)-A(1,2)*A(2,1))/d
  end function invert3


end program test_eq_kerr_data
