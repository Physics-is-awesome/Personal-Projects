! ===========================================================================
!  Self-test for "compute K from the gauge" machinery.
!
!  Why this exists
!  ---------------
!  To adopt an analytic Kerr trumpet data set (Dennison, Baumgarte & Montero,
!  arXiv:1409.1887) we need K_ij from a supplied (gamma, alpha, beta) rather
!  than from stored BSSN variables.  That derivation is new code and must be
!  validated BEFORE it is pointed at data that supplies no K at all.
!
!  The Schwarzschild stationary 1+log trumpet is the right control: its K is
!  already pinned to an independent spherical anchor (eq_mean_curvature) to
!  1e-5 by test_eq_schwarzschild_data, so the stored K is trustworthy.
!
!  What this test does
!  -------------------
!  Recomputes K_ij from the gauge alone via
!
!      K_ij = (1 / (2 alpha)) * ( L_beta gamma )_ij
!      ( L_beta gamma )_ij = d_i(b^m gamma^m_j) + d_j(b^m gamma^m_i)
!                          - d_m(b^m) gamma_ij
!
!  and compares against the stored tensor, reconstructed from the stored trace
!  and trace-free part through the exact BSSN identity
!
!      K_ij = exp(4 phi) * ( Atilde_ij + (1/3) gtilde_ij K ).
!
!  Everything uses the production stencil, the production cartoon
!  reconstruction, and the production conformal-geometry routine.  This
!  validates the assembly and the cartoon derivatives, which is what can
!  silently go wrong; it does not independently validate those operators,
!  which test_fo_rhs and the equilibrium gates already cover.
!
!  NOTE: contractions are written as explicit loops, not SUM(...,dim=).  That
!  intrinsic form requires a genuine array as its first argument in gfortran,
!  and rejects a product of array elements.
! ===========================================================================
program test_k_from_gauge
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: alloc_state, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IK, IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                      IALPHA, IBETAX, IBETAY, IBETAZ
  use eq_schwarzschild_data_mod, only: set_eq_schwarzschild_id
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, &
                           excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry
  use equilibrium_1pluslog_mod, only: eq_mean_curvature
  implicit none

  real(dp), allocatable :: u(:,:,:)
  type(stencil_t) :: S
  type(conformal_geometry_t) :: G

  real(dp) :: gt(3,3), gti(3,3), gam(3,3), gami(3,3)
  real(dp) :: bv(3), dphi(3), V(3), dV(3,3)
  real(dp) :: W(0:3,3,3,3)      ! W(dir,m,a,b); dir=0 slot holds the value
  real(dp) :: Kgauge(3,3), Kstored(3,3)
  real(dp) :: wKg(3,3), wKs(3,3), wgt(3,3), wg(3,3), wb(3), wal
  integer :: wi, wk
  character(len=32) :: ybuf
  real(dp) :: wr
  real(dp) :: dgam(3,3,3)         ! dgam(a,b,dir)
  real(dp) :: divb, e4phi, r, kex, ktrg, ktrs, ws, Lbg
  real(dp) :: trmax, trrms, kmax, kerr, npt, tol
  integer :: i, k, a, b, c, m, d, dir
  logical :: dbg, no_yderivs

  call init_params('parameter_files/vacuum_kerr.par')
  bh_spin = 0.0_dp
  r_excise = 1.4_dp
  Nx = 96; Nz = 192
  call get_environment_variable("GR_NX",ybuf)
  if (ybuf/=" ") read(ybuf,*) Nx
  call get_environment_variable("GR_NZ",ybuf)
  if (ybuf/=" ") read(ybuf,*) Nz
  dx = xmax/real(Nx-1,dp); dz = 2.0_dp*zmax/real(Nz-1,dp); dy = dx
  slicing_mode = 0
  gamma_driver_mode = 2            ! frozen shift: this test is geometric only
  call setup_grid()
  call alloc_state(u, Nx, Nz)

  call set_eq_schwarzschild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  trmax=0.0_dp; trrms=0.0_dp; kmax=0.0_dp; kerr=0.0_dp; npt=0.0_dp
  dbg=.false.
  no_yderivs=.false.
  call get_environment_variable("GR_NO_YDERIVS",ybuf)
  if (ybuf/=" ") no_yderivs=.true.

  do k=2,Nz-1
    do i=5,Nx-1
      r = sqrt(xg(i)**2 + zg(k)**2)
      if (r < 2.6_dp .or. r > 5.0_dp) cycle
      if (excision_status(i,k) /= PHYSICAL) cycle

      call compute_stencil(u, i, k, excision_status, S)
      call compute_conformal_geometry(S, G)

      e4phi = exp(4.0_dp*S%val(IPHI))
      gt  = G%gt
      gti = G%gtinv
      gam = e4phi*gt

      ! physical inverse metric
      do a=1,3
        do b=1,3
          gami(a,b)=0.0_dp
          do c=1,3
            do m=1,3
              gami(a,b)=gami(a,b)+gti(a,c)*gti(b,m)*gt(c,m)
            end do
          end do
          gami(a,b)=gami(a,b)/e4phi
        end do
      end do

      bv   = [S%val(IBETAX), S%val(IBETAY), S%val(IBETAZ)]
      dphi = [S%d1x(IPHI), S%d1y(IPHI), S%d1z(IPHI)]

      ! d_dir gamma_ab = e^{4phi} [ 4 d_dir(phi) gtilde_ab + d_dir gtilde_ab ]
      do dir=1,3
        do a=1,3
          do b=1,3
            dgam(a,b,dir)=e4phi*( 4.0_dp*dphi(dir)*gt(a,b) + d1g(S,a,b,dir) )
          end do
        end do
      end do

      ! W(dir,m,a,b) = d_dir ( b^m gamma^m_j );  dir=0 holds the value itself
      do dir=0,3
        do m=1,3
          do a=1,3
            do b=1,3
              W(dir,m,a,b)=0.0_dp
              do c=1,3
                do d=1,3
                  if (dir==0) then
                    W(dir,m,a,b)=W(dir,m,a,b)+gti(m,c)*gt(c,d)*bv(d)*gt(d,b)
                  else
                    W(dir,m,a,b)=W(dir,m,a,b)+gti(m,c)*d1g(S,c,d,dir)*bv(d)*gt(d,b)
                  end if
                end do
              end do
              W(dir,m,a,b)=W(dir,m,a,b)*e4phi
              if (dir/=0) then
                ws=0.0_dp
                do c=1,3
                  ws=ws+gti(m,c)*gt(c,b)*bv(c)
                end do
                W(dir,m,a,b)=W(dir,m,a,b)+4.0_dp*e4phi*dphi(dir)*ws
              end if
            end do
          end do
        end do
      end do

      ! V^m = gamma^{mn} b_n ,  d_dir V^m
      do m=1,3
        V(m)=0.0_dp
        do a=1,3
          do b=1,3
            V(m)=V(m)+gami(m,a)*gam(a,b)*bv(b)
          end do
        end do
        do dir=1,3
          dV(m,dir) = -4.0_dp*dphi(dir)*V(m)
          do a=1,3
            do b=1,3
              dV(m,dir)=dV(m,dir)+gami(m,a)*( dgam(a,b,dir)*bv(b) &
                                           + gam(a,b)*d1bv(S,b,dir) )
            end do
          end do
        end do
      end do
      divb=0.0_dp
      do m=1,3
        divb=divb+dV(m,m)
      end do

      ! L_beta gamma_ab = beta^m d_m gamma_ab + gamma_bm d_a beta^m
      !                                     + gamma_am d_b beta^m
      ! (direct Lie-derivative form; the divergence form needs a metric-symmetry
      !  identity that does not hold, and silently gives the wrong answer.)
      do a=1,3
        do b=1,3
          Lbg=0.0_dp
          do m=1,3
            Lbg=Lbg+bv(m)*dgam(a,b,m)
          end do
          do m=1,3
            Lbg=Lbg+gam(b,m)*d1bv(S,m,a) + gam(a,m)*d1bv(S,m,b)
          end do
          Kgauge(a,b)=Lbg/(2.0_dp*S%val(IALPHA))
        end do
      end do

      ! stored K_ij = e^{4phi}( Atilde_ij + (1/3) gtilde_ij K )
      do a=1,3
        do b=1,3
          Kstored(a,b)=e4phi*(atld_c(S,a,b) + gt(a,b)*S%val(IK)/3.0_dp)
        end do
      end do

      ktrg=0.0_dp
      do a=1,3
        do b=1,3
          ktrg=ktrg+gami(a,b)*Kgauge(a,b)
        end do
      end do
      ktrs = S%val(IK)
      kex  = eq_mean_curvature(bh_mass,r)

      if (.false.) then


      end if

      if (abs(ktrg-ktrs) > trmax) then
        trmax = abs(ktrg-ktrs); wi=i; wk=k; wr=r
        wKg=Kgauge; wKs=Kstored; wgt=gami; wg=gam; wb=bv; wal=S%val(IALPHA)
      end if
      trrms = trrms + (ktrg-ktrs)**2
      kmax  = max(kmax,  abs(ktrg))
      kerr  = max(kerr,  abs(ktrg-kex))
      npt   = npt + 1.0_dp
    end do
  end do
  trrms = sqrt(trrms/max(npt,1.0_dp))

  write(*,'(A,I0)')       'cells tested: ', nint(npt)
  write(*,'(A,ES12.4)')  'max |K| in annulus:          ', kmax
  write(*,'(A,ES12.4)')  'max |K_gauge - K_stored|:   ', trmax
  write(*,'(A,ES12.4)')  'rms |K_gauge - K_stored|:   ', trrms
  write(*,'(A,ES12.4)')  'max |K_gauge - K_analytic|: ', kerr
  write(*,'(A,I0,A,I0,A,ES12.4)') 'worst cell i=',wi,' k=',wk,' r=',wr
  write(*,'(A,6ES12.3)') '  wKg row1: ',wKg(1,1),wKg(1,2),wKg(1,3)
  write(*,'(A,6ES12.3)') '  wKg row2: ',wKg(2,1),wKg(2,2),wKg(2,3)
  write(*,'(A,6ES12.3)') '  wKg row3: ',wKg(3,1),wKg(3,2),wKg(3,3)
  write(*,'(A,6ES12.3)') '  wKs row1: ',wKs(1,1),wKs(1,2),wKs(1,3)
  write(*,'(A,6ES12.3)') '  wKs row2: ',wKs(2,1),wKs(2,2),wKs(2,3)
  write(*,'(A,6ES12.3)') '  wKs row3: ',wKs(3,1),wKs(3,2),wKs(3,3)
  write(*,'(A,6ES12.3)') '  gami row1: ',wgt(1,1),wgt(1,2),wgt(1,3)
  write(*,'(A,6ES12.3)') '  gami row2: ',wgt(2,1),wgt(2,2),wgt(2,3)
  write(*,'(A,6ES12.3)') '  gami row3: ',wgt(3,1),wgt(3,2),wgt(3,3)

  if (npt < 100.0_dp) error stop 'annulus too small to be meaningful'
  tol = 5.0e-3_dp*max(kmax,1.0_dp)
  if (trmax > tol) then
     write(*,'(A)') 'KNOWN FAILURE: components with a y index disagree.'
     write(*,'(A)') 'The x and z components of K_gauge reproduce K_stored to 4-5'
     write(*,'(A)') 'significant figures; only the y-slot is wrong.  Forcing all'
     write(*,'(A)') 'y-derivatives to zero makes it worse, so d_y is genuinely'
     write(*,'(A)') 'required (gamma_xy ~ f^-1 x y / r^2 is odd in y).  The error'
     write(*,'(A)') 'rms does NOT fall under refinement (1.08e-2 at 48x96,'
     write(*,'(A)') '9.58e-3 at 96x192, 9.40e-3 at 192x384), so this is a defect in'
     write(*,'(A)') 'the cartoon y-derivative reconstruction, not truncation error.'
     write(*,'(A)') 'See LONG_TERM_GAUGE_DRIFT_REPORT.md section 6, theory D.'
     error stop 'K from the gauge: cartoon y-derivative does not converge'
  end if
  write(*,'(A)') 'PASS: K from (alpha,beta,gamma) reproduces the stored K.'

contains

  pure function gtvar(a,b) result(n)
    integer,intent(in) :: a,b
    integer :: n
    select case (3*a+b)
      case (4);  n=IGXX      ! (1,1)
      case (5);  n=IGXY      ! (1,2),(2,1)
      case (6);  n=IGXZ      ! (1,3),(3,1)
      case (8);  n=IGYY      ! (2,2)
      case (9);  n=IGYZ      ! (2,3),(3,2)
      case (12); n=IGZZ      ! (3,3)
      case default; n=IGXX
    end select
  end function gtvar

  ! d_dir gtilde_ab along cartesian direction dir (1=x,2=y,3=z)
  function d1g(St,a,b,dir) result(d)
    type(stencil_t),intent(in) :: St
    integer,intent(in) :: a,b,dir
    real(dp) :: d
    integer :: n
    if (dir==2 .and. no_yderivs) then
      d=0.0_dp
      return
    end if
    n=gtvar(a,b)
    select case (dir)
      case (1); d=St%d1x(n)
      case (2); d=St%d1y(n)
      case (3); d=St%d1z(n)
      case default; d=0.0_dp
    end select
  end function d1g

  function d1bv(St,c,dir) result(d)
    type(stencil_t),intent(in) :: St
    integer,intent(in) :: c,dir
    real(dp) :: d
    integer :: n
    if (dir==2 .and. no_yderivs) then
      d=0.0_dp
      return
    end if
    select case (c)
      case (1); n=IBETAX
      case (2); n=IBETAY
      case (3); n=IBETAZ
      case default; n=0
    end select
    select case (dir)
      case (1); d=St%d1x(n)
      case (2); d=St%d1y(n)
      case (3); d=St%d1z(n)
      case default; d=0.0_dp
    end select
  end function d1bv

  ! Atilde_ab component from the stencil
  function atld_c(St,a,b) result(v)
    type(stencil_t),intent(in) :: St
    integer,intent(in) :: a,b
    real(dp) :: v
    integer :: n
    select case (3*a+b)
      case (4);  n=IAXX
      case (5);  n=IAXY
      case (6);  n=IAXZ
      case (8);  n=IAYY
      case (9);  n=IAYZ
      case (12); n=IAZZ
      case default; n=IAXX
    end select
    v=St%val(n)
  end function atld_c

  end program test_k_from_gauge