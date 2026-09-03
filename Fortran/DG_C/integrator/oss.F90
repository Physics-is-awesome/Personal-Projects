!=======================================================================
!  mod_limiter : the OE technique of Sec. 3.6 and the PP limiter
!
!  ------------------------------------------------------------------
!  A) Oscillation eliminating damping, eq. (3.38)
!
!     d/dt~ int_K U_sig . v + sum_{m=0}^{k} theta^m_K(U_h)
!                             int_K (U_sig - P^{m-1} U_sig) . v = 0
!
!  whose exact solution at t~ = dt is the modal damping
!
!     F_dt U_h = U^(0)_K phi^(0)_K
!              + sum_{j=1}^{k} exp( -dt sum_{m=0}^{j} theta^m_K )
!                sum_{|alpha|=j} U^(alpha)_K phi^(alpha)_K
!
!  with
!     theta^m_K(U_h) = sum_{e in dK} beta_e sigma^m_{e,K}(U_h) / h_{e,K}
!
!     sigma^m_{e,K}(u) = 0                        if u is constant, else
!
!         (2m+1) h^m_{e,K}              1
!        ---------------------  sum    ---  int_e | [[ d^alpha u ]]_e | dS
!          2 (2k-1) m!         |alpha|=m |e|
!        -------------------------------------------------------------
!                     || u - avg(u) ||_{L_inf(Omega)}
!
!     sigma^m_{e,K}(U_h) = max_{1<=i<=N} sigma^m_{e,K}(u^(i)_h)
!
!     beta_e = spectral radius of sum_i n^(i)_e dF_i/dU |_{u = ubar_K}
!            = |ubar_K . n_e| + c_K
!
!     h_{e,K} = sup_{x in K} dist(x,e) = h_m for a face normal to e_m.
!
!  IMPORTANT.  The inner sum runs over EVERY multi-index alpha of total
!  order m -- C(m+d-1,d-1) of them, not d.  For d = 2, k = 2 that is
!  {(2,0),(1,1),(0,2)} at m = 2; for d = 3, k = 2 it is six terms.  The
!  exact traces d^alpha b_a are precomputed in mod_basis (array dabf),
!  so sigma_m below is now a literal transcription of the formula rather
!  than an approximation.
!
!  ------------------------------------------------------------------
!  B) WB-compatible application, eq. (3.39):
!
!     W_h = W^e_h + F_dt( W_h~ - W^e_h )
!
!  so that at the discrete equilibrium the argument vanishes and the
!  damping is the identity, leaving Theorem 4.1 intact.  Note that
!  (3.39) evaluates theta^m_K at the UNDAMPED stage value W~_h, and the
!  exponential multiplies only the perturbation modes with |alpha| >= 1;
!  the perturbation cell average W^{delta,(0)}_K is left untouched, so
!  the OE step is conservative.
!
!  ------------------------------------------------------------------
!  C) Positivity-preserving limiter [36,60,61]
!
!     u_h <- ubar + theta (u_h - ubar),  theta in [0,1] maximal subject
!     to rho_h >= eps and p_h >= eps at every quadrature point.
!  The PP limiter acts on U (not on W), i.e. after E_tot has been
!  converted back to E.
!=======================================================================
module mod_limiter
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics
  use mod_riemann, only : spectral_radius_n
  implicit none
  private
  public :: oe_damping, pp_limiter

contains

  !=====================================================================
  !  F_dt applied to the perturbation  Y - Yref     (eq. (3.39))
  !=====================================================================
  subroutine oe_damping(Y, Yref, dt)
    real(dp), intent(inout) :: Y(nb,nvar,ncell)
    real(dp), intent(in)    :: Yref(nb,nvar,ncell)
    real(dp), intent(in)    :: dt
    real(dp), allocatable :: D(:,:,:), Dnew(:,:,:), uinf(:)
    real(dp) :: theta(0:kdeg), cum, damp
    real(dp) :: ubar(nvar), nvecm(3), beta, hek, sg
    integer  :: ic, jc, a, m, s, q, iv, mm, j

    if (.not. use_oe) return

    allocate(D(nb,nvar,ncell), Dnew(nb,nvar,ncell), uinf(nvar))

    !  The OE technique of [50] damps the solution itself; the WB variant
    !  (3.39) damps the perturbation W^delta = W~ - W^e.
    D    = Y - Yref
    Dnew = D

    !--- || u - avg(u) ||_{Linf(Omega)} , the global normalisation ------
    !    avg is the cell average, i.e. the first modal coefficient.
    uinf = 1.0e-30_dp
    do ic = 1, ncell
       do q = 1, nqv
          do iv = 1, nvar
             uinf(iv) = max(uinf(iv), abs(eval_v(D(:,iv,ic),q) - D(1,iv,ic)))
          end do
       end do
    end do

    do ic = 1, ncell

       ubar  = Y(1,:,ic)
       theta = zero

       do m = 1, nd
          hek = h_eK(m)                       ! h_{e,K} = h_m
          do s = 1, 2
             jc = nbr(s,m,ic)
             if (jc <= 0) cycle                ! no jump on a physical face
             nvecm    = zero
             nvecm(m) = merge(-one, one, s == 1)
             if (ubar(IRHO) > zero) then
                beta = spectral_radius_n(ubar, nvecm)
             else
                beta = zero
             end if
             beta = oe_beta*beta

             do mm = 0, kdeg
                sg = sigma_m(ic, jc, s, m, mm, D, uinf, hek)
                theta(mm) = theta(mm) + beta*sg/hek
             end do
          end do
       end do

       !--- damp every mode of total degree j >= 1 ----------------------
       cum = theta(0)
       do j = 1, kdeg
          cum  = cum + theta(j)
          damp = exp(-dt*cum)
          do a = 1, nb
             if (idsum(a) /= j) cycle
             do iv = 1, nvar
                Dnew(a,iv,ic) = D(a,iv,ic)*damp
             end do
          end do
       end do
    end do

    Y = Yref + Dnew
    deallocate(D, Dnew, uinf)
  end subroutine oe_damping

  !---------------------------------------------------------------------
  !  sigma^m_{e,K} for one face, maximised over the N solution
  !  components, summing over ALL multi-indices with |alpha| = mm.
  !
  !  Face weights wf sum to 1, so  sum_mu wf_mu |.|  is exactly
  !  (1/|e|) int_e |.| dS  as the formula requires.
  !---------------------------------------------------------------------
  real(dp) function sigma_m(ic, jc, s, m, mm, D, uinf, hek) result(sg)
    integer,  intent(in) :: ic, jc, s, m, mm
    real(dp), intent(in) :: D(nb,nvar,ncell)
    real(dp), intent(in) :: uinf(nvar)
    real(dp), intent(in) :: hek
    integer  :: iv, q, ii
    real(dp) :: acc, din, dout, fac

    sg = zero

    ! (2m+1) h_{e,K}^m / ( 2 (2k-1) m! )
    fac = real(2*mm+1,dp)*hek**mm                                        &
          / (two*real(2*kdeg-1,dp)*factorial(mm))

    do iv = 1, nvar
       acc = zero
       do ii = 1, nmi
          if (misum(ii) /= mm) cycle          ! keep only |alpha| = mm
          do q = 1, nqf
             din  = eval_dab(D(:,iv,ic), ii, q, s,   m)
             dout = eval_dab(D(:,iv,jc), ii, q, 3-s, m)
             acc  = acc + wf(q)*abs(din - dout)
          end do
       end do
       sg = max(sg, fac*acc/uinf(iv))
    end do
  end function sigma_m

  pure real(dp) function factorial(n) result(f)
    integer, intent(in) :: n
    integer :: i
    f = one
    do i = 2, n
       f = f*real(i,dp)
    end do
  end function factorial

  !=====================================================================
  !  scaling limiter of [60,61] on density and pressure
  !=====================================================================
  subroutine pp_limiter(U)
    real(dp), intent(inout) :: U(nb,nvar,ncell)
    integer  :: ic, a, m, s, q, iv, np, ip
    real(dp) :: ubar(nvar), upoint(nvar), theta, th, rmin
    real(dp), allocatable :: upt(:,:)

    if (.not. use_pp) return

    np = nqv + 2*nd*nqf
    allocate(upt(nvar,np))

    do ic = 1, ncell
       ubar = U(1,:,ic)
       if (ubar(IRHO) <= pp_eps) cycle

       !--- gather every volume and face quadrature point --------------
       ip = 0
       do q = 1, nqv
          ip = ip + 1
          do iv = 1, nvar
             upt(iv,ip) = eval_v(U(:,iv,ic), q)
          end do
       end do
       do m = 1, nd
          do s = 1, 2
             do q = 1, nqf
                ip = ip + 1
                do iv = 1, nvar
                   upt(iv,ip) = eval_f(U(:,iv,ic), q, s, m)
                end do
             end do
          end do
       end do

       !--- density ---------------------------------------------------
       rmin = minval(upt(IRHO,1:np))
       if (rmin < pp_eps) then
          theta = (ubar(IRHO) - pp_eps)/(ubar(IRHO) - rmin)
          theta = max(zero, min(one, theta))
          do a = 2, nb
             U(a,IRHO,ic) = theta*U(a,IRHO,ic)
          end do
          do ip = 1, np
             upt(IRHO,ip) = ubar(IRHO) + theta*(upt(IRHO,ip) - ubar(IRHO))
          end do
       end if

       !--- pressure : bisection on the scaling factor -----------------
       theta = one
       do ip = 1, np
          upoint = upt(:,ip)
          if (upoint(IRHO) <= zero) then
             theta = zero
             exit
          end if
          if (pressure(upoint) < pp_eps) then
             th = bisect_theta(ubar, upoint)
             theta = min(theta, th)
          end if
       end do

       if (theta < one) then
          do a = 2, nb
             do iv = 1, nvar
                U(a,iv,ic) = theta*U(a,iv,ic)
             end do
          end do
       end if
    end do

    deallocate(upt)
  end subroutine pp_limiter

  !---------------------------------------------------------------------
  !  largest theta in [0,1] with p( ubar + theta (u - ubar) ) >= pp_eps
  !---------------------------------------------------------------------
  pure real(dp) function bisect_theta(ubar, u) result(th)
    real(dp), intent(in) :: ubar(nvar), u(nvar)
    real(dp) :: lo, hi, md, uu(nvar)
    integer  :: it
    lo = zero ; hi = one
    do it = 1, 60
       md = half*(lo + hi)
       uu = ubar + md*(u - ubar)
       if (uu(IRHO) > zero .and. pressure(uu) >= pp_eps) then
          lo = md
       else
          hi = md
       end if
    end do
    th = lo
  end function bisect_theta

end module mod_limiter
