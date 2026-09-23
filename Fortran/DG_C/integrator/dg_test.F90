!=======================================================================
!  mod_dg : the two spatial operators of Sec. 3.5
!
!     standard scheme          U_t = L(U_h, t)          (3.11)
!     structure-preserving     W_t = H(W_h, t)          (3.36)
!
!  ---------------------------------------------------------------
!  STANDARD  (3.11a)-(3.11c), U = (rho, rho u, E):
!
!    |K| dc_a/dt = int_K F(U_h).grad b_a - sum_E int_E Fhat b_a
!                  + int_K S(U_h, g_h) b_a
!
!  with Fhat the plain HLLC flux (3.4) and S = (0, -rho g, -rho u.g)^T.
!  The single Poisson solve is  (g_h, phi_h) = D_1(4 pi G rho_h).
!
!  ---------------------------------------------------------------
!  STRUCTURE PRESERVING, W = (rho, rho u, E_tot), Sec. 3.4.4:
!
!  Step 1.  (g^delta, phi^delta) = D_1(4 pi G (rho_h - rho^e_h))    (3.30)
!           (g_h, phi_h) = (g^e_h, phi^e_h) + (g^delta, phi^delta)  (3.29)
!           E_h = (E_tot)_h - P( 1/2 rho_h phi_h )                  (3.32)
!  Step 2.  modified HLLC flux (3.33) -> fhat^[1], fhat^[2], fhat^[3]
!  Step 3.  (gdot, phidot) = D_2~( 4 pi G div(rho u)_h )            (3.31)
!           F_g   = 1/(8 pi G)(phi_h gdot - phidot g_h) + (rho u)_h phi_h  (3.34)
!           Fghat = 1/(8 pi G)(phihat gdothat - phidothat ghat).n
!                   + fhat^[1] phihat                               (3.35)
!  Step 4.  evolve (3.36a)-(3.36c) with the WB source (3.23).
!
!  Note the ORDER: the momentum source (3.23) needs the modified HLLC
!  flux only through p^e, but the energy flux needs fhat^[1], so the
!  HLLC pass is completed and stored before the energy flux is formed,
!  exactly as stated after (3.25).
!=======================================================================
module mod_dg
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics
  use mod_riemann
  use mod_poisson
  implicit none
  private
  public :: dg_init, dg_destroy, dg_equilibrium, dg_residual,            &
            dg_max_speed, dg_project_ic, dg_W_from_U, dg_U_from_W,       &
            dg_recover_U, rhoe, pe, phie, ge, Weq, phi_h, g_h

  !--- frozen discrete equilibrium (initialised once, never updated) ---
  real(dp), allocatable :: rhoe(:,:)      ! (nb,ncell)  rho^e_h = P rho^e
  real(dp), allocatable :: pe(:,:)        ! (nb,ncell)  p^e_h   = P p^e
  real(dp), allocatable :: phie(:,:)      ! (nb,ncell)  phi^e_h from D_1
  real(dp), allocatable :: ge(:,:,:)      ! (nb,3,ncell) g^e_h
  real(dp), allocatable :: Weq(:,:,:)     ! (nb,nvar,ncell) equilibrium W

  !--- last computed fields (diagnostics / output) ---------------------
  real(dp), allocatable :: phi_h(:,:)     ! (nb,ncell)
  real(dp), allocatable :: g_h(:,:,:)     ! (nb,3,ncell)

  !--- scratch ---------------------------------------------------------
  real(dp), allocatable :: phid(:,:), gd(:,:,:)
  real(dp), allocatable :: phidot(:,:), gdot(:,:,:)
  real(dp), allocatable :: Ework(:,:), momc(:,:,:)
  real(dp), allocatable :: fh1(:,:,:,:), fh3(:,:,:,:)
  !  fhmom/pew cache the momentum-flux components and the p^e trace that
  !  Step 2 of dg_residual computes; Step 4 used to recompute the exact
  !  same Riemann solve a second time for the same (ic,m,s,q) -- reusing
  !  these instead removes that duplicated work entirely (see dg_residual).
  real(dp), allocatable :: fhmom(:,:,:,:,:), pew(:,:,:,:)
  !  Uwrk/rcwrk/rdwrk replace what used to be automatic arrays declared
  !  inside dg_residual (real(dp) :: U(nb,nvar,ncell), and the rc/rdel
  !  BLOCK-local arrays). Those were allocated and torn down on every
  !  single call to dg_residual (i.e. every RK stage) -- promoting them
  !  to persistent module scratch, sized once in dg_init, avoids that
  !  repeated allocation/deallocation churn (and the stack pressure of a
  !  full (nb,nvar,ncell) automatic array) on the hottest path in the code.
  real(dp), allocatable :: Uwrk(:,:,:)
  real(dp), allocatable :: rcwrk(:,:), rdwrk(:,:)

contains

  !---------------------------------------------------------------------
  subroutine dg_init()
    call dg_destroy()
    allocate(rhoe(nb,ncell), pe(nb,ncell), phie(nb,ncell), ge(nb,3,ncell))
    allocate(Weq(nb,nvar,ncell))
    allocate(phi_h(nb,ncell), g_h(nb,3,ncell))
    allocate(phid(nb,ncell), gd(nb,3,ncell))
    allocate(phidot(nb,ncell), gdot(nb,3,ncell))
    allocate(Ework(nb,ncell), momc(nb,3,ncell))
    allocate(fh1(nqf,2,3,ncell), fh3(nqf,2,3,ncell))
    allocate(fhmom(nd,nqf,2,3,ncell), pew(nqf,2,3,ncell))
    allocate(Uwrk(nb,nvar,ncell))
    allocate(rcwrk(nb,ncell), rdwrk(nb,ncell))
    rhoe = zero ; pe = zero ; phie = zero ; ge = zero ; Weq = zero
    phi_h = zero ; g_h = zero
    phid = zero ; gd = zero ; phidot = zero ; gdot = zero
  end subroutine dg_init

  subroutine dg_destroy()
    if (allocated(rhoe))   deallocate(rhoe, pe, phie, ge)
    if (allocated(Weq))    deallocate(Weq)
    if (allocated(phi_h))  deallocate(phi_h, g_h)
    if (allocated(phid))   deallocate(phid, gd)
    if (allocated(phidot)) deallocate(phidot, gdot)
    if (allocated(Ework))  deallocate(Ework, momc)
    if (allocated(fh1))    deallocate(fh1, fh3)
    if (allocated(fhmom))  deallocate(fhmom, pew)
    if (allocated(Uwrk))   deallocate(Uwrk)
    if (allocated(rcwrk))  deallocate(rcwrk, rdwrk)
  end subroutine dg_destroy

  !=====================================================================
  !  the discrete equilibrium:  rho^e_h = P rho^e ,  p^e_h = P p^e ,
  !  (g^e_h, phi^e_h) = D_1(4 pi G rho^e_h)                       Sec. 3.4
  !=====================================================================
  subroutine dg_equilibrium()
    integer  :: ic, q, its, bcm
    real(dp) :: x(3), rho, p, phi, res
    real(dp) :: fr(nqv), fp(nqv), vel(3), u(nvar)

    !$omp parallel do default(shared) private(ic,q,x,rho,p,phi,fr,fp) &
    !$omp   schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          call ref2phys(ic, xv(:,q), x)
          call steady_state(x, rho, p, phi)
          fr(q) = rho
          fp(q) = p
       end do
       call project_nodal(fr, rhoe(:,ic))
       call project_nodal(fp, pe(:,ic))
    end do
    !$omp end parallel do

    bcm = merge(BCP_ZERO, BCP_EQ, is_periodic)
    phie = zero
    if (ieq == EQ_CONST) then
       ! Jeans: Delta phi = 4 pi G (rho - rho0), so phi^e = 0 exactly
       phie = zero
       ge   = zero
    else
       call solve_rho(rhoe, bcm, phie, ge, its, res)
    end if

    ! W^e = ( rho^e, 0, p^e/(gam-1) + P(1/2 rho^e phi^e) )
    vel = zero
    !$omp parallel do default(shared) private(ic) schedule(static)
    do ic = 1, ncell
       Weq(:,:,ic) = zero
       Weq(:,IRHO,ic) = rhoe(:,ic)
       Weq(:,IEN,ic)  = pe(:,ic)/(gam - one)
    end do
    !$omp end parallel do
    call add_half_rho_phi(rhoe, phie, Weq)
  end subroutine dg_equilibrium

  !  W(:,IEN,:) += P( 1/2 rho phi )
  subroutine add_half_rho_phi(rc, pc, W)
    real(dp), intent(in)    :: rc(nb,ncell), pc(nb,ncell)
    real(dp), intent(inout) :: W(nb,nvar,ncell)
    real(dp) :: c(nb), fq(nqv)
    integer  :: ic, q
    !$omp parallel do default(shared) private(ic,q,fq,c) schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          fq(q) = half*eval_v(rc(:,ic),q)*eval_v(pc(:,ic),q)
       end do
       call project_nodal(fq, c)
       W(:,IEN,ic) = W(:,IEN,ic) + c
    end do
    !$omp end parallel do
  end subroutine add_half_rho_phi

  !=====================================================================
  !  initial data:  U^0_h = P U^0 (3.27);  for the SP scheme also
  !  (E_tot)^0_h = E^0_h + P(1/2 rho^0_h phi^0_h)  (3.28)
  !=====================================================================
  subroutine dg_project_ic(Y)
    real(dp), intent(out) :: Y(nb,nvar,ncell)
    integer  :: ic, q, iv, its, bcm
    real(dp) :: x(3), u(nvar), res
    real(dp) :: fq(nqv,nvar)
    real(dp), allocatable :: rhoc(:,:), pht(:,:), gt(:,:,:), rdel(:,:)

    !$omp parallel do default(shared) private(ic,q,iv,x,u,fq) schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          call ref2phys(ic, xv(:,q), x)
          call init_state(x, u)
          fq(q,:) = u
       end do
       do iv = 1, nvar
          call project_nodal(fq(:,iv), Y(:,iv,ic))
       end do
    end do
    !$omp end parallel do

    if (trim(scheme) /= 'sp') return

    allocate(rhoc(nb,ncell), pht(nb,ncell), gt(nb,3,ncell), rdel(nb,ncell))
    rhoc = Y(:,IRHO,:)
    rdel = rhoc - rhoe
    bcm  = BCP_ZERO
    pht  = zero
    call solve_rho(rdel, bcm, pht, gt, its, res)
    pht = pht + phie
    call add_half_rho_phi(rhoc, pht, Y)
    deallocate(rhoc, pht, gt, rdel)
  end subroutine dg_project_ic

  !---------------------------------------------------------------------
  !  U from W (Step 1) and W from U, both using the projector P
  !---------------------------------------------------------------------
  subroutine dg_W_from_U(U, rc, pc, W)
    real(dp), intent(in)  :: U(nb,nvar,ncell)
    real(dp), intent(in)  :: rc(nb,ncell), pc(nb,ncell)
    real(dp), intent(out) :: W(nb,nvar,ncell)
    W = U
    call add_half_rho_phi(rc, pc, W)
  end subroutine dg_W_from_U

  subroutine dg_U_from_W(W, rc, pc, U)
    real(dp), intent(in)  :: W(nb,nvar,ncell)
    real(dp), intent(in)  :: rc(nb,ncell), pc(nb,ncell)
    real(dp), intent(out) :: U(nb,nvar,ncell)
    real(dp) :: c(nb), fq(nqv)
    integer  :: ic, q
    U = W
    !$omp parallel do default(shared) private(ic,q,fq,c) schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          fq(q) = half*eval_v(rc(:,ic),q)*eval_v(pc(:,ic),q)
       end do
       call project_nodal(fq, c)
       U(:,IEN,ic) = W(:,IEN,ic) - c
    end do
    !$omp end parallel do
  end subroutine dg_U_from_W

  !---------------------------------------------------------------------
  !  public helper: given the evolved Y, return U and phi_h
  !---------------------------------------------------------------------
  subroutine dg_recover_U(Y, U)
    real(dp), intent(in)  :: Y(nb,nvar,ncell)
    real(dp), intent(out) :: U(nb,nvar,ncell)
    integer  :: its
    real(dp) :: res
    real(dp), allocatable :: rdel(:,:)
    if (trim(scheme) /= 'sp') then
       U = Y
       allocate(rdel(nb,ncell))
       rdel = Y(:,IRHO,:)
       call solve_rho(rdel, merge(BCP_ZERO,BCP_EQ,is_periodic), phi_h, g_h, its, res)
       deallocate(rdel)
       return
    end if
    allocate(rdel(nb,ncell))
    rdel = Y(:,IRHO,:) - rhoe
    ! Retain the latest potential as the PCG initial iterate; this is mathematically identical after convergence.
    ! A CFL step changes the state only slightly, so it avoids cold-start iterations.
    call solve_rho(rdel, BCP_ZERO, phid, gd, its, res)
    phi_h = phie + phid
    g_h   = ge   + gd
    call dg_U_from_W(Y, Y(:,IRHO,:), phi_h, U)
    deallocate(rdel)
  end subroutine dg_recover_U

  !=====================================================================
  !  CFL speed:  max_K ( ||ubar_K||_inf + cbar_K )        (Sec. 5)
  !=====================================================================
  real(dp) function dg_max_speed(U) result(alp)
    real(dp), intent(in) :: U(nb,nvar,ncell)
    integer  :: ic, m
    real(dp) :: ub(nvar), rho, vel(3), p, c, vmax
    alp = zero
    !$omp parallel do default(shared) private(ic,m,ub,rho,vel,p,c,vmax) &
    !$omp   reduction(max:alp) schedule(static)
    do ic = 1, ncell
       ub = U(1,:,ic)                      ! cell averages (b_1 = 1)
       if (ub(IRHO) <= zero) cycle
       call cons2prim(ub, rho, vel, p)
       if (p <= zero) cycle
       c = sqrt(gam*p/rho)
       vmax = zero
       do m = 1, nd
          vmax = max(vmax, abs(vel(m)))
       end do
       alp = max(alp, vmax + c)
    end do
    !$omp end parallel do
  end function dg_max_speed

  !=====================================================================
  !                        THE SPATIAL OPERATOR
  !=====================================================================
  subroutine dg_residual(Y, t, res)
    real(dp), intent(in)  :: Y(nb,nvar,ncell)
    real(dp), intent(in)  :: t
    real(dp), intent(out) :: res(nb,nvar,ncell)

    integer  :: ic, jc, a, m, s, q, iv, its, bcm
    real(dp) :: uq(nvar), fq(nvar), x(3)
    real(dp) :: ul(nvar), ur(nvar), fh(nvar)
    real(dp) :: rho, vel(3), p, gq(3), sgn, area, w
    real(dp) :: pel, per, resid
    real(dp) :: rbar, rebar, rq, req, dpe(3), Fg(3), fgh
    real(dp) :: phq, pdq, gqv(3), gdv(3), momq(3)
    real(dp) :: pin, pout, phat, pdin, pdout, pdhat
    real(dp) :: gin, gout, ghn, gdin, gdout, gdhn
    real(dp) :: c8

    c8 = one/(8.0_dp*pi*Ggrav)
    res = zero

    !==================================================================
    !  STANDARD SCHEME  (3.11)
    !==================================================================
    if (trim(scheme) /= 'sp') then

       bcm = merge(BCP_ZERO, BCP_EQ, is_periodic)
       rcwrk = Y(:,IRHO,:)
       if (ieq == EQ_CONST) rcwrk = rcwrk - rhoe   ! Jeans: source is rho-rho0
       ! phi_h/g_h are NOT reset to zero before this solve: they still hold
       ! the converged values from the previous RK stage/timestep, which
       ! solve_rho can use as its initial iterate (warm start) instead of
       ! restarting the elliptic solve from scratch every call. Confirm
       ! solve_rho actually treats the incoming phi_h/g_h as an initial
       ! guess (typical for CG/multigrid) -- if so, this alone should cut
       ! Poisson iteration counts substantially, since phi barely changes
       ! between sub-stages of one RK step.
       call solve_rho(rcwrk, bcm, phi_h, g_h, its, resid)
       poisson_iters = its ; poisson_resid = resid

       ! volume + face terms, fused into a single pass over cells.
       ! res(:,:,ic) is written only for that same ic in every branch
       ! below, so different ic's never touch the same memory: the loop
       ! is embarrassingly parallel with no write races.
       !$omp parallel do default(shared) schedule(static) &
       !$omp   private(ic,q,iv,m,a,s,jc,uq,fq,gq,rho,w,sgn,area,ul,ur,fh)
       do ic = 1, ncell
          do q = 1, nqv
             do iv = 1, nvar
                uq(iv) = eval_v(Y(:,iv,ic), q)
             end do
             do m = 1, nd
                call phys_flux(uq, m, fq)
                w = wv(q)
                do iv = 1, nvar
                   do a = 1, nb
                      res(a,iv,ic) = res(a,iv,ic) + w*fq(iv)*dbv(a,m,q)
                   end do
                end do
             end do
             ! source  S = (0, -rho g, -rho u . g)
             do m = 1, nd
                gq(m) = eval_v(g_h(:,m,ic), q)
             end do
             rho = uq(IRHO)
             do m = 1, nd
                do a = 1, nb
                   res(a,IMX+m-1,ic) = res(a,IMX+m-1,ic)                 &
                        - wv(q)*rho*gq(m)*bv(a,q)
                end do
             end do
             w = zero
             do m = 1, nd
                w = w + uq(IMX+m-1)*gq(m)
             end do
             do a = 1, nb
                res(a,IEN,ic) = res(a,IEN,ic) - wv(q)*w*bv(a,q)
             end do
          end do

          do m = 1, nd
             area = dxi(m)
             do s = 1, 2
                sgn = merge(-one, one, s == 1)
                jc  = nbr(s,m,ic)
                do q = 1, nqf
                   do iv = 1, nvar
                      ul(iv) = eval_f(Y(:,iv,ic), q, s, m)
                   end do
                   call outer_state(ic, jc, q, s, m, Y, ul, ur)
                   call hllc_flux(ul, ur, m, sgn, fh)
                   do iv = 1, nvar
                      do a = 1, nb
                         res(a,iv,ic) = res(a,iv,ic)                     &
                              - area*wf(q)*fh(iv)*bf(a,q,s,m)
                      end do
                   end do
                end do
             end do
          end do
       end do
       !$omp end parallel do
       return
    end if

    !==================================================================
    !  STRUCTURE-PRESERVING SCHEME (3.36)
    !==================================================================

    !--- Step 1 : Poisson for the perturbation, then E_h ---------------
    rdwrk = Y(:,IRHO,:) - rhoe
    ! phid/gd are NOT reset to zero here: they carry the previous stage's
    ! converged perturbation potential in as the initial guess for this
    ! solve (warm start) -- see the matching note in the standard-scheme
    ! branch above. Same caveat: confirm solve_rho actually uses the
    ! incoming array as an initial iterate.
    call solve_rho(rdwrk, BCP_ZERO, phid, gd, its, resid)
    poisson_iters = its ; poisson_resid = resid

    phi_h = phie + phid
    g_h   = ge   + gd
    call dg_U_from_W(Y, Y(:,IRHO,:), phi_h, Uwrk)   ! (3.32)

    momc = zero
    do m = 1, nd
       momc(:,m,:) = Uwrk(:,IMX+m-1,:)
    end do

    !--- Step 2 : modified HLLC flux (3.33), cached face by face -------
    !  Every component of fh (not just fh1/fh3) and pel are stored here
    !  and reused verbatim in Step 4 below. Step 4 used to redo this
    !  exact Riemann solve a second time for the same (ic,m,s,q); since
    !  U, pe, m, s, q, ic, jc are unchanged between the two steps, that
    !  second solve always reproduced the same fh -- pure duplicated
    !  work that this cache removes.
    fh1 = zero ; fh3 = zero ; fhmom = zero ; pew = zero
    !$omp parallel do default(shared) schedule(static) &
    !$omp   private(ic,m,s,q,iv,jc,sgn,ul,ur,fh,pel,per)
    do ic = 1, ncell
       do m = 1, nd
          do s = 1, 2
             sgn = merge(-one, one, s == 1)
             jc  = nbr(s,m,ic)
             do q = 1, nqf
                do iv = 1, nvar
                   ul(iv) = eval_f(Uwrk(:,iv,ic), q, s, m)
                end do
                call outer_state(ic, jc, q, s, m, Uwrk, ul, ur)
                pel = eval_f(pe(:,ic), q, s, m)
                if (jc > 0) then
                   per = eval_f(pe(:,jc), q, 3-s, m)
                else
                   per = pel
                end if
                call hllc_flux_wb(ul, ur, pel, per, m, sgn, fh)
                fh1(q,s,m,ic)     = fh(IRHO)
                fh3(q,s,m,ic)     = fh(IEN)
                fhmom(:,q,s,m,ic) = fh(IMX:IMX+nd-1)
                pew(q,s,m,ic)     = pel
             end do
          end do
       end do
    end do
    !$omp end parallel do

    !--- Step 3 : D_2~ for (gdot, phidot) then the energy flux ---------
    ! phidot/gdot likewise keep the previous stage's converged values as
    ! the initial guess here instead of being reset to zero -- same
    ! warm-start reasoning and caveat as Step 1.
    call solve_dot(momc, fh1, phidot, gdot, its, resid)

    !--- Step 4 : assemble -----------------------------------------------
    ! res(:,:,ic) is written only for that same ic throughout this loop,
    ! so different ic's never touch the same memory: embarrassingly
    ! parallel, same as the standard-scheme loop above.
    !$omp parallel do default(shared) schedule(static) private(            &
    !$omp   ic,q,iv,m,a,s,jc,uq,fq,fh,pel,sgn,area,w,rbar,rebar,            &
    !$omp   rq,req,dpe,Fg,fgh,phq,pdq,gqv,gdv,momq,phat,pdhat,ghn,gdhn)
    do ic = 1, ncell

       rbar  = Uwrk(1,IRHO,ic)          ! (rho_h)_K  cell average
       rebar = rhoe(1,ic)               ! (rho^e_h)_K

       !------------- volume: F(W).grad v  and  F_g.grad v -------------
       do q = 1, nqv
          do iv = 1, nvar
             uq(iv) = eval_v(Uwrk(:,iv,ic), q)
          end do
          do m = 1, nd
             call phys_flux(uq, m, fq)
             do iv = 1, nvar
                do a = 1, nb
                   res(a,iv,ic) = res(a,iv,ic) + wv(q)*fq(iv)*dbv(a,m,q)
                end do
             end do
          end do

          ! F_g = 1/(8 pi G)(phi gdot - phidot g) + (rho u) phi     (3.34)
          phq = eval_v(phi_h(:,ic),  q)
          pdq = eval_v(phidot(:,ic), q)
          do m = 1, nd
             gqv(m)  = eval_v(g_h(:,m,ic),  q)
             gdv(m)  = eval_v(gdot(:,m,ic), q)
             momq(m) = uq(IMX+m-1)
             Fg(m)   = c8*(phq*gdv(m) - pdq*gqv(m)) + momq(m)*phq
          end do
          do m = 1, nd
             do a = 1, nb
                res(a,IEN,ic) = res(a,IEN,ic) + wv(q)*Fg(m)*dbv(a,m,q)
             end do
          end do

          !------------- WB source (3.23), first and third groups ------
          rq  = uq(IRHO)
          req = eval_v(rhoe(:,ic), q)
          do m = 1, nd
             dpe(m) = eval_dv(pe(:,ic), q, m)
             gdv(m) = eval_v(gd(:,m,ic), q)          ! g^delta only
          end do
          ! ( rho/rho^e - rhobar/rho^e_bar ) grad p^e
          w = rq/req - rbar/rebar
          do m = 1, nd
             do a = 1, nb
                res(a,IMX+m-1,ic) = res(a,IMX+m-1,ic)                    &
                     + wv(q)*w*dpe(m)*bv(a,q)
             end do
          end do
          ! - rho g^delta
          do m = 1, nd
             do a = 1, nb
                res(a,IMX+m-1,ic) = res(a,IMX+m-1,ic)                    &
                     - wv(q)*rq*gdv(m)*bv(a,q)
             end do
          end do
          ! - (rhobar/rho^e_bar) * int_K p^e grad v
          do m = 1, nd
             do a = 1, nb
                res(a,IMX+m-1,ic) = res(a,IMX+m-1,ic)                    &
                     - (rbar/rebar)*wv(q)*eval_v(pe(:,ic),q)*dbv(a,m,q)
             end do
          end do
       end do

       !------------- faces --------------------------------------------
       do m = 1, nd
          area = dxi(m)
          do s = 1, 2
             sgn = merge(-one, one, s == 1)
             jc  = nbr(s,m,ic)
             do q = 1, nqf

                ! (a) HLLC flux: reuse the value cached in Step 2 above --
                !     ul, ur, pel, per, m, sgn, jc are identical to Step 2
                !     for this (ic,m,s,q), so re-solving the same Riemann
                !     problem here would just reproduce the same fh.
                fh(IRHO)         = fh1(q,s,m,ic)
                fh(IMX:IMX+nd-1) = fhmom(:,q,s,m,ic)
                fh(IEN)          = fh3(q,s,m,ic)
                pel              = pew(q,s,m,ic)
                do iv = 1, nvar
                   do a = 1, nb
                      res(a,iv,ic) = res(a,iv,ic)                        &
                           - area*wf(q)*fh(iv)*bf(a,q,s,m)
                   end do
                end do

                ! (b) WB source, boundary group of (3.23):
                !     + (rhobar/rho^e_bar) sum_E int_E p^e v n
                do a = 1, nb
                   res(a,IMX+m-1,ic) = res(a,IMX+m-1,ic)                 &
                        + (rbar/rebar)*area*wf(q)*pel*bf(a,q,s,m)*sgn
                end do

                ! (c) energy flux (3.35)
                call trace_hats(ic, jc, q, s, m, sgn,                    &
                                phat, pdhat, ghn, gdhn)
                fgh = c8*(phat*gdhn - pdhat*ghn) + fh(IRHO)*phat
                do a = 1, nb
                   res(a,IEN,ic) = res(a,IEN,ic)                         &
                        - area*wf(q)*fgh*bf(a,q,s,m)
                end do
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine dg_residual

  !=====================================================================
  !  exterior trace, including the physical boundary conditions
  !=====================================================================
  subroutine outer_state(ic, jc, q, s, m, Y, ul, ur)
    integer,  intent(in)  :: ic, jc, q, s, m
    real(dp), intent(in)  :: Y(nb,nvar,ncell)
    real(dp), intent(in)  :: ul(nvar)
    real(dp), intent(out) :: ur(nvar)
    integer  :: iv
    real(dp) :: x(3), phi, t0
    if (jc > 0) then
       do iv = 1, nvar
          ur(iv) = eval_f(Y(:,iv,jc), q, 3-s, m)
       end do
    else
       select case (bc_fluid)
       case (BC_TRANS)
          ur = ul                                   ! zero gradient
       case (BC_EXACT)
          call ref2phys(ic, xf(:,q,s,m), x)
          call exact_state(x, zero, ur, phi)        ! stationary data
       case default
          ur = ul
       end select
    end if
  end subroutine outer_state

  !=====================================================================
  !  the single-valued LDG traces of (3.18) needed by (3.35):
  !     phihat = {{phi}} + C12.[[phi]]
  !     ghat.n = {{g}}.n - C11 [[phi]].n - C12 [[g]]   (per component)
  !  and the same for (phidot, gdot).
  !=====================================================================
  subroutine trace_hats(ic, jc, q, s, m, sgn, phat, pdhat, ghn, gdhn)
    integer,  intent(in)  :: ic, jc, q, s, m
    real(dp), intent(in)  :: sgn
    real(dp), intent(out) :: phat, pdhat, ghn, gdhn
    real(dp) :: pin, pout, gin, gout, x(3)

    ! ---- phi and g -------------------------------------------------
    pin = eval_f(phi_h(:,ic), q, s, m)
    gin = eval_f(g_h(:,m,ic), q, s, m)
    if (jc > 0) then
       pout = eval_f(phi_h(:,jc), q, 3-s, m)
       gout = eval_f(g_h(:,m,jc), q, 3-s, m)
       phat = half*(pin + pout) + sgn*C12(m)*(pin - pout)
       ghn  = sgn*half*(gin + gout) - C11*(pin - pout) - C12(m)*(gin - gout)
    else
       if (bc_pois == BC_EXACT) then
          call ref2phys(ic, xf(:,q,s,m), x)
          pout = phi_eq_bc(x)
          phat = pout
          ghn  = sgn*gin - C11*(pin - pout)
       else
          phat = pin
          ghn  = sgn*gin
       end if
    end if

    ! ---- phidot and gdot -------------------------------------------
    pin = eval_f(phidot(:,ic), q, s, m)
    gin = eval_f(gdot(:,m,ic), q, s, m)
    if (jc > 0) then
       pout = eval_f(phidot(:,jc), q, 3-s, m)
       gout = eval_f(gdot(:,m,jc), q, 3-s, m)
       pdhat = half*(pin + pout) + sgn*C12(m)*(pin - pout)
       gdhn  = sgn*half*(gin + gout) - C11*(pin - pout) - C12(m)*(gin - gout)
    else
       pdhat = zero                       ! compactly supported phidot
       gdhn  = sgn*gin - C11*pin
    end if
  end subroutine trace_hats

end module mod_dg
