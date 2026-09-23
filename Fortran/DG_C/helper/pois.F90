!=======================================================================
!  mod_poisson : the LDG solvers  D_1  and  D_2~  of Sec. 3.3 / 3.4.3
!
!  Mixed form (3.9b)-(3.9c):  for every K and every w in Sigma_h^k,
!  psi in V_h^k,
!
!    int_K g_h . w   = sum_E int_E phihat w^int . n_{E,K} - int_K phi_h div w
!    int_K g_h .grad psi = sum_E int_E psi^int ghat . n_{E,K}
!                          - int_K 4 pi G rho_h psi
!
!  with the alternating LDG fluxes (3.10)
!
!    ghat   = {{g_h}} - C11 [[phi_h]] - C12 [[g_h]]
!    phihat = {{phi_h}} + C12 . [[phi_h]]
!
!  and C11 = 1, C12 = (0.5,...,0.5)^T as fixed in Sec. 3.3.
!
!  Eliminating g_h is exactly the Schur complement of (3.12), i.e.
!  A = S + B^T K^{-1} B of (3.13).  Because the mass matrix is diagonal
!  in our orthonormal modal basis, K^{-1} is trivial and A is applied
!  matrix-free.  A is symmetric positive definite for Dirichlet data,
!  and symmetric positive semi-definite with the constant nullspace for
!  fully periodic data (3.14); in the latter case the mean is projected
!  out at every iteration.  We therefore use PCG throughout, which is
!  the alternative explicitly allowed in Remark 3.2.
!
!  Sign convention used here.  With exact traces the second equation
!  gives  L(phi) := int g.grad psi - sum_E int psi ghat.n  =  - int Lap(phi) psi,
!  so  A := L  discretises  -Laplacian  and the Poisson problem
!  Lap(phi) = 4 pi G rho  becomes
!
!         A phi = - 4 pi G rho_h .
!
!  For the time derivative, mass conservation gives (2.16)
!         Lap(phidot) = 4 pi G d(rho)/dt = - 4 pi G div(rho u),
!  hence          A phidot = + 4 pi G div(rho u).
!  Written weakly with the IBP of (3.24) this is
!         A phidot = 4 pi G [ sum_E int_E fhat^[1] psi - int_K (rho u).grad psi ].
!
!  NOTE.  Equations (3.17e) and (3.24) as printed in the paper carry the
!  opposite sign on this right hand side, which is inconsistent with
!  (2.16)/(3.15i).  We implement the consistent sign; set
!  literal_paper_sign = .true. to reproduce the printed formula.
!=======================================================================
module mod_poisson
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics, only : phi_eq_bc
  implicit none
  private
  ! First declare variables
  integer, parameter :: BCP_ZERO = 0
  integer, parameter :: BCP_EQ   = 1

  integer  :: poisson_iters = 0
  real(dp) :: poisson_resid = zero

   logical :: literal_paper_sign = .false.
  ! THEN declare what is public
  public :: poisson_init, poisson_destroy, ldg_grad, ldg_apply,          &
            solve_rho, solve_dot, poisson_iters, poisson_resid,          &
            BCP_ZERO, BCP_EQ, literal_paper_sign

  ! Other variables
  real(dp), allocatable :: rr(:,:), pp(:,:), ap(:,:), zz(:,:)
  real(dp), allocatable :: diagA(:,:)
  real(dp), allocatable :: gwrk(:,:,:)
  
  logical               :: inited = .false.
  
contains

  !---------------------------------------------------------------------
  subroutine poisson_init()
    call poisson_destroy()
    allocate(rr(nb,ncell), pp(nb,ncell), ap(nb,ncell), zz(nb,ncell))
    allocate(diagA(nb,ncell), gwrk(nb,3,ncell))
    call build_diag()
    inited = .true.
  end subroutine poisson_init

  subroutine poisson_destroy()
    if (allocated(rr))    deallocate(rr, pp, ap, zz)
    if (allocated(diagA)) deallocate(diagA)
    if (allocated(gwrk))  deallocate(gwrk)
    inited = .false.
  end subroutine poisson_destroy

  !=====================================================================
  !  g_h = grad_h(phi_h)  from (3.9b).  With the diagonal mass matrix,
  !
  !    g^a_m = - sum_q w_q phi_q (d b_a/dx_m)(x_q)
  !            + sum_{E in dK} (|E|/|K|) sum_mu w_mu phihat b_a n_m
  !=====================================================================
  subroutine ldg_grad(ph, gh, bcmode)
    real(dp), intent(in)  :: ph(nb,ncell)
    real(dp), intent(out) :: gh(nb,3,ncell)
    integer,  intent(in)  :: bcmode
    integer  :: ic, jc, a, m, s, q
    real(dp) :: pin, pout, phat, sgn, area, x(3)

    gh = zero

    !$omp parallel do default(shared) private(ic,q,m,a,pin) schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          pin = eval_v(ph(:,ic), q)
          do m = 1, nd
             do a = 1, nb
                gh(a,m,ic) = gh(a,m,ic) - wv(q)*pin*dbv(a,m,q)
             end do
          end do
       end do
    end do
    !$omp end parallel do

    !$omp parallel do default(shared) private(ic,m,area,s,sgn,jc,q,pin,pout,phat,a,x) schedule(static)
    do ic = 1, ncell
       do m = 1, nd
          area = dxi(m)                       ! |E|/|K| = 1/h_m
          do s = 1, 2
             sgn = merge(-one, one, s == 1)
             jc  = nbr(s,m,ic)
             do q = 1, nqf
                pin = eval_f(ph(:,ic), q, s, m)
                if (jc > 0) then
                   pout = eval_f(ph(:,jc), q, 3-s, m)
                   phat = half*(pin + pout) + sgn*C12(m)*(pin - pout)
                else
                   if (bcmode == BCP_EQ) then
                      call ref2phys(ic, xf(:,q,s,m), x)
                      phat = phi_eq_bc(x)
                   else
                      phat = zero
                   end if
                end if
                do a = 1, nb
                   gh(a,m,ic) = gh(a,m,ic) + area*wf(q)*phat*bf(a,q,s,m)*sgn
                end do
             end do
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ldg_grad

  !=====================================================================
  !  (A phi)_a = sum_q w_q g.(grad b_a) - sum_E (|E|/|K|) sum_mu w_mu ghat.n b_a
  !=====================================================================
  subroutine ldg_apply(ph, aph, bcmode)
    real(dp), intent(in)  :: ph(nb,ncell)
    real(dp), intent(out) :: aph(nb,ncell)
    integer,  intent(in)  :: bcmode
    integer  :: ic, jc, a, m, s, q
    real(dp) :: gin, gout, pin, pout, ghn, sgn, area, gq(3), x(3), gD

    call ldg_grad(ph, gwrk, bcmode)

    aph = zero

    !$omp parallel do default(shared) private(ic,q,m,a,gq) schedule(static)
    do ic = 1, ncell
       do q = 1, nqv
          do m = 1, nd
             gq(m) = eval_v(gwrk(:,m,ic), q)
          end do
          do a = 1, nb
             do m = 1, nd
                aph(a,ic) = aph(a,ic) + wv(q)*gq(m)*dbv(a,m,q)
             end do
          end do
       end do
    end do
    !$omp end parallel do

    !$omp parallel do default(shared) private(ic,m,area,s,sgn,jc,q,gin,gout,pin,pout,ghn,a,x,gD) schedule(static)
    do ic = 1, ncell
       do m = 1, nd
          area = dxi(m)
          do s = 1, 2
             sgn = merge(-one, one, s == 1)
             jc  = nbr(s,m,ic)
             do q = 1, nqf
                gin = eval_f(gwrk(:,m,ic), q, s, m)
                pin = eval_f(ph(:,ic),     q, s, m)
                if (jc > 0) then
                   gout = eval_f(gwrk(:,m,jc), q, 3-s, m)
                   pout = eval_f(ph(:,jc),     q, 3-s, m)
                   ! ghat.n = sgn {{g_m}} - C11 (phi^int-phi^ext) - C12_m (g^int_m-g^ext_m)
                   ghn = sgn*half*(gin + gout) - C11*(pin - pout)          &
                       - C12(m)*(gin - gout)
                else
                   gD = zero
                   if (bcmode == BCP_EQ) then
                      call ref2phys(ic, xf(:,q,s,m), x)
                      gD = phi_eq_bc(x)
                   end if
                   ghn = sgn*gin - C11*(pin - gD)
                end if
                do a = 1, nb
                   aph(a,ic) = aph(a,ic) - area*wf(q)*ghn*bf(a,q,s,m)
                end do
             end do
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ldg_apply

  !---------------------------------------------------------------------
  !  exact diagonal of A (homogeneous data) for Jacobi preconditioning
  !---------------------------------------------------------------------
  subroutine build_diag()
    real(dp), allocatable :: e(:,:), y(:,:)
    integer :: a, ic
    allocate(e(nb,ncell), y(nb,ncell))
    diagA = one
    do a = 1, nb
       e = zero
       e(a,:) = one
       call ldg_apply(e, y, BCP_ZERO)
       do ic = 1, ncell
          diagA(a,ic) = y(a,ic)
       end do
    end do
    where (abs(diagA) < 1.0e-300_dp) diagA = one
    deallocate(e, y)
  end subroutine build_diag

  !---------------------------------------------------------------------
  subroutine kill_mean(v)
    real(dp), intent(inout) :: v(nb,ncell)
    real(dp) :: s
    integer  :: ic
    if (.not. is_periodic) return
    s = zero
    do ic = 1, ncell
       s = s + v(1,ic)
    end do
    s = s/real(ncell,dp)
    do ic = 1, ncell
       v(1,ic) = v(1,ic) - s
    end do
  end subroutine kill_mean

  !---------------------------------------------------------------------
  !  preconditioned CG for  A phi = b  (homogeneous operator part)
  !---------------------------------------------------------------------
  subroutine pcg(b, ph, its, res)
    real(dp), intent(in)    :: b(nb,ncell)
    real(dp), intent(inout) :: ph(nb,ncell)
    integer,  intent(out)   :: its
    real(dp), intent(out)   :: res
    real(dp) :: rz, rzold, alp, bet, pap, bnorm
    integer  :: it

    call kill_mean(ph)
    call ldg_apply(ph, ap, BCP_ZERO)
    rr = b - ap
    call kill_mean(rr)
    bnorm = sqrt(sum(b*b))
    if (bnorm <= zero) bnorm = one

    zz = rr/diagA
    call kill_mean(zz)
    pp = zz
    rz = sum(rr*zz)

    its = 0
    res = sqrt(sum(rr*rr))/bnorm
    do it = 1, pois_maxit
       if (res <= pois_tol) exit
       call ldg_apply(pp, ap, BCP_ZERO)
       call kill_mean(ap)
       pap = sum(pp*ap)
       if (abs(pap) <= 1.0e-300_dp) exit
       alp = rz/pap
       ph  = ph + alp*pp
       rr  = rr - alp*ap
       call kill_mean(rr)
       zz  = rr/diagA
       call kill_mean(zz)
       rzold = rz
       rz = sum(rr*zz)
       bet = rz/rzold
       pp = zz + bet*pp
       its = it
       res = sqrt(sum(rr*rr))/bnorm
    end do
    call kill_mean(ph)
  end subroutine pcg

  !=====================================================================
  !  D_1 :  (g_h, phi_h) = D_1( 4 pi G rho_h )                    (3.19)
  !
  !  solves   A phi = -4 pi G rho_h  +  (Dirichlet lift)
  !=====================================================================
  subroutine solve_rho(rhoc, bcmode, ph, gh, its, res)
    real(dp), intent(in)    :: rhoc(nb,ncell)
    integer,  intent(in)    :: bcmode
    real(dp), intent(inout) :: ph(nb,ncell)
    real(dp), intent(out)   :: gh(nb,3,ncell)
    integer,  intent(out)   :: its
    real(dp), intent(out)   :: res
    real(dp), allocatable   :: b(:,:), z0(:,:), lift(:,:)

    allocate(b(nb,ncell), z0(nb,ncell), lift(nb,ncell))
    b = -four*pi*Ggrav*rhoc

    if (bcmode == BCP_EQ) then
       z0 = zero
       call ldg_apply(z0, lift, BCP_EQ)     ! A(0) with inhomogeneous data
       b = b - lift
    end if
    call kill_mean(b)

    call pcg(b, ph, its, res)
    call ldg_grad(ph, gh, bcmode)

    deallocate(b, z0, lift)
  end subroutine solve_rho

  !=====================================================================
  !  D_2~ :  (gdot_h, phidot_h) = D_2~( 4 pi G div(rho u)_h )      (3.25)
  !
  !  IBP form (3.24):
  !     A phidot = 4 pi G [ sum_E (|E|/|K|) sum_mu w_mu fhat^[1] b_a
  !                         - sum_q w_q (rho u).grad b_a ]
  !
  !  non-IBP form (3.17e), kept for the comparison of Table 5.2:
  !     A phidot = 4 pi G sum_q w_q ( div_h (rho u) )(x_q) b_a(x_q)
  !=====================================================================
  subroutine solve_dot(momc, fhat1, ph, gh, its, res)
    real(dp), intent(in)    :: momc(nb,3,ncell)          ! (rho u)_h modes
    real(dp), intent(in)    :: fhat1(nqf,2,3,ncell)      ! fhat^[1] . n_{E,K}
    real(dp), intent(inout) :: ph(nb,ncell)
    real(dp), intent(out)   :: gh(nb,3,ncell)
    integer,  intent(out)   :: its
    real(dp), intent(out)   :: res
    real(dp), allocatable   :: b(:,:)
    integer  :: ic, a, m, s, q
    real(dp) :: area, dv, c4

    allocate(b(nb,ncell))
    b  = zero
    c4 = four*pi*Ggrav
    if (literal_paper_sign) c4 = -c4

    if (use_ibp) then
       do ic = 1, ncell
          do q = 1, nqv
             do m = 1, nd
                dv = eval_v(momc(:,m,ic), q)
                do a = 1, nb
                   b(a,ic) = b(a,ic) - c4*wv(q)*dv*dbv(a,m,q)
                end do
             end do
          end do
          do m = 1, nd
             area = dxi(m)
             do s = 1, 2
                do q = 1, nqf
                   do a = 1, nb
                      b(a,ic) = b(a,ic) + c4*area*wf(q)*fhat1(q,s,m,ic)*bf(a,q,s,m)
                   end do
                end do
             end do
          end do
       end do
    else
       do ic = 1, ncell
          do q = 1, nqv
             dv = zero
             do m = 1, nd
                dv = dv + eval_dv(momc(:,m,ic), q, m)     ! strong divergence
             end do
             do a = 1, nb
                b(a,ic) = b(a,ic) + c4*wv(q)*dv*bv(a,q)
             end do
          end do
       end do
    end if

    call kill_mean(b)
    call pcg(b, ph, its, res)
    call ldg_grad(ph, gh, BCP_ZERO)

    deallocate(b)
  end subroutine solve_dot

end module mod_poisson
