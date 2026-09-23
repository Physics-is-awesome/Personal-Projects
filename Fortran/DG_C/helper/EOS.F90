!=======================================================================
!  mod_physics : thermodynamics, steady states and analytic data
!
!  U = (rho, rho u_1..rho u_d, E)^T,  E = p/(gamma-1) + 1/2 rho |u|^2 (1.2)
!  W = (rho, rho u, E_tot)^T,         E_tot = E + 1/2 rho phi        (3.15h)
!
!  Hydrostatic equilibrium (2.3):  u = 0,  grad p = -rho grad phi,
!  with the polytropic closure  p = kappa rho^nu ,  nu = (n+1)/n  (2.4),(2.6)
!  and                       phi^e = -(kappa nu/(nu-1)) (rho^e)^(nu-1)  (2.12)
!
!  Four equilibrium families are needed by Sec. 5:
!
!   EQ_PLANE  rho^e = lambda sin(k.x),  k = 1/a e_hat        (5.1),(5.6)
!             phi   = -4 pi G a^2 rho   (consistent: lap phi = 4 pi G rho)
!   EQ_BESSEL rho^e = lambda theta(r)^n, theta from (2.11)   (5.2)-(5.4)
!             theta(r) = (1/pi) int_0^pi cos(r sin alpha) dalpha  = J_0(r)
!   EQ_LANE1  rho^e = rho0 sin(alpha r)/(alpha r)            (5.7)-(5.9)
!             p^e = K (rho^e)^2 ,  phi^e = -2 K rho^e
!   EQ_CONST  rho^e = rho0, p^e = p0, phi^e = 0 (Jeans)      (5.4)
!=======================================================================
module mod_physics
  use mod_kinds
  use mod_params
  implicit none
  public

contains

  !====================== thermodynamics ===============================

  pure real(dp) function pressure(u) result(p)
    real(dp), intent(in) :: u(nvar)
    p = (gam - one)*(u(IEN) - half*sum(u(IMX:IMX+nd-1)**2)/u(IRHO))
  end function pressure

  pure real(dp) function soundspeed(u) result(c)
    real(dp), intent(in) :: u(nvar)
    c = sqrt(max(gam*pressure(u)/u(IRHO), zero))
  end function soundspeed

  pure subroutine cons2prim(u, rho, vel, p)
    real(dp), intent(in)  :: u(nvar)
    real(dp), intent(out) :: rho, vel(3), p
    integer :: m
    rho = u(IRHO)
    vel = zero
    do m = 1, nd
       vel(m) = u(IMX+m-1)/rho
    end do
    p = (gam - one)*(u(IEN) - half*rho*sum(vel(1:nd)**2))
  end subroutine cons2prim

  pure subroutine prim2cons(rho, vel, p, u)
    real(dp), intent(in)  :: rho, vel(3), p
    real(dp), intent(out) :: u(nvar)
    integer :: m
    u(IRHO) = rho
    do m = 1, nd
       u(IMX+m-1) = rho*vel(m)
    end do
    u(IEN) = p/(gam - one) + half*rho*sum(vel(1:nd)**2)
  end subroutine prim2cons

  !---------------------------------------------------------------------
  !  F_m(U) of (2.1) : the Euler flux in direction m
  !---------------------------------------------------------------------
  pure subroutine phys_flux(u, m, f)
    real(dp), intent(in)  :: u(nvar)
    integer,  intent(in)  :: m
    real(dp), intent(out) :: f(nvar)
    real(dp) :: rho, vel(3), p, vm
    integer  :: r
    call cons2prim(u, rho, vel, p)
    vm = vel(m)
    f(IRHO) = rho*vm
    do r = 1, nd
       f(IMX+r-1) = rho*vel(r)*vm
    end do
    f(IMX+m-1) = f(IMX+m-1) + p
    f(IEN)     = (u(IEN) + p)*vm
  end subroutine phys_flux

  !====================== equilibrium profiles =========================

  pure real(dp) function radius(x) result(r)
    real(dp), intent(in) :: x(3)
    r = sqrt(sum(x(1:nd)**2))
  end function radius

  !---------------------------------------------------------------------
  !  2D radial polytrope, eq. (2.11) with n = 1 :
  !     theta(r) = (1/pi) int_0^pi cos(r sin alpha) dalpha
  !  evaluated with a composite midpoint rule, which is spectrally
  !  accurate for this periodic integrand.
  !---------------------------------------------------------------------
  pure real(dp) function theta_bessel(r) result(th)
    real(dp), intent(in) :: r
    integer,  parameter  :: nq = 400
    real(dp) :: s, al
    integer  :: i
    s = zero
    do i = 1, nq
       al = pi*(real(i,dp) - half)/real(nq,dp)
       s  = s + cos(r*sin(al))
    end do
    th = s/real(nq,dp)
  end function theta_bessel

  !  d theta / dr  =  -(1/pi) int_0^pi sin(alpha) sin(r sin alpha) dalpha
  pure real(dp) function dtheta_bessel(r) result(dth)
    real(dp), intent(in) :: r
    integer,  parameter  :: nq = 400
    real(dp) :: s, al
    integer  :: i
    s = zero
    do i = 1, nq
       al = pi*(real(i,dp) - half)/real(nq,dp)
       s  = s - sin(al)*sin(r*sin(al))
    end do
    dth = s/real(nq,dp)
  end function dtheta_bessel

  !---------------------------------------------------------------------
  !  3D Lane-Emden n = 1 :  theta = sin(xi)/xi   (2.10)
  !---------------------------------------------------------------------
  pure real(dp) function sinc(z) result(f)
    real(dp), intent(in) :: z
    if (abs(z) < 1.0e-8_dp) then
       f = one - z*z/6.0_dp + z**4/120.0_dp
    else
       f = sin(z)/z
    end if
  end function sinc

  pure real(dp) function dsinc(z) result(f)     ! d/dz [ sin z / z ]
    real(dp), intent(in) :: z
    if (abs(z) < 1.0e-6_dp) then
       f = -z/3.0_dp + z**3/30.0_dp
    else
       f = cos(z)/z - sin(z)/(z*z)
    end if
  end function dsinc

  !---------------------------------------------------------------------
  !  the steady state  { rho^e, p^e, phi^e }  at a point
  !---------------------------------------------------------------------
  pure subroutine steady_state(x, rho, p, phi)
    real(dp), intent(in)  :: x(3)
    real(dp), intent(out) :: rho, p, phi
    real(dp) :: r, th, arg

    select case (ieq)

    case (EQ_PLANE)
       arg = sum(kvec(1:nd)*x(1:nd))
       rho = lam*sin(arg)
       p   = kap*rho**pnu
       phi = -four*pi*Ggrav*aLE*aLE*rho

    case (EQ_BESSEL)
       r   = radius(x)/aLE
       th  = theta_bessel(r)
       rho = lam*th**pol_n
       p   = kap*rho**pnu
       phi = -(kap*pnu/(pnu - one))*rho**(pnu - one)

    case (EQ_LANE1)
       r   = radius(x)
       rho = rho0*sinc(alph*r)
       p   = Kpol*rho*rho
       phi = -two*Kpol*rho

    case default                      ! EQ_CONST : Jeans swindle
       rho = rho0
       p   = p0_bg
       phi = zero
    end select
  end subroutine steady_state

  !---------------------------------------------------------------------
  !  grad phi^e  (needed only for diagnostics; the scheme uses g^e_h)
  !---------------------------------------------------------------------
  pure subroutine grad_phi_eq(x, g)
    real(dp), intent(in)  :: x(3)
    real(dp), intent(out) :: g(3)
    real(dp) :: r, c, drho
    integer  :: m
    g = zero
    select case (ieq)
    case (EQ_PLANE)
       c = -four*pi*Ggrav*aLE*aLE*lam*cos(sum(kvec(1:nd)*x(1:nd)))
       do m = 1, nd
          g(m) = c*kvec(m)
       end do
    case (EQ_BESSEL)
       r = radius(x)
       if (r < 1.0e-14_dp) return
       drho = lam*pol_n*theta_bessel(r/aLE)**(pol_n-one)*dtheta_bessel(r/aLE)/aLE
       c    = -kap*pnu*(lam*theta_bessel(r/aLE)**pol_n)**(pnu-two)*drho*(pnu-one) &
              /(pnu-one)
       c    = -kap*pnu*(lam*theta_bessel(r/aLE)**pol_n)**(pnu-two)*drho
       do m = 1, nd
          g(m) = c*x(m)/r
       end do
    case (EQ_LANE1)
       r = radius(x)
       if (r < 1.0e-14_dp) return
       c = -two*Kpol*rho0*dsinc(alph*r)*alph
       do m = 1, nd
          g(m) = c*x(m)/r
       end do
    end select
  end subroutine grad_phi_eq

  !====================== exact / initial data =========================

  !---------------------------------------------------------------------
  !  U_exact(x,t) for the accuracy tests, the steady state otherwise
  !---------------------------------------------------------------------
  subroutine exact_state(x, t, u, phi)
    real(dp), intent(in)  :: x(3), t
    real(dp), intent(out) :: u(nvar)
    real(dp), intent(out) :: phi
    real(dp) :: rho, p, vel(3), xx(3), arg
    integer  :: m

    vel = zero
    select case (icase)

    case (C_ACC2D, C_ACC3D)
       ! rho = sin( k . (x - u0 t) ), u = u0, p = kappa rho^2,
       ! phi = -4 pi G a^2 rho                             (5.1),(5.6)
       arg = zero
       do m = 1, nd
          arg = arg + kvec(m)*(x(m) - u_adv(m)*t)
       end do
       rho = lam*sin(arg)
       p   = kap*rho**pnu
       phi = -four*pi*Ggrav*aLE*aLE*rho
       vel(1:nd) = u_adv(1:nd)

    case default
       xx = x
       call steady_state(xx, rho, p, phi)
    end select

    call prim2cons(rho, vel, p, u)
  end subroutine exact_state

  !---------------------------------------------------------------------
  !  Dirichlet data for the Poisson solver on physical boundaries.
  !  For the accuracy tests this is the moving analytic potential; in
  !  every other case it is the (time independent) equilibrium value
  !  plus the perturbation, which is handled separately: the boundary
  !  datum for phi^delta is taken as zero (compact support), consistent
  !  with the "compactly supported" hypothesis of Theorem 4.2.
  !---------------------------------------------------------------------
  real(dp) function phi_eq_bc(x) result(phi)
    real(dp), intent(in) :: x(3)
    real(dp) :: rho, p
    call steady_state(x, rho, p, phi)
  end function phi_eq_bc

  !---------------------------------------------------------------------
  !  initial data of every example in Sec. 5
  !---------------------------------------------------------------------
  subroutine init_state(x, u)
    real(dp), intent(in)  :: x(3)
    real(dp), intent(out) :: u(nvar)
    real(dp) :: rho, p, phi, vel(3), r, r2, arg, dpsum
    integer  :: m, ib

    vel = zero
    call steady_state(x, rho, p, phi)

    select case (icase)

    !------------------------------------------- Examples 5.1 / 5.6 ----
    case (C_ACC2D, C_ACC3D)
       call exact_state(x, zero, u, phi)
       return

    !------------------------------------------- Examples 5.2 / 5.7 ----
    case (C_WB2D, C_WB3D)
       ! exact equilibrium, u = 0

    !------------------------------------------- Examples 5.3 / 5.8 ----
    case (C_PERT2D, C_PERT3D)
       ! p = p^e + mu exp(-100 |x - x0|^2)
       r2 = sum((x(1:nd) - pert_x0(1:nd))**2)
       p  = p + pert_mu*exp(-100.0_dp*r2)

    !------------------------------------------- Example 5.4 ----------
    case (C_BLAST2D)
       if (blast_multi) then
          print *, "Blast is working"
          ! dp = 100 sum_{i=1..5} I_{B(x_i,r)}(x)
          dpsum = zero
          do ib = 1, 5
             r2 = sum((x(1:nd) - bx(1:nd,ib))**2)
             if (sqrt(r2) < bmr) dpsum = dpsum + blast_dp
          end do
          p = p + dpsum
       else
          ! p_0(r) = p^e(r) + 100 for r < 0.1
          r = radius(x)
          if (r < blast_r0) p = p + blast_dp
       end if

    !------------------------------------------- Example 5.5 ----------
    case (C_JEANS2D)
       arg = sum(kwave(1:nd)*x(1:nd))
       rho = rho0*(one + delta0*sin(arg))
       p   = p0_bg*(one + delta0*sin(arg))

    !------------------------------------------- Example 5.9 ----------
    case (C_EXPL3D)
       r = radius(x)
       if (r < blast_r0) p = expl_alpha*p

    end select

    call prim2cons(rho, vel, p, u)
  end subroutine init_state

end module mod_physics
