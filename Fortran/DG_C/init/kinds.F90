!=======================================================================
!  L. Pan, W. Chen, J. Qiu, T. Xiong,
!  "High order well-balanced and total-energy-conserving local
!   discontinuous Galerkin methods for compressible self-gravitating
!   Euler equations", J. Comput. Phys. 556 (2026) 114807.
!
!  mod_params.F90 : kinds, namelist, per-example constants (Sec. 2, 5)
!  Requires preprocessing:  -cpp -DNDIM=2   or   -cpp -DNDIM=3
!=======================================================================

module mod_kinds
  implicit none
  public
  integer,  parameter :: dp = selected_real_kind(15,307)
  real(dp), parameter :: pi   = 3.141592653589793238462643383279502884_dp
  real(dp), parameter :: zero = 0.0_dp, half = 0.5_dp, one = 1.0_dp
  real(dp), parameter :: two  = 2.0_dp, three = 3.0_dp, four = 4.0_dp
end module mod_kinds

!-----------------------------------------------------------------------
module mod_params
  use mod_kinds
  implicit none
  public

#ifndef NDIM
#define NDIM 2
#endif

  !--- spatial dimension d and the conserved vector U = (rho, rho u, E) --
  integer, parameter :: nd   = NDIM
  integer, parameter :: nvar = nd + 2
  integer, parameter :: IRHO = 1              ! rho
  integer, parameter :: IMX  = 2              ! rho u_1
  integer, parameter :: IEN  = nd + 2         ! E   (in W: E_tot)

  !--- boundary conditions ----------------------------------------------
  integer, parameter :: BC_EXACT = 1          ! Dirichlet from analytic data
  integer, parameter :: BC_TRANS = 2          ! transmissive (zero gradient)
  integer, parameter :: BC_PER   = 3          ! periodic

  !--- equilibrium families, eq. (2.12) ---------------------------------
  integer, parameter :: EQ_PLANE  = 1   ! rho^e = lam sin(k.x)          (5.1, 5.6)
  integer, parameter :: EQ_BESSEL = 2   ! rho^e = lam theta(r)^n, (2.11) (5.2-5.4)
  integer, parameter :: EQ_LANE1  = 3   ! rho^e = rho0 sin(al r)/(al r)  (5.7-5.9)
  integer, parameter :: EQ_CONST  = 4   ! rho^e = rho0  (Jeans swindle)  (5.5)

  !--- test case identifiers, Examples 5.1 - 5.9 ------------------------
  integer, parameter :: C_ACC2D   = 1
  integer, parameter :: C_WB2D    = 2
  integer, parameter :: C_PERT2D  = 3
  integer, parameter :: C_BLAST2D = 4
  integer, parameter :: C_JEANS2D = 5
  integer, parameter :: C_ACC3D   = 6
  integer, parameter :: C_WB3D    = 7
  integer, parameter :: C_PERT3D  = 8
  integer, parameter :: C_EXPL3D  = 9

  !======================= namelist controlled ==========================
  character(len=24) :: case_name  = 'wb2d'
  character(len=8)  :: scheme     = 'sp'     ! 'sp'  structure preserving (3.36)
                                             ! 'std' standard LDG        (3.11)
  character(len=1)  :: basis_type = 'P'      ! 'P' : P^k  (paper) , 'Q' : Q^k
  integer  :: kdeg       = 2                 ! polynomial degree k
  integer  :: nxyz(3)    = (/ 20, 20, 20 /)
  integer  :: rk_order   = 3                 ! 1,2,3 SSP-RK   (Sec. 3.5)
  logical  :: use_oe     = .false.           ! OE damping     (Sec. 3.6)
  logical  :: use_pp     = .false.           ! positivity limiter
  logical  :: use_ibp    = .true.            ! D2tilde (3.24) vs D2 (3.17e)
  real(dp) :: cfl        = 0.15_dp
  real(dp) :: tfinal     = -1.0_dp           ! <0 : case default
  real(dp) :: pois_tol   = 1.0e-14_dp
  integer  :: pois_maxit = 50000
  real(dp) :: G_override = -1.0_dp
  real(dp) :: mu_override= -1.0_dp
  real(dp) :: pp_eps     = 1.0e-13_dp
  real(dp) :: oe_beta    = 1.0_dp            ! multiplier on beta_e
  integer  :: nout       = 0
  logical  :: verbose    = .true.
  logical  :: hK_diam    = .false.           ! .true. : h_K = diam(K)  (Sec. 5)
                                             ! .false.: h_K = min_m dx_m

  namelist /control/ case_name, scheme, basis_type, kdeg, nxyz, rk_order,   &
       use_oe, use_pp, use_ibp, cfl, tfinal, pois_tol, pois_maxit,          &
       G_override, mu_override, pp_eps, oe_beta, nout, verbose, hK_diam

  !========================== derived data ==============================
  integer  :: icase, ieq
  integer  :: bc_fluid, bc_pois
  logical  :: is_periodic

  !--- polytropic / gravitational data, eqs. (2.4)-(2.12) ---------------
  real(dp) :: gam   = 2.0_dp        ! heat ratio gamma
  real(dp) :: pnu   = 2.0_dp        ! polytropic exponent nu  (may differ from gam)
  real(dp) :: Ggrav = 1.0_dp        ! G
  real(dp) :: kap   = 1.0_dp        ! kappa
  real(dp) :: lam   = 1.0_dp        ! lambda
  real(dp) :: pol_n = 1.0_dp        ! polytropic index n , nu = (n+1)/n
  real(dp) :: aLE   = 1.0_dp        ! a  of eq. (2.7)
  real(dp) :: alph  = 1.0_dp        ! alpha = sqrt(4 pi G /(2K))   (5.7)
  real(dp) :: Kpol  = 1.0_dp        ! K    of (5.7)
  real(dp) :: rho0  = 1.0_dp        ! rho0 of (5.7) / Jeans background

  !--- plane wave equilibrium (Examples 5.1, 5.6) -----------------------
  real(dp) :: kvec(3)  = zero       ! |k| = 1/a
  real(dp) :: u_adv(3) = zero       ! (u0,v0,w0)

  !--- Jeans (Example 5.5) ----------------------------------------------
  real(dp) :: p0_bg   = 0.0_dp
  real(dp) :: delta0  = 1.0e-3_dp
  real(dp) :: kwave(3)= zero
  logical  :: jeans_delta = .false. ! E_tot = E + 1/2 (rho-rho0) phi

  !--- perturbations / blasts -------------------------------------------
  real(dp) :: pert_mu    = 0.0_dp
  real(dp) :: pert_x0(3) = zero
  real(dp) :: blast_r0   = 0.1_dp
  real(dp) :: blast_dp   = 100.0_dp
  real(dp) :: expl_alpha = 10.0_dp
  logical  :: blast_multi= .false.
  real(dp) :: bx(3,5)    = zero
  real(dp) :: bmr        = 0.05_dp

  !--- domain -----------------------------------------------------------
  real(dp) :: xlo(3) = zero
  real(dp) :: xhi(3) = one

  !--- LDG numerical flux parameters, eq. (3.10) ------------------------
  !    C11 = 1 and C12 = (0.5,...,0.5)^T exactly as stated in Sec. 3.3.
  real(dp) :: C11    = 1.0_dp
  real(dp) :: C12(3) = (/ 0.5_dp, 0.5_dp, 0.5_dp /)

contains

  !---------------------------------------------------------------------
  subroutine read_input(fname)
    character(len=*), intent(in) :: fname
    integer :: iu, ios
    logical :: ex
    inquire(file=fname, exist=ex)
    if (ex) then
       open(newunit=iu, file=fname, status='old', action='read')
       read(iu, nml=control, iostat=ios)
       if (ios /= 0) then
          write(*,*) 'ERROR: cannot read namelist /control/ from ', trim(fname)
          stop 1
       end if
       close(iu)
    else
       write(*,*) 'input file ', trim(fname), ' not found - using defaults'
    end if
  end subroutine read_input

  !---------------------------------------------------------------------
  subroutine require_dim(d)
    integer, intent(in) :: d
    if (nd /= d) then
       write(*,'(a,i0,a,i0)') 'ERROR: case needs NDIM=', d, ' but binary has NDIM=', nd
       stop 1
    end if
  end subroutine require_dim

  !---------------------------------------------------------------------
  !  a = sqrt( kappa (n+1) lambda^((1-n)/n) / (4 pi G) )        (2.7)
  !---------------------------------------------------------------------
  subroutine set_aLE()
    aLE = sqrt( kap*(pol_n + one)*lam**((one - pol_n)/pol_n)/(four*pi*Ggrav) )
  end subroutine set_aLE

  !---------------------------------------------------------------------
  !  Every physical constant, domain and boundary condition of Sec. 5.
  !---------------------------------------------------------------------
  subroutine setup_case()
    real(dp) :: L
    blast_multi = .false.
    jeans_delta = .false.
    u_adv = zero ; kvec = zero ; pert_x0 = zero

    select case (trim(case_name))

    !----------------------------------------------- Example 5.1 --------
    !  rho = sin( sqrt(2)/(2a) (x+y-2t) ), u=v=1, p = kappa rho^2,
    !  phi = -4 pi G a^2 rho ;  kappa=2pi, gamma=2, G=1/4, lam=1, n=1, nu=2
    case ('acc2d')
       icase = C_ACC2D ; ieq = EQ_PLANE
       call require_dim(2)
       kap = two*pi ; gam = two ; Ggrav = 0.25_dp ; lam = one
       pol_n = one  ; pnu = two
       call set_aLE()                                   ! a = 2
       kvec(1:2) = sqrt(two)/(two*aLE)
       u_adv(1:2) = one
       xlo(1:2) = 0.125_dp*sqrt(two)*pi*aLE
       xhi(1:2) = 0.375_dp*sqrt(two)*pi*aLE
       bc_fluid = BC_EXACT ; bc_pois = BC_EXACT
       if (tfinal < zero) tfinal = 0.8_dp

    !----------------------------------------------- Example 5.2 --------
    case ('wb2d','wb2d_isentropic')
       icase = C_WB2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = two ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()                                   ! a = sqrt(kap/(2 pi G))
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_EXACT ; bc_pois = BC_EXACT
       if (tfinal < zero) tfinal = 5.0_dp

    !  general polytropic equilibrium of Table 5.6 :  gamma = 1.4 , nu = 2
    case ('wb2d_general')
       icase = C_WB2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = 1.4_dp ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_EXACT ; bc_pois = BC_EXACT
       if (tfinal < zero) tfinal = 1.0_dp

    !----------------------------------------------- Example 5.3 --------
    case ('pert2d','pert2d_sym')
       icase = C_PERT2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = two ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       pert_mu = 0.01_dp ; pert_x0 = zero
       if (tfinal < zero) tfinal = 0.1_dp

    case ('pert2d_asym')
       icase = C_PERT2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = two ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       pert_mu = 0.1_dp ; pert_x0(1:2) = 0.3_dp
       if (tfinal < zero) tfinal = 0.1_dp

    !----------------------------------------------- Example 5.4 --------
    case ('blast2d')
       icase = C_BLAST2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = two ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       blast_r0 = 0.1_dp ; blast_dp = 100.0_dp
       use_oe = .true. ; use_pp = .true.
       if (tfinal < zero) tfinal = 0.05_dp

    !  the five-ball perturbation of [29], T = 0.02
    case ('blast2d5')
       icase = C_BLAST2D ; ieq = EQ_BESSEL
       call require_dim(2)
       kap = one ; gam = two ; Ggrav = one ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()
       xlo(1:2) = -half ; xhi(1:2) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       blast_multi = .true. ; blast_dp = 100.0_dp ; bmr = 0.05_dp
       bx = zero
       bx(1,1) = -0.25_dp  ; bx(2,1) = 0.3_dp
       bx(1,2) = -0.25_dp  ; bx(2,2) = 0.1_dp
       bx(1,3) =  0.025_dp ; bx(2,3) = 0.3_dp
       bx(1,4) =  0.025_dp ; bx(2,4) = 0.225_dp
       bx(1,5) =  0.1_dp   ; bx(2,5) = -0.1_dp
       use_oe = .true. ; use_pp = .true.
       if (tfinal < zero) tfinal = 0.02_dp

    !----------------------------------------------- Example 5.5 --------
    !  Jeans instability.  Omega=[0,1]^2 periodic, gamma=5/3, rho0=1,
    !  p0=rho0/gamma, delta0=1e-3, k1=k2=2pi.  G=6.674 unstable,
    !  G=0.6674 stable.  Delta phi = 4 pi G (rho - rho0).
    case ('jeans2d','jeans2d_stable')
       icase = C_JEANS2D ; ieq = EQ_CONST
       call require_dim(2)
       gam = 5.0_dp/3.0_dp ; pnu = gam
       rho0 = one ; p0_bg = rho0/gam
       Ggrav = 6.674_dp
       if (trim(case_name) == 'jeans2d_stable') Ggrav = 0.6674_dp
       delta0 = 1.0e-3_dp
       kwave(1:2) = two*pi
       xlo(1:2) = zero ; xhi(1:2) = one
       bc_fluid = BC_PER ; bc_pois = BC_PER
       jeans_delta = .true.
       use_oe = .true. ; use_pp = .true.
       if (tfinal < zero) tfinal = 2.8_dp

    !----------------------------------------------- Example 5.6 --------
    case ('acc3d')
       icase = C_ACC3D ; ieq = EQ_PLANE
       call require_dim(3)
       kap = two*pi ; gam = two ; Ggrav = one/pi ; lam = one
       pol_n = one ; pnu = two
       call set_aLE()                                   ! a = sqrt(pi)
       kvec(1:3) = sqrt(three)/(three*aLE)
       u_adv(1) = 0.2_dp ; u_adv(2) = 0.3_dp ; u_adv(3) = 0.5_dp
       L = sqrt(three)*pi*aLE
       xlo(1:3) = L/18.0_dp
       xhi(1:3) = 5.0_dp*L/18.0_dp
       bc_fluid = BC_EXACT ; bc_pois = BC_EXACT
       if (tfinal < zero) tfinal = 0.3_dp

    !----------------------------------------------- Example 5.7 --------
    !  rho^e = rho0 sin(alpha r)/(alpha r), p^e = K rho^e^2,
    !  phi^e = -2 K rho^e ,  alpha = sqrt(4 pi G /(2K)) , K=rho0=1, G=1/pi
    case ('wb3d')
       icase = C_WB3D ; ieq = EQ_LANE1
       call require_dim(3)
       Kpol = one ; rho0 = one ; Ggrav = one/pi
       gam = two ; pnu = two
       kap = Kpol ; lam = rho0 ; pol_n = one
       alph = sqrt(four*pi*Ggrav/(two*Kpol))
       aLE = one/alph
       xlo(1:3) = -half ; xhi(1:3) = half
       bc_fluid = BC_EXACT ; bc_pois = BC_EXACT
       if (tfinal < zero) tfinal = 1.0_dp

    !----------------------------------------------- Example 5.8 --------
    case ('pert3d')
       icase = C_PERT3D ; ieq = EQ_LANE1
       call require_dim(3)
       Kpol = one ; rho0 = one ; Ggrav = one/pi
       gam = two ; pnu = two
       kap = Kpol ; lam = rho0 ; pol_n = one
       alph = sqrt(four*pi*Ggrav/(two*Kpol))
       aLE = one/alph
       xlo(1:3) = -half ; xhi(1:3) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       pert_mu = 1.0e-3_dp ; pert_x0 = zero
       if (tfinal < zero) tfinal = 0.1_dp

    !----------------------------------------------- Example 5.9 --------
    case ('expl3d')
       icase = C_EXPL3D ; ieq = EQ_LANE1
       call require_dim(3)
       Kpol = one ; rho0 = one ; Ggrav = one
       gam = two ; pnu = two
       kap = Kpol ; lam = rho0 ; pol_n = one
       alph = sqrt(four*pi*Ggrav/(two*Kpol))
       aLE = one/alph
       xlo(1:3) = -half ; xhi(1:3) = half
       bc_fluid = BC_TRANS ; bc_pois = BC_EXACT
       expl_alpha = 10.0_dp ; blast_r0 = 0.1_dp
       use_oe = .true. ; use_pp = .true.
       if (tfinal < zero) tfinal = 0.15_dp

    case default
       write(*,*) 'ERROR: unknown case_name = ', trim(case_name)
       stop 1
    end select

    if (G_override  > zero) then
       Ggrav = G_override
       if (ieq == EQ_LANE1) alph = sqrt(four*pi*Ggrav/(two*Kpol))
       if (ieq == EQ_BESSEL .or. ieq == EQ_PLANE) call set_aLE()
    end if
    if (mu_override >= zero) pert_mu = mu_override

    is_periodic = (bc_fluid == BC_PER)
  end subroutine setup_case

end module mod_params
