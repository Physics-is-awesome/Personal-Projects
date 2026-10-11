! Analytic normal principal blocks for the first-order scaffold.
!
! This module intentionally does not claim a complete 57-variable
! characteristic map.  Analytic lapse/gravity/shift blocks and the explicitly
! classified transport completion of the 57-field layout are exposed.
module fo_normal_system_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IK, IALPHA, IBETAX, IBETAY, IBETAZ
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  implicit none
  private
  integer, parameter, public :: FO_BLOCK_LAPSE=1, FO_BLOCK_GRAVITY=2, FO_BLOCK_SHIFT=3
  integer, parameter, public :: FO_MAX_BLOCK=4
  integer, parameter, public :: FO_NSTATE=57
  integer, parameter, public :: FO_MODE_UNSUPPORTED=0, FO_MODE_ADVECTION=1

  ! Number of first-order variables per sector (Brown et al. PRD 85 084004).
  integer, parameter, public :: FO_N_GAUGE   = 20
  integer, parameter, public :: FO_N_NONGAGE = 32
  ! Published gauge-block multiplicities (Sec. III.C of that paper):
  !   11 zero-speed, 4 at beta_n +- alpha sqrt(G H),
  !   2 at beta_n +- alpha sqrt(f), 2 at beta_n +- alpha lambda.
  integer, parameter, public :: FO_NG_ZERO = 11
  integer, parameter, public :: FO_NG_GH    = 4
  integer, parameter, public :: FO_NG_F     = 2
  integer, parameter, public :: FO_NG_LAM   = 2
  integer, parameter, public :: FO_NG_NGAUGE = FO_NG_ZERO+FO_NG_GH+FO_NG_F+FO_NG_LAM
  ! Non-gauge block: 27 zero-speed, 3 at beta_n +- alpha.
  integer, parameter, public :: FO_NN_ZERO = 27
  integer, parameter, public :: FO_NN_PM   = 3
  integer, parameter, public :: FO_NN_NGAUGE = FO_NN_ZERO + 2*FO_NN_PM

  public :: fo_normal_block_eigensystem, fo_normal_block_name
  public :: fo_normal_state_eigensystem, fo_normal_mode_class
  public :: fo_complete_normal_matrix
  public :: fo_published_speeds, fo_gauge_condition_residual

contains

  !---------------------------------------------------------------------------
  ! Characteristic speeds of the FIRST-ORDER BSSN system, taken from
  !
  !   J. D. Brown, P. Diener, S. E. Field, J. S. Hesthaven, F. Herrmann,
  !   A. H. Mroue, O. Sarbach, E. Schnetter, M. Tiglio, M. Wagman,
  !   "Numerical simulations with a first order BSSN formulation of Einstein's
  !    field equations", Phys. Rev. D 85, 084004 (2012), arXiv:1202.1038,
  !   Sec. III.C, eqs. (3.22)-(3.30).
  !
  ! With the standard gauge choices used by this code,
  !
  !     1+log slicing   f = 2 / alpha
  !     Gamma-driver    G = 3 / (4 alpha^2),  H = e^{4 phi},  lambda = sqrt(4GH/3)
  !
  ! the three families of gauge speeds reduce to
  !
  !     alpha sqrt(G H) = (sqrt(3)/2) e^{2 phi}
  !     alpha sqrt(f)   = sqrt(2 alpha)
  !     alpha lambda    = e^{2 phi}
  !
  ! and the non-gauge block propagates at alpha.
  !
  ! The paper works in the physical ORTHONORMAL frame, where the normal is unit
  ! w.r.t. gamma-hat.  For a normal direction carrying physical gamma^{nn} (the
  ! quantity passed in as `gamma_nn`, i.e. e^{-4phi} gtilde^{nn}) every speed is
  ! multiplied by sqrt(gamma^{nn}) to convert back to that convention.
  !
  ! IMPORTANT (a previous version of this module got this wrong): the published
  ! propagating GAUGE mode is
  !
  !     G^{(alpha,+-)} = K -+ alpha_n / (alpha sqrt(f))     speed beta_n +- alpha sqrt(f)
  !
  ! i.e. the +-speed sector is built from K and alpha_n, NOT from alpha and
  ! d_n alpha.  The lapse alpha itself sits in the ZERO-speed sector together
  ! with beta_n, beta_A, alpha_A, beta_AB and beta_An.  A spatially uniform
  ! lapse perturbation is therefore a zero-speed (gauge) mode and is NOT damped
  ! by a penalty applied only to the +-speed families.
  !---------------------------------------------------------------------------
  subroutine fo_published_speeds(alpha, phi, gamma_nn, beta_n, gauge, nongauge)
    real(dp), intent(in) :: alpha, phi, gamma_nn, beta_n
    real(dp), intent(out) :: gauge(FO_NG_NGAUGE), nongauge(FO_NN_NGAUGE)
    real(dp) :: gn, c_gh, c_f, c_lam, c_ng

    if (alpha <= 0.0_dp) error stop 'fo_published_speeds: non-positive lapse'
    if (gamma_nn <= 0.0_dp) error stop 'fo_published_speeds: non-positive gamma^nn'

    gn = sqrt(gamma_nn)
    c_gh = (sqrt(3.0_dp)/2.0_dp)*exp(2.0_dp*phi)   ! alpha sqrt(G H)
    c_f  = sqrt(2.0_dp*alpha)                    ! alpha sqrt(f), f = 2/alpha
    c_lam = exp(2.0_dp*phi)                        ! alpha lambda
    c_ng = alpha                                    ! non-gauge wave speed

    ! gauge block: zero sector first, then the three +- pairs
    gauge(1:FO_NG_ZERO) = beta_n
    gauge(FO_NG_ZERO+1 : FO_NG_ZERO+FO_NG_GH) = [ &
         beta_n + c_gh*gn, beta_n - c_gh*gn, beta_n + c_gh*gn, beta_n - c_gh*gn]
    gauge(FO_NG_ZERO+FO_NG_GH+1 : FO_NG_ZERO+FO_NG_GH+FO_NG_F) = &
         [beta_n + c_f*gn, beta_n - c_f*gn]
    gauge(FO_NG_ZERO+FO_NG_GH+FO_NG_F+1 : FO_NG_NGAUGE) = &
         [beta_n + c_lam*gn, beta_n - c_lam*gn]

    ! non-gauge block: 27 zero-speed, then 3 pairs at beta_n +- alpha
    nongauge(1:FO_NN_ZERO) = beta_n
    nongauge(FO_NN_ZERO+1 : FO_NN_NGAUGE) = &
         [beta_n + c_ng*gn, beta_n - c_ng*gn, beta_n + c_ng*gn, beta_n - c_ng*gn, &
          beta_n + c_ng*gn, beta_n - c_ng*gn]
  end subroutine fo_published_speeds

  !---------------------------------------------------------------------------
  ! Strong-hyperbolicity condition for the gauge block,
  !   4 G H != 3 f        (eq. 3.25 of the paper)
  ! With the standard choices this is  e^{4phi} != 2 alpha.  Returns the signed
  ! residual 4GH - 3f; zero means the gauge block is only WEAKLY hyperbolic and
  ! the characteristic fields become degenerate.
  !---------------------------------------------------------------------------
  function fo_gauge_condition_residual(alpha, phi) result(res)
    real(dp), intent(in) :: alpha, phi
    real(dp) :: res, G, H, f
    f = 2.0_dp/alpha
    G = 3.0_dp/(4.0_dp*alpha*alpha)
    H = exp(4.0_dp*phi)
    res = 4.0_dp*G*H - 3.0_dp*f
  end function fo_gauge_condition_residual

  ! Normal-direction principal block, in the slot ordering the callers use:
  !
  !     slot 1 = the primary variable       q      (alpha / gamma_nn / beta_n)
  !     slot 2 = its normal derivative      d_n q
  !     slot 3 = the block's secondary slot s      (K / A_nn / beta_n aux)
  !
  ! All three blocks share the same first-order structure: a symmetric
  ! "acoustic" pair (q, d_n q) carrying the propagating +-kappa
  ! characteristics, plus the still-unresolved third slot whose speed is the
  ! pure advection speed -beta_n.
  !
  ! The coupling between slot 1 and slot 2 is what makes the block usable for
  ! an SAT.  If q were an eigenvector by itself (as it was in the previous
  ! scaffold) its characteristic speed was -beta_n, so a spatially uniform
  ! perturbation of q -- exactly the perturbation an excision boundary has to
  ! damp -- lay entirely in that zero-speed mode and no penalty could ever be
  ! applied to it.  Coupling q to its normal derivative reproduces the standard
  ! first-order reduction of the gauge wave, in which d_n q is the momentum
  ! variable carried by the +-kappa characteristics.
  !
  ! kappa values are the normal-direction 1+log speeds:
  !
  !     lapse   : +- alpha sqrt(2 gamma^nn)     (1+log diagonal gauge sector)
  !     gravity : +- alpha sqrt(gamma^nn / 2)
  !     shift   : +- alpha sqrt(gamma^nn) / 2   (shift wave speed)
  !
  ! `lambda` is measured along the normal handed in by the caller.  For the
  ! excision SAT the normal is grad(r)/|grad(r)| (pointing towards increasing
  ! radius), so lambda > 0 is the mode that travels INTO the evolved exterior,
  ! i.e. the mode that must be damped.
  ! 3x3 reduction of the published first-order blocks, for the SAT.
  !
  ! NOTE this is a REDUCTION, not the full published characteristic map.  Each
  ! block keeps the +-speed pair of one published family plus a zero-speed slot.
  ! The rates are the published ones:
  !
  !   lapse   +-sqrt(2 alpha) sqrt(gamma^nn)       (gauge, f = 2/alpha)
  !   shift   +-(sqrt3/2) e^{2phi} sqrt(gamma^nn)  (gauge, alpha sqrt(G H))
  !   gravity +-alpha sqrt(gamma^nn)              (non-gauge)
  !
  ! SLOT ORDER FOLLOWS THE PUBLISHED MODES, which matters:
  !
  !   FO_BLOCK_LAPSE   slot 1 = K,  slot 2 = alpha_n,  slot 3 = alpha
  !       because the published propagating gauge mode is
  !           G^{(alpha,+-)} = K -+ alpha_n / (alpha sqrt(f)),
  !       and the Lapse itself is in the ZERO-speed sector.  A spatially uniform
  !       lapse offset is therefore a gauge (zero-speed) mode and is
  !       deliberately NOT damped by this block; perturbing K is.
  !
  !   FO_BLOCK_GRAVITY slot 1 = gamma_nn, slot 2 = d_n gamma_nn, slot 3 = A~_nn
  !   FO_BLOCK_SHIFT   slot 1 = beta_n,  slot 2 = d_n beta_n,  slot 3 = beta_n aux
  subroutine fo_normal_block_eigensystem(block, alpha, phi, gamma_nn, beta_n, &
                                         n, A, lambda, R, L, labels)
    integer, intent(in) :: block
    real(dp), intent(in) :: alpha, phi, gamma_nn, beta_n
    integer, intent(out) :: n
    real(dp), intent(out) :: A(FO_MAX_BLOCK,FO_MAX_BLOCK), lambda(FO_MAX_BLOCK)
    real(dp), intent(out) :: R(FO_MAX_BLOCK,FO_MAX_BLOCK), L(FO_MAX_BLOCK,FO_MAX_BLOCK)
    character(len=32), intent(out) :: labels(FO_MAX_BLOCK)
    real(dp) :: kappa
    A=0.0_dp; lambda=0.0_dp; R=0.0_dp; L=0.0_dp; labels=''
    if (alpha <= 0.0_dp .or. gamma_nn <= 0.0_dp) error stop 'fo normal block: nonphysical state'
    select case (block)
    case (FO_BLOCK_LAPSE)
      n=3; kappa=sqrt(2.0_dp*alpha*gamma_nn)
      labels(1:3)=[character(len=32) :: 'lapse_plus','lapse_minus','lapse_zero']
    case (FO_BLOCK_GRAVITY)
      n=3; kappa=alpha*sqrt(gamma_nn)
      labels(1:3)=[character(len=32) :: 'gravity_plus','gravity_minus','gravity_zero']
    case (FO_BLOCK_SHIFT)
      n=3; kappa=(sqrt(3.0_dp)/2.0_dp)*exp(2.0_dp*phi)*sqrt(gamma_nn)
      labels(1:3)=[character(len=32) :: 'shift_plus','shift_minus','shift_zero']
    case default
      error stop 'fo normal block: unsupported block (full 57 map unavailable)'
    end select
    if (.not. (kappa > 0.0_dp)) error stop 'fo normal block: non-positive characteristic speed'

    A(1,1)=-beta_n; A(2,2)=-beta_n; A(3,3)=-beta_n
    A(1,2)=kappa; A(2,1)=kappa
    lambda(1)=-beta_n+kappa; lambda(2)=-beta_n-kappa; lambda(3)=-beta_n

    R=0.0_dp
    R(1,1)=1.0_dp; R(1,2)=1.0_dp
    R(2,1)=1.0_dp; R(2,2)=-1.0_dp
    R(3,3)=1.0_dp
    L=0.0_dp
    L(1,1)=0.5_dp; L(1,2)=0.5_dp
    L(2,1)=0.5_dp; L(2,2)=-0.5_dp
    L(3,3)=1.0_dp
  end subroutine fo_normal_block_eigensystem

  ! Principal symbol available without introducing equations which are not
  ! present in bssn_rhs_mod.  The reduction equations contain the transport
  ! part beta^n*d_n q for every stored field.  All other entries of the
  ! legacy 57-variable symbol (in particular the transverse gauge, Gamma,
  ! and constraint-coupling blocks) are deliberately not guessed here.
  !
  ! Consequently this routine is a useful, state-dependent transport
  ! eigensystem, rather than a claim of a complete strongly-hyperbolic BSSN
  ! formulation.  `supported` is false for the modes which are only the
  ! identity/advection completion.
  subroutine fo_normal_state_eigensystem(q, normal, A, lambda, R, L, labels, supported)
    real(dp), intent(in) :: q(FO_NSTATE), normal(3)
    real(dp), intent(out) :: A(FO_NSTATE,FO_NSTATE), lambda(FO_NSTATE)
    real(dp), intent(out) :: R(FO_NSTATE,FO_NSTATE), L(FO_NSTATE,FO_NSTATE)
    character(len=32), intent(out) :: labels(FO_NSTATE)
    logical, intent(out) :: supported(FO_NSTATE)
    real(dp) :: nn(3), nrm, beta_n, gnn
    integer :: j

    nrm = sqrt(sum(normal*normal))
    if (nrm <= tiny(1.0_dp)) error stop 'fo normal state: zero normal'
    nn = normal/nrm
    if (q(18) <= 0.0_dp) error stop 'fo normal state: nonphysical lapse'
    gnn = q(2)*nn(1)*nn(1) + 2.0_dp*q(3)*nn(1)*nn(2) + &
          2.0_dp*q(4)*nn(1)*nn(3) + q(5)*nn(2)*nn(2) + &
          2.0_dp*q(6)*nn(2)*nn(3) + q(7)*nn(3)*nn(3)
    if (gnn <= 0.0_dp) error stop 'fo normal state: nonpositive conformal metric'
    beta_n = q(19)*nn(1) + q(20)*nn(2) + q(21)*nn(3)

    A = 0.0_dp; R = 0.0_dp; L = 0.0_dp
    labels = ''; supported = .false.; lambda = -beta_n
    do j = 1, FO_NSTATE
      A(j,j) = -beta_n
      R(j,j) = 1.0_dp
      L(j,j) = 1.0_dp
      labels(j) = 'unsupported_advection'
    end do
    ! The diagonal transport symbol is the only complete statement supported
    ! by the legacy RHS for the 57-field state.  Marking these modes as
    ! advection (rather than BSSN gauge/gravity modes) prevents callers from
    ! mistaking the completion for fabricated characteristic fields.
    supported = .false.
    do j = 25, FO_NSTATE
      labels(j) = 'reduction_advection'
      supported(j) = .true.
    end do
  end subroutine fo_normal_state_eigensystem

  subroutine fo_complete_normal_matrix(q, normal, A, lambda, labels, supported)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS), normal(3)
    real(dp), intent(out) :: A(NFO_COMPLETE_VARS,NFO_COMPLETE_VARS)
    real(dp), intent(out) :: lambda(NFO_COMPLETE_VARS)
    character(len=32), intent(out) :: labels(NFO_COMPLETE_VARS)
    logical, intent(out) :: supported(NFO_COMPLETE_VARS)
    real(dp) :: nn(3), nrm, beta_n, gnn, ginv_nn, c_lapse
    integer :: j, d

    nrm = sqrt(sum(normal*normal))
    if (nrm <= tiny(1.0_dp)) error stop 'complete normal matrix: zero normal'
    nn = normal/nrm
    if (q(IALPHA) <= 0.0_dp) error stop 'complete normal matrix: nonphysical lapse'
    gnn = q(IGXX)*nn(1)*nn(1) + 2.0_dp*q(IGXY)*nn(1)*nn(2) + &
          2.0_dp*q(IGXZ)*nn(1)*nn(3) + q(IGYY)*nn(2)*nn(2) + &
          2.0_dp*q(IGYZ)*nn(2)*nn(3) + q(IGZZ)*nn(3)*nn(3)
    if (gnn <= 0.0_dp) error stop 'complete normal matrix: nonpositive metric'
    beta_n = q(IBETAX)*nn(1) + q(IBETAY)*nn(2) + q(IBETAZ)*nn(3)
    ginv_nn = exp(-4.0_dp*q(IPHI))/gnn
    c_lapse = sqrt(2.0_dp*q(IALPHA)*ginv_nn)

    A = 0.0_dp
    lambda = -beta_n
    labels = 'unsupported'
    supported = .false.
    do j = 1, NFO_COMPLETE_VARS
      A(j,j) = -beta_n
    end do

    ! Every independent reduction variable has the exact transport part.
    do d = 1, 3
      do j = 1, NVARS
        A(fo_complete_index(j,d),fo_complete_index(j,d)) = -beta_n
        lambda(fo_complete_index(j,d)) = -beta_n
        labels(fo_complete_index(j,d)) = 'reduction-advection'
        supported(fo_complete_index(j,d)) = .true.
      end do
    end do

    ! Closed principal coupling that is derivable from the existing 1+log
    ! and K equations: K couples to the normal lapse derivative.  The
    ! remaining Ricci, Gamma-driver, and constraint-coupling blocks are
    ! intentionally left unsupported until their first-order equations are
    ! implemented.
    do d = 1, 3
      A(IK,fo_complete_index(IALPHA,d)) = -ginv_nn*nn(d)
      A(fo_complete_index(IALPHA,d),IK) = -2.0_dp*q(IALPHA)*nn(d)
    end do
    labels(IK) = 'lapse-K-coupled'
    supported(IK) = .false.
    do d = 1, 3
      labels(fo_complete_index(IALPHA,d)) = 'lapse-K-coupled'
      supported(fo_complete_index(IALPHA,d)) = .false.
    end do
    lambda(IK) = -beta_n
    do d = 1, 3
      lambda(fo_complete_index(IALPHA,d)) = -beta_n
    end do
    if (.not. (c_lapse > 0.0_dp)) error stop 'complete normal matrix: invalid gauge speed'
  end subroutine fo_complete_normal_matrix

  integer function fo_normal_mode_class(label) result(mode)
    character(len=*), intent(in) :: label
    if (index(trim(label),'unsupported') == 1) then
      mode = FO_MODE_UNSUPPORTED
    else if (index(trim(label),'advection') > 0) then
      mode = FO_MODE_ADVECTION
    else
      mode = FO_MODE_UNSUPPORTED
    end if
  end function fo_normal_mode_class

  function fo_normal_block_name(block) result(name)
    integer, intent(in) :: block
    character(len=32) :: name
    select case(block)
    case(FO_BLOCK_LAPSE); name='lapse'
    case(FO_BLOCK_GRAVITY); name='gravity'
    case(FO_BLOCK_SHIFT); name='shift'
    case default; name='unsupported'
    end select
  end function fo_normal_block_name
end module fo_normal_system_mod
