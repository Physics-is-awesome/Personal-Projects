module excision_mod

  use kinds_mod,  only: dp
  use params_mod, only: Nx, Nz, r_excise, dx, bh_spin, excision_order, slicing_mode
  use grid_mod,   only: xg, zg
  use vars_mod,   only: NVARS, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                        IALPHA, IBETAX, IBETAZ

  implicit none
  private

  !---------------------------------------------------------------------------
  ! Point classification
  !---------------------------------------------------------------------------

  integer, parameter, public :: EXCISED  = 0
  integer, parameter, public :: PHYSICAL = 1
  integer, parameter, public :: GHOST    = 2

  ! Number of points required by the centered finite-difference stencil.
  !
  ! For a 4th-order centered derivative:
  !
  !     f'(x_i) =
  !       [f(i-2) - 8f(i-1) + 8f(i+1) - f(i+2)]/(12 dx)
  !
  ! therefore the stencil radius is 2.
  !
  integer, parameter, public :: FD_RADIUS = 2

  real(dp), parameter :: R_EPS = 1.0e-12_dp

  public :: initialize_excision
  public :: is_excised
  public :: fill_excision_ghosts
  public :: extrapolate_excised
  public :: point_status
  public :: stencil_is_safe_x
  public :: stencil_is_safe_z
  public :: excision_status
  public :: set_excision_background
  public :: get_excision_background_point
  public :: excision_characteristic_check
  public :: excision_reset_geometry
  public :: kerr_radius_squared

  !---------------------------------------------------------------------------
  ! Shared excision state.
  !
  ! excision_geom   : PERMANENT geometry classification (EXCISED/PHYSICAL),
  !                   built once from the fixed horizon geometry and never
  !                   mutated afterwards.
  !
  ! excision_status : WORKING per-step mask. Reset from excision_geom at the
  !                   top of every extrapolate_excised() call, then mutated
  !                   in place by fill_excision_ghosts (EXCISED -> GHOST as
  !                   points get filled). Callers that need to know which
  !                   points currently hold usable data (e.g. stencil_mod's
  !                   mask argument) should read THIS array.
  !
  ! Re-deriving excision_status from excision_geom on every call is required:
  ! without the reset, a point flips EXCISED -> GHOST exactly once and is
  ! then skipped by "if (status(i,k) /= EXCISED) cycle" on every subsequent
  ! call, so its ghost value is set once and then silently frozen forever --
  ! reproducing the pure-freezing instability this per-step refill scheme was
  ! introduced to fix.
  !---------------------------------------------------------------------------
  integer, allocatable :: excision_geom(:,:)
  integer, allocatable :: excision_status(:,:)
  logical :: geom_built = .false.
  real(dp), allocatable :: background_state(:,:,:)

contains

  ! Drop the cached permanent geometry mask.  extract_geom is built once per
  ! process by extrapolate_excised, so a resolution change (Nx/Nz) would
  ! otherwise keep the OLD mask and silently mis-classify every new point.
  subroutine excision_reset_geometry()
    if (allocated(excision_geom))   deallocate(excision_geom)
    if (allocated(excision_status)) deallocate(excision_status)
    geom_built = .false.
  end subroutine excision_reset_geometry

  subroutine set_excision_background(u)
    real(dp), intent(in) :: u(:,:,:)
    if (allocated(background_state)) deallocate(background_state)
    allocate(background_state(Nx,Nz,NVARS))
    background_state = u
  end subroutine set_excision_background

  subroutine get_excision_background_point(i, k, bg_pt)
    integer, intent(in) :: i, k
    real(dp), intent(out) :: bg_pt(NVARS)
    if (allocated(background_state)) then
      bg_pt = background_state(i, k, :)
    else
      bg_pt = 0.0_dp
    end if
  end subroutine get_excision_background_point

  !===========================================================================
  ! Excision geometry
  !
  ! Temporary debugging geometry:
  !
  !     x^2 + z^2 < r_excise^2
  !
  ! This should eventually be replaced with the Kerr-Schild horizon geometry.
  !===========================================================================

  logical function is_excised(i,k)

    integer, intent(in) :: i,k

    is_excised = (kerr_radius_squared(xg(i), zg(k)) < r_excise**2)

  end function is_excised

  real(dp) function kerr_radius_squared(x,z)
    real(dp), intent(in) :: x,z
    real(dp) :: rho2, a2
    rho2 = x*x + z*z
    a2 = bh_spin*bh_spin
    kerr_radius_squared = 0.5_dp * (rho2 - a2 + &
      sqrt((rho2-a2)**2 + 4.0_dp*a2*z*z))
  end function kerr_radius_squared

  subroutine excision_characteristic_check(u, max_speed, n_bad)
    real(dp), intent(in) :: u(:,:,:)
    real(dp), intent(out) :: max_speed
    integer, intent(out) :: n_bad
    integer :: i,k
    real(dp) :: phi, alpha, beta_n, gamma_nn, speed
    real(dp) :: gxx, gxy, gxz, gyy, gyz, gzz, det
    real(dp) :: gi11, gi12, gi13, gi22, gi23, gi33, normal_x, normal_z
    real(dp) :: rho2, a2, disc, grad_norm, grad_qx, grad_qz

    max_speed = -huge(1.0_dp)
    n_bad = 0
    do k = 2, Nz-1
      do i = 2, Nx-1
        if (is_excised(i,k)) cycle
        if (kerr_radius_squared(xg(i),zg(k)) > (r_excise+2.0_dp*max(dx,abs(zg(2)-zg(1))))**2) cycle
        ! The excision surface is r_Kerr(x,z)=constant, not a coordinate
        ! sphere.  Use the Euclidean unit covector normal grad(r_Kerr^2) /
        ! |grad(r_Kerr^2)| when contracting beta^i and gamma^{ij}.
        rho2 = xg(i)**2 + zg(k)**2
        a2 = bh_spin*bh_spin
        disc = sqrt((rho2-a2)**2 + 4.0_dp*a2*zg(k)**2)
        if (disc <= tiny(1.0_dp)) cycle
        grad_qx = xg(i)*(1.0_dp + (rho2-a2)/disc)
        grad_qz = zg(k)*(1.0_dp + (rho2+a2)/disc)
        grad_norm = sqrt(grad_qx*grad_qx + grad_qz*grad_qz)
        if (grad_norm <= tiny(1.0_dp)) cycle
        normal_x = grad_qx/grad_norm
        normal_z = grad_qz/grad_norm
        phi = u(i,k,IPHI)
        alpha = u(i,k,IALPHA)
        beta_n = u(i,k,IBETAX)*normal_x + u(i,k,IBETAZ)*normal_z
        gxx=u(i,k,IGXX); gxy=u(i,k,IGXY); gxz=u(i,k,IGXZ)
        gyy=u(i,k,IGYY); gyz=u(i,k,IGYZ); gzz=u(i,k,IGZZ)
        det = gxx*(gyy*gzz-gyz*gyz)-gxy*(gxy*gzz-gyz*gxz) &
            +gxz*(gxy*gyz-gyy*gxz)
        if (det <= 0.0_dp) cycle
        gi11=(gyy*gzz-gyz*gyz)/det
        gi12=(gxz*gyz-gxy*gzz)/det
        gi13=(gxy*gyz-gxz*gyy)/det
        gi22=(gxx*gzz-gxz*gxz)/det
        gi23=(gxy*gxz-gxx*gyz)/det
        gi33=(gxx*gyy-gxy*gxy)/det
        gamma_nn = exp(-4.0_dp*phi) * (gi11*normal_x*normal_x + &
          2.0_dp*gi13*normal_x*normal_z + gi33*normal_z*normal_z)
        if (gamma_nn <= 0.0_dp) cycle
        ! Bona-Masso lapse speed is alpha*sqrt(f*gamma^{nn}).  The
        ! production 1+log slicing has f=2/alpha; maximal slicing has
        ! f=1/alpha.  Mode 2 freezes the lapse, so it has no propagating
        ! lapse-gauge characteristic to include in this check.
        select case(slicing_mode)
        case(1)
          speed = -beta_n + sqrt(alpha*gamma_nn)
        case(2)
          speed = -beta_n
        case default
          speed = -beta_n + sqrt(2.0_dp*alpha*gamma_nn)
        end select
        max_speed = max(max_speed, speed)
        if (speed >= 0.0_dp) n_bad = n_bad + 1
      end do
    end do
    if (max_speed == -huge(1.0_dp)) max_speed = 0.0_dp
  end subroutine excision_characteristic_check


  !===========================================================================
  ! Return point classification
  !===========================================================================

  integer function point_status(i,k,status)

    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    if (i < 1 .or. i > Nx .or. k < 1 .or. k > Nz) then
      point_status = EXCISED
      return
    end if

    point_status = status(i,k)

  end function point_status


  !===========================================================================
  ! Construct permanent geometry mask.
  !===========================================================================

  subroutine initialize_excision(status)

    integer, intent(out) :: status(:,:)

    integer :: i,k

    do k = 1,Nz
      do i = 1,Nx

        if (is_excised(i,k)) then
          status(i,k) = EXCISED
        else
          status(i,k) = PHYSICAL
        end if

      end do
    end do

  end subroutine initialize_excision


  !===========================================================================
  ! Determine whether a fourth-order centered x derivative is legal.
  !
  ! The derivative requires:
  !
  !     i-2, i-1, i+1, i+2
  !
  ! None may be excised.
  !===========================================================================

  logical function stencil_is_safe_x(i,k,status)

    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    stencil_is_safe_x = .false.

    if (i-2 < 1 .or. i+2 > Nx) return

    if (status(i-2,k) == EXCISED) return
    if (status(i-1,k) == EXCISED) return
    if (status(i+1,k) == EXCISED) return
    if (status(i+2,k) == EXCISED) return

    stencil_is_safe_x = .true.

  end function stencil_is_safe_x


  !===========================================================================
  ! Determine whether a fourth-order centered z derivative is legal.
  !===========================================================================

  logical function stencil_is_safe_z(i,k,status)

    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    stencil_is_safe_z = .false.

    if (k-2 < 1 .or. k+2 > Nz) return

    if (status(i,k-2) == EXCISED) return
    if (status(i,k-1) == EXCISED) return
    if (status(i,k+1) == EXCISED) return
    if (status(i,k+2) == EXCISED) return

    stencil_is_safe_z = .true.

  end function stencil_is_safe_z


  !===========================================================================
  ! Build (once) the permanent horizon-geometry mask, then refresh the
  ! working ghost-fill mask fresh FROM that permanent mask and extrapolate.
  !
  ! This is the single entry point drivers should call once per timestep
  ! (after the exterior has been advanced): it owns excision_geom and
  ! excision_status so callers never need to allocate or track a mask
  ! themselves.
  !===========================================================================

  subroutine extrapolate_excised(u)

    real(dp), intent(inout) :: u(:,:,:)

    if (.not. geom_built) then
      if (allocated(excision_geom)) deallocate(excision_geom)
      allocate(excision_geom(Nx,Nz))
      call initialize_excision(excision_geom)
      geom_built = .true.
    end if

    if (.not. allocated(excision_status)) allocate(excision_status(Nx,Nz))

    ! Reset EVERY call -- do not carry last step's GHOST classification
    ! forward. See the module-level comment above excision_status.
    excision_status = excision_geom

    call fill_excision_ghosts(u, excision_status)

  end subroutine extrapolate_excised


  !===========================================================================
  ! Fill ghost points.
  !
  ! Important:
  !
  ! The EXCISED region itself is never evolved. Values are constructed there
  ! only because exterior finite-difference stencils may need artificial
  ! values.
  !===========================================================================

  subroutine fill_excision_ghosts(u,status)

    real(dp), intent(inout) :: u(:,:,:)
    integer,  intent(inout) :: status(:,:)

    integer :: i,k
    integer :: sweep
    integer :: nsweeps

    integer :: nfilled
    integer :: nfailed

    logical :: filled
    integer :: order

    integer :: n1,n2,n3,n4

    integer, allocatable :: status_snapshot(:,:)

    !-----------------------------------------------------------------------
    ! We only need enough sweeps to propagate data through the excision
    ! region. This is NOT related to FD_RADIUS.
    !-----------------------------------------------------------------------

    nsweeps = ceiling(r_excise/dx) + FD_RADIUS + 2

    nfilled = 0
    nfailed = 0

    n1 = 0
    n2 = 0
    n3 = 0
    n4 = 0

    allocate(status_snapshot(Nx,Nz))

    do sweep = 1,nsweeps

      ! Freeze this sweep's view of what's usable BEFORE any point in this
      ! sweep is filled. Without this snapshot, a point whose best-aligned
      ! (truly outward) direction isn't ready yet falls back, WITHIN THE
      ! SAME SWEEP, to a worse-aligned direction that chains through other
      ! points filled moments earlier in that same raster pass -- rather
      ! than waiting one more sweep for its real neighbor to be ready with
      ! genuinely physical (or shallowly-filled) data. Each link in such a
      ! same-sweep chain reapplies the [4,-6,4,-1] extrapolation (L1 norm
      ! 15), so a chain just a few links deep amplifies an O(1) boundary
      ! value into O(10^4) garbage -- exactly the axis failure this fixes.
      ! Gating every usability check this sweep on a frozen snapshot forces
      ! proper onion-peeling inward from the true boundary: a point only
      ! ever uses data that was already resolved as of the END of the
      ! PREVIOUS sweep (or genuinely physical data), never a same-sweep
      ! neighbor.
      status_snapshot = status

      do k = 1,Nz
        do i = 1,Nx

          if (status_snapshot(i,k) /= EXCISED) cycle

          call fill_one_point(u,status_snapshot,i,k,filled,order)

          if (.not. filled) cycle

          status(i,k) = GHOST

          nfilled = nfilled + 1

          select case(order)
          case(1)
            n1 = n1 + 1
          case(2)
            n2 = n2 + 1
          case(3)
            n3 = n3 + 1
          case(4)
            n4 = n4 + 1
          end select

        end do
      end do

      if (.not. any(status == EXCISED)) exit

    end do

    deallocate(status_snapshot)

    nfailed = count(status == EXCISED)

    if (nfailed > 0) then
      write(*,'(A,I0,A,I0,A,4(1X,I0))') 'Excision fill failed: ', nfailed, &
        ' points after ', sweep, ' sweeps; orders 1-4:', n1, n2, n3, n4
      error stop 'Excision: unable to construct all ghost values'
    end if

    call enforce_ghost_metric_positivity(u, status)

  end subroutine fill_excision_ghosts

  subroutine enforce_ghost_metric_positivity(u, status)
    real(dp), intent(inout) :: u(:,:,:)
    integer, intent(in) :: status(:,:)
    integer :: i, k, n
    real(dp) :: g(3,3), gb(3,3), trial(3,3), theta

    do k = 1, Nz
      do i = 1, Nx
        if (status(i,k) /= GHOST) cycle
        g = metric_from_state(u(i,k,:))
        if (metric_is_spd(g)) cycle

        gb = metric_from_state(background_state(i,k,:))
        theta = 1.0_dp
        do n = 1, 40
          theta = 0.5_dp*theta
          trial = (1.0_dp-theta)*gb + theta*g
          if (metric_is_spd(trial)) exit
        end do
        if (.not. metric_is_spd(trial)) then
          error stop 'Excision: unable to preserve positive ghost metric'
        end if
        call store_metric(u(i,k,:), trial)
      end do
    end do
  end subroutine enforce_ghost_metric_positivity

  logical function metric_is_spd(g)
    real(dp), intent(in) :: g(3,3)
    real(dp) :: d2, d3
    d2 = g(1,1)*g(2,2) - g(1,2)*g(1,2)
    d3 = g(1,1)*(g(2,2)*g(3,3)-g(2,3)*g(2,3)) &
       - g(1,2)*(g(1,2)*g(3,3)-g(1,3)*g(2,3)) &
       + g(1,3)*(g(1,2)*g(2,3)-g(1,3)*g(2,2))
    metric_is_spd = g(1,1) > 1.0e-12_dp .and. d2 > 1.0e-12_dp .and. &
                    d3 > 1.0e-12_dp
  end function metric_is_spd

  function metric_from_state(v) result(g)
    real(dp), intent(in) :: v(:)
    real(dp) :: g(3,3)
    g = reshape([v(IGXX),v(IGXY),v(IGXZ),v(IGXY),v(IGYY),v(IGYZ), &
                 v(IGXZ),v(IGYZ),v(IGZZ)], [3,3])
  end function metric_from_state

  subroutine store_metric(v, g)
    real(dp), intent(inout) :: v(:)
    real(dp), intent(in) :: g(3,3)
    v(IGXX)=g(1,1); v(IGXY)=g(1,2); v(IGXZ)=g(1,3)
    v(IGYY)=g(2,2); v(IGYZ)=g(2,3); v(IGZZ)=g(3,3)
  end subroutine store_metric


  !===========================================================================
  ! Fill a single point.
  !===========================================================================

  subroutine fill_one_point(u,status,i,k,filled,order)

    real(dp), intent(inout) :: u(:,:,:)
    integer,  intent(inout) :: status(:,:)

    integer, intent(in) :: i,k

    logical, intent(out) :: filled
    integer, intent(out) :: order

    integer :: dirs_i(8)
    integer :: dirs_k(8)

    integer :: n
    integer :: si,sk

    real(dp) :: r
    real(dp) :: nx,nz

    logical :: ok
    integer :: this_order

    filled = .false.
    order  = 0

    r = sqrt(xg(i)**2 + zg(k)**2)

    if (r < R_EPS) then
      nx = 0.0_dp
      nz = 1.0_dp
    else
      nx = xg(i)/r
      nz = zg(k)/r
    end if

    ! Cardinal + diagonal directions.
    dirs_i = [ 1,-1, 0, 0, 1, 1,-1,-1 ]
    dirs_k = [ 0, 0, 1,-1, 1,-1, 1,-1 ]

    call sort_directions(dirs_i,dirs_k,nx,nz)

    do n = 1,8

      si = dirs_i(n)
      sk = dirs_k(n)

      call extrapolate_direction( &
        u,status,i,k,si,sk,ok,this_order)

      if (ok) then
        filled = .true.
        order  = this_order
        return
      end if

    end do

  end subroutine fill_one_point


  !===========================================================================
  ! Directional extrapolation.
  !===========================================================================

  subroutine extrapolate_direction(u,status,i,k,si,sk,filled,order)

    real(dp), intent(inout) :: u(:,:,:)
    integer,  intent(in)    :: status(:,:)

    integer, intent(in) :: i,k
    integer, intent(in) :: si,sk

    logical, intent(out) :: filled
    integer, intent(out) :: order

    real(dp) :: b0(NVARS), b1(NVARS), b2(NVARS), b3(NVARS), b4(NVARS)
    real(dp) :: d1(NVARS), d2(NVARS), d3(NVARS), d4(NVARS)

    filled = .false.
    order  = 0

    call background_at(i,k,b0)
    call background_at(i+si,k+sk,b1)
    call background_at(i+2*si,k+2*sk,b2)
    call background_at(i+3*si,k+3*sk,b3)
    call background_at(i+4*si,k+4*sk,b4)
    call perturbation_at(u,i+si,k+sk,b1,d1)
    call perturbation_at(u,i+2*si,k+2*sk,b2,d2)
    call perturbation_at(u,i+3*si,k+3*sk,b3,d3)
    call perturbation_at(u,i+4*si,k+4*sk,b4,d4)

    ! Use the highest-order one-sided extension supported by the frozen
    ! working mask.  The mask snapshot prevents extrapolation chains within
    ! a sweep, so each value is based on already-resolved exterior data.

    if (excision_order >= 4 .and. usable_point(status,i+si,k+sk)   .and. &
        usable_point(status,i+2*si,k+2*sk) .and. &
        usable_point(status,i+3*si,k+3*sk) .and. &
        usable_point(status,i+4*si,k+4*sk)) then

      u(i,k,:) = b0 + 4.0_dp*d1 - 6.0_dp*d2 + 4.0_dp*d3 - d4

      filled = .true.
      order  = 4
      return

    end if

    if (excision_order >= 3 .and. usable_point(status,i+si,k+sk) .and. &
        usable_point(status,i+2*si,k+2*sk) .and. &
        usable_point(status,i+3*si,k+3*sk)) then

      u(i,k,:) = b0 + 3.0_dp*d1 - 3.0_dp*d2 + d3

      filled = .true.
      order  = 3
      return

    end if

    if (excision_order >= 2 .and. usable_point(status,i+si,k+sk) .and. &
        usable_point(status,i+2*si,k+2*sk)) then

      u(i,k,:) = b0 + 2.0_dp*d1 - d2

      filled = .true.
      order  = 2
      return
    end if

    if (usable_point(status,i+si,k+sk)) then

      u(i,k,:) = b0 + d1

      filled = .true.
      order  = 1
      return

    end if

  end subroutine extrapolate_direction

  subroutine background_at(i,k,state)
    integer, intent(in) :: i,k
    real(dp), intent(out) :: state(NVARS)
    if (i < 1 .or. i > Nx .or. k < 1 .or. k > Nz) then
      state = 0.0_dp
    else
      if (.not. allocated(background_state)) then
        state = 0.0_dp
      else
        state = background_state(i,k,:)
      end if
    end if
  end subroutine background_at

  subroutine perturbation_at(u,i,k,bg,delta)
    real(dp), intent(in) :: u(:,:,:), bg(NVARS)
    integer, intent(in) :: i,k
    real(dp), intent(out) :: delta(NVARS)
    if (i < 1 .or. i > Nx .or. k < 1 .or. k > Nz) then
      delta = 0.0_dp
    else
      delta = u(i,k,:) - bg
    end if
  end subroutine perturbation_at


  !===========================================================================
  ! A PHYSICAL or already-filled GHOST point can be used as source data.
  !===========================================================================

  logical function usable_point(status,i,k)

    integer, intent(in) :: status(:,:)
    integer, intent(in) :: i,k

    if (i < 1 .or. i > Nx .or. k < 1 .or. k > Nz) then
      usable_point = .false.
      return
    end if

    usable_point = status(i,k) /= EXCISED

  end function usable_point


  !===========================================================================
  ! Rank directions according to their alignment with the outward normal.
  !===========================================================================

  subroutine sort_directions(di,dk,nx,nz)

    integer, intent(inout) :: di(8),dk(8)
    real(dp), intent(in) :: nx,nz

    integer :: a,b
    integer :: ti,tk

    real(dp) :: sa,sb
    real(dp) :: na,nb

    do a = 1,7
      do b = a+1,8

        na = sqrt(real(di(a)**2 + dk(a)**2,dp))
        nb = sqrt(real(di(b)**2 + dk(b)**2,dp))

        sa = (nx*real(di(a),dp) + nz*real(dk(a),dp))/na
        sb = (nx*real(di(b),dp) + nz*real(dk(b),dp))/nb

        if (sb > sa) then

          ti = di(a)
          tk = dk(a)

          di(a) = di(b)
          dk(a) = dk(b)

          di(b) = ti
          dk(b) = tk

        end if

      end do
    end do

  end subroutine sort_directions

end module excision_mod
