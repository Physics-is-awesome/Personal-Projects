module constraints_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz
  use grid_mod, only: xg
  use vars_mod
  use geometry_mod, only: sym3_inverse, pack6, pack6A, conformal_geometry_t, &
                          compute_conformal_geometry
  use stencil_mod, only: stencil_t, compute_stencil
  use excision_mod, only: excision_status
  use cartoon_mod, only: get_neighbor
  !use hydro_vars_mod, only: ID, ISX, ISY, ISZ, ITAU
  !use primitive_recovery_mod, only: con2prim
  implicit none
  private
  public :: hamiltonian_constraint, momentum_constraint
  public :: elliptic_hamiltonian_correction

contains

  ! ---------------------------------------------------------------------------
  ! ---------------------------------------------------------------------------
  ! Conformal (BSSN) momentum constraint, Brown et al.,
  !   "Numerical simulations with a first order BSSN formulation of Einstein's
  !    field equations", Phys. Rev. D 85, 084004 (2012), arXiv:1202.1038,
  ! eq. (2.19b) of that paper:
  !
  !   Mtilde^i = Dtilde_j Atilde^{ij} + 6 Atilde^{ij} d_j phi
  !              - (2/3) Dtilde^i K  =  0
  !
  ! with Dtilde the connection built from gamma-tilde, Atilde = e^{-4phi} K_ij
  ! - (1/3) gamma-tilde_ij K, and Ktilde^{ij} = gamma-tilde^{ik} gamma-tilde^{jl}
  ! Atilde_kl.  Note there are NO Gammabar / gamma_kk algebra terms in this
  ! form -- they are already absorbed, which is why the earlier hand-derived
  ! attempts (see git history) could not be made to converge on exact data.
  !
  ! The same paper's Hamiltonian constraint (eq. 2.19a) is what
  ! hamiltonian_constraint() below evaluates; its Ricci^phi contraction gives
  ! -8 Dtilde^i Dtilde_i phi - 8 Dtilde^i phi Dtilde_i phi, matching the
  ! Rphi term built in geometry_mod.
  !
  ! Range: i in [2, Nx-1], k in [2, Nz-1] (one layer of derivatives needed).
  ! ---------------------------------------------------------------------------
  subroutine momentum_constraint(u, Mout)
    real(dp), intent(in)  :: u(:,:,:)
    real(dp), intent(out) :: Mout(:,:,:)
    integer, parameter :: IA(6) = [1,1,1,2,2,3]
    integer, parameter :: IB(6) = [1,2,3,2,3,3]
    type(stencil_t) :: S
    type(conformal_geometry_t) :: G
    real(dp) :: gt(3,3), gtinv(3,3), det, Ktr
    real(dp) :: Ab(3,3), dAb(3,3,3), Abup(3,3), dAbup(3,3,3)
    real(dp) :: dgt(3,3,3), dgtinv(3,3,3), dphi(3), dKtr(3)
    real(dp) :: t1(NVARS), t2(NVARS), t3(NVARS), tx(NVARS)
    real(dp) :: acc, q
    integer :: i, k, j, a, b, c, d, dir, iv

    Mout = 0.0_dp
    do k = 2, Nz - 1
      do i = 2, Nx - 1
        call compute_stencil(u, i, k, excision_status, S)
        call compute_conformal_geometry(S, G)
        gt = G%gt
        call sym3_inverse(gt, gtinv, det)
        Ktr = u(i,k,IK)

        t1 = S%d1x;  t2 = S%d1y;  t3 = S%d1z
        dphi = [t1(IPHI), t2(IPHI), t3(IPHI)]
        dKtr = [t1(IK),   t2(IK),   t3(IK)  ]

        ! ---- Atilde_ij and its directional derivatives
        Ab = 0.0_dp
        do iv = 1, 6
          a = IA(iv); b = IB(iv)
          Ab(a,b) = S%val(IATILDE(iv))
          Ab(b,a) = Ab(a,b)
        end do
        dgt = 0.0_dp
        dAb = 0.0_dp
        do dir = 1, 3
          select case (dir)
          case (1); tx = t1
          case (2); tx = t2
          case (3); tx = t3
          end select
          do iv = 1, 6
            a = IA(iv); b = IB(iv)
            dgt(dir,a,b)  = tx(ICONFMET(iv)); dgt(dir,b,a)  = tx(ICONFMET(iv))
            dAb(dir,a,b)  = tx(IATILDE(iv));   dAb(dir,b,a)  = tx(IATILDE(iv))
          end do
        end do

        ! ---- Atilde^{ij} = gtilde^{ik} gtilde^{jl} Atilde_kl, and d_dir thereof
        Abup = 0.0_dp
        do a = 1, 3
          do b = 1, 3
            q = 0.0_dp
            do c = 1, 3
              do d = 1, 3
                q = q + gtinv(a,c)*gtinv(b,d)*Ab(c,d)
              end do
            end do
            Abup(a,b) = q
          end do
        end do
        do dir = 1, 3
          do a = 1, 3
            do b = 1, 3
              ! d_dir gtilde^{ij} = -gamma-tilde^{ac} gamma-tilde^{bd} d_dir gtilde_cd
              q = 0.0_dp
              do c = 1, 3
                do d = 1, 3
                  q = q + gtinv(a,c)*gtinv(b,d)*dgt(dir,c,d)
                end do
              end do
              dgtinv(dir,a,b) = -q
              acc = 0.0_dp
              do c = 1, 3
                do d = 1, 3
                  acc = acc + dgtinv(dir,a,c)*gtinv(b,d)*Ab(c,d) &
                            + gtinv(a,c)*dgtinv(dir,b,d)*Ab(c,d) &
                            + gtinv(a,c)*gtinv(b,d)*dAb(dir,c,d)
                end do
              end do
              dAbup(dir,a,b) = acc
            end do
          end do
        end do

        ! ---- Mtilde^i = Dtilde_j Atilde^{ij} + 6 Atilde^{ij} d_j phi
        !              - (2/3) gamma-tilde^{ij} d_j K
        !
        ! with j and m summed, i free.  G%Gam(k,i,j) = Gtilde^k_{ij}.
        do j = 1, 3                      ! here j plays the role of free index i
          acc = 0.0_dp
          do dir = 1, 3                  ! dir = j (summed)
            ! d_j Atilde^{ij}
            acc = acc + dAbup(dir,dir,j)
            ! Gtilde^i_{jm} Atilde^{mj}   (m summed over a)
            do a = 1, 3
              acc = acc + G%Gam(j,dir,a)*Abup(a,dir)
              ! Gtilde^m_{jm} Atilde^{ij}  (m summed over a)
              acc = acc + G%Gam(a,dir,a)*Abup(dir,j)
            end do
            ! + 6 Atilde^{ij} d_j phi
            acc = acc + 6.0_dp*Abup(dir,j)*dphi(dir)
            ! - (2/3) gamma-tilde^{ij} d_j K
            acc = acc - (2.0_dp/3.0_dp)*gtinv(dir,j)*dKtr(dir)
          end do
          Mout(i,k,j) = acc
        end do
      end do
    end do
  end subroutine momentum_constraint

  ! Physical metric gamma_ij = exp(4 phi) gamma-tilde_ij at the point offset
  ! from (i,k) by (ix_off, iz_off, y_val), fetched via the same Cartoon
  ! machinery used by the evolution (so this exercises that code too).
  subroutine physical_metric_at(u, i, k, ix_off, iz_off, y_val, gij)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k, ix_off, iz_off
    real(dp), intent(in)  :: y_val
    real(dp), intent(out) :: gij(3,3)
    real(dp) :: v(NVARS), phi

    v = get_neighbor(u, i, k, ix_off, iz_off, y_val)
    phi = v(IPHI)
    gij = exp(4.0_dp*phi) * pack6(v)
  end subroutine physical_metric_at

  ! Physical-metric Christoffel symbols at the point offset from (i,k) by
  ! (ix_off, iz_off, y_val), built from a fresh, independent single-layer
  ! metric stencil centered on THAT point (reached via combined offsets from
  ! the original (i,k) - no recursion needed since get_neighbor always
  ! measures offsets from the original grid point).
  subroutine christoffel_at(u, i, k, ix_off, iz_off, y_val, Gam)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k, ix_off, iz_off
    real(dp), intent(in)  :: y_val
    real(dp), intent(out) :: Gam(3,3,3)
    real(dp) :: g0(3,3), gxp(3,3), gxm(3,3), gyp(3,3), gym(3,3), gzp(3,3), gzm(3,3)
    real(dp) :: ginv(3,3), det
    real(dp) :: dg(3,3,3), Chr_low(3,3,3)
    integer :: p, q, kk

    call physical_metric_at(u, i, k, ix_off,   iz_off,   y_val,      g0)
    call physical_metric_at(u, i, k, ix_off+1, iz_off,   y_val,      gxp)
    call physical_metric_at(u, i, k, ix_off-1, iz_off,   y_val,      gxm)
    call physical_metric_at(u, i, k, ix_off,   iz_off,   y_val+dy,   gyp)
    call physical_metric_at(u, i, k, ix_off,   iz_off,   y_val-dy,   gym)
    call physical_metric_at(u, i, k, ix_off,   iz_off+1, y_val,      gzp)
    call physical_metric_at(u, i, k, ix_off,   iz_off-1, y_val,      gzm)

    call sym3_inverse(g0, ginv, det)

    dg(1,:,:) = (gxp - gxm) / (2.0_dp*dx)
    dg(2,:,:) = (gyp - gym) / (2.0_dp*dy)
    dg(3,:,:) = (gzp - gzm) / (2.0_dp*dz)

    do kk = 1, 3
      do p = 1, 3
        do q = 1, 3
          Chr_low(kk,p,q) = 0.5_dp*(dg(p,q,kk) + dg(q,p,kk) - dg(kk,p,q))
        end do
      end do
    end do
    do kk = 1, 3
      do p = 1, 3
        do q = 1, 3
          Gam(kk,p,q) = sum(ginv(kk,:) * Chr_low(:,p,q))
        end do
      end do
    end do
  end subroutine christoffel_at

  ! Physical-metric Ricci scalar at (i,k) via direct Christoffel
  ! differentiation: R = g^ij( d_k Gam^k_ij - d_j Gam^k_ik
  !                            + Gam^k_kl Gam^l_ij - Gam^k_jl Gam^l_ik )
  function ricci_scalar_direct(u, i, k) result(Rscalar)
    real(dp), intent(in) :: u(:,:,:)
    integer,  intent(in) :: i, k
    real(dp) :: Rscalar
    real(dp) :: g0(3,3), ginv(3,3), det
    real(dp) :: Gam0(3,3,3), Gamxp(3,3,3), Gamxm(3,3,3)
    real(dp) :: Gamyp(3,3,3), Gamym(3,3,3), Gamzp(3,3,3), Gamzm(3,3,3)
    real(dp) :: dGam(3,3,3,3)   ! dGam(m,k,i,j) = d_m Gam^k_ij
    real(dp) :: Rij(3,3)
    real(dp) :: divGam(3,3)     ! d_k Gam^k_ij  (sum over k)
    integer  :: p, q, kk, l

    call physical_metric_at(u, i, k, 0, 0, 0.0_dp, g0)
    call sym3_inverse(g0, ginv, det)

    call christoffel_at(u, i, k,  0, 0, 0.0_dp,  Gam0)
    call christoffel_at(u, i, k,  1, 0, 0.0_dp,  Gamxp)
    call christoffel_at(u, i, k, -1, 0, 0.0_dp,  Gamxm)
    call christoffel_at(u, i, k,  0, 0,  dy,     Gamyp)
    call christoffel_at(u, i, k,  0, 0, -dy,     Gamym)
    call christoffel_at(u, i, k,  0, 1, 0.0_dp,  Gamzp)
    call christoffel_at(u, i, k,  0,-1, 0.0_dp,  Gamzm)

    dGam(1,:,:,:) = (Gamxp - Gamxm) / (2.0_dp*dx)
    dGam(2,:,:,:) = (Gamyp - Gamym) / (2.0_dp*dy)
    dGam(3,:,:,:) = (Gamzp - Gamzm) / (2.0_dp*dz)

    do p = 1, 3
      do q = 1, 3
        divGam(p,q) = dGam(1,1,p,q) + dGam(2,2,p,q) + dGam(3,3,p,q)
      end do
    end do

    ! Assemble R_ij directly:
    !   R_ij = d_k Gam^k_ij - d_j Gam^k_ik + Gam^k_kl Gam^l_ij - Gam^k_jl Gam^l_ik
    ! (all sums over k, l implicit)
    do p = 1, 3
      do q = 1, 3
        Rij(p,q) = divGam(p,q)
        do kk = 1, 3
          Rij(p,q) = Rij(p,q) - dGam(q, kk, p, kk)
          do l = 1, 3
            Rij(p,q) = Rij(p,q) + Gam0(kk,kk,l)*Gam0(l,p,q) - Gam0(kk,q,l)*Gam0(l,p,kk)
          end do
        end do
      end do
    end do

    Rscalar = sum(ginv * Rij)
  end function ricci_scalar_direct

  ! Full Hamiltonian constraint H = R + (2/3) K^2 - Atij Atij (conformal),
  ! evaluated over the interior grid (excluding the axis-adjacent double
  ! stencil margin needed by ricci_scalar_direct: i in [2, Nx-2], k in
  ! [3, Nz-2]).
  subroutine hamiltonian_constraint(u, Hout, imin, imax, kmin, kmax, hydro_cons)
    real(dp), intent(in)  :: u(:,:,:)
    real(dp), intent(out) :: Hout(:,:)
    integer,  intent(out) :: imin, imax, kmin, kmax
    real(dp), intent(in), optional :: hydro_cons(:,:,:)
    integer :: i, k
    real(dp) :: gt(3,3), gtinv(3,3), Aij(3,3), Aup(3,3), AijAij, det, Ktrace, R
    type(stencil_t) :: S
    type(conformal_geometry_t) :: G
    real(dp) :: rho, vx, vy, vz, eps, phi
    real(dp) :: ginv_phys(3,3)
    logical :: ok

    imin = 2; imax = Nx - 2
    kmin = 3; kmax = Nz - 2

    Hout = 0.0_dp
    do k = kmin, kmax
      do i = imin, imax
        gt = pack6(u(i,k,:))
        call sym3_inverse(gt, gtinv, det)
        Aij = pack6A(u(i,k,:))
        Aup = matmul(gtinv, matmul(Aij, gtinv))
        AijAij = sum(Aij*Aup)
        Ktrace = u(i,k,IK)

        ! Evaluate R with the same excision-aware, fourth-order conformal
        ! geometry operator used by the BSSN RHS.  The former independent
        ! direct-Christoffel diagnostic used second-order Cartesian
        ! derivatives and measured its truncation error as a spurious
        ! Hamiltonian violation near the hole.
        call compute_stencil(u, i, k, excision_status, S)
        call compute_conformal_geometry(S, G)
        R = exp(-4.0_dp*u(i,k,IPHI)) * sum(G%gtinv * (G%Rt + G%Rphi))

        Hout(i,k) = R + (2.0_dp/3.0_dp)*Ktrace*Ktrace - AijAij
        !if (present(hydro_cons)) then
        !  phi = u(i,k,IPHI)
        !  ginv_phys = exp(-4.0_dp*phi)*gtinv
        !  call con2prim(hydro_cons(i,k,ID), hydro_cons(i,k,ISX), hydro_cons(i,k,ISY), &
        !                hydro_cons(i,k,ISZ), hydro_cons(i,k,ITAU), ginv_phys, rho, vx, vy, vz, eps, ok)
        !  if (ok) Hout(i,k) = Hout(i,k) - 16.0_dp*acos(-1.0_dp)*rho
        !end if
      end do
    end do
  end subroutine hamiltonian_constraint

  ! Prototype Hamiltonian projection.  It solves the linearized conformal
  ! equation Delta(delta phi) = H/8 with damped Jacobi iterations, holding
  ! tilde-gamma, K, and A fixed.  This is deliberately not a CTS solve:
  ! boundaries and momentum constraints are untouched.  The cylindrical
  ! axisymmetric Laplacian (dxx + dx/x + dzz) is used away from x=0.
  subroutine elliptic_hamiltonian_correction(u, hydro_cons, niter, omega, hbefore, hafter)
    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(in), optional :: hydro_cons(:,:,:)
    integer, intent(in) :: niter
    real(dp), intent(in) :: omega
    real(dp), intent(out) :: hbefore, hafter
    real(dp), allocatable :: H(:,:), delta(:,:), next(:,:), phi_old(:,:)
    integer :: i, k, it, imin, imax, kmin, kmax
    real(dp) :: source, den, xp, xm, zp, zm, dnew, step, oldnorm, trialnorm
    integer :: ncell

    allocate(H(Nx,Nz), delta(Nx,Nz), next(Nx,Nz), phi_old(Nx,Nz))
    delta = 0.0_dp
    if (present(hydro_cons)) then
      call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
    else
      call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
    end if
    ncell = (imax-imin+1)*(kmax-kmin+1)
    hbefore = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
    do it = 1, max(0,niter)
      oldnorm = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
      next = delta
      do k = kmin, kmax
        do i = imin, imax
          source = H(i,k)/8.0_dp
          xp = 1.0_dp/dx**2 + 1.0_dp/(2.0_dp*max(xg(i),dx)*dx)
          xm = 1.0_dp/dx**2 - 1.0_dp/(2.0_dp*max(xg(i),dx)*dx)
          zp = 1.0_dp/dz**2
          zm = zp
          den = 2.0_dp/dx**2 + 2.0_dp/dz**2
          dnew = (xp*delta(i+1,k) + xm*delta(i-1,k) + zp*delta(i,k+1) + &
                  zm*delta(i,k-1) - source)/den
          next(i,k) = delta(i,k) + min(max(omega,0.0_dp),1.0_dp)*(dnew-delta(i,k))
        end do
      end do
      do k = kmin, kmax
        do i = imin, imax
          next(i,k) = delta(i,k) + min(max(next(i,k)-delta(i,k),-0.05_dp),0.05_dp)
        end do
      end do
      phi_old = u(:,:,IPHI)
      step = 1.0_dp
      do
        u(:,:,IPHI) = phi_old + step*(next-delta)
        if (present(hydro_cons)) then
          call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
        else
          call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
        end if
        trialnorm = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
        if (trialnorm < oldnorm .or. step <= 1.0e-4_dp) exit
        step = 0.5_dp*step
      end do
      if (trialnorm < oldnorm) then
        delta = delta + step*(next-delta)
      else
        u(:,:,IPHI) = phi_old
      end if
      if (present(hydro_cons)) then
        call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
      else
        call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
      end if
    end do
    if (present(hydro_cons)) then
      call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
    else
      call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
    end if
    hafter = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
    deallocate(H,delta,next,phi_old)
  end subroutine elliptic_hamiltonian_correction


end module constraints_mod
