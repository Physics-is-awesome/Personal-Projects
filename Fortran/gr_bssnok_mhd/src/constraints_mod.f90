module constraints_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz
  use grid_mod, only: xg
  use vars_mod
  use geometry_mod, only: sym3_inverse, pack6, pack6A
  use cartoon_mod, only: get_neighbor
  use hydro_vars_mod, only: ID, ISX, ISY, ISZ, ITAU
  use primitive_recovery_mod, only: con2prim
  implicit none
  private
  public :: hamiltonian_constraint, elliptic_hamiltonian_correction

contains

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

        R = ricci_scalar_direct(u, i, k)

        Hout(i,k) = R + (2.0_dp/3.0_dp)*Ktrace*Ktrace - AijAij
        if (present(hydro_cons)) then
          phi = u(i,k,IPHI)
          ginv_phys = exp(-4.0_dp*phi)*gtinv
          call con2prim(hydro_cons(i,k,ID), hydro_cons(i,k,ISX), hydro_cons(i,k,ISY), &
                        hydro_cons(i,k,ISZ), hydro_cons(i,k,ITAU), ginv_phys, rho, vx, vy, vz, eps, ok)
          if (ok) Hout(i,k) = Hout(i,k) - 16.0_dp*acos(-1.0_dp)*rho
        end if
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
    real(dp), intent(in) :: hydro_cons(:,:,:)
    integer, intent(in) :: niter
    real(dp), intent(in) :: omega
    real(dp), intent(out) :: hbefore, hafter
    real(dp), allocatable :: H(:,:), delta(:,:), next(:,:), phi_old(:,:)
    integer :: i, k, it, imin, imax, kmin, kmax
    real(dp) :: source, den, xp, xm, zp, zm, dnew, step, oldnorm, trialnorm
    integer :: ncell

    allocate(H(Nx,Nz), delta(Nx,Nz), next(Nx,Nz), phi_old(Nx,Nz))
    delta = 0.0_dp
    call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
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
        call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
        trialnorm = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
        if (trialnorm < oldnorm .or. step <= 1.0e-4_dp) exit
        step = 0.5_dp*step
      end do
      if (trialnorm < oldnorm) then
        delta = delta + step*(next-delta)
      else
        u(:,:,IPHI) = phi_old
      end if
      call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
    end do
    call hamiltonian_constraint(u,H,imin,imax,kmin,kmax,hydro_cons)
    hafter = sqrt(sum(H(imin:imax,kmin:kmax)**2)/real(ncell,dp))
    deallocate(H,delta,next,phi_old)
  end subroutine elliptic_hamiltonian_correction

end module constraints_mod
