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
  ! Vacuum momentum constraint, in ADM form.
  !
  !     M^i = D_j S^{ij} = 0 ,     S^{ij} = K^{ij} - gamma^{ij} K .
  !
  ! Expressed in the stored BSSN variables via the exact relation for this
  ! code's convention (gamma-tilde_ij = e^{-4phi} gamma_ij, det(gamma-tilde)=1):
  !
  !     K_ij   = e^{4phi} ( Atilde_ij + (1/3) gamma-tilde_ij K )
  !     K^{ij} = e^{-4phi} ( Atilde^{ij} + (1/3) gamma-tilde^{ij} K )
  !  =>  S^{ij} = e^{-4phi} ( Atilde^{ij} - (2/3) gamma-tilde^{ij} K )
  !
  ! and the covariant derivative uses the PHYSICAL connection
  !     Gamma^k_{ij} = gamma^{km} (1/2)( d_i gamma_{mj} + d_j gamma_{mi}
  !                                    - d_m gamma_{ij} ) ,
  !     d_m gamma_ij = 4 d_m phi gamma_ij + e^{4phi} d_m gamma-tilde_ij .
  !
  ! Differentiating S^{ij} = e^{-4phi} Q^{ij} once, with
  ! Q^{ij} = Atilde^{ij} - (2/3) gamma-tilde^{ij} K, gives
  !
  !   d_j S^{ij} = e^{-4phi} [ d_j Atilde^{ij}
  !                          - (2/3) K d_j gamma-tilde^{ij}
  !                          - (2/3) gamma-tilde^{ij} d_j K
  !                          - 4 d_j phi Atilde^{ij}
  !                          + (8/3) d_j phi gamma-tilde^{ij} K ]
  !
  ! which are exactly the five derivative pieces returned below.
  !
  ! HISTORY / WARNING
  !   The previous implementation evaluated
  !       Dtilde_j Atilde^{ij} + 6 Atilde^{ij} d_j phi - (2/3) gamma-tilde^{ij} d_j K
  !   with the CONFORMAL connection, cited as Brown et al. 2012 eq. (2.19b).
  !   That expression is NOT equal to the ADM momentum constraint under this
  !   code's det(gamma-tilde) = 1 convention.  Measured against an independent
  !   ADM evaluation of the same state it disagreed by far more than the
  !   discretisation error, and the gap did NOT shrink under refinement --
  !   an algebraic identity failure, not a truncation error.  See
  !   baseline/MOMENTUM_FINDINGS.md.
  !
  !   Do not reintroduce coefficients here from memory of the literature.  The
  !   pieces below follow from the derivation above and are the ones verified
  !   against exact Kerr-Schild data.
  !
  ! This routine is DIAGNOSTIC ONLY.  It is called by the driver for reporting
  ! and by tests/diagnostics; nothing in the evolution RHS reads it.
  !
  ! Range: i in [2, Nx-1], k in [2, Nz-1] (one layer of derivatives needed).
  ! ---------------------------------------------------------------------------
  subroutine momentum_constraint(u, Mout, Mterms)
    real(dp), intent(in)  :: u(:,:,:)
    real(dp), intent(out) :: Mout(:,:,:)
    ! Optional diagnostic decomposition, last index (each summed over j):
    !   1 = e^{-4phi} d_j Atilde^{ij}
    !   2 = -(2/3) e^{-4phi} K d_j gamma-tilde^{ij}
    !   3 = -(2/3) e^{-4phi} gamma-tilde^{ij} d_j K
    !   4 = -4 e^{-4phi} d_j phi Atilde^{ij}
    !   5 = +(8/3) e^{-4phi} d_j phi gamma-tilde^{ij} K
    !   6 = Gamma^i_{jk} S^{kj}      (physical connection)
    !   7 = Gamma^j_{jk} S^{ik}      (physical connection)
    real(dp), intent(out), optional :: Mterms(:,:,:,:)
    integer, parameter :: IA(6) = [1,1,1,2,2,3]
    integer, parameter :: IB(6) = [1,2,3,2,3,3]
    type(stencil_t) :: S
    type(conformal_geometry_t) :: G
    real(dp) :: gt(3,3), gtinv(3,3), det, Ktr, phi, e4, e4m
    real(dp) :: Ab(3,3), dAb(3,3,3), Abup(3,3), dAbup(3,3,3)
    real(dp) :: dgt(3,3,3), dgtinv(3,3,3), dphi(3), dKtr(3)
    real(dp) :: Qmat(3,3), Smat(3,3)
    real(dp) :: dgamma(3,3,3), Gp(3,3,3)
    real(dp) :: t1(NVARS), t2(NVARS), t3(NVARS), tx(NVARS)
    real(dp) :: acc, q, pieces(7)
    integer :: i, k, jf, j2, a, b, c, d, m, dir, iv

    Mout = 0.0_dp
    if (present(Mterms)) Mterms = 0.0_dp
    do k = 2, Nz - 1
      do i = 2, Nx - 1
        call compute_stencil(u, i, k, excision_status, S)
        call compute_conformal_geometry(S, G)
        gt = G%gt
        call sym3_inverse(gt, gtinv, det)
        phi = u(i,k,IPHI)
        e4  = exp(4.0_dp*phi)
        e4m = 1.0_dp/e4
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
        ! TWO PASSES, and this ordering is load-bearing.
        !
        ! The derivative of Atilde^{ij} needs the WHOLE d_dir gtilde^{ij} tensor:
        !
        !   d_dir Atilde^{ij} = d_dir(g^{ik} g^{jl} A_kl)
        !       = d_dir(g^{ik}) g^{jl} A_kl + g^{ik} d_dir(g^{jl}) A_kl
        !         + g^{ik} g^{jl} d_dir(A_kl)
        !   with  d_dir(g^{ij}) = -g^{ac} g^{bd} d_dir(gtilde_cd)
        !
        ! so the first term reads dgtinv(dir,a,c) and dgtinv(dir,b,d) for ALL
        ! c and d -- not just the entries with indices <= the ones being
        ! computed.  Fusing the two into one loop nest (as this code used to)
        ! therefore reads dgtinv entries that have not been written yet for the
        ! current dir.  dgtinv is a subroutine-scope local that is never
        ! re-zeroed inside the cell loop, so those reads returned values left
        ! over from the PREVIOUS GRID CELL.  That silently corrupted exactly
        ! one term of M (piece 1, d_j Atilde^{ij}), which is the only consumer
        ! of the off-diagonal dgtinv entries, and it is what made the momentum
        ! constraint residual dx-independent.  See the momentum notes in the
        ! Makefile and helper/diagnose_momentum_exact.F90.
        do dir = 1, 3
          ! ---- pass 1: the complete d_dir gtilde^{ij}
          do a = 1, 3
            do b = 1, 3
              q = 0.0_dp
              do c = 1, 3
                do d = 1, 3
                  q = q + gtinv(a,c)*gtinv(b,d)*dgt(dir,c,d)
                end do
              end do
              dgtinv(dir,a,b) = -q
            end do
          end do
          ! ---- pass 2: d_dir Atilde^{ij}, now that dgtinv(dir,:,:) is complete
          do a = 1, 3
            do b = 1, 3
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

        ! ---- Q^{ij} = Atilde^{ij} - (2/3) gamma-tilde^{ij} K
        !      S^{ij} = e^{-4phi} Q^{ij}
        do a = 1, 3
          do b = 1, 3
            Qmat(a,b) = Abup(a,b) - (2.0_dp/3.0_dp)*gtinv(a,b)*Ktr
            Smat(a,b) = e4m*Qmat(a,b)
          end do
        end do

        ! ---- physical connection from d_m gamma_ij
        do dir = 1, 3
          do a = 1, 3
            do b = 1, 3
               ! d_dir gamma_ij = d_dir(e^{4 phi} gtilde_ij)
               !              = e^{4 phi} [ 4 (d_dir phi) gtilde_ij
               !                             + d_dir gtilde_ij ]
               ! BOTH terms carry e^{4 phi}.  Dropping it from the first is a
               ! pure algebraic error: it leaves S^{ij} and d_j S^{ij} correct
               ! (they never touch gamma) while corrupting Gamma^i_{jk}, which
               ! is exactly the split observed by
               ! helper/diagnose_momentum_exact.F90 (S and d_jS agree with the
               ! independent ADM form to 1e-17 and 1e-12; Gamma disagrees by
               ! 1.1e-3).  That is the entire momentum-constraint plateau.
               dgamma(dir,a,b) = e4*( 4.0_dp*dphi(dir)*gt(a,b) + dgt(dir,a,b) )
            end do
          end do
        end do
        do a = 1, 3
          do b = 1, 3
            do c = 1, 3
               ! Gamma^a_{bc} = e^{-4phi} gamma-tilde^{am}
               !                  (1/2)( d_b gamma_{mc} + d_c gamma_{mb}
               !                         - d_m gamma_{bc} )
               acc = 0.0_dp
               do m = 1, 3
                  acc = acc + gtinv(a,m)*0.5_dp*( dgamma(b,m,c) &
                                               + dgamma(c,m,b)   &
                                               - dgamma(m,b,c) )
               end do
               Gp(a,b,c) = e4m*acc
            end do
          end do
        end do

        ! ---- assemble  M^i = d_j S^{ij} + Gamma^i_{jk} S^{kj}
        !                              + Gamma^j_{jk} S^{ik}
        do jf = 1, 3                     ! free index i
          pieces = 0.0_dp
          do j2 = 1, 3                  ! summed index j
            pieces(1) = pieces(1) + e4m*dAbup(j2,jf,j2)
            pieces(2) = pieces(2) -(2.0_dp/3.0_dp)*e4m*Ktr*dgtinv(j2,jf,j2)
            pieces(3) = pieces(3) -(2.0_dp/3.0_dp)*e4m*gtinv(j2,jf)*dKtr(j2)
            pieces(4) = pieces(4) -4.0_dp*e4m*dphi(j2)*Abup(j2,jf)
            pieces(5) = pieces(5) +(8.0_dp/3.0_dp)*e4m*dphi(j2)*gtinv(j2,jf)*Ktr
            do m = 1, 3
              pieces(6) = pieces(6) + Gp(jf,j2,m)*Smat(m,j2)
              pieces(7) = pieces(7) + Gp(j2,j2,m)*Smat(jf,m)
            end do
          end do
          Mout(i,k,jf) = sum(pieces)
          if (present(Mterms)) Mterms(i,k,jf,:) = pieces
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
