!===============================================================================
! probe_momentum_terms -- identify the momentum constraint empirically.
!
! On EXACT Kerr-Schild data every constraint vanishes, so each candidate piece
! is printed and whichever combination tends to zero under refinement is the
! constraint.  Pieces:
!
!   DIV = nabla_k[ gamma^{kj} (K_ij - gamma_ij K) ]      (unambiguous)
!   C1  = gamma^{kj} Abar_ij K
!   C2  = gamma^{kj} (Gammabar_j) gamma_ii
!   C3  = gamma^{kj} Gammabar_jk K_ij
!
! with gamma = e^{4phi} gtilde (so det gamma = e^{12phi} is NOT constant, which
! is why the algebra terms do not simply drop out under the code's
! det(gtilde)=1 convention).
!
! Usage: probe_momentum_terms [parfile] [rin] [rout]
!===============================================================================
program probe_momentum_terms
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: extrapolate_excised, set_excision_background, &
                         excision_reset_geometry, excision_status
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: sym3_inverse, conformal_geometry_t, compute_conformal_geometry

  implicit none
  integer, parameter :: IA(6) = [1,1,1,2,2,3]
  integer, parameter :: IB(6) = [1,2,3,2,3,3]

  real(dp), allocatable :: u(:,:,:)
  type(stencil_t) :: S
  type(conformal_geometry_t) :: Gm
  real(dp), allocatable :: acc(:,:)
  real(dp) :: g(3,3), gi(3,3), dg(3,3,3), dgi(3,3,3)
  real(dp) :: Gam(3,3,3), Kij(3,3), dK(3,3,3), Ab(3,3)
  real(dp) :: dgt(3,3,3), dphi(3), e4, Ktr, gt(3,3), gtinv(3,3), det
  real(dp) :: t1(NVARS), t2(NVARS), t3(NVARS), tx(NVARS)
  real(dp) :: W(3), div(3), c1(3), c2(3), c3(3), q, gbar_j
  real(dp) :: rin, rout, rr, npts, ssum(4)
  character(len=256) :: par_file, buf
  real(dp) :: best(4), coarse(4)
  integer :: i, k, j, a, b, c, d, dir, iv, ir, nx0, nz0

  par_file = 'parameter_files/vacuum_kerr.par'; rin = 3.0_dp; rout = 5.0_dp
  buf = ''
  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) par_file = buf
  buf = ''
  call get_command_argument(2, buf); if (len_trim(buf) > 0) read(buf,*) rin
  call get_command_argument(3, buf); if (len_trim(buf) > 0) read(buf,*) rout
  call init_params(trim(par_file)); nx0 = Nx; nz0 = Nz

  allocate(acc(4,3))
  write(*,'(A)') '=== exact Kerr-Schild: momentum-term probe (RMS over annulus) ==='
  do ir = 1, 2
    call init_params(trim(par_file))
    if (ir == 2) then
      Nx = 2*nx0; Nz = 2*nz0
      dx = xmax/real(Nx-1,dp); dz = 2.0_dp*zmax/real(Nz-1,dp); dy = dx
    end if
    if (allocated(xg)) deallocate(xg)
    if (allocated(zg)) deallocate(zg)
    call setup_grid()
    if (allocated(u)) deallocate(u)
    call alloc_state(u, Nx, Nz)
    call set_kerr_schild_id(u)
    call compute_conformal_connections(u)
    call set_excision_background(u)
    call init_deriv_stencils()
    call excision_reset_geometry()
    call extrapolate_excised(u)

    acc = 0.0_dp; npts = 0.0_dp
    do k = 3, Nz-2
      do i = 2, Nx-1
        rr = sqrt(xg(i)**2 + zg(k)**2)
        if (rr < rin .or. rr > rout) cycle

        call compute_stencil(u, i, k, excision_status, S)
        call compute_conformal_geometry(S, Gm)
        gt = Gm%gt
        call sym3_inverse(gt, gtinv, det)
        Ktr = u(i,k,IK)
        e4  = exp(4.0_dp*S%val(IPHI))
        t1 = S%d1x; t2 = S%d1y; t3 = S%d1z
        dphi = [t1(IPHI), t2(IPHI), t3(IPHI)]

        dgt = 0.0_dp
        do iv = 1, 6
          a = IA(iv); b = IB(iv)
          dgt(1,a,b) = t1(ICONFMET(iv)); dgt(1,b,a) = t1(ICONFMET(iv))
          dgt(2,a,b) = t2(ICONFMET(iv)); dgt(2,b,a) = t2(ICONFMET(iv))
          dgt(3,a,b) = t3(ICONFMET(iv)); dgt(3,b,a) = t3(ICONFMET(iv))
        end do

        g = e4*gt
        gi = gtinv/e4
        dg = 0.0_dp; dgi = 0.0_dp
        do dir = 1, 3
          do a = 1, 3
            do b = 1, 3
              dg(dir,a,b)  = 4.0_dp*dphi(dir)*g(a,b) + e4*dgt(dir,a,b)
              dgi(dir,a,b) = ( dgtinv_local(gtinv, dgt, dir, a, b) )/e4 &
                             - 4.0_dp*dphi(dir)*gtinv(a,b)
            end do
          end do
        end do

        Gam = 0.0_dp
        do a = 1, 3
          do b = 1, 3
            do c = 1, 3
              q = 0.0_dp
              do d = 1, 3
                q = q + gi(a,d)*(dg(b,c,d) + dg(c,b,d) - dg(d,b,c))
              end do
              Gam(a,b,c) = 0.5_dp*q
            end do
          end do
        end do

        Kij = 0.0_dp
        do iv = 1, 6
          a = IA(iv); b = IB(iv)
          Kij(a,b) = e4*(S%val(IATILDE(iv)) + gt(a,b)*Ktr/3.0_dp)
          Kij(b,a) = Kij(a,b)
        end do
        Ab = 0.0_dp
        do a = 1, 3
          do b = 1, 3
            Ab(a,b) = Kij(a,b) - 0.5_dp*g(a,b)*Ktr
          end do
        end do

        dK = 0.0_dp
        do dir = 1, 3
          select case (dir)
          case (1); tx = t1
          case (2); tx = t2
          case (3); tx = t3
          end select
          do iv = 1, 6
            a = IA(iv); b = IB(iv)
            dK(dir,a,b) = e4*(4.0_dp*dphi(dir)*(S%val(IATILDE(iv)) + gt(a,b)*Ktr/3.0_dp) &
                       + tx(IATILDE(iv)) + dgt(dir,a,b)*Ktr/3.0_dp)
            dK(dir,b,a) = dK(dir,a,b)
          end do
        end do

        do j = 1, 3
          do a = 1, 3
            W(a) = 0.0_dp
            do b = 1, 3
              W(a) = W(a) + gi(a,b)*(Kij(j,b) - g(j,b)*Ktr)
            end do
          end do
          div(j) = 0.0_dp
          do dir = 1, 3
            do a = 1, 3
              div(j) = div(j) + dgi(dir,a,j)*(Kij(j,a) - g(j,a)*Ktr) &
                             + gi(a,j)*(dK(dir,j,a) - dg(dir,j,a)*Ktr)
            end do
            do a = 1, 3
              do b = 1, 3
                div(j) = div(j) + Gam(a,a,b)*W(b)
                div(j) = div(j) + Gam(b,a,b)*gi(b,j)*(Kij(j,a) - g(j,a)*Ktr)
              end do
            end do
          end do
          c1(j) = 0.0_dp
          do a = 1, 3
            c1(j) = c1(j) + gi(a,j)*Ab(j,a)*Ktr
          end do
          c2(j) = 0.0_dp
          do a = 1, 3
            do b = 1, 3
              gbar_j = sum(g(b,:)*Gam(:,b,b))
              c2(j) = c2(j) + gi(a,b)*gbar_j*g(j,a)
            end do
          end do
          c3(j) = 0.0_dp
          do a = 1, 3
            do b = 1, 3
              c3(j) = c3(j) + gi(a,b)*Gam(b,b,a)*Kij(j,a)
            end do
          end do
          acc(1,j) = acc(1,j) + div(j)**2
          acc(2,j) = acc(2,j) + c1(j)**2
          acc(3,j) = acc(3,j) + c2(j)**2
          acc(4,j) = acc(4,j) + c3(j)**2
        end do
        npts = npts + 1.0_dp
      end do
    end do

    do a = 1, 4
      ssum(a) = sum(acc(a,:))/(3.0_dp*max(npts,1.0_dp))
      if (ir == 1) then
        coarse(a) = sqrt(ssum(a))
      else
        best(a) = sqrt(ssum(a))
      end if
    end do
    write(*,'(A,I5,A,F8.4,A,4(1X,ES10.3))') ' Nx=', Nx, '  dx=', dx, &
         '   RMS DIV,C1,C2,C3 =', coarse
    if (ir == 2) then
      write(*,'(A,4(1X,ES10.3))') ' refined            ', best
    end if
    deallocate(u)
  end do

contains

  function dgtinv_local(gi0, dgt, dir, a, b) result(val)
    real(dp), intent(in) :: gi0(3,3), dgt(3,3,3)
    integer, intent(in) :: dir, a, b
    real(dp) :: val, q
    integer :: c, d
    q = 0.0_dp
    do c = 1, 3
      do d = 1, 3
        q = q + gi0(a,c)*gi0(b,d)*dgt(dir,c,d)
      end do
    end do
    val = -q
  end function dgtinv_local

end program probe_momentum_terms