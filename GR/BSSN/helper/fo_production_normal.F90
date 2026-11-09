! Normal second-derivative principal matrix of the production BSSN/gauge RHS.
!
! This module deliberately covers only the highest normal derivative carried
! by each production equation.  It is *not* a first-order characteristic
! transform and it is not used by an excision penalty.  The remaining
! first-normal-derivative (transport, Lie, Ricci/connection) couplings and the
! constraint block are still open; see `LONG_TERM_CONTINUATION_PLAN.md` P2.2.
!
! Convention used here and in the manufactured test:
!
!     delta( RHS_v ) = ... + C2(v,w) * d_n^2 delta(u_w) + ...
!
! so C2 is the coefficient of the second normal derivative, not a matrix in a
! first-order reduction.  For a plane-wave perturbation
! delta(u_w) ~ exp(i k n.x), the response is
!
!     delta(RHS_v) = C0 - k^2 C2(v,w) delta(u_w) + ...
!
! and the test extracts C2 by differencing two long-wavelength cosine
! perturbations.  The analytic entries below follow directly from
! `rhs_point()` in `integrators/bssn_rhs.F90`:
!
!   K:      -e^{-4 phi} gtilde^{ij} D_i D_j alpha
!   Atilde: e^{-4 phi} XTF, X = -D_i D_j alpha + alpha (Rtilde + Rphi)
!   Gamma:  gtilde^{bc} d_b d_c beta^a
!           + 1/3 gtilde^{ab} d_b d_c beta^c
!   B:      same second-order shift block for gamma_driver_mode 0/3/4
!
! `D_i D_j alpha` contributes n_i n_j alpha_nn, `Rphi` contributes the
! conformal-factor second derivative, and `Rtilde` contributes
! -1/2 gtilde^{nn} gtilde_ij,nn.  The trace-free projection is included
! exactly.  No transverse (Cartoon-y) normal is assumed; the caller supplies
! the physical unit normal in the x-z symmetry plane.
module fo_production_normal_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IK, IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, &
                      IGTX, IGTY, IGTZ, IALPHA, IBETAX, IBETAY, IBETAZ, &
                      IBX, IBY, IBZ
  use params_mod, only: gamma_driver_mode
  implicit none
  private
  public :: production_normal_second_matrix, production_normal_second_labels

contains

  subroutine production_normal_second_matrix(q, normal, C2, labels, supported)
    real(dp), intent(in) :: q(NVARS), normal(3)
    real(dp), intent(out) :: C2(NVARS,NVARS)
    character(len=32), intent(out) :: labels(NVARS)
    logical, intent(out) :: supported(NVARS)
    real(dp) :: g(3,3), gi(3,3), det, n(3), nrm, gnn, e4m, alpha
    integer :: i, j, k, l, v, w, dm

    nrm = sqrt(sum(normal*normal))
    if (nrm <= tiny(1.0_dp)) error stop 'production normal matrix: zero normal'
    n = normal/nrm

    g = reshape([q(IGXX), q(IGXY), q(IGXZ), &
                 q(IGXY), q(IGYY), q(IGYZ), &
                 q(IGXZ), q(IGYZ), q(IGZZ)], [3,3])
    call inverse_metric(g, gi, det)
    if (det <= 0.0_dp) error stop 'production normal matrix: non-positive metric'
    alpha = q(IALPHA)
    if (alpha <= 0.0_dp) error stop 'production normal matrix: non-positive lapse'

    gnn = sum(gi * spread(n,1,3) * spread(n,2,3))
    e4m = exp(-4.0_dp*q(IPHI))

    C2 = 0.0_dp
    labels = ''
    supported = .false.

    ! K_t = -e^{-4 phi} gtilde^{ij} D_i D_j alpha + ...
    C2(IK, IALPHA) = -e4m*gnn
    labels(IK) = 'K<-alpha_nn'
    supported(IK) = .true.

    ! Atilde_ij,t = e^{-4 phi} XTF_ij + ...
    do i = 1, 3
      do j = i, 3
        v = a_index(i,j)
        C2(v,IALPHA) = e4m*(-n(i)*n(j) + g(i,j)*gnn/3.0_dp)
        C2(v,IPHI) = e4m*alpha*(-2.0_dp*n(i)*n(j) + &
                                 (2.0_dp/3.0_dp)*g(i,j)*gnn)
        do k = 1, 3
          do l = k, 3
            w = metric_index(k,l)
            ! trace_X = sum(gtinv * X) counts both (a,b) and (b,a)
            ! for an off-diagonal conformal-metric component.
            C2(v,w) = e4m*alpha*( &
                 -0.5_dp*gnn*delta_ij_kl(i,j,k,l) + &
                 (1.0_dp/6.0_dp)*gnn*g(i,j)*metric_multiplicity(k,l)*gi(k,l))
          end do
        end do
        labels(v) = 'Atilde<-alpha_nn,phi_nn,gt_nn'
        supported(v) = .true.
      end do
    end do

    ! Gamma_t and the non-advective driver variable.  In mode 4
    ! B_t = Gamma_t - eta B, so the second-order shift block is identical.
    ! Modes 0 and 3 also retain it.  Modes 1 and 2 do not.
    dm = gamma_driver_mode
    do i = 1, 3
      v = gamma_index(i)
      do k = 1, 3
        w = beta_index(k)
        C2(v,w) = gnn*delta3(i,k) + &
                  (1.0_dp/3.0_dp)*sum(gi(i,:)*n(:))*n(k)
        labels(v) = 'Gamma<-beta_nn'
        supported(v) = .true.
        if (dm == 0 .or. dm == 3 .or. dm == 4) then
          C2(b_index(i),w) = C2(v,w)
          labels(b_index(i)) = 'B<-beta_nn'
          supported(b_index(i)) = .true.
        end if
      end do
    end do

    ! The remaining equations carry no second normal derivative.
    do v = 1, NVARS
      if (.not. supported(v)) then
        labels(v) = 'no_second_normal_derivative'
        supported(v) = .true.
      end if
    end do
  end subroutine production_normal_second_matrix

  subroutine production_normal_second_labels(labels)
    character(len=32), intent(out) :: labels(NVARS)
    integer :: v
    do v = 1, NVARS
      labels(v) = trim(var_name(v))
    end do
  end subroutine production_normal_second_labels

  pure real(dp) function delta_ij_kl(i,j,k,l) result(d)
    integer, intent(in) :: i,j,k,l
    d = 0.0_dp
    if (i == k .and. j == l) d = 1.0_dp
    if (i == l .and. j == k) d = 1.0_dp
  end function delta_ij_kl

  pure integer function metric_multiplicity(k,l) result(m)
    integer, intent(in) :: k,l
    if (k == l) then
      m = 1
    else
      m = 2
    end if
  end function metric_multiplicity

  pure integer function delta3(i,k) result(d)
    integer, intent(in) :: i,k
    if (i == k) then
      d = 1
    else
      d = 0
    end if
  end function delta3

  subroutine inverse_metric(g, gi, det)
    real(dp), intent(in) :: g(3,3)
    real(dp), intent(out) :: gi(3,3), det
    det = g(1,1)*(g(2,2)*g(3,3)-g(2,3)*g(3,2)) &
        - g(1,2)*(g(1,2)*g(3,3)-g(1,3)*g(2,3)) &
        + g(1,3)*(g(1,2)*g(2,3)-g(2,2)*g(1,3))
    if (det <= 0.0_dp) return
    gi(1,1) = (g(2,2)*g(3,3)-g(2,3)*g(3,2))/det
    gi(1,2) = (g(1,3)*g(2,3)-g(1,2)*g(3,3))/det
    gi(1,3) = (g(1,2)*g(2,3)-g(1,3)*g(2,2))/det
    gi(2,1) = gi(1,2)
    gi(2,2) = (g(1,1)*g(3,3)-g(1,3)*g(3,1))/det
    gi(2,3) = (g(1,2)*g(1,3)-g(1,1)*g(2,3))/det
    gi(3,1) = gi(1,3)
    gi(3,2) = gi(2,3)
    gi(3,3) = (g(1,1)*g(2,2)-g(1,2)*g(2,1))/det
  end subroutine inverse_metric

  pure integer function metric_index(i,j) result(v)
    integer, intent(in) :: i,j
    integer, parameter :: tab(3,3) = reshape([IGXX,IGXY,IGXZ, &
                                               IGXY,IGYY,IGYZ, &
                                               IGXZ,IGYZ,IGZZ], [3,3])
    v = tab(i,j)
  end function metric_index

  pure integer function a_index(i,j) result(v)
    integer, intent(in) :: i,j
    integer, parameter :: tab(3,3) = reshape([IAXX,IAXY,IAXZ, &
                                               IAXY,IAYY,IAYZ, &
                                               IAXZ,IAYZ,IAZZ], [3,3])
    v = tab(i,j)
  end function a_index

  pure integer function gamma_index(i) result(v)
    integer, intent(in) :: i
    integer, parameter :: tab(3) = [IGTX, IGTY, IGTZ]
    v = tab(i)
  end function gamma_index

  pure integer function beta_index(i) result(v)
    integer, intent(in) :: i
    integer, parameter :: tab(3) = [IBETAX, IBETAY, IBETAZ]
    v = tab(i)
  end function beta_index

  pure integer function b_index(i) result(v)
    integer, intent(in) :: i
    integer, parameter :: tab(3) = [IBX, IBY, IBZ]
    v = tab(i)
  end function b_index

  pure function var_name(v) result(name)
    integer, intent(in) :: v
    character(len=32) :: name
    select case (v)
    case (IPHI);   name = 'phi'
    case (IGXX);   name = 'gt_xx'
    case (IGXY);   name = 'gt_xy'
    case (IGXZ);   name = 'gt_xz'
    case (IGYY);   name = 'gt_yy'
    case (IGYZ);   name = 'gt_yz'
    case (IGZZ);   name = 'gt_zz'
    case (IK);     name = 'K'
    case (IAXX);   name = 'A_xx'
    case (IAXY);   name = 'A_xy'
    case (IAXZ);   name = 'A_xz'
    case (IAYY);   name = 'A_yy'
    case (IAYZ);   name = 'A_yz'
    case (IAZZ);   name = 'A_zz'
    case (IGTX);   name = 'Gamma_x'
    case (IGTY);   name = 'Gamma_y'
    case (IGTZ);   name = 'Gamma_z'
    case (IALPHA); name = 'alpha'
    case (IBETAX); name = 'beta_x'
    case (IBETAY); name = 'beta_y'
    case (IBETAZ); name = 'beta_z'
    case (IBX);    name = 'B_x'
    case (IBY);    name = 'B_y'
    case (IBZ);    name = 'B_z'
    case default;  name = 'unknown'
    end select
  end function var_name

end module fo_production_normal_mod
