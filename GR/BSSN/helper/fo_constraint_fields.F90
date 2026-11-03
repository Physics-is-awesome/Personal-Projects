module fo_constraint_fields_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, IK, &
                      IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ, IGTX, IGTY, IGTZ
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  implicit none
  private
  public :: fo_gamma_constraint, fo_momentum_constraint_principal
  public :: fo_momentum_constraint_full
  public :: fo_constraint_principal_matrix, fo_constraint_characteristics

contains

  subroutine fo_constraint_principal_matrix(beta_n, normal_matrix, coupling)
    real(dp), intent(in) :: beta_n
    real(dp), intent(out) :: normal_matrix(6,6), coupling(6,6)
    normal_matrix = 0.0_dp
    coupling = 0.0_dp
    normal_matrix(1:6,1:6) = -beta_n*identity6()
    coupling(1,4) = 2.0_dp
    coupling(2,5) = 2.0_dp
    coupling(3,6) = 2.0_dp
  end subroutine fo_constraint_principal_matrix

  subroutine fo_constraint_characteristics(beta_n, speeds, transform, complete)
    real(dp), intent(in) :: beta_n
    real(dp), intent(out) :: speeds(6), transform(6,6)
    logical, intent(out) :: complete
    speeds = -beta_n
    transform = identity6()
    complete = .false.
  end subroutine fo_constraint_characteristics

  subroutine fo_gamma_constraint(q, constraint)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: constraint(3)
    real(dp) :: g(3,3), gi(3,3), dg(3,3,3), det
    integer :: i, j, k, l

    call unpack_metric(q, g)
    call inverse_metric(g, gi, det)
    if (det <= 0.0_dp) error stop 'fo_gamma_constraint: non-positive metric'
    do k = 1, 3
      do i = 1, 3
        do j = 1, 3
          dg(k,i,j) = q(fo_complete_index(metric_index(i,j), k))
        end do
      end do
    end do
    do i = 1, 3
      constraint(i) = q(gamma_index(i))
      do j = 1, 3
        do k = 1, 3
          do l = 1, 3
            constraint(i) = constraint(i) - gi(i,k)*gi(j,l)*dg(j,k,l)
          end do
        end do
      end do
    end do
  end subroutine fo_gamma_constraint

  subroutine fo_momentum_constraint_principal(q, constraint)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: constraint(3)
    real(dp) :: g(3,3), gi(3,3), a(3,3), aup(3,3), det
    real(dp) :: dg(3,3,3), da(3,3,3), dainv(3,3,3), dK(3), dphi(3)
    real(dp) :: dAup(3,3,3)
    integer :: i, j, k, l, m

    call unpack_metric(q, g)
    call inverse_metric(g, gi, det)
    if (det <= 0.0_dp) error stop 'fo_momentum_constraint: non-positive metric'
    call unpack_a(q, a)
    aup = matmul(gi, matmul(a, gi))
    do j = 1, 3
      dK(j) = q(fo_complete_index(IK,j))
      dphi(j) = q(fo_complete_index(IPHI,j))
      do k = 1, 3
        do l = 1, 3
          dg(j,k,l) = q(fo_complete_index(metric_index(k,l),j))
          da(j,k,l) = q(fo_complete_index(a_index(k,l),j))
        end do
      end do
    end do
    do j = 1, 3
      do i = 1, 3
        do k = 1, 3
          dainv(j,i,k) = 0.0_dp
          do l = 1, 3
            do m = 1, 3
              dainv(j,i,k) = dainv(j,i,k) - gi(i,l)*gi(k,m)*dg(j,l,m)
            end do
          end do
        end do
      end do
    end do
    do j = 1, 3
      do i = 1, 3
        do k = 1, 3
          dAup(j,i,k) = 0.0_dp
          do l = 1, 3
            do m = 1, 3
              dAup(j,i,k) = dAup(j,i,k) + dainv(j,i,l)*gi(k,m)*a(l,m) &
                            + gi(i,l)*dainv(j,k,m)*a(l,m) &
                            + gi(i,l)*gi(k,m)*da(j,l,m)
            end do
          end do
        end do
      end do
    end do
    do i = 1, 3
      constraint(i) = 0.0_dp
      do j = 1, 3
        constraint(i) = constraint(i) + dAup(j,i,j) - (2.0_dp/3.0_dp)*gi(i,j)*dK(j) &
                        - 4.0_dp*dphi(j)*aup(i,j)
      end do
    end do
  end subroutine fo_momentum_constraint_principal

  subroutine fo_momentum_constraint_full(q, constraint)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: constraint(3)
    real(dp) :: g(3,3), gi(3,3), a(3,3), aup(3,3), det
    real(dp) :: dg(3,3,3), da(3,3,3), dainv(3,3,3), dK(3), dphi(3)
    real(dp) :: dAup(3,3,3), dginv(3,3,3), christ(3,3,3)
    real(dp) :: qmat(3,3), smat(3,3), phi, e4m
    integer :: i, j, k, l, m

    call unpack_metric(q, g)
    call inverse_metric(g, gi, det)
    if (det <= 0.0_dp) error stop 'fo_momentum_constraint_full: non-positive metric'
    call unpack_a(q, a)
    aup = matmul(gi, matmul(a, gi))
    phi = q(IPHI)
    e4m = exp(-4.0_dp*phi)

    do j = 1, 3
      dK(j) = q(fo_complete_index(IK,j))
      dphi(j) = q(fo_complete_index(IPHI,j))
      do k = 1, 3
        do l = 1, 3
          dg(j,k,l) = q(fo_complete_index(metric_index(k,l),j))
          da(j,k,l) = q(fo_complete_index(a_index(k,l),j))
        end do
      end do
    end do

    do j = 1, 3
      do i = 1, 3
        do k = 1, 3
          dainv(j,i,k) = 0.0_dp
          do l = 1, 3
            do m = 1, 3
              dainv(j,i,k) = dainv(j,i,k) - gi(i,l)*gi(k,m)*dg(j,l,m)
            end do
          end do
          dginv(j,i,k) = dainv(j,i,k)
          dAup(j,i,k) = 0.0_dp
          do l = 1, 3
            do m = 1, 3
              dAup(j,i,k) = dAup(j,i,k) + dainv(j,i,l)*gi(k,m)*a(l,m) &
                            + gi(i,l)*dainv(j,k,m)*a(l,m) &
                            + gi(i,l)*gi(k,m)*da(j,l,m)
            end do
          end do
        end do
      end do
    end do

    do i = 1, 3
      do j = 1, 3
        qmat(i,j) = aup(i,j) - (2.0_dp/3.0_dp)*gi(i,j)*q(IK)
        smat(i,j) = e4m*qmat(i,j)
      end do
    end do

    ! Christoffel symbols of the physical metric gamma_ij=e^(4 phi) g_ij.
    do j = 1, 3
      do k = 1, 3
        do i = 1, 3
          christ(i,j,k) = 0.0_dp
          do l = 1, 3
            christ(i,j,k) = christ(i,j,k) + gi(i,l)*0.5_dp * &
              (4.0_dp*dphi(j)*g(l,k) + dg(j,l,k) + &
               4.0_dp*dphi(k)*g(l,j) + dg(k,l,j) - &
               4.0_dp*dphi(l)*g(j,k) - dg(l,j,k))
          end do
        end do
      end do
    end do

    constraint = 0.0_dp
    do i = 1, 3
      do j = 1, 3
        constraint(i) = constraint(i) + e4m*dAup(j,i,j) &
          -(2.0_dp/3.0_dp)*e4m*q(IK)*dginv(j,i,j) &
          -(2.0_dp/3.0_dp)*e4m*gi(i,j)*dK(j) &
          -4.0_dp*e4m*dphi(j)*aup(i,j) &
          +(8.0_dp/3.0_dp)*e4m*dphi(j)*gi(i,j)*q(IK)
        do m = 1, 3
          constraint(i) = constraint(i) + christ(i,j,m)*smat(m,j) &
                                     + christ(j,j,m)*smat(i,m)
        end do
      end do
    end do
  end subroutine fo_momentum_constraint_full

  integer function metric_index(i,j) result(v)
    integer, intent(in) :: i,j
    integer, parameter :: tab(3,3) = reshape([IGXX,IGXY,IGXZ,IGXY,IGYY,IGYZ, &
                                               IGXZ,IGYZ,IGZZ],[3,3])
    v = tab(i,j)
  end function metric_index

  integer function a_index(i,j) result(v)
    integer, intent(in) :: i,j
    integer, parameter :: tab(3,3) = reshape([IAXX,IAXY,IAXZ,IAXY,IAYY,IAYZ, &
                                               IAXZ,IAYZ,IAZZ],[3,3])
    v = tab(i,j)
  end function a_index

  integer function gamma_index(i) result(v)
    integer, intent(in) :: i
    integer, parameter :: tab(3) = [IGTX,IGTY,IGTZ]
    v = tab(i)
  end function gamma_index

  subroutine unpack_metric(q, g)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: g(3,3)
    g = reshape([q(IGXX),q(IGXY),q(IGXZ),q(IGXY),q(IGYY),q(IGYZ), &
                 q(IGXZ),q(IGYZ),q(IGZZ)],[3,3])
  end subroutine unpack_metric

  subroutine unpack_a(q, a)
    real(dp), intent(in) :: q(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: a(3,3)
    a = reshape([q(IAXX),q(IAXY),q(IAXZ),q(IAXY),q(IAYY),q(IAYZ), &
                 q(IAXZ),q(IAYZ),q(IAZZ)],[3,3])
  end subroutine unpack_a

  subroutine inverse_metric(g, gi, det)
    real(dp), intent(in) :: g(3,3)
    real(dp), intent(out) :: gi(3,3), det
    det = g(1,1)*(g(2,2)*g(3,3)-g(2,3)*g(2,3)) &
        - g(1,2)*(g(1,2)*g(3,3)-g(2,3)*g(1,3)) &
        + g(1,3)*(g(1,2)*g(2,3)-g(2,2)*g(1,3))
    gi(1,1)=(g(2,2)*g(3,3)-g(2,3)*g(2,3))/det
    gi(1,2)=(g(1,3)*g(2,3)-g(1,2)*g(3,3))/det
    gi(1,3)=(g(1,2)*g(2,3)-g(1,3)*g(2,2))/det
    gi(2,1)=gi(1,2); gi(2,2)=(g(1,1)*g(3,3)-g(1,3)*g(1,3))/det
    gi(2,3)=(g(1,2)*g(1,3)-g(1,1)*g(2,3))/det
    gi(3,1)=gi(1,3); gi(3,2)=gi(2,3); gi(3,3)=(g(1,1)*g(2,2)-g(1,2)*g(1,2))/det
  end subroutine inverse_metric

  pure function identity6() result(matrix)
    real(dp) :: matrix(6,6)
    integer :: i
    matrix = 0.0_dp
    do i = 1, 6
      matrix(i,i) = 1.0_dp
    end do
  end function identity6

end module fo_constraint_fields_mod
