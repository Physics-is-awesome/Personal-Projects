module reference_element_mod
  !
  ! Reference element and DG basis setup for a tetrahedron.
  !
  ! Defines:
  !   - Reference tetrahedron vertices and barycentric coordinates
  !   - Local basis function ordering
  !   - Quadrature point generation on the reference tetrahedron
  !
  ! The reference tetrahedron has vertices at:
  !   ξ1 = (0, 0, 0)
  !   ξ2 = (1, 0, 0)
  !   ξ3 = (0, 1, 0)
  !   ξ4 = (0, 0, 1)
  !
  ! A general point ξ ∈ [0,1]³ is in the reference tetrahedron if:
  !   ξ1 + ξ2 + ξ3 ≤ 1, with ξ1, ξ2, ξ3 ≥ 0
  !
  use iso_fortran_env, only: real64
  implicit none
  private

  integer, parameter, public :: dp = real64

  ! Reference tetrahedron vertices
  integer, parameter, public :: NVERT_REF = 4
  real(dp), parameter, public :: xi_ref(NVERT_REF, 3) = reshape([ &
       0.0_dp, 0.0_dp, 0.0_dp, &
       1.0_dp, 0.0_dp, 0.0_dp, &
       0.0_dp, 1.0_dp, 0.0_dp, &
       0.0_dp, 0.0_dp, 1.0_dp &
       ], shape=[NVERT_REF, 3])

  ! Barycentric coordinate index mapping
  ! λ1 = 1 - ξ1 - ξ2 - ξ3
  ! λ2 = ξ1
  ! λ3 = ξ2
  ! λ4 = ξ3

  public :: quadrature_ref_tet

contains

  subroutine quadrature_ref_tet(order, nqp, qp, w)
    !
    ! Generate quadrature points and weights on the reference tetrahedron.
    !
    ! Uses a standard Gaussian quadrature on tetrahedra.
    ! For a given quadrature order, returns the number of quadrature points (nqp),
    ! their coordinates (qp), and weights (w).
    !
    ! order: desired quadrature order (1, 2, 3, 4, etc.)
    ! nqp: number of quadrature points (output)
    ! qp(i,:): i-th quadrature point in reference coordinates ξ1, ξ2, ξ3
    ! w(i): i-th quadrature weight
    !
    integer, intent(in) :: order
    integer, intent(out) :: nqp
    real(dp), allocatable, intent(out) :: qp(:,:), w(:)

    select case(order)
    case(1)
       ! Order 1 (single point at centroid, weight = 1/6 * 6 = 1)
       nqp = 1
       allocate(qp(nqp, 3), w(nqp))
       qp(1, :) = [0.25_dp, 0.25_dp, 0.25_dp]  ! centroid
       w(1) = 1.0_dp

    case(2)
       ! Order 2 (4 points, vertices of smaller tetrahedra)
       nqp = 4
       allocate(qp(nqp, 3), w(nqp))
       w(:) = 0.25_dp
       qp(1, :) = [0.1381966011250105_dp, 0.1381966011250105_dp, 0.1381966011250105_dp]
       qp(2, :) = [0.5854101966249685_dp, 0.1381966011250105_dp, 0.1381966011250105_dp]
       qp(3, :) = [0.1381966011250105_dp, 0.5854101966249685_dp, 0.1381966011250105_dp]
       qp(4, :) = [0.1381966011250105_dp, 0.1381966011250105_dp, 0.5854101966249685_dp]

    case(3)
       ! Order 3 (5 points: 1 centroid + 4 edge midpoints, weighted)
       nqp = 5
       allocate(qp(nqp, 3), w(nqp))
       w(1) = -0.8_dp  ! centroid weight
       qp(1, :) = [0.25_dp, 0.25_dp, 0.25_dp]
       w(2:5) = 0.45_dp  ! corner weights
       qp(2, :) = [0.05_dp, 0.05_dp, 0.05_dp]
       qp(3, :) = [0.6_dp, 0.05_dp, 0.05_dp]
       qp(4, :) = [0.05_dp, 0.6_dp, 0.05_dp]
       qp(5, :) = [0.05_dp, 0.05_dp, 0.6_dp]

    case(4)
       ! Order 4 (14 points)
       nqp = 14
       allocate(qp(nqp, 3), w(nqp))

       ! Centroid
       qp(1, :) = [0.25_dp, 0.25_dp, 0.25_dp]
       w(1) = 0.1493508771999374_dp

       ! Face quadrature points (6 points on faces)
       qp(2, :) = [0.09197107805272303_dp, 0.09197107805272303_dp, 0.09197107805272303_dp]
       w(2) = 0.1130412779766693_dp
       qp(3, :) = [0.724069164671727_dp, 0.09197107805272303_dp, 0.09197107805272303_dp]
       w(3) = 0.1130412779766693_dp
       qp(4, :) = [0.09197107805272303_dp, 0.724069164671727_dp, 0.09197107805272303_dp]
       w(4) = 0.1130412779766693_dp
       qp(5, :) = [0.09197107805272303_dp, 0.09197107805272303_dp, 0.724069164671727_dp]
       w(5) = 0.1130412779766693_dp

       ! Edge quadrature points (8 points)
       qp(6, :) = [0.31088591926330060_dp, 0.31088591926330060_dp, 0.06622816653498766_dp]
       w(6) = 0.04260926956232624_dp
       qp(7, :) = [0.31088591926330060_dp, 0.06622816653498766_dp, 0.31088591926330060_dp]
       w(7) = 0.04260926956232624_dp
       qp(8, :) = [0.06622816653498766_dp, 0.31088591926330060_dp, 0.31088591926330060_dp]
       w(8) = 0.04260926956232624_dp
       qp(9, :) = [0.63284273652532850_dp, 0.12357922424529850_dp, 0.12357922424529850_dp]
       w(9) = 0.04260926956232624_dp
       qp(10, :) = [0.12357922424529850_dp, 0.63284273652532850_dp, 0.12357922424529850_dp]
       w(10) = 0.04260926956232624_dp
       qp(11, :) = [0.12357922424529850_dp, 0.12357922424529850_dp, 0.63284273652532850_dp]
       w(11) = 0.04260926956232624_dp
       qp(12, :) = [0.31088591926330060_dp, 0.31088591926330060_dp, 0.31088591926330060_dp]
       w(12) = 0.1149208223405762_dp
       qp(13, :) = [0.09197107805272303_dp, 0.09197107805272303_dp, 0.09197107805272303_dp]
       w(13) = 0.08846889119088234_dp
       qp(14, :) = [0.724069164671727_dp, 0.09197107805272303_dp, 0.09197107805272303_dp]
       w(14) = 0.08846889119088234_dp

    case default
       print *, 'Error: Unsupported quadrature order ', order
       stop
    end select

  end subroutine quadrature_ref_tet

  function barycentric(xi) result(lambda)
    !
    ! Compute barycentric coordinates for a point ξ in the reference tetrahedron.
    !
    ! Returns λ1, λ2, λ3, λ4 such that:
    !   λ1 + λ2 + λ3 + λ4 = 1
    !   λi ≥ 0 for a point inside the tetrahedron
    !
    ! The mapping is:
    !   λ1 = 1 - ξ1 - ξ2 - ξ3
    !   λ2 = ξ1
    !   λ3 = ξ2
    !   λ4 = ξ3
    !
    real(dp), intent(in) :: xi(3)
    real(dp) :: lambda(4)

    lambda(1) = 1.0_dp - xi(1) - xi(2) - xi(3)
    lambda(2) = xi(1)
    lambda(3) = xi(2)
    lambda(4) = xi(3)

  end function barycentric

  logical function point_in_ref_tet(xi)
    !
    ! Check if a point ξ is in the reference tetrahedron.
    !
    real(dp), intent(in) :: xi(3)
    real(dp) :: lambda(4)

    lambda = barycentric(xi)
    point_in_ref_tet = all(lambda >= -1.0d-14) .and. (sum(lambda) >= 1.0_dp - 1.0d-14)

  end function point_in_ref_tet

end module reference_element_mod
