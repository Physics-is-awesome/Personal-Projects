module projection_matrix_1d
  use mesh_1d
  implicit none
  real(8), allocatable :: D(:,:)
  public :: initialize_projection_matrix, apply_weak_derivative

contains

  subroutine initialize_projection_matrix()
    integer :: i
    real(8) :: dphi_dx
    if (allocated(D)) then
      deallocate(D)
    end if

    allocate(D(N, N))
    D = 0.0d0

    dphi_dx = 1.0d0 / dx

    ! Loop over elements and assemble D
    do i = 2, N-1
      ! Local contributions from basis functions over each element
      D(i, i-1) = -dphi_dx / 2.0d0
      D(i, i  ) =  0.0d0
      D(i, i+1) =  dphi_dx / 2.0d0
    end do

    ! Optional: Neumann (zero derivative) at boundaries
    D(1,:) = 0.0d0
    D(N,:) = 0.0d0
  end subroutine initialize_projection_matrix

  ! Applies D to f to compute (phi_i, ∂x f)
  subroutine apply_weak_derivative(f, d_proj)
    implicit none
    real(8), intent(in)  :: f(N)
    real(8), intent(out) :: d_proj(N)
    integer :: i

    d_proj = 0.0d0
    do i = 1, N
      d_proj(i) = sum(D(i,:) * f(:))
    end do
  end subroutine apply_weak_derivative

end module projection_matrix_1d
