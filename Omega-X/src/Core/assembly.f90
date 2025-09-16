module assembly
    use mesh
    implicit none

    real, dimension(nx, nx) :: M      ! Mass matrix
    real, dimension(nx, nx) :: K      ! Stiffness matrix

contains

!=======================================================
! Initialize the global mass and stiffness matrices
! Using standard linear FEM on uniform grid
!=======================================================
subroutine assemble_matrices()
    integer :: e, i_local, j_local
    integer :: i_global, j_global
    real :: h, m_e(2,2), k_e(2,2)

    ! Zero matrices
    M = 0.0
    K = 0.0

    ! Loop over elements
    do e = 1, ne
        h = dx(e)

        ! Element mass matrix (2x2)
        m_e(1,1) = h / 3.0
        m_e(1,2) = h / 6.0
        m_e(2,1) = h / 6.0
        m_e(2,2) = h / 3.0

        ! Element stiffness matrix (2x2)
        k_e(1,1) =  1.0 / h
        k_e(1,2) = -1.0 / h
        k_e(2,1) = -1.0 / h
        k_e(2,2) =  1.0 / h

        ! Global assembly
        do i_local = 1, 2
            i_global = conn(i_local, e)
            do j_local = 1, 2
                j_global = conn(j_local, e)
                M(i_global, j_global) = M(i_global, j_global) + m_e(i_local, j_local)
                K(i_global, j_global) = K(i_global, j_global) + k_e(i_local, j_local)
            end do
        end do
    end do
end subroutine assemble_matrices


!=======================================================
! Matrix-vector multiplication: y = M x
!=======================================================
subroutine apply_mass_matrix(x, y)
    real, intent(in)  :: x(nx)
    real, intent(out) :: y(nx)
    integer :: i, j

    do i = 1, nx
        y(i) = 0.0
        do j = 1, nx
            y(i) = y(i) + M(i,j) * x(j)
        end do
    end do
end subroutine apply_mass_matrix


!=======================================================
! Matrix-vector multiplication: y = K x
!=======================================================
subroutine apply_stiffness_matrix(x, y)
    real, intent(in)  :: x(nx)
    real, intent(out) :: y(nx)
    integer :: i, j

    do i = 1, nx
        y(i) = 0.0
        do j = 1, nx
            y(i) = y(i) + K(i,j) * x(j)
        end do
    end do
end subroutine apply_stiffness_matrix

end module assembly
