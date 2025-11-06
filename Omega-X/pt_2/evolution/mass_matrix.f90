module mass_matrix
  use quad
  use basis

contains 

  subroutine compute_mass_matrix()
    real(8), allocatable :: M(:, :) ! update to be dynamically 
    integer :: i, j, q
    allocate(M(3, 3))

    do i = 1, 3
      do j = 1, 3
        do q = 1, 3
          M(i,j) = M(i,j) + w_q(q) * phi(i,q) * phi(j,q)
        end do
      end do
    end do
  end subroutine compute_mass_matrix
  
end module mass_matrix

