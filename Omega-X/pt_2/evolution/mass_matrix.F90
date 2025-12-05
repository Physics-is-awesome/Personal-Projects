module mass_matrix
  implicit none
contains 

  subroutine compute_mass_matrix(Omega)
    type(Omega-X), intent(inout) :: Omega
    integer :: i, j, q


    do i = 1, Omega%m%p
      do j = 1, Omega%m%p
        do q = 1, Omega%m%p
          Omega%proj%M(i,j) = Omega%proj%M(i,j) + Omega%proj%w_q(q) * Omega%proj%phi(i,q) * Omega%proj%phi(j,q)
        end do
      end do
    end do
  end subroutine compute_mass_matrix
  
end module mass_matrix

