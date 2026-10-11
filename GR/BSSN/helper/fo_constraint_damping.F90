module fo_constraint_damping_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: damp_reduction_constraints

contains

  subroutine damp_reduction_constraints(c, kappa, rhs)
    real(dp), intent(in) :: c(:), kappa
    real(dp), intent(inout) :: rhs(:)
    if (size(c) /= size(rhs)) error stop 'damp_reduction_constraints: inconsistent state'
    if (kappa < 0.0_dp) error stop 'damp_reduction_constraints: negative damping'
    rhs = rhs - kappa*c
  end subroutine damp_reduction_constraints

end module fo_constraint_damping_mod
