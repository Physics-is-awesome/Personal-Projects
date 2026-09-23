module kinds_mod
  implicit none
  private
  public :: dp

  integer, parameter :: dp = selected_real_kind(15, 307)

end module kinds_mod
