program test_fo_complete_layout
  use fo_complete_vars_mod
  use vars_mod, only: NVARS
  implicit none
  integer :: v, d, idx
  logical :: seen(NFO_COMPLETE_VARS)

  if (NFO_COMPLETE_VARS /= NVARS + 3*NVARS) error stop 'bad complete FO size'
  seen = .false.
  do d = 1, 3
    do v = 1, NVARS
      idx = fo_complete_index(v,d)
      if (idx <= NVARS .or. idx > NFO_COMPLETE_VARS) error stop 'bad FO index'
      if (seen(idx)) error stop 'duplicate FO index'
      seen(idx) = .true.
    end do
  end do
  if (count(seen(NVARS+1:)) /= 3*NVARS) error stop 'incomplete FO index map'
  write(*,'(A,I0,A)') 'complete first-order layout: ', NFO_COMPLETE_VARS, ' variables'
end program test_fo_complete_layout
