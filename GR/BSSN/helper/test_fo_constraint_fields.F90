program test_fo_constraint_fields
  use kinds_mod, only: dp
  use vars_mod, only: NVARS, IGXX, IGYY, IGZZ, IAXX, IGTX
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  use fo_constraint_fields_mod, only: fo_gamma_constraint, fo_momentum_constraint_principal, &
                                      fo_momentum_constraint_full, &
                                      fo_constraint_principal_matrix, fo_constraint_characteristics
  implicit none
  real(dp) :: q(NFO_COMPLETE_VARS), cg(3), cm(3), nm(6,6), cp(6,6), speeds(6), tr(6,6)
  logical :: complete

  q = 0.0_dp
  q(IGXX) = 1.0_dp
  q(IGYY) = 1.0_dp
  q(IGZZ) = 1.0_dp
  q(fo_complete_index(IGXX,1)) = 0.1_dp
  q(IGTX) = 0.1_dp
  call fo_gamma_constraint(q, cg)
  if (maxval(abs(cg)) > 1.0e-14_dp) error stop 'Gamma constraint cancellation failed'

  q = 0.0_dp
  q(IGXX) = 1.0_dp
  q(IGYY) = 1.0_dp
  q(IGZZ) = 1.0_dp
  q(fo_complete_index(IAXX,1)) = 0.2_dp
  call fo_momentum_constraint_principal(q, cm)
  if (abs(cm(1)-0.2_dp) > 1.0e-14_dp) error stop 'momentum constraint field failed'
  if (maxval(abs(cm(2:3))) > 1.0e-14_dp) error stop 'momentum constraint leakage'
  call fo_momentum_constraint_full(q, cm)
  if (abs(cm(1)-0.2_dp) > 1.0e-14_dp) error stop 'full momentum constraint field failed'
  if (maxval(abs(cm(2:3))) > 1.0e-14_dp) error stop 'full momentum constraint leakage'

  call fo_constraint_principal_matrix(0.2_dp, nm, cp)
  call fo_constraint_characteristics(0.2_dp, speeds, tr, complete)
  if (maxval(abs(speeds+0.2_dp)) > 1.0e-14_dp) error stop 'constraint speed mismatch'
  if (complete) error stop 'curved constraint block marked complete'
  if (abs(tr(1,1)-1.0_dp) > 1.0e-14_dp) error stop 'constraint transform mismatch'
  write(*,'(A)') 'explicit FO constraint combinations: PASS'
end program test_fo_constraint_fields
