! Report semidiscrete residuals of exact Kerr-Schild data with Kerr-Schild
! lapse and shift frozen. This is diagnostic, not an acceptance test: finite
! difference residuals are expected to be nonzero. It separates inner-ring
! behavior from the nearby bulk and reports every momentum component.
program diagnose_stationary_ks_rhs
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, bh_spin, slicing_mode, gamma_driver_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  use constraints_mod, only: hamiltonian_constraint, momentum_constraint
  implicit none
  real(dp), allocatable :: u(:,:,:), rhs(:,:,:)
  real(dp), allocatable :: H(:,:), Mcon(:,:,:), Mterms(:,:,:,:)
  real(dp) :: rho2, a2, rks2, rks, hcell, target_r, best_dr, dr
  real(dp) :: rhs_max(NVARS,2), rhs_sum(NVARS,2), con_max(4,2), con_sum(4,2)
  integer :: i, k, v, c, reg, ncell(2), ncon(2), imin, imax, kmin, kmax
  integer :: ibad, kbad, ictl, kctl
  integer :: imaxloc(NVARS,2), kmaxloc(NVARS,2), icmax(4,2), kcmax(4,2)
  real(dp) :: hstep, r_near_lo, r_near_hi
  character(len=256) :: par_file, buf

  par_file = 'parameter_files/vacuum_kerr.par'
  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) par_file = buf
  call init_params(trim(par_file))
  slicing_mode = 2
  gamma_driver_mode = 2
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(Nx,Nz,NVARS))
  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call extrapolate_excised(u)
  call init_deriv_stencils()
  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)

  allocate(H(Nx,Nz), Mcon(Nx,Nz,3), Mterms(Nx,Nz,3,7))
  call hamiltonian_constraint(u,H,imin,imax,kmin,kmax)
  call momentum_constraint(u,Mcon,Mterms)

  rhs_max = 0.0_dp; rhs_sum = 0.0_dp; ncell = 0
  con_max = 0.0_dp; con_sum = 0.0_dp; ncon = 0
  imaxloc = 0; kmaxloc = 0; icmax = 0; kcmax = 0
  a2 = bh_spin*bh_spin
  hstep = max(dx,dz)
  ! Hold physical-radius bands fixed across resolution runs. The lower cut
  ! stays at least two cells outside excision, but never below r_excise+0.5M;
  ! the upper near-field boundary is fixed at 2.3M.
  r_near_lo = max(r_excise + 2.0_dp*hstep, r_excise + 0.5_dp)
  r_near_hi = 2.3_dp
  do k = 2, Nz-1
    do i = 1, Nx-1
      rho2 = xg(i)*xg(i) + zg(k)*zg(k)
      rks2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)*(rho2-a2) + &
                                    4.0_dp*a2*zg(k)*zg(k)))
      rks = sqrt(max(0.0_dp,rks2))
      if (rks < r_near_lo) cycle
      if (rks > 4.0_dp) cycle
      if (rks < r_near_hi) then
        reg = 1
      else
        reg = 2
      end if
      ncell(reg) = ncell(reg) + 1
      do v = 1, NVARS
        rhs_sum(v,reg) = rhs_sum(v,reg) + rhs(i,k,v)*rhs(i,k,v)
        if (abs(rhs(i,k,v)) > rhs_max(v,reg)) then
          rhs_max(v,reg) = abs(rhs(i,k,v))
          imaxloc(v,reg) = i; kmaxloc(v,reg) = k
        end if
      end do

      ! Constraint routines have distinct valid index ranges. Use only their
      ! overlap with the same fixed radial regions, preserving Mx, My, Mz.
      if (i >= imin .and. i <= imax .and. k >= kmin .and. k <= kmax) then
        ncon(reg) = ncon(reg) + 1
        hcell = abs(H(i,k))
        con_sum(1,reg) = con_sum(1,reg) + H(i,k)*H(i,k)
        if (hcell > con_max(1,reg)) then
          con_max(1,reg) = hcell; icmax(1,reg)=i; kcmax(1,reg)=k
        end if
        do c = 1, 3
          con_sum(c+1,reg) = con_sum(c+1,reg) + Mcon(i,k,c)*Mcon(i,k,c)
          if (abs(Mcon(i,k,c)) > con_max(c+1,reg)) then
            con_max(c+1,reg)=abs(Mcon(i,k,c)); icmax(c+1,reg)=i; kcmax(c+1,reg)=k
          end if
        end do
      end if
    end do
  end do

  write(*,'(A,I0,A,I0,A,ES10.3,A,ES10.3)') &
       'Frozen Kerr-Schild RHS: Nx=',Nx,' Nz=',Nz,' dx=',dx,' dz=',dz
  write(*,'(A,ES10.3,A,ES10.3,A,ES10.3,A)') 'near=',r_near_lo,' .. ',r_near_hi, &
       '; bulk=',r_near_hi,' .. 4M'
  write(*,'(A,I0,A,I0)') 'Sample counts: near=',ncell(1),' bulk=',ncell(2)
  write(*,'(A)') 'field  near max       near RMS       bulk max       bulk RMS       near location       bulk location'
  do v = 1, NVARS
    write(*,'(I4,4(1X,ES12.4),4(1X,F8.4))') v, rhs_max(v,1), &
         sqrt(rhs_sum(v,1)/real(max(1,ncell(1)),dp)), rhs_max(v,2), &
         sqrt(rhs_sum(v,2)/real(max(1,ncell(2)),dp)), &
         xg(max(1,imaxloc(v,1))),zg(max(1,kmaxloc(v,1))), &
         xg(max(1,imaxloc(v,2))),zg(max(1,kmaxloc(v,2)))
  end do
  write(*,'(A,I0,A,I0)') 'Constraint sample counts: near=',ncon(1),' bulk=',ncon(2)
  write(*,'(A)') 'constraint       near max       near RMS       bulk max       bulk RMS       component'
  do c = 1, 4
    if (c == 1) then
      write(*,'(A)',advance='no') 'H'
    else
      write(*,'(A,I0)',advance='no') 'M',c-1
    end if
    write(*,'(4(1X,ES12.4),2(1X,F8.4))') con_max(c,1), &
         sqrt(con_sum(c,1)/real(max(1,ncon(1)),dp)),con_max(c,2), &
         sqrt(con_sum(c,2)/real(max(1,ncon(2)),dp)), &
         xg(max(1,icmax(c,1))),zg(max(1,kcmax(c,1)))
  end do
  ibad = icmax(4,1); kbad = kcmax(4,1)
  rho2 = xg(ibad)*xg(ibad) + zg(kbad)*zg(kbad)
  rks2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)*(rho2-a2)+4.0_dp*a2*zg(kbad)*zg(kbad)))
  target_r = sqrt(max(0.0_dp,rks2))
  write(*,'(A,2(1X,F10.6),A,ES12.4)') 'Near-band Mz maximum at (x,z)=', &
       xg(ibad),zg(kbad),'  Mz=',Mcon(ibad,kbad,3)
  write(*,'(A,7(1X,ES13.5))') &
       '  [dAup,-(2/3)K.dgt,-(2/3)gt.dK,-4dphi.A,(8/3)dphi.gtK,Gam_i_S,Gam_j_S] =',Mterms(ibad,kbad,3,:)

  ! Compare at nearly the same Kerr radius but away from the axis strip.
  best_dr = huge(1.0_dp); ictl = 1; kctl = 1
  do k = 2, Nz-1
    do i = 2, Nx-1
      if (xg(i) < 5.0_dp*hstep) cycle
      rho2 = xg(i)*xg(i) + zg(k)*zg(k)
      rks2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)*(rho2-a2)+4.0_dp*a2*zg(k)*zg(k)))
      rks = sqrt(max(0.0_dp,rks2))
      dr = abs(rks-target_r)
      if (dr < best_dr) then
        best_dr = dr; ictl = i; kctl = k
      end if
    end do
  end do
  write(*,'(A,2(1X,F10.6),A,ES12.4,A,ES10.3)') 'Same-radius off-axis control (x,z)=', &
       xg(ictl),zg(kctl),'  Mz=',Mcon(ictl,kctl,3),'  |dr|=',best_dr
  write(*,'(A,7(1X,ES13.5))') &
       '  [dAup,-(2/3)K.dgt,-(2/3)gt.dK,-4dphi.A,(8/3)dphi.gtK,Gam_i_S,Gam_j_S] =',Mterms(ictl,kctl,3,:)
  if (any(rhs /= rhs)) error stop 'non-finite frozen Kerr-Schild RHS'
end program diagnose_stationary_ks_rhs
