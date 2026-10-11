!===============================================================================
! test_horizon.f90 -- validates horizon_mod against exact solutions
!   1. Kerr-Schild Schwarzschild (a=0), K /= 0, eq_sym, cell- and vertex-centred
!   2. Kerr-Schild a=0.5 (twist: gamma_xy, gamma_yz, K_xy, K_yz /= 0), eq_sym
!   3. Isotropic Schwarzschild (K = 0, horizon at R = M/2)
!   4. Kerr-Schild a=0.5 shifted off-origin in z, NO equatorial symmetry
! Each case is run at 3 resolutions; errors should shrink at >= 2nd order.
!===============================================================================
!-------------------------------------------------------------------------------
program test_horizon
  use horizon_mod
  use ks_data
  implicit none
  real(hdp), parameter :: pi = 3.141592653589793238462643383279502884_hdp

  write(*,'(a)') '=== Case 1: Kerr-Schild a=0 (Schwarzschild), eq_sym, cell-centred ==='
  call run_case(1, 1.0_hdp, 0.0_hdp, 0.0_hdp, .true.,  .false., 6, 5.0_hdp, [0.2_hdp,0.1_hdp,0.05_hdp], 2.4_hdp)
  write(*,'(a)') '=== Case 1b: same, vertex-centred (x0=z0=0) ==='
  call run_case(1, 1.0_hdp, 0.0_hdp, 0.0_hdp, .true.,  .true.,  6, 5.0_hdp, [0.2_hdp,0.1_hdp,0.05_hdp], 2.4_hdp)
  write(*,'(a)') '=== Case 2: Kerr-Schild a=0.5 (twist), eq_sym ==='
  call run_case(1, 1.0_hdp, 0.5_hdp, 0.0_hdp, .true.,  .false., 8, 5.0_hdp, [0.2_hdp,0.1_hdp,0.05_hdp], 2.4_hdp)
  write(*,'(a)') '=== Case 3: isotropic Schwarzschild (K=0), horizon at R=M/2 ==='
  call run_case(2, 1.0_hdp, 0.0_hdp, 0.0_hdp, .true.,  .false., 6, 2.0_hdp, [0.05_hdp,0.025_hdp,0.0125_hdp], 0.8_hdp)
  write(*,'(a)') '=== Case 4: Kerr-Schild a=0.5 shifted dz=+0.7, NO equatorial symmetry ==='
  call run_case(1, 1.0_hdp, 0.5_hdp, 0.7_hdp, .false., .false., 10, 5.0_hdp, [0.2_hdp,0.1_hdp,0.05_hdp], 2.4_hdp)

contains

  subroutine run_case(kind, M, a, zshift, eqs, vertex, nl, L, dxs, guess)
    integer,   intent(in) :: kind, nl
    real(hdp), intent(in) :: M, a, zshift, L, dxs(3), guess
    logical,   intent(in) :: eqs, vertex
    type(horizon_t) :: hz
    real(hdp), allocatable :: g(:,:,:), k(:,:,:)
    real(hdp) :: dx, x0, z0, A_ex, J_ex, rp, errA(3), errJ(3), xx, zz, g6(6), k6(6)
    integer :: ir, nx, nz, i, j, ierr
    do ir = 1, 3
       dx = dxs(ir)
       if (vertex) then
          x0 = 0.0_hdp; nx = nint(L/dx) + 1
       else
          x0 = 0.5_hdp*dx; nx = nint(L/dx)
       end if
       if (eqs) then
          z0 = x0; nz = nx
       else
          z0 = -L + x0 + zshift; nz = nint(2*L/dx) + merge(1, 0, vertex)
       end if
       allocate(g(6,nx,nz), k(6,nx,nz))
       do j = 1, nz
          do i = 1, nx
             xx = x0 + (i-1)*dx
             zz = z0 + (j-1)*dx
             if (kind == 1) then
                call ks_adm(M, a, [xx, 0.0_hdp, zz - zshift], g6, k6)
             else
                call iso_adm(M, [xx, 0.0_hdp, zz - zshift], g6, k6)
             end if
             g(:,i,j) = g6;  k(:,i,j) = k6
          end do
       end do
       call horizon_init_grid(x0, z0, dx, dx, nx, nz, eqs)
       call horizon_load_adm(g, k)
       call horizon_init(hz, nl, zshift, eqs)
       call find_horizon(hz, guess, ierr)
       if (kind == 1) then
          rp = M + sqrt(M*M - a*a);  A_ex = 8*pi*M*rp;  J_ex = a*M
       else
          A_ex = 16*pi*M*M;  J_ex = 0.0_hdp
       end if
       errA(ir) = abs(hz%area/A_ex - 1.0_hdp)
       errJ(ir) = abs(hz%j_spin - J_ex)
       write(*,'(a,f7.4,a,i2,a,i3,a,es9.2)') ' dx=', dx, '  ierr=', ierr, '  iters=', hz%niter, '  max|Theta|=', hz%resid
       write(*,'(a,es10.2,a,f10.6,a,f10.6)') '    A/A_exact-1 =', hz%area/A_ex - 1.0_hdp, &
            '   M_irr=', hz%m_irr, '   M_H=', hz%m_h
       write(*,'(a,f10.6,a,f10.6,a,f9.5,a,f9.5)') '    J=', hz%j_spin, ' (exact', J_ex, ')   hmin=', hz%hmin, ' hmax=', hz%hmax
       write(*,'(a,es10.2)') '    |J - J_exact| =', abs(hz%j_spin - J_ex)
       deallocate(g, k)
    end do
    write(*,'(a,f6.2,a,f6.2)') '  observed order (area):', log(errA(1)/errA(2))/log(2.0_hdp), &
         ' ,', log(errA(2)/errA(3))/log(2.0_hdp)
    if (abs(a) > 0.0_hdp) write(*,'(a,f6.2,a,f6.2)') '  observed order (J):   ', &
         log(errJ(1)/errJ(2))/log(2.0_hdp), ' ,', log(errJ(2)/errJ(3))/log(2.0_hdp)
    write(*,*)
  end subroutine run_case

end program test_horizon
