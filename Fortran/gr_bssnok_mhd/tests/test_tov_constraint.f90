program test_tov_constraint
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: alloc_state
  use hydro_vars_mod, only: NPRIM, alloc_cons
  use tov_mod, only: tov_profile_t, solve_tov
  use tov_id_mod, only: set_tov_id
  use eos_mod, only: set_eos_gamma
  use constraints_mod, only: hamiltonian_constraint
  use hydro_rhs_dynamic_mod, only: recover_all_primitives_dynamic
  use matter_source_grid_mod, only: build_matter_source_grid
  implicit none

  type(tov_profile_t) :: prof
  real(dp), allocatable :: bssn_u(:,:,:), hydro_cons(:,:,:), prim(:,:,:), msrc(:,:,:)
  real(dp), allocatable :: Hgrid(:,:)
  integer :: imin, imax, kmin, kmax, i, k
  real(dp) :: fourpi, Hmax_vac, Hmax_matter, Hrms_matter
  integer :: npts

  r_excise = 0.0_dp
  call init_params()
  call setup_grid()

  call set_eos_gamma(2.0_dp)
  call solve_tov(1.28e-3_dp, 100.0_dp, 2.0_dp, prof, 50.0_dp, 200000)

  call alloc_state(bssn_u, Nx, Nz)
  call alloc_cons(hydro_cons, Nx, Nz)
  allocate(prim(Nx,Nz,NPRIM), msrc(Nx,Nz,10))

  call set_tov_id(bssn_u, hydro_cons, prof)
  call recover_all_primitives_dynamic(hydro_cons, bssn_u, prim)
  call build_matter_source_grid(prim, bssn_u, msrc)

  allocate(Hgrid(Nx,Nz))
  call hamiltonian_constraint(bssn_u, Hgrid, imin, imax, kmin, kmax)

  fourpi = 4.0_dp*3.14159265358979323846_dp
  Hmax_vac = maxval(abs(Hgrid(imin:imax,kmin:kmax)))

  Hmax_matter = 0.0_dp; Hrms_matter = 0.0_dp; npts = 0
  do k = kmin, kmax
    do i = imin, imax
      block
        real(dp) :: Hfull
        Hfull = Hgrid(i,k) - 4.0_dp*fourpi*msrc(i,k,1)   ! subtract 16 pi E
        Hmax_matter = max(Hmax_matter, abs(Hfull))
        Hrms_matter = Hrms_matter + Hfull**2
        npts = npts + 1
      end block
    end do
  end do
  Hrms_matter = sqrt(Hrms_matter/real(npts,dp))

  print *, "=== TOV initial-data Hamiltonian constraint check (t=0) ==="
  print '(A,ES12.4)', " max|H_vacuum_formula| (no matter subtracted) = ", Hmax_vac
  print '(A,ES12.4)', " max|H_vacuum - 16*pi*E| (should be small)    = ", Hmax_matter
  print '(A,ES12.4)', " rms|H_vacuum - 16*pi*E|                      = ", Hrms_matter
  print *
  print *, "Sample: E, R(from Hgrid+16piE... no, Hgrid IS R+2K^2/3-AijAij), radius:"
  do k = kmin, kmax, max(1,(kmax-kmin)/10)
    print '(A,F8.4,A,ES12.4,A,ES12.4,A,ES12.4)', "  z=", zg(k), "  E=", msrc(1,k,1), &
      "  H_vac=", Hgrid(1,k), "  H_vac-16piE=", Hgrid(1,k)-4.0_dp*fourpi*msrc(1,k,1)
  end do

end program test_tov_constraint
