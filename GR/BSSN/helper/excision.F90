module excision_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, r_excise
  use grid_mod, only: xg, zg
  implicit none
  private
  public :: extrapolate_excised

contains

  logical function is_excised(i, k) result(r)
    integer, intent(in) :: i, k
    r = sqrt(xg(i)**2 + zg(k)**2) < r_excise
  end function is_excised

  ! Fills every excised grid point from the average of its immediate,
  ! currently-non-excised grid neighbors (a few Jacobi sweeps so information
  ! propagates all the way to the center of a multi-point-wide excision
  ! region). This keeps the frozen interior continuously consistent with the
  ! evolving exterior, rather than drifting stale as pure freezing does.
  ! Works on any state array (Nx, Nz, nfields) -- reused for both the BSSN
  ! and hydro conserved states.
  subroutine extrapolate_excised(u)
    real(dp), intent(inout) :: u(:,:,:)
    real(dp), allocatable :: unew(:,:,:)
    integer :: i, k, sweep, cnt, nfields
    real(dp), allocatable :: acc(:)

    nfields = size(u,3)
    allocate(unew(Nx, Nz, nfields), acc(nfields))

    do sweep = 1, 4
      unew = u
      do k = 2, Nz - 1
        do i = 1, Nx - 1
          if (.not. is_excised(i,k)) cycle
          acc = 0.0_dp; cnt = 0
          if (i+1 <= Nx   .and. .not. is_excised(i+1,k)) then; acc = acc + u(i+1,k,:); cnt = cnt+1; end if
          if (i-1 >= 1    .and. .not. is_excised(i-1,k)) then; acc = acc + u(i-1,k,:); cnt = cnt+1; end if
          if (k+1 <= Nz   .and. .not. is_excised(i,k+1)) then; acc = acc + u(i,k+1,:); cnt = cnt+1; end if
          if (k-1 >= 1    .and. .not. is_excised(i,k-1)) then; acc = acc + u(i,k-1,:); cnt = cnt+1; end if
          if (cnt > 0) unew(i,k,:) = acc / real(cnt, dp)
        end do
      end do
      u = unew
    end do

    deallocate(unew, acc)
  end subroutine extrapolate_excised

end module excision_mod
