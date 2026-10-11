module fd_excision_mod

  use kinds_mod,   only: dp
  use params_mod,  only: Nx, Nz, dx
  use excision_mod, only: EXCISED, PHYSICAL, GHOST, &
                          stencil_is_safe_x, stencil_is_safe_z

  implicit none
  private

  public :: deriv_x
  public :: deriv_z
  public :: second_deriv_x
  public :: second_deriv_z

contains

  !===========================================================================
  ! First derivative in x.
  !
  ! Normal region:
  !
  !     4th-order centered
  !
  ! Boundary region:
  !
  !     one-sided derivative
  !===========================================================================

  real(dp) function deriv_x(f,i,k,status) result(df)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    if (stencil_is_safe_x(i,k,status)) then

      df = ( f(i-2,k) &
           - 8.0_dp*f(i-1,k) &
           + 8.0_dp*f(i+1,k) &
           -       f(i+2,k) ) / (12.0_dp*dx)

    else

      call one_sided_x(f,i,k,status,df)

    end if

  end function deriv_x


  !===========================================================================
  ! First derivative in z.
  !===========================================================================

  real(dp) function deriv_z(f,i,k,status) result(df)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    if (stencil_is_safe_z(i,k,status)) then

      df = ( f(i,k-2) &
           - 8.0_dp*f(i,k-1) &
           + 8.0_dp*f(i,k+1) &
           -       f(i,k+2) ) / (12.0_dp*dx)

    else

      call one_sided_z(f,i,k,status,df)

    end if

  end function deriv_z


  !===========================================================================
  ! One-sided x derivative.
  !
  ! Prefer the exterior direction that does not cross the excision region.
  !===========================================================================

  subroutine one_sided_x(f,i,k,status,df)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    real(dp), intent(out) :: df

    logical :: plus_ok
    logical :: minus_ok

    plus_ok = valid_x_side(i,k,+1,status)
    minus_ok = valid_x_side(i,k,-1,status)

    !-----------------------------------------------------------------------
    ! Four-point forward stencil:
    !
    ! f'(x_i) =
    !   [-11f_i + 18f_{i+1} - 9f_{i+2}
    !       + 2f_{i+3}]/(6h)
    !
    !-----------------------------------------------------------------------

    if (plus_ok) then

      df = (-11.0_dp*f(i,k) &
          + 18.0_dp*f(i+1,k) &
          -  9.0_dp*f(i+2,k) &
          +  2.0_dp*f(i+3,k)) / (6.0_dp*dx)

      return

    end if

    !-----------------------------------------------------------------------
    ! Four-point backward stencil:
    !-----------------------------------------------------------------------

    if (minus_ok) then

      df = (11.0_dp*f(i,k) &
          - 18.0_dp*f(i-1,k) &
          +  9.0_dp*f(i-2,k) &
          -  2.0_dp*f(i-3,k)) / (6.0_dp*dx)

      return

    end if

    write(*,*) 'ERROR: no valid x derivative stencil'
    write(*,*) 'i,k = ',i,k
    error stop 'one_sided_x'

  end subroutine one_sided_x


  !===========================================================================
  ! One-sided z derivative.
  !===========================================================================

  subroutine one_sided_z(f,i,k,status,df)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    real(dp), intent(out) :: df

    logical :: plus_ok
    logical :: minus_ok

    plus_ok = valid_z_side(i,k,+1,status)
    minus_ok = valid_z_side(i,k,-1,status)

    if (plus_ok) then

      df = (-11.0_dp*f(i,k) &
          + 18.0_dp*f(i,k+1) &
          -  9.0_dp*f(i,k+2) &
          +  2.0_dp*f(i,k+3)) / (6.0_dp*dx)

      return

    end if

    if (minus_ok) then

      df = (11.0_dp*f(i,k) &
          - 18.0_dp*f(i,k-1) &
          +  9.0_dp*f(i,k-2) &
          -  2.0_dp*f(i,k-3)) / (6.0_dp*dx)

      return

    end if

    write(*,*) 'ERROR: no valid z derivative stencil'
    write(*,*) 'i,k = ',i,k
    error stop 'one_sided_z'

  end subroutine one_sided_z


  !===========================================================================
  ! Check three points in the +x direction.
  !===========================================================================

  logical function valid_x_side(i,k,sgn,status)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: status(:,:)

    integer :: q,ii

    valid_x_side = .false.

    do q = 1,3

      ii = i + sgn*q

      if (ii < 1 .or. ii > Nx) return

      if (status(ii,k) == EXCISED) return

    end do

    valid_x_side = .true.

  end function valid_x_side


  !===========================================================================
  ! Check three points in the +z/-z direction.
  !===========================================================================

  logical function valid_z_side(i,k,sgn,status)

    integer, intent(in) :: i,k,sgn
    integer, intent(in) :: status(:,:)

    integer :: q,kk

    valid_z_side = .false.

    do q = 1,3

      kk = k + sgn*q

      if (kk < 1 .or. kk > Nz) return

      if (status(i,kk) == EXCISED) return

    end do

    valid_z_side = .true.

  end function valid_z_side


  !===========================================================================
  ! Second x derivative.
  !===========================================================================

  real(dp) function second_deriv_x(f,i,k,status) result(d2f)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    if (stencil_is_safe_x(i,k,status)) then

      d2f = (-f(i+2,k) &
             +16.0_dp*f(i+1,k) &
             -30.0_dp*f(i,k) &
             +16.0_dp*f(i-1,k) &
             -f(i-2,k)) / (12.0_dp*dx**2)

    else

      ! Temporary robust fallback:
      !
      ! Construct d/dx of d/dx using the same boundary-aware operator.
      !
      ! This is not formally 4th-order at the excision boundary, but is
      ! preferable to reading through the excision mask.

      d2f = deriv_x_local(f,i,k,status)

    end if

  end function second_deriv_x


  !===========================================================================
  ! Second z derivative.
  !===========================================================================

  real(dp) function second_deriv_z(f,i,k,status) result(d2f)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    if (stencil_is_safe_z(i,k,status)) then

      d2f = (-f(i,k+2) &
             +16.0_dp*f(i,k+1) &
             -30.0_dp*f(i,k) &
             +16.0_dp*f(i,k-1) &
             -f(i,k-2)) / (12.0_dp*dx**2)

    else

      d2f = deriv_z_local(f,i,k,status)

    end if

  end function second_deriv_z


  !===========================================================================
  ! Placeholder second derivative constructed from boundary-aware first
  ! derivatives.
  !
  ! This is a debugging-safe implementation. Once the code is stable,
  ! replace this with a formally derived one-sided second derivative.
  !===========================================================================

  real(dp) function deriv_x_local(f,i,k,status) result(d2f)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    real(dp) :: fp,fm

    fp = deriv_x(f,min(i+1,Nx),k,status)
    fm = deriv_x(f,max(i-1,1),k,status)

    d2f = (fp-fm)/(2.0_dp*dx)

  end function deriv_x_local


  real(dp) function deriv_z_local(f,i,k,status) result(d2f)

    real(dp), intent(in) :: f(:,:)
    integer, intent(in) :: i,k
    integer, intent(in) :: status(:,:)

    real(dp) :: fp,fm

    fp = deriv_z(f,i,min(k+1,Nz),status)
    fm = deriv_z(f,i,max(k-1,1),status)

    d2f = (fp-fm)/(2.0_dp*dx)

  end function deriv_z_local

end module fd_excision_mod
