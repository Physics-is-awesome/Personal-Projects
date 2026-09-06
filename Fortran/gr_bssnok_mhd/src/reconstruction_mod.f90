module reconstruction_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use hydro_vars_mod, only: NPRIM
  use hydro_cartoon_mod, only: get_neighbor_prim
  implicit none
  private
  public :: reconstruct_x_faces, reconstruct_z_faces

contains

  function minmod_arr(a, b) result(m)
    real(dp), intent(in) :: a(NPRIM), b(NPRIM)
    real(dp) :: m(NPRIM)
    integer :: v
    do v = 1, NPRIM
      if (a(v)*b(v) <= 0.0_dp) then
        m(v) = 0.0_dp
      else
        m(v) = sign(min(abs(a(v)), abs(b(v))), a(v))
      end if
    end do
  end function minmod_arr

  ! Both x-faces of cell (i,k): face i+1/2 (subscript _p) and face i-1/2
  ! (subscript _m). PLM where the 5-point stencil fits (i-2 is always
  ! reachable via the axis Cartoon ghost; only the +x outer boundary is a
  ! hard limit), falling back to piecewise-constant (donor-cell) within 2
  ! cells of that boundary.
  subroutine reconstruct_x_faces(prim, i, k, primL_p, primR_p, primL_m, primR_m)
    real(dp), intent(in)  :: prim(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: primL_p(NPRIM), primR_p(NPRIM), primL_m(NPRIM), primR_m(NPRIM)
    real(dp) :: pm2(NPRIM), pm1(NPRIM), p0(NPRIM), pp1(NPRIM), pp2(NPRIM)
    real(dp) :: sm1(NPRIM), s0(NPRIM), sp1(NPRIM)
    logical :: can_plm

    can_plm = (i+2 <= Nx)

    p0  = prim(i,k,:)
    pp1 = get_neighbor_prim(prim, i, k,  1, 0, 0.0_dp)
    pm1 = get_neighbor_prim(prim, i, k, -1, 0, 0.0_dp)

    if (can_plm) then
      pm2 = get_neighbor_prim(prim, i, k, -2, 0, 0.0_dp)
      pp2 = get_neighbor_prim(prim, i, k,  2, 0, 0.0_dp)

      sm1 = minmod_arr(pm1-pm2, p0-pm1)
      s0  = minmod_arr(p0-pm1,  pp1-p0)
      sp1 = minmod_arr(pp1-p0,  pp2-pp1)

      primL_p = p0  + 0.5_dp*s0
      primR_p = pp1 - 0.5_dp*sp1
      primL_m = pm1 + 0.5_dp*sm1
      primR_m = p0  - 0.5_dp*s0
    else
      primL_p = p0;  primR_p = pp1
      primL_m = pm1; primR_m = p0
    end if
  end subroutine reconstruct_x_faces

  ! Both z-faces of cell (i,k): face k+1/2 (_p) and face k-1/2 (_m).
  subroutine reconstruct_z_faces(prim, i, k, primL_p, primR_p, primL_m, primR_m)
    real(dp), intent(in)  :: prim(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: primL_p(NPRIM), primR_p(NPRIM), primL_m(NPRIM), primR_m(NPRIM)
    real(dp) :: pm2(NPRIM), pm1(NPRIM), p0(NPRIM), pp1(NPRIM), pp2(NPRIM)
    real(dp) :: sm1(NPRIM), s0(NPRIM), sp1(NPRIM)
    logical :: can_plm

    can_plm = (k-2 >= 1) .and. (k+2 <= Nz)

    p0  = prim(i,k,:)
    pp1 = prim(i,k+1,:)
    pm1 = prim(i,k-1,:)

    if (can_plm) then
      pm2 = prim(i,k-2,:)
      pp2 = prim(i,k+2,:)

      sm1 = minmod_arr(pm1-pm2, p0-pm1)
      s0  = minmod_arr(p0-pm1,  pp1-p0)
      sp1 = minmod_arr(pp1-p0,  pp2-pp1)

      primL_p = p0  + 0.5_dp*s0
      primR_p = pp1 - 0.5_dp*sp1
      primL_m = pm1 + 0.5_dp*sm1
      primR_m = p0  - 0.5_dp*s0
    else
      primL_p = p0;  primR_p = pp1
      primL_m = pm1; primR_m = p0
    end if
  end subroutine reconstruct_z_faces

end module reconstruction_mod
