!=======================================================================
!  mod_output : ASCII dumps, line cuts and convergence tables
!=======================================================================
module mod_output
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_basis
  use mod_physics
  use mod_dg, only : phi_h
  implicit none
  private
  public :: write_solution, write_line_diag, write_line_axis,            &
            write_header, write_error_row, open_energy, log_energy,      &
            close_energy

  integer :: iue = -1

contains

  !---------------------------------------------------------------------
  subroutine write_solution(U, t, tag)
    real(dp), intent(in) :: U(nb,nvar,ncell)
    real(dp), intent(in) :: t
    character(len=*), intent(in) :: tag
    integer  :: iu, ic, iv
    real(dp) :: xi(3), x(3), u1(nvar), rho, vel(3), p, ph
    character(len=256) :: fname

    write(fname,'(a,a,a,a,i0,a)') trim(case_name),'_',trim(scheme),'_',nx(1),'.dat'
    if (len_trim(tag) > 0) &
       write(fname,'(a,a,a,a,a,a,i0,a)') trim(case_name),'_',trim(scheme),'_', &
             trim(tag),'_',nx(1),'.dat'

    open(newunit=iu, file=trim(fname), status='replace', action='write')
    write(iu,'(a,es16.8)') '# time = ', t
    if (nd == 2) then
       write(iu,'(a)') '# x  y  rho  u  v  p  phi'
    else
       write(iu,'(a)') '# x  y  z  rho  u  v  w  p  phi'
    end if

    xi = zero
    do ic = 1, ncell
       call ref2phys(ic, xi, x)
       do iv = 1, nvar
          u1(iv) = U(1,iv,ic)
       end do
       ph = phi_h(1,ic)
       call cons2prim(u1, rho, vel, p)
       write(iu,'(20es20.10)') x(1:nd), rho, vel(1:nd), p, ph
       if (mod(ic, nx(1)) == 0) write(iu,*)
    end do
    close(iu)
  end subroutine write_solution

  !---------------------------------------------------------------------
  !  cut along the diagonal x = y  (Fig. 5.3c,d)
  !---------------------------------------------------------------------
  subroutine write_line_diag(U, t)
    real(dp), intent(in) :: U(nb,nvar,ncell)
    real(dp), intent(in) :: t
    integer  :: iu, i, k, ic, iv
    real(dp) :: x(3), u1(nvar), rho, vel(3), p, ph, xi(3)
    character(len=256) :: fname
    if (nd < 2) return
    write(fname,'(a,a,a,a,i0,a)') trim(case_name),'_',trim(scheme),'_diag_',nx(1),'.dat'
    open(newunit=iu, file=trim(fname), status='replace', action='write')
    write(iu,'(a,es16.8)') '# time = ', t
    write(iu,'(a)') '# s  x  rho  u  p  phi     (along x = y)'
    k = max(1, nx(3)/2)
    xi = zero
    do i = 1, min(nx(1), nx(2))
       ic = cid(i, i, k)
       call ref2phys(ic, xi, x)
       do iv = 1, nvar
          u1(iv) = U(1,iv,ic)
       end do
       ph = phi_h(1,ic)
       call cons2prim(u1, rho, vel, p)
       write(iu,'(10es20.10)') sqrt(two)*x(1), x(1), rho, vel(1), p, ph
    end do
    close(iu)
  end subroutine write_line_diag

  !---------------------------------------------------------------------
  !  cut along y = z = 0  (Fig. 5.10)
  !---------------------------------------------------------------------
  subroutine write_line_axis(U, t)
    real(dp), intent(in) :: U(nb,nvar,ncell)
    real(dp), intent(in) :: t
    integer  :: iu, i, j, k, ic, iv
    real(dp) :: x(3), u1(nvar), rho, vel(3), p, ph, xi(3)
    character(len=256) :: fname
    write(fname,'(a,a,a,a,i0,a)') trim(case_name),'_',trim(scheme),'_axis_',nx(1),'.dat'
    open(newunit=iu, file=trim(fname), status='replace', action='write')
    write(iu,'(a,es16.8)') '# time = ', t
    write(iu,'(a)') '# x  rho  u  p  phi'
    j = max(1, nx(2)/2)
    k = max(1, nx(3)/2)
    xi = zero
    do i = 1, nx(1)
       ic = cid(i,j,k)
       call ref2phys(ic, xi, x)
       do iv = 1, nvar
          u1(iv) = U(1,iv,ic)
       end do
       ph = phi_h(1,ic)
       call cons2prim(u1, rho, vel, p)
       write(iu,'(10es20.10)') x(1), rho, vel(1), p, ph
    end do
    close(iu)
  end subroutine write_line_axis

  !---------------------------------------------------------------------
  !  E_tot(t) history  (Figs. 5.4 and 5.7)
  !---------------------------------------------------------------------
  subroutine open_energy()
    character(len=256) :: fname
    write(fname,'(a,a,a,a,i0,a)') trim(case_name),'_',trim(scheme),'_etot_',nx(1),'.dat'
    open(newunit=iue, file=trim(fname), status='replace', action='write')
    write(iue,'(a)') '# t   E_tot   E_tot - E_tot(0)'
  end subroutine open_energy

  subroutine log_energy(t, e, e0)
    real(dp), intent(in) :: t, e, e0
    if (iue > 0) write(iue,'(3es24.15)') t, e, e - e0
  end subroutine log_energy

  subroutine close_energy()
    if (iue > 0) close(iue)
    iue = -1
  end subroutine close_energy

  !---------------------------------------------------------------------
  subroutine write_header()
    write(*,'(a)') '======================================================================'
    write(*,'(a)') ' High order well-balanced and total-energy-conserving LDG methods'
    write(*,'(a)') ' for the compressible self-gravitating Euler equations'
    write(*,'(a)') ' Pan, Chen, Qiu & Xiong,  J. Comput. Phys. 556 (2026) 114807'
    write(*,'(a)') '======================================================================'
    write(*,'(a,a)')      '  case          : ', trim(case_name)
    write(*,'(a,a)')      '  scheme        : ', trim(scheme)
    write(*,'(a,a1,i0)')  '  space         : ', basis_type, kdeg
    write(*,'(a,i0)')     '  dimension     : ', nd
    write(*,'(a,3i6)')    '  cells         : ', nx(1:nd)
    write(*,'(a,i0)')     '  SSP-RK order  : ', rk_order
    write(*,'(a,f8.4)')   '  CFL           : ', cfl
    write(*,'(a,es12.4)') '  t_final       : ', tfinal
    write(*,'(a,l1,a,l1,a,l1)') '  OE = ', use_oe, '   PP = ', use_pp,    &
                                '   IBP = ', use_ibp
    write(*,'(a,es11.4,a,es11.4,a,es11.4)')                               &
         '  gamma = ', gam, '   nu = ', pnu, '   G = ', Ggrav
    write(*,'(a,es11.4,a,es11.4)') '  kappa = ', kap, '   a = ', aLE
    write(*,'(a)') '----------------------------------------------------------------------'
  end subroutine write_header

  !---------------------------------------------------------------------
  !  convergence table in the format of Tables 5.1-5.8
  !---------------------------------------------------------------------
  subroutine write_error_row(n, e1, e2, ei, e1p, e2p, eip, first)
    integer,  intent(in) :: n
    real(dp), intent(in) :: e1, e2, ei, e1p, e2p, eip
    logical,  intent(in) :: first
    real(dp) :: r1, r2, ri
    if (first) then
       write(*,'(a)') '   N        L1 error  order       L2 error  order      Linf error  order'
       write(*,'(i5,3(es15.5,a))') n, e1, '      - ', e2, '      - ', ei, '      - '
    else
       r1 = log(e1p/e1)/log(two)
       r2 = log(e2p/e2)/log(two)
       ri = log(eip/ei)/log(two)
       write(*,'(i5,3(es15.5,f7.2))') n, e1, r1, e2, r2, ei, ri
    end if
  end subroutine write_error_row

end module mod_output
