!===============================================================================
! ks_data.f90 -- exact Kerr-Schild and isotropic-Schwarzschild ADM data (test only)
!===============================================================================
module ks_data
  use horizon_mod, only: hdp
  implicit none
contains

  subroutine ks_g(M, a, p, g, bl, H, ok)
    real(hdp), intent(in)  :: M, a, p(3)
    real(hdp), intent(out) :: g(3,3), bl(3), H
    logical,   intent(out) :: ok
    real(hdp) :: Rsq, rr2, r, l(3), x, y, z
    integer :: i, j
    x = p(1); y = p(2); z = p(3)
    Rsq = x*x + y*y + z*z
    rr2 = 0.5_hdp*(Rsq - a*a + sqrt((Rsq - a*a)**2 + 4*a*a*z*z))
    r  = sqrt(rr2)
    H  = M*r**3/(r**4 + a*a*z*z)
    l  = [(r*x + a*y)/(rr2 + a*a), (r*y - a*x)/(rr2 + a*a), z/r]
    do j = 1, 3
       do i = 1, 3
          g(i,j) = 2*H*l(i)*l(j)
       end do
       g(j,j) = g(j,j) + 1.0_hdp
    end do
    bl = 2*H*l
    ok = .true.
  end subroutine ks_g

  ! Exact (stationary) Kerr-Schild ADM data: K_ij = (D_i b_j + D_j b_i)/(2 alpha)
  subroutine ks_adm(M, a, p0, g6, k6)
    real(hdp), intent(in)  :: M, a, p0(3)
    real(hdp), intent(out) :: g6(6), k6(6)
    real(hdp), parameter :: st = 2.0e-3_hdp
    real(hdp) :: p(3), pp(3), g(3,3), bl(3), H
    real(hdp) :: dg(3,3,3), db(3,3), gi(3,3), Gam(3,3,3), Dbt(3,3), kk(3,3), alpha
    real(hdp) :: gp2(3,3), gm2(3,3), bp2(3), bm2(3), Hd, R
    integer :: i, j, k, l, m2
    logical :: ok
    p = p0
    R = sqrt(sum(p*p))
    if (R < 0.6_hdp) then                 ! junk inside, never used by the finder
       if (R < 1.0e-12_hdp) then
          p = [0.6_hdp, 0.0_hdp, 0.0_hdp]
       else
          p = p*(0.6_hdp/R)
       end if
    end if
    call ks_g(M, a, p, g, bl, H, ok)
    do k = 1, 3
       pp = p; pp(k) = p(k) + st;   call ks_g(M, a, pp, gp2, bp2, Hd, ok)
       dg(k,:,:) = 8*gp2; db(k,:) = 8*bp2
       pp = p; pp(k) = p(k) - st;   call ks_g(M, a, pp, gm2, bm2, Hd, ok)
       dg(k,:,:) = dg(k,:,:) - 8*gm2; db(k,:) = db(k,:) - 8*bm2
       pp = p; pp(k) = p(k) + 2*st; call ks_g(M, a, pp, gp2, bp2, Hd, ok)
       dg(k,:,:) = dg(k,:,:) - gp2; db(k,:) = db(k,:) - bp2
       pp = p; pp(k) = p(k) - 2*st; call ks_g(M, a, pp, gm2, bm2, Hd, ok)
       dg(k,:,:) = (dg(k,:,:) + gm2)/(12*st); db(k,:) = (db(k,:) + bm2)/(12*st)
    end do
    call inv3(g, gi)
    do l = 1, 3; do i = 1, 3; do j = 1, 3
       Gam(l,i,j) = 0
       do m2 = 1, 3
          Gam(l,i,j) = Gam(l,i,j) + 0.5_hdp*gi(l,m2)*(dg(i,m2,j) + dg(j,m2,i) - dg(m2,i,j))
       end do
    end do; end do; end do
    alpha = 1.0_hdp/sqrt(1.0_hdp + 2*H)
    do i = 1, 3; do j = 1, 3
       Dbt(i,j) = db(i,j) - sum(Gam(:,i,j)*bl(:))
    end do; end do
    do i = 1, 3; do j = 1, 3
       kk(i,j) = (Dbt(i,j) + Dbt(j,i))/(2*alpha)
    end do; end do
    g6 = [g(1,1), g(1,2), g(1,3), g(2,2), g(2,3), g(3,3)]
    k6 = [kk(1,1), kk(1,2), kk(1,3), kk(2,2), kk(2,3), kk(3,3)]
  end subroutine ks_adm

  subroutine iso_adm(M, p, g6, k6)
    real(hdp), intent(in)  :: M, p(3)
    real(hdp), intent(out) :: g6(6), k6(6)
    real(hdp) :: R, psi4
    R = max(sqrt(sum(p*p)), 0.2_hdp)
    psi4 = (1.0_hdp + 0.5_hdp*M/R)**4
    g6 = [psi4, 0.0_hdp, 0.0_hdp, psi4, 0.0_hdp, psi4]
    k6 = 0.0_hdp
  end subroutine iso_adm

  subroutine inv3(a, b)
    real(hdp), intent(in)  :: a(3,3)
    real(hdp), intent(out) :: b(3,3)
    real(hdp) :: d
    b(1,1) = a(2,2)*a(3,3) - a(2,3)*a(3,2)
    b(1,2) = a(1,3)*a(3,2) - a(1,2)*a(3,3)
    b(1,3) = a(1,2)*a(2,3) - a(1,3)*a(2,2)
    b(2,1) = a(2,3)*a(3,1) - a(2,1)*a(3,3)
    b(2,2) = a(1,1)*a(3,3) - a(1,3)*a(3,1)
    b(2,3) = a(1,3)*a(2,1) - a(1,1)*a(2,3)
    b(3,1) = a(2,1)*a(3,2) - a(2,2)*a(3,1)
    b(3,2) = a(1,2)*a(3,1) - a(1,1)*a(3,2)
    b(3,3) = a(1,1)*a(2,2) - a(1,2)*a(2,1)
    d = a(1,1)*b(1,1) + a(1,2)*b(2,1) + a(1,3)*b(3,1)
    b = b/d
  end subroutine inv3
end module ks_data
