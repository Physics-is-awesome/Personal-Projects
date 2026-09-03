!=======================================================================
!  mod_basis : the broken finite element spaces (3.2a)-(3.2b)
!
!     V_h^k     = { v : v|_K in P^k(K)  or  Q^k(K) }
!     Sigma_h^k = [V_h^k]^d
!
!  On the reference cell [-1,1]^d the basis is the tensor product of the
!  L2-normalised Legendre polynomials
!
!     L_m(xi) = sqrt(2m+1) P_m(xi),   int_{-1}^{1} L_m L_n dxi = 2 d_mn
!
!  so that  int_K b_a b_b dx = |K| delta_ab  (diagonal mass matrix) and
!  b_1 == 1, i.e. the first modal coefficient is the cell average, which
!  is what the OE damping of (3.39) and the PP limiter both require.
!
!  Quadrature: the volume rule {x_K^(q), w_K^(q)}_{q=1..Q} of (3.23) and
!  the face rule {x_E^(mu), w_E^(mu)}_{mu=1..N} of (3.21).  Weights are
!  normalised so that   (1/|K|) int_K f = sum_q w_q f_q   and
!                       (1/|e|) int_e f = sum_mu w_mu f_mu.
!
!  ---------------------------------------------------------------------
!  MULTI-INDEX DERIVATIVE TABLES  (new; required by Sec. 3.6)
!
!  The OE indicator needs, for every m = 0..k,
!
!     sigma^m_{e,K}(u) ~ sum_{|alpha| = m} (1/|e|) int_e |[[ d^alpha u ]]| dS
!
!  i.e. the jump of EVERY partial derivative of total order m, where
!
!     d^alpha u = d^{|alpha|} u / dx_1^{alpha_1} ... dx_d^{alpha_d}.
!
!  The number of such multi-indices is  C(m+d-1, d-1)  -- in 2D that is
!  m+1 (so 3 for m = 2, 4 for m = 3), in 3D (m+1)(m+2)/2.  Summing only
!  d terms per m, as a first-derivative-only implementation does, is NOT
!  the paper's indicator for k >= 2.  We therefore precompute the exact
!  PHYSICAL derivative traces
!
!     dabf(a, ii, mu, s, m) = d^{mi(:,ii)} b_a  at face point mu
!
!  for all |alpha| <= k, using the r-times differentiated Legendre
!  recurrence in mod_quadrature and the chain rule d/dx_m = (2/h_m) d/dxi_m.
!=======================================================================
module mod_basis
  use mod_kinds
  use mod_params
  use mod_mesh
  use mod_quadrature, only : gauss_legendre, leg_table
  implicit none
  public

  integer :: nb   = 0        ! dim V_h^k on one cell
  integer :: nq1  = 0        ! 1D Gauss points per direction
  integer :: nqv  = 0        ! Q = nq1^d
  integer :: nqf  = 0        ! N = nq1^(d-1)
  integer :: nmi  = 0        ! # multi-indices with |alpha| <= k

  integer,  allocatable :: ideg(:,:)      ! (3,nb)   basis exponents
  integer,  allocatable :: idsum(:)       ! (nb)     |alpha| of basis fn
  integer,  allocatable :: mi(:,:)        ! (3,nmi)  derivative multi-index
  integer,  allocatable :: misum(:)       ! (nmi)    |alpha|

  !--- volume rule -----------------------------------------------------
  real(dp), allocatable :: xv(:,:)        ! (3,nqv)
  real(dp), allocatable :: wv(:)          ! (nqv)     sum = 1
  real(dp), allocatable :: bv(:,:)        ! (nb,nqv)  b_a(x_q)
  real(dp), allocatable :: dbv(:,:,:)     ! (nb,3,nqv) d b_a/d x_m (PHYSICAL)

  !--- face rules ------------------------------------------------------
  real(dp), allocatable :: xf(:,:,:,:)    ! (3,nqf,2,3)
  real(dp), allocatable :: wf(:)          ! (nqf)     sum = 1
  real(dp), allocatable :: bf(:,:,:,:)    ! (nb,nqf,2,3)
  real(dp), allocatable :: dbf(:,:,:,:,:) ! (nb,3,nqf,2,3)  first derivatives
  real(dp), allocatable :: dabf(:,:,:,:,:)! (nb,nmi,nqf,2,3) all |alpha|<=k

  !--- 1D rule kept for the PP limiter ---------------------------------
  real(dp), allocatable :: g1x(:), g1w(:)

contains

  !---------------------------------------------------------------------
  subroutine build_basis()
    integer  :: i1, i2, i3, m, s, q, r, cnt, imax, idx(3), lev
    real(dp) :: xi(3), scal
    real(dp), allocatable :: gx(:), gw(:)

    imax = kdeg

    !--- basis exponents ------------------------------------------------
    cnt = 0
    do i3 = 0, merge(imax,0,nd>=3)
       do i2 = 0, merge(imax,0,nd>=2)
          do i1 = 0, imax
             if (keep_mode(i1,i2,i3)) cnt = cnt + 1
          end do
       end do
    end do
    nb = cnt
    if (allocated(ideg)) deallocate(ideg, idsum)
    allocate(ideg(3,nb), idsum(nb))
    cnt = 0
    do i3 = 0, merge(imax,0,nd>=3)
       do i2 = 0, merge(imax,0,nd>=2)
          do i1 = 0, imax
             if (keep_mode(i1,i2,i3)) then
                cnt = cnt + 1
                ideg(:,cnt) = (/ i1, i2, i3 /)
                idsum(cnt)  = i1 + i2 + i3
             end if
          end do
       end do
    end do

    !--- derivative multi-indices, ordered by |alpha| -------------------
    cnt = 0
    do lev = 0, kdeg
       do i3 = 0, merge(lev,0,nd>=3)
          do i2 = 0, merge(lev,0,nd>=2)
             do i1 = 0, lev
                if (i1+i2+i3 == lev) cnt = cnt + 1
             end do
          end do
       end do
    end do
    nmi = cnt
    if (allocated(mi)) deallocate(mi, misum)
    allocate(mi(3,nmi), misum(nmi))
    cnt = 0
    do lev = 0, kdeg
       do i3 = 0, merge(lev,0,nd>=3)
          do i2 = 0, merge(lev,0,nd>=2)
             do i1 = 0, lev
                if (i1+i2+i3 == lev) then
                   cnt = cnt + 1
                   mi(:,cnt)  = (/ i1, i2, i3 /)
                   misum(cnt) = lev
                end if
             end do
          end do
       end do
    end do

    !--- Gauss rule exact for degree 2k+1 (needed by (3.21),(3.23)) ----
    nq1 = kdeg + 2
    allocate(gx(nq1), gw(nq1))
    call gauss_legendre(nq1, gx, gw)
    if (allocated(g1x)) deallocate(g1x, g1w)
    allocate(g1x(nq1), g1w(nq1))
    g1x = gx ; g1w = gw

    nqv = nq1**nd
    nqf = nq1**(nd-1)

    if (allocated(xv))   deallocate(xv, wv, bv, dbv)
    allocate(xv(3,nqv), wv(nqv), bv(nb,nqv), dbv(nb,3,nqv))
    if (allocated(xf))   deallocate(xf, wf, bf, dbf)
    allocate(xf(3,nqf,2,3), wf(nqf), bf(nb,nqf,2,3), dbf(nb,3,nqf,2,3))
    if (allocated(dabf)) deallocate(dabf)
    allocate(dabf(nb,nmi,nqf,2,3))

    xv = zero ; wv = zero ; bv = zero ; dbv = zero
    xf = zero ; wf = zero ; bf = zero ; dbf = zero ; dabf = zero

    !--- volume points --------------------------------------------------
    q = 0
    do i3 = 1, merge(nq1,1,nd>=3)
       do i2 = 1, merge(nq1,1,nd>=2)
          do i1 = 1, nq1
             q = q + 1
             idx = (/ i1, i2, i3 /)
             wv(q) = one
             do m = 1, nd
                xv(m,q) = gx(idx(m))
                wv(q)   = wv(q)*half*gw(idx(m))
             end do
             call fill_basis(xv(:,q), bv(:,q), dbv(:,:,q))
          end do
       end do
    end do

    !--- face points ----------------------------------------------------
    do m = 1, nd
       do s = 1, 2
          q = 0
          do i3 = 1, merge(nq1,1,nd>=3)
             do i2 = 1, merge(nq1,1,nd>=2)
                do i1 = 1, nq1
                   idx = (/ i1, i2, i3 /)
                   if (idx(m) /= 1) cycle
                   q = q + 1
                   xi = zero ; scal = one
                   do r = 1, nd
                      if (r == m) then
                         xi(r) = merge(-one, one, s == 1)
                      else
                         xi(r) = gx(idx(r))
                         scal  = scal*half*gw(idx(r))
                      end if
                   end do
                   xf(:,q,s,m) = xi
                   wf(q)       = scal
                   call fill_basis(xi, bf(:,q,s,m), dbf(:,:,q,s,m))
                   call fill_dalpha(xi, dabf(:,:,q,s,m))
                end do
             end do
          end do
       end do
    end do

    deallocate(gx, gw)

  contains

    logical function keep_mode(j1,j2,j3)
      integer, intent(in) :: j1,j2,j3
      if (basis_type == 'P' .or. basis_type == 'p') then
         keep_mode = (j1+j2+j3 <= kdeg)
      else
         keep_mode = (max(j1,max(j2,j3)) <= kdeg)
      end if
    end function keep_mode

  end subroutine build_basis

  !---------------------------------------------------------------------
  !  b_a(xi) and the PHYSICAL first derivatives  d b_a / d x_m
  !  (chain rule:  d/dx_m = (2/h_m) d/dxi_m)
  !---------------------------------------------------------------------
  subroutine fill_basis(xi, b, db)
    real(dp), intent(in)  :: xi(3)
    real(dp), intent(out) :: b(nb), db(nb,3)
    real(dp), allocatable :: t(:,:,:), tab(:,:)
    integer :: aa, mm, rr
    allocate(t(0:kdeg,0:1,3), tab(0:kdeg,0:1))
    t = zero
    do mm = 1, nd
       call leg_table(kdeg, 1, xi(mm), tab)
       t(0:kdeg,0:1,mm) = tab(0:kdeg,0:1)
    end do
    db = zero
    do aa = 1, nb
       b(aa) = one
       do mm = 1, nd
          b(aa) = b(aa)*t(ideg(mm,aa),0,mm)
       end do
       do mm = 1, nd
          db(aa,mm) = t(ideg(mm,aa),1,mm)*two*dxi(mm)
          do rr = 1, nd
             if (rr /= mm) db(aa,mm) = db(aa,mm)*t(ideg(rr,aa),0,rr)
          end do
       end do
    end do
    deallocate(t, tab)
  end subroutine fill_basis

  !---------------------------------------------------------------------
  !  ALL physical partial derivatives up to total order k :
  !
  !     dab(a,ii) = d^{mi(:,ii)} b_a (xi)
  !               = prod_m  [ d^{alpha_m} L_{ideg(m,a)} / dxi_m^{alpha_m} ]
  !                          * (2/h_m)^{alpha_m}
  !
  !  Verified against nested central differences to 1e-12 relative.
  !---------------------------------------------------------------------
  subroutine fill_dalpha(xi, dab)
    real(dp), intent(in)  :: xi(3)
    real(dp), intent(out) :: dab(nb,nmi)
    real(dp), allocatable :: t(:,:,:), tab(:,:)
    real(dp) :: v, jac
    integer  :: aa, ii, mm, ar
    allocate(t(0:kdeg,0:kdeg,3), tab(0:kdeg,0:kdeg))
    t = zero
    do mm = 1, nd
       call leg_table(kdeg, kdeg, xi(mm), tab)
       t(0:kdeg,0:kdeg,mm) = tab(0:kdeg,0:kdeg)
    end do
    do ii = 1, nmi
       do aa = 1, nb
          v = one
          do mm = 1, nd
             ar = mi(mm,ii)
             if (ar > ideg(mm,aa)) then
                v = zero            ! derivative of order > degree vanishes
                exit
             end if
             jac = (two*dxi(mm))**ar
             v = v*t(ideg(mm,aa),ar,mm)*jac
          end do
          dab(aa,ii) = v
       end do
    end do
    deallocate(t, tab)
  end subroutine fill_dalpha

  !---------------------------------------------------------------------
  pure real(dp) function eval_v(c, q)
    real(dp), intent(in) :: c(:)
    integer,  intent(in) :: q
    eval_v = dot_product(c(1:nb), bv(1:nb,q))
  end function eval_v

  pure real(dp) function eval_f(c, q, s, m)
    real(dp), intent(in) :: c(:)
    integer,  intent(in) :: q, s, m
    eval_f = dot_product(c(1:nb), bf(1:nb,q,s,m))
  end function eval_f

  !  physical gradient component r of a modal field at a volume point
  pure real(dp) function eval_dv(c, q, r)
    real(dp), intent(in) :: c(:)
    integer,  intent(in) :: q, r
    eval_dv = dot_product(c(1:nb), dbv(1:nb,r,q))
  end function eval_dv

  pure real(dp) function eval_df(c, q, s, m, r)
    real(dp), intent(in) :: c(:)
    integer,  intent(in) :: q, s, m, r
    eval_df = dot_product(c(1:nb), dbf(1:nb,r,q,s,m))
  end function eval_df

  !  d^{mi(:,ii)} of a modal field at a face quadrature point
  pure real(dp) function eval_dab(c, ii, q, s, m)
    real(dp), intent(in) :: c(:)
    integer,  intent(in) :: ii, q, s, m
    eval_dab = dot_product(c(1:nb), dabf(1:nb,ii,q,s,m))
  end function eval_dab

  !---------------------------------------------------------------------
  !  L2 projection P of (3.3).  With the diagonal mass matrix,
  !      c_a = (1/|K|) int_K f b_a = sum_q w_q f_q b_a(x_q)
  !---------------------------------------------------------------------
  pure subroutine project_nodal(fq, c)
    real(dp), intent(in)  :: fq(:)
    real(dp), intent(out) :: c(:)
    integer :: a, q
    do a = 1, nb
       c(a) = zero
       do q = 1, nqv
          c(a) = c(a) + wv(q)*fq(q)*bv(a,q)
       end do
    end do
  end subroutine project_nodal

  !---------------------------------------------------------------------
  subroutine destroy_basis()
    if (allocated(ideg)) deallocate(ideg, idsum)
    if (allocated(mi))   deallocate(mi, misum)
    if (allocated(xv))   deallocate(xv, wv, bv, dbv)
    if (allocated(xf))   deallocate(xf, wf, bf, dbf)
    if (allocated(dabf)) deallocate(dabf)
    if (allocated(g1x))  deallocate(g1x, g1w)
    nb = 0 ; nqv = 0 ; nqf = 0 ; nmi = 0
  end subroutine destroy_basis

end module mod_basis
