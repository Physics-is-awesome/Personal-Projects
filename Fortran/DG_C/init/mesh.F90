!=======================================================================
!  mod_mesh : uniform tensor-product Cartesian partition
!             T_h = { K }  of  Omega = prod_m [xlo(m), xhi(m)]   (Sec. 3.1)
!
!  The paper computes "on the tensor mesh" (Sec. 3.3), so every cell is
!  a box of size h_1 x ... x h_d.  All face loops of (3.9), (3.17) and
!  (3.24) are driven by the neighbour table built here.
!=======================================================================
module mod_mesh
  use mod_kinds
  use mod_params
  implicit none
  public

  integer  :: nx(3)   = 1
  integer  :: ncell   = 0
  real(dp) :: dx(3)   = one
  real(dp) :: dxi(3)  = one
  real(dp) :: cvol    = one          ! |K|
  real(dp) :: hmin    = one
  real(dp) :: hdiam   = one          ! diam(K)
  real(dp) :: hglob   = one          ! h = max_K h_K   (time step, Sec. 5)
  real(dp) :: dlen(3) = one
  real(dp) :: domvol  = one

  !  nbr(s,m,ic) : neighbour of ic across the face with outward normal
  !                (-1)^s e_m ;  s = 1 -> "minus" side, s = 2 -> "plus".
  !                0 flags a physical (non periodic) boundary face.
  integer,  allocatable :: nbr(:,:,:)
  integer,  allocatable :: cijk(:,:)
  real(dp), allocatable :: xcen(:,:)

contains

  pure integer function cid(i,j,k)
    integer, intent(in) :: i,j,k
    cid = i + nx(1)*((j-1) + nx(2)*(k-1))
  end function cid

  !---------------------------------------------------------------------
  subroutine build_mesh()
    integer :: i, j, k, m, ic, s, ii(3), jj(3)

    nx = 1
    do m = 1, nd
       nx(m) = max(1, nxyz(m))
    end do
    ncell = nx(1)*nx(2)*nx(3)

    dx = one ; dxi = one ; dlen = one
    do m = 1, nd
       dlen(m) = xhi(m) - xlo(m)
       dx(m)   = dlen(m)/real(nx(m),dp)
       dxi(m)  = one/dx(m)
    end do
    cvol   = product(dx(1:nd))
    domvol = product(dlen(1:nd))
    hmin   = minval(dx(1:nd))
    hdiam  = sqrt(sum(dx(1:nd)**2))
    hglob  = merge(hdiam, hmin, hK_diam)

    if (allocated(nbr))  deallocate(nbr)
    if (allocated(cijk)) deallocate(cijk)
    if (allocated(xcen)) deallocate(xcen)
    allocate(nbr(2,3,ncell), cijk(3,ncell), xcen(3,ncell))
    nbr = 0

    do k = 1, nx(3)
       do j = 1, nx(2)
          do i = 1, nx(1)
             ic = cid(i,j,k)
             cijk(:,ic) = (/ i, j, k /)
             xcen(:,ic) = zero
             do m = 1, nd
                xcen(m,ic) = xlo(m) + (real(cijk(m,ic),dp) - half)*dx(m)
             end do
          end do
       end do
    end do

    do ic = 1, ncell
       ii = cijk(:,ic)
       do m = 1, nd
          do s = 1, 2
             jj = ii
             jj(m) = ii(m) + merge(-1, 1, s == 1)
             if (jj(m) < 1 .or. jj(m) > nx(m)) then
                if (is_periodic) then
                   jj(m) = modulo(jj(m)-1, nx(m)) + 1
                   nbr(s,m,ic) = cid(jj(1),jj(2),jj(3))
                else
                   nbr(s,m,ic) = 0
                end if
             else
                nbr(s,m,ic) = cid(jj(1),jj(2),jj(3))
             end if
          end do
       end do
    end do
  end subroutine build_mesh

  !---------------------------------------------------------------------
  !  |e| for a face whose normal points along direction m :  |K|/h_m
  !---------------------------------------------------------------------
  pure real(dp) function face_area(m)
    integer, intent(in) :: m
    face_area = cvol*dxi(m)
  end function face_area

  !---------------------------------------------------------------------
  !  h_{e,K} = sup_{x in K} dist(x,e)  =  h_m  for a face normal to e_m
  !---------------------------------------------------------------------
  pure real(dp) function h_eK(m)
    integer, intent(in) :: m
    h_eK = dx(m)
  end function h_eK

  !---------------------------------------------------------------------
  pure subroutine ref2phys(ic, xi, x)
    integer,  intent(in)  :: ic
    real(dp), intent(in)  :: xi(3)
    real(dp), intent(out) :: x(3)
    integer :: m
    x = zero
    do m = 1, nd
       x(m) = xcen(m,ic) + half*dx(m)*xi(m)
    end do
  end subroutine ref2phys

  subroutine destroy_mesh()
    if (allocated(nbr))  deallocate(nbr)
    if (allocated(cijk)) deallocate(cijk)
    if (allocated(xcen)) deallocate(xcen)
    ncell = 0
  end subroutine destroy_mesh

end module mod_mesh
