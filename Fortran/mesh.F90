module mesh_mod
  !
  ! Minimal mesh topology module for 3D tetrahedral volume mesh.
  !
  ! Stores only:
  !   - node coordinates
  !   - element connectivity (tetrahedra)
  !
  ! Derived quantities (volumes, Jacobians, face data, etc.) are computed
  ! by the geometry module, not stored here.
  !
  use iso_fortran_env, only: real64
  implicit none
  private

  integer, parameter, public :: dp = real64

  type, public :: mesh_t
     integer :: nnode = 0
     integer :: nelem = 0
     real(dp), allocatable :: x(:,:)     ! x(i,1:3): coordinates of node i, i=1..nnode
     integer, allocatable :: conn(:,:)   ! conn(e,1:4): tetrahedron node indices, e=1..nelem
  end type mesh_t

  public :: create_mesh_from_gmsh

contains

  subroutine create_mesh_from_gmsh(mesh, filename)
    !
    ! Read a tetrahedral mesh from a Gmsh .msh file (MSH2 format).
    !
    ! This routine parses the Gmsh ASCII format and extracts:
    !   - node coordinates
    !   - tetrahedral element connectivity (tag 4 in Gmsh)
    !
    type(mesh_t), intent(inout) :: mesh
    character(len=*), intent(in) :: filename

    integer :: io_unit, ios, nnode_file, nelem_file, i, e, tag, elem_type, nelem_tags
    integer :: n1, n2, n3, n4, gid
    real(dp) :: xx, yy, zz
    integer, allocatable :: node_id(:), elem_id(:)

    open(newunit=io_unit, file=trim(filename), status='old', action='read', iostat=ios)
    if (ios /= 0) then
       print *, 'Error: Cannot open mesh file ', trim(filename)
       stop
    end if

    ! Read header and skip until $Nodes section
    call skip_to_section(io_unit, '$Nodes')

    read(io_unit, *) nnode_file
    allocate(node_id(nnode_file))
    allocate(mesh%x(nnode_file, 3))

    do i = 1, nnode_file
       read(io_unit, *) gid, xx, yy, zz
       node_id(i) = gid
       mesh%x(i, 1) = xx
       mesh%x(i, 2) = yy
       mesh%x(i, 3) = zz
    end do
    mesh%nnode = nnode_file

    ! Read elements section
    call skip_to_section(io_unit, '$Elements')

    read(io_unit, *) nelem_file
    allocate(elem_id(nelem_file))

    ! First pass: count tetrahedra (Gmsh element type 4)
    mesh%nelem = 0
    do e = 1, nelem_file
       read(io_unit, *) gid, elem_type
       if (elem_type == 4) mesh%nelem = mesh%nelem + 1
    end do

    ! Allocate connectivity
    allocate(mesh%conn(mesh%nelem, 4))

    ! Second pass: read tetrahedra
    rewind(io_unit)
    call skip_to_section(io_unit, '$Nodes')
    read(io_unit, *) nnode_file
    do i = 1, nnode_file
       read(io_unit, *)
    end do

    call skip_to_section(io_unit, '$Elements')
    read(io_unit, *) nelem_file

    e = 0
    do i = 1, nelem_file
       read(io_unit, *) gid, elem_type, nelem_tags, &
            (tag, tag=1,nelem_tags), n1, n2, n3, n4
       if (elem_type == 4) then
          e = e + 1
          mesh%conn(e, 1) = n1
          mesh%conn(e, 2) = n2
          mesh%conn(e, 3) = n3
          mesh%conn(e, 4) = n4
       end if
    end do

    close(io_unit)
    deallocate(node_id, elem_id)

    print *, 'Mesh read successfully from ', trim(filename)
    print *, '  Nodes: ', mesh%nnode
    print *, '  Tetrahedra: ', mesh%nelem

  end subroutine create_mesh_from_gmsh

  subroutine skip_to_section(io_unit, section_name)
    integer, intent(in) :: io_unit
    character(len=*), intent(in) :: section_name
    character(len=256) :: line

    do
       read(io_unit, '(A)', end=999) line
       if (trim(line) == trim(section_name)) return
    end do

999 print *, 'Error: Section ', trim(section_name), ' not found in mesh file'
    stop

  end subroutine skip_to_section

end module mesh_mod
