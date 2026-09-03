module vtk_output
  use precision
  use init
  implicit none

contains

  !==============================================================
  ! Write one snapshot of the system as an XML VTK PolyData file
  ! (.vtp). Each body is a point (with a Verts block so ParaView
  ! renders them directly, e.g. via a Glyph filter), with per-point
  ! mass and body_index as scalar data.
  !
  ! NOTE: this must be .vtp (XML PolyData), not legacy .vtk. The
  ! .pvd collection reader determines each dataset's type from its
  ! file extension, and only recognizes the XML VTK formats
  ! (.vtp/.vtu/.vti/...) for that -- legacy .vtk doesn't self-declare
  ! its type via extension, so a .pvd referencing .vtk files fails
  ! with "Could not determine the data type for the first dataset."
  !
  ! Operates on state%q (Cartesian) -- caller is responsible for
  ! having called jacobi_to_cartesian(state) first if the state's
  ! master representation is currently qj/pj (see note in main.F90:
  ! the main loop doesn't keep q/p synced every step for speed, so
  ! it's synced only at the cadence frames are actually written).
  !==============================================================

  subroutine write_vtk_frame(state, filename)

    type(nbody_state), intent(in) :: state
    character(len=*),  intent(in) :: filename

    integer :: unit, i

    open(newunit=unit, file=filename, status='replace', action='write', form='formatted')

    write(unit,'(A)') '<?xml version="1.0"?>'
    write(unit,'(A)') '<VTKFile type="PolyData" version="0.1" byte_order="LittleEndian">'
    write(unit,'(A)') '  <PolyData>'
    write(unit,'(A,I0,A,I0,A)') &
      '    <Piece NumberOfPoints="', state%n, &
      '" NumberOfVerts="', state%n, &
      '" NumberOfLines="0" NumberOfStrips="0" NumberOfPolys="0">'

    write(unit,'(A)') '      <PointData Scalars="mass">'

    write(unit,'(A)') '        <DataArray type="Float64" Name="mass" format="ascii">'
    do i = 1, state%n
      write(unit,'(ES20.10)') state%mass(i)
    end do
    write(unit,'(A)') '        </DataArray>'

    write(unit,'(A)') '        <DataArray type="Int32" Name="body_index" format="ascii">'
    do i = 1, state%n
      write(unit,'(I0)') i
    end do
    write(unit,'(A)') '        </DataArray>'

    write(unit,'(A)') '      </PointData>'

    write(unit,'(A)') '      <Points>'
    write(unit,'(A)') '        <DataArray type="Float64" NumberOfComponents="3" format="ascii">'
    do i = 1, state%n
      write(unit,'(ES20.10,1X,ES20.10,1X,ES20.10)') state%q(1,i), state%q(2,i), state%q(3,i)
    end do
    write(unit,'(A)') '        </DataArray>'
    write(unit,'(A)') '      </Points>'

    write(unit,'(A)') '      <Verts>'
    write(unit,'(A)') '        <DataArray type="Int32" Name="connectivity" format="ascii">'
    do i = 1, state%n
      write(unit,'(I0)') i-1
    end do
    write(unit,'(A)') '        </DataArray>'
    write(unit,'(A)') '        <DataArray type="Int32" Name="offsets" format="ascii">'
    do i = 1, state%n
      write(unit,'(I0)') i
    end do
    write(unit,'(A)') '        </DataArray>'
    write(unit,'(A)') '      </Verts>'

    write(unit,'(A)') '    </Piece>'
    write(unit,'(A)') '  </PolyData>'
    write(unit,'(A)') '</VTKFile>'

    close(unit)

  end subroutine write_vtk_frame


  !==============================================================
  ! Write the .pvd collection file that ties a set of per-frame VTK
  ! files to their physical simulation times. Open this file (not
  ! the individual .vtk files) in ParaView -- it gives you the
  ! animation time slider with correct physical time, rather than
  ! just a bare frame index.
  !==============================================================

  subroutine write_pvd(times, filenames, n_frames, pvd_filename)

    real(real64),     intent(in) :: times(:)
    character(len=*), intent(in) :: filenames(:)
    integer,          intent(in) :: n_frames
    character(len=*), intent(in) :: pvd_filename

    integer :: unit, k

    open(newunit=unit, file=pvd_filename, status='replace', action='write', form='formatted')

    write(unit,'(A)') '<?xml version="1.0"?>'
    write(unit,'(A)') '<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">'
    write(unit,'(A)') '  <Collection>'

    do k = 1, n_frames
      write(unit,'(A,ES16.8,A,A,A)') &
        '    <DataSet timestep="', times(k), '" group="" part="0" file="', &
        trim(filenames(k)), '"/>'
    end do

    write(unit,'(A)') '  </Collection>'
    write(unit,'(A)') '</VTKFile>'

    close(unit)

  end subroutine write_pvd

end module vtk_output
