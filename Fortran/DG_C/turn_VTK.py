import numpy as np
import re
from pathlib import Path
import vtk
from vtk.util.numpy_support import numpy_to_vtk

DATA_DIR = Path(".")
OUT_DIR = Path("vti")
OUT_DIR.mkdir(exist_ok=True)

pattern = re.compile(r"^expl3d_sp_(\d+|final)_30\.dat$")


def get_time(filename):
    with open(filename) as f:
        for line in f:
            if line.startswith("# time"):
                return float(line.split("=")[1])
    return 0.0


def make_array(name, data, nx, ny, nz, xs, ys, zs):

    x = data[:, 0]
    y = data[:, 1]
    z = data[:, 2]

    ix = np.searchsorted(xs, x)
    iy = np.searchsorted(ys, y)
    iz = np.searchsorted(zs, z)

    field = np.empty((nx, ny, nz), dtype=np.float64)

    field[ix, iy, iz] = data

    vtk_array = numpy_to_vtk(
        field.ravel(order="F"),
        deep=True
    )

    vtk_array.SetName(name)

    return vtk_array


# ------------------------------------------------------------
# Find only actual time snapshots
# ------------------------------------------------------------

files = [
    f for f in DATA_DIR.glob("expl3d_sp_*.dat")
    if pattern.match(f.name)
]

files.sort(key=get_time)

print(f"Found {len(files)} snapshots")


# ------------------------------------------------------------
# Convert every snapshot
# ------------------------------------------------------------

for filename in files:

    print(f"Converting {filename.name}")

    data = np.loadtxt(filename, comments="#")

    x = data[:, 0]
    y = data[:, 1]
    z = data[:, 2]

    rho = data[:, 3]
    u   = data[:, 4]
    v   = data[:, 5]
    w   = data[:, 6]
    p   = data[:, 7]
    phi = data[:, 8]

    xs = np.unique(x)
    ys = np.unique(y)
    zs = np.unique(z)

    nx = len(xs)
    ny = len(ys)
    nz = len(zs)

    print(f"Grid = {nx} x {ny} x {nz}")

    # --------------------------------------------------------
    # Create structured Cartesian grid
    # --------------------------------------------------------

    grid = vtk.vtkImageData()

    grid.SetDimensions(nx, ny, nz)

    dx = xs[1] - xs[0] if nx > 1 else 1.0
    dy = ys[1] - ys[0] if ny > 1 else 1.0
    dz = zs[1] - zs[0] if nz > 1 else 1.0

    grid.SetOrigin(xs[0], ys[0], zs[0])
    grid.SetSpacing(dx, dy, dz)

    # --------------------------------------------------------
    # Add scalar fields
    # --------------------------------------------------------

    fields = {
        "density": rho,
        "u": u,
        "v": v,
        "w": w,
        "pressure": p,
        "potential": phi
    }

    for name, values in fields.items():

        ix = np.searchsorted(xs, x)
        iy = np.searchsorted(ys, y)
        iz = np.searchsorted(zs, z)

        field = np.empty((nx, ny, nz), dtype=np.float64)

        field[ix, iy, iz] = values

        vtk_field = numpy_to_vtk(
            field.ravel(order="F"),
            deep=True
        )

        vtk_field.SetName(name)

        grid.GetPointData().AddArray(vtk_field)

    # --------------------------------------------------------
    # Velocity vector
    # --------------------------------------------------------

    velocity = np.empty((nx, ny, nz, 3), dtype=np.float64)

    velocity[..., 0] = u.reshape((nx, ny, nz), order="F")
    velocity[..., 1] = v.reshape((nx, ny, nz), order="F")
    velocity[..., 2] = w.reshape((nx, ny, nz), order="F")

    vtk_velocity = numpy_to_vtk(
        velocity.reshape((-1, 3), order="F"),
        deep=True
    )

    vtk_velocity.SetName("velocity")

    grid.GetPointData().SetVectors(vtk_velocity)

    # --------------------------------------------------------
    # Write VTI
    # --------------------------------------------------------

    output = OUT_DIR / f"{filename.stem}.vti"

    writer = vtk.vtkXMLImageDataWriter()
    writer.SetFileName(str(output))
    writer.SetInputData(grid)

    # Let VTK handle the binary XML correctly
    writer.SetDataModeToBinary()

    writer.Write()

    print(f"  -> {output}")


print("Conversion complete.")
