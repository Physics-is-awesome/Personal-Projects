import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from pathlib import Path
import re

# ============================================================
# Configuration
# ============================================================

DATA_DIR = Path(".")

VARIABLE = "rho"

OUTPUT = "density.mp4"

# ============================================================
# Find snapshot files
# ============================================================

files = []

for f in DATA_DIR.glob("expl3d_sp_*.dat"):
    if re.match(r"^expl3d_sp_(\d+|final)_30\.dat$", f.name):
        files.append(f)

if not files:
    raise RuntimeError("No snapshot files found.")

# ============================================================
# Read a snapshot
# ============================================================

def read_dat(filename):

    with open(filename, "r") as f:
        lines = f.readlines()

    time = None

    for line in lines:
        if line.startswith("# time"):
            time = float(line.split("=")[1])
            break

    data = np.loadtxt(filename, comments="#")

    if data.shape[1] != 9:
        raise RuntimeError(
            f"{filename} has {data.shape[1]} columns"
        )

    x   = data[:, 0]
    y   = data[:, 1]
    z   = data[:, 2]
    rho = data[:, 3]
    u   = data[:, 4]
    v   = data[:, 5]
    w   = data[:, 6]
    p   = data[:, 7]
    phi = data[:, 8]

    variables = {
        "x": x,
        "y": y,
        "z": z,
        "rho": rho,
        "u": u,
        "v": v,
        "w": w,
        "p": p,
        "phi": phi,
    }

    return time, variables


# ============================================================
# Read all snapshots
# ============================================================

snapshots = []

for filename in files:

    time, variables = read_dat(filename)

    snapshots.append({
        "time": time,
        "data": variables
    })

# Sort by actual simulation time
snapshots.sort(key=lambda s: s["time"])

print(f"Found {len(snapshots)} snapshots.")

for s in snapshots:
    print(f"t = {s['time']:.8e}")


# ============================================================
# Determine grid
# ============================================================

first = snapshots[0]["data"]

x = first["x"]
y = first["y"]
z = first["z"]

x_unique = np.unique(x)
y_unique = np.unique(y)
z_unique = np.unique(z)

nx = len(x_unique)
ny = len(y_unique)
nz = len(z_unique)

print()
print(f"Grid:")
print(f"nx = {nx}")
print(f"ny = {ny}")
print(f"nz = {nz}")
print(f"Total points = {nx * ny * nz}")
print(f"Data points  = {len(x)}")


# ============================================================
# Convert a 3D point array into a 3D field
# ============================================================

def make_grid(data):

    # Create empty 3D array
    field = np.empty((nx, ny, nz))

    # Find grid indices
    ix = np.searchsorted(x_unique, data["x"])
    iy = np.searchsorted(y_unique, data["y"])
    iz = np.searchsorted(z_unique, data["z"])

    field[ix, iy, iz] = data[VARIABLE]

    return field


# ============================================================
# Construct all fields
# ============================================================

fields = []

for snapshot in snapshots:

    field = make_grid(snapshot["data"])

    fields.append(field)


# ============================================================
# Choose a z slice
# ============================================================

z_index = nz // 2

print(f"Using z slice:")
print(f"z = {z_unique[z_index]}")


# ============================================================
# Global color limits
# ============================================================

vmin = min(np.min(field) for field in fields)
vmax = max(np.max(field) for field in fields)

print(f"{VARIABLE} range:")
print(f"min = {vmin}")
print(f"max = {vmax}")


# ============================================================
# Create figure
# ============================================================

fig, ax = plt.subplots(figsize=(8, 7))

initial = fields[0][:, :, z_index]

image = ax.imshow(
    initial.T,
    origin="lower",
    extent=[
        x_unique.min(),
        x_unique.max(),
        y_unique.min(),
        y_unique.max()
    ],
    aspect="equal",
    vmin=vmin,
    vmax=vmax,
    interpolation="bilinear"
)

colorbar = fig.colorbar(image, ax=ax)
colorbar.set_label(VARIABLE)

title = ax.set_title(
    f"{VARIABLE}, t = {snapshots[0]['time']:.6e}"
)

ax.set_xlabel("x")
ax.set_ylabel("y")


# ============================================================
# Animation
# ============================================================

def update(frame):

    image.set_data(
        fields[frame][:, :, z_index].T
    )

    title.set_text(
        f"{VARIABLE}, "
        f"t = {snapshots[frame]['time']:.6e}"
    )

    return image, title


animation = FuncAnimation(
    fig,
    update,
    frames=len(fields),
    interval=100,
    blit=True
)


# ============================================================
# Save animation
# ============================================================

animation.save(
    OUTPUT,
    writer="ffmpeg",
    fps=10
)

print()
print(f"Saved animation to {OUTPUT}")

plt.show()

