import glob
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# --- User settings ---
script_dir = os.path.dirname(os.path.abspath(__file__))  # src/visual
data_dir = os.path.abspath(os.path.join(script_dir, "../../makefile/output"))
file_pattern = "fields_t*.dat"  # File pattern
movie_filename = "simulation.mp4"
final_image_filename = "final_field.png"

# --- Find all output files ---
file_list = sorted(glob.glob(os.path.join(data_dir, "fields_t*.dat")))

if not file_list:
    raise FileNotFoundError(f"No files found in {data_dir} matching {file_pattern}")

# --- Load first file to get grid size ---
data0 = np.loadtxt(file_list[0])
nx = data0.shape[0]

# --- Prepare figure ---
fig, ax = plt.subplots()
line, = ax.plot(np.zeros(nx))
ax.set_xlim(0, nx-1)
ax.set_ylim(np.min(data0), np.max(data0))
ax.set_xlabel("Grid index")
ax.set_ylabel("Field value")
ax.set_title("Simulation")

# --- Animation function ---
def update(frame):
    data = np.loadtxt(file_list[frame])
    line.set_ydata(data)
    ax.set_ylim(np.min(data), np.max(data))  # Optional: dynamic y-scale
    ax.set_title(f"Time step {frame}")
    return line,

# --- Create animation ---
ani = animation.FuncAnimation(fig, update, frames=len(file_list), blit=True)

# --- Save movie ---
ani.save(movie_filename, writer='ffmpeg', fps=10)
print(f"Movie saved as {movie_filename}")

# --- Plot final field as image ---
final_data = np.loadtxt(file_list[-1])
plt.figure()
plt.plot(final_data)
plt.xlabel("Grid index")
plt.ylabel("Field value")
plt.title("Final field")
plt.grid(True)
plt.savefig(final_image_filename)
print(f"Final field image saved as {final_image_filename}")
plt.show()
