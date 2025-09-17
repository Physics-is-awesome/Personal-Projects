import os
import glob
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# -----------------------------
# 1. Locate output folder
# -----------------------------
script_dir = os.path.dirname(os.path.abspath(__file__))  # src/visual
data_dir = os.path.abspath(os.path.join(script_dir, "../../makefile/output"))

if not os.path.exists(data_dir):
    raise FileNotFoundError(f"Output folder not found: {data_dir}")
print(f"Using data folder: {data_dir}")

# -----------------------------
# 2. Find all field files
# -----------------------------
file_list = sorted(glob.glob(os.path.join(data_dir, "fields_t*.dat")))

if not file_list:
    raise FileNotFoundError(f"No fields_t*.dat files found in {data_dir}")
print(f"Found {len(file_list)} field files.")

# -----------------------------
# 3. Prepare figure for animation
# -----------------------------
fig, ax = plt.subplots()
ax.set_xlabel("Grid index")
ax.set_ylabel("Field value")
ax.set_title("Simulation")

def update(frame):
    ax.clear()  # clear previous lines
    data = np.loadtxt(file_list[frame])
    x = np.arange(len(data))
    ax.plot(x, data, color='blue')
    ax.set_xlim(0, len(data)-1)
    ax.set_ylim(np.min(data), np.max(data))
    ax.set_xlabel("Grid index")
    ax.set_ylabel("Field value")
    ax.set_title(f"Time step {frame}")
    ax.grid(True)

# -----------------------------
# 4. Create and save animation
# -----------------------------
movie_filename = os.path.join(data_dir, "simulation.mp4")
ani = animation.FuncAnimation(fig, update, frames=len(file_list))
ani.save(movie_filename, writer='ffmpeg', fps=10)
print(f"Movie saved as {movie_filename}")

# -----------------------------
# 5. Plot final field
# -----------------------------
final_data = np.loadtxt(file_list[-1])
plt.figure()
plt.plot(np.arange(len(final_data)), final_data, color='red')
plt.xlabel("Grid index")
plt.ylabel("Field value")
plt.title("Final field")
plt.grid(True)
final_image_filename = os.path.join(data_dir, "final_field.png")
plt.savefig(final_image_filename)
print(f"Final field image saved as {final_image_filename}")
plt.show()

# -----------------------------
# 6. Optional: Plot conserved quantities
# -----------------------------
conserved_file = os.path.join(data_dir, "conserved.dat")
if os.path.exists(conserved_file):
    data = np.loadtxt(conserved_file, comments="#")
    time = data[:, 0]
    H = data[:, 1]
    S = data[:, 2]

    plt.figure()
    plt.plot(time, H, label="Hamiltonian H")
    plt.plot(time, S, label="Entropy S")
    plt.xlabel("Time")
    plt.ylabel("Value")
    plt.title("Conserved Quantities")
    plt.legend()
    plt.grid(True)
    conserved_image = os.path.join(data_dir, "conserved.png")
    plt.savefig(conserved_image)
    print(f"Conserved quantities image saved as {conserved_image}")
    plt.show()

