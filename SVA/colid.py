import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from IPython.display import HTML

# --- Parameters ---
G = 6.67430e-11   # gravitational constant
M_earth = 5.972e24  # mass of Earth (kg)
R_earth = 6.371e6   # radius of Earth (m)

# Initial asteroid state: [x, y, vx, vy]
# Start far away, moving toward Earth
y = np.array([10*R_earth, 3*R_earth, -2000.0, -500.0])  # position (m), velocity (m/s)

dt = 10.0   # timestep (s)
steps = 2000

# --- Equations of motion ---
def deriv(y):
    x, y_pos, vx, vy = y
    r = np.sqrt(x**2 + y_pos**2)
    ax = -G*M_earth*x / r**3
    ay = -G*M_earth*y_pos / r**3
    return np.array([vx, vy, ax, ay])

# --- Runge-Kutta 4 integrator ---
def rk4_step(y, dt):
    k1 = deriv(y)
    k2 = deriv(y + 0.5*dt*k1)
    k3 = deriv(y + 0.5*dt*k2)
    k4 = deriv(y + dt*k3)
    return y + (dt/6.0)*(k1 + 2*k2 + 2*k3 + k4)

# --- Integrate trajectory ---
trajectory = []
for _ in range(steps):
    y = rk4_step(y, dt)
    trajectory.append(y)
trajectory = np.array(trajectory)

x_vals = trajectory[:,0]
y_vals = trajectory[:,1]

# --- Animate ---
fig, ax = plt.subplots()
ax.set_xlim(-12*R_earth, 12*R_earth)
ax.set_ylim(-12*R_earth, 12*R_earth)
ax.set_aspect('equal')
ax.set_title("Asteroid Approaching Earth")

# Draw Earth
earth = plt.Circle((0,0), R_earth, color='blue', alpha=0.5)
ax.add_patch(earth)

line, = ax.plot([], [], '-', lw=1, color='orange')
point, = ax.plot([], [], 'o', color='red')

def init():
    line.set_data([], [])
    point.set_data([], [])
    return line, point

def update(i):
    line.set_data(x_vals[:i], y_vals[:i])
    point.set_data(x_vals[i], y_vals[i])
    return line, point

ani = animation.FuncAnimation(fig, update, frames=steps,
                              init_func=init, blit=True, interval=20)

HTML(ani.to_jshtml())
