import numpy as np
import matplotlib.pyplot as plt

# Parameters
N = 50  # Grid points per dimension
L = 1.0  # Domain size
dx = L / N  # Grid spacing
dy = dx
dt = 0.001  # Time step
T = 0.2  # Total simulation time
gamma = 5/3  # Adiabatic index
mu0 = 1.0  # Normalized permeability
B0 = 1.0  # Background magnetic field
v0 = 0.1  # Velocity shear amplitude
epsilon = 0.5  # Density/pressure gradient amplitude
delta = 0.1  # Transition width for tanh

# Initialize 2D arrays
x = np.linspace(0, L, N, endpoint=False)
y = np.linspace(0, L, N, endpoint=False)
X, Y = np.meshgrid(x, y)
rho = 1.0 + epsilon * np.tanh((Y - 0.5) / delta)  # Nonuniform density
v_x = v0 * np.tanh((Y - 0.5) / delta)  # Shear velocity
v_y = np.zeros((N, N))  # No initial y-velocity
B_x = B0 * np.ones((N, N))  # Uniform B_x
B_y = 0.2 * np.tanh((Y - 0.5) / delta)  # Nonuniform B_y
P = 1.0 + epsilon * np.tanh((Y - 0.5) / delta)  # Nonuniform pressure
E = 0.5 * rho * (v_x**2 + v_y**2) + P / (gamma - 1) + (B_x**2 + B_y**2) / (2 * mu0)  # Total energy

# Flux calculation
def flux(rho, v_x, v_y, B_x, B_y, P, E):
    v2 = v_x**2 + v_y**2
    B2 = B_x**2 + B_y**2
    v_dot_B = v_x * B_x + v_y * B_y
    f_rho_x = rho * v_x
    f_rho_y = rho * v_y
    f_mom_x_x = rho * v_x**2 + P + 0.5 * B_y**2 / mu0 - 0.5 * B_x**2 / mu0
    f_mom_x_y = rho * v_x * v_y - B_x * B_y / mu0
    f_mom_y_x = rho * v_x * v_y - B_x * B_y / mu0
    f_mom_y_y = rho * v_y**2 + P + 0.5 * B_x**2 / mu0 - 0.5 * B_y**2 / mu0
    f_B_x_y = v_y * B_x - v_x * B_y
    f_B_y_x = v_x * B_y - v_y * B_x
    f_E_x = (0.5 * rho * v2 + gamma * P / (gamma - 1) + B2 / mu0) * v_x - (B_x * v_dot_B) / mu0
    f_E_y = (0.5 * rho * v2 + gamma * P / (gamma - 1) + B2 / mu0) * v_y - (B_y * v_dot_B) / mu0
    return f_rho_x, f_rho_y, f_mom_x_x, f_mom_x_y, f_mom_y_x, f_mom_y_y, f_B_x_y, f_B_y_x, f_E_x, f_E_y

# Time evolution
t = 0
while t < T:
    f_rho_x, f_rho_y, f_mom_x_x, f_mom_x_y, f_mom_y_x, f_mom_y_y, f_B_x_y, f_B_y_x, f_E_x, f_E_y = flux(rho, v_x, v_y, B_x, B_y, P, E)
    
    # Lax-Friedrichs scheme
    rho_new = np.zeros((N, N))
    v_x_new = np.zeros((N, N))
    v_y_new = np.zeros((N, N))
    B_x_new = np.zeros((N, N))
    B_y_new = np.zeros((N, N))
    E_new = np.zeros((N, N))
    
    for i in range(N):
        for j in range(N):
            i_left = (i - 1) % N
            i_right = (i + 1) % N
            j_down = (j - 1) % N
            j_up = (j + 1) % N
            
            # Update density
            rho_new[i, j] = 0.25 * (rho[i_left, j] + rho[i_right, j] + rho[i, j_down] + rho[i, j_up]) - \
                            0.5 * (dt / dx) * (f_rho_x[i_right, j] - f_rho_x[i_left, j]) - \
                            0.5 * (dt / dy) * (f_rho_y[i, j_up] - f_rho_y[i, j_down])
            
            # Update x-momentum
            v_x_new[i, j] = 0.25 * (v_x[i_left, j] + v_x[i_right, j] + v_x[i, j_down] + v_x[i, j_up]) - \
                            0.5 * (dt / dx) * (f_mom_x_x[i_right, j] - f_mom_x_x[i_left, j]) / rho[i, j] - \
                            0.5 * (dt / dy) * (f_mom_x_y[i, j_up] - f_mom_x_y[i, j_down]) / rho[i, j]
            
            # Update y-momentum
            v_y_new[i, j] = 0.25 * (v_y[i_left, j] + v_y[i_right, j] + v_y[i, j_down] + v_y[i, j_up]) - \
                            0.5 * (dt / dx) * (f_mom_y_x[i_right, j] - f_mom_y_x[i_left, j]) / rho[i, j] - \
                            0.5 * (dt / dy) * (f_mom_y_y[i, j_up] - f_mom_y_y[i, j_down]) / rho[i, j]
            
            # Update B_x
            B_x_new[i, j] = 0.25 * (B_x[i_left, j] + B_x[i_right, j] + B_x[i, j_down] + B_x[i, j_up]) - \
                            0.5 * (dt / dy) * (f_B_x_y[i, j_up] - f_B_x_y[i, j_down])
            
            # Update B_y
            B_y_new[i, j] = 0.25 * (B_y[i_left, j] + B_y[i_right, j] + B_y[i, j_down] + B_y[i, j_up]) - \
                            0.5 * (dt / dx) * (f_B_y_x[i_right, j] - f_B_y_x[i_left, j])
            
            # Update energy
            E_new[i, j] = 0.25 * (E[i_left, j] + E[i_right, j] + E[i, j_down] + E[i, j_up]) - \
                          0.5 * (dt / dx) * (f_E_x[i_right, j] - f_E_x[i_left, j]) - \
                          0.5 * (dt / dy) * (f_E_y[i, j_up] - f_E_y[i, j_down])
    
    # Update variables
    rho = rho_new.copy()
    v_x = v_x_new.copy()
    v_y = v_y_new.copy()
    B_x = B_x_new.copy()
    B_y = B_y_new.copy()
    E = E_new.copy()
    P = (gamma - 1) * (E - 0.5 * rho * (v_x**2 + v_y**2) - (B_x**2 + B_y**2) / (2 * mu0))
    t += dt

# Plot results
plt.figure(figsize=(10, 6))
plt.contourf(X, Y, P, cmap='viridis')
plt.colorbar(label='Pressure')
plt.xlabel('x')
plt.ylabel('y')
plt.title(f'MHD Simulation: Pressure at t = {t:.2f}')
plt.show()
