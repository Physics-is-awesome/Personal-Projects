import numpy as np
import h5py
from scipy.integrate import solve_ivp
import os

# ==========================================
# 1. Constants & Initial Conditions
# ==========================================
G = 4.0 * np.pi**2       # Gravitational constant in AU^3 / (M_sun * yr^2)
M_sun = 1.0              # Solar mass
C_EXAGGERATED = 100.0    # Speed of light in AU/yr (Artificially lowered to make precession visible)

# Planet initial states: [x, y, z, vx, vy, vz]
# Mercury-like (Inner orbit, high eccentricity)
mercury = [0.307, 0.0, 0.0, 0.0, 12.44, 0.0]
# Earth-like (Outer orbit, tilted to show 3D movement in VisIt)
earth = [0.984, 0.0, 0.0, 0.0, 6.38, 0.5]

# Flatten initial state for the ODE solver
initial_state = np.array(mercury + earth)
N_PLANETS = 2

# ==========================================
# 2. The Equations of Motion
# ==========================================
def compute_derivatives(t, state, theory, params):
    """Calculates planetary accelerations based on the chosen gravity theory."""
    pos = state[:3 * N_PLANETS].reshape((N_PLANETS, 3))
    vel = state[3 * N_PLANETS:].reshape((N_PLANETS, 3))
    acc = np.zeros_like(pos)

    for i in range(N_PLANETS):
        r_vec = pos[i]
        v_vec = vel[i]
        r_mag = np.linalg.norm(r_vec)

        # Standard Newtonian Gravity
        a_newton = - (G * M_sun / r_mag**3) * r_vec

        if theory in ['GR', 'Scalar-Tensor']:
            # PPN Formalism
            gamma = params.get('gamma', 1.0)
            beta = params.get('beta', 1.0)

            v_mag_sq = np.inner(v_vec, v_vec)
            r_dot_v = np.inner(r_vec, v_vec)

            term_r = (2 * (gamma + beta) * G * M_sun / r_mag) - gamma * v_mag_sq
            term_v = 2 * (1 + gamma) * r_dot_v

            a_pn = (G * M_sun / (C_EXAGGERATED**2 * r_mag**3)) * (term_r * r_vec + term_v * v_vec)
            acc[i] = a_newton + a_pn

        elif theory in ['f(R)', 'Multi-Dimensional']:
            # Yukawa Corrections
            alpha = params.get('alpha', 0.0)
            lam = params.get('lambda', 1.0)

            yukawa = alpha * (1.0 + r_mag / lam) * np.exp(-r_mag / lam)
            acc[i] = a_newton * (1.0 + yukawa)

    return np.concatenate((vel.flatten(), acc.flatten()))

# ==========================================
# 3. Export to HDF5 + XDMF for VisIt
# ==========================================
def save_for_visit(filename, sol):
    """Writes the time-series data to HDF5 and creates an XDMF descriptor."""
    num_steps = len(sol.t)
    h5_filename = f"{filename}.h5"
    xdmf_filename = f"{filename}.xdmf"

    # 1. Write the Heavy Data (HDF5)
    with h5py.File(h5_filename, "w") as h5f:
        for i in range(num_steps):
            grp = h5f.create_group(f"step_{i}")

            # Arrays to hold Sun (origin) + Planets
            pos = np.zeros((N_PLANETS + 1, 3))
            vel = np.zeros((N_PLANETS + 1, 3))

            # Fill planet data (Sun remains at 0,0,0)
            pos[1:] = sol.y[:3 * N_PLANETS, i].reshape((N_PLANETS, 3))
            vel[1:] = sol.y[3 * N_PLANETS:, i].reshape((N_PLANETS, 3))

            grp.create_dataset("positions", data=pos, dtype='float64')
            grp.create_dataset("velocities", data=vel, dtype='float64')

    # 2. Write the Light Data Map (XDMF)
    with open(xdmf_filename, "w") as f:
        f.write('<?xml version="1.0" ?>\n')
        f.write('<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>\n')
        f.write('<Xdmf Version="2.0">\n')
        f.write('  <Domain>\n')
        f.write('    <Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">\n')

        for i in range(num_steps):
            f.write(f'      <Grid Name="Step_{i}" GridType="Uniform">\n')
            f.write(f'        <Time Value="{sol.t[i]:.4f}"/>\n')
            # Polyvertex topology renders the coordinates as a 3D point cloud
            f.write(f'        <Topology TopologyType="Polyvertex" NumberOfElements="{N_PLANETS + 1}"/>\n')
            f.write('        <Geometry GeometryType="XYZ">\n')
            f.write(f'          <DataItem Dimensions="{N_PLANETS + 1} 3" NumberType="Float" Precision="8" Format="HDF">\n')
            f.write(f'            {h5_filename}:/step_{i}/positions\n')
            f.write('          </DataItem>\n')
            f.write('        </Geometry>\n')

            # Attach velocity vectors to the particles
            f.write('        <Attribute Name="Velocity" AttributeType="Vector" Center="Node">\n')
            f.write(f'          <DataItem Dimensions="{N_PLANETS + 1} 3" NumberType="Float" Precision="8" Format="HDF">\n')
            f.write(f'            {h5_filename}:/step_{i}/velocities\n')
            f.write('          </DataItem>\n')
            f.write('        </Attribute>\n')
            f.write('      </Grid>\n')

        f.write('    </Grid>\n')
        f.write('  </Domain>\n')
        f.write('</Xdmf>\n')

    print(f"Saved {xdmf_filename} and {h5_filename}")

# ==========================================
# 4. Run the Simulation Suite
# ==========================================
theories = {
    "1_General_Relativity": {"type": "GR", "params": {"gamma": 1.0, "beta": 1.0}},
    "2_Scalar_Tensor": {"type": "Scalar-Tensor", "params": {"gamma": -0.5, "beta": 1.0}}, # Exaggerated scalar coupling
    "3_fR_Gravity": {"type": "f(R)", "params": {"alpha": 0.3, "lambda": 2.0}},            # Chameleon screened scale
    "4_Extra_Dimensions": {"type": "Multi-Dimensional", "params": {"alpha": 1.5, "lambda": 0.5}} # Short-range gravity leak
}

t_span = (0, 15)  # Simulate 15 years
t_eval = np.linspace(t_span[0], t_span[1], 1500) # 1500 frames for smooth VisIt animation

for name, config in theories.items():
    print(f"Simulating {name}...")
    sol = solve_ivp(
        compute_derivatives,
        t_span,
        initial_state,
        args=(config["type"], config["params"]),
        t_eval=t_eval,
        method='RK45',
        rtol=1e-8,
        atol=1e-8
    )
    save_for_visit(name, sol)

print("Simulation complete. Data is ready for VisIt.")
