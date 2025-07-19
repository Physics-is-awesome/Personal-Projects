# Advanced Python program for ELM-MHD simulation using metriplectic 4-bracket formalism
# 2D flux-coordinate grid (psi, theta), implicit time-stepping, MPI parallelization
# State variables: rho (density), m (momentum), s (entropy), B (magnetic field)
# Includes resistivity, viscosity, heat conduction, particle diffusion, and SOL transport

import numpy as np
from mpi4py import MPI
import os

# Constants (equivalent to elm_mhd_mod)
MU_0 = 1.25663706e-6  # Magnetic permeability (H/m)
PSI_MIN, PSI_MAX = 0.0, 1.2  # Flux coordinate range
THETA_MIN, THETA_MAX = 0.0, 2.0 * np.pi
NPSI, NTHETA = 200, 100  # Grid points
DPSI = (PSI_MAX - PSI_MIN) / (NPSI - 1)
DTHETA = (THETA_MAX - THETA_MIN) / (NTHETA - 1)
DT = 1.0e-5  # Time step (s)
N_STEPS = 1000  # Number of time steps
ETA_0 = 1.0e-5  # Resistivity (m^2/s)
MU_0_VISC = 1.0e-4  # Viscosity (kg/m/s)
KAPPA_0 = 1.0e-3  # Thermal conductivity (W/m/K)
D_0 = 1.0e-4  # Particle diffusion coefficient (m^2/s)

def initialize_fields():
    """Initialize fields with pedestal and ELM perturbation."""
    rho = np.zeros((NPSI, NTHETA))
    m_psi = np.zeros((NPSI, NTHETA))
    m_theta = np.zeros((NPSI, NTHETA))
    s = np.zeros((NPSI, NTHETA))
    B_psi = np.zeros((NPSI, NTHETA))
    B_theta = np.zeros((NPSI, NTHETA))
    eta = np.zeros((NPSI, NTHETA))
    mu_visc = np.zeros((NPSI, NTHETA))
    kappa = np.zeros((NPSI, NTHETA))
    D = np.zeros((NPSI, NTHETA))
    
    psi = np.linspace(PSI_MIN, PSI_MAX, NPSI)
    theta = np.linspace(THETA_MIN, THETA_MAX, NTHETA)
    PSI, THETA = np.meshgrid(psi, theta, indexing='ij')
    r = np.abs(PSI - 1.0)  # Distance from separatrix
    pedestal_width = 0.1
    B0 = 1.0
    
    rho[...] = 1.0 + 0.5 * np.exp(-r**2 / pedestal_width**2) + 0.1 * np.sin(5.0 * THETA) * np.exp(-r**2)
    s[...] = 1.0 + 0.3 * np.exp(-r**2 / pedestal_width**2) + 0.05 * np.sin(5.0 * THETA) * np.exp(-r**2)
    m_psi[...] = 0.0
    m_theta[...] = 0.0
    B_psi[...] = B0 * np.cos(0.1 * r)
    B_theta[...] = B0 * np.sin(0.1 * r)
    eta[...] = ETA_0 * (1.0 + 10.0 * np.exp(-r**2 / (2.0 * pedestal_width**2)))
    mu_visc[...] = MU_0_VISC * (1.0 + 10.0 * np.exp(-r**2 / (2.0 * pedestal_width**2)))
    kappa[...] = KAPPA_0 * (1.0 + 10.0 * np.exp(-r**2 / (2.0 * pedestal_width**2)))
    D[...] = D_0 * (1.0 + 10.0 * np.exp(-r**2 / (2.0 * pedestal_width**2)))
    
    return rho, m_psi, m_theta, s, B_psi, B_theta, eta, mu_visc, kappa, D

def compute_derived(rho, m_psi, m_theta, s):
    """Compute derived quantities: velocity, temperature, pressure, chemical potential."""
    u_psi = np.zeros_like(rho)
    u_theta = np.zeros_like(rho)
    T = np.zeros_like(rho)
    p = np.zeros_like(rho)
    mu_chem = np.zeros_like(rho)
    
    u_psi[...] = m_psi / np.maximum(rho, 1.0e-10)  # Avoid division by zero
    u_theta[...] = m_theta / np.maximum(rho, 1.0e-10)
    T[...] = s / np.maximum(rho, 1.0e-10)  # Simplified EOS
    p[...] = rho * T
    e = T  # Simplified internal energy
    mu_chem[...] = -0.5 * (u_psi**2 + u_theta**2) + e
    
    return u_psi, u_theta, T, p, mu_chem

def compute_poisson(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, p):
    """Compute Poisson bracket contributions."""
    drho_dt = np.zeros_like(rho)
    dm_psi_dt = np.zeros_like(rho)
    dm_theta_dt = np.zeros_like(rho)
    ds_dt = np.zeros_like(rho)
    dB_psi_dt = np.zeros_like(rho)
    dB_theta_dt = np.zeros_like(rho)
    div_rho_u = np.zeros_like(rho)
    div_m_u = np.zeros_like(rho)
    curl_uB = np.zeros_like(rho)
    
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            # div(rho*u)
            div_rho_u[i,j] = (rho[i+1,j] * u_psi[i+1,j] - rho[i-1,j] * u_psi[i-1,j]) / (2.0 * DPSI) + \
                             (rho[i,j+1] * u_theta[i,j+1] - rho[i,j-1] * u_theta[i,j-1]) / (2.0 * DTHETA)
            # div(m*u + (p + |B|^2/2mu_0)*I - B*B/mu_0)
            div_m_u[i,j] = (m_psi[i+1,j] * u_psi[i+1,j] - m_psi[i-1,j] * u_psi[i-1,j]) / (2.0 * DPSI) + \
                           (m_psi[i,j+1] * u_theta[i,j+1] - m_psi[i,j-1] * u_theta[i,j-1]) / (2.0 * DTHETA) + \
                           (p[i+1,j] + 0.5 * (B_psi[i+1,j]**2 + B_theta[i+1,j]**2) / MU_0 - \
                            (p[i-1,j] + 0.5 * (B_psi[i-1,j]**2 + B_theta[i-1,j]**2) / MU_0)) / (2.0 * DPSI) - \
                           (B_psi[i+1,j] * B_psi[i+1,j] - B_psi[i-1,j] * B_psi[i-1,j]) / (2.0 * DPSI * MU_0)
            # div(s*u)
            ds_dt[i,j] = (s[i+1,j] * u_psi[i+1,j] - s[i-1,j] * u_psi[i-1,j]) / (2.0 * DPSI) + \
                         (s[i,j+1] * u_theta[i,j+1] - s[i,j-1] * u_theta[i,j-1]) / (2.0 * DTHETA)
            # curl(u x B)
            curl_uB[i,j] = ((u_theta[i,j+1] * B_psi[i,j+1] - u_theta[i,j-1] * B_psi[i,j-1]) / (2.0 * DTHETA) - \
                            (u_psi[i+1,j] * B_theta[i+1,j] - u_psi[i-1,j] * B_theta[i-1,j]) / (2.0 * DPSI))
    
    drho_dt[...] = -div_rho_u
    dm_psi_dt[...] = -div_m_u
    dm_theta_dt[...] = -div_m_u  # Simplified for 2D
    ds_dt[...] = -ds_dt
    dB_psi_dt[...] = curl_uB
    dB_theta_dt[...] = -curl_uB
    
    return drho_dt, dm_psi_dt, dm_theta_dt, ds_dt, dB_psi_dt, dB_theta_dt

def compute_4bracket(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, T, mu_chem, eta, mu_visc, kappa, D):
    """Compute 4-bracket contributions."""
    grad_ux = np.zeros((2, NPSI, NTHETA))
    grad_uy = np.zeros((2, NPSI, NTHETA))
    grad_T = np.zeros((2, NPSI, NTHETA))
    grad_mu = np.zeros((2, NPSI, NTHETA))
    curl_B = np.zeros((NPSI, NTHETA))
    tau = np.zeros((2, 2, NPSI, NTHETA))
    J_rho = np.zeros((2, NPSI, NTHETA))
    q = np.zeros((2, NPSI, NTHETA))
    J = np.zeros((2, NPSI, NTHETA))
    drho_dt = np.zeros((NPSI, NTHETA))
    dm_psi_dt = np.zeros((NPSI, NTHETA))
    dm_theta_dt = np.zeros((NPSI, NTHETA))
    ds_dt = np.zeros((NPSI, NTHETA))
    dB_psi_dt = np.zeros((NPSI, NTHETA))
    dB_theta_dt = np.zeros((NPSI, NTHETA))
    div_tau = np.zeros((NPSI, NTHETA))
    div_q = np.zeros((NPSI, NTHETA))
    div_J_rho = np.zeros((NPSI, NTHETA))
    curl_J = np.zeros((NPSI, NTHETA))
    
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            grad_ux[0,i,j] = (u_psi[i+1,j] - u_psi[i-1,j]) / (2.0 * DPSI)
            grad_ux[1,i,j] = (u_psi[i,j+1] - u_psi[i,j-1]) / (2.0 * DTHETA)
            grad_uy[0,i,j] = (u_theta[i+1,j] - u_theta[i-1,j]) / (2.0 * DPSI)
            grad_uy[1,i,j] = (u_theta[i,j+1] - u_theta[i,j-1]) / (2.0 * DTHETA)
            grad_T[0,i,j] = (T[i+1,j] - T[i-1,j]) / (2.0 * DPSI)
            grad_T[1,i,j] = (T[i,j+1] - T[i,j-1]) / (2.0 * DTHETA)
            grad_mu[0,i,j] = (mu_chem[i+1,j] - mu_chem[i-1,j]) / (2.0 * DPSI)
            grad_mu[1,i,j] = (mu_chem[i,j+1] - mu_chem[i,j-1]) / (2.0 * DTHETA)
            curl_B[i,j] = (B_theta[i+1,j] - B_theta[i-1,j]) / (2.0 * DPSI) - (B_psi[i,j+1] - B_psi[i,j-1]) / (2.0 * DTHETA)
    
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            tau[0,0,i,j] = mu_visc[i,j] * (2.0 * grad_ux[0,i,j] - (2.0/3.0) * (grad_ux[0,i,j] + grad_uy[1,i,j]))
            tau[0,1,i,j] = mu_visc[i,j] * (grad_ux[1,i,j] + grad_uy[0,i,j])
            tau[1,0,i,j] = tau[0,1,i,j]
            tau[1,1,i,j] = mu_visc[i,j] * (2.0 * grad_uy[1,i,j] - (2.0/3.0) * (grad_ux[0,i,j] + grad_uy[1,i,j]))
            J_rho[0,i,j] = -D[i,j] * rho[i,j] * grad_mu[0,i,j]
            J_rho[1,i,j] = -D[i,j] * rho[i,j] * grad_mu[1,i,j]
            q[0,i,j] = -kappa[i,j] * rho[i,j] * grad_T[0,i,j]
            q[1,i,j] = -kappa[i,j] * rho[i,j] * grad_T[1,i,j]
            J[0,i,j] = (eta[i,j] / MU_0) * curl_B[i,j]
            J[1,i,j] = -(eta[i,j] / MU_0) * curl_B[i,j]
    
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            div_J_rho[i,j] = (J_rho[0,i+1,j] - J_rho[0,i-1,j]) / (2.0 * DPSI) + (J_rho[1,i,j+1] - J_rho[1,i,j-1]) / (2.0 * DTHETA)
            div_tau[i,j] = (tau[0,0,i+1,j] - tau[0,0,i-1,j]) / (2.0 * DPSI) + (tau[0,1,i,j+1] - tau[0,1,i,j-1]) / (2.0 * DTHETA)
            div_q[i,j] = (q[0,i+1,j] - q[0,i-1,j]) / (2.0 * DPSI) + (q[1,i,j+1] - q[1,i,j-1]) / (2.0 * DTHETA)
            curl_J[i,j] = (J[1,i+1,j] - J[1,i-1,j]) / (2.0 * DPSI) - (J[0,i,j+1] - J[0,i,j-1]) / (2.0 * DTHETA)
    
    drho_dt[...] = drho_dt - div_J_rho
    dm_psi_dt[...] = dm_psi_dt + div_tau
    dm_theta_dt[...] = dm_theta_dt + div_tau
    ds_dt[...] = ds_dt - div_q / np.maximum(rho * T, 1.0e-10) + \
                 (mu_visc / np.maximum(T, 1.0e-10) * \
                  (grad_ux[0]**2 + grad_ux[1]**2 + grad_uy[0]**2 + grad_uy[1]**2) + \
                  eta / (MU_0**2 * np.maximum(T, 1.0e-10)) * curl_B**2 + \
                  D / np.maximum(T, 1.0e-10) * (grad_mu[0]**2 + grad_mu[1]**2)) / \
                  np.maximum(T, 1.0e-10)
    dB_psi_dt[...] = dB_psi_dt - curl_J
    dB_theta_dt[...] = dB_theta_dt + curl_J
    
    return drho_dt, dm_psi_dt, dm_theta_dt, ds_dt, dB_psi_dt, dB_theta_dt, grad_ux, grad_uy, grad_T, grad_mu, curl_B, tau, J_rho, q, J

def implicit_solve(rho, m_psi, m_theta, s, B_psi, B_theta, rho_new, m_psi_new, m_theta_new, s_new, B_psi_new, B_theta_new):
    """Placeholder for implicit solve (explicit update for now)."""
    rho[...] = rho + DT * rho_new
    m_psi[...] = m_psi + DT * m_psi_new
    m_theta[...] = m_theta + DT * m_theta_new
    s[...] = s + DT * s_new
    B_psi[...] = B_psi + DT * B_psi_new
    B_theta[...] = B_theta + DT * B_theta_new

def apply_boundary_conditions(rho, m_psi, m_theta, s, B_psi, B_theta):
    """Apply boundary conditions: outflow in psi, periodic in theta."""
    # SOL: Outflow; Pedestal: No-flux
    rho[0, :] = rho[1, :]
    rho[-1, :] = rho[-2, :]
    m_psi[0, :] = 0.0
    m_psi[-1, :] = 0.0
    m_theta[0, :] = 0.0
    m_theta[-1, :] = 0.0
    s[0, :] = s[1, :]
    s[-1, :] = s[-2, :]
    B_psi[0, :] = B_psi[1, :]
    B_psi[-1, :] = B_psi[-2, :]
    B_theta[0, :] = B_theta[1, :]
    B_theta[-1, :] = B_theta[-2, :]
    # Periodic in theta
    rho[:, 0] = rho[:, -2]
    rho[:, -1] = rho[:, 1]
    m_psi[:, 0] = m_psi[:, -2]
    m_psi[:, -1] = m_psi[:, 1]
    m_theta[:, 0] = m_theta[:, -2]
    m_theta[:, -1] = m_theta[:, 1]
    s[:, 0] = s[:, -2]
    s[:, -1] = s[:, 1]
    B_psi[:, 0] = B_psi[:, -2]
    B_psi[:, -1] = B_psi[:, 1]
    B_theta[:, 0] = B_theta[:, -2]
    B_theta[:, -1] = B_theta[:, 1]

def compute_energy_entropy(rho, m_psi, m_theta, s, B_psi, B_theta, T, grad_ux, grad_T, curl_B, mu_chem, eta, mu_visc, kappa, D):
    """Compute energy, entropy, and entropy production."""
    grad_mu = np.zeros((2, NPSI, NTHETA))
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            grad_mu[0,i,j] = (mu_chem[i+1,j] - mu_chem[i-1,j]) / (2.0 * DPSI)
            grad_mu[1,i,j] = (mu_chem[i,j+1] - mu_chem[i,j-1]) / (2.0 * DTHETA)
    
    energy = 0.0
    entropy = 0.0
    entropy_prod = 0.0
    for i in range(1, NPSI-1):
        for j in range(1, NTHETA-1):
            e = T[i,j]
            energy += (0.5 * (m_psi[i,j]**2 + m_theta[i,j]**2) / max(rho[i,j], 1.0e-10) + \
                       rho[i,j] * e + 0.5 * (B_psi[i,j]**2 + B_theta[i,j]**2) / MU_0) * DPSI * DTHETA
            entropy += s[i,j] * DPSI * DTHETA
            entropy_prod += (D[i,j] / max(T[i,j], 1.0e-10) * \
                             (grad_mu[0,i,j]**2 + grad_mu[1,i,j]**2) + \
                             mu_visc[i,j] / max(T[i,j], 1.0e-10) * \
                             (grad_ux[0,i,j]**2 + grad_ux[1,i,j]**2) + \
                             kappa[i,j] / (max(T[i,j], 1.0e-10)**2) * \
                             (grad_T[0,i,j]**2 + grad_T[1,i,j]**2) + \
                             eta[i,j] / (MU_0**2 * max(T[i,j], 1.0e-10)) * curl_B[i,j]**2) * \
                             rho[i,j] * max(T[i,j], 1.0e-10) * DPSI * DTHETA
    
    return energy, entropy, entropy_prod

def save_fields(rho, m_psi, m_theta, s, B_psi, B_theta):
    """Save fields to file."""
    try:
        with open('elm_mhd_output.dat', 'w') as f:
            for i in range(NPSI):
                for j in range(NTHETA):
                    f.write(f"{i} {j} {rho[i,j]} {m_psi[i,j]} {m_theta[i,j]} {s[i,j]} {B_psi[i,j]} {B_theta[i,j]}\n")
    except IOError:
        print("Error writing to output file")

def main():
    # Initialize MPI
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    nprocs = comm.Get_size()
    
    # Initialize fields
    rho, m_psi, m_theta, s, B_psi, B_theta, eta, mu_visc, kappa, D = initialize_fields()
    
    # Allocate temporary arrays
    rho_new = np.zeros((NPSI, NTHETA))
    m_psi_new = np.zeros((NPSI, NTHETA))
    m_theta_new = np.zeros((NPSI, NTHETA))
    s_new = np.zeros((NPSI, NTHETA))
    B_psi_new = np.zeros((NPSI, NTHETA))
    B_theta_new = np.zeros((NPSI, NTHETA))
    u_psi = np.zeros((NPSI, NTHETA))
    u_theta = np.zeros((NPSI, NTHETA))
    T = np.zeros((NPSI, NTHETA))
    p = np.zeros((NPSI, NTHETA))
    mu_chem = np.zeros((NPSI, NTHETA))
    
    # Main time-stepping loop
    for t in range(1, N_STEPS + 1):
        u_psi, u_theta, T, p, mu_chem = compute_derived(rho, m_psi, m_theta, s)
        
        rho_new[:], m_psi_new[:], m_theta_new[:], s_new[:], B_psi_new[:], B_theta_new[:] = \
            compute_poisson(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, p)
        
        rho_new[:], m_psi_new[:], m_theta_new[:], s_new[:], B_psi_new[:], B_theta_new[:], \
        grad_ux, grad_uy, grad_T, grad_mu, curl_B, tau, J_rho, q, J = \
            compute_4bracket(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, T, mu_chem,
                             eta, mu_visc, kappa, D)
        
        implicit_solve(rho, m_psi, m_theta, s, B_psi, B_theta,
                      rho_new, m_psi_new, m_theta_new, s_new, B_psi_new, B_theta_new)
        
        apply_boundary_conditions(rho, m_psi, m_theta, s, B_psi, B_theta)
        
        energy, entropy, entropy_prod = compute_energy_entropy(
            rho, m_psi, m_theta, s, B_psi, B_theta, T, grad_ux, grad_T, curl_B, mu_chem,
            eta, mu_visc, kappa, D)
        
        if t % 100 == 0 and rank == 0:
            print(f"Step: {t}, Energy: {energy}, Entropy Production: {entropy_prod}")
    
    # Save final state
    if rank == 0:
        save_fields(rho, m_psi, m_theta, s, B_psi, B_theta)

if __name__ == "__main__":
    main()
