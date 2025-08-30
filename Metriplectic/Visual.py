
"""
plot_solution.py

Read the output of the 1D metriplectic Fortran solver (solution.dat)
and visualize density, velocity, and pressure as functions of x.
"""

import numpy as np
import matplotlib.pyplot as plt

def read_solution(filename):
    """
    Load data from filename.
    
    Expects columns: x, rho, u, p with a header line starting with '#'.
    Returns four 1D NumPy arrays.
    """
    # Skip any lines beginning with '#', unpack columns
    x, rho, u, p = np.loadtxt(filename, comments='#', unpack=True)
    return x, rho, u, p

def plot_profiles(x, rho, u, p, savefig=False):
    """
    Create a 3-panel figure of ρ(x), u(x), and p(x).
    
    If savefig is True, the plot is saved to 'results.png'.
    """
    fig, axes = plt.subplots(3, 1, figsize=(8, 10), sharex=True)
    
    # Density profile
    axes[0].plot(x, rho, color='tab:blue')
    axes[0].set_ylabel('Density ρ')
    axes[0].grid(True)
    
    # Velocity profile
    axes[1].plot(x, u, color='tab:orange')
    axes[1].set_ylabel('Velocity u')
    axes[1].grid(True)
    
    # Pressure profile
    axes[2].plot(x, p, color='tab:green')
    axes[2].set_ylabel('Pressure p')
    axes[2].set_xlabel('Position x')
    axes[2].grid(True)
    
    plt.tight_layout()
    
    if savefig:
        fig.savefig('results.png', dpi=300)
        print("Saved figure to results.png")
    else:
        plt.show()

def main():
    # 1) Read the data file
    filename = 'solution.dat'
    x, rho, u, p = read_solution(filename)
    
    # 2) Plot and display
    plot_profiles(x, rho, u, p, savefig=False)

if __name__ == "__main__":
    main()
