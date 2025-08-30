import os
import sys
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def read_solution(path):
    # solution.dat: may contain header beginning with '#'
    # expected columns: x rho vel p
    try:
        df = pd.read_csv(path, comment='#', delim_whitespace=True, header=None,
                         names=['x', 'rho', 'vel', 'p'])
        return df
    except Exception as e:
        raise RuntimeError(f"Failed to read '{path}': {e}")

def read_twocol(path):
    # energy.dat and entropy.dat: two columns (t, value) with possible header
    try:
        df = pd.read_csv(path, comment='#', delim_whitespace=True, header=None, names=['t', 'val'])
        return df
    except Exception as e:
        raise RuntimeError(f"Failed to read '{path}': {e}")

def plot_profiles(df_solution, outpath='solution_profiles.png'):
    plt.figure(figsize=(8,5))
    plt.plot(df_solution['x'], df_solution['rho'], label='rho')
    plt.plot(df_solution['x'], df_solution['vel'], label='vel')
    plt.plot(df_solution['x'], df_solution['p'], label='p')
    plt.xlabel('x')
    plt.ylabel('Value')
    plt.title('Solution profiles (rho, vel, p) vs x')
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(outpath)
    plt.close()
    print(f"Wrote {outpath}")

def plot_energy(df_energy, outpath='energy_vs_time.png'):
    plt.figure(figsize=(8,4))
    plt.plot(df_energy['t'], df_energy['val'])
    plt.xlabel('time')
    plt.ylabel('Total Energy H(t)')
    plt.title('Total Energy vs Time')
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(outpath)
    plt.close()
    print(f"Wrote {outpath}")

def plot_entropy(df_entropy, outpath='entropy_vs_time.png'):
    plt.figure(figsize=(8,4))
    plt.plot(df_entropy['t'], df_entropy['val'])
    plt.xlabel('time')
    plt.ylabel('Total Entropy S(t)')
    plt.title('Total Entropy vs Time')
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(outpath)
    plt.close()
    print(f"Wrote {outpath}")

def make_demo_data(n=200, L=1.0):
    x = np.linspace(0.0, L, n, endpoint=False) + 0.5*(L/n)
    rho = 1.0 + 0.1*np.exp(-50.0*(x-0.5)**2)
    vel = 0.02 * np.sin(2*np.pi*x/L)
    p = 1.0 + 0.05*np.cos(2*np.pi*x/L)
    df_sol = pd.DataFrame({'x': x, 'rho': rho, 'vel': vel, 'p': p})
    t = np.linspace(0, 0.5, 101)
    H = 1.0 + 0.01*np.exp(-5*t)
    S = 0.1 + 0.02*(1.0 - np.exp(-3*t))
    df_H = pd.DataFrame({'t': t, 'val': H})
    df_S = pd.DataFrame({'t': t, 'val': S})
    return df_sol, df_H, df_S

def main():
    parser = argparse.ArgumentParser(description='Plot metriplectic output files.')
    parser.add_argument('--solution', default='solution.dat', help='solution file (x rho vel p)')
    parser.add_argument('--energy', default='energy.dat', help='energy file (t H)')
    parser.add_argument('--entropy', default='entropy.dat', help='entropy file (t S)')
    parser.add_argument('--demo', action='store_true', help='create demo data if files missing')
    args = parser.parse_args()

    have_solution = os.path.exists(args.solution)
    have_energy = os.path.exists(args.energy)
    have_entropy = os.path.exists(args.entropy)

    if args.demo:
        df_sol, df_H, df_S = make_demo_data(n=200, L=1.0)
    else:
        if not (have_solution and have_energy and have_entropy):
            missing = [f for f,ok in [('solution',have_solution),('energy',have_energy),('entropy',have_entropy)] if not ok]
            raise SystemExit(f"Missing files: {missing}. Run with --demo to generate synthetic data for testing.")
        df_sol = read_solution(args.solution)
        df_H = read_twocol(args.energy)
        df_S = read_twocol(args.entropy)

    # Create plots
    plot_profiles(df_sol, outpath='solution_profiles.png')
    plot_energy(df_H, outpath='energy_vs_time.png')
    plot_entropy(df_S, outpath='entropy_vs_time.png')
    print('All plots written to current directory.')

if __name__ == '__main__':
    main()
