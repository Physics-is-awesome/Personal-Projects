import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("output/fields_t0000.dat", comments="#")
x, rho, u, e, T, s = data.T

plt.plot(x, rho, label='Density')
plt.plot(x, u, label='Velocity')
plt.plot(x, e, label='Energy')
plt.plot(x, s, label='Entropy')

plt.legend()
plt.title("Metriplectic Fields at t = 0")
plt.xlabel("x")
plt.show()
