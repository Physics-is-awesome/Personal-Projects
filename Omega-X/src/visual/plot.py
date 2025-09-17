import numpy as np
import matplotlib.pyplot as plt
import os
import glob
import numpy as np

data_dir = "results"
file_list = sorted(glob.glob(os.path.join(data_dir, "fields_t*.dat")))

for filename in file_list:
    data = np.loadtxt(filename)
x, rho, u, e, T, s = data.T

plt.plot(x, rho, label='Density')
plt.plot(x, u, label='Velocity')
plt.plot(x, e, label='Energy')
plt.plot(x, s, label='Entropy')

plt.legend()
plt.title("Metriplectic Fields at t = 0")
plt.xlabel("x")
plt.show()
