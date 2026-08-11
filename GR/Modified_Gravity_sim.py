import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from scipy.integrate import solve_ivp


# ==========================================
# 1. Constants & Initial Conditions
# ==========================================

# Solar units:
# distance = AU
# mass = solar masses
# time = years

G = 4.0 * np.pi**2
M_sun = 1.0

# Artificially reduced speed of light to make relativistic
# effects visible. Real value is ~63241 AU/year.
C_EXAGGERATED = 1000.0


# Positions first, then velocities
mercury_pos = [0.307, 0.0, 0.0]
earth_pos   = [0.984, 0.0, 0.0]

mercury_vel = [0.0, 12.44, 0.0]
earth_vel   = [0.0, 6.38, 0.5]


initial_state = np.array(
    mercury_pos +
    earth_pos +
    mercury_vel +
    earth_vel,
    dtype=float
)


N_PLANETS = 2



# ==========================================
# 2. Physics Engine
# ==========================================

def compute_derivatives(t, state, theory, params):

    positions = state[:3*N_PLANETS].reshape(N_PLANETS,3)
    velocities = state[3*N_PLANETS:].reshape(N_PLANETS,3)

    accelerations = np.zeros_like(positions)


    for i in range(N_PLANETS):

        r = positions[i]
        v = velocities[i]

        r_mag = np.linalg.norm(r)

        # Newtonian gravity
        a = -(G*M_sun/r_mag**3)*r


        # ----------------------------------
        # Post-Newtonian / GR correction
        # ----------------------------------

        if theory in ["GR", "Scalar-Tensor"]:

            gamma = params.get("gamma",1.0)
            beta  = params.get("beta",1.0)


            v_squared = np.dot(v,v)
            r_dot_v = np.dot(r,v)


            radial_term = (
                2*(gamma+beta)*G*M_sun/r_mag
                -
                gamma*v_squared
            )


            velocity_term = (
                2*(1+gamma)*r_dot_v
            )


            a_PN = (
                G*M_sun /
                (C_EXAGGERATED**2*r_mag**3)
            ) * (
                radial_term*r
                +
                velocity_term*v
            )


            a += a_PN



        # ----------------------------------
        # Yukawa modified gravity
        # ----------------------------------

        elif theory in ["f(R)", "Multi-Dimensional"]:

            alpha = params.get("alpha",0.0)
            lam = params.get("lambda",1.0)


            correction = (
                alpha*np.exp(-r_mag/lam)
            )


            a *= (1+correction)



        accelerations[i] = a



    return np.concatenate(
        (
            velocities.flatten(),
            accelerations.flatten()
        )
    )



# ==========================================
# 3. Theories
# ==========================================

theories = {

    "General Relativity":
    {
        "type":"GR",
        "params":
        {
            "gamma":1.0,
            "beta":1.0
        }
    },


    "Scalar Tensor":
    {
        "type":"Scalar-Tensor",
        "params":
        {
            "gamma":-0.5,
            "beta":1.0
        }
    },


    "f(R) Gravity":
    {
        "type":"f(R)",
        "params":
        {
            "alpha":0.3,
            "lambda":2.0
        }
    },


    "Extra Dimensions":
    {
        "type":"Multi-Dimensional",
        "params":
        {
            "alpha":1.5,
            "lambda":0.5
        }
    }

}



# ==========================================
# 4. Run Simulations
# ==========================================

t_span = (0,15)

frames = 800

t_eval = np.linspace(
    t_span[0],
    t_span[1],
    frames
)


results = {}


print("Running simulations...")


for name,config in theories.items():

    print(" ->",name)

    solution = solve_ivp(
        compute_derivatives,
        t_span,
        initial_state,
        args=(
            config["type"],
            config["params"]
        ),
        t_eval=t_eval,
        method="RK45",
        rtol=1e-9,
        atol=1e-10
    )


    results[name] = solution.y



# ==========================================
# 5. Animation Setup
# ==========================================

plt.style.use("dark_background")


fig,axs = plt.subplots(
    2,
    2,
    figsize=(10,10)
)


fig.suptitle(
    "Modified Gravity Solar System",
    fontsize=16
)


axs = axs.flatten()


planet_colors = [
    "gray",
    "dodgerblue"
]


lines = []
points = []



for subplot,(name,data) in enumerate(results.items()):

    ax = axs[subplot]


    ax.set_title(name)

    ax.set_xlim(
        -1.2,
        1.2
    )

    ax.set_ylim(
        -1.2,
        1.2
    )


    ax.set_aspect("equal")

    ax.axis("off")


    # Sun

    ax.plot(
        0,
        0,
        "yo",
        markersize=12
    )


    for p in range(N_PLANETS):

        line, = ax.plot(
            [],
            [],
            color=planet_colors[p],
            alpha=0.6
        )


        point, = ax.plot(
            [],
            [],
            "o",
            color=planet_colors[p],
            markersize=5
        )


        lines.append(line)
        points.append(point)




# ==========================================
# 6. Animation Update
# ==========================================

def update(frame):

    index = 0


    for name,data in results.items():


        for p in range(N_PLANETS):

            x = data[
                3*p,
                :frame
            ]


            y = data[
                3*p+1,
                :frame
            ]


            lines[index].set_data(
                x,
                y
            )


            if frame > 0:

                points[index].set_data(
                    [
                        x[-1]
                    ],
                    [
                        y[-1]
                    ]
                )


            index += 1


    return lines + points




ani = animation.FuncAnimation(
    fig,
    update,
    frames=frames,
    interval=20,
    blit=True
)



# ==========================================
# 7. Save Movie
# ==========================================

filename = "modified_gravity_orbits.gif"


print("Saving animation...")


ani.save(
    filename,
    writer="pillow",
    fps=30
)


print(
    "Saved:",
    filename
)
