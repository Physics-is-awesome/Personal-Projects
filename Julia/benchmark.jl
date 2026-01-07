############################################################
# General Explicit Runge–Kutta (ERK) Solver
############################################################

using LinearAlgebra

# ----------------------------------------------------------
# User-defined ODE: du/dt = f(u, t)
# Example: van der Pol oscillator
# ----------------------------------------------------------
function f(u, t)
    μ = 10.0
    return [
        u[2],
        μ * (1 - u[1]^2) * u[2] - u[1]
    ]
end

# Initial condition and time span
u0 = [2.0, 0.0]
tspan = (0.0, 20.0)
#===========================================================
# Time
#===========================================================
t_explicit = @elapsed begin
# ----------------------------------------------------------
# General ERK single-step function
# ----------------------------------------------------------
function erk_step!(f, u, t, h, A, b, c, k)
    s = length(b)

    # Stage loop
    for i in 1:s
        ui = copy(u)

        for j in 1:i-1
            ui .+= h * A[i, j] * k[j]
        end

        k[i] = f(ui, t + c[i] * h)
    end

    # Solution update
    for i in 1:s
        u .+= h * b[i] * k[i]
    end
end

# ----------------------------------------------------------
# Full ERK time integrator
# ----------------------------------------------------------
function erk_solve(f, u0, tspan, h, A, b, c)
    t0, tf = tspan
    N = Int(floor((tf - t0) / h))
    s = length(b)

    u = copy(u0)
    t = t0

    sol = Vector{typeof(u0)}(undef, N + 1)
    sol[1] = copy(u)

    # Preallocate stage storage
    k = [similar(u0) for _ in 1:s]

    for n in 1:N
        erk_step!(f, u, t, h, A, b, c, k)
        t += h
        sol[n + 1] = copy(u)
    end

    return sol
end

# ----------------------------------------------------------
# Classical RK4 Butcher tableau
# ----------------------------------------------------------
A = [
    0.0  0.0  0.0  0.0;
    0.5  0.0  0.0  0.0;
    0.0  0.5  0.0  0.0;
    0.0  0.0  1.0  0.0
]

b = [1/6, 1/3, 1/3, 1/6]
c = [0.0, 0.5, 0.5, 1.0]

# ----------------------------------------------------------
# Run solver
# ----------------------------------------------------------
h = 0.01
solution = erk_solve(f, u0, tspan, h, A, b, c)
end
