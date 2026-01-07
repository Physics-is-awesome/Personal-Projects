############################################################
# General Explicit Runge–Kutta (ERK) Solver
############################################################

using LinearAlgebra
using Printf
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
#------------------------------------------
# Explicit Runge-Kutta
#------------------------------------------
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
#----------------------------------------------------------------
# Implicit Runge-Kutta solver
# ---------------------------------------------------------------
t_implicit = @elapsed begin
    #------------------------------------
    # General Newton solver
    #------------------------------------
    function newton_solve(F, x0; tol=1e-10, maxiter=20)
        x = copy(x0)
        n = length(x)
    
        for iter in 1:maxiter
            r = F(x)
            norm_r = norm(r)
            println(@sprintf("  Newton iter %d, ||r|| = %.3e", iter, norm_r))
            if norm_r < tol
                return x
            end
    
            # Finite-difference Jacobian
            J = zeros(n, n)
            ε = 1e-8
            for j in 1:n
                dx = zeros(n)
                dx[j] = ε
                J[:, j] = (F(x .+ dx) .- r) / ε
            end
    
            # Optional tiny regularization if singular
            J += 1e-12 * I
    
            # Solve linear system
            δ = -J \ r
            x .+= δ

        end

        error("Newton did not converge after $maxiter iterations")
    end
    # ----------------------------------------
    # One IRK timestep
    # ----------------------------------------

    function irk_step!(f, u, t, h, A, b, c)
        s = length(b)
        d = length(u)

        # Flattened stage vector
        K0 = zeros(s*d)
        
        function residual(K)
            R = similar(K)

            for i in 1:s
                ki = view(K, (i-1)*d+1:i*d)

                ui = copy(u)

                for j in 1:s
                    kj = view(K, (j-1)*d+1:j*d)
                    ui .+= h * A[i, j] * kj
                end

                fi = f(ui, t + c[i]*h)
                R[(i-1)*d+1:i*d] .= ki .- fi
            end

            return R
        end

        K = newton_solve(residual, K0)

        # Update solution
        for i in 1:s
            ki = view(K, (i-1)*d+1:i*d)
            u .+= h * b[i] * ki
        end
    end


    # -------------------------------
    # IRK solver
    # -------------------------------
    function irk_solve(f, u0, tspan, h, A, b, c)
        t0, tf = tspan
        N = Int(floor((tf - t0)/h))
        u = copy(u0)
        t = t0

        sol = Vector{typeof(u0)}(undef, N + 1)
        sol[1] = copy(u)

        for n in 1:N
            irk_step!(f, u, t, h, A, b, c)
            t += h
            sol[n+1] = copy(u)
        end

        return sol
    end

    #----------------
    # Butcher Tableau
    # --------------
    A = [0.5]
    b = [1.0]
    c = [0.5]

    #--------------
    # Run
    #--------------
    sol = irk_solve(f, u0, (0.0, 20.0), 0.1, A, b, c)

end
# printing time
println("Explicit RK time: $t_explicit")
println("Implicit RK time: $t_implicit")
