# metriplectic_paper_exact_plot.jl
# Paper-exact P1 finite-element metriplectic 1D model with plotting & diagnostics
using LinearAlgebra
using SparseArrays
using Printf
using Plots

# -------------------- User parameters --------------------------------
const γ = 1.4                       # gamma
const L = 1.0                       # domain length
const N = 60                        # number of nodes (periodic)
const NE = N                        # number of elements (periodic)
const h = L / NE
const Re = 100.0
const Pr = 1.0
const CFL = 0.25
const Tfinal = 0.2
const max_newton = 40
const newton_tol = 1e-10
const rho_min = 1e-12
const T_min = 1e-12

# plotting frequency
const PLOT_EVERY = 5
mkpath("plots")  # ensure folder exists

# -------------------- FE assembly: M, K, C --------------------------------
# P1 element local matrices (on element length h):
# Me = (h/6) * [2 1; 1 2]
# Ke = (1/h) * [1 -1; -1 1]
# Ce = local ∫ φ_i ∂φ_j/∂x dx  -> Ce = [-1/2 1/2; -1/2 1/2]

function assemble_M_K_C(N::Int, h::Float64)
    M = spzeros(N,N)
    K = spzeros(N,N)
    C = spzeros(N,N)

    Me = (h/6.0) * [2.0 1.0; 1.0 2.0]
    Ke = (1.0/h) * [1.0 -1.0; -1.0 1.0]
    Ce = [-0.5 0.5; -0.5 0.5]  # no h factor

    for e in 1:NE
        n1 = e
        n2 = (e % NE) + 1
        nodes = (n1, n2)
        for a in 1:2, b in 1:2
            i = nodes[a]; j = nodes[b]
            M[i,j] += Me[a,b]
            K[i,j] += Ke[a,b]
            C[i,j] += Ce[a,b]
        end
    end
    return M, K, C
end

# -------------------- EOS and internal energy ---------------------------
# u(rho,s) = rho^(γ-1)/(γ-1) * exp((γ-1)*s)
u_of(rho, s) = (rho^(γ-1) / (γ - 1.0)) * exp((γ-1.0)*s)
u_r(rho, s) = rho^(γ-2) * exp((γ-1.0)*s)           # ∂u/∂ρ (holding s)
u_s(rho, s) = (γ-1.0) * u_of(rho, s)               # ∂u/∂s

# -------------------- Variational derivatives (nodal) -------------------
# For node i with variables r,m,σ:
# s = σ / r
# dH/dρ = -m^2/(2 r^2) + u + r u_r - s u_s  (=> simplifies to using u)
# dH/dm = m / r
# dH/dσ = u_s
# dS/dσ = 1

function compute_dH_dS_nodal(U::Vector{Float64})
    # U ordering: [ρ(1..N), m(1..N), σ(1..N)]
    dH = zeros(3N)
    dS = zeros(3N)
    rho = @view U[1:N]
    m   = @view U[N+1:2N]
    σ   = @view U[2N+1:3N]

    for i in 1:N
        r = max(rho[i], rho_min)
        mi = m[i]
        si = σ[i] / r              # entropy per mass
        u = u_of(r, si)

        dH[i] = -0.5 * mi^2 / r^2 + u + r * u_r(r, si) - si * u_s(r, si)
        dH[N+i] = mi / r
        dH[2N+i] = u_s(r, si)

        dS[i] = 0.0
        dS[N+i] = 0.0
        dS[2N+i] = 1.0
    end
    return dH, dS
end

# -------------------- Build Dloc in global block ordering ----------------
# We build Dloc directly in block order: indices:
# rho_idx = i
# m_idx = N + i
# s_idx = 2N + i
# Block 3x3 per node at those global indices.

function build_Dloc_block(U::Vector{Float64})
    Dloc = spzeros(3N, 3N)
    rho = @view U[1:N]; m = @view U[N+1:2N]; σ = @view U[2N+1:3N]

    for i in 1:N
        r = max(rho[i], rho_min)
        mi = m[i]
        si = σ[i] / r
        E = exp((γ-1.0)*si)
        u = u_of(r, si)
        ur = u_r(r, si)
        us = u_s(r, si)

        B = γ - (γ-1.0)*si  # used earlier

        # compute entries
        A11 = mi^2 / r^3 + B * ur + us * si / r * 0.0  # last term handled below more precisely
        # derive exact A11: mi^2/r^3 + B*ur + (γ-1)*u*si/r
        A11 = mi^2 / r^3 + B * ur + (γ - 1.0) * u * si / r

        A12 = - mi / r^2
        A13 = (γ - 1.0) * u * (B - 1.0) / r   # derived earlier: (γ-1)u*(B-1)/r

        B11 = - mi / r^2
        B12 = 1.0 / r
        B13 = 0.0

        C11 = (γ - 1.0) * ur
        C12 = 0.0
        C13 = (γ - 1.0)^2 * u / r

        # place into global Dloc (block ordering)
        iρ = i
        im = N + i
        isg = 2N + i

        Dloc[iρ, iρ]     = A11
        Dloc[iρ, im]     = A12
        Dloc[iρ, isg]    = A13

        Dloc[im, iρ]     = B11
        Dloc[im, im]     = B12
        Dloc[im, isg]    = B13

        Dloc[isg, iρ]    = C11
        Dloc[isg, im]    = C12
        Dloc[isg, isg]   = C13
    end

    return Dloc
end

# -------------------- Helpers to map nodal -> coefficients (M^{-1} action) ---
# We will factorize M once and use solves M \ rhs for each block.

function compute_M_factor(M::SparseMatrixCSC{Float64,Int})
    # use dense factorization for small-to-moderate N for exactness;
    # for larger N, use sparse factorization (e.g. cholesky)
    return lu(Matrix(M))
end

# Apply Minv_full to a vector v (length 3N) by solving 3 block systems M * x_block = v_block
function apply_Minv_full_block(v::Vector{Float64}, Mfactor)
    n = N
    out = zeros(3n)
    for b in 0:2
        idx = b*n .+ (1:n)
        out[idx] = Mfactor \ v[idx]
    end
    return out
end

# Map nodal->coef for dH/dS (solve M * coef_block = nodal_block)
function map_nodal_to_coef!(coef::Vector{Float64}, nodal::Vector{Float64}, Mfactor)
    for b in 0:2
        idx = b*N .+ (1:N)
        coef[idx] .= Mfactor \ nodal[idx]
    end
end

# -------------------- Assemble P and G --------------------------------
function assemble_P_G(Minv_times, K::SparseMatrixCSC{Float64,Int}, C::SparseMatrixCSC{Float64,Int}, Re::Float64, Pr::Float64)
    n = size(K,1)
    Mdim = 3n
    P = spzeros(Mdim, Mdim)
    G = spzeros(Mdim, Mdim)

    # D = Minv * C  -- instead of explicitly computing Minv, we compute action: we want matrix D = Minv_times * C
    # We'll compute D as dense sparse-matrix product by solving columns: for each column j of C, solve M * x = column_j
    # But since C is N×N, we can compute D = Minv * C by solving M * X = C for X via factorization; easier:
    # We'll simply compute MinvMatrix = Matrix(Minv_times) by applying Minv to each unit column.
    # However to remain numerically robust, we will build Minv_dense by solving M * X = I.
    # Note: Minv_times is actually LU factor object; we will build Minv_dense explicitly here (exact mapping).
    Minv_dense = zeros(n,n)
    Iden = Matrix(I, n, n)
    for j in 1:n
        e = Iden[:,j]
        Minv_dense[:,j] = Minv_times \ e
    end
    D = Minv_dense * Matrix(C)    # result is dense; small N expected

    # set blocks (use dense-to-sparse conversion where useful)
    # P blocks:
    P[1:n, n+1:2n]       = -sparse(D)
    P[n+1:2n, 1:n]       = sparse(transpose(D))
    P[n+1:2n, 2n+1:3n]   = sparse(Matrix(K))
    P[2n+1:3n, n+1:2n]   = -sparse(transpose(Matrix(K)))

    # G blocks (symmetric PSD)
    G[n+1:2n, n+1:2n]   = (1.0 / Re) * sparse(Matrix(K))
    G[2n+1:3n, 2n+1:3n] = (1.0 / (Re * Pr)) * sparse(Matrix(K))

    # enforce symmetry
    G = 0.5 * (G + transpose(G))

    return P, G, Minv_dense  # return Minv_dense for diagnostics if needed
end

# -------------------- Residual and analytic Jacobian --------------------
# Residual: R(Unew) = Unew - Uold - dt * ( P * dHcoef(mid) + G * dScoef(mid) )
# where dHcoef(mid) = Minv_full * dHnod(mid)

function residual_midpoint(Uold::Vector{Float64}, Unew::Vector{Float64}, dt::Float64, P, G, Mfactor)
    Umid = 0.5 .* (Uold .+ Unew)
    dHnod, dSnod = compute_dH_dS_nodal(Umid)
    dHcoef = zeros(3N); dScoef = zeros(3N)
    map_nodal_to_coef!(dHcoef, dHnod, Mfactor)
    map_nodal_to_coef!(dScoef, dSnod, Mfactor)
    RHS = P * dHcoef + G * dScoef
    R = Unew .- Uold .- dt .* RHS
    return R, dHnod, dSnod, dHcoef, dScoef
end

# Build analytic Jacobian: J = I - (dt/2) * P * Minv_full * Dloc
# We'll compute S = Minv_full * Dloc by applying per-block solves to columns of Dloc.
function build_jacobian(Uold::Vector{Float64}, Unew::Vector{Float64}, dt::Float64, P, Mfactor)
    Umid = 0.5 .* (Uold .+ Unew)
    Dloc = build_Dloc_block(Umid)  # sparse 3N×3N

    # Compute S = Minv_full * Dloc
    # For each column j of Dloc, solve per-block systems
    n = N
    S = zeros(3n, 3n)
    # Work column-by-column (fine for moderate N)
    for j in 1:3n
        col = Array(Dloc[:,j])   # dense column
        # apply Minv_full: for each block b solve M * x_block = col_block
        outcol = zeros(3n)
        for b in 0:2
            idx = b*n .+ (1:n)
            # solve M * x = col[idx]
            outcol[idx] = Mfactor \ col[idx]
        end
        S[:,j] = outcol
    end

    J = I - (dt/2.0) .* (P * S)
    # Convert J to dense matrix for direct dense solve in Newton
    return Matrix(J)
end

# -------------------- Newton solver for implicit midpoint ----------------
function solve_midpoint_newton(Uold::Vector{Float64}, dt::Float64, P, G, Mfactor)
    # predictor: explicit Euler-like predictor using current RHS
    dHnod0, dSnod0 = compute_dH_dS_nodal(Uold)
    dHcoef0 = zeros(3N); dScoef0 = zeros(3N)
    map_nodal_to_coef!(dHcoef0, dHnod0, Mfactor)
    map_nodal_to_coef!(dScoef0, dSnod0, Mfactor)
    RHS0 = P * dHcoef0 + G * dScoef0
    Unew = Uold .+ dt .* RHS0

    for it in 1:max_newton
        R, dHnod, dSnod, dHcoef, dScoef = residual_midpoint(Uold, Unew, dt, P, G, Mfactor)
        if any(x -> !isfinite(x), R)
            return Unew, it, false, "NaN_or_Inf_in_residual"
        end
        rnorm = norm(R)
        if rnorm < newton_tol
            return Unew, it, true, ""
        end

        # analytic Jacobian
        J = build_jacobian(Uold, Unew, dt, P, Mfactor)

        if any(x -> !isfinite(x), J)
            return Unew, it, false, "NaN_or_Inf_in_Jacobian"
        end

        # Solve J * delta = -R
        # Add tiny diag regularization if singular
        Jmod = copy(J)
        for i in 1:size(Jmod,1)
            if abs(Jmod[i,i]) < 1e-18
                Jmod[i,i] += 1e-18
            end
        end
        delta = Jmod \ (-R)

        if any(x -> !isfinite(x), delta)
            return Unew, it, false, "NaN_or_Inf_in_delta"
        end

        # line search / damping
        alpha = 1.0
        Rcur = rnorm
        Utry = Unew .+ alpha .* delta
        Rtry, _, _, _, _ = residual_midpoint(Uold, Utry, dt, P, G, Mfactor)
        rtry = norm(Rtry)
        while rtry > 0.99 * Rcur && alpha > 1e-6
            alpha *= 0.5
            Utry = Unew .+ alpha .* delta
            Rtry, _, _, _, _ = residual_midpoint(Uold, Utry, dt, P, G, Mfactor)
            rtry = norm(Rtry)
        end

        Unew = Utry

        if any(x -> !isfinite(x), Unew)
            return Unew, it, false, "NaN_or_Inf_in_Unew"
        end
    end

    return Unew, max_newton, false, "no_convergence"
end

# -------------------- Diagnostics: check P antisymmetry and G symmetry ----
function check_P_G(M::SparseMatrixCSC{Float64,Int}, P::SparseMatrixCSC{Float64,Int}, G::SparseMatrixCSC{Float64,Int})
    n = size(M,1)
    Mblock = spzeros(3n, 3n)
    Mblock[1:n,1:n] = M
    Mblock[n+1:2n, n+1:2n] = M
    Mblock[2n+1:3n, 2n+1:3n] = M
    S = Matrix(Mblock * P)
    antisym_norm = norm(S + transpose(S))
    Gsym_norm = norm(Matrix(G - transpose(G)))
    eigsG = eigen(Matrix(G)).values
    return antisym_norm, Gsym_norm, minimum(real(eigsG))
end

# -------------------- Discrete H and S integrals -----------------------
function compute_H_discrete(U::Vector{Float64}, M::SparseMatrixCSC{Float64,Int})
    rho = @view U[1:N]; m = @view U[N+1:2N]; σ = @view U[2N+1:3N]
    hv = zeros(N)
    for i in 1:N
        r = max(rho[i], rho_min)
        mi = m[i]
        si = σ[i] / r
        u = u_of(r, si)
        hv[i] = 0.5 * mi^2 / r + r * u
    end
    H = dot(hv, Matrix(M) * ones(N))
    return H
end

function compute_S_discrete(U::Vector{Float64}, M::SparseMatrixCSC{Float64,Int})
    σ = @view U[2N+1:3N]
    S = dot(σ, Matrix(M) * ones(N))
    return S
end

# -------------------- Initial condition --------------------------------
function initial_condition()
    x = [ (i-1)*h for i in 1:N ]
    rho0 = ones(N) .+ 0.1 .* exp.(-50.0 .* (x .- 0.5).^2)
    vel0 = zeros(N)
    σ0 = rho0 .* 0.0
    m0 = rho0 .* vel0
    U0 = vcat(rho0, m0, σ0)
    return U0, x
end

# -------------------- Plot helpers -------------------------------------
function plot_fields(x::Vector{Float64}, U::Vector{Float64}, step::Int)
    rho = @view U[1:N]
    m   = @view U[N+1:2N]
    σ   = @view U[2N+1:3N]
    vel = zeros(N)
    p = zeros(N)
    for i in 1:N
        r = max(rho[i], rho_min)
        vel[i] = m[i] / r
        s = σ[i] / r
        T = max(r^(γ-1.0) * exp((γ-1.0)*s), T_min)
        p[i] = r * T
    end

    # density
    plt1 = plot(x, rho, lw=2, label="ρ", xlabel="x", ylabel="Density", title="density step=$step")
    savefig(plt1, @sprintf("plots/density_step%04d.png", step))
    close(plt1)

    # velocity
    plt2 = plot(x, vel, lw=2, label="u", xlabel="x", ylabel="Velocity", title="velocity step=$step")
    savefig(plt2, @sprintf("plots/velocity_step%04d.png", step))
    close(plt2)

    # pressure
    plt3 = plot(x, p, lw=2, label="p", xlabel="x", ylabel="Pressure", title="pressure step=$step")
    savefig(plt3, @sprintf("plots/pressure_step%04d.png", step))
    close(plt3)
end

# -------------------- Main simulation ----------------------------------
function main()
    println("Assembling FE matrices...")
    M, K, C = assemble_M_K_C(N, h)
    println("Assembled M, K, C: N=$N, h=$(h)")

    # factorize M for block solves
    println("Factorizing M ...")
    Mfactor = compute_M_factor(M)   # LU factor of M (dense) -> mathematically exact mapping

    # assemble P and G
    println("Assembling P and G ...")
    P, G, Minv_dense = assemble_P_G(Mfactor, K, C, Re, Pr)
    # run diagnostics
    antisym_norm, Gsym_norm, Gmin_ev = check_P_G(M, P, G)
    @printf("Mass-weighted antisymmetry norm ||Mblock*P + (Mblock*P)'|| = %.6e\n", antisym_norm)
    @printf("G symmetry norm ||G - G'|| = %.6e\n", Gsym_norm)
    @printf("G min eigenvalue = %.6e\n", Gmin_ev)

    # initial condition
    U, x = initial_condition()
    H0 = compute_H_discrete(U, M)
    S0 = compute_S_discrete(U, M)
    @printf("Initial H = %.12e  S = %.12e\n", H0, S0)

    umax = maximum([ abs(U[N+i] / max(U[i], rho_min)) + sqrt(γ * max(U[i], rho_min)^(γ-1.0)) for i in 1:N ])
    dt = CFL * h / umax
    @printf("Initial dt = %.6e (umax=%.6e)\n", dt, umax)

    # history
    t = 0.0
    step = 0
    Hhist = Float64[]
    Shist = Float64[]
    Thist = Float64[]

    while t < Tfinal - 1e-14
        step += 1
        Uold = copy(U)

        Unew, niters, success, reason = solve_midpoint_newton(Uold, dt, P, G, Mfactor)
        if !success
            @printf("Newton failed at step %d: reason=%s; reducing dt and retrying\n", step, reason)
            dt *= 0.5
            if dt < 1e-14
                error("dt too small, aborting")
            end
            step -= 1
            continue
        end

        U = Unew
        t += dt

        # safety: check NaNs/Infs
        if any(x -> !isfinite(x), U)
            idxnan = findall(x -> !isfinite(x), U)
            @printf("NaN/Inf detected in U at step %d indices: %s\n", step, string(idxnan))
            error("Non-finite state detected")
        end

        # record history & print
        if step % max(1, Int(ceil((Tfinal/dt)/20))) == 0 || t >= Tfinal - 1e-14
            Hval = compute_H_discrete(U, M)
            Sval = compute_S_discrete(U, M)
            push!(Hhist, Hval); push!(Shist, Sval); push!(Thist, t)
            @printf("t=%.6f  step=%4d  niters=%2d  H=%.12e  S=%.12e\n", t, step, niters, Hval, Sval)
        end

        # plotting at intervals
        if step % PLOT_EVERY == 0 || t >= Tfinal - 1e-14
            plot_fields(x, U, step)
            # also plot H,S vs time so far
            if !isempty(Thist)
                plt = plot(Thist, Hhist, label="H", xlabel="t", ylabel="H,S", lw=2)
                plot!(plt, Thist, Shist, label="S", lw=2)
                savefig(plt, @sprintf("plots/H_S_until_step%04d.png", step))
                close(plt)
            end
        end
    end

    # final outputs
    open("solution.dat","w") do io
        @printf(io, "# x rho vel p\n")
        rho = @view U[1:N]
        m = @view U[N+1:2N]
        σ = @view U[2N+1:3N]
        for i in 1:N
            r = rho[i]
            v = m[i] / max(r, rho_min)
            s = σ[i] / max(r, rho_min)
            T = max(r^(γ-1.0) * exp((γ-1.0)*s), T_min)
            pval = r * T
            xval = (i-1) * h
            @printf(io, "%12.6f  %12.6e  %12.6e  %12.6e\n", xval, r, v, pval)
        end
    end

    @printf("Done. Outputs: solution.dat and PNGs in ./plots\n")
end

# Run main if executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end

