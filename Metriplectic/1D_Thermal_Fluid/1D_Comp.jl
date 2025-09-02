# metriplectic_paper_exact.jl
# Paper-exact Galerkin metriplectic 1D discretization (P1 elements, periodic)
using LinearAlgebra
using SparseArrays
using Printf

# ----------------------- User parameters -------------------------
const γ = 1.4                      # adiabatic index (choose paper value)
const L = 1.0                      # domain length
const N = 60                       # number of nodes (periodic)
const NE = N                       # number of elements (periodic)
const h = L / NE                   # element size
const Tfinal = 0.2
const CFL = 0.25
const Re = 100.0
const Pr = 1.0
const max_newton = 40
const newton_tol = 1e-10
const rho_min = 1e-12
const T_min = 1e-12

# ----------------------- Index helpers ---------------------------
# global index: block b in {0,1,2} (rho,m,sigma) and node i in 1..N
global_index(b, i) = b*N + i       # b=0 -> rho, b=1 -> m, b=2 -> sigma

# ----------------------- FE assembly (P1 periodic) ----------------
"""
assemble_M_K_C(N, h)

Returns:
  M :: N×N consistent mass matrix (sparse)
  K :: N×N stiffness matrix (sparse)  (∫ ∂φ_i ∂φ_j dx)
  C :: N×N mixed matrix (∫ φ_i ∂φ_j dx)
Periodic assembly.
"""
function assemble_M_K_C(N, h)
    M = spzeros(N,N)
    K = spzeros(N,N)
    C = spzeros(N,N)

    # Local element contributions for P1 on element of length h:
    # Local mass:  (h/6) * [2 1; 1 2]
    # Local stiffness: (1/h) * [1 -1; -1 1]
    # Local mixed C_ij = ∫ φ_i dφ_j/dx dx over element:
    # we computed earlier local C = [ -1/2  1/2 ; -1/2 1/2 ]  (independent of h)
    Me = h / 6.0 * [2.0 1.0; 1.0 2.0]
    Ke = (1.0 / h) * [1.0 -1.0; -1.0 1.0]
    Ce = [-0.5 0.5; -0.5 0.5]  # ∫ φ_i dφ_j/dx dx on the element

    # Periodic assembly: elements e from 1..NE connect nodes e and e+1 (with wrap)
    for e in 1:NE
        n1 = e
        n2 = (e % NE) + 1  # wrap-around
        nodes = (n1, n2)
        for a in 1:2, b in 1:2
            I = nodes[a]
            J = nodes[b]
            M[I,J] += Me[a,b]
            K[I,J] += Ke[a,b]
            C[I,J] += Ce[a,b]
        end
    end

    return M, K, C
end

# ------------------------- Thermodynamics/EOS ---------------------
# Use u(rho,s) = rho^(γ-1)/(γ-1) * exp((γ-1)*s)
# derivatives: u_r, u_s etc.

"""
u_of(rho, s)
"""
u_of(rho, s) = (rho^(γ-1) / (γ - 1.0)) * exp((γ-1.0) * s)

"""
u_r(rho, s) = ∂u/∂ρ (holding s)
"""
u_r(rho, s) = rho^(γ-2) * exp((γ-1.0) * s)

"""
u_s(rho, s) = ∂u/∂s
"""
u_s(rho, s) = (γ-1.0) * u_of(rho, s)

# --------------------- Variational derivatives (nodal) -------------
"""
compute_dH_dS_nodal(U)

Input:
  U length 3N vector (nodal conservative variables: rho, m, sigma)
Output:
  dHnod (3N) -- nodal variational derivative δH/δU evaluated at nodes
  dSnod (3N) -- nodal variational derivative δS/δU evaluated at nodes
Also returns nodal diagnostics like u_i, s_i for later use.
"""
function compute_dH_dS_nodal(U)
    dH = zeros(3N)
    dS = zeros(3N)
    # store local helpers for Jacobian
    rho = view(U, 1:N)
    m   = view(U, N+1:2N)
    σ   = view(U, 2N+1:3N)

    for i in 1:N
        r = max(rho[i], rho_min)        # floor density
        mi = m[i]
        si = σ[i] / r                   # entropy per mass
        u = u_of(r, si)

        # δH/δρ = -0.5 * m^2 / r^2 + γ * u
        dH[i] = -0.5 * mi^2 / r^2 + γ * u

        # δH/δm = m / r
        dH[N + i] = mi / r

        # δH/δσ = (γ-1) * u  (since σ = ρ s and δ/δσ of ρ u gives u_s * ∂s/∂σ = u_s / ρ -> but multiplying by ρ cancels)
        dH[2N + i] = (γ - 1.0) * u

        # Entropy functional S = ∫ σ dx  => nodal derivative δS/δσ = 1
        dS[i] = 0.0
        dS[N + i] = 0.0
        dS[2N + i] = 1.0
    end

    return dH, dS
end

# -------------------- Local 3x3 derivative Dloc per node -------------
"""
build_Dloc(U) returns Dloc (3N×3N) block-diagonal matrix where each
node i contributes a 3×3 block = ∂(dHnod_node)/∂(U_node).

Block structure per node (rows: [dH_r, dH_m, dH_sigma]; cols: [ρ, m, σ]):

We derived:
A11 = ∂dH_r/∂ρ = m^2 / r^3 + γ * r^(γ-2) * exp((γ-1)*s)
A12 = ∂dH_r/∂m = - m / r^2
A13 = ∂dH_r/∂σ = γ*(γ-1) * u / r

B11 = ∂dH_m/∂ρ = - m / r^2
B12 = ∂dH_m/∂m = 1 / r
B13 = 0

C11 = ∂dH_σ/∂ρ = (γ-1) * r^(γ-2) * exp((γ-1)*s)
C12 = 0
C13 = (γ-1)^2 * u / r
"""
function build_Dloc(U)
    Dloc = spzeros(3N, 3N)
    rho = view(U, 1:N)
    m   = view(U, N+1:2N)
    σ   = view(U, 2N+1:3N)

    for i in 1:N
        r = max(rho[i], rho_min)
        mi = m[i]
        si = σ[i] / r
        E = exp((γ-1.0)*si)
        u = u_of(r, si)

        A11 = mi^2 / r^3 + γ * r^(γ-2) * E
        A12 = - mi / r^2
        A13 = γ * (γ-1.0) * u / r

        B11 = - mi / r^2
        B12 = 1.0 / r
        B13 = 0.0

        C11 = (γ-1.0) * r^(γ-2) * E
        C12 = 0.0
        C13 = (γ-1.0)^2 * u / r

        # Place block into Dloc (global indices)
        row = 3*(i-1)
        # rows row+1..row+3 correspond to node i
        Dloc[row+1, row+1] = A11
        Dloc[row+1, row+2] = A12
        Dloc[row+1, row+3] = A13

        Dloc[row+2, row+1] = B11
        Dloc[row+2, row+2] = B12
        Dloc[row+2, row+3] = B13

        Dloc[row+3, row+1] = C11
        Dloc[row+3, row+2] = C12
        Dloc[row+3, row+3] = C13
    end

    # Note: Dloc is using node-local ordering [ρ1,m1,σ1, ρ2,m2,σ2, ...]
    # But our global U ordering is [ρ1..ρN, m1..mN, σ1..σN]. We need Dloc in that ordering.
    #
    # The above Dloc uses contiguous triplets; we must permute it to match U ordering.
    #
    return reorder_Dloc_node_triplet_to_block(Dloc)
end

# Helper: permutations to map triplet ordering -> block ordering (ρ1..ρN, m1..mN, σ1..σN)
function reorder_Dloc_node_triplet_to_block(Dtrip)
    # Dtrip: 3N x 3N with ordering [ρ1,m1,σ1, ρ2,m2,σ2, ...]
    # Want Dblock: 3N x 3N with ordering [ρ1..ρN, m1..mN, σ1..σN]
    perm = Vector{Int}(undef, 3N)
    # mapping from new position -> old position index
    # new idx: 1..N -> rho positions old: 1,4,7,...
    for i in 1:N
        perm[i] = 3*(i-1) + 1           # new pos i takes old pos of ρ_i
    end
    for i in 1:N
        perm[N + i] = 3*(i-1) + 2       # m_i old pos
    end
    for i in 1:N
        perm[2N + i] = 3*(i-1) + 3      # sigma_i old pos
    end
    # Build permutation matrix P such that Dblock = P * Dtrip * P'
    P = spzeros(3N, 3N)
    for newi in 1:3N
        oldi = perm[newi]
        P[newi, oldi] = 1.0
    end
    Dblock = P * Dtrip * transpose(P)
    return Dblock
end

# ------------------- Map nodal variation -> coefficient vector -----------
# Solve M * alpha = nodal_vector for each block (three times)
# We'll compute Minv explicitly via factorization for small N (paper-exact).
function compute_Minv(M)
    # use dense inverse via factorization (paper-exact mapping)
    Minv = Array{Float64}(undef, size(M,1), size(M,2))
    # Solve M * X = I for X
    Id = Matrix(sparse(I,size(M,1)))
    # use LU on dense conversion for stability
    Minv .= Matrix(M) \ Id
    return Minv
end

# produce block-diagonal Minv_full
function build_Minv_full(Minv)
    n = size(Minv,1)
    Mfull = spzeros(3n, 3n)
    for b in 0:2
        rows = b*n .+ (1:n)
        cols = rows
        Mfull[rows, cols] = Minv
    end
    return Mfull
end

# ------------------- Assemble P and G (3N x 3N) -----------------------
"""
assemble_P_G(Minv, K, C, Re, Pr)

We use:
  D = Minv * C
  P blocks:
    P[ρ,m] = -D
    P[m,ρ] = D'    (so mass-weighted antisymmetry)
    P[m,σ] = K
    P[σ,m] = -K'
  G blocks (symmetric PSD):
    G[m,m] = (1/Re) * K
    G[σ,σ] = (1/(Re*Pr)) * K
"""
function assemble_P_G(Minv, K, C, Re, Pr)
    n = size(Minv,1)
    Mdim = 3n
    P = spzeros(Mdim, Mdim)
    G = spzeros(Mdim, Mdim)

    D = Minv * C   # exact mapping

    # fill P
    P[1:n, n+1:2n] = -D
    P[n+1:2n, 1:n] = transpose(D)
    P[n+1:2n, 2n+1:3n] = K
    P[2n+1:3n, n+1:2n] = -transpose(K)

    # fill G
    G[n+1:2n, n+1:2n] = (1.0 / Re) * K
    G[2n+1:3n, 2n+1:3n] = (1.0 / (Re * Pr)) * K

    # enforce symmetry of G numerically
    G = 0.5 * (G + transpose(G))

    return P, G
end

# ------------------ Helpers: norms and checks ------------------------
# mass-weighted antisymmetry check: || Mblock*P + (Mblock*P)' ||
function check_P_antisymmetry(M, P)
    n = size(M,1)
    Mblock = spzeros(3n, 3n)
    Mblock[1:n,1:n] = M
    Mblock[n+1:2n, n+1:2n] = M
    Mblock[2n+1:3n, 2n+1:3n] = M
    S = Mblock * P
    norm_val = norm(Matrix(S + transpose(S)))
    return norm_val
end

function check_G_symmetry(G)
    return norm(Matrix(G - transpose(G)))
end

# ------------------ Residual and Jacobian for implicit midpoint ----------
"""
Given Uold and Unew, form residual R = Unew - Uold - dt * ( P * α_H(mid) + G * α_S(mid) )
where α_H(mid) = Minv_full * dHnod(mid)  and similarly for α_S,
and dHnod(mid) is computed at Umid = 0.5*(Uold + Unew).

Returns residual vector R (3N).
"""
function residual_midpoint(Uold, Unew, dt, Minv, Minv_full, P, G)
    Umid = 0.5 * (Uold + Unew)
    dHnod, dSnod = compute_dH_dS_nodal(Umid)
    # map to coefficient vectors
    dHcoef = Minv_full * dHnod
    dScoef = Minv_full * dSnod
    RHS = P * dHcoef + G * dScoef
    R = Unew - Uold - dt * RHS
    return R, dHnod, dSnod, dHcoef, dScoef
end

"""
Analytic Jacobian J = I - (dt/2) * P * Minv_full * Dloc
where Dloc = ∂(dHnod)/∂U evaluated at Umid (3N×3N, in block ordering).
"""
function build_jacobian(Uold, Unew, dt, Minv_full, P)
    Umid = 0.5 * (Uold + Unew)
    Dloc = build_Dloc(Umid)                # in block ordering
    J = I - (dt/2.0) * (P * (Minv_full * Dloc))
    return J
end

# ------------------ Newton solver for implicit midpoint -----------------
"""
solve_midpoint_newton(Uold, dt, Minv, Minv_full, P, G)

Returns Unew (solution), number of Newton iterations, boolean success.
"""
function solve_midpoint_newton(Uold, dt, Minv, Minv_full, P, G)
    # initial guess: explicit predictor
    dHnod0, dSnod0 = compute_dH_dS_nodal(Uold)
    dHcoef0 = Minv_full * dHnod0
    dScoef0 = Minv_full * dSnod0
    RHS0 = P * dHcoef0 + G * dScoef0
    Unew = Uold + dt * RHS0   # predictor

    for it in 1:max_newton
        R, dHnod, dSnod, dHcoef, dScoef = residual_midpoint(Uold, Unew, dt, Minv, Minv_full, P, G)
        rnorm = norm(R)
        if rnorm < newton_tol
            return Unew, it, true
        end

        # Build Jacobian analytically
        J = build_jacobian(Uold, Unew, dt, Minv_full, P)

        # Solve J * delta = -R
        # Convert to dense for direct solve (OK for moderate N)
        Jmat = Matrix(J)
        # small regularization for stability
        for i in 1:size(Jmat,1)
            if abs(Jmat[i,i]) < 1e-16
                Jmat[i,i] += 1e-16
            end
        end
        delta = Jmat \ (-R)

        # damping (optional): simple backtracking if needed
        alpha = 1.0
        Utry = Unew + alpha * delta
        Rtry, _, _, _, _ = residual_midpoint(Uold, Utry, dt, Minv, Minv_full, P, G)
        rtry = norm(Rtry)
        while rtry > 0.99 * rnorm && alpha > 1e-6
            alpha *= 0.5
            Utry = Unew + alpha * delta
            Rtry, _, _, _, _ = residual_midpoint(Uold, Utry, dt, Minv, Minv_full, P, G)
            rtry = norm(Rtry)
        end
        Unew = Utry

        # basic NaN/Inf check
        if any(!isfinite, Unew)
            return Unew, it, false
        end
    end
    return Unew, max_newton, false
end

# -------------------- Energy & Entropy functionals (discrete) ----------
# Discrete H = ∑ nodes (0.5*m^2/ρ + ρ * u(ρ,s)) * w_i, with w_i=mass diagonal? For Galerkin use integral via M.
# We'll compute H = (dHnod)' * (M * ones?) Simpler: reconstruct nodal densities and integrate via mass:
function compute_H_discrete(U, M)
    # H = ∑_ij [ (1/2 m_i^2 / ρ_i + ρ_i * u_i) * M_ii? ]
    # Exact integral in FE: H = sum_k ( dHnod_k * (M * Uvar)_k?) It's simpler to compute pointwise and integrate with M.
    rho = U[1:N]
    m = U[N+1:2N]
    σ = U[2N+1:3N]
    hvals = zeros(N)
    for i in 1:N
        r = max(rho[i], rho_min)
        mi = m[i]
        si = σ[i] / r
        u = u_of(r, si)
        hvals[i] = 0.5 * mi^2 / r + r * u
    end
    # integrate hvals with mass matrix: H = hvals' * (M * ones)
    H = dot(hvals, Matrix(M) * ones(N))
    return H
end

function compute_S_discrete(U, M)
    # S = ∫ σ dx approximated by nodal quadrature: S = σ' * (M * ones)
    σ = U[2N+1:3N]
    S = dot(σ, Matrix(M) * ones(N))
    return S
end

# ----------------------- Initial condition --------------------------
function initial_condition(N)
    x = collect(0:h:(L - h))
    rho0 = ones(N) .+ 0.1 * exp.(-50.0 * (x .- 0.5).^2)
    vel0 = zeros(N)
    σ0 = rho0 .* 0.0   # zero entropy per mass initially
    m0 = rho0 .* vel0
    U0 = vcat(rho0, m0, σ0)
    return U0
end

# ----------------------- Main program -------------------------------
function main()
    # Assemble FE matrices
    M, K, C = assemble_M_K_C(N, h)
    @printf("Assembled M, K, C: N=%d, h=%g\n", N, h)

    # compute Minv and Minv_full
    Minv = compute_Minv(M)             # dense inverse (paper-exact)
    Minv_full = build_Minv_full(Minv)  # sparse block diag

    # assemble P and G
    P, G = assemble_P_G(Minv, K, C, Re, Pr)

    # verification checks
    antisym_norm = check_P_antisymmetry(M, P)
    Gsym_norm = check_G_symmetry(G)
    @printf("Mass-weighted antisymmetry norm ||Mblock*P + (Mblock*P)'|| = %.6e\n", antisym_norm)
    @printf("G symmetry norm ||G - G'|| = %.6e\n", Gsym_norm)

    # optionally check G eigenvalues (small N)
    if N <= 200
        geigs = eigen(Matrix(G)).values
        min_ev = minimum(real(geigs))
        @printf("G min eigenvalue = %.6e\n", min_ev)
    end

    # initial condition
    U = initial_condition(N)
    @printf("Initial H = %.12e  S = %.12e\n", compute_H_discrete(U, M), compute_S_discrete(U, M))

    # time-stepping: implicit midpoint with Newton
    t = 0.0
    step = 0
    # compute initial umax for CFL
    # approximate T ~ rho^(γ-1) * exp((γ-1)s)
    function estimate_umax(U)
        rho = U[1:N]; m = U[N+1:2N]; σ = U[2N+1:3N]
        vmax = 0.0
        for i in 1:N
            r = max(rho[i], rho_min)
            vel = abs(m[i] / r)
            s = σ[i] / r
            T = max(r^(γ-1.0) * exp((γ-1.0)*s), T_min)
            vmax = max(vmax, vel + sqrt(γ * T))
        end
        return vmax
    end

    umax = estimate_umax(U)
    dt = CFL * h / umax
    @printf("Initial dt = %.6e (umax=%.6e)\n", dt, umax)

    # history outputs
    t_history = Float64[]
    H_history = Float64[]
    S_history = Float64[]

    while t < Tfinal - 1e-12
        step += 1
        Uold = copy(U)

        # solve implicit midpoint
        Unew, niters, success = solve_midpoint_newton(Uold, dt, Minv, Minv_full, P, G)

        if !success
            @printf("Newton failed at step %d, reducing dt and retrying\n", step)
            dt *= 0.5
            if dt < 1e-12
                error("dt too small, aborting")
            end
            step -= 1  # retry same step
            continue
        end

        U = Unew
        t += dt

        if step % max(1, Int(ceil((Tfinal/dt) / 20))) == 0 || t >= Tfinal - 1e-12
            Hval = compute_H_discrete(U, M)
            Sval = compute_S_discrete(U, M)
            push!(t_history, t); push!(H_history, Hval); push!(S_history, Sval)
            @printf("t=%.6f  step=%4d  niters=%2d  H=%.12e  S=%.12e\n", t, step, niters, Hval, Sval)
        end
    end

    # output solution file
    open("solution.dat","w") do io
        @printf(io, "# x rho vel p\n")
        rho = U[1:N]; m = U[N+1:2N]; σ = U[2N+1:3N]
        for i in 1:N
            r = rho[i]
            v = m[i] / max(r, rho_min)
            s = σ[i] / max(r, rho_min)
            T = r^(γ-1.0) * exp((γ-1.0)*s)
            p = r * T
            x = (i-1) * h
            @printf(io, "%12.6f  %12.6e  %12.6e  %12.6e\n", x, r, v, p)
        end
    end
    @printf("Saved solution.dat, energy/entropy histories available in memory arrays.\n")
end

# Run main
main()
