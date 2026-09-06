"""
make_figures.py
================
Runs the toy pipeline (gwmem_toy.py) across all 3 CCSN models x 3 source
distances and reproduces toy analogs of the paper's Figures 1, 3, 4, 5.

Usage: python3 make_figures.py
Outputs PNGs into ./figures/
"""
import os
import time
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.signal import welch

import gwmem_toy as gw

OUTDIR = "figures"
os.makedirs(OUTDIR, exist_ok=True)

# ---------------------------------------------------------------------
# Global toy-experiment parameters (all much smaller than the paper's
# real analysis -- see README.md for the full list of what's scaled
# down and why).
# ---------------------------------------------------------------------
FS = 1024.0          # sample rate [Hz]        (paper: ~4096 Hz, GWOSC default)
TRAIN_DUR = 150.0    # LPF training data [s]   (paper: 2048 s)
SEARCH_DUR = 400.0   # search/background [s]   (paper: 4096 s)
LPF_ORDER = 500      # LPF taps                (paper: 16384 taps)
SEG_SEC = 2.0        # FAP on-source window [s] (paper: 2 s -- kept the same)
T_INJ = 200.0        # injection time within the search stream [s]
DISTANCES = [1.0, 10.0, 100.0]   # kpc, same as paper's Fig. 5
MODELS = list(gw.MODELS.keys())  # ["D9.6", "D15", "D25"]
SEED = 20240402

rng = np.random.default_rng(SEED)
N_SEARCH = int(round(SEARCH_DUR * FS))

print("=== 1. Generating synthetic detector noise (stand-in for GWOSC O3b) ===")
t0 = time.time()
train_H = gw.generate_noise(TRAIN_DUR, FS, rng=rng)
train_L = gw.generate_noise(TRAIN_DUR, FS, rng=rng)
search_H = gw.generate_noise(SEARCH_DUR, FS, rng=rng)
search_L = gw.generate_noise(SEARCH_DUR, FS, rng=rng)
print(f"    done in {time.time()-t0:.1f} s")

print("=== 2. High-pass conditioning (8 Hz Butterworth) ===")
train_H_hp = gw.highpass(train_H, FS)
train_L_hp = gw.highpass(train_L, FS)
search_H_hp = gw.highpass(search_H, FS)
search_L_hp = gw.highpass(search_L, FS)

print(f"=== 3. Training LPF ({LPF_ORDER} taps) per detector ===")
t0 = time.time()
coeffs_H = gw.train_lpf(train_H_hp, LPF_ORDER)
coeffs_L = gw.train_lpf(train_L_hp, LPF_ORDER)
print(f"    done in {time.time()-t0:.1f} s")

print("=== 4. Whitening the search data: S_hat = S - S_LPF (Eq. 2) ===")
Hhat_noise_only = gw.apply_lpf(search_H_hp, coeffs_H)
Lhat_noise_only = gw.apply_lpf(search_L_hp, coeffs_L)

# ---------------------------------------------------------------------
# Per-model matched filtering, exploiting linearity: correlate the
# *pure noise* once per model/template, then add each distance's
# injection-only correlation on top (see injection_only_correlation
# docstring for why this is exact, not approximate).
# ---------------------------------------------------------------------
print("=== 5. Matched filtering (Eq. 3-4) across all models/distances ===")
results = {}
for model in MODELS:
    template, pad = gw.make_compact_template(model, distance_kpc=10.0, fs=FS)
    template_hp = gw.highpass(template, FS)

    corr_noise_H = gw.discrete_correlation(Hhat_noise_only, template_hp)
    corr_noise_L = gw.discrete_correlation(Lhat_noise_only, template_hp)
    net_noise_only = gw.network_statistic(corr_noise_H, corr_noise_L)

    idx_inj = gw.injection_index(model, 10.0, FS, T_INJ, N_SEARCH, template_hp)

    per_distance = {}
    for D in DISTANCES:
        inj_corr_H = gw.injection_only_correlation(
            model, D, FS, T_INJ, N_SEARCH, coeffs_H, template_hp)
        inj_corr_L = gw.injection_only_correlation(
            model, D, FS, T_INJ, N_SEARCH, coeffs_L, template_hp)
        full_corr_H = corr_noise_H + inj_corr_H
        full_corr_L = corr_noise_L + inj_corr_L
        full_net = full_corr_H * full_corr_L
        ref_val = full_net[idx_inj]
        per_distance[D] = dict(full_net=full_net, ref_val=ref_val)

    results[model] = dict(
        template=template, template_hp=template_hp, pad=pad,
        net_noise_only=net_noise_only, idx_inj=idx_inj,
        per_distance=per_distance,
    )
    print(f"    {model}: idx_inj={idx_inj} "
          f"(t={idx_inj/FS:.2f}s, injected at {T_INJ}s)")

t_axis = np.arange(N_SEARCH) / FS

# ---------------------------------------------------------------------
# FIGURE 1 analog: waveform + fit, raw and high-pass filtered
# (paper's Fig. 1 -- here the "fit" IS the model by construction,
# since we start from the analytic template rather than fitting one
# to simulation output)
# ---------------------------------------------------------------------
print("=== Figure 1 analog: templates, raw and high-passed ===")
fig, axes = plt.subplots(2, 1, figsize=(7, 6), sharex=True)
t_plot = np.linspace(-0.5, 6.5, 4000)
colors = {"D9.6": "tab:green", "D15": "tab:orange", "D25": "tab:blue"}
for model in MODELS:
    p = gw.MODELS[model]
    h = gw.memory_template(t_plot, **p)
    scale = 10 if model == "D9.6" else 1
    axes[0].plot(t_plot, h * scale, color=colors[model],
                 label=f"{model}" + (r" $\times 10$" if scale != 1 else ""))
    h_hp = gw.highpass(gw.memory_template(np.arange(-2, 8, 1/FS), **p), FS)
    t_hp = np.arange(-2, 8, 1/FS)
    mask = (t_hp > -0.5) & (t_hp < 6.5)
    axes[1].plot(t_hp[mask], h_hp[mask] * scale, color=colors[model])
axes[0].set_ylabel(r"$D h_+$ [cm]")
axes[0].set_title("Toy analog of Fig. 1: analytic memory templates (Eq. 1, Table I)")
axes[0].legend()
axes[1].set_ylabel(r"Filtered $D h_+$ [cm]")
axes[1].set_xlabel("Time [s]")
axes[1].set_title("After 8 Hz high-pass Butterworth filter")
fig.tight_layout()
fig.savefig(f"{OUTDIR}/fig1_templates.png", dpi=140)
plt.close(fig)

# ---------------------------------------------------------------------
# FIGURE 3 analog: ASD before/after LPF whitening, vs. signal ASDs
# ---------------------------------------------------------------------
print("=== Figure 3 analog: ASD before/after LPF ===")
fig, axes = plt.subplots(2, 1, figsize=(7, 7), sharex=True)

f_noise, Pxx_noise = welch(search_H_hp, fs=FS, nperseg=8192)
axes[0].loglog(f_noise, np.sqrt(Pxx_noise), color="gray", label="Noise (pre-LPF)")

f_noise_w, Pxx_noise_w = welch(Hhat_noise_only, fs=FS, nperseg=8192)
axes[1].loglog(f_noise_w, np.sqrt(Pxx_noise_w), color="gray",
               label="Noise, LPF-subtracted")

for model in MODELS:
    template, pad = gw.make_compact_template(model, distance_kpc=1.0, fs=FS)
    padded = np.zeros(N_SEARCH)
    padded[:len(template)] = template
    f_s, Pxx_s = welch(padded, fs=FS, nperseg=8192)
    axes[0].loglog(f_s, np.sqrt(Pxx_s), color=colors[model], label=model)

    template_hp = gw.highpass(template, FS)
    padded_hp = np.zeros(N_SEARCH)
    padded_hp[:len(template_hp)] = template_hp
    whitened_sig = gw.apply_lpf(padded_hp, coeffs_H)
    f_sw, Pxx_sw = welch(whitened_sig, fs=FS, nperseg=8192)
    axes[1].loglog(f_sw, np.sqrt(Pxx_sw), color=colors[model])

for ax in axes:
    ax.set_xlim(1, FS / 2)
    ax.set_ylabel(r"ASD [Hz$^{-1/2}$]")
    ax.legend(fontsize=8)
axes[0].set_title("Toy analog of Fig. 3: ASD before LPF (signals @ 1 kpc)")
axes[1].set_title("After LPF whitening (Eq. 2)")
axes[1].set_xlabel("Frequency [Hz]")
fig.tight_layout()
fig.savefig(f"{OUTDIR}/fig3_asd.png", dpi=140)
plt.close(fig)

# ---------------------------------------------------------------------
# FIGURE 4 analog: normalized network correlation vs time.
# One column at 1 kpc (clear detections) and one at 10 kpc (marginal --
# only the strongest model stands out), matching the paper's own
# finding that detectability is very distance-sensitive. Per-panel
# y-limits are percentile-based (not fixed) because, exactly as in the
# paper's discussion of False Alarm Probability, the injected peak is
# not always the largest feature in the stream -- clipping the axis
# to a fixed range would hide that (real, important) behavior.
# ---------------------------------------------------------------------
print("=== Figure 4 analog: normalized correlation time series ===")
fig, axes = plt.subplots(len(MODELS), 2, figsize=(11, 7), sharex=True)
for row, model in enumerate(MODELS):
    r = results[model]
    for col, D in enumerate([1.0, 10.0]):
        ax = axes[row, col]
        full_net = r["per_distance"][D]["full_net"]
        ref = r["per_distance"][D]["ref_val"]
        y = full_net / ref
        ax.plot(t_axis, y, color=colors[model], lw=0.6)
        ax.axvline(T_INJ, color="k", ls="--", lw=1, alpha=0.6)
        ax.plot(T_INJ, y[r["idx_inj"]], "r*", markersize=8, zorder=5)
        ylim = max(1.5, np.percentile(np.abs(y), 99.5))
        ax.set_ylim(-ylim, ylim)
        if col == 0:
            ax.set_ylabel(model)
        if row == 0:
            ax.set_title(f"{D:.0f} kpc")
for ax in axes[-1, :]:
    ax.set_xlabel("Time [s]")
fig.suptitle("Toy analog of Fig. 4: two-detector network statistic\n"
             r"$\langle S,\hat h_{fit}\rangle_N(t_n)/\langle S,\hat h_{fit}\rangle_N(t_{inj})$"
             " (red star = injection; dashed = injection time)")
fig.tight_layout()
fig.savefig(f"{OUTDIR}/fig4_correlation.png", dpi=140)
plt.close(fig)

# ---------------------------------------------------------------------
# FIGURE 5 analog: FAP vs match threshold, for all models/distances
# ---------------------------------------------------------------------
print("=== Figure 5 analog: False Alarm Probability curves ===")
thresholds = np.linspace(0, 1, 60)
markers = {1.0: "o", 10.0: "^", 100.0: "s"}

fig, ax = plt.subplots(figsize=(7, 6))
for model in MODELS:
    r = results[model]
    bg_vals, _ = gw.segment_max(r["net_noise_only"], FS, SEG_SEC)
    for D in DISTANCES:
        ref = r["per_distance"][D]["ref_val"]
        fap = gw.false_alarm_probability(bg_vals, abs(ref), thresholds)
        ax.plot(thresholds, fap, color=colors[model], marker=markers[D],
                 markersize=3, markevery=6, linewidth=1,
                 label=f"{model}, {int(D)} kpc" if D != 10 else f"{model}, {int(D)} kpc")
ax.set_xlabel("Match threshold")
ax.set_ylabel("False Alarm Probability")
ax.set_title("Toy analog of Fig. 5: FAP vs. match threshold")
ax.legend(fontsize=7, ncol=3)
fig.tight_layout()
fig.savefig(f"{OUTDIR}/fig5_fap.png", dpi=140)
plt.close(fig)

print("\nAll figures written to", os.path.abspath(OUTDIR))

# ---------------------------------------------------------------------
# Print a compact numeric summary (toy analog of the paper's headline
# numbers): SNR-like ratio and a representative FAP at threshold=0.5
# ---------------------------------------------------------------------
print("\n=== Summary ===")
print(f"{'Model':6s} {'Dist [kpc]':>10s} {'ref/bg_std':>12s} {'FAP@0.5':>10s}")
for model in MODELS:
    r = results[model]
    bg_vals, _ = gw.segment_max(r["net_noise_only"], FS, SEG_SEC)
    bg_std = r["net_noise_only"].std()
    for D in DISTANCES:
        ref = r["per_distance"][D]["ref_val"]
        fap_at_half = gw.false_alarm_probability(bg_vals, abs(ref), [0.5])[0]
        print(f"{model:6s} {D:10.1f} {abs(ref)/bg_std:12.2f} {fap_at_half:10.3f}")
