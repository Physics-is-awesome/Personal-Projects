"""
gwmem_toy.py
============
A compact, from-scratch toy model of the pipeline in:

    Richardson, Andresen, Mezzacappa, Zanolin, Benjamin, Marronetti,
    Lentz & Szczepanczyk (2024), "Detecting Gravitational Wave Memory
    in the Next Galactic Core-Collapse Supernova", PRL 133, 231401
    (arXiv:2404.02131).

Every function here is labeled with the equation / figure of the paper
it stands in for. Nothing here is the real analysis (no CHIMERA
simulation output, no GWOSC strain) -- it is a minimal, self-contained
re-implementation of the *method*, built entirely with synthetic data,
so the logic (and its strengths/weaknesses) can be inspected directly.

Sections
--------
1. Synthetic detector noise            (stand-in for GWOSC O3b data)
2. Analytic memory template            (paper's Eq. 1, Table I)
3. Injection + high-pass conditioning  (paper's "Tapering"/"8 Hz Butterworth")
4. Linear Prediction Filter (LPF)      (paper's Eq. 2, Fig. 2/3)
5. Matched filtering / network stat    (paper's Eq. 3-4, Fig. 4)
6. False Alarm Probability             (paper's Fig. 5)
"""

import numpy as np
from scipy.signal import butter, filtfilt, welch

# ---------------------------------------------------------------------------
# 1. SYNTHETIC DETECTOR NOISE  (stand-in for GWOSC O3b Hanford/Livingston data)
# ---------------------------------------------------------------------------
#
# The paper injects signals into *real* LVK strain downloaded from GWOSC.
# We don't have network access to gwosc.org here, so instead we generate
# stationary Gaussian noise colored to a smooth analytic approximation of
# the aLIGO amplitude spectral density (ASD). This keeps the essential
# physics -- a steep low-frequency "seismic wall" below ~10-20 Hz, a
# sensitive "bucket" around 100-300 Hz, and a rising floor at high
# frequency -- without claiming to reproduce real detector artifacts
# (glitches, spectral lines, non-Gaussianity) that the paper's FAP study
# explicitly grapples with (their "glitch" near the D15 injection, e.g.).

def aligo_asd(f, f_seismic=12.0, n_seismic=8, f_bucket=180.0, asd_floor=4e-24,
              f_floor=1.0):
    """
    A smooth phenomenological aLIGO-like ASD [strain/sqrt(Hz)].

    Not a fit to the real noise curve -- just captures the qualitative
    shape (steep wall below ~f_seismic, minimum near f_bucket, mild rise
    at high frequency) that matters for this toy pipeline.

    `f_floor` clamps the frequency used in the (f_seismic/f)^n_seismic
    term to a minimum of 1 Hz. Without this, the power law keeps
    diverging as f -> 0 and, for a finite-duration FFT whose lowest
    bins sit at ~1/duration Hz, produces a dynamic range of >20 orders
    of magnitude between the DC bin and the ~100 Hz "bucket". That is
    unphysical (real noise budgets don't actually diverge) and it also
    silently destroys float64 precision in the FFT/IFFT round-trip used
    to color the noise -- the tiny, physically relevant bins get rounded
    away next to the enormous near-DC ones. Clamping at 1 Hz keeps the
    qualitative "steep wall" shape while keeping the dynamic range
    (here, ~4-5 orders of magnitude) representable.
    """
    f = np.asarray(f, dtype=float)
    f_eff = np.maximum(f, f_floor)
    seismic = (f_seismic / f_eff) ** n_seismic
    shot = (f_eff / (6 * f_bucket)) ** 2
    shape = 1.0 + seismic + shot
    return asd_floor * np.sqrt(shape)


def generate_noise(duration, fs, asd_func=aligo_asd, rng=None):
    """
    Generate a stationary Gaussian noise time series with one-sided ASD
    given by asd_func, using the standard frequency-domain coloring
    trick: draw white Gaussian noise in the frequency domain, scale each
    bin by the target ASD, inverse-FFT back to the time domain.
    """
    rng = np.random.default_rng() if rng is None else rng
    n = int(round(duration * fs))
    freqs = np.fft.rfftfreq(n, d=1.0 / fs)

    asd = asd_func(freqs)
    # Variance per (real,imag) frequency-domain component for a real
    # signal of length n sampled at fs, one-sided PSD S(f) = asd(f)**2:
    #   <|X_k|^2> = S(f_k) * n * fs / 4   (k not 0 or Nyquist)
    scale = asd * np.sqrt(n * fs) / 2.0
    re = rng.normal(0.0, 1.0, size=freqs.size)
    im = rng.normal(0.0, 1.0, size=freqs.size)
    xf = scale * (re + 1j * im)
    xf[0] = xf[0].real  # DC must be real
    if n % 2 == 0:
        xf[-1] = xf[-1].real  # Nyquist must be real
    x = np.fft.irfft(xf, n=n)
    return x


# ---------------------------------------------------------------------------
# 2. ANALYTIC MEMORY TEMPLATE  (paper's Eq. 1 and Table I)
# ---------------------------------------------------------------------------
#
#   h_fit(t) = L / (1 + exp(-k(t - t0)))                    ,  t < ts
#            + (L/2) * (1 + cos(2*pi*ft*(t - ts)))           ,  ts <= t < ts + 1/(2 ft)
#            + 0                                             ,  t >= ts + 1/(2 ft)
#
# t0: center of the logistic rise; k: inverse rise time; L: memory
# saturation value (in cm, i.e. D*h -- distance times strain); ts: time
# tapering begins; ft: tapering frequency. The cosine term smoothly
# tapers the saturated memory to zero over a half period 1/(2 ft), which
# is what keeps the abrupt end of a finite CCSN simulation from ringing
# in the Fourier domain.
#
# Table I values (D9.6, D15, D25) are reproduced verbatim from the paper
# and used here as the three toy "models".

MODELS = {
    "D9.6": dict(t0=0.28, L=-1.30, k=29.45, ts=0.300, ft=0.1),
    "D15":  dict(t0=0.60, L=17.73, k=22.60, ts=0.7414, ft=0.1),
    "D25":  dict(t0=0.41, L=24.23, k=18.73, ts=0.472, ft=0.1),
}

KPC_IN_CM = 3.0856775814913673e21


def memory_template(t, t0, L, k, ts, ft):
    """Eq. 1: tapered logistic memory waveform, D*h+ in cm."""
    t = np.asarray(t, dtype=float)
    logistic = L / (1.0 + np.exp(-k * (t - t0)))
    half_period = 1.0 / (2.0 * ft)
    taper = 0.5 * (1.0 + np.cos(2.0 * np.pi * ft * (t - ts)))
    tapered = np.where(t - ts < half_period, L * taper, 0.0)
    return np.where(t < ts, logistic, tapered)


def strain_at_distance(t, model, distance_kpc):
    """
    Convert the D*h template (cm) into actual dimensionless strain h(t)
    at a given source distance in kpc: h = (D*h) / D.
    """
    p = MODELS[model]
    Dh_cm = memory_template(t, **p)  # cm
    D_cm = distance_kpc * KPC_IN_CM
    return Dh_cm / D_cm


# ---------------------------------------------------------------------------
# 3. INJECTION + HIGH-PASS CONDITIONING
# ---------------------------------------------------------------------------

def highpass(x, fs, cutoff=8.0, order=4):
    """8 Hz Butterworth high-pass, applied the same way the paper applies
    it to GWOSC strain that already carries LVK's own high-pass."""
    b, a = butter(order, cutoff / (fs / 2.0), btype="highpass")
    return filtfilt(b, a, x)


def inject_signal(noise, fs, model, distance_kpc, t_inj, pad=2.0):
    """
    Inject strain_at_distance(model) into a noise time series starting
    at t_inj (seconds from the start of `noise`). `pad` seconds of extra
    template on either side (mostly zero, thanks to the taper) keep the
    injection smooth at its edges.
    """
    n = len(noise)
    t_full = np.arange(n) / fs
    p = MODELS[model]
    t_end = p["ts"] + 1.0 / (2 * p["ft"])  # template is exactly zero after this
    t_rel = t_full - t_inj
    sig = np.zeros(n)
    mask = (t_rel >= -pad) & (t_rel <= t_end + pad)
    sig[mask] = strain_at_distance(t_rel[mask], model, distance_kpc)
    return noise + sig, sig


# ---------------------------------------------------------------------------
# 4. LINEAR PREDICTION FILTER  (paper's Eq. 2, Fig. 2/3)
# ---------------------------------------------------------------------------
#
# The paper trains a 16384-tap linear predictor on a signal-free stretch
# of data and subtracts its one-step-ahead prediction from the full
# stream: S_hat = S - S_LPF. This is exactly an autoregressive (AR)
# prediction-error ("whitening") filter. We estimate the AR coefficients
# via the Levinson-Durbin recursion on the training segment's
# autocorrelation, then apply the resulting FIR prediction-error filter
# to the full stream with scipy's lfilter.

def levinson_durbin(autocorr, order):
    """
    Levinson-Durbin recursion. Returns AR coefficients a[1..order] such
    that the one-step predictor is x_hat(n) = sum_k a[k] * x(n-k).
    """
    r = autocorr
    a = np.zeros(order + 1)
    a[0] = 1.0
    e = r[0]
    for m in range(1, order + 1):
        acc = r[m] + np.dot(a[1:m], r[m - 1:0:-1])
        k = -acc / e
        a_new = a.copy()
        a_new[1:m] += k * a[m - 1:0:-1]
        a_new[m] = k
        a = a_new
        e *= (1 - k ** 2)
        if e <= 0:
            break
    # a[0]=1 is the "current sample" coefficient of the *error* filter
    # A(z) = 1 + a1 z^-1 + ... ; the predictor coefficients are -a[1:].
    return -a[1:], e


def train_lpf(training_data, order):
    """Estimate `order` LPF (AR) coefficients from a signal-free segment."""
    n = len(training_data)
    x = training_data - np.mean(training_data)
    # biased autocorrelation estimate, lags 0..order
    full = np.correlate(x, x, mode="full") / n
    mid = len(full) // 2
    r = full[mid: mid + order + 1]
    coeffs, err = levinson_durbin(r, order)
    return coeffs


def apply_lpf(data, coeffs):
    """
    Subtract the LPF's one-step-ahead prediction from `data` (Eq. 2:
    S_hat = S - S_LPF). Implemented as the prediction-error FIR filter
    e(n) = x(n) - sum_k coeffs[k-1] x(n-k).
    """
    p = len(coeffs)
    b = np.zeros(p + 1)
    b[0] = 1.0
    b[1:] = -coeffs
    # zero-phase-free (causal) filter -- causality matters here, unlike
    # the high-pass conditioning step, because this is a *predictor*.
    from scipy.signal import lfilter
    return lfilter(b, [1.0], data)


# ---------------------------------------------------------------------------
# 5. MATCHED FILTER / TWO-DETECTOR NETWORK STATISTIC  (Eq. 3-4, Fig. 4)
# ---------------------------------------------------------------------------

def make_compact_template(model, distance_kpc, fs, pad=2.0):
    """
    Build a short, standalone template array (NOT padded out to the full
    data length) spanning the memory waveform's own support, from
    t = -pad to t = t_end + pad relative to the template's own t=0. This
    is the array that gets *slid* across the data by discrete_correlation
    -- it is the analog of h_fit(t) in Eq. 3, considered on its own time
    axis rather than pre-placed at any particular trigger time.
    """
    p = MODELS[model]
    t_end = p["ts"] + 1.0 / (2 * p["ft"])
    n = int(round((t_end + 2 * pad) * fs))
    t_rel = np.arange(n) / fs - pad
    return strain_at_distance(t_rel, model, distance_kpc), pad


def discrete_correlation(s_hat, template):
    """
    Eq. 3: <S_hat, h_fit>(t_n) = sum_m S_hat(t_m) h_fit(t_{m-n})
    i.e. the cross-correlation of the whitened data stream with a
    *compact* template, evaluated at every possible trigger time n.
    `template` should be the short array from make_compact_template
    (or any array much shorter than s_hat) -- this is what makes the
    output directly interpretable as "matched-filter statistic vs.
    trigger time" on the same time axis as s_hat.
    """
    corr = np.correlate(s_hat, template, mode="same")
    return corr


def network_statistic(corr_H, corr_L):
    """Eq. 4: network statistic = product of single-detector correlations."""
    return corr_H * corr_L


def injection_only_correlation(model, distance_kpc, fs, t_inj, n_samples,
                                lpf_coeffs, template):
    """
    Correlation contribution from the injected signal ALONE (no noise),
    put through the same high-pass + LPF + matched-filter chain as the
    real data. Because both the high-pass filter, the LPF prediction-
    error filter, and the correlation are linear operators, this can be
    *added* to a pre-computed noise-only correlation to get the
    correlation of (noise + signal) -- without re-running the expensive
    LPF training/whitening for every model/distance combination:

        corr(noise + signal) = corr(noise) + corr(signal)

    This is exact (not an approximation) given the linearity of every
    stage, and it is what lets this toy sweep many (model, distance)
    combinations quickly using a single detector-noise realization and
    a single trained LPF.
    """
    clean = np.zeros(n_samples)
    clean_inj, _ = inject_signal(clean, fs, model, distance_kpc, t_inj)
    clean_hp = highpass(clean_inj, fs)
    clean_hat = apply_lpf(clean_hp, lpf_coeffs)
    return discrete_correlation(clean_hat, template)


def injection_index(model, distance_kpc, fs, t_inj, n_samples, template):
    """
    numpy's correlate(..., mode='same') introduces a fixed integer-sample
    offset between "where the signal actually is" and "where its peak
    shows up in the correlation output" (set by the template length and
    correlate's centering convention). Rather than re-deriving that
    offset analytically, we calibrate it directly: build a noise-free
    copy of the injection, correlate it against the same template, and
    read off the peak. This gives the exact sample index to treat as
    "the injection" (i.e. the paper's t_inj) in the correlation /
    network-statistic time series, for use as the FAP normalization
    reference and for excluding the on-source window from the background.
    """
    clean = np.zeros(n_samples)
    clean_inj, _ = inject_signal(clean, fs, model, distance_kpc, t_inj)
    corr = discrete_correlation(clean_inj, template)
    return int(np.argmax(np.abs(corr)))


# ---------------------------------------------------------------------------
# 6. FALSE ALARM PROBABILITY  (Fig. 5)
# ---------------------------------------------------------------------------

def segment_max(stat, fs, seg_sec, exclude_idx=None, exclude_width_sec=1.0):
    """
    Split `stat` into contiguous segments of length seg_sec (the toy
    analog of the paper's 2 s on-source window) and return the max of
    |stat| in each segment. Optionally exclude a window around
    exclude_idx (the injection) so the FAP background doesn't include
    the true event.
    """
    n = len(stat)
    seg_len = int(seg_sec * fs)
    n_seg = n // seg_len
    vals = np.empty(n_seg)
    excl_lo = excl_hi = None
    if exclude_idx is not None:
        w = int(exclude_width_sec * fs)
        excl_lo, excl_hi = exclude_idx - w, exclude_idx + w
    keep = np.ones(n_seg, dtype=bool)
    for i in range(n_seg):
        lo, hi = i * seg_len, (i + 1) * seg_len
        vals[i] = np.max(np.abs(stat[lo:hi]))
        if excl_lo is not None and not (hi < excl_lo or lo > excl_hi):
            keep[i] = False
    return vals, keep


def false_alarm_probability(background_vals, ref_val, thresholds):
    """
    FAP(threshold) = fraction of background segments whose (normalized)
    trigger statistic exceeds `threshold`, where normalization is by
    ref_val (the statistic's value at the true injection, as in the
    paper's ratio <S,h_fit>_N(t_n) / <S,h_fit>_N(t_inj)).
    """
    normalized = background_vals / ref_val
    return np.array([np.mean(normalized > th) for th in thresholds])
