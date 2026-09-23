#!/usr/bin/env python3

"""
GWOSC gravitational-wave strain cleaning pipeline.

Example:

    python testing.py --event GW150914 --detector H1

Pipeline:

    GWOSC data
        |
        v
    detrending
        |
        v
    PSD estimation
        |
        v
    whitening
        |
        v
    bandpass
        |
        v
    cleaned strain
        |
        +----> time-domain waveform
        |
        +----> spectrogram
        |
        +----> PSD

The program uses:
    - gwosc
    - requests
    - h5py
    - numpy
    - scipy
    - matplotlib

The GWOSC API is queried directly because this makes the
file-selection logic explicit and avoids assumptions about the
structure returned by older versions of the gwosc Python client.
"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path
from urllib.parse import urlparse

import h5py
import matplotlib.pyplot as plt
import numpy as np
import requests

from scipy import signal


# ================================================================
# Configuration
# ================================================================

API_BASE = "https://gwosc.org/api/v2"

CACHE_DIR = Path("gwosc_cache")
OUTPUT_DIR = Path("gw_output")

DEFAULT_SAMPLE_RATE = 4096
DEFAULT_DURATION = 4096

DEFAULT_LOW_FREQ = 20.0
DEFAULT_HIGH_FREQ = 500.0

DEFAULT_PSD_SEGMENT = 8.0
DEFAULT_PSD_OVERLAP = 4.0


# ================================================================
# HTTP utilities
# ================================================================

def get_json(url: str, params: dict | None = None) -> dict:
    """
    Perform a GET request and return JSON.
    """

    try:
        response = requests.get(
            url,
            params=params,
            timeout=30,
        )

        response.raise_for_status()

    except requests.RequestException as exc:
        raise RuntimeError(
            f"GWOSC request failed:\n"
            f"  URL: {url}\n"
            f"  Error: {exc}"
        ) from exc

    try:
        return response.json()

    except ValueError as exc:
        raise RuntimeError(
            f"GWOSC returned invalid JSON:\n"
            f"  URL: {response.url}"
        ) from exc


# ================================================================
# Event metadata
# ================================================================
def get_event_version(event: str) -> dict:
    """
    Retrieve the correct GWOSC event version for an event.

    For example:
        GW150914 -> GW150914-v2
    """

    url = f"https://gwosc.org/api/v2/events/{event}?format=api"

    response = requests.get(url, timeout=30)
    response.raise_for_status()

    data = response.json()

    if data.get("name") != event:
        raise RuntimeError(
            f"GWOSC returned unexpected event: "
            f"{data.get('name')!r}"
        )

    versions = data.get("versions", [])

    if not versions:
        raise RuntimeError(
            f"No GWOSC versions found for {event!r}."
        )

    # Prefer version 2 for GW150914 because it contains
    # the corrected 4 kHz data.
    preferred = next(
        (v for v in versions if v.get("version") == 2),
        None,
    )

    if preferred is None:
        preferred = versions[0]

    version_number = preferred["version"]

    detail_url = (
        f"https://gwosc.org/api/v2/"
        f"event-versions/{event}-v{version_number}"
        f"?format=api"
    )

    detail_response = requests.get(
        detail_url,
        timeout=30,
    )
    detail_response.raise_for_status()

    return detail_response.json()


# ================================================================
# Strain-file discovery
# ================================================================

def get_event_strain_files(
    event_version: str,
) -> list[dict]:
    """
    Query the event-version strain-file endpoint.

    The v2 API exposes:
        detector
        sample_rate_kHz
        duration
        file_format
        download_url
    """

    url = (
        f"{API_BASE}/event-versions/"
        f"{event_version}/strain-files"
    )

    data = get_json(
        url,
        params={
            "pagesize": 100,
        },
    )

    return data.get("results", [])


def choose_strain_file(
    files: list[dict],
    detector: str,
    sample_rate: int,
    duration: int,
) -> tuple[str, dict]:
    """
    Select an HDF5 strain file matching the requested detector,
    sample rate, and duration.
    """

    desired_khz = sample_rate // 1000

    candidates = []

    for item in files:

        item_detector = str(
            item.get("detector", "")
        ).upper()

        item_rate = item.get(
            "sample_rate_kHz"
        )

        item_duration = item.get(
            "duration"
        )

        item_format = str(
            item.get("file_format", "")
        ).upper()

        if item_detector != detector.upper():
            continue

        if item_rate != desired_khz:
            continue

        if item_duration != duration:
            continue

        if item_format not in {
            "HDF",
            "HDF5",
        }:
            continue

        download_url = item.get(
            "download_url"
        )

        if download_url:
            candidates.append(
                (download_url, item)
            )

    if not candidates:

        available = []

        for item in files:

            if str(
                item.get("detector", "")
            ).upper() == detector.upper():

                available.append(
                    (
                        item.get("sample_rate_kHz"),
                        item.get("duration"),
                        item.get("file_format"),
                    )
                )

        raise RuntimeError(
            "Could not find the requested GWOSC HDF5 file.\n\n"
            f"Requested:\n"
            f"  detector    = {detector}\n"
            f"  sample rate = {sample_rate} Hz\n"
            f"  duration    = {duration} s\n\n"
            f"Available files for {detector}:\n"
            f"  {available}"
        )

    return candidates[0]


# ================================================================
# Download
# ================================================================

def download_file(
    url: str,
    destination: Path,
) -> Path:
    """
    Download a file, unless it is already cached.
    """

    destination.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    if destination.exists():

        print(
            f"[download] Using cached file:\n"
            f"           {destination}"
        )

        return destination

    print(
        f"[download] Downloading:\n"
        f"           {url}"
    )

    print(
        f"[download] Destination:\n"
        f"           {destination}"
    )

    try:

        with requests.get(
            url,
            stream=True,
            timeout=60,
        ) as response:

            response.raise_for_status()

            with open(
                destination,
                "wb",
            ) as output:

                for chunk in response.iter_content(
                    chunk_size=1024 * 1024,
                ):

                    if chunk:
                        output.write(chunk)

    except requests.RequestException as exc:

        if destination.exists():
            destination.unlink()

        raise RuntimeError(
            f"Could not download GWOSC file:\n{exc}"
        ) from exc

    print(
        "[download] Download complete."
    )

    return destination


def obtain_strain_file(
    event_version: str,
    detector: str,
    sample_rate: int,
    duration: int,
) -> Path:
    """
    Discover and download the requested GWOSC HDF5 file.
    """

    files = get_event_strain_files(
        event_version
    )

    url, metadata = choose_strain_file(
        files,
        detector=detector,
        sample_rate=sample_rate,
        duration=duration,
    )

    parsed = urlparse(url)

    filename = os.path.basename(
        parsed.path
    )

    if not filename:
        filename = (
            f"{detector}_"
            f"{sample_rate}Hz_"
            f"{duration}s.hdf5"
        )

    destination = (
        CACHE_DIR / filename
    )

    print(
        f"[pipeline] Selected GWOSC file:"
    )

    print(
        f"           detector    = "
        f"{metadata.get('detector')}"
    )

    print(
        f"           sample rate = "
        f"{metadata.get('sample_rate_kHz')} kHz"
    )

    print(
        f"           duration    = "
        f"{metadata.get('duration')} s"
    )

    print(
        f"           format      = "
        f"{metadata.get('file_format')}"
    )

    return download_file(
        url,
        destination,
    )


# ================================================================
# HDF5 reading
# ================================================================

def read_hdf5(
    filename: Path,
) -> tuple[np.ndarray, np.ndarray, float]:
    """
    Read GWOSC HDF5 strain.

    Returns:

        time
        strain
        sample_rate
    """

    print(
        f"[hdf5] Reading {filename}"
    )

    with h5py.File(
        filename,
        "r",
    ) as f:

        if "strain" not in f:

            raise RuntimeError(
                "HDF5 file does not contain "
                "a 'strain' group."
            )

        if "Strain" not in f["strain"]:

            raise RuntimeError(
                "HDF5 file does not contain "
                "'strain/Strain'."
            )

        strain = np.asarray(
            f["strain"]["Strain"][:],
            dtype=np.float64,
        )

        if "meta" not in f:

            raise RuntimeError(
                "HDF5 file does not contain "
                "a 'meta' group."
            )

        meta = f["meta"]

        if "GPSstart" not in meta:
            raise RuntimeError(
                "HDF5 metadata does not contain "
                "'GPSstart'."
            )

        if "Xspacing" not in meta:
            raise RuntimeError(
                "HDF5 metadata does not contain "
                "'Xspacing'."
            )

        gps_start = float(
            meta["GPSstart"][()]
        )

        xspacing = float(
            meta["Xspacing"][()]
        )

    if xspacing <= 0:

        raise RuntimeError(
            f"Invalid Xspacing: {xspacing}"
        )

    sample_rate = 1.0 / xspacing

    time = (
        gps_start
        + np.arange(
            len(strain),
            dtype=np.float64,
        ) / sample_rate
    )

    if not np.all(
        np.isfinite(strain)
    ):

        raise RuntimeError(
            "Strain contains NaN or infinite values."
        )

    print(
        f"[hdf5] GPS start:    {gps_start:.6f}"
    )

    print(
        f"[hdf5] GPS end:      {time[-1]:.6f}"
    )

    print(
        f"[hdf5] samples:      {len(strain):,}"
    )

    print(
        f"[hdf5] sample rate:  {sample_rate:.6f} Hz"
    )

    return (
        time,
        strain,
        sample_rate,
    )


# ================================================================
# Conditioning
# ================================================================

def detrend(
    strain: np.ndarray,
) -> np.ndarray:

    return signal.detrend(
        strain,
        type="linear",
    )


def taper(
    strain: np.ndarray,
    alpha: float = 0.02,
) -> np.ndarray:

    window = signal.windows.tukey(
        len(strain),
        alpha=alpha,
    )

    return strain * window


# ================================================================
# PSD
# ================================================================

def estimate_psd(
    strain: np.ndarray,
    sample_rate: float,
    segment_duration: float = DEFAULT_PSD_SEGMENT,
    overlap_duration: float = DEFAULT_PSD_OVERLAP,
) -> tuple[np.ndarray, np.ndarray]:

    nperseg = int(
        segment_duration * sample_rate
    )

    noverlap = int(
        overlap_duration * sample_rate
    )

    nperseg = min(
        nperseg,
        len(strain),
    )

    noverlap = min(
        noverlap,
        nperseg // 2,
    )

    frequencies, psd = signal.welch(
        strain,
        fs=sample_rate,
        window="hann",
        nperseg=nperseg,
        noverlap=noverlap,
        detrend="constant",
        scaling="density",
    )

    return frequencies, psd


# ================================================================
# Whitening
# ================================================================

def whiten(
    strain: np.ndarray,
    sample_rate: float,
    psd_frequency: np.ndarray,
    psd: np.ndarray,
) -> np.ndarray:
    """
    Frequency-domain whitening.

    Conceptually:

        h_w(f) = h(f) / sqrt(S_n(f))

    The PSD is interpolated onto the FFT frequency grid.
    """

    n = len(strain)

    strain_fft = np.fft.rfft(
        strain
    )

    frequencies = np.fft.rfftfreq(
        n,
        d=1.0 / sample_rate,
    )

    valid = (
        np.isfinite(psd_frequency)
        & np.isfinite(psd)
        & (psd > 0)
    )

    if np.count_nonzero(valid) < 2:

        raise RuntimeError(
            "PSD contains insufficient valid data."
        )

    psd_interp = np.interp(
        frequencies,
        psd_frequency[valid],
        psd[valid],
        left=np.nan,
        right=np.nan,
    )

    valid_fft = (
        np.isfinite(psd_interp)
        & (psd_interp > 0)
    )

    whitened_fft = np.zeros_like(
        strain_fft,
        dtype=np.complex128,
    )

    whitened_fft[valid_fft] = (
        strain_fft[valid_fft]
        / np.sqrt(
            psd_interp[valid_fft]
        )
    )

    # Discrete Fourier normalization.
    whitened_fft *= np.sqrt(
        2.0 / sample_rate
    )

    result = np.fft.irfft(
        whitened_fft,
        n=n,
    )

    return result


# ================================================================
# Bandpass
# ================================================================

def bandpass(
    strain: np.ndarray,
    sample_rate: float,
    low: float,
    high: float,
    order: int = 4,
) -> np.ndarray:

    nyquist = sample_rate / 2.0

    if low <= 0:
        raise ValueError(
            "Low frequency must be positive."
        )

    if high >= nyquist:
        raise ValueError(
            f"High frequency must be below "
            f"Nyquist = {nyquist} Hz."
        )

    if low >= high:
        raise ValueError(
            "Low frequency must be less than "
            "high frequency."
        )

    sos = signal.butter(
        order,
        [low, high],
        btype="bandpass",
        fs=sample_rate,
        output="sos",
    )

    return signal.sosfiltfilt(
        sos,
        strain,
    )


# ================================================================
# Event window
# ================================================================

def extract_window(
    time: np.ndarray,
    strain: np.ndarray,
    gps_event: float,
    before: float,
    after: float,
) -> tuple[np.ndarray, np.ndarray]:

    start = gps_event - before
    stop = gps_event + after

    mask = (
        (time >= start)
        & (time <= stop)
    )

    if np.count_nonzero(mask) == 0:

        raise RuntimeError(
            "The requested event window does not "
            "overlap the downloaded strain data."
        )

    return (
        time[mask],
        strain[mask],
    )


# ================================================================
# Plotting
# ================================================================

def plot_raw(
    time: np.ndarray,
    strain: np.ndarray,
    gps_event: float,
    output: Path,
):
    """
    Plot the raw strain around the event.
    """

    relative_time = (
        time - gps_event
    )

    plt.figure(
        figsize=(12, 5)
    )

    plt.plot(
        relative_time,
        strain,
        linewidth=0.6,
    )

    plt.axvline(
        0.0,
        linestyle="--",
        label="Event GPS time",
    )

    plt.xlabel(
        "Time relative to event [s]"
    )

    plt.ylabel(
        "Strain"
    )

    plt.title(
        "Raw detector strain"
    )

    plt.grid(
        True,
        alpha=0.3,
    )

    plt.legend()

    plt.tight_layout()

    plt.savefig(
        output,
        dpi=200,
    )

    plt.close()


def plot_psd(
    frequency: np.ndarray,
    psd: np.ndarray,
    output: Path,
):
    """
    Plot amplitude spectral density.
    """

    valid = (
        (frequency > 0)
        & (psd > 0)
        & np.isfinite(psd)
    )

    plt.figure(
        figsize=(10, 6)
    )

    plt.loglog(
        frequency[valid],
        np.sqrt(
            psd[valid]
        ),
    )

    plt.xlabel(
        "Frequency [Hz]"
    )

    plt.ylabel(
        r"ASD [$1/\sqrt{\mathrm{Hz}}$]"
    )

    plt.title(
        "Estimated detector noise"
    )

    plt.grid(
        True,
        which="both",
        alpha=0.3,
    )

    plt.tight_layout()

    plt.savefig(
        output,
        dpi=200,
    )

    plt.close()


def plot_whitened(
    time: np.ndarray,
    strain: np.ndarray,
    gps_event: float,
    output: Path,
):
    """
    Plot whitened strain.
    """

    relative_time = (
        time - gps_event
    )

    plt.figure(
        figsize=(12, 5)
    )

    plt.plot(
        relative_time,
        strain,
        linewidth=0.7,
    )

    plt.axvline(
        0.0,
        linestyle="--",
        label="Event time",
    )

    plt.xlabel(
        "Time relative to event [s]"
    )

    plt.ylabel(
        "Whitened strain"
    )

    plt.title(
        "Whitened gravitational-wave strain"
    )

    plt.grid(
        True,
        alpha=0.3,
    )

    plt.legend()

    plt.tight_layout()

    plt.savefig(
        output,
        dpi=200,
    )

    plt.close()


def plot_cleaned(
    time: np.ndarray,
    strain: np.ndarray,
    gps_event: float,
    low: float,
    high: float,
    output: Path,
):
    """
    Plot final cleaned strain.
    """

    relative_time = (
        time - gps_event
    )

    plt.figure(
        figsize=(12, 5)
    )

    plt.plot(
        relative_time,
        strain,
        linewidth=0.8,
    )

    plt.axvline(
        0.0,
        linestyle="--",
        label="Event time",
    )

    plt.xlabel(
        "Time relative to event [s]"
    )

    plt.ylabel(
        "Whitened strain"
    )

    plt.title(
        f"Cleaned GW strain: "
        f"{low:g}-{high:g} Hz"
    )

    plt.grid(
        True,
        alpha=0.3,
    )

    plt.legend()

    plt.tight_layout()

    plt.savefig(
        output,
        dpi=200,
    )

    plt.close()


def plot_spectrogram(
    strain: np.ndarray,
    sample_rate: float,
    output: Path,
    max_frequency: float,
):
    """
    Plot a time-frequency representation.
    """

    nperseg = int(
        0.25 * sample_rate
    )

    noverlap = int(
        0.20 * sample_rate
    )

    frequency, time, power = (
        signal.spectrogram(
            strain,
            fs=sample_rate,
            window="hann",
            nperseg=nperseg,
            noverlap=noverlap,
            scaling="density",
            mode="psd",
        )
    )

    mask = (
        frequency <= max_frequency
    )

    power_db = 10.0 * np.log10(
        power[mask] + 1e-30
    )

    plt.figure(
        figsize=(12, 6)
    )

    plt.pcolormesh(
        time,
        frequency[mask],
        power_db,
        shading="auto",
    )

    plt.xlabel(
        "Time [s]"
    )

    plt.ylabel(
        "Frequency [Hz]"
    )

    plt.title(
        "Cleaned strain spectrogram"
    )

    plt.colorbar(
        label="Power [dB]"
    )

    plt.tight_layout()

    plt.savefig(
        output,
        dpi=200,
    )

    plt.close()


# ================================================================
# Saving
# ================================================================

def save_results(
    filename: Path,
    time: np.ndarray,
    gps_event: float,
    raw: np.ndarray,
    whitened: np.ndarray,
    cleaned: np.ndarray,
    sample_rate: float,
):
    """
    Save the processed data.
    """

    np.savez(
        filename,

        time=time,

        relative_time=(
            time - gps_event
        ),

        raw_strain=raw,

        whitened_strain=whitened,

        cleaned_strain=cleaned,

        sample_rate=sample_rate,

        event_gps=gps_event,
    )


# ================================================================
# Command line
# ================================================================

def parse_arguments():

    parser = argparse.ArgumentParser(
        description=(
            "Download and clean gravitational-wave "
            "strain data from GWOSC."
        )
    )

    parser.add_argument(
        "--event",
        default="GW150914",
        help="GWOSC event name.",
    )

    parser.add_argument(
        "--detector",
        default="H1",
        help="Detector, e.g. H1, L1, or V1.",
    )

    parser.add_argument(
        "--sample-rate",
        type=int,
        default=DEFAULT_SAMPLE_RATE,
        choices=[4096, 16384],
        help="Sampling rate in Hz.",
    )

    parser.add_argument(
        "--duration",
        type=int,
        default=DEFAULT_DURATION,
        choices=[32, 4096],
        help="GWOSC strain-file duration.",
    )

    parser.add_argument(
        "--low",
        type=float,
        default=DEFAULT_LOW_FREQ,
        help="Lower bandpass frequency.",
    )

    parser.add_argument(
        "--high",
        type=float,
        default=DEFAULT_HIGH_FREQ,
        help="Upper bandpass frequency.",
    )

    parser.add_argument(
        "--before",
        type=float,
        default=32.0,
        help="Seconds before merger/event.",
    )

    parser.add_argument(
        "--after",
        type=float,
        default=8.0,
        help="Seconds after merger/event.",
    )

    return parser.parse_args()


# ================================================================
# Main
# ================================================================

def main():

    args = parse_arguments()

    CACHE_DIR.mkdir(
        parents=True,
        exist_ok=True,
    )

    OUTPUT_DIR.mkdir(
        parents=True,
        exist_ok=True,
    )

    print()
    print("=" * 70)
    print("GWOSC GRAVITATIONAL-WAVE CLEANING PIPELINE")
    print("=" * 70)
    print()

    # ------------------------------------------------------------
    # Event version
    # ------------------------------------------------------------

    print(
        f"[pipeline] Looking up event: "
        f"{args.event}"
    )

    event_version = get_event_version(
        args.event
    )

    print(
        f"[pipeline] Event version: "
        f"{event_version}"
    )

    # ------------------------------------------------------------
    # Event metadata
    # ------------------------------------------------------------

    metadata = get_event_metadata(
        event_version
    )

    gps_event = extract_gps_time(
        metadata
    )

    print(
        f"[pipeline] Event GPS time: "
        f"{gps_event:.6f}"
    )

    # ------------------------------------------------------------
    # Strain file
    # ------------------------------------------------------------

    strain_file = obtain_strain_file(
        event_version=event_version,
        detector=args.detector,
        sample_rate=args.sample_rate,
        duration=args.duration,
    )

    # ------------------------------------------------------------
    # Read HDF5
    # ------------------------------------------------------------

    time, strain, sample_rate = read_hdf5(
        strain_file
    )

    # ------------------------------------------------------------
    # Raw event window
    # ------------------------------------------------------------

    event_time, event_strain = extract_window(
        time,
        strain,
        gps_event,
        before=args.before,
        after=args.after,
    )

    print()
    print(
        "[pipeline] Event window:"
    )

    print(
        f"           start = "
        f"{event_time[0] - gps_event:+.3f} s"
    )

    print(
        f"           end   = "
        f"{event_time[-1] - gps_event:+.3f} s"
    )

    # ------------------------------------------------------------
    # Raw plot
    # ------------------------------------------------------------

    print(
        "[pipeline] Plotting raw strain..."
    )

    plot_raw(
        event_time,
        event_strain,
        gps_event,
        OUTPUT_DIR / "01_raw_strain.png",
    )

    # ------------------------------------------------------------
    # PSD
    #
    # Estimate PSD from the complete downloaded file.
    # This is preferable to estimating it from only the merger
    # window because the event occupies a tiny fraction of the
    # long data segment.
    # ------------------------------------------------------------

    print(
        "[pipeline] Estimating detector PSD..."
    )

    psd_input = detrend(
        strain
    )

    frequency_psd, psd = estimate_psd(
        psd_input,
        sample_rate,
    )

    plot_psd(
        frequency_psd,
        psd,
        OUTPUT_DIR / "02_noise_psd.png",
    )

    # ------------------------------------------------------------
    # Detrend event segment
    # ------------------------------------------------------------

    print(
        "[pipeline] Detrending event data..."
    )

    conditioned = detrend(
        event_strain
    )

    # ------------------------------------------------------------
    # Taper
    # ------------------------------------------------------------

    print(
        "[pipeline] Applying Tukey taper..."
    )

    conditioned = taper(
        conditioned
    )

    # ------------------------------------------------------------
    # Whitening
    # ------------------------------------------------------------

    print(
        "[pipeline] Whitening..."
    )

    whitened = whiten(
        conditioned,
        sample_rate,
        frequency_psd,
        psd,
    )

    plot_whitened(
        event_time,
        whitened,
        gps_event,
        OUTPUT_DIR / "03_whitened_strain.png",
    )

    # ------------------------------------------------------------
    # Bandpass
    # ------------------------------------------------------------

    print(
        f"[pipeline] Applying "
        f"{args.low:g}-{args.high:g} Hz bandpass..."
    )

    cleaned = bandpass(
        whitened,
        sample_rate,
        args.low,
        args.high,
    )

    # ------------------------------------------------------------
    # Final plot
    # ------------------------------------------------------------

    plot_cleaned(
        event_time,
        cleaned,
        gps_event,
        args.low,
        args.high,
        OUTPUT_DIR / "04_cleaned_strain.png",
    )

    # ------------------------------------------------------------
    # Spectrogram
    # ------------------------------------------------------------

    print(
        "[pipeline] Computing spectrogram..."
    )

    plot_spectrogram(
        cleaned,
        sample_rate,
        OUTPUT_DIR / "05_spectrogram.png",
        max_frequency=args.high,
    )

    # ------------------------------------------------------------
    # Save data
    # ------------------------------------------------------------

    save_results(
        OUTPUT_DIR / "cleaned_strain.npz",
        event_time,
        gps_event,
        event_strain,
        whitened,
        cleaned,
        sample_rate,
    )

    # ------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------

    print()
    print("=" * 70)
    print("PIPELINE COMPLETE")
    print("=" * 70)
    print()

    print(
        f"Event:          {args.event}"
    )

    print(
        f"Event version:  {event_version}"
    )

    print(
        f"Detector:       {args.detector}"
    )

    print(
        f"GPS event:      {gps_event:.6f}"
    )

    print(
        f"Sample rate:    {sample_rate:.1f} Hz"
    )

    print(
        f"Bandpass:       "
        f"{args.low:g}-{args.high:g} Hz"
    )

    print()

    print(
        f"Output:         "
        f"{OUTPUT_DIR.resolve()}"
    )

    print()

    print(
        "Generated:"
    )

    print(
        "  01_raw_strain.png"
    )

    print(
        "  02_noise_psd.png"
    )

    print(
        "  03_whitened_strain.png"
    )

    print(
        "  04_cleaned_strain.png"
    )

    print(
        "  05_spectrogram.png"
    )

    print(
        "  cleaned_strain.npz"
    )

    print()


# ================================================================
# Entry point
# ================================================================

if __name__ == "__main__":

    try:

        main()

    except KeyboardInterrupt:

        print(
            "\nInterrupted."
        )

        sys.exit(1)

    except Exception as exc:

        print(
            "\nERROR:",
            exc,
            file=sys.stderr,
        )

        sys.exit(1)
