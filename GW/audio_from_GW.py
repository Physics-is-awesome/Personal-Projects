from statistics import mean

import numpy as np
import soundfile as sf
from scipy.signal import butter, sosfiltfilt, detrend
import h5py



import h5py

def inspect_h5(filename):

    with h5py.File(filename, "r") as f:

        print("=== STRAIN DATASET ===")
        h = f["strain"]["Strain"]

        print(h)
        print("Shape:", h.shape)
        print("Dtype:", h.dtype)

        print("\nStrain dataset attributes:")
        for key, value in h.attrs.items():
            print(key, "=", value)

        print("\n=== STRAIN GROUP ===")
        print("Strain group attributes:")
        for key, value in f["strain"].attrs.items():
            print(key, "=", value)

        print("\n=== META ===")
        print("Meta members:", list(f["meta"].keys()))

        print("\nMeta attributes:")
        for key, value in f["meta"].attrs.items():
            print(key, "=", value)

        print("\n=== QUALITY ===")
        print("Quality members:", list(f["quality"].keys()))

        print("\n=== COMPLETE HDF5 STRUCTURE ===")
        f.visititems(
            lambda name, obj:
            print(name, type(obj), getattr(obj, "shape", ""))
        )

def audio_from_GW(c, file):
    if GW_BH == True:
        with h5py.File(file, "r") as f:
            #time = f["time"][:]  maybe depending on structure
                strain_dataset = f["strain"]["Strain"]

                h = strain_dataset[:]

                dt = strain_dataset.attrs["Xspacing"]
                t0 = strain_dataset.attrs["Xstart"]

                sample_rate = 1.0 / dt
                time = t0 + np.arange(len(h)) * dt

    if A_test == True:
        with h5py.File(file, "r") as f:
            time = f["time"][:]
            h = f["strain"][:]

            dt = time[1] - time[0]
            sample_rate = 1.0 / dt

    # stuff I am not sure if I need or not
    if mean_normalizing == True:
        h = h - np.mean(h) 




    
    if detrending == True:
        h = detrend(h)
      


    if bandpass_filer == True:
        sos = butter(
        4,
        [20, 2000],
        btype="bandpass",
        fs=sample_rate,
        output="sos"
        )

        h = sosfiltfilt(sos, h)

        audio = h / np.max(np.abs(h))  




    if fading == True:
        fade_samples = int(0.01 * sample_rate)

        window = np.ones(len(h))

        window[:fade_samples] = np.linspace(0, 1, fade_samples)
        window[-fade_samples:] = np.linspace(1, 0, fade_samples)

        h *= window



    if normalizing == True:
        h = 0.8 * h / np.max(np.abs(h))


    if speed == True:
        h = int(len(h) / c)

    # mandatory stuff

    dh = np.diff(h)
    dt_array = np.diff(time)

    dh_dt = dh / dt_array
    fs = 1 / dt
    sf.write("audio_A.wav", dh_dt, int(fs))
    print("Created audio_A.wav")


###########################################
def test_h5():
    sample_rate = 44100
    duration = 3.0
    frequency = 440.0

    N = int(sample_rate * duration)

    t = np.arange(N) / sample_rate

    x = np.sin(2 * np.pi * frequency * t)
    with h5py.File("A4.h5", "w") as f:
        f.create_dataset("time", data=t)
        f.create_dataset("strain", data=x)

        f.attrs["sample_rate"] = sample_rate
        f.attrs["frequency"] = frequency

    print("Created A4.h5")

# paramaters
GW_BH = False # Use GW data from a black hole merger event (such as GW150914) instead of a test signal.

A_test = True # Use a test signal (such as a 440 Hz A4 tone) instead of GW data.
make_test = False # Create a known test signal (such as a 440 Hz A4 tone) instead of using GW data.

normalizing = False # Scale the waveform so its amplitude fits within the desired audio range.

mean_normalizing = False # Remove the waveform's mean value (DC offset) so it is centered around zero.

detrending = False # Remove a gradual linear or low-frequency trend from the waveform.

fading = False # Gradually fade the audio in at the beginning and out at the end to prevent clicks.

bandpass_filer = False # Keep only frequencies within a specified range to remove unwanted low- and high-frequency components.

speed = False # Change the time scale by a factor C, making the waveform play faster or slower while shifting its audible frequencies.
c = 1 # speed level 
if GW_BH == True:
    file ="H-H1_LOSC_4_V2-1126259446-32.hdf5"     
elif A_test == True:
    file = "A4.h5"





# running
if make_test == True:
    test_h5()


#inspect_h5(file)
audio_from_GW(c, file)

