from statistics import mean
import numpy as np
import soundfile as sf
from scipy.signal import butter, sosfiltfilt, detrend, resample
import h5py
from gwpy.timeseries import TimeSeries
from gwpy.frequencyseries import FrequencySeries
from matplotlib import pyplot as plt
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

def audio_from_GW(c, file, file_name):
    if GW_BH == True:
        with h5py.File(file, "r") as f:
            #time = f["time"][:]  maybe depending on structure
                strain_dataset = f["strain"]["Strain"]

                h = strain_dataset[:]

                dt = strain_dataset.attrs["Xspacing"]
                t0 = strain_dataset.attrs["Xstart"]

                sample_rate = 1.0 / dt
                time = t0 + np.arange(len(h)) * dt


    elif A_test == True:
        with h5py.File(file, "r") as f:
            time = f["time"][:]
            h = f["strain"][:]

            dt = time[1] - time[0]
            sample_rate = 1.0 / dt

    elif GW_D15 == True:
        with open(file, "r") as f:
            data = f.read().split()
            h = []
            for elem in data:
                try:
                    h.append(float(elem))
                except ValueError:
                    pass
            dt = 1 / 16384

            sample_rate = 16384

            h = np.array(h)

            

    else:
        # -- Set a GPS time:
        t0 = 1126259462.4    # -- GW150914 (1187008882.4 was actually GW170817!)
 
        #-- Choose detector as H1, L1, or V1
        detector = 'H1'
        center = int(t0)
        h = TimeSeries.fetch_open_data(detector, center-16, center+16, sample_rate=4096)
        dt = h.dt.value
        sample_rate = 1.0 / dt
        t_start = h.t0.value  # GPS time of the first fetched sample; other steps below strip
                               # this off, so we save it here to relocate the merger later
    BANDPASS_LOW, BANDPASS_HIGH = 20, 350
    #other stuff I may not need
    if detrending == True:
        h = h.detrend('linear')
 
 



    if whiten_do == True:
        h = TimeSeries(h, dt=dt, t0=t_start)
        h = h.whiten()
        edge = 1.0
        h = h.crop(h.span[0] + edge, h.span[1] - edge)
 
        q_source = h

        crop_before, crop_after = 1.0, 1.0     # seconds around the merger to keep
        #h = h.crop(t0 - crop_before, t0 + crop_after)
        h = h.value
    else:
        q_source = None
        crop_before, crop_after = 1.0, 0.5
 
    if bandpass_filer_2 == True:
            sos = butter(
            4,
            [BANDPASS_LOW, BANDPASS_HIGH],
            btype="bandpass",
            fs=sample_rate,
            output="sos"
            )
    
            h = sosfiltfilt(sos, h)
    
        # mandatory stuff
    if normalizing == True:
        h = h / np.max(np.abs(h))

    if use_derivative == True:
        dh = np.diff(h)
        dh_dt = dh / dt
    else:
        dh_dt = h.copy()
    fs = 1 / dt
 
 
    # stuff I am not sure if I need or
 
 
    
 
 
 
 
 
    if fading == True:
        fade_samples = int(0.01 * sample_rate)
 
        window = np.ones(len(dh_dt))
 
        window[:fade_samples] = np.linspace(0, 1, fade_samples)
        window[-fade_samples:] = np.linspace(1, 0, fade_samples)
 
        dh_dt *= window
 
 
 

 
 
    if speed:
        dh_dt = resample(
            dh_dt,
            int(len(dh_dt) / c)
        )


    sf.write(file_name, dh_dt, int(fs))
 
    t_axis = np.arange(len(dh_dt)) * dt
    fig_wave, ax_wave = plt.subplots()
    ax_wave.plot(t_axis, dh_dt)
    ax_wave.set_xlabel("Time [s]")
    ax_wave.set_ylabel("Amplitude (normalized)")
    ax_wave.set_title("Time-domain audio signal")
    fig_wave.savefig("chirp_waveform.png")
 
    if q_source is not None:
        qspec = q_source.q_transform(
            frange=(20, 500),
            qrange=(4, 64),
            outseg=(t0 - crop_before, t0 + crop_after),
            whiten=False,
        )
        plot = qspec.plot()
        ax = plot.gca()
        ax.set_yscale("log")
        ax.set_ylabel("Frequency [Hz]")
        ax.set_xlabel("Time [s]")
        ax.set_title("Q-transform (chirp check)")
        plot.savefig("chirp_qtransform.png")
    else:
        print("Skipping Q-transform: needs whiten_do=True (it needs several seconds "
              "of context to estimate the background, which the short final audio "
              "clip alone can't provide).")
 
    plt.show()
 
    print("Created ", file_name)
 
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
GW_BH = False # Use GW data from GW150914 to test
 
GW_D15 = False
 
A_test = False # Use A4 note file
 
make_test = False # Make h5 file for A4 note
 
normalizing = True # Scale to desired zone
 
whiten_do = True
 
mean_normalizing = False # Center around 0
 
detrending = True # Remove a low-frequency trend from the data
 
fading = False # Fade the audio in and out
 
bandpass_filer_2 = True # Remove frequincies outside of desired range

use_derivative = False # Use the derivative of the waveform to make it more audible

bandpass_filer_data = True # Remove frequincies outside of desired range
speed = False # Change the time scale by a factor C, making the waveform play faster or slower while shifting its audible frequencies.
c = 1 # speed level

if GW_BH == True:
    file ="H-H1_LOSC_4_V2-1126259446-32.hdf5"  

elif A_test == True:
    file = "A4.h5"
elif GW_D15 == True:
    file = "../../../../Documents/D15-3D_hp_flow_0090_090_16384Hz_001kpc.txt"

else:
    file = "nothing"

file_name = "Chirp_3.wav"



# running
if make_test == True:
    test_h5()


#inspect_h5(file)
audio_from_GW(c, file, file_name)
