from PIL import Image
import os, glob

pictures_path = "/mnt/c/Users/ajcas/OneDrive/Pictures"
files = glob.glob(os.path.join(pictures_path, "*.png"))
files.sort()

frames = [Image.open(f).convert("RGB") for f in files]

# Resize all frames to match the first
target_size = frames[0].size
frames = [f.resize(target_size) for f in frames]

frames[0].save("output.gif",
               save_all=True,
               append_images=frames[1:],
               duration=500,
               loop=0)
