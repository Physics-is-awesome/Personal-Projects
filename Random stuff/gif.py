import imageio
from PIL import Image
import os, glob

pictures_path = "/mnt/c/Users/ajcas/OneDrive/Pictures"
files = glob.glob(os.path.join(pictures_path, "*.png"))
files.sort()

# Open first image to get target size
base_img = Image.open(files[0])
target_size = base_img.size  # (width, height)

images = []
for f in files:
    img = Image.open(f).convert("RGB")
    img = img.resize(target_size)  # force same size
    images.append(img)

# Convert PIL images to numpy arrays for imageio
images = [imageio.imread(f) for f in files]

imageio.mimsave("output.gif", images, duration=0.5)

