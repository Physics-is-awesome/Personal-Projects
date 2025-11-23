import imageio
from moviepy.editor import VideoFileClip
from PIL import Image
import os
import glob
video_picture = "picture" # or video  
pictures_path = "/mnt/c/Users/ajcas/Onedrive/Pictures"
if video_picture == "picture":
  files = glob.glob(os.path.join(pictures_path, ".png")) #gif_*
  files.sort()
  print("Found files:", files)  # Debugging
  if not files:
      raise RuntimeError("No matching files found! Check path and naming convention.")
  images = []
  for f in files:
    img = Image.open(f).convert("RGB")
    img = img.resize((400, 400))  # force consistent size
    images.append(img)
  images = [imageio.imread(f) for f in files]
  print("Number of frames:", len(images))


  imageio.mimsave("output.gif", images, duration=0.5)
elif video_picture == "video":
  clip = VideoFileClip("input.mp4")
  clip = clip.subclip(0, 5)   # first 5 seconds
  clip.write_gif("output.gif")
