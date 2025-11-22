import imageio
from moviepy.editor import VideoFileClip
import os
import glob
video_picture = "picture" # or video  
pictures_path = "/mnt/c/Users/ajcas/Pictures"
if video_picture == "picture":
  files = glob.glob(os.path.join(pictures_path, "*.png")) #gif_
  print("Found files:", files)  # Debugging
  if not files:
      raise RuntimeError("No matching files found! Check path and naming convention.")
  #images = [imageio.imread(f) for f in files]



  #imageio.mimsave("output.gif", images, duration=0.5)
elif video_picture == "video":
  clip = VideoFileClip("input.mp4")
  clip = clip.subclip(0, 5)   # first 5 seconds
  clip.write_gif("output.gif")
