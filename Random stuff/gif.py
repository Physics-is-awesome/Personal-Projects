import imageio
from moviepy.editor import VideoFileClip
import os
import glob
video_picture = "picture" # or video  
pictures_path = "/mnt/c/Users/ajcas/PicturesHo"
if video_picture == "picture":
  files = glob.glob(os.path.join(pictures_path, "gif_*.png"))
  images_used = [imageio.imread(f) for f in files]
  images = []


  imageio.mimsave("output.gif", images, duration=0.5)
elif video_picture == "video":
  clip = VideoFileClip("input.mp4")
  clip = clip.subclip(0, 5)   # first 5 seconds
  clip.write_gif("output.gif")
