import imageio
from moviepy.editor import VideoFileClip
video_picture = input("Video of Picture ")  

if video_picture == "picture":

  images = []
  for filename in ["gif_1.png", "gif_2.png", "gif_3.png", "gif_4.png"]:
      images.append(imageio.imread(filename))

  imageio.mimsave("output.gif", images, duration=0.5)
elif video_picture == "video":
  clip = VideoFileClip("input.mp4")
  clip = clip.subclip(0, 5)   # first 5 seconds
  clip.write_gif("output.gif")
