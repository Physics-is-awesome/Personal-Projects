import turtle
import random
import math

# Screen setup
screen = turtle.Screen()
screen.title("Asteroid Approaching Earth")
screen.bgcolor("black")
screen.setup(width=800, height=600)

# Draw simple stars
star = turtle.Turtle(visible=False)
star.speed(0)
star.color("white")
for _ in range(150):
    x = random.randint(-390, 390)
    y = random.randint(-290, 290)
    star.penup()
    star.goto(x, y)
    star.dot(random.randint(1, 3))

# Earth setup
earth = turtle.Turtle()
earth.speed(0)
earth.penup()
earth.goto(200, -50)  # Earth position
earth.color("#1E90FF")  # DodgerBlue
earth.shape("circle")
earth.shapesize(stretch_wid=4, stretch_len=4)  # Bigger circle
earth_radius = 40  # matches shapesize*10 roughly

# Earth atmosphere glow (optional visual)
halo = turtle.Turtle(visible=False)
halo.speed(0)
halo.penup()
halo.goto(earth.xcor(), earth.ycor() - earth_radius)
halo.color("#00BFFF")  # DeepSkyBlue
halo.pendown()
halo.width(2)
halo.circle(earth_radius + 12)

# Asteroid setup
asteroid = turtle.Turtle()
asteroid.speed(0)
asteroid.penup()
asteroid.goto(-350, 220)  # starting point
asteroid.color("#A9A9A9")  # DarkGray
asteroid.shape("circle")
asteroid.shapesize(stretch_wid=1.4, stretch_len=1.8)  # slightly oblong
asteroid_radius = 14

# Add surface speckles to the asteroid
speck = turtle.Turtle(visible=False)
speck.speed(0)
speck.color("#808080")

def draw_speckle():
    speck.penup()
    ax, ay = asteroid.position()
    for _ in range(6):
        dx = random.randint(-12, 12)
        dy = random.randint(-10, 10)
        speck.goto(ax + dx, ay + dy)
        speck.dot(random.randint(2, 4))

draw_speckle()

# Velocity aimed roughly at Earth
vx, vy = 2.0, -1.8

# Trail
trail = turtle.Turtle(visible=False)
trail.speed(0)
trail.color("#FFA500")  # Orange
trail.width(2)

# Impact text
impact_text = turtle.Turtle(visible=False)
impact_text.color("white")
impact_text.penup()

# Simple sound/flash effect (visual only)
flash = turtle.Turtle(visible=False)
flash.speed(0)

def distance(t1, t2):
    x1, y1 = t1.position()
    x2, y2 = t2.position()
    return math.hypot(x2 - x1, y2 - y1)

def explode():
    # Flash concentric circles
    flash.penup()
    flash.goto(earth.xcor(), earth.ycor() - earth_radius)
    flash.color("#FFD700")  # Gold
    for r in range(earth_radius, earth_radius + 100, 10):
        flash.pendown()
        flash.circle(r)
        flash.penup()
    # Change Earth color briefly
    earth.color("#FF6347")  # Tomato
    impact_text.goto(0, 200)
    impact_text.write("Impact!", align="center", font=("Arial", 28, "bold"))

# Main animation loop
frame = 0
running = True
while running:
    # Move asteroid
    ax, ay = asteroid.position()
    ax += vx
    ay += vy
    asteroid.goto(ax, ay)

    # Flicker trail
    if frame % 2 == 0:
        trail.penup()
        trail.goto(ax, ay)
        trail.pendown()
        trail.goto(ax - vx * 5, ay - vy * 5)

    # Slight drift and acceleration to feel more dynamic
    vx *= 1.002
    vy *= 1.002
    vy -= 0.003  # gravity-like pull

    # Redraw speckles occasionally to follow asteroid
    if frame % 10 == 0:
        draw_speckle()

    # Check collision
    if distance(asteroid, earth) <= (asteroid_radius + earth_radius):
        explode()
        running = False

    # Off-screen safety
    if not (-410 < ax < 410 and -310 < ay < 310):
        running = False

    frame += 1

# Hold screen after end
turtle.done()
