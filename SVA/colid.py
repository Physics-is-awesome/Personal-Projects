import pygame
import random
import math
import sys

# Initialize pygame
pygame.init()

# Screen setup
WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Asteroid Approaching Earth")

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
EARTH_BLUE = (30, 144, 255)
ATMOSPHERE = (135, 206, 250)
ASTEROID_GRAY = (169, 169, 169)
EXPLOSION_COLORS = [(255, 69, 0), (255, 140, 0), (255, 215, 0)]

# Clock
clock = pygame.time.Clock()

# Earth setup
earth_pos = (WIDTH - 200, HEIGHT // 2)
earth_radius = 60

# Asteroid setup
asteroid_pos = [100, 100]
asteroid_radius = 20
velocity = [3, 2]

# Stars
stars = [(random.randint(0, WIDTH), random.randint(0, HEIGHT)) for _ in range(150)]

def draw_stars():
    for (x, y) in stars:
        pygame.draw.circle(screen, WHITE, (x, y), random.randint(1, 2))

def draw_earth():
    # Atmosphere glow
    pygame.draw.circle(screen, ATMOSPHERE, earth_pos, earth_radius + 15)
    # Earth body
    pygame.draw.circle(screen, EARTH_BLUE, earth_pos, earth_radius)

def draw_asteroid():
    pygame.draw.circle(screen, ASTEROID_GRAY, asteroid_pos, asteroid_radius)
    # Add speckles
    for _ in range(5):
        dx = random.randint(-asteroid_radius, asteroid_radius)
        dy = random.randint(-asteroid_radius, asteroid_radius)
        if dx**2 + dy**2 < asteroid_radius**2:
            pygame.draw.circle(screen, (105, 105, 105),
                               (asteroid_pos[0] + dx, asteroid_pos[1] + dy), 2)

def explode():
    for r in range(30, 150, 20):
        color = random.choice(EXPLOSION_COLORS)
        pygame.draw.circle(screen, color, earth_pos, r, 5)
    font = pygame.font.SysFont("Arial", 48, bold=True)
    text = font.render("IMPACT!", True, WHITE)
    screen.blit(text, (WIDTH//2 - 100, HEIGHT//2 - 20))

running = True
impact = False

while running:
    screen.fill(BLACK)
    draw_stars()
    draw_earth()

    if not impact:
        # Move asteroid
        asteroid_pos[0] += velocity[0]
        asteroid_pos[1] += velocity[1]
        draw_asteroid()

        # Check collision
        dist = math.hypot(asteroid_pos[0] - earth_pos[0],
                          asteroid_pos[1] - earth_pos[1])
        if dist < asteroid_radius + earth_radius:
            impact = True
    else:
        explode()

    # Event handling
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    pygame.display.flip()
    clock.tick(60)

pygame.quit()
sys.exit()


frame += 1

# Hold screen after end
turtle.done()
