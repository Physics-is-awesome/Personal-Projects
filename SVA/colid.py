import pygame
import math
import random
import sys

# ---- Setup ----
pygame.init()
WIDTH, HEIGHT = 900, 650
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Asteroid Orbit and Impact (Gravity)")

clock = pygame.time.Clock()
FONT = pygame.font.SysFont("Arial", 18)

# ---- Colors ----
BLACK = (5, 5, 15)
WHITE = (240, 240, 240)
EARTH_BLUE = (50, 140, 255)
EARTH_GREEN = (46, 204, 113)
ATMOS = (120, 180, 255, 40)
ASTEROID = (180, 180, 180)
SMOKE = (255, 140, 0)

# ---- World objects ----
earth_pos = pygame.Vector2(WIDTH * 0.68, HEIGHT * 0.55)
earth_radius = 70

# Gravitational parameter (scaled for 2D screen):
# Larger -> stronger pull (curvier orbits / faster impact)
mu = 1  # acts like G*M in screen units

# ---- Asteroid initial state ----
pos = pygame.Vector2(120, 160)
vel = pygame.Vector2(0.3, 0.3)  # try changing direction/speed
asteroid_radius = 18

# ---- Visual extras ----
stars = [(random.randint(0, WIDTH), random.randint(0, HEIGHT), random.randint(1, 2)) for _ in range(180)]
trail = []  # store previous positions
max_trail = 300
impact = False
paused = False

# ---- Helpers ----
def draw_stars():
    for x, y, s in stars:
        c = (220 + random.randint(-20, 20),) * 3
        pygame.draw.circle(screen, c, (x, y), s)

def draw_earth(surface):
    # Atmosphere glow
    atmos_surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
    pygame.draw.circle(atmos_surface, ATMOS, earth_pos, earth_radius + 18)
    surface.blit(atmos_surface, (0, 0))
    # Earth body
    pygame.draw.circle(surface, EARTH_BLUE, earth_pos, earth_radius)
    # Simple continents
    for dx, dy, r in [(10, -5, 26), (-28, 12, 18), (25, 24, 14)]:
        pygame.draw.circle(surface, EARTH_GREEN, (int(earth_pos.x + dx), int(earth_pos.y + dy)), r)

def draw_asteroid(surface, p):
    pygame.draw.circle(surface, ASTEROID, (int(p.x), int(p.y)), asteroid_radius)
    # speckles
    for _ in range(6):
        a = random.uniform(0, 2 * math.pi)
        rr = random.uniform(3, asteroid_radius - 3)
        sx = int(p.x + rr * math.cos(a))
        sy = int(p.y + rr * math.sin(a))
        pygame.draw.circle(surface, (130, 130, 130), (sx, sy), 2)

def draw_trail(surface, pts):
    if len(pts) > 3:
        for i in range(1, len(pts)):
            c = (255, 160, 80) if i % 2 == 0 else (255, 210, 120)
            pygame.draw.line(surface, c, pts[i - 1], pts[i], 2)

def explode(surface):
    # expanding rings
    for r in range(earth_radius, earth_radius + 180, 18):
        color = (255, 90 + (r % 60), 0)
        pygame.draw.circle(surface, color, earth_pos, r, 3)
    msg = pygame.font.SysFont("Arial", 42, bold=True).render("IMPACT!", True, WHITE)
    rect = msg.get_rect(center=(WIDTH // 2, HEIGHT // 8))
    surface.blit(msg, rect)

def collide(p):
    return (p - earth_pos).length() <= (earth_radius + asteroid_radius)

# ---- Main loop ----
running = True
while running:
    dt = clock.tick(60) / 1000.0  # seconds per frame
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                paused = not paused
            elif event.key == pygame.K_r:
                # Reset
                pos = pygame.Vector2(120, 160)
                vel = pygame.Vector2(2.4, 0.9)
                trail.clear()
                impact = False
                paused = False
            elif event.key == pygame.K_UP:
                vel *= 1.08
            elif event.key == pygame.K_DOWN:
                vel *= 0.92
            elif event.key == pygame.K_LEFT:
                mu *= 0.9
            elif event.key == pygame.K_RIGHT:
                mu *= 1.1

    if not paused and not impact:
        # Gravity
        r_vec = earth_pos - pos
        r = r_vec.length()
        if r != 0:
            # a = mu * r_vec / r^3
            acc = (mu / (r**3)) * r_vec
        else:
            acc = pygame.Vector2(0, 0)

        # Integrate velocity and position
        vel += acc * dt
        pos += vel * dt

        # Store trail
        trail.append((int(pos.x), int(pos.y)))
        if len(trail) > max_trail:
            trail.pop(0)

        # Impact check
        if collide(pos):
            impact = True

        # Keep within screen (bounce lightly if desired)
        if pos.x < 0 or pos.x > WIDTH or pos.y < 0 or pos.y > HEIGHT:
            # Simple soft wrap to keep it visible
            pos.x = max(10, min(WIDTH - 10, pos.x))
            pos.y = max(10, min(HEIGHT - 10, pos.y))
            vel *= 0.95

    # ---- Draw ----
    screen.fill(BLACK)
    draw_stars()
    draw_trail(screen, trail)
    draw_earth(screen)
    if not impact:
        draw_asteroid(screen, pos)
    else:
        explode(screen)

    # HUD
    hud = [
        f"Space: pause/resume | R: reset",
        f"ArrowUp/Down: speed x={vel.x:.2f} y={vel.y:.2f}",
        f"ArrowLeft/Right: gravity mu={mu:.0f}",
        f"Distance r={(earth_pos - pos).length():.1f}",
    ]
    for i, t in enumerate(hud):
        txt = FONT.render(t, True, (220, 220, 220))
        screen.blit(txt, (12, 12 + i * 22))

    pygame.display.flip()

pygame.quit()
sys.exit()
