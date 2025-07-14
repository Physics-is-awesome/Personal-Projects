import pygame
import math
import random
import numpy as np

# Initialize Pygame
pygame.init()
pygame.font.init()
pygame.mixer.init()

# Constants
WIDTH = 800
HEIGHT = 600
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
RED = (255, 0, 0)
GREEN = (0, 255, 0)
ASTEROID_SIZES = [30, 20, 10]
ASTEROID_SPEED = 2
BREAKUP_SPEED = 1
SHIP_SPEED = 0.2
FRICTION = 0.99
ROTATION_SPEED = 5
BULLET_SPEED = 10
UFO_BULLET_SPEED = 5
UFO_SHOOT_INTERVAL = 60
SPEED_OF_LIGHT = 10
G = 1000

# Game objects
ship = {"x": WIDTH / 2, "y": HEIGHT / 2, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
asteroids = []
bullets = []
enemy_bullets = []
black_holes = []
dark_matter_clouds = []
particles = []
ufo = None
ufo_hum_small = None
ufo_hum_large = None
score = 0
lives = 3
level = 1
game_state = "start"
current_mode = "classic"
shot_count = 0
shoot_cooldown = 0
shot_reset_timer = 180
ufo_spawn_timer = random.randint(600, 1200)
timer = None
last_outputs = None  # For PPO visualization
last_reward = 0  # For PPO visualization

# Pygame setup
screen = pygame.display.set_mode((WIDTH, HEIGHT))
clock = pygame.time.Clock()
font = pygame.font.SysFont("monospace", 20)

# Sound (comment out if files missing)
try:
    shoot_sound = pygame.mixer.Sound("shoot.wav")
    explosion_sound = pygame.mixer.Sound("explosion.wav")
    ufo_hum_small = pygame.mixer.Sound("ufo_small.wav")
    ufo_hum_large = pygame.mixer.Sound("ufo_large.wav")
except:
    shoot_sound = explosion_sound = ufo_hum_small = ufo_hum_large = None

# Game modes
GAME_MODES = {
    "classic": {
        "name": "Classic Mode",
        "initial_asteroids": 4,
        "asteroids_per_wave": lambda level: 4 + level - 1,
        "ufo_spawn_min": 600,
        "ufo_spawn_max": 1200,
        "lives": 3,
        "score_multiplier": 1,
        "shot_limit": None,
        "shot_cooldown": None,
        "time_limit": None,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": False
    },
    "relativistic": {
        "name": "Relativistic Mode",
        "initial_asteroids": 4,
        "asteroids_per_wave": lambda level: 4 + level - 1,
        "ufo_spawn_min": 900,
        "ufo_spawn_max": 1500,
        "lives": 3,
        "score_multiplier": 1,
        "shot_limit": 3,
        "shot_cooldown": 30,
        "time_limit": None,
        "newtonian_gravity": False,
        "dark_matter": False,
        "relativistic": True
    }
}

def spawn_asteroid(size, x=None, y=None):
    asteroid = {
        "x": random.randint(0, WIDTH) if x is None else x,
        "y": random.randint(0, HEIGHT) if y is None else y,
        "dx": (random.random() - 0.5) * ASTEROID_SPEED,
        "dy": (random.random() - 0.5) * ASTEROID_SPEED,
        "radius": size,
        "vertices": random.randint(8, 12),
        "offsets": [random.uniform(0.8, 1.2) for _ in range(8, 12)],
        "mass": 10 * (size / ASTEROID_SIZES[2]) ** 2
    }
    if x is None and y is None:
        while math.hypot(asteroid["x"] - ship["x"], asteroid["y"] - ship["y"]) < 100:
            asteroid["x"] = random.randint(0, WIDTH)
            asteroid["y"] = random.randint(0, HEIGHT)
    asteroids.append(asteroid)

def spawn_ufo():
    global ufo, ufo_hum_small, ufo_hum_large
    side = random.choice([1, -1])
    ufo_type = random.choice(["small", "large"])
    ufo = {
        "x": 0 if side == 1 else WIDTH,
        "y": random.randint(100, HEIGHT - 100),
        "dx": side * (2 if ufo_type == "small" else 1),
        "dy": (random.random() - 0.5) * 2,
        "radius": 10 if ufo_type == "small" else 15,
        "type": ufo_type,
        "points": 1000 if ufo_type == "small" else 2000,
        "shoot_timer": UFO_SHOOT_INTERVAL,
        "change_direction_timer": random.randint(30, 90)
    }
    if ufo_type == "small" and ufo_hum_small:
        ufo_hum_small.play(-1)
    elif ufo_type == "large" and ufo_hum_large:
        ufo_hum_large.play(-1)

def spawn_black_holes():
    global black_holes
    black_holes.clear()
    num_black_holes = random.randint(1, 2)
    for _ in range(num_black_holes):
        bh = {
            "x": random.randint(100, WIDTH - 100),
            "y": random.randint(100, HEIGHT - 100),
            "dx": (random.random() - 0.5) * 0.5,
            "dy": (random.random() - 0.5) * 0.5,
            "radius": 10,
            "accretion_radius": 50,
            "mass": 1000
        }
        while math.hypot(bh["x"] - ship["x"], bh["y"] - ship["y"]) < 100:
            bh["x"] = random.randint(100, WIDTH - 100)
            bh["y"] = random.randint(100, HEIGHT - 100)
        black_holes.append(bh)

def apply_gravity():
    for obj in [ship] + asteroids + ([ufo] if ufo else []) + bullets + enemy_bullets:
        for obj2 in asteroids + black_holes:
            if obj is not obj2:
                dx = obj2["x"] - obj["x"]
                dy = obj2["y"] - obj["y"]
                if dx > WIDTH / 2:
                    dx -= WIDTH
                elif dx < -WIDTH / 2:
                    dx += WIDTH
                if dy > HEIGHT / 2:
                    dy -= HEIGHT
                elif dy < -HEIGHT / 2:
                    dy += HEIGHT
                r = math.hypot(dx, dy)
                if r > 0:
                    force = G * obj.get("mass", 1) * obj2.get("mass", 1) / r ** 2
                    if "radius" in obj2 and obj2 in black_holes:
                        force *= 5
                    if GAME_MODES[current_mode].get("newtonian_gravity", False) or obj2 in black_holes:
                        obj["dx"] += force * dx / r / obj.get("mass", 1)
                        obj["dy"] += force * dy / r / obj.get("mass", 1)

def apply_relativistic_effects(obj, update=True):
    if not GAME_MODES[current_mode].get("relativistic", False):
        return True
    speed = math.hypot(obj["dx"], obj["dy"])
    gamma = 1 / math.sqrt(1 - (speed / SPEED_OF_LIGHT) ** 2) if speed < SPEED_OF_LIGHT else float('inf')
    if update and random.random() > 1 / gamma:
        return False
    obj["contraction"] = max(0.8, 1 / gamma) if obj is ship else 1 / gamma
    motion_angle = math.atan2(obj["dy"], obj["dx"]) if speed > 0 else 0
    obj["motion_angle"] = motion_angle
    doppler = speed / SPEED_OF_LIGHT
    obj["doppler"] = doppler
    obj["brightness"] = min(255, 255 * (1 / gamma))
    return True

def reset_game():
    global ship, asteroids, bullets, enemy_bullets, black_holes, dark_matter_clouds, particles
    global score, lives, level, shot_count, shoot_cooldown, shot_reset_timer, ufo, ufo_spawn_timer, timer
    ship = {"x": WIDTH / 2, "y": HEIGHT / 2, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
    asteroids.clear()
    bullets.clear()
    enemy_bullets.clear()
    black_holes.clear()
    dark_matter_clouds.clear()
    particles.clear()
    ufo = None
    if ufo_hum_small:
        ufo_hum_small.stop()
    if ufo_hum_large:
        ufo_hum_large.stop()
    score = 0
    lives = GAME_MODES[current_mode]["lives"]
    level = 1
    shot_count = 0
    shoot_cooldown = 0
    shot_reset_timer = 180
    ufo_spawn_timer = random.randint(GAME_MODES[current_mode]["ufo_spawn_min"], GAME_MODES[current_mode]["ufo_spawn_max"])
    timer = GAME_MODES[current_mode]["time_limit"]
    asteroid_count = GAME_MODES[current_mode]["initial_asteroids"]
    for _ in range(asteroid_count):
        spawn_asteroid(ASTEROID_SIZES[0])
    if GAME_MODES[current_mode].get("relativistic", False):
        spawn_black_holes()

def update_bullets():
    global bullets, enemy_bullets, lives, game_state, ship
    for bullet in bullets[:]:
        bullet["x"] += bullet["dx"]
        bullet["y"] += bullet["dy"]
        bullet["life"] -= 1
        if bullet["x"] < 0 or bullet["x"] > WIDTH or bullet["y"] < 0 or bullet["y"] > HEIGHT or bullet["life"] <= 0:
            bullets.remove(bullet)
    for bullet in enemy_bullets[:]:
        bullet["x"] += bullet["dx"]
        bullet["y"] += bullet["dy"]
        bullet["life"] -= 1
        if bullet["x"] < 0 or bullet["x"] > WIDTH or bullet["y"] < 0 or bullet["y"] > HEIGHT or bullet["life"] <= 0:
            enemy_bullets.remove(bullet)
        elif math.hypot(bullet["x"] - ship["x"], bullet["y"] - ship["y"]) < ship["radius"]:
            enemy_bullets.remove(bullet)
            lives -= 1
            ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
            ship["dx"], ship["dy"] = 0, 0
            if lives <= 0 and current_mode != "time_attack":
                game_state = "game_over"

def update_asteroids():
    global asteroids, bullets, score, lives, ship, game_state, particles
    for asteroid in asteroids[:]:
        if apply_relativistic_effects(asteroid):
            asteroid["x"] += asteroid["dx"]
            asteroid["y"] += asteroid["dy"]
            asteroid["x"] %= WIDTH
            asteroid["y"] %= HEIGHT
            for bullet in bullets[:]:
                if math.hypot(bullet["x"] - asteroid["x"], bullet["y"] - asteroid["y"]) < asteroid["radius"]:
                    bullets.remove(bullet)
                    score += 100 * (ASTEROID_SIZES.index(asteroid["radius"]) + 1) * GAME_MODES[current_mode]["score_multiplier"]
                    if explosion_sound:
                        explosion_sound.play()
                    for _ in range(10):
                        particles.append({
                            "x": asteroid["x"],
                            "y": asteroid["y"],
                            "dx": (random.random() - 0.5) * 5,
                            "dy": (random.random() - 0.5) * 5,
                            "life": 30
                        })
                    if asteroid["radius"] > ASTEROID_SIZES[2]:
                        new_size = ASTEROID_SIZES[ASTEROID_SIZES.index(asteroid["radius"]) + 1]
                        angle = random.random() * 2 * math.pi
                        dx1 = math.cos(angle) * BREAKUP_SPEED
                        dy1 = math.sin(angle) * BREAKUP_SPEED
                        dx2 = -dx1
                        dy2 = -dy1
                        spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                        asteroids[-1]["dx"] += dx1
                        asteroids[-1]["dy"] += dy1
                        spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                        asteroids[-1]["dx"] += dx2
                        asteroids[-1]["dy"] += dy2
                    asteroids.remove(asteroid)
                    break
            if math.hypot(ship["x"] - asteroid["x"], ship["y"] - asteroid["y"]) < ship["radius"] + asteroid["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                if lives <= 0 and current_mode != "time_attack":
                    game_state = "game_over"
        else:
            asteroid["x"] %= WIDTH
            asteroid["y"] %= HEIGHT
        for bh in black_holes:
            if math.hypot(asteroid["x"] - bh["x"], asteroid["y"] - bh["y"]) < bh["radius"] + asteroid["radius"]:
                asteroids.remove(asteroid)
                break

def update_ufo():
    global ufo, bullets, score, lives, ship, game_state, particles
    if ufo is not None:
        if apply_relativistic_effects(ufo):
            ufo["x"] += ufo["dx"]
            ufo["y"] += ufo["dy"]
            ufo["change_direction_timer"] -= 1
            if ufo["change_direction_timer"] <= 0:
                ufo["dy"] = (random.random() - 0.5) * 2
                ufo["change_direction_timer"] = random.randint(30, 90)
            ufo["shoot_timer"] -= 1
            if ufo["shoot_timer"] <= 0:
                angle = math.atan2(ship["y"] - ufo["y"], ship["x"] - ufo["x"])
                if ufo["type"] == "small":
                    angle += random.uniform(-0.2, 0.2)
                enemy_bullets.append({
                    "x": ufo["x"],
                    "y": ufo["y"],
                    "dx": math.cos(angle) * UFO_BULLET_SPEED,
                    "dy": math.sin(angle) * UFO_BULLET_SPEED,
                    "life": 60
                })
                ufo["shoot_timer"] = UFO_SHOOT_INTERVAL
            for bullet in bullets[:]:
                if math.hypot(bullet["x"] - ufo["x"], bullet["y"] - ufo["y"]) < ufo["radius"]:
                    bullets.remove(bullet)
                    score += ufo["points"] * GAME_MODES[current_mode]["score_multiplier"]
                    if explosion_sound:
                        explosion_sound.play()
                    for _ in range(15):
                        particles.append({
                            "x": ufo["x"],
                            "y": ufo["y"],
                            "dx": (random.random() - 0.5) * 5,
                            "dy": (random.random() - 0.5) * 5,
                            "life": 30
                        })
                    ufo = None
                    if ufo_hum_small:
                        ufo_hum_small.stop()
                    if ufo_hum_large:
                        ufo_hum_large.stop()
                    break
            if ufo is not None and math.hypot(ship["x"] - ufo["x"], ship["y"] - ufo["y"]) < ship["radius"] + ufo["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                ufo = None
                if ufo_hum_small:
                    ufo_hum_small.stop()
                if ufo_hum_large:
                    ufo_hum_large.stop()
                if lives <= 0 and current_mode != "time_attack":
                    game_state = "game_over"
        if ufo is not None:
            for bh in black_holes:
                if math.hypot(ufo["x"] - bh["x"], ufo["y"] - bh["y"]) < bh["radius"] + ufo["radius"]:
                    ufo = None
                    if ufo_hum_small:
                        ufo_hum_small.stop()
                    if ufo_hum_large:
                        ufo_hum_large.stop()
                    break
        if ufo is not None and (ufo["x"] < -ufo["radius"] or ufo["x"] > WIDTH + ufo["radius"]) and not GAME_MODES[current_mode].get("arena", False):
            ufo = None
            if ufo_hum_small:
                ufo_hum_small.stop()
            if ufo_hum_large:
                ufo_hum_large.stop()

def draw_objects():
    screen.fill(BLACK)
    if GAME_MODES[current_mode].get("relativistic", False):
        for bh in black_holes:
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 165, 0, 50), (int(bh["x"]), int(bh["y"])), bh["accretion_radius"])
            screen.blit(surface, (0, 0))
            pygame.draw.circle(screen, BLACK, (int(bh["x"]), int(bh["y"])), bh["radius"])
    
    apply_relativistic_effects(ship, update=False)
    contraction = ship.get("contraction", 1)
    motion_angle = ship.get("motion_angle", 0)
    doppler = ship.get("doppler", 0)
    brightness = ship.get("brightness", 255)
    color = (
        min(255, int(255 * (1 - doppler))),
        min(255, int(255 * (1 - abs(doppler)))),
        min(255, int(255 * (1 + doppler)))
    ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
    ship_points = [
        (
            ship["x"] + (math.cos(math.radians(ship["angle"])) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"])) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"])) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"])) * ship["radius"] * math.sin(motion_angle)**2)
        ),
        (
            ship["x"] + (math.cos(math.radians(ship["angle"] + 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"] + 140)) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"] + 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"] + 140)) * ship["radius"] * math.sin(motion_angle)**2)
        ),
        (
            ship["x"] + (math.cos(math.radians(ship["angle"] - 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(ship["angle"] - 140)) * ship["radius"] * math.sin(motion_angle)**2),
            ship["y"] - (math.sin(math.radians(ship["angle"] - 140)) * ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(ship["angle"] - 140)) * ship["radius"] * math.sin(motion_angle)**2)
        )
    ]
    pygame.draw.polygon(screen, color, ship_points, 1)
    nose_x = ship["x"] + math.cos(math.radians(ship["angle"])) * ship["radius"]
    nose_y = ship["y"] - math.sin(math.radians(ship["angle"])) * ship["radius"]
    surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
    pygame.draw.circle(surface, (255, 0, 0, 100), (int(nose_x), int(nose_y)), 5)
    pygame.draw.circle(surface, RED, (int(nose_x), int(nose_y)), 3)
    screen.blit(surface, (0, 0))
    
    for bullet in bullets:
        pygame.draw.circle(screen, RED, (int(bullet["x"]), int(bullet["y"])), 2)
    
    for bullet in enemy_bullets:
        pygame.draw.circle(screen, GREEN, (int(bullet["x"]), int(bullet["y"])), 2)
    
    for asteroid in asteroids:
        apply_relativistic_effects(asteroid, update=False)
        contraction = asteroid.get("contraction", 1)
        motion_angle = asteroid.get("motion_angle", 0)
        doppler = asteroid.get("doppler", 0)
        brightness = asteroid.get("brightness", 255)
        ship_speed = math.hypot(ship["dx"], ship["dy"])
        dx = asteroid["x"] - ship["x"]
        dy = asteroid["y"] - ship["y"]
        if dx > WIDTH / 2:
            dx -= WIDTH
        elif dx < -WIDTH / 2:
            dx += WIDTH
        if dy > HEIGHT / 2:
            dy -= HEIGHT
        elif dy < -HEIGHT / 2:
            dy += HEIGHT
        r = math.hypot(dx, dy)
        if r > 0 and ship_speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
            theta = math.atan2(dy, dx)
            ship_vx = ship["dx"] / ship_speed if ship_speed > 0 else 0
            ship_vy = ship["dy"] / ship_speed if ship_speed > 0 else 0
            aberration = ship_speed / SPEED_OF_LIGHT
            cos_theta = math.cos(theta) * ship_vx + math.sin(theta) * ship_vy
            theta_prime = theta + math.asin(aberration * cos_theta)
            asteroid_x = ship["x"] + r * math.cos(theta_prime)
            asteroid_y = ship["y"] + r * math.sin(theta_prime)
        else:
            asteroid_x = asteroid["x"]
            asteroid_y = asteroid["y"]
        color = (
            min(255, int(brightness * (1 - doppler))),
            min(255, int(brightness * (1 - abs(doppler)))),
            min(255, int(brightness * (1 + doppler)))
        ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
        points = []
        for i in range(asteroid["vertices"]):
            angle = i * 2 * math.pi / asteroid["vertices"]
            radius = asteroid["radius"] * asteroid["offsets"][i]
            x = asteroid_x + (math.cos(angle) * radius * contraction * math.cos(motion_angle)**2 + math.sin(angle) * radius * math.sin(motion_angle)**2)
            y = asteroid_y + (math.sin(angle) * radius * contraction * math.cos(motion_angle)**2 - math.cos(angle) * radius * math.sin(motion_angle)**2)
            points.append((x, y))
        pygame.draw.polygon(screen, color, points, 1)
        if GAME_MODES[current_mode].get("relativistic", False) and math.hypot(asteroid_x - ship["x"], asteroid_y - ship["y"]) < 100 and math.hypot(asteroid["dx"], asteroid["dy"]) > 0.3 * SPEED_OF_LIGHT:
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 255, 255, 20), (int(asteroid_x), int(asteroid_y)), asteroid["radius"] + 5)
            screen.blit(surface, (0, 0))
    
    if ufo is not None:
        apply_relativistic_effects(ufo, update=False)
        contraction = ufo.get("contraction", 1)
        motion_angle = ufo.get("motion_angle", 0)
        doppler = ufo.get("doppler", 0)
        brightness = ufo.get("brightness", 255)
        dx = ufo["x"] - ship["x"]
        dy = ufo["y"] - ship["y"]
        if dx > WIDTH / 2:
            dx -= WIDTH
        elif dx < -WIDTH / 2:
            dx += WIDTH
        if dy > HEIGHT / 2:
            dy -= HEIGHT
        elif dy < -HEIGHT / 2:
            dy += HEIGHT
        r = math.hypot(dx, dy)
        if r > 0 and ship_speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
            theta = math.atan2(dy, dx)
            theta_prime = theta + math.asin(aberration * cos_theta)
            ufo_x = ship["x"] + r * math.cos(theta_prime)
            ufo_y = ship["y"] + r * math.sin(theta_prime)
        else:
            ufo_x = ufo["x"]
            ufo_y = ufo["y"]
        color = (
            min(255, int(brightness * (1 - doppler))),
            min(255, int(brightness * (1 - abs(doppler)))),
            min(255, int(brightness * (1 + doppler)))
        ) if GAME_MODES[current_mode].get("relativistic", False) else WHITE
        scale = 1.5 if ufo["type"] == "large" else 1
        ufo_points = [
            (
                ufo_x + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                ufo_y + (-ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - ufo["radius"] * scale * math.sin(motion_angle)**2),
                ufo_y + (-ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + ufo["radius"] * scale * math.sin(motion_angle)**2)
            ),
            (
                ufo_x + (-ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                ufo_y + (ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
            )
        ]
        pygame.draw.polygon(screen, color, ufo_points, 1)
        if GAME_MODES[current_mode].get("relativistic", False) and math.hypot(ufo_x - ship["x"], ufo_y - ship["y"]) < 100 and math.hypot(ufo["dx"], ufo["dy"]) > 0.3 * SPEED_OF_LIGHT:
            surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
            pygame.draw.circle(surface, (255, 255, 255, 20), (int(ufo_x), int(ufo_y)), ufo["radius"] + 5)
            screen.blit(surface, (0, 0))
    
    if GAME_MODES[current_mode].get("relativistic", False):
        for obj in [ship] + asteroids + ([ufo] if ufo is not None else []):
            speed = math.hypot(obj["dx"], obj["dy"])
            if speed > 0.3 * SPEED_OF_LIGHT:
                alpha = int(150 * (speed / (0.5 * SPEED_OF_LIGHT))) if obj is ship else int(100 * (speed / (0.5 * SPEED_OF_LIGHT)))
                trail_length = 10 if obj is ship else 5
                color = (0, 0, 255, min(255, alpha)) if obj is ship else (255, 255, 255, min(255, alpha))
                surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
                pygame.draw.line(surface, color,
                                (obj["x"], obj["y"]),
                                (obj["x"] - obj["dx"] * trail_length, obj["y"] - obj["dy"] * trail_length), 2)
                screen.blit(surface, (0, 0))
    
    for particle in particles:
        pygame.draw.circle(screen, WHITE, (int(particle["x"]), int(particle["y"])), 2)
    
    if GAME_MODES[current_mode].get("dark_matter", False):
        for cloud in dark_matter_clouds:
            visible = any(math.hypot(particle["x"] - cloud["x"], particle["y"] - cloud["y"]) < cloud["radius"] for particle in particles)
            if visible:
                surface = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
                pygame.draw.circle(surface, (255, 255, 255, 50), (int(cloud["x"]), int(cloud["y"])), cloud["radius"])
                screen.blit(surface, (0, 0))
    
    score_text = font.render(f"Score: {score}", True, WHITE)
    lives_text = font.render(f"Lives: {int(lives) if lives != float('inf') else '-'}", True, WHITE)
    level_text = font.render(f"Level: {level}", True, WHITE)
    mode_text = font.render(GAME_MODES[current_mode]["name"], True, WHITE)
    timer_text = font.render(f"Time: {int(timer / 60) if timer is not None else '-'}", True, WHITE)
    shots_text = font.render(f"Shots: {GAME_MODES[current_mode]['shot_limit'] - shot_count if GAME_MODES[current_mode]['shot_limit'] else '-'}", True, WHITE)
    screen.blit(score_text, (10, 10))
    screen.blit(lives_text, (10, 40))
    screen.blit(level_text, (10, 70))
    screen.blit(mode_text, (10, 130))
    screen.blit(timer_text, (10, 160))
    screen.blit(shots_text, (10, 190))
    if shoot_cooldown > 0:
        cooldown_text = font.render("Cool-down!", True, RED)
        screen.blit(cooldown_text, (10, 220))
    if last_outputs is not None:
        outputs_text = font.render(f"Actions: {last_outputs}", True, WHITE)
        screen.blit(outputs_text, (10, 250))
    if last_reward != 0:
        reward_text = font.render(f"Reward: {last_reward:.2f}", True, WHITE)
        screen.blit(cooldown_text, (10, 280))

# Main game loop (for manual play)
keys = set()
running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            if game_state == "start":
                if event.key == pygame.K_1:
                    current_mode = "classic"
                    game_state = "playing"
                    reset_game()
                elif event.key == pygame.K_6:
                    current_mode = "relativistic"
                    game_state = "playing"
                    reset_game()
            elif game_state == "game_over" and event.key == pygame.K_r:
                game_state = "start"
    
    if game_state == "start":
        start_text = font.render("Press 1: Classic, 6: Relativistic", True, WHITE)
        screen.blit(start_text, (WIDTH // 2 - start_text.get_width() // 2, HEIGHT // 2))
    elif game_state == "game_over":
        game_over_text = font.render(f"Game Over! Score: {score} Press R to Restart", True, WHITE)
        screen.blit(game_over_text, (WIDTH // 2 - game_over_text.get_width() // 2, HEIGHT // 2))
    elif game_state == "playing":
        apply_gravity()
        if apply_relativistic_effects(ship):
            if pygame.K_LEFT in keys:
                ship["angle"] += ROTATION_SPEED
            if pygame.K_RIGHT in keys:
                ship["angle"] -= ROTATION_SPEED
            ship["thrusting"] = pygame.K_UP in keys
            if ship["thrusting"]:
                ship["dx"] += math.cos(math.radians(ship["angle"])) * SHIP_SPEED
                ship["dy"] -= math.sin(math.radians(ship["angle"])) * SHIP_SPEED
        ship["x"] += ship["dx"]
        ship["y"] += ship["dy"]
        ship["dx"] *= FRICTION
        ship["dy"] *= FRICTION
        ship["x"] %= WIDTH
        ship["y"] %= HEIGHT
        
        shoot_cooldown = max(0, shoot_cooldown - 1)
        shot_reset_timer = max(0, shot_reset_timer - 1)
        if shot_reset_timer <= 0:
            shot_count = 0
        if pygame.K_SPACE in keys and shoot_cooldown <= 0 and (GAME_MODES[current_mode]["shot_limit"] is None or shot_count < GAME_MODES[current_mode]["shot_limit"]):
            speed = math.hypot(ship["dx"], ship["dy"])
            if speed > 0 and GAME_MODES[current_mode].get("relativistic", False):
                theta = math.atan2(-ship["dy"], ship["dx"])
                bullet_angle = math.radians(ship["angle"])
                cos_theta = math.cos(bullet_angle) * math.cos(theta) + math.sin(bullet_angle) * math.sin(theta)
                beta = speed / SPEED_OF_LIGHT
                bullet_angle_prime = bullet_angle + math.asin(beta * cos_theta)
                bullet_dx = math.cos(bullet_angle_prime) * BULLET_SPEED
                bullet_dy = -math.sin(bullet_angle_prime) * BULLET_SPEED
            else:
                bullet_dx = math.cos(math.radians(ship["angle"])) * BULLET_SPEED
                bullet_dy = -math.sin(math.radians(ship["angle"])) * BULLET_SPEED
            bullets.append({
                "x": ship["x"],
                "y": ship["y"],
                "dx": bullet_dx + ship["dx"],
                "dy": bullet_dy + ship["dy"],
                "life": 60
            })
            if shoot_sound:
                shoot_sound.play()
            shot_count += 1
            shoot_cooldown = GAME_MODES[current_mode]["shot_cooldown"] or 0
            shot_reset_timer = 180
        
        update_bullets()
        update_asteroids()
        update_ufo()
        
        ufo_spawn_timer -= 1
        if ufo_spawn_timer <= 0 and ufo is None:
            spawn_ufo()
            ufo_spawn_timer = random.randint(GAME_MODES[current_mode]["ufo_spawn_min"], GAME_MODES[current_mode]["ufo_spawn_max"])
        
        for cloud in dark_matter_clouds:
            cloud["x"] += cloud["dx"]
            cloud["y"] += cloud["dy"]
            cloud["x"] %= WIDTH
            cloud["y"] %= HEIGHT
        
        for bh in black_holes:
            bh["x"] += bh["dx"]
            bh["y"] += bh["dy"]
            bh["x"] %= WIDTH
            bh["y"] %= HEIGHT
        
        if len(asteroids) == 0:
            level += 1
            asteroid_count = GAME_MODES[current_mode]["asteroids_per_wave"](level)
            for _ in range(asteroid_count):
                spawn_asteroid(ASTEROID_SIZES[0])
            if GAME_MODES[current_mode].get("relativistic", False):
                spawn_black_holes()
        
        for particle in particles[:]:
            particle["x"] += particle["dx"]
            particle["y"] += particle["dy"]
            particle["life"] -= 1
            if particle["life"] <= 0:
                particles.remove(particle)
        
        for bh in black_holes:
            if math.hypot(ship["x"] - bh["x"], ship["y"] - bh["y"]) < bh["radius"] + ship["radius"]:
                lives -= 1
                ship["x"], ship["y"] = WIDTH / 2, HEIGHT / 2
                ship["dx"], ship["dy"] = 0, 0
                if lives <= 0 and current_mode != "time_attack":
                    game_state = "game_over"
        
        if timer is not None:
            timer -= 1
            if timer <= 0:
                game_state = "game_over"
        
        draw_objects()
    
    pygame.display.flip()
    clock.tick(60)

pygame.quit()
