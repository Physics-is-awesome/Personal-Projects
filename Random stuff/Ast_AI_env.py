import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions.bernoulli import Bernoulli
import pygame
import math
import random
import json
import os
import argparse

class Game:
    def __init__(self):
        pygame.init()
        self.WIDTH, self.HEIGHT = 800, 600
        self.screen = pygame.display.set_mode((self.WIDTH, self.HEIGHT))
        pygame.display.set_caption("Asteroids")
        self.clock = pygame.time.Clock()
        self.WHITE = (255, 255, 255)
        self.BLACK = (0, 0, 0)
        self.RED = (255, 0, 0)
        self.ORANGE = (255, 165, 0)
        self.GREEN = (0, 255, 0)
        self.BLUE = (0, 0, 255)
        self.keys = set()
        self.ship = {
            "x": self.WIDTH / 2,
            "y": self.HEIGHT / 2,
            "angle": 0,
            "dx": 0,
            "dy": 0,
            "radius": 15,
            "thrusting": False,
            "mass": 10
        }
        self.bullets = []
        self.asteroids = []
        self.enemy_bullets = []
        self.particles = []
        self.ufo = None
        self.dark_matter_clouds = []
        self.black_holes = []
        self.SHIP_SPEED = 0.2
        self.ROTATION_SPEED = 5
        self.BULLET_SPEED = 7
        self.ASTEROID_SPEED = 2
        self.ASTEROID_SIZES = [40, 20, 10]
        self.FRICTION = 0.99
        self.UFO_BULLET_SPEED = 5
        self.UFO_SHOOT_INTERVAL = 120
        self.GRAVITY_CONSTANT = 0.6
        self.MIN_DISTANCE = 10
        self.BREAKUP_SPEED = 1
        self.SPEED_OF_LIGHT = 20
        self.GAME_MODES = {
            "classic": {
                "name": "Classic Mode",
                "initial_asteroids": 4,
                "asteroids_per_wave": lambda level: 4 + level - 1,
                "ufo_spawn_min": 600,
                "ufo_spawn_max": 1200,
                "lives": 3,
                "score_multiplier": 1,
                "shot_limit": 3,
                "shot_cooldown": 30,
                "time_limit": None,
                "newtonian_gravity": False,
                "dark_matter": False,
                "relativistic": False
            },
            "survival": {
                "name": "Survival Mode",
                "initial_asteroids": 6,
                "asteroids_per_wave": lambda level: 6,
                "ufo_spawn_min": 300,
                "ufo_spawn_max": 600,
                "lives": 1,
                "score_multiplier": 2,
                "shot_limit": None,
                "shot_cooldown": 0,
                "time_limit": None,
                "newtonian_gravity": False,
                "dark_matter": False,
                "relativistic": False
            },
            "time_attack": {
                "name": "Time Attack Mode",
                "initial_asteroids": 8,
                "asteroids_per_wave": lambda level: 8,
                "ufo_spawn_min": 300,
                "ufo_spawn_max": 600,
                "lives": float('inf'),
                "score_multiplier": 1,
                "shot_limit": 5,
                "shot_cooldown": 18,
                "time_limit": 3600,
                "newtonian_gravity": False,
                "dark_matter": False,
                "relativistic": False
            },
            "newtonian_gravity": {
                "name": "Newtonian Gravity Mode",
                "initial_asteroids": 5,
                "asteroids_per_wave": lambda level: 5 + level - 1,
                "ufo_spawn_min": 600,
                "ufo_spawn_max": 1200,
                "lives": 3,
                "score_multiplier": 1,
                "shot_limit": 3,
                "shot_cooldown": 30,
                "time_limit": None,
                "newtonian_gravity": True,
                "dark_matter": False,
                "relativistic": False
            },
            "dark_matter": {
                "name": "Dark Matter Mode",
                "initial_asteroids": 6,
                "asteroids_per_wave": lambda level: 6 + level - 1,
                "ufo_spawn_min": 720,
                "ufo_spawn_max": 1080,
                "lives": 2,
                "score_multiplier": 1.5,
                "shot_limit": 3,
                "shot_cooldown": 30,
                "time_limit": None,
                "newtonian_gravity": True,
                "dark_matter": True,
                "relativistic": False
            },
            "relativistic": {
                "name": "Relativistic Mode",
                "initial_asteroids": 5,
                "asteroids_per_wave": lambda level: 5 + level - 1,
                "ufo_spawn_min": 600,
                "ufo_spawn_max": 1200,
                "lives": 3,
                "score_multiplier": 1.5,
                "shot_limit": 3,
                "shot_cooldown": 30,
                "time_limit": None,
                "newtonian_gravity": False,
                "dark_matter": False,
                "relativistic": True
            }
        }
        self.current_mode = "relativistic"  # Default to relativistic for AI
        self.score = 0
        self.lives = self.GAME_MODES[self.current_mode]["lives"]
        self.level = 1
        self.high_score = 0
        self.ufo_spawn_timer = random.randint(self.GAME_MODES[self.current_mode]["ufo_spawn_min"], self.GAME_MODES[self.current_mode]["ufo_spawn_max"])
        self.game_state = "playing"  # Start in playing state for AI
        self.shot_count = 0
        self.shoot_cooldown = 0
        self.shot_reset_timer = 60
        self.timer = self.GAME_MODES[self.current_mode]["time_limit"]
        self.font = pygame.font.SysFont("arial", 24)
        self.space_pressed = False
        try:
            pygame.mixer.init()
            self.shoot_sound = pygame.mixer.Sound("shoot.wav")
            self.explosion_sound = pygame.mixer.Sound("explosion.wav")
            self.ufo_hum_small = pygame.mixer.Sound("ufo_hum_small.wav")
            self.ufo_hum_large = pygame.mixer.Sound("ufo_hum_large.wav")
        except:
            print("Sound files missing; running without audio")
            self.shoot_sound = self.explosion_sound = self.ufo_hum_small = self.ufo_hum_large = None
        try:
            with open("highscore.json", "r") as f:
                self.high_score = json.load(f).get("high_score", 0)
        except:
            self.high_score = 0
        # Initialize asteroids
        for _ in range(self.GAME_MODES[self.current_mode]["initial_asteroids"]):
            self.spawn_asteroid(self.ASTEROID_SIZES[0])
        self.spawn_dark_matter_clouds()
        if self.GAME_MODES[self.current_mode].get("relativistic", False):
            self.spawn_black_holes()

    def save_high_score(self):
        if self.score > self.high_score:
            self.high_score = self.score
            with open("highscore.json", "w") as f:
                json.dump({"high_score": self.high_score}, f)

    def reset_game(self):
        self.ship["x"], self.ship["y"] = self.WIDTH / 2, self.HEIGHT / 2
        self.ship["dx"], self.ship["dy"] = 0, 0
        self.ship["angle"] = 0
        self.asteroids.clear()
        self.bullets.clear()
        self.enemy_bullets.clear()
        self.particles.clear()
        self.ufo = None
        self.black_holes.clear()
        self.score = 0
        self.lives = self.GAME_MODES[self.current_mode]["lives"]
        self.level = 1
        self.ufo_spawn_timer = random.randint(self.GAME_MODES[self.current_mode]["ufo_spawn_min"], self.GAME_MODES[self.current_mode]["ufo_spawn_max"])
        self.shot_count = 0
        self.shoot_cooldown = 0
        self.shot_reset_timer = 60
        self.timer = self.GAME_MODES[self.current_mode]["time_limit"]
        for _ in range(self.GAME_MODES[self.current_mode]["initial_asteroids"]):
            self.spawn_asteroid(self.ASTEROID_SIZES[0])
        self.spawn_dark_matter_clouds()
        if self.GAME_MODES[self.current_mode].get("relativistic", False):
            self.spawn_black_holes()

    def spawn_asteroid(self, size, x=None, y=None):
        num_vertices = random.randint(6, 12)
        asteroid = {
            "x": x or random.randint(0, self.WIDTH),
            "y": y or random.randint(0, self.HEIGHT),
            "dx": (random.random() - 0.5) * self.ASTEROID_SPEED * (1 + (self.level - 1) * 0.1),
            "dy": (random.random() - 0.5) * self.ASTEROID_SPEED * (1 + (self.level - 1) * 0.1),
            "radius": size,
            "vertices": num_vertices,
            "offsets": [random.uniform(0.8, 1.2) for _ in range(num_vertices)],
            "mass": 20 if size == 40 else 10 if size == 20 else 5
        }
        if math.hypot(asteroid["x"] - self.ship["x"], asteroid["y"] - self.ship["y"]) < asteroid["radius"] + self.ship["radius"] + 50:
            asteroid["x"] = random.randint(0, self.WIDTH)
            asteroid["y"] = random.randint(0, self.HEIGHT)
        for bh in self.black_holes:
            if math.hypot(asteroid["x"] - bh["x"], asteroid["y"] - bh["y"]) < bh["accretion_radius"] + asteroid["radius"]:
                asteroid["x"] = random.randint(0, self.WIDTH)
                asteroid["y"] = random.randint(0, self.HEIGHT)
        self.asteroids.append(asteroid)

    def spawn_ufo(self):
        ufo_type = random.choice(["small", "large"])
        radius = 15 if ufo_type == "small" else 25
        speed = 3 if ufo_type == "small" else 1.5
        points = 2000 if ufo_type == "small" else 1000
        shoot_interval = 60 if ufo_type == "small" else 120
        side = random.choice([-1, 1])
        self.ufo = {
            "x": 0 if side == 1 else self.WIDTH,
            "y": random.randint(100, self.HEIGHT - 100),
            "dx": side * speed,
            "dy": 0,
            "radius": radius,
            "shoot_timer": shoot_interval,
            "type": ufo_type,
            "points": points,
            "change_direction_timer": random.randint(30, 90),
            "mass": 8 if ufo_type == "small" else 15
        }
        for bh in self.black_holes:
            if math.hypot(self.ufo["x"] - bh["x"], self.ufo["y"] - bh["y"]) < bh["accretion_radius"] + self.ufo["radius"]:
                self.ufo["x"] = 0 if side == 1 else self.WIDTH
                self.ufo["y"] = random.randint(100, self.HEIGHT - 100)
        if self.ufo["type"] == "small" and self.ufo_hum_small:
            self.ufo_hum_small.play(-1)
        elif self.ufo_hum_large:
            self.ufo_hum_large.play(-1)

    def spawn_dark_matter_clouds(self):
        self.dark_matter_clouds.clear()
        if not self.GAME_MODES[self.current_mode].get("dark_matter", False):
            return
        for _ in range(random.randint(3, 5)):
            cloud = {
                "x": random.randint(100, self.WIDTH - 100),
                "y": random.randint(100, self.HEIGHT - 100),
                "mass": 50,
                "radius": 100,
                "dx": (random.random() - 0.5) * 3,
                "dy": (random.random() - 0.5) * 3
            }
            if math.hypot(cloud["x"] - self.ship["x"], cloud["y"] - self.ship["y"]) < cloud["radius"] + self.ship["radius"] + 50:
                cloud["x"] = random.randint(100, self.WIDTH - 100)
                cloud["y"] = random.randint(100, self.HEIGHT - 100)
            for bh in self.black_holes:
                if math.hypot(cloud["x"] - bh["x"], cloud["y"] - bh["y"]) < bh["accretion_radius"] + cloud["radius"]:
                    cloud["x"] = random.randint(100, self.WIDTH - 100)
                    cloud["y"] = random.randint(100, self.HEIGHT - 100)
            self.dark_matter_clouds.append(cloud)

    def spawn_black_holes(self):
        self.black_holes.clear()
        num_black_holes = random.randint(1, 2)
        for _ in range(num_black_holes):
            bh = {
                "x": random.randint(100, self.WIDTH - 100),
                "y": random.randint(100, self.HEIGHT - 100),
                "mass": 100,
                "radius": 10,
                "accretion_radius": 50,
                "dx": (random.random() - 0.5) * 0.5,
                "dy": (random.random() - 0.5) * 0.5
            }
            if math.hypot(bh["x"] - self.ship["x"], bh["y"] - self.ship["y"]) < bh["accretion_radius"] + self.ship["radius"] + 100:
                bh["x"] = random.randint(100, self.WIDTH - 100)
                bh["y"] = random.randint(100, self.HEIGHT - 100)
            self.black_holes.append(bh)

    def apply_gravity(self):
        if not (self.GAME_MODES[self.current_mode]["newtonian_gravity"] or self.GAME_MODES[self.current_mode].get("dark_matter", False) or self.GAME_MODES[self.current_mode].get("relativistic", False)):
            return
        massive_objects = [self.ship] + self.asteroids + ([self.ufo] if self.ufo is not None else []) + (self.dark_matter_clouds if self.GAME_MODES[self.current_mode].get("dark_matter", False) else []) + self.black_holes
        accelerations = [{"ax": 0, "ay": 0} for _ in massive_objects]
        for i, obj1 in enumerate(massive_objects):
            for j, obj2 in enumerate(massive_objects):
                if i == j:
                    continue
                dx = obj2["x"] - obj1["x"]
                dy = obj2["y"] - obj1["y"]
                if dx > self.WIDTH / 2:
                    dx -= self.WIDTH
                elif dx < -self.WIDTH / 2:
                    dx += self.WIDTH
                if dy > self.HEIGHT / 2:
                    dy -= self.HEIGHT
                elif dy < -self.HEIGHT / 2:
                    dy += self.HEIGHT
                r = math.hypot(dx, dy)
                if r < self.MIN_DISTANCE:
                    r = self.MIN_DISTANCE
                if r > 0:
                    force = self.GRAVITY_CONSTANT * obj1["mass"] * obj2["mass"] / (r * r)
                    if "radius" in obj2 and obj2 in self.black_holes:
                        force *= 5
                    accelerations[i]["ax"] += force * dx / (r * obj1["mass"])
                    accelerations[i]["ay"] += force * dy / (r * obj1["mass"])
        for obj, acc in zip(massive_objects, accelerations):
            obj["dx"] += acc["ax"]
            obj["dy"] += acc["ay"]

    def apply_relativistic_effects(self, obj, update=True):
        if not self.GAME_MODES[self.current_mode].get("relativistic", False):
            return True
        speed = math.hypot(obj["dx"], obj["dy"])
        if speed > 0.5 * self.SPEED_OF_LIGHT:
            scale = 0.5 * self.SPEED_OF_LIGHT / speed
            obj["dx"] *= scale
            obj["dy"] *= scale
            speed = 0.5 * self.SPEED_OF_LIGHT
        gamma = 1 / math.sqrt(1 - (speed**2 / self.SPEED_OF_LIGHT**2)) if speed < self.SPEED_OF_LIGHT else 10
        if update and random.random() > 1 / gamma:
            return False
        obj["contraction"] = max(0.8, 1 / gamma) if obj is self.ship else 1 / gamma
        obj["motion_angle"] = math.atan2(obj["dy"], obj["dx"]) if speed > 0 else 0
        rel_vx = obj["dx"] - self.ship["dx"]
        rel_vy = obj["dy"] - self.ship["dy"]
        rel_speed = math.hypot(rel_vx, rel_vy)
        if rel_speed > 0:
            cos_theta = (rel_vx * (self.ship["x"] - obj["x"]) + rel_vy * (self.ship["y"] - obj["y"])) / (rel_speed * math.hypot(self.ship["x"] - obj["x"], self.ship["y"] - obj["y"]))
            doppler_factor = rel_speed / self.SPEED_OF_LIGHT * cos_theta
            obj["doppler"] = doppler_factor
            obj["brightness"] = min(255, 255 * (1 + gamma / 2)) if cos_theta > 0 else 255
        else:
            obj["doppler"] = 0
            obj["brightness"] = 255
        return True

    def shoot_bullet(self):
        bullet_angle = math.radians(self.ship["angle"])
        ship_speed = math.hypot(self.ship["dx"], self.ship["dy"])
        if ship_speed > 0 and self.GAME_MODES[self.current_mode].get("relativistic", False):
            ship_vx = self.ship["dx"] / ship_speed if ship_speed > 0 else 0
            ship_vy = self.ship["dy"] / ship_speed if ship_speed > 0 else 0
            aberration = ship_speed / self.SPEED_OF_LIGHT
            cos_theta = math.cos(bullet_angle) * ship_vx + math.sin(bullet_angle) * ship_vy
            bullet_angle += math.asin(aberration * cos_theta)
        self.bullets.append({
            "x": self.ship["x"] + math.cos(bullet_angle) * self.ship["radius"],
            "y": self.ship["y"] - math.sin(bullet_angle) * self.ship["radius"],
            "dx": math.cos(bullet_angle) * self.BULLET_SPEED + self.ship["dx"],
            "dy": -math.sin(bullet_angle) * self.BULLET_SPEED + self.ship["dy"],
            "life": 60
        })
        if self.shoot_sound:
            self.shoot_sound.play()
        self.shot_count += 1
        self.shot_reset_timer = 60
        if self.GAME_MODES[self.current_mode]["shot_limit"] and self.shot_count >= self.GAME_MODES[self.current_mode]["shot_limit"]:
            self.shoot_cooldown = self.GAME_MODES[self.current_mode]["shot_cooldown"]

    def update(self):
        print(f"Processing keys: {self.keys}")  # Debug AI actions
        self.apply_gravity()
        self.asteroid_destroyed = False

        for asteroid in self.asteroids[:]:

            for bullet in self.bullets[:]:
                if math.hypot(bullet["x"] - asteroid["x"], bullet["y"] - asteroid["y"]) < asteroid["radius"]:
                    self.bullets.remove(bullet)
                    self.asteroid_destroyed = True

        if self.apply_relativistic_effects(self.ship):
            if pygame.K_LEFT in self.keys:
                self.ship["angle"] += self.ROTATION_SPEED
            if pygame.K_RIGHT in self.keys:
                self.ship["angle"] -= self.ROTATION_SPEED
            self.ship["thrusting"] = pygame.K_UP in self.keys
            if self.ship["thrusting"]:
                self.ship["dx"] += math.cos(math.radians(self.ship["angle"])) * self.SHIP_SPEED
                self.ship["dy"] -= math.sin(math.radians(self.ship["angle"])) * self.SHIP_SPEED
                for _ in range(2):
                    angle = math.radians(self.ship["angle"] + 180 + random.uniform(-20, 20))
                    self.particles.append({
                        "x": self.ship["x"] + math.cos(math.radians(self.ship["angle"] + 180)) * self.ship["radius"],
                        "y": self.ship["y"] - math.sin(math.radians(self.ship["angle"] + 180)) * self.ship["radius"],
                        "dx": math.cos(angle) * 3 + self.ship["dx"],
                        "dy": -math.sin(angle) * 3 + self.ship["dy"],
                        "life": 15
                    })
            if pygame.K_SPACE in self.keys and not self.space_pressed and self.shoot_cooldown <= 0:
                if self.GAME_MODES[self.current_mode]["shot_limit"] is None or self.shot_count < self.GAME_MODES[self.current_mode]["shot_limit"]:
                    self.shoot_bullet()
                    self.space_pressed = True
        if pygame.K_SPACE not in self.keys:
            self.space_pressed = False
        self.ship["x"] += self.ship["dx"]
        self.ship["y"] += self.ship["dy"]
        self.ship["dx"] *= self.FRICTION
        self.ship["dy"] *= self.FRICTION
        self.ship["x"] %= self.WIDTH
        self.ship["y"] %= self.HEIGHT
        for bh in self.black_holes[:]:
            if math.hypot(self.ship["x"] - bh["x"], self.ship["y"] - bh["y"]) < bh["radius"] + self.ship["radius"]:
                self.lives -= 1
                self.ship["x"], self.ship["y"] = self.WIDTH / 2, self.HEIGHT / 2
                self.ship["dx"], self.ship["dy"] = 0, 0
                if self.lives <= 0 and self.current_mode != "time_attack":
                    self.game_state = "game_over"
        for bullet in self.bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["x"] < 0 or bullet["x"] > self.WIDTH or bullet["y"] < 0 or bullet["y"] > self.HEIGHT or bullet["life"] <= 0:
                self.bullets.remove(bullet)
        for bullet in self.enemy_bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["x"] < 0 or bullet["x"] > self.WIDTH or bullet["y"] < 0 or bullet["y"] > self.HEIGHT or bullet["life"] <= 0:
                self.enemy_bullets.remove(bullet)
            elif math.hypot(bullet["x"] - self.ship["x"], bullet["y"] - self.ship["y"]) < self.ship["radius"]:
                self.enemy_bullets.remove(bullet)
                self.lives -= 1
                self.ship["x"], self.ship["y"] = self.WIDTH / 2, self.HEIGHT / 2
                self.ship["dx"], self.ship["dy"] = 0, 0
                if self.lives <= 0 and self.current_mode != "time_attack":
                    self.game_state = "game_over"
        for asteroid in self.asteroids[:]:
            if self.apply_relativistic_effects(asteroid):
                asteroid["x"] += asteroid["dx"]
                asteroid["y"] += asteroid["dy"]
                asteroid["x"] %= self.WIDTH
                asteroid["y"] %= self.HEIGHT
                for bullet in self.bullets[:]:
                    if math.hypot(bullet["x"] - asteroid["x"], bullet["y"] - asteroid["y"]) < asteroid["radius"]:
                        self.bullets.remove(bullet)
                        self.score += 100 * (self.ASTEROID_SIZES.index(asteroid["radius"]) + 1) * self.GAME_MODES[self.current_mode]["score_multiplier"]
                        if self.explosion_sound:
                            self.explosion_sound.play()
                        for _ in range(10):
                            self.particles.append({
                                "x": asteroid["x"],
                                "y": asteroid["y"],
                                "dx": (random.random() - 0.5) * 5,
                                "dy": (random.random() - 0.5) * 5,
                                "life": 30
                            })
                        if asteroid["radius"] > self.ASTEROID_SIZES[2]:
                            new_size = self.ASTEROID_SIZES[self.ASTEROID_SIZES.index(asteroid["radius"]) + 1]
                            angle = random.random() * 2 * math.pi
                            dx1 = math.cos(angle) * self.BREAKUP_SPEED
                            dy1 = math.sin(angle) * self.BREAKUP_SPEED
                            dx2 = -dx1
                            dy2 = -dy1
                            self.spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                            self.asteroids[-1]["dx"] += dx1
                            self.asteroids[-1]["dy"] += dy1
                            self.spawn_asteroid(new_size, asteroid["x"], asteroid["y"])
                            self.asteroids[-1]["dx"] += dx2
                            self.asteroids[-1]["dy"] += dy2
                        self.asteroids.remove(asteroid)
                        break
                if math.hypot(self.ship["x"] - asteroid["x"], self.ship["y"] - asteroid["y"]) < self.ship["radius"] + asteroid["radius"]:
                    self.lives -= 1
                    self.ship["x"], self.ship["y"] = self.WIDTH / 2, self.HEIGHT / 2
                    self.ship["dx"], self.ship["dy"] = 0, 0
                    if self.lives <= 0 and self.current_mode != "time_attack":
                        self.game_state = "game_over"
            else:
                asteroid["x"] %= self.WIDTH
                asteroid["y"] %= self.HEIGHT
            for bh in self.black_holes:
                if math.hypot(asteroid["x"] - bh["x"], asteroid["y"] - bh["y"]) < bh["radius"] + asteroid["radius"]:
                    self.asteroids.remove(asteroid)
                    break
        if self.ufo is not None:
            if self.apply_relativistic_effects(self.ufo):
                self.ufo["x"] += self.ufo["dx"]
                self.ufo["y"] += self.ufo["dy"]
                self.ufo["change_direction_timer"] -= 1
                if self.ufo["change_direction_timer"] <= 0:
                    self.ufo["dy"] = (random.random() - 0.5) * 2
                    self.ufo["change_direction_timer"] = random.randint(30, 90)
                self.ufo["shoot_timer"] -= 1
                if self.ufo["shoot_timer"] <= 0:
                    angle = math.atan2(self.ship["y"] - self.ufo["y"], self.ship["x"] - self.ufo["x"])
                    if self.ufo["type"] == "small":
                        angle += random.uniform(-0.2, 0.2)
                    self.enemy_bullets.append({
                        "x": self.ufo["x"],
                        "y": self.ufo["y"],
                        "dx": math.cos(angle) * self.UFO_BULLET_SPEED,
                        "dy": math.sin(angle) * self.UFO_BULLET_SPEED,
                        "life": 60
                    })
                    self.ufo["shoot_timer"] = self.UFO_SHOOT_INTERVAL
                for bullet in self.bullets[:]:
                    if math.hypot(bullet["x"] - self.ufo["x"], bullet["y"] - self.ufo["y"]) < self.ufo["radius"]:
                        self.bullets.remove(bullet)
                        self.score += self.ufo["points"] * self.GAME_MODES[self.current_mode]["score_multiplier"]
                        if self.explosion_sound:
                            self.explosion_sound.play()
                        for _ in range(15):
                            self.particles.append({
                                "x": self.ufo["x"],
                                "y": self.ufo["y"],
                                "dx": (random.random() - 0.5) * 5,
                                "dy": (random.random() - 0.5) * 5,
                                "life": 30
                            })
                        self.ufo = None
                        if self.ufo_hum_small:
                            self.ufo_hum_small.stop()
                        if self.ufo_hum_large:
                            self.ufo_hum_large.stop()
                        break
                if self.ufo is not None and math.hypot(self.ship["x"] - self.ufo["x"], self.ship["y"] - self.ufo["y"]) < self.ship["radius"] + self.ufo["radius"]:
                    self.lives -= 1
                    self.ship["x"], self.ship["y"] = self.WIDTH / 2, self.HEIGHT / 2
                    self.ship["dx"], self.ship["dy"] = 0, 0
                    self.ufo = None
                    if self.ufo_hum_small:
                        self.ufo_hum_small.stop()
                    if self.ufo_hum_large:
                        self.ufo_hum_large.stop()
                    if self.lives <= 0 and self.current_mode != "time_attack":
                        self.game_state = "game_over"
            if self.ufo is not None:
                for bh in self.black_holes:
                    if math.hypot(self.ufo["x"] - bh["x"], self.ufo["y"] - bh["y"]) < bh["radius"] + self.ufo["radius"]:
                        self.ufo = None
                        if self.ufo_hum_small:
                            self.ufo_hum_small.stop()
                        if self.ufo_hum_large:
                            self.ufo_hum_large.stop()
                        break
                if self.ufo is not None and (self.ufo["x"] < -self.ufo["radius"] or self.ufo["x"] > self.WIDTH + self.ufo["radius"]):
                    self.ufo = None
                    if self.ufo_hum_small:
                        self.ufo_hum_small.stop()
                    if self.ufo_hum_large:
                        self.ufo_hum_large.stop()
        self.ufo_spawn_timer -= 1
        if self.ufo_spawn_timer <= 0 and self.ufo is None:
            self.spawn_ufo()
            self.ufo_spawn_timer = random.randint(self.GAME_MODES[self.current_mode]["ufo_spawn_min"], self.GAME_MODES[self.current_mode]["ufo_spawn_max"])
        for cloud in self.dark_matter_clouds:
            cloud["x"] += cloud["dx"]
            cloud["y"] += cloud["dy"]
            cloud["x"] %= self.WIDTH
            cloud["y"] %= self.HEIGHT
        for bh in self.black_holes:
            bh["x"] += bh["dx"]
            bh["y"] += bh["dy"]
            bh["x"] %= self.WIDTH
            bh["y"] %= self.HEIGHT
        if len(self.asteroids) == 0:
            self.level += 1
            asteroid_count = self.GAME_MODES[self.current_mode]["asteroids_per_wave"](self.level)
            for _ in range(asteroid_count):
                self.spawn_asteroid(self.ASTEROID_SIZES[0])
            if self.GAME_MODES[self.current_mode].get("relativistic", False):
                self.spawn_black_holes()
        if self.shoot_cooldown > 0:
            self.shoot_cooldown -= 1
        if self.shot_reset_timer > 0:
            self.shot_reset_timer -= 1
        else:
            self.shot_count = 0
        if self.timer is not None:
            self.timer -= 1
            if self.timer <= 0:
                self.game_state = "game_over"
        for particle in self.particles[:]:
            particle["x"] += particle["dx"]
            particle["y"] += particle["dy"]
            particle["life"] -= 1
            if particle["life"] <= 0:
                self.particles.remove(particle)

    def draw_objects(self):
        self.screen.fill(self.BLACK)
        if self.GAME_MODES[self.current_mode].get("relativistic", False):
            for bh in self.black_holes:
                surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
                pygame.draw.circle(surface, (255, 165, 0, 50), (int(bh["x"]), int(bh["y"])), bh["accretion_radius"])
                self.screen.blit(surface, (0, 0))
                pygame.draw.circle(self.screen, self.BLACK, (int(bh["x"]), int(bh["y"])), bh["radius"])
        self.apply_relativistic_effects(self.ship, update=False)
        contraction = self.ship.get("contraction", 1)
        motion_angle = self.ship.get("motion_angle", 0)
        doppler = self.ship.get("doppler", 0)
        brightness = self.ship.get("brightness", 255)
        color = (
            min(255, int(255 * (1 - doppler))),
            min(255, int(255 * (1 - abs(doppler)))),
            min(255, int(255 * (1 + doppler)))
        ) if self.GAME_MODES[self.current_mode].get("relativistic", False) else self.WHITE
        ship_points = [
            (
                self.ship["x"] + (math.cos(math.radians(self.ship["angle"])) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(self.ship["angle"])) * self.ship["radius"] * math.sin(motion_angle)**2),
                self.ship["y"] - (math.sin(math.radians(self.ship["angle"])) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(self.ship["angle"])) * self.ship["radius"] * math.sin(motion_angle)**2)
            ),
            (
                self.ship["x"] + (math.cos(math.radians(self.ship["angle"] + 140)) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(self.ship["angle"] + 140)) * self.ship["radius"] * math.sin(motion_angle)**2),
                self.ship["y"] - (math.sin(math.radians(self.ship["angle"] + 140)) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(self.ship["angle"] + 140)) * self.ship["radius"] * math.sin(motion_angle)**2)
            ),
            (
                self.ship["x"] + (math.cos(math.radians(self.ship["angle"] - 140)) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 + math.sin(math.radians(self.ship["angle"] - 140)) * self.ship["radius"] * math.sin(motion_angle)**2),
                self.ship["y"] - (math.sin(math.radians(self.ship["angle"] - 140)) * self.ship["radius"] * contraction * math.cos(motion_angle)**2 - math.cos(math.radians(self.ship["angle"] - 140)) * self.ship["radius"] * math.sin(motion_angle)**2)
            )
        ]
        pygame.draw.polygon(self.screen, color, ship_points, 1)
        nose_x = self.ship["x"] + math.cos(math.radians(self.ship["angle"])) * self.ship["radius"]
        nose_y = self.ship["y"] - math.sin(math.radians(self.ship["angle"])) * self.ship["radius"]
        surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
        pygame.draw.circle(surface, (255, 0, 0, 100), (int(nose_x), int(nose_y)), 5)
        pygame.draw.circle(surface, self.RED, (int(nose_x), int(nose_y)), 3)
        self.screen.blit(surface, (0, 0))
        for bullet in self.bullets:
            pygame.draw.circle(self.screen, self.RED, (int(bullet["x"]), int(bullet["y"])), 2)
        for bullet in self.enemy_bullets:
            pygame.draw.circle(self.screen, self.GREEN, (int(bullet["x"]), int(bullet["y"])), 2)
        for asteroid in self.asteroids:
            self.apply_relativistic_effects(asteroid, update=False)
            contraction = asteroid.get("contraction", 1)
            motion_angle = asteroid.get("motion_angle", 0)
            doppler = asteroid.get("doppler", 0)
            brightness = asteroid.get("brightness", 255)
            ship_speed = math.hypot(self.ship["dx"], self.ship["dy"])
            dx = asteroid["x"] - self.ship["x"]
            dy = asteroid["y"] - self.ship["y"]
            if dx > self.WIDTH / 2:
                dx -= self.WIDTH
            elif dx < -self.WIDTH / 2:
                dx += self.WIDTH
            if dy > self.HEIGHT / 2:
                dy -= self.HEIGHT
            elif dy < -self.HEIGHT / 2:
                dy += self.HEIGHT
            r = math.hypot(dx, dy)
            if r > 0 and ship_speed > 0 and self.GAME_MODES[self.current_mode].get("relativistic", False):
                theta = math.atan2(dy, dx)
                ship_vx = self.ship["dx"] / ship_speed if ship_speed > 0 else 0
                ship_vy = self.ship["dy"] / ship_speed if ship_speed > 0 else 0
                aberration = ship_speed / self.SPEED_OF_LIGHT
                cos_theta = math.cos(theta) * ship_vx + math.sin(theta) * ship_vy
                theta_prime = theta + math.asin(aberration * cos_theta)
                asteroid_x = self.ship["x"] + r * math.cos(theta_prime)
                asteroid_y = self.ship["y"] + r * math.sin(theta_prime)
            else:
                asteroid_x = asteroid["x"]
                asteroid_y = asteroid["y"]
            color = (
                min(255, int(brightness * (1 - doppler))),
                min(255, int(brightness * (1 - abs(doppler)))),
                min(255, int(brightness * (1 + doppler)))
            ) if self.GAME_MODES[self.current_mode].get("relativistic", False) else self.WHITE
            points = []
            for i in range(asteroid["vertices"]):
                angle = i * 2 * math.pi / asteroid["vertices"]
                radius = asteroid["radius"] * asteroid["offsets"][i]
                x = asteroid_x + (math.cos(angle) * radius * contraction * math.cos(motion_angle)**2 + math.sin(angle) * radius * math.sin(motion_angle)**2)
                y = asteroid_y + (math.sin(angle) * radius * contraction * math.cos(motion_angle)**2 - math.cos(angle) * radius * math.sin(motion_angle)**2)
                points.append((x, y))
            pygame.draw.polygon(self.screen, color, points, 1)
            if self.GAME_MODES[self.current_mode].get("relativistic", False) and math.hypot(asteroid_x - self.ship["x"], asteroid_y - self.ship["y"]) < 100 and math.hypot(asteroid["dx"], asteroid["dy"]) > 0.3 * self.SPEED_OF_LIGHT:
                surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
                pygame.draw.circle(surface, (255, 255, 255, 20), (int(asteroid_x), int(asteroid_y)), asteroid["radius"] + 5)
                self.screen.blit(surface, (0, 0))
        if self.ufo is not None:
            self.apply_relativistic_effects(self.ufo, update=False)
            contraction = self.ufo.get("contraction", 1)
            motion_angle = self.ufo.get("motion_angle", 0)
            doppler = self.ufo.get("doppler", 0)
            brightness = self.ufo.get("brightness", 255)
            dx = self.ufo["x"] - self.ship["x"]
            dy = self.ufo["y"] - self.ship["y"]
            if dx > self.WIDTH / 2:
                dx -= self.WIDTH
            elif dx < -self.WIDTH / 2:
                dx += self.WIDTH
            if dy > self.HEIGHT / 2:
                dy -= self.HEIGHT
            elif dy < -self.HEIGHT / 2:
                dy += self.HEIGHT
            r = math.hypot(dx, dy)
            if r > 0 and ship_speed > 0 and self.GAME_MODES[self.current_mode].get("relativistic", False):
                theta = math.atan2(dy, dx)
                theta_prime = theta + math.asin(aberration * cos_theta)
                ufo_x = self.ship["x"] + r * math.cos(theta_prime)
                ufo_y = self.ship["y"] + r * math.sin(theta_prime)
            else:
                ufo_x = self.ufo["x"]
                ufo_y = self.ufo["y"]
            color = (
                min(255, int(brightness * (1 - doppler))),
                min(255, int(brightness * (1 - abs(doppler)))),
                min(255, int(brightness * (1 + doppler)))
            ) if self.GAME_MODES[self.current_mode].get("relativistic", False) else self.WHITE
            scale = 1.5 if self.ufo["type"] == "large" else 1
            ufo_points = [
                (
                    ufo_x + (-self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + self.ufo["radius"] * scale * math.sin(motion_angle)**2),
                    ufo_y + (self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + self.ufo["radius"] * scale * math.sin(motion_angle)**2)
                ),
                (
                    ufo_x + (self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + self.ufo["radius"] * scale * math.sin(motion_angle)**2),
                    ufo_y + (self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - self.ufo["radius"] * scale * math.sin(motion_angle)**2)
                ),
                (
                    ufo_x + (self.ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                    ufo_y + (-self.ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
                ),
                (
                    ufo_x + (self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - self.ufo["radius"] * scale * math.sin(motion_angle)**2),
                    ufo_y + (-self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - self.ufo["radius"] * scale * math.sin(motion_angle)**2)
                ),
                (
                    ufo_x + (-self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 - self.ufo["radius"] * scale * math.sin(motion_angle)**2),
                    ufo_y + (-self.ufo["radius"] * scale * contraction * math.cos(motion_angle)**2 + self.ufo["radius"] * scale * math.sin(motion_angle)**2)
                ),
                (
                    ufo_x + (-self.ufo["radius"] * 1.5 * scale * contraction * math.cos(motion_angle)**2),
                    ufo_y + (self.ufo["radius"] * 1.5 * scale * math.sin(motion_angle)**2)
                )
            ]
            pygame.draw.polygon(self.screen, color, ufo_points, 1)
            if self.GAME_MODES[self.current_mode].get("relativistic", False) and math.hypot(ufo_x - self.ship["x"], ufo_y - self.ship["y"]) < 100 and math.hypot(self.ufo["dx"], self.ufo["dy"]) > 0.3 * self.SPEED_OF_LIGHT:
                surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
                pygame.draw.circle(surface, (255, 255, 255, 20), (int(ufo_x), int(ufo_y)), self.ufo["radius"] + 5)
                self.screen.blit(surface, (0, 0))
        if self.GAME_MODES[self.current_mode].get("relativistic", False):
            for obj in [self.ship] + self.asteroids + ([self.ufo] if self.ufo is not None else []):
                speed = math.hypot(obj["dx"], obj["dy"])
                if speed > 0.3 * self.SPEED_OF_LIGHT:
                    alpha = int(150 * (speed / (0.5 * self.SPEED_OF_LIGHT))) if obj is self.ship else int(100 * (speed / (0.5 * self.SPEED_OF_LIGHT)))
                    trail_length = 10 if obj is self.ship else 5
                    color = (0, 0, 255, min(255, alpha)) if obj is self.ship else (255, 255, 255, min(255, alpha))
                    surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
                    pygame.draw.line(surface, color,
                                    (obj["x"], obj["y"]),
                                    (obj["x"] - obj["dx"] * trail_length, obj["y"] - obj["dy"] * trail_length), 2)
                    self.screen.blit(surface, (0, 0))
        for particle in self.particles:
            pygame.draw.circle(self.screen, self.WHITE, (int(particle["x"]), int(particle["y"])), 2)
        if self.GAME_MODES[self.current_mode].get("dark_matter", False):
            for cloud in self.dark_matter_clouds:
                visible = any(math.hypot(particle["x"] - cloud["x"], particle["y"] - cloud["y"]) < cloud["radius"] for particle in self.particles)
                if visible:
                    surface = pygame.Surface((self.WIDTH, self.HEIGHT), pygame.SRCALPHA)
                    pygame.draw.circle(surface, (255, 255, 255, 50), (int(cloud["x"]), int(cloud["y"])), cloud["radius"])
                    self.screen.blit(surface, (0, 0))
        score_text = self.font.render(f"Score: {self.score}", True, self.WHITE)
        lives_text = self.font.render(f"Lives: {int(self.lives) if self.lives != float('inf') else '-'}", True, self.WHITE)
        level_text = self.font.render(f"Level: {self.level}", True, self.WHITE)
        mode_text = self.font.render(self.GAME_MODES[self.current_mode]["name"], True, self.WHITE)
        timer_text = self.font.render(f"Time: {int(self.timer / 60) if self.timer is not None else '-'}", True, self.WHITE)
        shots_text = self.font.render(f"Shots: {self.GAME_MODES[self.current_mode]['shot_limit'] - self.shot_count if self.GAME_MODES[self.current_mode]['shot_limit'] else '-'}", True, self.WHITE)
        self.screen.blit(score_text, (10, 10))
        self.screen.blit(lives_text, (10, 40))
        self.screen.blit(level_text, (10, 70))
        self.screen.blit(mode_text, (10, 130))
        self.screen.blit(timer_text, (10, 160))
        self.screen.blit(shots_text, (10, 190))
        if self.shoot_cooldown > 0:
            cooldown_text = self.font.render("Cool-down!", True, self.RED)
            self.screen.blit(cooldown_text, (10, 220))

class ActorCritic(nn.Module):
    def __init__(self, input_size=21, hidden_size=64, output_size=4):
        super(ActorCritic, self).__init__()
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.shared = nn.Sequential(
            nn.Linear(input_size, hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, hidden_size),
            nn.ReLU()
        )
        self.actor = nn.Linear(hidden_size, output_size)
        self.critic = nn.Linear(hidden_size, 1)
        self.to(self.device)
    
    def forward(self, x):
        x = torch.FloatTensor(x).to(self.device)
        shared = self.shared(x)
        action_logits = self.actor(shared)
        value = self.critic(shared)
        return action_logits, value

class PPOAgent:
    def __init__(self, input_size=21, hidden_size=64, output_size=4, lr=3e-4, clip_eps=0.2, gae_lambda=0.95, gamma=0.99):
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.model = ActorCritic(input_size, hidden_size, output_size).to(self.device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lr)
        self.clip_eps = clip_eps
        self.gae_lambda = gae_lambda
        self.gamma = gamma
        self.input_size = input_size
    
    def get_game_state(self, game):
        state = [
            game.ship["x"] / game.WIDTH,
            game.ship["y"] / game.HEIGHT,
            game.ship["dx"] / game.SPEED_OF_LIGHT,
            game.ship["dy"] / game.SPEED_OF_LIGHT,
            game.ship["angle"] / 360
        ]
        if game.asteroids:
            distances = [np.hypot(a["x"] - game.ship["x"], a["y"] - game.ship["y"]) for a in game.asteroids]
            nearest = game.asteroids[np.argmin(distances)]
            dx = nearest["x"] - game.ship["x"]
            dy = nearest["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                nearest["dx"] / game.SPEED_OF_LIGHT,
                nearest["dy"] / game.SPEED_OF_LIGHT,
                angle,
                game.ASTEROID_SIZES.index(nearest["radius"]) / 2 if nearest["radius"] in game.ASTEROID_SIZES else 0
            ])
        else:
            state.extend([0] * 5)
        if game.ufo:
            dx = game.ufo["x"] - game.ship["x"]
            dy = game.ufo["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                game.ufo["dx"] / game.SPEED_OF_LIGHT,
                game.ufo["dy"] / game.SPEED_OF_LIGHT,
                angle,
                0 if game.ufo["type"] == "small" else 1
            ])
        else:
            state.extend([0] * 5)
        if game.black_holes:
            distances = [np.hypot(bh["x"] - game.ship["x"], bh["y"] - game.ship["y"]) for bh in game.black_holes]
            nearest = game.black_holes[np.argmin(distances)]
            dx = nearest["x"] - game.ship["x"]
            dy = nearest["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                nearest["dx"] / game.SPEED_OF_LIGHT,
                nearest["dy"] / game.SPEED_OF_LIGHT,
                angle
            ])
        else:
            state.extend([0] * 4)
        state.extend([len(game.bullets) / 10, len(game.enemy_bullets) / 10])
        assert len(state) == 21, f"Expected 21 features, got {len(state)}"
        return state

    def collect_rollouts(self, game, episodes=10, max_steps=3600, headless=True):
        rollouts = []
        print(f"Starting collect_rollouts: episodes={episodes}, max_steps={max_steps}, headless={headless}")
        for ep in range(episodes):
            game.game_state = "playing"
            game.current_mode = "relativistic"
            print(f"Episode {ep+1}: Resetting game")
            game.reset_game()
            states, actions, log_probs, rewards, values, dones = [], [], [], [], [], []
            steps = 0
            prev_lives = game.lives
            prev_score = 0
            prev_ufo = None
            while game.game_state == "playing" and steps < max_steps:
                pygame.event.clear()
                pygame.event.pump()
                
                try:
                    state = self.get_game_state(game)
                except Exception as e:
                    print(f"Error in get_game_state: {e}")
                    break
                
                state_tensor = torch.FloatTensor(state).to(self.device)
                with torch.no_grad():
                    action_logits, value = self.model(state_tensor)
                probs = torch.sigmoid(action_logits)
                dist = Bernoulli(probs)
                action = dist.sample()
                log_prob = dist.log_prob(action).sum()
                
                game.keys = set()
                if action[0] > 0.5:
                    game.keys.add(pygame.K_LEFT)
                if action[1] > 0.5:
                    game.keys.add(pygame.K_RIGHT)
                if action[2] > 0.5:
                    game.keys.add(pygame.K_UP)
                if action[3] > 0.5 and game.shoot_cooldown <= 0:
                    game.keys.add(pygame.K_SPACE)
                print(f"Step {steps}: AI actions: {game.keys}, Ship: x={game.ship['x']:.2f}, y={game.ship['y']:.2f}, angle={game.ship['angle']:.2f}, Bullets={len(game.bullets)}, Score={game.score}")
                
                try:
                    game.update()
                except Exception as e:
                    print(f"Error in game.update: {e}")
                    break
                
                reward = 1
                if game.asteroid_destroyed:
                    reward += 800

                if game.lives < prev_lives:
                    reward -= 1000
                if any(np.hypot(game.ship["x"] - bh["x"], game.ship["y"] - bh["y"]) < bh["radius"] + game.ship["radius"] for bh in game.black_holes):
                    reward -= 5000
                reward += (game.score - prev_score)
                if game.ufo is None and prev_ufo is not None:
                    reward += 8000
                prev_lives = game.lives
                prev_score = game.score
                prev_ufo = game.ufo
                
                states.append(state)
                actions.append(action.cpu().numpy())
                log_probs.append(log_prob.cpu().item())
                rewards.append(reward)
                values.append(value.cpu().item())
                dones.append(game.game_state != "playing")
                
                steps += 1
                if not headless and game.screen is not None:
                    print(f"Rendering: Step {steps}")
                    game.draw_objects()
                    pygame.display.flip()
                    game.clock.tick(60)
                
                if game.lives <= 0 and game.current_mode != "time_attack":
                    game.game_state = "game_over"
            
            if steps < max_steps and game.game_state == "playing":
                with torch.no_grad():
                    _, value = self.model(torch.FloatTensor(self.get_game_state(game)).to(self.device))
                values.append(value.cpu().item())
            else:
                values.append(0)
            
            rollouts.append((states, actions, log_probs, rewards, values, dones))
            print(f"Episode {ep+1}: Score={game.score}, Steps={steps}, Reward={sum(rewards):.2f}")
        
        return rollouts

    def compute_gae(self, rewards, values, dones):
        advantages = []
        returns = []
        gae = 0
        for i in reversed(range(len(rewards))):
            delta = rewards[i] + self.gamma * values[i + 1] * (1 - dones[i]) - values[i]
            gae = delta + self.gamma * self.gae_lambda * (1 - dones[i]) * gae
            advantages.insert(0, gae)
            returns.insert(0, gae + values[i])
        return advantages, returns

    def update(self, rollouts, epochs=10, batch_size=64):
        states = []
        actions = []
        old_log_probs = []
        advantages = []
        returns = []
        for rollout in rollouts:
            rollout_states, rollout_actions, rollout_log_probs, rollout_rewards, rollout_values, rollout_dones = rollout
            rollout_advantages, rollout_returns = self.compute_gae(rollout_rewards, rollout_values, rollout_dones)
            states.extend(rollout_states)
            actions.extend(rollout_actions)
            old_log_probs.extend(rollout_log_probs)
            advantages.extend(rollout_advantages)
            returns.extend(rollout_returns)
        
        states = torch.FloatTensor(states).to(self.device)
        actions = torch.FloatTensor(actions).to(self.device)
        old_log_probs = torch.FloatTensor(old_log_probs).to(self.device)
        advantages = torch.FloatTensor(advantages).to(self.device)
        advantages = (advantages - advantages.mean()) / (advantages.std() + 1e-8)
        returns = torch.FloatTensor(returns).to(self.device)
        
        for _ in range(epochs):
            for i in range(0, len(states), batch_size):
                batch_indices = slice(i, i + batch_size)
                batch_states = states[batch_indices]
                batch_actions = actions[batch_indices]
                batch_old_log_probs = old_log_probs[batch_indices]
                batch_advantages = advantages[batch_indices]
                batch_returns = returns[batch_indices]
                
                action_logits, value = self.model(batch_states)
                dist = Bernoulli(torch.sigmoid(action_logits))
                new_log_probs = dist.log_prob(batch_actions).sum(dim=1)
                entropy = dist.entropy().mean()
                
                ratio = torch.exp(new_log_probs - batch_old_log_probs)
                surr1 = ratio * batch_advantages
                surr2 = torch.clamp(ratio, 1 - self.clip_eps, 1 + self.clip_eps) * batch_advantages
                actor_loss = -torch.min(surr1, surr2).mean()
                critic_loss = nn.MSELoss()(value.squeeze(), batch_returns)
                loss = actor_loss + 0.5 * critic_loss - 0.05 * entropy
                
                self.optimizer.zero_grad()
                loss.backward()
                self.optimizer.step()
        print("Completed PPO update")

    def save_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        torch.save(self.model.state_dict(), path)
        print(f"Saved model to {path}")

    def load_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        if os.path.exists(path):
            self.model.load_state_dict(torch.load(path))
            print(f"Loaded model from {path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="PPO for Asteroids AI")
    parser.add_argument("--test", action="store_true", help="Run test mode with trained model")
    parser.add_argument("--manual", action="store_true", help="Run in manual play mode")
    args = parser.parse_args()
    
    try:
        pygame.init()
        game = Game()
        
        if args.manual:
            print("Running in manual mode...")
            game.game_state = "playing"
            game.current_mode = "relativistic"
            game.reset_game()
            while game.game_state == "playing":
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        game.game_state = "game_over"
                    elif event.type == pygame.KEYDOWN:
                        game.keys.add(event.key)
                    elif event.type == pygame.KEYUP:
                        game.keys.discard(event.key)
                game.update()
                if game.screen is not None:
                    game.draw_objects()
                    pygame.display.flip()
                    game.clock.tick(60)
        else:
            agent = PPOAgent()
            agent.load_model()
            
            if args.test:
                print("Testing trained agent...")
                rollouts = agent.collect_rollouts(game, episodes=1, headless=False, max_steps=7200)
                print("Completed test run")
            else:
                iterations = 100000
                for i in range(iterations):
                    print(f"Iteration {i+1}/{iterations}")
                    headless = i % 100000 != 0
                    rollouts = agent.collect_rollouts(game, episodes=5, headless=headless, max_steps=3600)
                    agent.update(rollouts)
                    total_rewards = [sum(rollout[3]) for rollout in rollouts]
                    print(f"Average Reward: {sum(total_rewards)/len(total_rewards):.2f}, Max: {max(total_rewards):.2f}")
                    agent.save_model()
                print("Testing with best model...")
                agent.collect_rollouts(game, episodes=1, headless=False, max_steps=7200)
    except Exception as e:
        print(f"Error occurred: {e}")
        import traceback
        traceback.print_exc()
    finally:
        pygame.quit()
