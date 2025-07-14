import pygame
import numpy as np

class Game:
    def __init__(self):
        pygame.init()
        self.WIDTH = 800
        self.HEIGHT = 600
        self.screen = pygame.display.set_mode((self.WIDTH, self.HEIGHT))
        self.clock = pygame.time.Clock()
        self.BLACK = (0, 0, 0)
        self.keys = set()
        self.ship = {"x": 400, "y": 300, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
        self.bullets = []
        self.enemy_bullets = []
        self.asteroids = []
        self.ufo = None
        self.black_holes = []
        self.dark_matter_clouds = []
        self.particles = []
        self.lives = 3
        self.score = 0
        self.level = 1
        self.game_state = "playing"
        self.current_mode = "relativistic"
        self.SPEED_OF_LIGHT = 10
        self.SHIP_SPEED = 0.5
        self.ROTATION_SPEED = 5
        self.FRICTION = 0.99
        self.shoot_cooldown = 0
        self.shot_reset_timer = 60
        self.shot_count = 0
        self.ufo_spawn_timer = 100
        self.ASTEROID_SIZES = [30, 20, 10]
        self.GAME_MODES = {
            "relativistic": {
                "ufo_spawn_min": 50,
                "ufo_spawn_max": 200,
                "asteroids_per_wave": lambda level: 4 + level,
                "relativistic": True
            }
        }
        self.last_outputs = None
        self.last_reward = 0

    def reset_game(self):
        self.ship = {"x": 400, "y": 300, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
        self.bullets = []
        self.enemy_bullets = []
        self.asteroids = []
        self.ufo = None
        self.black_holes = []
        self.particles = []
        self.lives = 3
        self.score = 0
        self.level = 1
        self.game_state = "playing"
        self.ufo_spawn_timer = 100
        self.keys = set()

    def update_bullets(self):
        for bullet in self.bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["life"] <= 0:
                self.bullets.remove(bullet)
        for bullet in self.enemy_bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["life"] <= 0:
                self.enemy_bullets.remove(bullet)

    def shoot_bullet(self):
        self.bullets.append({
            "x": self.ship["x"],
            "y": self.ship["y"],
            "dx": np.cos(np.radians(self.ship["angle"])) * 10,
            "dy": -np.sin(np.radians(self.ship["angle"])) * 10,
            "life": 30
        })
        self.shoot_cooldown = 10
        self.shot_count += 1
        self.shot_reset_timer = 60

    def update(self):
        print(f"Processing keys: {self.keys}")  # Debug
        self.apply_gravity()
        if self.apply_relativistic_effects(self.ship):
            if pygame.K_LEFT in self.keys:
                self.ship["angle"] += self.ROTATION_SPEED
            if pygame.K_RIGHT in self.keys:
                self.ship["angle"] -= self.ROTATION_SPEED
            self.ship["thrusting"] = pygame.K_UP in self.keys
            if self.ship["thrusting"]:
                self.ship["dx"] += np.cos(np.radians(self.ship["angle"])) * self.SHIP_SPEED
                self.ship["dy"] -= np.sin(np.radians(self.ship["angle"])) * self.SHIP_SPEED
            if pygame.K_SPACE in self.keys and self.shoot_cooldown <= 0:
                self.shoot_bullet()
        self.ship["x"] += self.ship["dx"]
        self.ship["y"] += self.ship["dy"]
        self.ship["dx"] *= self.FRICTION
        self.ship["dy"] *= self.FRICTION
        self.ship["x"] %= self.WIDTH
        self.ship["y"] %= self.HEIGHT
        self.update_bullets()
        self.update_asteroids()
        self.update_ufo()
        self.ufo_spawn_timer -= 1
        if self.ufo_spawn_timer <= 0 and self.ufo is None:
            self.spawn_ufo()
            self.ufo_spawn_timer = np.random.randint(
                self.GAME_MODES[self.current_mode]["ufo_spawn_min"],
                self.GAME_MODES[self.current_mode]["ufo_spawn_max"]
            )
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
        for particle in self.particles[:]:
            particle["x"] += particle["dx"]
            particle["y"] += particle["dy"]
            particle["life"] -= 1
            if particle["life"] <= 0:
                self.particles.remove(particle)

    def apply_gravity(self):
        pass  # Implement if needed

    def apply_relativistic_effects(self, obj):
        return True  # Implement if needed

    def update_asteroids(self):
        for asteroid in self.asteroids[:]:
            asteroid["x"] += asteroid["dx"]
            asteroid["y"] += asteroid["dy"]
            asteroid["x"] %= self.WIDTH
            asteroid["y"] %= self.HEIGHT

    def update_ufo(self):
        if self.ufo:
            self.ufo["x"] += self.ufo["dx"]
            self.ufo["y"] += self.ufo["dy"]
            self.ufo["x"] %= self.WIDTH
            self.ufo["y"] %= self.HEIGHT

    def spawn_ufo(self):
        self.ufo = {"x": np.random.randint(0, self.WIDTH), "y": np.random.randint(0, self.HEIGHT),
                    "dx": np.random.uniform(-2, 2), "dy": np.random.uniform(-2, 2), "type": "small"}

    def spawn_asteroid(self, size):
        self.asteroids.append({
            "x": np.random.randint(0, self.WIDTH),
            "y": np.random.randint(0, self.HEIGHT),
            "dx": np.random.uniform(-1, 1),
            "dy": np.random.uniform(-1, 1),
            "radius": size
        })

    def spawn_black_holes(self):
        self.black_holes.append({
            "x": np.random.randint(0, self.WIDTH),
            "y": np.random.randint(0, self.HEIGHT),
            "dx": 0,
            "dy": 0,
            "radius": 50
        })

    def draw_objects(self):
        self.screen.fill(self.BLACK)
        pygame.draw.circle(self.screen, (255, 255, 255), (int(self.ship["x"]), int(self.ship["y"])), self.ship["radius"])
        for bullet in self.bullets:
            pygame.draw.circle(self.screen, (0, 255, 0), (int(bullet["x"]), int(bullet["y"])), 2)
        for bullet in self.enemy_bullets:
            pygame.draw.circle(self.screen, (255, 0, 0), (int(bullet["x"]), int(bullet["y"])), 2)
        for asteroid in self.asteroids:
            pygame.draw.circle(self.screen, (128, 128, 128), (int(asteroid["x"]), int(asteroid["y"])), asteroid["radius"])
        if self.ufo:
            pygame.draw.circle(self.screen, (0, 0, 255), (int(self.ufo["x"]), int(self.ufo["y"])), 15)
        for bh in self.black_holes:
            pygame.draw.circle(self.screen, (0, 0, 0), (int(bh["x"]), int(bh["y"])), bh["radius"])
        for particle in self.particles:
            pygame.draw.circle(self.screen, (255, 255, 0), (int(particle["x"]), int(particle["y"])), 1)
