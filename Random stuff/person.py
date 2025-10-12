"""
mario_like.py
Simple "Mario-like" platformer demo using Pygame (single-file).
Controls:
  - Left / Right arrow or A / D : move
  - Space or W or Up arrow      : jump
  - Esc                         : quit
Features:
  - Tile-based level from ASCII map
  - Platforms (solid tiles), coins, patrolling enemies
  - Basic collision, gravity, jump
  - Side-scrolling camera
  - Score and win-screen
No external assets required (everything is drawn with rectangles/colors).
"""

import pygame
import sys

pygame.init()
pygame.display.set_caption("Mini Mario-like (Pygame)")

# Config
SCREEN_WIDTH, SCREEN_HEIGHT = 960, 540
TILE_SIZE = 48
FPS = 60

# Colors
BG_COLOR = (135, 206, 235)  # sky blue
GROUND_COLOR = (100, 60, 20)
PLAYER_COLOR = (230, 50, 50)
COIN_COLOR = (255, 200, 0)
ENEMY_COLOR = (50, 50, 200)
FLAG_COLOR = (20, 160, 20)
TEXT_COLOR = (20, 20, 20)

screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
clock = pygame.time.Clock()
font = pygame.font.SysFont("Arial", 24)

# Level map using ASCII:
# '#' = solid block, 'P' = player start, 'C' = coin, 'E' = enemy, 'F' = finish flag, '.' = empty
level_map = [
    "................................................................",
    "................................................................",
    "................................................................",
    "................................................................",
    ".............C..................................................",
    ".....................###.......................................",
    ".....###..................................................C....",
    "........................................E.....................",
    "..............###.............................................",
    ".......................................................###.....",
    "....P..............................................###.........",
    "###############################....############################",
    "###############################....############################",
]

# Build level objects
tiles = []
coins = []
enemies = []
player_spawn = None
flag_rect = None

for row_index, row in enumerate(level_map):
    for col_index, ch in enumerate(row):
        x = col_index * TILE_SIZE
        y = row_index * TILE_SIZE
        if ch == '#':
            tiles.append(pygame.Rect(x, y, TILE_SIZE, TILE_SIZE))
        elif ch == 'C':
            coins.append(pygame.Rect(x + TILE_SIZE//4, y + TILE_SIZE//4, TILE_SIZE//2, TILE_SIZE//2))
        elif ch == 'E':
            # Enemy will be a dict with rect, direction, patrol range later
            enemies.append({"rect": pygame.Rect(x, y + 8, TILE_SIZE - 8, TILE_SIZE - 8), "dir": -1, "speed": 1.5})
        elif ch == 'P':
            player_spawn = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        elif ch == 'F':
            flag_rect = pygame.Rect(x, y - TILE_SIZE, TILE_SIZE, TILE_SIZE * 2)

if player_spawn is None:
    # fallback spawn at left-top of first empty space
    player_spawn = pygame.Rect(TILE_SIZE, TILE_SIZE, TILE_SIZE, TILE_SIZE)


# Player class
class Player:
    def __init__(self, rect):
        self.rect = rect.copy()
        self.vx = 0.0
        self.vy = 0.0
        self.speed = 4.2
        self.jump_strength = -12.0
        self.on_ground = False
        self.score = 0
        self.alive = True

    def handle_input(self, keys):
        self.vx = 0
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            self.vx = -self.speed
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            self.vx = self.speed
        if (keys[pygame.K_UP] or keys[pygame.K_w] or keys[pygame.K_SPACE]) and self.on_ground:
            self.vy = self.jump_strength
            self.on_ground = False

    def apply_gravity(self):
        GRAVITY = 0.55
        self.vy += GRAVITY
        # cap fall speed
        if self.vy > 15:
            self.vy = 15

    def move_and_collide(self, tiles):
        # Horizontal movement & collision
        self.rect.x += int(self.vx)
        collided = [t for t in tiles if self.rect.colliderect(t)]
        for t in collided:
            if self.vx > 0:
                self.rect.right = t.left
            elif self.vx < 0:
                self.rect.left = t.right

        # Vertical movement & collision
        self.rect.y += int(self.vy)
        collided = [t for t in tiles if self.rect.colliderect(t)]
        self.on_ground = False
        for t in collided:
            if self.vy > 0:
                self.rect.bottom = t.top
                self.vy = 0
                self.on_ground = True
            elif self.vy < 0:
                self.rect.top = t.bottom
                self.vy = 0

    def update(self, keys, tiles):
        self.handle_input(keys)
        self.apply_gravity()
        self.move_and_collide(tiles)


player = Player(player_spawn)

# Determine level width for camera clamp
level_width = len(level_map[0]) * TILE_SIZE
level_height = len(level_map) * TILE_SIZE

# Simple helper to draw text
def draw_text(surf, text, x, y):
    img = font.render(text, True, TEXT_COLOR)
    surf.blit(img, (x, y))


def run_game():
    camera_x = 0
    camera_y = 0
    running = True
    win = False
    win_timer = 0

    global coins, enemies

    while running:
        dt = clock.tick(FPS) / 1000.0

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()
            if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                pygame.quit()
                sys.exit()

        keys = pygame.key.get_pressed()
        if player.alive and not win:
            player.update(keys, tiles)

            # Coin pickup
            new_coins = []
            for c in coins:
                if player.rect.colliderect(c):
                    player.score += 1
                else:
                    new_coins.append(c)
            coins = new_coins

            # Enemy logic: simple patrol and collision with player
            for en in enemies:
                en_rect = en["rect"]
                # Move enemy
                en_rect.x += en["dir"] * en["speed"]
                # Turn around if hits a tile on either side (simple)
                side_rect = en_rect.move(en["dir"] * 2, 0)
                if any(side_rect.colliderect(t) for t in tiles):
                    en["dir"] *= -1

                # If enemy is standing on nothing, let it fall (simple gravity)
                below = en_rect.move(0, 2)
                if not any(below.colliderect(t) for t in tiles):
                    en_rect.y += 4  # fall
                # Player vs enemy collision
                if player.rect.colliderect(en_rect):
                    # if player is falling and hits top of enemy -> bounce and remove enemy
                    if player.vy > 0 and player.rect.bottom - en_rect.top < 18:
                        player.vy = player.jump_strength * 0.5
                        try:
                            enemies.remove(en)
                            player.score += 2
                        except ValueError:
                            pass
                    else:
                        # player dies, for demo reset to spawn after short delay
                        player.alive = False

            # Win condition: reach far right of map OR reach flag if provided
            if flag_rect:
                if player.rect.colliderect(flag_rect):
                    win = True
            else:
                if player.rect.x > level_width - TILE_SIZE * 2:
                    win = True

        # If player died, reset after short delay by pressing R
        if not player.alive:
            # quick reset with R
            if keys[pygame.K_r]:
                reset_game()
                return run_game()

        # Camera follows player horizontally, clamp to level bounds
        camera_x = player.rect.centerx - SCREEN_WIDTH // 2
        camera_x = max(0, min(camera_x, level_width - SCREEN_WIDTH))
        camera_y = 0  # fixed vertical camera for simplicity; can be adjusted similarly

        # Draw background
        screen.fill(BG_COLOR)

        # Draw tiles
        for t in tiles:
            rect = pygame.Rect(t.x - camera_x, t.y - camera_y, t.width, t.height)
            pygame.draw.rect(screen, GROUND_COLOR, rect)
            # optional tile border
            pygame.draw.rect(screen, (20, 20, 20), rect, 1)

        # Draw coins
        for c in coins:
            rect = pygame.Rect(c.x - camera_x, c.y - camera_y, c.width, c.height)
            pygame.draw.ellipse(screen, COIN_COLOR, rect)
            pygame.draw.ellipse(screen, (160, 120, 0), rect.inflate(-6, -6), 1)

        # Draw enemies
        for en in enemies:
            er = en["rect"]
            rect = pygame.Rect(er.x - camera_x, er.y - camera_y, er.width, er.height)
            pygame.draw.rect(screen, ENEMY_COLOR, rect)
            # "eyes"
            eye_w = en["dir"] * 3
            pygame.draw.rect(screen, (255,255,255), (rect.x+8, rect.y+8, 6,6))

        # Draw flag if exists
        if flag_rect:
            rect = pygame.Rect(flag_rect.x - camera_x, flag_rect.y - camera_y, flag_rect.width, flag_rect.height)
            pygame.draw.rect(screen, FLAG_COLOR, rect)
            pygame.draw.rect(screen, (0,100,0), (rect.x + rect.width // 3, rect.y + 8, rect.width // 2, rect.height - 16))

        # Draw player
        prect = pygame.Rect(player.rect.x - camera_x, player.rect.y - camera_y, player.rect.width, player.rect.height)
        pygame.draw.rect(screen, PLAYER_COLOR, prect)
        # simple eye
        pygame.draw.rect(screen, (255,255,255), (prect.x + 8, prect.y + 8, 6, 6))

        # HUD
        draw_text(screen, f"Score: {player.score}", 16, 10)
        draw_text(screen, "Press R to reset when dead", 16, 36)

        if not player.alive:
            draw_text(screen, "You Died! Press R to respawn.", SCREEN_WIDTH//2 - 160, SCREEN_HEIGHT//2 - 10)

        if win:
            win_timer += 1
            draw_text(screen, "You Win! 🎉 Press Esc to quit or R to play again", SCREEN_WIDTH//2 - 260, SCREEN_HEIGHT//2 - 10)
            # freeze player
            # allow restarting
            if keys[pygame.K_r]:
                reset_game()
                return run_game()

        # Flip frame
        pygame.display.flip()

    pygame.quit()


def reset_game():
    # Reset player position, coins, enemies, score, etc.
    global player, coins, enemies
    player = Player(player_spawn)
    # Recreate coins/enemies from original level map
    coins = []
    enemies = []
    for row_index, row in enumerate(level_map):
        for col_index, ch in enumerate(row):
            x = col_index * TILE_SIZE
            y = row_index * TILE_SIZE
            if ch == 'C':
                coins.append(pygame.Rect(x + TILE_SIZE//4, y + TILE_SIZE//4, TILE_SIZE//2, TILE_SIZE//2))
            elif ch == 'E':
                enemies.append({"rect": pygame.Rect(x, y + 8, TILE_SIZE - 8, TILE_SIZE - 8), "dir": -1, "speed": 1.5})


if __name__ == "__main__":
    run_game()
