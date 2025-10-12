# ragdoll_platformer.py
# Run: python ragdoll_platformer.py
# Requires: pygame

import pygame, sys, math, random, time
pygame.init()
SCREEN_W, SCREEN_H = 1100, 680
screen = pygame.display.set_mode((SCREEN_W, SCREEN_H))
clock = pygame.time.Clock()
FONT = pygame.font.SysFont("Consolas", 16)

# -----------------------
# Lightweight 2D vector
# -----------------------
class Vec:
    __slots__ = ("x","y")
    def __init__(self, x=0.0, y=0.0):
        self.x = float(x); self.y = float(y)
    def copy(self): return Vec(self.x, self.y)
    def tuple(self): return (self.x, self.y)
    def __add__(self, o): return Vec(self.x + o.x, self.y + o.y)
    def __iadd__(self, o): self.x += o.x; self.y += o.y; return self
    def __sub__(self, o): return Vec(self.x - o.x, self.y - o.y)
    def __mul__(self, s): return Vec(self.x * s, self.y * s)
    __rmul__ = __mul__
    def __truediv__(self, s): return Vec(self.x / s, self.y / s)
    def __neg__(self): return Vec(-self.x, -self.y)
    def length(self): return math.hypot(self.x, self.y)
    def normalize(self):
        L = self.length()
        if L == 0: return Vec(0,0)
        return Vec(self.x / L, self.y / L)
    def dot(self, o): return self.x * o.x + self.y * o.y

# -----------------------
# Simple rectangular rigid body (no orientation used for collisions)
# -----------------------
class Body:
    def __init__(self, pos, w, h, mass=1.0, color=(200,80,80), static=False):
        self.pos = pos.copy()
        self.w = w; self.h = h
        self.mass = mass
        self.inv_mass = 0.0 if static or mass <= 0 else 1.0 / mass
        self.v = Vec(0,0)
        self.forces = Vec(0,0)
        self.static = static
        self.color = color

    def aabb(self):
        return pygame.Rect(self.pos.x - self.w/2, self.pos.y - self.h/2, self.w, self.h)

    def apply_force(self, f):
        if self.static: return
        self.forces += f

    def apply_impulse(self, imp):
        if self.static: return
        self.v += imp * self.inv_mass

    def integrate(self, dt, gravity):
        if self.static:
            self.forces = Vec(0,0)
            return
        # add gravity
        self.forces += gravity * self.mass
        acc = self.forces * self.inv_mass
        self.v += acc * dt
        # simple damping
        self.v *= 0.995
        self.pos += self.v * dt
        self.forces = Vec(0,0)

# -----------------------
# Ragdoll: articulated set of bodies + joints (distance constraints)
# -----------------------
class Ragdoll:
    def __init__(self, x, y, scale=1.0, color=(170,170,250)):
        s = scale
        # create parts: pelvis, torso, head, upper/lower limbs
        self.pelvis = Body(Vec(x, y), 28*s, 18*s, mass=3.0*s, color=color)
        self.torso  = Body(Vec(x, y-28*s), 30*s, 36*s, mass=4.0*s, color=color)
        self.head   = Body(Vec(x, y-66*s), 20*s, 20*s, mass=1.5*s, color=(240,220,200))
        # arms: left upper, left lower, right upper, right lower
        self.l_u = Body(Vec(x-28*s, y-20*s), 12*s, 24*s, mass=0.7*s, color=color)
        self.l_l = Body(Vec(Vec(x-28*s, y+4*s).x, Vec(x-28*s, y+4*s).y), 10*s, 22*s, mass=0.6*s, color=color)
        self.r_u = Body(Vec(x+28*s, y-20*s), 12*s, 24*s, mass=0.7*s, color=color)
        self.r_l = Body(Vec(Vec(x+28*s, y+4*s).x, Vec(x+28*s, y+4*s).y), 10*s, 22*s, mass=0.6*s, color=color)
        # legs: left/right upper/lower
        self.l_th = Body(Vec(x-12*s, y+32*s), 14*s, 28*s, mass=1.2*s, color=color)
        self.l_sh = Body(Vec(x-12*s, y+56*s), 12*s, 26*s, mass=1.1*s, color=color)
        self.r_th = Body(Vec(x+12*s, y+32*s), 14*s, 28*s, mass=1.2*s, color=color)
        self.r_sh = Body(Vec(x+12*s, y+56*s), 12*s, 26*s, mass=1.1*s, color=color)

        # collect parts for easy loops
        self.parts = [self.pelvis, self.torso, self.head,
                      self.l_u, self.l_l, self.r_u, self.r_l,
                      self.l_th, self.l_sh, self.r_th, self.r_sh]

        # joints: list of (bodyA, bodyB, rest_length, stiffness)
        def joint(a,b, rest, k=0.8):
            return [a,b,rest,k]
        self.joints = [
            joint(self.pelvis, self.torso, 28*s, 0.99),
            joint(self.torso, self.head, 38*s, 0.99),
            # arms
            joint(self.torso, self.l_u, 26*s, 0.99),
            joint(self.l_u, self.l_l, 26*s, 0.99),
            joint(self.torso, self.r_u, 26*s, 0.99),
            joint(self.r_u, self.r_l, 26*s, 0.99),
            # legs
            joint(self.pelvis, self.l_th, 36*s, 0.99),
            joint(self.l_th, self.l_sh, 28*s, 0.99),
            joint(self.pelvis, self.r_th, 36*s, 0.99),
            joint(self.r_th, self.r_sh, 28*s, 0.99),
            # cross stabilizer (pelvis to head)
            joint(self.pelvis, self.head, 74*s, 0.40)
        ]

        # simple orient stabilization params
        self.stand_torque = 30.0

        # state flags
        self.is_player = False
        self.on_ground = False

    def apply_force_to_pelvis(self, f):
        self.pelvis.apply_force(f)

    def apply_impulse_to_pelvis(self, imp):
        self.pelvis.apply_impulse(imp)

    def integrate(self, dt, gravity):
        # integrate parts
        for p in self.parts:
            p.integrate(dt, gravity)

    def relax_constraints(self, iterations=5):
        for _ in range(iterations):
            for (a,b,rest,k) in self.joints:
                # positional correction by mass weighting
                delta = b.pos - a.pos
                d = delta.length()
                if d == 0: continue
                diff = (d - rest) / d
                # how much each body moves (by inv_mass ratio)
                w1 = a.inv_mass; w2 = b.inv_mass
                wsum = w1 + w2
                if wsum == 0: continue
                # scale with stiffness
                corr = diff * k
                if not a.static:
                    a.pos += delta * (corr * (w1 / wsum))
                if not b.static:
                    b.pos -= delta * (corr * (w2 / wsum))

    def apply_balance(self):
        # Small PD-like stabilization: try to align torso roughly above pelvis
        target = Vec(self.pelvis.pos.x, self.pelvis.pos.y - 28)
        error = target - self.torso.pos
        # apply small restoring force to torso and pelvis
        self.torso.apply_force(error * 45.0)
        self.pelvis.apply_force((-error) * 12.0)

    def parts_on_ground(self, tiles):
        # simple ground test: if any lower-leg/shin collides with tiles
        on = False
        for limb in (self.l_sh, self.r_sh, self.l_th, self.r_th, self.pelvis):
            for t in tiles:
                if limb.aabb().colliderect(t):
                    on = True
        self.on_ground = on
        return on

# -----------------------
# small collision resolver (AABB)
# -----------------------
def resolve_part_vs_tile(part, tile):
    a = part.aabb()
    if not a.colliderect(tile):
        return False
    overlap = a.clip(tile)
    if overlap.w < overlap.h:
        # push in x
        if a.centerx < tile.centerx:
            part.pos.x -= overlap.w
            part.v.x = min(part.v.x, 0)
        else:
            part.pos.x += overlap.w
            part.v.x = max(part.v.x, 0)
    else:
        # push in y
        if a.centery < tile.centery:
            part.pos.y -= overlap.h
            part.v.y = min(part.v.y, 0)
        else:
            part.pos.y += overlap.h
            part.v.y = max(part.v.y, 0)
    return True

# -----------------------
# Make a simple world
# -----------------------
def make_world():
    tiles = []
    ground_y = SCREEN_H - 80
    # floor (wide)
    for x in range(-400, SCREEN_W*3, 80):
        tiles.append(pygame.Rect(x, ground_y, 80, 80))
    # platforms
    tiles.append(pygame.Rect(420, ground_y-200, 220, 24))
    tiles.append(pygame.Rect(760, ground_y-300, 220, 24))
    tiles.append(pygame.Rect(1040, ground_y-210, 180, 24))
    return tiles

# -----------------------
# Player controller wrapper around Ragdoll
# -----------------------
class PlayerController:
    def __init__(self, ragdoll):
        self.ragdoll = ragdoll
        ragdoll.is_player = True
        self.jump_windup = 0.0
        self.jump_hold_time = 0.18
        self.want_jump = False
        self.facing = 1

    def update_input(self, keys, dt):
        r = self.ragdoll
        # horizontal locomotion: apply horizontal force to pelvis
        accel = 2200.0
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            r.apply_force_to_pelvis(Vec(-accel, 0))
            self.facing = -1
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            r.apply_force_to_pelvis(Vec(accel, 0))
            self.facing = 1
        # small upward torque to torso to help balance when moving
        r.torso.apply_force(Vec((self.facing*-1)*12.0, 0))

        # variable jump: initial impulse on press, sustain small upward force while held
        if (keys[pygame.K_SPACE] or keys[pygame.K_w] or keys[pygame.K_UP]):
            if not self.want_jump and r.on_ground:
                # initial jump impulse to pelvis
                r.apply_impulse_to_pelvis(Vec(0, -520.0))
                self.want_jump = True
                self.jump_windup = 0.0
            elif self.want_jump and self.jump_windup < self.jump_hold_time:
                # sustain: small upward force while holding
                r.apply_force_to_pelvis(Vec(0, -1200.0))
                self.jump_windup += dt
        else:
            self.want_jump = False
            self.jump_windup = 0.0

        # use arrow keys to "flail" limbs (torque by applying forces to limb bodies)
        if keys[pygame.K_q]:
            r.l_u.apply_force(Vec(-4000, -2000))
        if keys[pygame.K_e]:
            r.r_u.apply_force(Vec(4000, -2000))

# -----------------------
# Enemy controller: simple AI to chase player
# -----------------------
class EnemyController:
    def __init__(self, ragdoll):
        self.ragdoll = ragdoll
        ragdoll.is_enemy = True
        self.ai_timer = 0.0

    def step(self, target_pos, dt):
        r = self.ragdoll
        # direct pelvis push towards player horizontally
        dir_x = target_pos.x - r.pelvis.pos.x
        if abs(dir_x) > 40:
            fx = 1400.0 * (1 if dir_x > 0 else -1)
            r.apply_force_to_pelvis(Vec(fx, 0))
            # occasional jump if obstacle
            if random.random() < 0.01 and r.on_ground:
                r.apply_impulse_to_pelvis(Vec(0, -420.0))
        # small random flail torque
        if random.random() < 0.02:
            limb = random.choice([r.l_u, r.r_u, r.l_sh, r.r_sh])
            limb.apply_force(Vec(random.uniform(-2000,2000), random.uniform(-2000,-500)))

# -----------------------
# Main loop
# -----------------------
def main():
    tiles = make_world()

    # player ragdoll
    player_rag = Ragdoll(180, SCREEN_H - 200, scale=1.0, color=(100,160,230))
    player_ctrl = PlayerController(player_rag)

    # enemies
    enemies = []
    controllers = []
    for i in range(3):
        x = 700 + i*140
        r = Ragdoll(x, SCREEN_H - 300 - i*20, scale=0.95, color=(200,120,120))
        enemies.append(r)
        controllers.append(EnemyController(r))

    gravity = Vec(0, 1400.0)

    cam_x = 0; cam_y = 0

    running = True
    paused = False

    while running:
        dt = clock.tick(60)/1000.0
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False
            if ev.type == pygame.KEYDOWN:
                if ev.key == pygame.K_ESCAPE:
                    running = False
                if ev.key == pygame.K_p:
                    paused = not paused
                if ev.key == pygame.K_r:
                    return main()

        if paused:
            continue

        keys = pygame.key.get_pressed()
        # player input
        player_ctrl.update_input(keys, dt)

        # integrate ragdolls + simple physics steps
        all_ragdolls = [player_rag] + enemies

        substeps = 2
        for _ in range(substeps):
            # integrate parts
            for rag in all_ragdolls:
                rag.integrate(dt/substeps, gravity)
            # relax constraints
            for rag in all_ragdolls:
                rag.relax_constraints(iterations=6)
            # collision with tiles
            for rag in all_ragdolls:
                for p in rag.parts:
                    for t in tiles:
                        resolve_part_vs_tile(p, t)
                rag.parts_on_ground(tiles)
            # enemy AI force
            for ctl in controllers:
                ctl.step(player_rag.pelvis.pos, dt/substeps)
            # stabilize player ragdoll
            player_rag.apply_balance()

        # simple camera follow
        cam_x = player_rag.pelvis.pos.x - SCREEN_W/2
        cam_y = player_rag.pelvis.pos.y - SCREEN_H/2
        if cam_x < 0: cam_x = 0
        if cam_y < 0: cam_y = 0

        # draw
        screen.fill((140,200,255))
        # draw tiles
        for t in tiles:
            rr = pygame.Rect(t.x - cam_x, t.y - cam_y, t.w, t.h)
            pygame.draw.rect(screen, (90,60,40), rr)
            pygame.draw.rect(screen, (30,20,10), rr, 1)

        # draw ragdolls (joints then parts)
        def draw_rag(ragdoll):
            # joints
            for (a,b,rest,k) in ragdoll.joints:
                pygame.draw.line(screen, (60,60,60),
                                 (int(a.pos.x - cam_x), int(a.pos.y - cam_y)),
                                 (int(b.pos.x - cam_x), int(b.pos.y - cam_y)), 2)
            # parts
            for p in ragdoll.parts:
                rect = pygame.Rect(p.pos.x - p.w/2 - cam_x, p.pos.y - p.h/2 - cam_y, p.w, p.h)
                pygame.draw.rect(screen, p.color, rect)
                pygame.draw.rect(screen, (20,20,20), rect, 1)

        draw_rag(player_rag)
        for r in enemies:
            draw_rag(r)

        # HUD
        lines = [
            "Controls: A/D or ←/→ to move, Space to jump (hold for higher jump)",
            "Q/E to flail limbs, R to reset, P pause, Esc quit"
        ]
        y = 8
        for L in lines:
            surf = FONT.render(L, True, (10,10,10))
            screen.blit(surf, (8, y))
            y += 20

        pygame.display.flip()

    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
