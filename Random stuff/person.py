# phys_platformer.py
# Single-file Pygame prototype adding many physics features requested.
# Run: python phys_platformer.py
# Requires: pygame

import pygame, math, random, sys, time
from collections import namedtuple

pygame.init()
SCREEN_W, SCREEN_H = 1200, 720
screen = pygame.display.set_mode((SCREEN_W, SCREEN_H))
clock = pygame.time.Clock()
FONT = pygame.font.SysFont("Consolas", 18)

# ----------------------
# Utility / Vec2
# ----------------------
class Vec:
    __slots__ = ("x","y")
    def __init__(self,x=0.0,y=0.0):
        self.x=float(x); self.y=float(y)
    def copy(self): return Vec(self.x,self.y)
    def tuple(self): return (self.x,self.y)
    def __add__(self,o): return Vec(self.x+o.x, self.y+o.y)
    def __iadd__(self,o): self.x+=o.x; self.y+=o.y; return self
    def __sub__(self,o): return Vec(self.x-o.x, self.y-o.y)
    def __mul__(self,s): return Vec(self.x*s, self.y*s)
    __rmul__ = __mul__
    def __truediv__(self,s): return Vec(self.x/s, self.y/s)
    def length(self): return math.hypot(self.x,self.y)
    def normalize(self):
        L=self.length()
        if L==0: return Vec(0,0)
        return self*(1.0/L)
    def dot(self,o): return self.x*o.x + self.y*o.y
    def perp(self): return Vec(-self.y, self.x)
    def clamp(self, mx):
        L=self.length()
        if L>mx:
            s = mx/L
            self.x *= s; self.y *= s

# ----------------------
# Basic physics body (2D rectangle-ish) simplified with rotation
# ----------------------
class Body:
    def __init__(self, pos, w, h, mass=1.0, angle=0.0, static=False, color=(200,100,100)):
        self.pos = pos.copy()
        self.w = w; self.h = h
        self.mass = mass
        self.inv_mass = 0.0 if static else (1.0/mass if mass>0 else 0.0)
        self.v = Vec(0,0)
        self.angle = angle
        self.ang = 0.0  # angular velocity
        self.inertia = (1/12)*mass*(w*w + h*h) if not static else 0.0
        self.inv_inertia = 0.0 if static or self.inertia==0 else 1.0/self.inertia
        self.static = static
        self.color = color
        self.forces = Vec(0,0)
        self.torque = 0.0
        self.friction = 0.9  # surface friction on collisions
        self.restitution = 0.0  # bounciness
    
    def aabb(self):
        # conservative axis-aligned bbox for collisions
        return pygame.Rect(self.pos.x - self.w/2, self.pos.y - self.h/2, self.w, self.h)

    def apply_force(self, f):
        if self.static: return
        self.forces += f

    def apply_impulse(self, impulse, contact_point=None):
        if self.static: return
        self.v += impulse * self.inv_mass
        if contact_point:
            r = contact_point - self.pos
            self.ang += self.inv_inertia * cross2(r, impulse)

    def integrate(self, dt, gravity):
        if self.static: 
            self.forces = Vec(0,0); self.torque = 0.0
            return
        # linear
        self.apply_force(gravity * self.mass)
        acc = self.forces * self.inv_mass
        self.v += acc * dt
        self.pos += self.v * dt
        # angular
        ang_acc = self.torque * (self.inv_inertia if self.inv_inertia else 0.0)
        self.ang += ang_acc * dt
        self.angle += self.ang * dt
        # damping
        self.v *= 0.999
        self.ang *= 0.995
        # reset accumulators
        self.forces = Vec(0,0)
        self.torque = 0.0

# small helper for 2D cross product scalar result
def cross2(a, b):
    # if both are Vec => scalar cross (a x b) = ax*by - ay*bx
    return a.x*b.y - a.y*b.x

# ----------------------
# Simple world with static tiles and many special fields/zones
# ----------------------
class World:
    def __init__(self):
        self.bodies = []
        self.static_tiles = []  # pygame.Rect
        self.moving_platforms = []
        self.ropes = []
        self.soft_blocks = []
        self.projectiles = []
        self.ragdolls = []
        self.wind = Vec(0,0)  # global wind vector (applies drag + push)
        self.wind_zones = []  # (rect, Vec)
        self.gravity_zones = []  # (rect, Vec)
        self.magnetic_fields = []  # (pos, strength, radius, polarity) polarity +/-1
        self.fluid_grid = PressureFluidGrid(80, 40, 30)  # grid cols, rows, cell_size
        self.spring_pads = []  # rect areas with strength
        self.debug = False

    def add_body(self, b): self.bodies.append(b)
    def add_tile(self, rect): self.static_tiles.append(rect)

# ----------------------
# Player (specialized Body)
# ----------------------
class Player(Body):
    def __init__(self,pos):
        super().__init__(pos, 34, 52, mass=3.0, color=(50,140,230))
        self.on_ground = False
        self.move_acc = 800.0
        self.max_speed = 320.0
        self.jump_speed = -560.0
        self.jump_hold_time = 0.18  # seconds you can hold to increase jump
        self.jump_timer = 0.0
        self.want_jump = False
        self.facing = 1
        self.score = 0
        self.charge = 1.0  # for magnetic interactions
        self.last_ground_y = pos.y

    def handle_input(self, keys, dt):
        # horizontal acceleration with momentum
        ax = 0.0
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            ax -= self.move_acc
            self.facing = -1
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            ax += self.move_acc
            self.facing = 1
        # apply horiz accel
        self.v.x += ax * dt
        # clamp
        if self.v.x > self.max_speed: self.v.x = self.max_speed
        if self.v.x < -self.max_speed: self.v.x = -self.max_speed

        # variable jump
        if (keys[pygame.K_UP] or keys[pygame.K_w] or keys[pygame.K_SPACE]):
            if self.on_ground and not self.want_jump:
                self.want_jump = True
                self.jump_timer = 0.0
                self.v.y = self.jump_speed
                self.on_ground = False
            elif self.want_jump and self.jump_timer < self.jump_hold_time:
                # sustain upward acceleration to allow higher jump while holding
                self.v.y += -1200.0 * dt  # extra upward impulse while held
                self.jump_timer += dt
        else:
            self.want_jump = False
            self.jump_timer = 0.0

# ----------------------
# Projectile (ballistic with drag)
# ----------------------
class Projectile:
    def __init__(self,pos,vel, radius=6, mass=0.2, color=(255,210,60)):
        self.pos = pos.copy(); self.v = vel.copy()
        self.r = radius; self.mass = mass; self.color=color
        self.life = 8.0

    def integrate(self, dt, gravity, world):
        # drag proportional to v^2 approximated linearly
        drag = self.v * (-0.9)  # simple proportional drag
        self.v += (gravity + drag) * dt
        # wind influence (global)
        self.v += world.wind * dt * 0.3
        # region wind zones
        for rect, wvec in world.wind_zones:
            if rect.collidepoint(self.pos.tuple()):
                self.v += wvec * dt * 0.8
        self.pos += self.v * dt
        self.life -= dt

# ----------------------
# Simple rope (Verlet with constraints)
# ----------------------
class Rope:
    def __init__(self, anchor_pos, length, segments, slack=1.0):
        self.points = [anchor_pos.copy()]
        for i in range(1, segments+1):
            self.points.append(Vec(anchor_pos.x, anchor_pos.y + (length/segments)*i))
        self.prev = [p.copy() for p in self.points]  # for verlet
        self.constraints = []
        self.stiffness = 0.9
        self.segment_length = length/segments
        self.pin0 = True  # anchor pinned
        self.drag = 0.999

    def verlet_step(self, dt, world_gravity):
        # verlet integration
        for i,p in enumerate(self.points):
            if i==0 and self.pin0:
                continue
            vx = (p.x - self.prev[i].x) * self.drag
            vy = (p.y - self.prev[i].y) * self.drag
            self.prev[i].x = p.x; self.prev[i].y = p.y
            p.x += vx + world_gravity.x * dt*dt
            p.y += vy + world_gravity.y * dt*dt
        # satisfy constraints (relaxation)
        for _ in range(4):
            for i in range(len(self.points)-1):
                a = self.points[i]; b = self.points[i+1]
                dx = b.x - a.x; dy = b.y - a.y
                d = math.hypot(dx,dy)
                if d==0: continue
                diff = (d - self.segment_length)/d
                if i==0 and self.pin0:
                    # move only b
                    b.x -= dx * diff * self.stiffness
                    b.y -= dy * diff * self.stiffness
                else:
                    adjust = 0.5 * diff * self.stiffness
                    a.x += dx * adjust
                    a.y += dy * adjust
                    b.x -= dx * adjust
                    b.y -= dy * adjust

# ----------------------
# Soft block (mass-spring grid)
# ----------------------
class SoftBlock:
    def __init__(self, topleft, cols, rows, cell, mass=0.5):
        self.cols=cols; self.rows=rows; self.cell = cell
        self.points=[]
        for r in range(rows):
            row=[]
            for c in range(cols):
                p = Vec(topleft[0] + c*cell, topleft[1] + r*cell)
                row.append({"pos":p, "prev":p.copy(), "mass":mass, "force":Vec(0,0), "pinned":False})
            self.points.append(row)
        # pin top row
        for c in range(cols):
            self.points[0][c]["pinned"]=True
        self.k = 2000.0  # spring stiffness
        self.damp = 0.995

    def step(self, dt, gravity):
        # forces and verlet-ish
        for r in range(self.rows):
            for c in range(self.cols):
                p = self.points[r][c]
                if p["pinned"]: continue
                # integrate simple explicit
                acc = gravity
                vx = (p["pos"].x - p["prev"].x) * self.damp
                vy = (p["pos"].y - p["prev"].y) * self.damp
                p["prev"].x = p["pos"].x; p["prev"].y = p["pos"].y
                p["pos"].x += vx + acc.x * dt*dt
                p["pos"].y += vy + acc.y * dt*dt
        # structural springs (horizontal & vertical)
        for r in range(self.rows):
            for c in range(self.cols):
                if c+1<self.cols: self._spring(r,c,r,c+1)
                if r+1<self.rows: self._spring(r,c,r+1,c)

    def _spring(self, r1,c1,r2,c2):
        a = self.points[r1][c1]["pos"]; b = self.points[r2][c2]["pos"]
        rest = self.cell
        dx = b.x - a.x; dy = b.y - a.y
        d = math.hypot(dx,dy)
        if d==0: return
        # positional correction
        diff = (d - rest)/d * 0.5
        if not self.points[r1][c1]["pinned"]:
            a.x += dx * diff
            a.y += dy * diff
        if not self.points[r2][c2]["pinned"]:
            b.x -= dx * diff
            b.y -= dy * diff

# ----------------------
# Simple pressure-fluid grid (very approximate)
# ----------------------
class PressureFluidGrid:
    def __init__(self, cols, rows, cell):
        self.cols=cols; self.rows=rows; self.cell=cell
        self.press = [[0.0 for _ in range(cols)] for _ in range(rows)]
        self.vel = [[Vec(0,0) for _ in range(cols)] for _ in range(rows)]
        # seed a pool in lower-left
        for r in range(rows-5, rows):
            for c in range(0, cols//3):
                self.press[r][c] = 1.0

    def step(self, dt):
        # very simple diffusion and decay
        newp = [[0.0 for _ in range(self.cols)] for _ in range(self.rows)]
        for r in range(self.rows):
            for c in range(self.cols):
                p = self.press[r][c]
                if p<=0: continue
                spread = p*0.1*dt
                newp[r][c] += p*(1-0.4*dt)
                for (nr,nc) in neighbors(r,c,self.rows,self.cols):
                    newp[nr][nc] += spread
        self.press = newp

    def get_force_at(self, x, y):
        c = int(x//self.cell); r = int(y//self.cell)
        if 0<=r<self.rows and 0<=c<self.cols:
            p = self.press[r][c]
            # upward force proportional to pressure
            return Vec(0, -1200.0 * p)
        return Vec(0,0)

def neighbors(r,c,rows,cols):
    for dr,dc in ((1,0),(-1,0),(0,1),(0,-1)):
        nr, nc = r+dr, c+dc
        if 0<=nr<rows and 0<=nc<cols:
            yield nr,nc

# ----------------------
# Ragdoll (2 segments) as two small bodies connected by distance constraint
# ----------------------
class Ragdoll:
    def __init__(self, pos):
        self.a = Body(Vec(pos.x, pos.y-10), 16, 20, mass=0.8, color=(200,60,60))
        self.b = Body(Vec(pos.x, pos.y+12), 18, 26, mass=1.0, color=(180,40,120))
        self.len = 24
    def step(self, dt, gravity):
        self.a.integrate(dt, gravity)
        self.b.integrate(dt, gravity)
        # relax distance
        for _ in range(3):
            dx = self.b.pos.x - self.a.pos.x; dy = self.b.pos.y - self.a.pos.y
            d = math.hypot(dx,dy)
            if d==0: continue
            diff = (d - self.len)/d * 0.5
            if not self.a.static:
                self.a.pos.x += dx*diff
                self.a.pos.y += dy*diff
            if not self.b.static:
                self.b.pos.x -= dx*diff
                self.b.pos.y -= dy*diff

# ----------------------
# Collision utilities (very simple AABB vs rect resolution)
# ----------------------
def resolve_body_tile(body, tile_rect):
    # simplest: push body out of tile's AABB if overlapping
    a = body.aabb()
    if not a.colliderect(tile_rect):
        return False
    overlap = a.clip(tile_rect)
    # push in smallest axis
    if overlap.w < overlap.h:
        # push left or right
        if a.centerx < tile_rect.centerx:
            body.pos.x -= overlap.w
            body.v.x = min(body.v.x, 0)
        else:
            body.pos.x += overlap.w
            body.v.x = max(body.v.x, 0)
    else:
        if a.centery < tile_rect.centery:
            body.pos.y -= overlap.h
            body.v.y = min(body.v.y, 0)
            body.on_ground = True if hasattr(body, "on_ground") else False
        else:
            body.pos.y += overlap.h
            body.v.y = max(body.v.y, 0)
    return True

# ----------------------
# Explosion: apply impulse & torque to bodies within radius
# ----------------------
def rotational_explosion(world, pos, radius, strength):
    for b in world.bodies:
        if b.static: continue
        dvec = b.pos - pos
        dist = dvec.length()
        if dist < radius and dist>0.0001:
            falloff = (1 - dist/radius)
            impulse = dvec.normalize() * (strength * falloff)
            contact = b.pos
            b.apply_impulse(impulse, contact)
            # also apply angular impulse (torque)
            torque = cross2(contact - b.pos, impulse) * 0.05
            b.ang += torque

# ----------------------
# Magnetic field force: simple inverse-square toward field center
# ----------------------
def apply_magnetic(world):
    for pos, strength, radius, polarity in world.magnetic_fields:
        for b in world.bodies:
            if b.static: continue
            d = b.pos - pos
            dist = d.length()
            if dist < radius and dist>0:
                f = (polarity * strength) / (dist*dist)
                b.apply_force(d.normalize() * (-f))

# ----------------------
# Gravity Zones: per-body gravity overrides
# ----------------------
def gravity_for_body(world, body, default):
    # check gravity zones
    for rect, gvec in world.gravity_zones:
        if rect.collidepoint(body.pos.tuple()):
            return gvec
    return default

# ----------------------
# Sample level topology + populating world
# ----------------------
def make_sample_world():
    w = World()
    # floor and some tiles
    ground_y = SCREEN_H - 80
    for x in range(0, SCREEN_W*2, 80):
        r = pygame.Rect(x, ground_y, 80, 80)
        w.add_tile(r)
    # a few floating tiles
    w.add_tile(pygame.Rect(400, ground_y-200, 160, 24))
    w.add_tile(pygame.Rect(700, ground_y-320, 160, 24))
    w.add_tile(pygame.Rect(1000, ground_y-240, 160, 24))
    # moving platform
    mp = Body(Vec(560, ground_y-120), 160, 18, static=False, mass=10.0, color=(120,160,100))
    mp.static = True  # we will move it kinematically (not simulated)
    mp.kinematic_range = (440, 680)
    mp.kinematic_dir = 1
    mp.kinematic_speed = 80.0
    w.moving_platforms.append(mp)
    # spring pad
    w.spring_pads.append((pygame.Rect(300, ground_y-40, 80, 40), 900.0))
    # rope anchored to tile
    rpos = Vec(420, ground_y-200)
    rope = Rope(rpos, 200, 8)
    w.ropes.append(rope)
    # soft block
    sb = SoftBlock((860, ground_y-280), 5, 5, 20)
    w.soft_blocks.append(sb)
    # magnetic field
    w.magnetic_fields.append((Vec(1050, ground_y-180), 120000.0, 260, 1))
    # gravity zone (inverted gravity)
    gz = pygame.Rect(940, ground_y-380, 220, 180)
    w.gravity_zones.append((gz, Vec(0, -280.0)))  # upward gravity in zone
    # wind zone
    wz = pygame.Rect(480, ground_y-420, 260, 220)
    w.wind_zones.append((wz, Vec(240, -20)))
    # seed ragdoll enemy
    rag = Ragdoll(Vec(760, ground_y-350))
    w.ragdolls.append(rag)
    # global wind mild
    w.wind = Vec(20.0, 0.0)
    return w

# ----------------------
# Main loop and integration
# ----------------------
def main():
    running = True
    world = make_sample_world()
    player = Player(Vec(120, SCREEN_H-200))
    world.add_body(player)
    # pushable box
    box = Body(Vec(320, SCREEN_H-200), 48, 48, mass=4.0, color=(180,140,90))
    world.add_body(box)
    # dynamic bodies
    for i in range(3):
        b = Body(Vec(520+i*40, SCREEN_H-300), 28, 28, mass=2.0, color=(200,80,80))
        world.add_body(b)
    # debug toggles
    debug = False
    PHYS_SUBSTEPS = 1
    GRAVITY = Vec(0, 900.0)
    last_t = time.time()

    while running:
        dt = clock.tick(60)/1000.0
        # cap dt
        if dt>0.05: dt = 0.05
        for e in pygame.event.get():
            if e.type==pygame.QUIT:
                running=False
            if e.type==pygame.KEYDOWN:
                if e.key==pygame.K_r:
                    return main()
                if e.key==pygame.K_f:
                    world.debug = not world.debug
                if e.key==pygame.K_e:
                    # explosion at mouse
                    mx,my = pygame.mouse.get_pos()
                    rotational_explosion(world, Vec(mx, my), 200, 18000.0)
            if e.type==pygame.MOUSEBUTTONDOWN:
                if e.button==1:
                    mx,my = pygame.mouse.get_pos()
                    # shoot projectile with player's facing
                    mouse = Vec(mx + cam_x, my + cam_y) if 'cam_x' in globals() else Vec(mx,my)
        # input
        keys = pygame.key.get_pressed()
        mouse = Vec(*pygame.mouse.get_pos())

        # fire projectile on mouse down (polling)
        if pygame.mouse.get_pressed()[0]:
            # spawn in front of player
            dir = Vec(1 if player.facing>0 else -1, -0.1)
            vel = dir.normalize() * 600 + player.v*0.5
            ppos = player.pos + Vec(player.w/2*player.facing, -6)
            world.projectiles.append(Projectile(ppos, vel))

        # kinematic moving platforms
        for mp in world.moving_platforms:
            # simple back-and-forth kinematic movement
            mp.pos.x += mp.kinematic_dir * mp.kinematic_speed * dt
            if mp.pos.x < mp.kinematic_range[0] or mp.pos.x > mp.kinematic_range[1]:
                mp.kinematic_dir *= -1
            # ensure body is present in static tiles if needed (visual only)

        # integrate many small steps for stability
        subdt = dt / PHYS_SUBSTEPS
        for _step in range(PHYS_SUBSTEPS):
            # update rope verlet
            for rope in world.ropes:
                rope.verlet_step(subdt, GRAVITY)
            # soft blocks
            for sb in world.soft_blocks:
                sb.step(subdt, GRAVITY)
            # ragdolls
            for rag in world.ragdolls:
                rag.step(subdt, GRAVITY)
            # projectiles
            for proj in world.projectiles[:]:
                proj.integrate(subdt, GRAVITY, world)
                if proj.life<=0:
                    world.projectiles.remove(proj)
            # apply magnetic & fluid forces
            apply_magnetic(world)
            world.fluid_grid.step(subdt)
            # bodies integrate with local gravity zones and wind & drag
            for b in world.bodies:
                # gravity zones override
                g = gravity_for_body(world, b, GRAVITY)
                # apply drag/wind
                # quadratic drag approximated: Fd = -k * v
                drag_k = 1.5
                b.apply_force(b.v * (-drag_k))
                # wind zones
                for rect, wvec in world.wind_zones:
                    if rect.collidepoint(b.pos.tuple()):
                        b.apply_force(wvec * 0.5)
                # global wind
                b.apply_force(world.wind * 0.02)
                # fluid buoyancy
                fb = world.fluid_grid.get_force_at(b.pos.x, b.pos.y)
                if fb.y != 0:
                    b.apply_force(fb * b.mass * 0.01)
                b.integrate(subdt, g)

            # simple collision against static tiles
            for b in world.bodies:
                b.on_ground = False
                for t in world.static_tiles:
                    resolve_body_tile(b, t)
                # moving platforms (kinematic) collision: treat as tile
                for mp in world.moving_platforms:
                    # create tile rect from mp
                    mrt = pygame.Rect(mp.pos.x-mp.w/2, mp.pos.y-mp.h/2, mp.w, mp.h)
                    if resolve_body_tile(b, mrt):
                        # if standing on mp, inherit velocity
                        if abs(b.pos.y - (mrt.top - b.h/2))<6:
                            b.v.x += mp.kinematic_dir * mp.kinematic_speed * 0.4

            # ropes interacting with bodies (simple: if the last rope segment collides with body, tug)
            for rope in world.ropes:
                for i,p in enumerate(rope.points):
                    for b in world.bodies:
                        if b.aabb().collidepoint(p.tuple()):
                            # pull body lightly toward rope point
                            dirp = Vec(p.x - b.pos.x, p.y - b.pos.y)
                            b.apply_force(dirp * 60.0)

            # spring pads apply impulse if body on them
            for rect, strength in world.spring_pads:
                for b in world.bodies:
                    if b.aabb().colliderect(rect):
                        # upward impulse
                        b.v.y = min(b.v.y, -strength * (1.0/b.mass))

        # collisions between dynamic bodies (simple elastic impulses)
        for i in range(len(world.bodies)):
            for j in range(i+1, len(world.bodies)):
                a = world.bodies[i]; b = world.bodies[j]
                if a.aabb().colliderect(b.aabb()):
                    # compute simple collision normal and impulse
                    n = b.pos - a.pos
                    dist = n.length()
                    if dist==0: continue
                    n = n.normalize()
                    rel = b.v - a.v
                    reln = rel.dot(n)
                    if reln>0: continue
                    e = min(a.restitution, b.restitution)
                    jimp = -(1+e)*reln / (a.inv_mass + b.inv_mass)
                    impulse = n * jimp
                    a.apply_impulse(-impulse)
                    b.apply_impulse(impulse)
                    # positional correction (separate a little)
                    sep = 0.5 * max(1.0, 1.0/(a.inv_mass + b.inv_mass))
                    a.pos += n * (-sep)
                    b.pos += n * (sep)

        # handle user input for player
        player.handle_input(keys, dt)

        # simple ground detection via tile collision heavy-handed
        player.on_ground = False
        for t in world.static_tiles:
            if resolve_body_tile(player, t):
                if player.pos.y + player.h/2 <= t.top + 6:
                    player.on_ground = True

        # camera centered on player with up-scrolling (allow vertical follow upward)
        global cam_x, cam_y
        cam_x = player.pos.x - SCREEN_W/2
        cam_y = player.pos.y - SCREEN_H/2
        # clamp camera bounds horizontally
        if cam_x < 0: cam_x = 0
        # no right bound for now
        if cam_y < 0: cam_y = 0

        # draw
        screen.fill((135, 200, 255))
        # draw fluid grid (pressure)
        for r in range(world.fluid_grid.rows):
            for c in range(world.fluid_grid.cols):
                p = world.fluid_grid.press[r][c]
                if p>0.02:
                    x = c*world.fluid_grid.cell - cam_x
                    y = r*world.fluid_grid.cell - cam_y
                    alpha = min(180, int(p*200))
                    surf = pygame.Surface((world.fluid_grid.cell, world.fluid_grid.cell), pygame.SRCALPHA)
                    surf.fill((20,80,160,alpha))
                    screen.blit(surf, (x,y))

        # draw static tiles
        for t in world.static_tiles:
            r = pygame.Rect(t.x-cam_x, t.y-cam_y, t.w, t.h) if hasattr(t,'w') else pygame.Rect(t.x-cam_x, t.y-cam_y, t.width, t.height)
            pygame.draw.rect(screen, (90,60,30), r)
            pygame.draw.rect(screen, (40,25,12), r,1)
        # spring pads
        for rect, strength in world.spring_pads:
            rr = pygame.Rect(rect.x-cam_x, rect.y-cam_y, rect.w, rect.h)
            pygame.draw.rect(screen, (220,180,40), rr)
        # moving platforms
        for mp in world.moving_platforms:
            r = pygame.Rect(mp.pos.x-mp.w/2-cam_x, mp.pos.y-mp.h/2-cam_y, mp.w, mp.h)
            pygame.draw.rect(screen, (120,160,100), r)

        # ropes
        for rope in world.ropes:
            for i in range(len(rope.points)-1):
                a = rope.points[i]; b = rope.points[i+1]
                pygame.draw.line(screen, (80,40,10), (a.x-cam_x,a.y-cam_y), (b.x-cam_x,b.y-cam_y), 4)
            for p in rope.points:
                pygame.draw.circle(screen, (150,120,100), (int(p.x-cam_x), int(p.y-cam_y)), 6)

        # soft blocks
        for sb in world.soft_blocks:
            for r in range(sb.rows):
                for c in range(sb.cols):
                    p = sb.points[r][c]["pos"]
                    pygame.draw.circle(screen, (180,120,120), (int(p.x-cam_x), int(p.y-cam_y)), 3)
                    # structural lines
                    if c+1<sb.cols:
                        p2 = sb.points[r][c+1]["pos"]
                        pygame.draw.line(screen, (140,90,90), (p.x-cam_x,p.y-cam_y),(p2.x-cam_x,p2.y-cam_y),1)
                    if r+1<sb.rows:
                        p2 = sb.points[r+1][c]["pos"]
                        pygame.draw.line(screen, (140,90,90), (p.x-cam_x,p.y-cam_y),(p2.x-cam_x,p2.y-cam_y),1)

        # ragdolls
        for rag in world.ragdolls:
            pygame.draw.rect(screen, rag.a.color, pygame.Rect(rag.a.pos.x-rag.a.w/2-cam_x, rag.a.pos.y-rag.a.h/2-cam_y, rag.a.w, rag.a.h))
            pygame.draw.rect(screen, rag.b.color, pygame.Rect(rag.b.pos.x-rag.b.w/2-cam_x, rag.b.pos.y-rag.b.h/2-cam_y, rag.b.w, rag.b.h))

        # projectiles
        for proj in world.projectiles:
            pygame.draw.circle(screen, proj.color, (int(proj.pos.x-cam_x), int(proj.pos.y-cam_y)), proj.r)

        # bodies
        for b in world.bodies:
            rect = pygame.Rect(b.pos.x - b.w/2 - cam_x, b.pos.y - b.h/2 - cam_y, b.w, b.h)
            pygame.draw.rect(screen, b.color, rect)
            # draw velocity vector
            if world.debug:
                end = (b.pos.x + b.v.x*0.12 - cam_x, b.pos.y + b.v.y*0.12 - cam_y)
                pygame.draw.line(screen, (0,0,0), (b.pos.x-cam_x,b.pos.y-cam_y), end, 2)

        # magnetic field visualize
        for pos, strength, radius, polarity in world.magnetic_fields:
            pygame.draw.circle(screen, (180,40,200), (int(pos.x-cam_x), int(pos.y-cam_y)), int(radius), 1)

        # gravity zones box
        for rect, gvec in world.gravity_zones:
            rr = pygame.Rect(rect.x-cam_x, rect.y-cam_y, rect.w, rect.h)
            pygame.draw.rect(screen, (100,100,250), rr, 2)

        # HUD and debug
        hud = [
            f"Pos: {player.pos.x:.1f},{player.pos.y:.1f}",
            f"Vel: {player.v.x:.1f},{player.v.y:.1f}",
            f"OnGround: {player.on_ground}",
            "LMB to shoot, E to explode at mouse, R reset, F toggle debug"
        ]
        y = 6
        for h in hud:
            surf = FONT.render(h, True, (10,10,10))
            screen.blit(surf, (8, y))
            y += 20

        if world.debug:
            # draw wind zones
            for rect, wvec in world.wind_zones:
                rr = pygame.Rect(rect.x-cam_x, rect.y-cam_y, rect.w, rect.h)
                pygame.draw.rect(screen, (0,180,0), rr, 1)
            # draw static tile boxes
            for t in world.static_tiles:
                r = pygame.Rect(t.x-cam_x, t.y-cam_y, t.width if hasattr(t,'width') else t.w, t.height if hasattr(t,'height') else t.h)
                pygame.draw.rect(screen, (200,0,0), r, 1)

        pygame.display.flip()

    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
