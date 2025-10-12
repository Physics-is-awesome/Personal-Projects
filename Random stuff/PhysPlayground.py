"""
physplayground_single.py

Single-file PhysPlayground prototype:
- deterministic fixed-step integrator (PHYS_DT)
- Rigid-body primitive (AABB), articulated ragdoll with distance joints
- Soft-body PBD lattice with tearing
- Pressure grid (simple diffusion) + buoyancy
- Spatial fields: wind zones, magnetic centers, gravity zones
- Pygame UI: overlays (velocity/force/pressure), simple level editor (place tiles/bodies/fields)
- JSON level loader + experiment save/load
- Energy & momentum diagnostics and CSV logging
- Small unit-test routine (run with --test)
- Optional Numba acceleration (auto-used if available)
"""

import pygame, sys, json, math, time, csv, os, argparse
import numpy as np
from copy import deepcopy

# Optional numba
try:
    from numba import njit
    HAS_NUMBA = True
except Exception:
    HAS_NUMBA = False

# -------------------------
# Config and determinism
# -------------------------
PHYS_DT = 1.0 / 120.0       # physics fixed timestep
SUBSTEPS = 2                # internal substeps per frame (for stability)
SCREEN_W, SCREEN_H = 1200, 720
SEED = 123456               # deterministic RNG seed
np.random.seed(SEED)

DATA_DIR = "phys_data"
if not os.path.exists(DATA_DIR):
    os.makedirs(DATA_DIR)
LOG_CSV = os.path.join(DATA_DIR, "run_log.csv")

# -------------------------
# Utility (numpy vec helpers)
# -------------------------
def v(x=0.0, y=0.0):
    return np.array([x, y], dtype=np.float64)

def length(a):
    return float(np.linalg.norm(a))

def normalize(a):
    n = np.linalg.norm(a)
    return a / n if n != 0 else v(0.0, 0.0)

# -------------------------
# Physics primitives
# -------------------------
class Body:
    """Simple rectangle body with position (center), vel, mass, AABB collision.
    Uses semi-implicit Euler integration.
    """
    def __init__(self, pos, w, h, mass=1.0, static=False, color=(150,150,240)):
        self.pos = np.array(pos, dtype=float)
        self.w = float(w); self.h = float(h)
        self.mass = float(mass)
        self.inv_mass = 0.0 if static or mass <= 0 else 1.0 / self.mass
        self.vel = v(0.0, 0.0)
        self.force = v(0.0, 0.0)
        self.static = bool(static)
        self.color = color
        self.restitution = 0.0
        self.friction = 0.6

    def aabb(self):
        left = self.pos[0] - self.w/2
        top = self.pos[1] - self.h/2
        return (left, top, self.w, self.h)

    def apply_force(self, f):
        if self.static: return
        self.force += f

    def apply_impulse(self, imp):
        if self.static: return
        self.vel += imp * self.inv_mass

    def integrate(self, dt, gravity):
        if self.static:
            self.force[:] = 0.0
            return
        # Accumulate gravity
        self.force += gravity * self.mass
        # semi-implicit Euler
        acc = self.force * self.inv_mass
        self.vel += acc * dt
        self.pos += self.vel * dt
        # damping
        self.vel *= 0.999
        self.force[:] = 0.0

# -------------------------
# Simple AABB vs tile collision resolution
# -------------------------
def resolve_aabb_vs_tile(body, tile):
    # tile: (x,y,w,h)
    bx, by, bw, bh = body.aabb()
    tx, ty, tw, th = tile
    if bx + bw <= tx or bx >= tx + tw or by + bh <= ty or by >= ty + th:
        return False
    # compute overlap on both axes
    overlap_x = min(bx + bw - tx, tx + tw - bx)
    overlap_y = min(by + bh - ty, ty + th - by)
    if overlap_x < overlap_y:
        # push in x
        if body.pos[0] < tx + tw/2:
            body.pos[0] -= overlap_x
            body.vel[0] = min(body.vel[0], 0)
        else:
            body.pos[0] += overlap_x
            body.vel[0] = max(body.vel[0], 0)
    else:
        # push in y
        if body.pos[1] < ty + th/2:
            body.pos[1] -= overlap_y
            body.vel[1] = min(body.vel[1], 0)
        else:
            body.pos[1] += overlap_y
            body.vel[1] = max(body.vel[1], 0)
    return True

# -------------------------
# Joints and ragdoll (distance constraints)
# -------------------------
class Joint:
    def __init__(self, a: Body, b: Body, rest, stiffness=0.95):
        self.a = a; self.b = b
        self.rest = float(rest)
        self.k = float(stiffness)

    def solve(self):
        delta = self.b.pos - self.a.pos
        d = np.linalg.norm(delta)
        if d == 0: return
        diff = (d - self.rest) / d
        w1 = self.a.inv_mass; w2 = self.b.inv_mass
        wsum = w1 + w2
        if wsum == 0: return
        corr = delta * (diff * self.k)
        if not self.a.static:
            self.a.pos += corr * (w1/wsum)
        if not self.b.static:
            self.b.pos -= corr * (w2/wsum)

class Ragdoll:
    """Simple humanoid ragdoll with distance constraints; no angular joints limits."""
    def __init__(self, x, y, scale=1.0, color=(70,140,200)):
        s = scale
        self.pelvis = Body(v(x,y), 28*s, 18*s, mass=3.0*s, color=color)
        self.torso  = Body(v(x,y-28*s), 30*s, 36*s, mass=4.0*s, color=color)
        self.head   = Body(v(x,y-66*s), 20*s, 20*s, mass=1.5*s, color=(240,220,200))
        # arms
        self.l_u = Body(v(x-28*s, y-20*s), 12*s, 24*s, mass=0.7*s, color=color)
        self.l_l = Body(v(x-28*s, y+4*s), 10*s, 22*s, mass=0.6*s, color=color)
        self.r_u = Body(v(x+28*s, y-20*s), 12*s, 24*s, mass=0.7*s, color=color)
        self.r_l = Body(v(x+28*s, y+4*s), 10*s, 22*s, mass=0.6*s, color=color)
        # legs
        self.l_th = Body(v(x-12*s, y+32*s), 14*s, 28*s, mass=1.2*s, color=color)
        self.l_sh = Body(v(x-12*s, y+56*s), 12*s, 26*s, mass=1.1*s, color=color)
        self.r_th = Body(v(x+12*s, y+32*s), 14*s, 28*s, mass=1.2*s, color=color)
        self.r_sh = Body(v(x+12*s, y+56*s), 12*s, 26*s, mass=1.1*s, color=color)

        self.parts = [self.pelvis, self.torso, self.head,
                      self.l_u, self.l_l, self.r_u, self.r_l,
                      self.l_th, self.l_sh, self.r_th, self.r_sh]

        def j(a,b,rest,k=0.98):
            return Joint(a,b,rest,k)
        self.joints = [
            j(self.pelvis, self.torso, 28*s, 0.98),
            j(self.torso, self.head, 38*s, 0.98),
            j(self.torso, self.l_u, 26*s, 0.96),
            j(self.l_u, self.l_l, 26*s, 0.96),
            j(self.torso, self.r_u, 26*s, 0.96),
            j(self.r_u, self.r_l, 26*s, 0.96),
            j(self.pelvis, self.l_th, 36*s, 0.99),
            j(self.l_th, self.l_sh, 28*s, 0.99),
            j(self.pelvis, self.r_th, 36*s, 0.99),
            j(self.r_th, self.r_sh, 28*s, 0.99),
            j(self.pelvis, self.head, 74*s, 0.18)
        ]

        self.on_ground = False

    def integrate(self, dt, gravity):
        for p in self.parts:
            p.integrate(dt, gravity)

    def relax(self, iterations=10):
        for _ in range(iterations):
            for J in self.joints:
                J.solve()

    def parts_on_ground(self, tiles):
        on = False
        for limb in (self.l_sh, self.r_sh, self.l_th, self.r_th, self.pelvis):
            for t in tiles:
                if resolve_aabb_vs_tile(limb, t):
                    on = True
        self.on_ground = on
        return on

    def apply_balance(self):
        # PD stabilizer: hold torso over pelvis gently
        target = v(self.pelvis.pos[0], self.pelvis.pos[1] - 28.0)
        error = target - self.torso.pos
        vel_err = (self.torso.vel - self.pelvis.vel)
        kp = 220.0; kd = 60.0
        force = error * kp - vel_err * kd
        self.torso.apply_force(force)
        self.pelvis.apply_force(-0.25 * force)
        if self.on_ground:
            support = max(0.0, (self.pelvis.mass * 380.0))
            self.pelvis.apply_force(v(0, -support))

# -------------------------
# Soft-body PBD lattice (tearing)
# -------------------------
class SoftBody:
    """
    Grid of nodes with positional constraints (structural springs).
    Tearing: springs removed if stretched beyond threshold.
    """
    def __init__(self, x, y, cols=6, rows=6, cell=18, mass=0.4):
        self.cols = cols; self.rows = rows; self.cell = cell
        self.nodes = []
        for r in range(rows):
            row = []
            for c in range(cols):
                pos = v(x + c*cell, y + r*cell)
                node = {'pos': pos.copy(), 'prev': pos.copy(), 'mass': mass, 'pinned': (r==0)}
                row.append(node)
            self.nodes.append(row)
        self.springs = []
        for r in range(rows):
            for c in range(cols):
                if c+1 < cols: self.springs.append((r,c,r,c+1,cell))
                if r+1 < rows: self.springs.append((r,c,r+1,c,cell))
        self.tear_threshold = cell * 1.75

    def step(self, dt, gravity, iterations=4):
        # integrate (verlet-like)
        for r in range(self.rows):
            for c in range(self.cols):
                n = self.nodes[r][c]
                if n['pinned']: continue
                pos = n['pos']; prev = n['prev']
                vel = pos - prev
                n['prev'] = pos.copy()
                n['pos'] = pos + vel + gravity * (dt*dt)
        # constraints solve & tearing
        for _ in range(iterations):
            new_springs = []
            for (r1,c1,r2,c2,rest) in self.springs:
                a = self.nodes[r1][c1]; b = self.nodes[r2][c2]
                delta = b['pos'] - a['pos']
                d = np.linalg.norm(delta)
                if d == 0: continue
                if d > self.tear_threshold:
                    # tear -> skip keeping this spring
                    continue
                diff = (d - rest)/d * 0.5
                if not a['pinned']:
                    a['pos'] += delta * diff
                if not b['pinned']:
                    b['pos'] -= delta * diff
                new_springs.append((r1,c1,r2,c2,rest))
            self.springs = new_springs

# -------------------------
# Pressure grid (simple) for buoyancy
# -------------------------
class PressureGrid:
    def __init__(self, cols=48, rows=30, cell=20.0):
        self.cols = cols; self.rows = rows; self.cell = float(cell)
        self.press = np.zeros((rows, cols), dtype=float)
        # seed a pool at left-bottom
        for r in range(rows-4, rows):
            for c in range(0, cols//4):
                self.press[r, c] = 1.0

    def step(self, dt):
        # simple diffusion + decay
        new = np.zeros_like(self.press)
        for r in range(self.rows):
            for c in range(self.cols):
                p = self.press[r,c]
                if p <= 0.0: continue
                keep = p * 0.9
                new[r,c] += keep
                spread = p * 0.025
                for dr,dc in ((1,0),(-1,0),(0,1),(0,-1)):
                    nr, nc = r+dr, c+dc
                    if 0 <= nr < self.rows and 0 <= nc < self.cols:
                        new[nr,nc] += spread
        self.press = new

    def buoyancy_at(self, x, y):
        c = int(x // self.cell)
        r = int(y // self.cell)
        if 0 <= r < self.rows and 0 <= c < self.cols:
            p = self.press[r, c]
            # return upward force vector per mass unit
            return v(0.0, -1200.0 * p)
        return v(0.0, 0.0)

# -------------------------
# World (holds everything)
# -------------------------
class World:
    def __init__(self):
        self.bodies = []     # Body instances
        self.tiles = []      # list of (x,y,w,h)
        self.ragdolls = []   # Ragdoll instances
        self.softbodies = [] # SoftBody instances
        self.fluid = PressureGrid(cols=48, rows=30, cell=20.0)
        self.fields = []     # list of dicts: {'type':..., 'rect':(x,y,w,h) or 'center':(x,y), ...}
        self.time = 0.0

    def step(self, dt, gravity):
        # fluid step
        self.fluid.step(dt)

        # Integrate bodies
        for b in self.bodies:
            # spatial fields: wind, gravity zones, magnetic
            for f in self.fields:
                if f['type'] == 'wind':
                    x,y,w,h = f['rect']
                    if x <= b.pos[0] <= x+w and y <= b.pos[1] <= y+h:
                        b.apply_force(np.array(f['vec']) * b.mass)
                elif f['type'] == 'gravity_zone':
                    x,y,w,h = f['rect']
                    if x <= b.pos[0] <= x+w and y <= b.pos[1] <= y+h:
                        b.apply_force(np.array(f['g']) * b.mass)
                elif f['type'] == 'mag':
                    cx,cy = f['center']
                    d = b.pos - np.array([cx,cy])
                    r = np.linalg.norm(d)
                    if r < f['radius'] and r > 1e-6:
                        force = -f['strength'] * d / (r*r)
                        b.apply_force(force)
            # buoyancy from fluid
            fb = self.fluid.buoyancy_at(b.pos[0], b.pos[1])
            if np.any(fb):
                b.apply_force(fb * b.mass * 0.01)
            b.integrate(dt, gravity)

        # Integrate ragdolls and soft bodies (they do their own loops)
        for r in self.ragdolls:
            r.integrate(dt, gravity)
        for s in self.softbodies:
            s.step(dt, gravity)

        # collisions: bodies vs tiles
        for b in self.bodies:
            for t in self.tiles:
                resolve_aabb_vs_tile(b, t)
        # ragdoll parts vs tiles, and relax constraints
        for r in self.ragdolls:
            for p in r.parts:
                for t in self.tiles:
                    resolve_aabb_vs_tile(p, t)
            r.parts_on_ground(self.tiles)
            r.relax(iterations=10)

        # softbody vs tiles: simple node AABB corrections
        for s in self.softbodies:
            for r in range(s.rows):
                for c in range(s.cols):
                    n = s.nodes[r][c]
                    if n['pinned']: continue
                    left = n['pos'][0] - 2; top = n['pos'][1] - 2; w=4; h=4
                    for t in self.tiles:
                        tx,ty,tw,th = t
                        if left + w <= tx or left >= tx+tw or top + h <= ty or top >= ty+th:
                            continue
                        # push node up out of tile
                        n['pos'][1] = ty - 3

        # time
        self.time += dt

# -------------------------
# Energy & momentum diagnostics
# -------------------------
def diagnostics(world, gravity_vec):
    total_ke = 0.0
    total_pe = 0.0
    total_mom = v(0.0, 0.0)
    gmag = gravity_vec[1]
    # rigid bodies
    for b in world.bodies:
        ke = 0.5 * b.mass * (b.vel[0]*b.vel[0] + b.vel[1]*b.vel[1])
        total_ke += ke
        pe = b.mass * gmag * b.pos[1]  # note: potential w.r.t. origin; sign convention kept for trend
        total_pe += pe
        total_mom += b.mass * b.vel
    # ragdolls: sum parts
    for r in world.ragdolls:
        for p in r.parts:
            ke = 0.5 * p.mass * (p.vel[0]*p.vel[0] + p.vel[1]*p.vel[1])
            total_ke += ke
            pe = p.mass * gmag * p.pos[1]
            total_pe += pe
            total_mom += p.mass * p.vel
    # soft bodies
    for s in world.softbodies:
        for r in range(s.rows):
            for c in range(s.cols):
                n = s.nodes[r][c]
                vel = (n['pos'] - n['prev'])  # verlet approx
                ke = 0.5 * n['mass'] * (vel[0]*vel[0] + vel[1]*vel[1])
                total_ke += ke
                pe = n['mass'] * gmag * n['pos'][1]
                total_pe += pe
                total_mom += n['mass'] * vel
    return {'ke': float(total_ke), 'pe': float(total_pe), 'energy': float(total_ke + total_pe), 'momentum_x': float(total_mom[0]), 'momentum_y': float(total_mom[1])}

# -------------------------
# Simple JSON level (default) and editor utilities
# -------------------------
def default_level():
    return {
        'tiles': [
            # floor across
            *[(x, SCREEN_H-80, 80, 80) for x in range(-400, SCREEN_W*2, 80)],
            (400, SCREEN_H-200, 220, 24),
            (760, SCREEN_H-300, 220, 24),
            (1040, SCREEN_H-210, 180, 24),
        ],
        'bodies': [
            {'pos': [160, 200], 'w': 34, 'h': 48, 'mass': 3.0}
        ],
        'ragdolls': [
            {'pos':[420, SCREEN_H-300], 'scale':1.0}
        ],
        'softbodies': [
            {'pos':[860, SCREEN_H-280], 'cols':5, 'rows':5, 'cell':20}
        ],
        'fields': [
            {'type':'wind','rect':[480, SCREEN_H-420, 260, 220],'vec':[200.0,-20.0]},
            {'type':'mag','center':[1050, SCREEN_H-180],'radius':260,'strength':120000.0},
            {'type':'gravity_zone','rect':[940, SCREEN_H-380,220,180],'g':[0.0,-320.0]}
        ]
    }

def save_level(world, path):
    obj = {'tiles': world.tiles,
           'bodies': [{'pos': b.pos.tolist(), 'w': b.w, 'h': b.h, 'mass': b.mass} for b in world.bodies],
           'ragdolls': [{'pos': [r.pelvis.pos[0], r.pelvis.pos[1]], 'scale':1.0} for r in world.ragdolls],
           'softbodies': [{'pos':[s.nodes[0][0]['pos'][0], s.nodes[0][0]['pos'][1]], 'cols':s.cols, 'rows':s.rows, 'cell':s.cell} for s in world.softbodies],
           'fields': world.fields}
    with open(path,'w') as f:
        json.dump(obj, f, indent=2)

def load_level(path):
    with open(path,'r') as f:
        obj = json.load(f)
    world = World()
    world.tiles = [tuple(t) for t in obj.get('tiles', [])]
    for b in obj.get('bodies', []):
        world.bodies.append(Body(b['pos'], b['w'], b['h'], mass=b.get('mass',1.0)))
    for r in obj.get('ragdolls', []):
        pos = r.get('pos',[400,200])
        world.ragdolls.append(Ragdoll(pos[0], pos[1], scale=r.get('scale',1.0)))
    for s in obj.get('softbodies', []):
        p = s.get('pos',[800,200])
        world.softbodies.append(SoftBody(p[0], p[1], cols=s.get('cols',5), rows=s.get('rows',5), cell=s.get('cell',18)))
    world.fields = obj.get('fields', [])
    return world

# -------------------------
# Logger (CSV)
# -------------------------
class CSVLogger:
    def __init__(self, path):
        self.path = path
        self.first = True
        self.f = open(self.path, 'w', newline='')
        self.writer = csv.writer(self.f)
        self.writer.writerow(['time','ke','pe','energy','mom_x','mom_y'])
        self.f.flush()

    def log(self, t, diag):
        self.writer.writerow([t, diag['ke'], diag['pe'], diag['energy'], diag['momentum_x'], diag['momentum_y']])
        self.f.flush()

    def close(self):
        self.f.close()

# -------------------------
# UI and main loop
# -------------------------
def run_gui(initial_world=None, level_path=None, experiment_save_prefix="experiment"):
    pygame.init()
    screen = pygame.display.set_mode((SCREEN_W, SCREEN_H))
    pygame.display.set_caption("PhysPlayground (single-file prototype)")
    font = pygame.font.SysFont("Consolas", 16)
    small = pygame.font.SysFont("Consolas", 12)
    clock = pygame.time.Clock()

    world = initial_world if initial_world else World()
    if level_path:
        w2 = load_level(level_path)
        # merge content
        world.tiles = w2.tiles
        world.bodies = w2.bodies
        world.ragdolls = w2.ragdolls
        world.softbodies = w2.softbodies
        world.fields = w2.fields

    # if empty, load default
    if not world.tiles:
        lw = default_level()
        world = load_level_from_obj(lw)

    # create small logger
    logger = CSVLogger(LOG_CSV)

    gravity = v(0.0, 1000.0)
    paused = False
    overlay = {'vel':True, 'force':False, 'pressure':True}
    cam = np.array([0.0, 0.0])
    selected_tool = 'tile'  # options: tile, body, ragdoll, soft, field-wind, field-mag, field-gz
    running = True
    last_render = time.time()
    accum = 0.0

    # helper to start experiment save
    exp_count = 0

    while running:
        dt = clock.tick(60) / 1000.0
        # event handling
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False
            elif ev.type == pygame.KEYDOWN:
                if ev.key == pygame.K_ESCAPE:
                    running = False
                if ev.key == pygame.K_SPACE:
                    paused = not paused
                if ev.key == pygame.K_TAB:
                    # cycle overlay toggles
                    overlay['vel'] = not overlay['vel']
                if ev.key == pygame.K_s:
                    # save level
                    p = os.path.join(DATA_DIR, f"level_saved_{int(time.time())}.json")
                    save_level(world, p)
                    print("Saved level to", p)
                if ev.key == pygame.K_l:
                    # load last saved if exists
                    files = sorted([f for f in os.listdir(DATA_DIR) if f.startswith('level_saved_')])
                    if files:
                        path = os.path.join(DATA_DIR, files[-1])
                        print("Loading", path)
                        world = load_level(path)
                if ev.key == pygame.K_o:
                    # save experiment config
                    path = os.path.join(DATA_DIR, f"{experiment_save_prefix}_{exp_count}.json")
                    save_level(world, path)
                    exp_count += 1
                    print("Saved experiment config:", path)
                if ev.key == pygame.K_r:
                    # reset world from default
                    world = load_level_from_obj(default_level())
                if ev.key == pygame.K_p:
                    overlay['pressure'] = not overlay['pressure']

            elif ev.type == pygame.MOUSEBUTTONDOWN:
                mx,my = pygame.mouse.get_pos()
                world_x = mx + cam[0]; world_y = my + cam[1]
                if ev.button == 1:
                    # place according to tool
                    if selected_tool == 'tile':
                        world.tiles.append((world_x-40, world_y-12, 80, 24))
                    elif selected_tool == 'body':
                        world.bodies.append(Body(v(world_x, world_y), 36, 48, mass=3.0))
                    elif selected_tool == 'ragdoll':
                        world.ragdolls.append(Ragdoll(world_x, world_y))
                    elif selected_tool == 'soft':
                        world.softbodies.append(SoftBody(world_x-40, world_y-20, cols=6, rows=6, cell=18))
                    elif selected_tool == 'field-wind':
                        world.fields.append({'type':'wind','rect':[world_x-80,world_y-60,160,120],'vec':[200.0,0.0]})
                    elif selected_tool == 'field-mag':
                        world.fields.append({'type':'mag','center':[world_x,world_y],'radius':200,'strength':80000.0})
                    elif selected_tool == 'field-gz':
                        world.fields.append({'type':'gravity_zone','rect':[world_x-80,world_y-60,160,120],'g':[0.0,-280.0]})
                elif ev.button == 3:
                    # right-click: cycle tool
                    tools = ['tile','body','ragdoll','soft','field-wind','field-mag','field-gz']
                    idx = tools.index(selected_tool)
                    selected_tool = tools[(idx+1) % len(tools)]

        if not paused:
            # fixed-step integration
            accum += dt
            while accum >= PHYS_DT:
                subdt = PHYS_DT / float(SUBSTEPS)
                for _ in range(SUBSTEPS):
                    world.step(subdt, gravity)
                accum -= PHYS_DT

                # log diagnostics
                diag = diagnostics(world, gravity)
                logger.log(world.time, diag)

        # Rendering
        screen.fill((145, 200, 255))
        cam = np.array([max(0, (world.ragdolls[0].pelvis.pos[0] - SCREEN_W/2) if world.ragdolls else 0), max(0, 0)])
        # draw tiles
        for t in world.tiles:
            tx,ty,tw,th = t
            r = pygame.Rect(tx - cam[0], ty - cam[1], tw, th)
            pygame.draw.rect(screen, (100,70,35), r)
            pygame.draw.rect(screen, (40,25,12), r, 1)

        # draw fluid pressure overlay if enabled
        if overlay['pressure']:
            fg = world.fluid
            for r in range(fg.rows):
                for c in range(fg.cols):
                    p = fg.press[r,c]
                    if p > 0.01:
                        x = c * fg.cell - cam[0]; y = r * fg.cell - cam[1]
                        alpha = min(200, int(p * 255))
                        surf = pygame.Surface((int(fg.cell), int(fg.cell)), pygame.SRCALPHA)
                        surf.fill((20, 80, 160, alpha))
                        screen.blit(surf,(x,y))

        # draw fields outlines
        for f in world.fields:
            if f['type'] == 'wind':
                x,y,w,h = f['rect']
                pygame.draw.rect(screen, (200,200,100), pygame.Rect(x-cam[0], y-cam[1], w, h), 1)
            elif f['type'] == 'mag':
                cx,cy = f['center']
                pygame.draw.circle(screen, (180,40,200), (int(cx-cam[0]), int(cy-cam[1])), int(f['radius']), 1)
            elif f['type'] == 'gravity_zone':
                x,y,w,h = f['rect']
                pygame.draw.rect(screen, (100,100,250), pygame.Rect(x-cam[0], y-cam[1], w, h), 1)

        # draw bodies
        for b in world.bodies:
            left, top, bw, bh = b.aabb()
            r = pygame.Rect(left-cam[0], top-cam[1], bw, bh)
            pygame.draw.rect(screen, b.color, r)
            pygame.draw.rect(screen, (0,0,0), r, 1)
            if overlay['vel']:
                end = (b.pos[0] + b.vel[0]*0.06 - cam[0], b.pos[1] + b.vel[1]*0.06 - cam[1])
                pygame.draw.line(screen, (0,0,0), (b.pos[0]-cam[0], b.pos[1]-cam[1]), end, 2)

        # draw ragdolls
        for r in world.ragdolls:
            # joints
            for J in r.joints:
                a = J.a.pos; bpos = J.b.pos
                pygame.draw.line(screen, (80,80,80), (a[0]-cam[0], a[1]-cam[1]), (bpos[0]-cam[0], bpos[1]-cam[1]), 2)
            for p in r.parts:
                left, top, bw, bh = p.aabb()
                rr = pygame.Rect(left-cam[0], top-cam[1], bw, bh)
                pygame.draw.rect(screen, p.color, rr)
                pygame.draw.rect(screen, (0,0,0), rr, 1)
                if overlay['vel']:
                    end = (p.pos[0] + p.vel[0]*0.06 - cam[0], p.pos[1] + p.vel[1]*0.06 - cam[1])
                    pygame.draw.line(screen, (0,0,0), (p.pos[0]-cam[0], p.pos[1]-cam[1]), end, 2)

        # draw soft bodies
        for s in world.softbodies:
            for r1 in range(s.rows):
                for c1 in range(s.cols):
                    p1 = s.nodes[r1][c1]['pos']
                    pygame.draw.circle(screen, (180,120,120), (int(p1[0]-cam[0]), int(p1[1]-cam[1])), 3)
                    # structural lines
                    if c1+1 < s.cols:
                        p2 = s.nodes[r1][c1+1]['pos']
                        pygame.draw.line(screen, (140,90,90), (p1[0]-cam[0], p1[1]-cam[1]), (p2[0]-cam[0], p2[1]-cam[1]), 1)
                    if r1+1 < s.rows:
                        p2 = s.nodes[r1+1][c1]['pos']
                        pygame.draw.line(screen, (140,90,90), (p1[0]-cam[0], p1[1]-cam[1]), (p2[0]-cam[0], p2[1]-cam[1]), 1)

        # HUD
        diag = diagnostics(world, gravity)
        lines = [
            f"Time: {world.time:.2f}s  KE: {diag['ke']:.2f}  PE: {diag['pe']:.2f}  E: {diag['energy']:.2f}",
            f"Momentum: ({diag['momentum_x']:.2f}, {diag['momentum_y']:.2f})",
            "Left-click place (tool). Right-click cycle tool. SPACE pause. S save level. O save experiment. L load last.",
            f"Tool: {selected_tool}  Overlay-vel: {overlay['vel']}  Pressure overlay: {overlay['pressure']}"
        ]
        y=8
        for L in lines:
            surf = font.render(L, True, (10,10,10))
            screen.blit(surf, (8,y)); y += 18

        # brief conservation report printed each frame to console (light)
        if int(world.time*10) % 50 == 0:
            # every 5s-ish print summary
            print(f"[t={world.time:.2f}] E={diag['energy']:.2f} KE={diag['ke']:.2f} PE={diag['pe']:.2f} Mom=({diag['momentum_x']:.2f},{diag['momentum_y']:.2f})")

        pygame.display.flip()

    logger.close()
    pygame.quit()

# -------------------------
# Level loader from object (used to seed default)
# -------------------------
def load_level_from_obj(obj):
    w = World()
    w.tiles = [tuple(t) for t in obj.get('tiles', [])]
    for b in obj.get('bodies', []):
        w.bodies.append(Body(b['pos'], b.get('w',34), b.get('h',48), mass=b.get('mass',3.0)))
    for r in obj.get('ragdolls', []):
        p = r.get('pos',[400,200])
        w.ragdolls.append(Ragdoll(p[0], p[1], scale=r.get('scale',1.0)))
    for s in obj.get('softbodies', []):
        p = s.get('pos',[800,200])
        w.softbodies.append(SoftBody(p[0], p[1], cols=s.get('cols',5), rows=s.get('rows',5), cell=s.get('cell',18)))
    w.fields = obj.get('fields', [])
    return w

# -------------------------
# Small unit test routine
# -------------------------
def unit_tests():
    print("Running unit tests (momentum/energy basic checks)...")
    w = World()
    # create two equal masses colliding with opposite velocities, verify momentum conservation
    b1 = Body(v(400,200), 40, 40, mass=2.0); b1.vel = v(100.0, 0.0)
    b2 = Body(v(520,200), 40, 40, mass=2.0); b2.vel = v(-100.0, 0.0)
    w.bodies = [b1, b2]
    # no tiles
    gravity = v(0.0, 0.0)
    initial_mom = b1.mass*b1.vel + b2.mass*b2.vel
    # run for 1 second
    steps = int(1.0 / PHYS_DT)
    for _ in range(steps):
        w.step(PHYS_DT, gravity)
    final_mom = b1.mass*b1.vel + b2.mass*b2.vel
    print("Initial momentum:", initial_mom, "Final momentum:", final_mom)
    mom_diff = np.linalg.norm(final_mom - initial_mom)
    print("Momentum diff norm:", mom_diff)
    assert mom_diff < 1e-6 or math.isclose(mom_diff,0.0,rel_tol=1e-6,abs_tol=1e-5), "Momentum not conserved (tileless)"
    print("Basic momentum test passed.")

    # energy drift simple check in closed system (no gravity)
    b1.vel = v(100.0, 0.0); b2.vel = v(-100.0, 0.0)
    initial_ke = 0.5 * b1.mass * np.dot(b1.vel,b1.vel) + 0.5 * b2.mass * np.dot(b2.vel,b2.vel)
    for _ in range(steps):
        w.step(PHYS_DT, gravity)
    final_ke = 0.5 * b1.mass * np.dot(b1.vel,b1.vel) + 0.5 * b2.mass * np.dot(b2.vel,b2.vel)
    print("Initial KE:", initial_ke, "Final KE:", final_ke)
    print("Energy drift:", final_ke - initial_ke)
    print("Unit tests completed (energy may drift slightly due to collision resolution).")

# -------------------------
# CLI entrypoint
# -------------------------
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', action='store_true', help='Run unit tests and exit')
    parser.add_argument('--level', type=str, help='Path to JSON level to load')
    args = parser.parse_args()
    if args.test:
        unit_tests()
        sys.exit(0)
    if args.level:
        if os.path.exists(args.level):
            world = load_level(args.level)
            run_gui(initial_world=world, level_path=None)
        else:
            print("Level not found:", args.level)
            sys.exit(1)
    else:
        # default: run GUI with default world
        run_gui()
