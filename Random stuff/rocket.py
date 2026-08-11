"""
ROCKET FLIGHT
Rocket starts sitting on the surface of Ignis.
Large scrolling solar system with proper Newtonian gravity.

Controls:
    W / UP      thrust
    A / LEFT    rotate left
    D / RIGHT   rotate right
    R           respawn on Ignis
    ESC         quit
"""

try:
    import pygame
except ImportError:
    print("pygame is required:  pip install pygame")
    raise SystemExit(1)

import math, random, sys

# ── window ────────────────────────────────────────────────────────────────────
WIDTH, HEIGHT = 1100, 750
FPS           = 60

# ── physics ───────────────────────────────────────────────────────────────────
# All units: world-pixels and seconds.  G = 1 (folded into masses).
#
# Design targets:
#   orbital speed at r=1800 → 80 px/s    →  GM_sun = 80²·1800 = 11 520 000
#   surface gravity on R=80 planet → 40 px/s²  →  M_planet = 40·80² = 256 000
#   THRUST = 90 px/s²  (beats surface gravity so you can lift off)
#   CRASH_SPEED = 200 px/s (fast enough to feel dangerous, forgiving enough to land)

G           = 1.0
GM_SUN      = 11_520_000          # gives nice 30-80 px/s orbital speeds
THRUST      = 90.0
ROT_SPEED   = 170.0
CRASH_SPEED = 200.0

def _v(r):
    """Circular orbital speed at radius r."""
    return math.sqrt(GM_SUN / r)

def _w(r):
    """Circular orbital angular speed (rad/s) at radius r."""
    return _v(r) / r

def _mp(R, g=40.0):
    """Planet mass that gives surface gravity g for radius R."""
    return g * R * R

# ── colours ───────────────────────────────────────────────────────────────────
BG      = (4,  5, 16)
C_ROCK  = (230, 240, 255)
C_FLAME = [(255,160,30),(255,90,10),(255,220,80)]
C_HUD   = (140, 200, 255)
C_CRASH = (255,  60,  40)
C_TRAIL = ( 50, 100, 190)
C_SUN   = (255, 230,  80)

# ── planet table ──────────────────────────────────────────────────────────────
# (radius, mass, colour, orbit_r, orbit_w rad/s, phase_rad, name)
# Ignis is first — that's the spawn planet.
_P = [
    ( 80, _mp(80),  (210,120, 60),  1_800, _w(1_800),  0.00, "Ignis"),
    ( 50, _mp(50),  (100,180,255),  3_200, _w(3_200),  1.10, "Glacius"),
    ( 36, _mp(36),  (130,220,130),  4_600, _w(4_600),  2.50, "Viridan"),
    ( 65, _mp(65),  (200,155,255),  6_200, _w(6_200),  0.70, "Magna"),
    ( 28, _mp(28),  (255,225, 90),  7_800, _w(7_800),  4.00, "Auros"),
    ( 44, _mp(44),  (180,100, 60),  9_500, _w(9_500),  3.20, "Ruber"),
    ( 24, _mp(24),  (160,210,255), 11_000, _w(11_000), 1.80, "Nebulis"),
]

SUN_R  = 260
SUN_M  = GM_SUN          # G=1 so mass == GM

# ── stars (tiled world-space) ─────────────────────────────────────────────────
_STILE = 2_400
_STARS = [(random.randint(0,_STILE), random.randint(0,_STILE),
           random.choice([1,1,2]), random.uniform(0.35,1.0))
          for _ in range(100)]

def _draw_stars(surf, cx, cy):
    ox = int(cx) % _STILE
    oy = int(cy) % _STILE
    for tx in range(-1, WIDTH//_STILE + 2):
        for ty in range(-1, HEIGHT//_STILE + 2):
            for sx,sy,sr,br in _STARS:
                px = tx*_STILE + sx - ox
                py = ty*_STILE + sy - oy
                if -2 <= px <= WIDTH+2 and -2 <= py <= HEIGHT+2:
                    c = int(br*220)
                    pygame.draw.circle(surf,(c,c,min(c+20,255)),(px,py),sr)

# ── helpers ───────────────────────────────────────────────────────────────────
def s(wx, wy, cx, cy):
    """World → screen coords."""
    return (int(wx - cx + WIDTH//2), int(wy - cy + HEIGHT//2))

# ── Sun ───────────────────────────────────────────────────────────────────────
class Sun:
    def __init__(self, x, y):
        self.x, self.y, self.r, self.mass = x, y, SUN_R, SUN_M

    def draw(self, surf, cx, cy):
        px, py = s(self.x, self.y, cx, cy)
        for i in range(5,0,-1):
            gr = self.r + i*28
            gs = pygame.Surface((gr*2,gr*2), pygame.SRCALPHA)
            pygame.draw.circle(gs,(255,200,50,max(0,12-i*2)),(gr,gr),gr)
            surf.blit(gs,(px-gr,py-gr))
        pygame.draw.circle(surf, C_SUN, (px,py), self.r)
        pygame.draw.circle(surf,(255,255,200),(px-self.r//4,py-self.r//4),self.r//3)

# ── Planet ────────────────────────────────────────────────────────────────────
class Planet:
    def __init__(self, spec, ox, oy):
        self.r, self.mass, self.col, self.orb_r, self.w, self.a, self.name = spec
        self.ox, self.oy = ox, oy
        r,g,b = self.col
        self.hi = (min(r+60,255),min(g+60,255),min(b+60,255))
        self._pos()

    def _pos(self):
        self.x = self.ox + self.orb_r * math.cos(self.a)
        self.y = self.oy + self.orb_r * math.sin(self.a)

    def vel(self):
        v = self.orb_r * self.w
        return (-math.sin(self.a)*v, math.cos(self.a)*v)

    def update(self, dt):
        self.a += self.w * dt
        self._pos()

    def draw(self, surf, cx, cy, font):
        px, py = s(self.x, self.y, cx, cy)
        if not (-self.r*4 < px < WIDTH+self.r*4 and -self.r*4 < py < HEIGHT+self.r*4):
            return
        for i in range(3,0,-1):
            gr = self.r + i*11
            gs = pygame.Surface((gr*2,gr*2), pygame.SRCALPHA)
            pygame.draw.circle(gs,(*self.col, 9-i*3),(gr,gr),gr)
            surf.blit(gs,(px-gr,py-gr))
        pygame.draw.circle(surf, self.col, (px,py), self.r)
        pygame.draw.circle(surf, self.hi, (px-self.r//4,py-self.r//4), self.r//3)
        lbl = font.render(self.name, True,(180,200,220))
        surf.blit(lbl,(px-lbl.get_width()//2, py+self.r+4))

# ── Rocket ────────────────────────────────────────────────────────────────────
_SHAPE = [(0,16),(5,4),(5,-7),(9,-16),(4,-11),(-4,-11),(-9,-16),(-5,-7),(-5,4)]

class Rocket:
    def __init__(self, x, y, angle, vx, vy):
        self.x, self.y   = float(x), float(y)
        self.vx, self.vy = float(vx), float(vy)
        self.ang         = float(angle)   # degrees, 90=up
        self.thrust      = False
        self.alive       = True
        self.crash_t     = 0.0
        self.trail       = []

    def _screen_verts(self, cx, cy):
        rad = math.radians(self.ang)
        ca, sa = math.cos(rad), math.sin(rad)
        px, py = s(self.x, self.y, cx, cy)
        return [(int(px + lx*ca - ly*sa), int(py + lx*sa + ly*ca))
                for lx,ly in _SHAPE]

    def update(self, dt, planets, sun):
        if not self.alive:
            self.crash_t -= dt
            return

        keys = pygame.key.get_pressed()
        if keys[pygame.K_LEFT]  or keys[pygame.K_a]: self.ang += ROT_SPEED*dt
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]: self.ang -= ROT_SPEED*dt
        self.thrust = keys[pygame.K_UP] or keys[pygame.K_w]
        if self.thrust:
            rad = math.radians(self.ang)
            self.vx += math.cos(rad)*THRUST*dt
            self.vy -= math.sin(rad)*THRUST*dt

        # gravity
        for body in [*planets, sun]:
            dx, dy = body.x-self.x, body.y-self.y
            d = math.hypot(dx,dy)
            if d < 1: continue
            f = G * body.mass / (d*d)
            self.vx += dx/d * f * dt
            self.vy += dy/d * f * dt

        self.x += self.vx*dt
        self.y += self.vy*dt

        self.trail.append((self.x, self.y))
        if len(self.trail) > 100: self.trail.pop(0)

        # collisions
        speed = math.hypot(self.vx, self.vy)
        for p in planets:
            d = math.hypot(p.x-self.x, p.y-self.y)
            sd = p.r + 11
            if d < sd:
                if speed > CRASH_SPEED:
                    self.alive, self.crash_t = False, 1.5
                    return
                nx, ny = (self.x-p.x)/d, (self.y-p.y)/d
                dot = self.vx*nx + self.vy*ny
                if dot < 0:
                    self.vx -= 2*dot*nx*0.35
                    self.vy -= 2*dot*ny*0.35
                self.x = p.x + nx*sd
                self.y = p.y + ny*sd
        if math.hypot(sun.x-self.x, sun.y-self.y) < sun.r+8:
            self.alive, self.crash_t = False, 1.5

    def draw(self, surf, cx, cy):
        # trail
        n = len(self.trail)
        if n > 1:
            ts = pygame.Surface((WIDTH,HEIGHT), pygame.SRCALPHA)
            for i in range(1, n):
                a = int(190 * i / n)
                w = 1 + (2 * i // n)
                pygame.draw.line(ts, (*C_TRAIL,a),
                                 s(*self.trail[i-1],cx,cy),
                                 s(*self.trail[i],  cx,cy), w)
            surf.blit(ts,(0,0))

        px, py = s(self.x, self.y, cx, cy)
        if not self.alive:
            if self.crash_t > 0:
                for _ in range(8):
                    pygame.draw.circle(surf, C_CRASH,
                        (px+random.randint(-25,25), py+random.randint(-25,25)),
                        random.randint(2,7))
            return

        verts = self._screen_verts(cx, cy)
        if self.thrust:
            rad = math.radians(self.ang)
            bx, by = px-math.cos(rad)*11, py+math.sin(rad)*11
            fl = random.uniform(16,34)
            tx, ty = bx-math.cos(rad)*fl, by+math.sin(rad)*fl
            for fc in C_FLAME:
                pygame.draw.line(surf,fc,(int(bx),int(by)),(int(tx),int(ty)),
                                 random.randint(3,7))
        pygame.draw.polygon(surf, C_ROCK, verts)
        rad = math.radians(self.ang)
        pygame.draw.circle(surf,(120,200,255),
                           (int(px+math.cos(rad)*7), int(py-math.sin(rad)*7)), 3)

# ── minimap ───────────────────────────────────────────────────────────────────
MS = 185
def draw_map(surf, sun, planets, rocket):
    scale = MS / 24_000
    ox, oy = WIDTH-MS-10, HEIGHT-MS-10
    ms = pygame.Surface((MS,MS), pygame.SRCALPHA)
    ms.fill((5,8,22,200))
    pygame.draw.rect(ms,(40,60,100),(0,0,MS,MS),1)
    def mp(wx,wy):
        return (int((wx-sun.x)*scale+MS//2), int((wy-sun.y)*scale+MS//2))
    for p in planets:
        pygame.draw.circle(ms,(20,32,55),mp(sun.x,sun.y),
                           max(1,int(p.orb_r*scale)),1)
    pygame.draw.circle(ms,C_SUN,mp(sun.x,sun.y),max(3,int(sun.r*scale)))
    for p in planets:
        pygame.draw.circle(ms,p.col,mp(p.x,p.y),max(2,int(p.r*scale)))
    if rocket.alive:
        pygame.draw.circle(ms,(255,255,255),mp(rocket.x,rocket.y),3)
    surf.blit(ms,(ox,oy))

# ── HUD ───────────────────────────────────────────────────────────────────────
def draw_hud(surf, f1, f2, rocket):
    spd = math.hypot(rocket.vx, rocket.vy)
    pygame.draw.rect(surf,(8,12,30),(10,10,240,74),border_radius=8)
    pygame.draw.rect(surf,C_HUD,   (10,10,240,74),1,border_radius=8)
    surf.blit(f1.render(f"SPEED  {spd:7.1f} u/s", True, C_HUD),(20,18))
    if not rocket.alive:
        st, col = "** CRASHED **", C_CRASH
    elif rocket.thrust:
        st, col = "THRUSTING", (255,200,70)
    else:
        st, col = "COASTING", C_HUD
    surf.blit(f1.render(st, True, col),(20,46))
    if not rocket.alive:
        msg = f1.render("PRESS  R  TO RESPAWN", True, C_CRASH)
        surf.blit(msg,(WIDTH//2-msg.get_width()//2, HEIGHT//2))
    tip = f2.render("W/↑ Thrust    A/← D/→ Rotate    R Respawn    ESC Quit",
                    True,(70,95,130))
    surf.blit(tip,(WIDTH//2-tip.get_width()//2, HEIGHT-22))

# ── spawn ─────────────────────────────────────────────────────────────────────
def spawn_on(planet):
    """
    Place rocket on planet surface pointing radially outward.
    Velocity = planet's orbital velocity (so rocket is stationary relative to planet).
    """
    # unit vector from sun to planet = outward normal at spawn point
    nx =  math.cos(planet.a)
    ny =  math.sin(planet.a)
    # position: just outside surface
    rx = planet.x + nx * (planet.r + 12)
    ry = planet.y + ny * (planet.r + 12)
    # nose angle: pointing away from planet (outward), accounting for flipped y
    nose_deg = math.degrees(math.atan2(-ny, nx))
    # velocity: exactly the planet's orbital velocity
    pvx, pvy = planet.vel()
    return Rocket(rx, ry, nose_deg, pvx, pvy)

# ── main ──────────────────────────────────────────────────────────────────────
def main():
    pygame.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    pygame.display.set_caption("ROCKET FLIGHT")
    clock  = pygame.time.Clock()
    f1 = pygame.font.SysFont("Courier New", 18, bold=True)
    f2 = pygame.font.SysFont("Courier New", 13)
    fl = pygame.font.SysFont("Courier New", 11)

    WX, WY   = 12_000, 12_000
    sun      = Sun(WX, WY)
    planets  = [Planet(spec, WX, WY) for spec in _P]
    rocket   = spawn_on(planets[0])
    cam_x, cam_y = rocket.x, rocket.y

    while True:
        dt = min(clock.tick(FPS)/1000.0, 0.04)

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT: pygame.quit(); sys.exit()
            if ev.type == pygame.KEYDOWN:
                if ev.key == pygame.K_ESCAPE: pygame.quit(); sys.exit()
                if ev.key == pygame.K_r: rocket = spawn_on(planets[0])

        for p in planets: p.update(dt)
        rocket.update(dt, planets, sun)

        # smooth camera follow
        cam_x += (rocket.x - cam_x) * 8 * dt
        cam_y += (rocket.y - cam_y) * 8 * dt

        screen.fill(BG)
        _draw_stars(screen, cam_x-WIDTH//2, cam_y-HEIGHT//2)

        # orbit rings
        scx, scy = s(sun.x, sun.y, cam_x, cam_y)
        for p in planets:
            pygame.draw.circle(screen,(18,26,50),(scx,scy), p.orb_r, 1)

        sun.draw(screen, cam_x, cam_y)
        for p in planets: p.draw(screen, cam_x, cam_y, fl)
        rocket.draw(screen, cam_x, cam_y)
        draw_hud(screen, f1, f2, rocket)
        draw_map(screen, sun, planets, rocket)
        pygame.display.flip()

if __name__ == "__main__":
    main()
