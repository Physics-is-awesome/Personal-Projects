"""
ROCKET FLIGHT  —  large scrolling solar system, physically correct.

Local-space convention (rocket body):
    +Y  = nose direction
    -Y  = tail / thruster direction
    +X  = right wing

World / screen convention:
    +x  = rightward on screen
    +y  = downward on screen   (standard pygame)

Local → screen transform (VERIFIED):
    screen_dx = -lx*sa + ly*ca
    screen_dy = -lx*ca - ly*sa
    where ca=cos(ang), sa=sin(ang), ang in degrees.

    ang=  0°  →  nose points RIGHT  (screen +x)
    ang= 90°  →  nose points UP     (screen -y)
    ang=180°  →  nose points LEFT   (screen -x)
    ang=270°  →  nose points DOWN   (screen +y)

Thrust formula (VERIFIED):
    vx += ca * THRUST * dt
    vy -= sa * THRUST * dt
    (pushes rocket in nose direction; ang=90° → vy decreases = moves up ✓)

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
# G=1 (folded into masses).
# GM_SUN designed so inner planet at r=4000 has orbital speed 100 px/s:
#   v² = GM/r  →  GM = v²·r = 100²·4000 = 40_000_000
# Surface gravity: g = G·M_planet / R²  →  M_planet = g·R²
# We use g_surface = 20 px/s² for all planets.
# THRUST = 50 px/s²  (more than gravity so you can definitely lift off,
#          but not so much that it's trivially easy)

G              = 30.0
GM_SUN         = 40_000_000
G_SURFACE      = 40.0        # px/s² at planet surface
THRUST         = 50.0        # px/s²
ROT_SPEED      = 130.0       # deg/s  (deliberate, weighty)
DRAG           = 0.00000010      # fractional velocity loss per frame (tiny)
CRASH_SPEED    = 180.0       # px/s  — impact speed that destroys rocket
RESTITUTION    = 0.28        # bounciness on planet surface

def _w(r):
    """Circular orbital angular speed at radius r."""
    return math.sqrt(GM_SUN) / r**1.5 * r   # = sqrt(GM/r) / r = sqrt(GM/r³)... 
    # Correct: omega = v/r = sqrt(GM/r)/r = sqrt(GM)/r^(3/2)

# Recalculate cleanly:
def _omega(r):
    return math.sqrt(GM_SUN / r) / r   # = sqrt(GM/r³) ... same thing

def _mplanet(R):
    """Planet mass given surface radius R and desired surface gravity."""
    return G_SURFACE * R * R / G

# ── colours ───────────────────────────────────────────────────────────────────
BG        = (3,  4, 14)
C_BODY    = (215, 228, 245)
C_ACCENT  = ( 70, 130, 210)
C_NOZZLE  = ( 90, 100, 120)
C_WINDOW  = (150, 220, 255)
# Flame layers: core (white-hot) → middle → outer → faint
C_FLAME   = [(255,255,220),(255,200, 60),(255,100, 15),(180, 40,  0)]
C_HUD     = (130, 195, 255)
C_CRASH   = (255,  55,  35)
C_TRAIL   = ( 40,  80, 170)
C_SUN     = (255, 235,  80)
BG_PANEL  = (  6,  9,  24, 210)

# ── planet table ──────────────────────────────────────────────────────────────
# (radius, mass, colour, orbit_r, omega rad/s, phase rad, name)
# Ignis is [0] — spawn planet.
_PLANETS = [
    (100, _mplanet(100), (210, 115, 55),  4_000, _omega( 4_000), 0.00, "Ignis"),
    ( 65, _mplanet( 65), ( 90, 165,255),  7_000, _omega( 7_000), 1.10, "Glacius"),
    ( 50, _mplanet( 50), (120, 215,120), 11_000, _omega(11_000), 2.50, "Viridan"),
    ( 82, _mplanet( 82), (195, 145,255), 16_000, _omega(16_000), 0.70, "Magna"),
    ( 36, _mplanet( 36), (255, 220, 80), 22_000, _omega(22_000), 4.00, "Auros"),
    ( 55, _mplanet( 55), (175,  95, 55), 29_000, _omega(29_000), 3.20, "Ruber"),
    ( 30, _mplanet( 30), (155, 205,255), 37_000, _omega(37_000), 1.80, "Nebulis"),
]

SUN_R = 600
SUN_M = GM_SUN / G

# ── rocket shape  (local-space, origin = centre of mass) ─────────────────────
#
#  Nose tip:        (0,  22)   ← local +Y
#  Nozzle centre:   (0, -22)   ← local -Y
#  The rocket is 44 px tall in local space, centred at (0,0).
#
#  Main body polygon:
_BODY_PTS  = [(0,22),(6,12),(6,-10),(5,-22),(-5,-22),(-6,-10),(-6,12)]
#  Right fin:
_FIN_R_PTS = [(6,-8),(18,-22),(5,-22)]
#  Left fin:
_FIN_L_PTS = [(-6,-8),(-18,-22),(-5,-22)]
#  Nozzle bell (drawn as thick line across tail):
_NOZZLE_L  = (-7, -22)
_NOZZLE_R  = ( 7, -22)
#  Flame base points (just inside nozzle):
_FLAME_L   = (-5, -22)
_FLAME_R   = ( 5, -22)
_FLAME_C   = ( 0, -22)
#  Cockpit window centre:
_WIN_C     = (0, 12)
_WIN_R     = 4
#  Centre-of-mass in local space — must be (0,0) for the transform to work:
_COM       = (0,  0)

# ── star field (tiled in world space) ─────────────────────────────────────────
_STILE = 3_500
_STARS = [(random.randint(0,_STILE), random.randint(0,_STILE),
           random.choice([1,1,1,2]), random.uniform(0.3, 1.0))
          for _ in range(130)]

def _draw_stars(surf, cam_x, cam_y):
    """Draw tiled star field scrolling with camera."""
    ox = int(cam_x) % _STILE
    oy = int(cam_y) % _STILE
    for tx in range(-1, WIDTH//_STILE + 2):
        for ty in range(-1, HEIGHT//_STILE + 2):
            for sx, sy, sr, br in _STARS:
                px = tx*_STILE + sx - ox
                py = ty*_STILE + sy - oy
                if -3 <= px <= WIDTH+3 and -3 <= py <= HEIGHT+3:
                    c = int(br * 215)
                    pygame.draw.circle(surf, (c, c, min(c+25,255)), (px,py), sr)

# ── coordinate helpers ────────────────────────────────────────────────────────
def w2s(wx, wy, cx, cy):
    """World → screen pixel (integer tuple)."""
    return (int(wx - cx + WIDTH//2), int(wy - cy + HEIGHT//2))

def l2s(lx, ly, px, py, ca, sa):
    """
    Local-space point → screen pixel.
    px, py  : screen position of rocket centre-of-mass
    ca, sa  : cos/sin of rocket angle
    Transform (derived & verified above):
        screen_dx = -lx*sa + ly*ca
        screen_dy = -lx*ca - ly*sa
    """
    return (int(px - lx*sa + ly*ca),
            int(py - lx*ca - ly*sa))

def l2s_poly(pts, px, py, ca, sa):
    return [l2s(lx, ly, px, py, ca, sa) for lx, ly in pts]

# ── Sun ───────────────────────────────────────────────────────────────────────
class Sun:
    def __init__(self, x, y):
        self.x, self.y = float(x), float(y)
        self.r         = SUN_R
        self.mass      = SUN_M

    def draw(self, surf, cx, cy):
        px, py = w2s(self.x, self.y, cx, cy)
        # corona
        for i in range(6, 0, -1):
            gr = self.r + i*35
            gs = pygame.Surface((gr*2, gr*2), pygame.SRCALPHA)
            pygame.draw.circle(gs, (255,200,50, max(0,10-i*2)), (gr,gr), gr)
            surf.blit(gs, (px-gr, py-gr))
        pygame.draw.circle(surf, C_SUN, (px,py), self.r)
        pygame.draw.circle(surf, (255,255,210),
                           (px-self.r//4, py-self.r//4), self.r//3)

# ── Planet ────────────────────────────────────────────────────────────────────
class Planet:
    def __init__(self, spec, ox, oy):
        self.r, self.mass, self.col, self.orb_r, self.omega, self.a, self.name = spec
        self.ox, self.oy = ox, oy
        r,g,b = self.col
        self.hi = (min(r+55,255), min(g+55,255), min(b+55,255))
        self._update()

    def _update(self):
        self.x = self.ox + self.orb_r * math.cos(self.a)
        self.y = self.oy + self.orb_r * math.sin(self.a)

    def vel(self):
        v = math.sqrt(GM_SUN / self.orb_r)
        return (-math.sin(self.a)*v, math.cos(self.a)*v)

    def update(self, dt):
        self.a += self.omega * dt
        self._update()

    def draw(self, surf, cx, cy, font):
        px, py = w2s(self.x, self.y, cx, cy)
        r = self.r
        if not (-r*5 < px < WIDTH+r*5 and -r*5 < py < HEIGHT+r*5):
            return
        for i in range(3, 0, -1):
            gr = r + i*12
            gs = pygame.Surface((gr*2,gr*2), pygame.SRCALPHA)
            pygame.draw.circle(gs, (*self.col, 8-i*2), (gr,gr), gr)
            surf.blit(gs, (px-gr, py-gr))
        pygame.draw.circle(surf, self.col, (px,py), r)
        pygame.draw.circle(surf, self.hi,  (px-r//4, py-r//4), r//3)
        lbl = font.render(self.name, True, (175,198,220))
        surf.blit(lbl, (px-lbl.get_width()//2, py+r+5))

# ── Rocket ────────────────────────────────────────────────────────────────────
class Rocket:
    def __init__(self, x, y, angle, vx, vy):
        self.x, self.y   = float(x), float(y)
        self.vx, self.vy = float(vx), float(vy)
        self.ang         = float(angle)  # degrees; 90° = nose UP
        self.thrusting   = False
        self.alive       = True
        self.crash_t     = 0.0
        self.trail       = []
        self._t          = 0.0           # time accumulator for flame animation

    def update(self, dt, planets, sun):
        if not self.alive:
            self.crash_t -= dt
            return

        self._t += dt
        keys = pygame.key.get_pressed()

        # rotation
        if keys[pygame.K_LEFT]  or keys[pygame.K_a]:  self.ang += ROT_SPEED * dt
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]:  self.ang -= ROT_SPEED * dt

        # thrust in nose direction — VERIFIED formula
        self.thrusting = keys[pygame.K_UP] or keys[pygame.K_w]
        if self.thrusting:
            rad = math.radians(self.ang)
            ca, sa = math.cos(rad), math.sin(rad)
            self.vx += ca * THRUST * dt
            self.vy -= sa * THRUST * dt   # minus because screen-y is flipped

        # tiny drag
        self.vx *= (1.0 - DRAG)
        self.vy *= (1.0 - DRAG)

        # gravity from every body
        for body in [*planets, sun]:
            dx = body.x - self.x
            dy = body.y - self.y
            d  = math.hypot(dx, dy)
            if d < 1.0:
                continue
            f = G * body.mass / (d * d)
            self.vx += (dx/d) * f * dt
            self.vy += (dy/d) * f * dt

        # integrate position
        self.x += self.vx * dt
        self.y += self.vy * dt

        # record trail (world-space)
        self.trail.append((self.x, self.y))
        if len(self.trail) > 140:
            self.trail.pop(0)

        # ── collisions ───────────────────────────────────────────────────────
        speed = math.hypot(self.vx, self.vy)
        for p in planets:
            d  = math.hypot(p.x-self.x, p.y-self.y)
            sd = p.r + 12          # surface distance (rocket half-height ≈12)
            if d < sd:
                if speed > CRASH_SPEED:
                    self.alive   = False
                    self.crash_t = 2.0
                    return
                # surface normal (outward from planet)
                nx = (self.x - p.x) / d
                ny = (self.y - p.y) / d
                # velocity relative to planet surface
                pvx, pvy = p.vel()
                rvx = self.vx - pvx
                rvy = self.vy - pvy
                # normal component of relative velocity
                vn = rvx*nx + rvy*ny
                if vn < 0:    # approaching
                    # impulse: reflect normal component with restitution
                    self.vx -= (1.0 + RESTITUTION) * vn * nx
                    self.vy -= (1.0 + RESTITUTION) * vn * ny
                # push out of surface
                self.x = p.x + nx * sd
                self.y = p.y + ny * sd

        if math.hypot(sun.x-self.x, sun.y-self.y) < sun.r + 12:
            self.alive   = False
            self.crash_t = 2.0

    def draw(self, surf, cx, cy):
        # ── trail ────────────────────────────────────────────────────────────
        n = len(self.trail)
        if n > 1:
            trail_surf = pygame.Surface((WIDTH,HEIGHT), pygame.SRCALPHA)
            for i in range(1, n):
                frac = i / n
                a    = int(170 * frac)
                w    = 1 + int(2 * frac)
                pygame.draw.line(trail_surf, (*C_TRAIL, a),
                                 w2s(*self.trail[i-1], cx, cy),
                                 w2s(*self.trail[i],   cx, cy), w)
            surf.blit(trail_surf, (0,0))

        # screen position of rocket origin (= centre of mass)
        px, py = w2s(self.x, self.y, cx, cy)

        # ── crash debris ─────────────────────────────────────────────────────
        if not self.alive:
            if self.crash_t > 0:
                rng = random.Random(int(self.crash_t * 40))
                for _ in range(12):
                    ox = rng.randint(-35,35)
                    oy = rng.randint(-35,35)
                    pygame.draw.circle(surf, C_CRASH, (px+ox,py+oy),
                                       rng.randint(2,9))
            return

        rad = math.radians(self.ang)
        ca, sa = math.cos(rad), math.sin(rad)

        # ── engine flame (from tail, pointing in -Y local = tail direction) ──
        if self.thrusting:
            t = self._t
            # Tail direction in screen space: local (0,-1):
            #   screen_dx = -0*sa + (-1)*ca = -ca
            #   screen_dy = -0*ca - (-1)*sa =  sa
            # So tail unit vector on screen: (-ca, sa)
            tail_dx = -ca
            tail_dy =  sa

            # Lateral unit vector (local +X = right wing):
            # local (1,0): screen_dx = -1*sa + 0*ca = -sa,  screen_dy = -1*ca - 0 = -ca
            lat_dx = -sa
            lat_dy = -ca

            for layer in range(4):
                wobble = math.sin(t * 15.0 + layer*1.3) * (1.5 + layer*2.0)
                length = random.uniform(14 + layer*7, 22 + layer*9)
                tip_x  = int(px + _FLAME_C[0]*(-sa) + _FLAME_C[1]*ca
                             + tail_dx*length + lat_dx*wobble)
                tip_y  = int(py + _FLAME_C[0]*(-ca) + _FLAME_C[1]*(-sa)
                             - tail_dy*length - lat_dy*wobble)
                # wait — let me compute flame base properly:
                # _FLAME_C = (0,-22), so:
                #   base_x = px + (-0*sa + (-22)*ca) = px - 22*ca
                #   base_y = py + (-0*ca - (-22)*sa) = py + 22*sa
                base_x = int(px - 22*ca)
                base_y = int(py + 22*sa)
                tip_x  = int(base_x + tail_dx*length + lat_dx*wobble)
                tip_y  = int(base_y + tail_dy*length + lat_dy*wobble)
                col    = C_FLAME[layer]
                wid    = max(1, 8 - layer*2)

                # left nozzle edge → tip
                lx_b = int(px - 5*sa - 22*ca)
                ly_b = int(py - 5*ca + 22*sa)
                rx_b = int(px + 5*sa - 22*ca)
                ry_b = int(py + 5*ca + 22*sa)
                pygame.draw.line(surf, col, (lx_b,ly_b), (tip_x,tip_y), wid)
                pygame.draw.line(surf, col, (rx_b,ry_b), (tip_x,tip_y), wid)
                if layer == 0:
                    pygame.draw.line(surf, col, (base_x,base_y), (tip_x,tip_y), wid+2)

        # ── rocket body ───────────────────────────────────────────────────────
        fins_r = l2s_poly(_FIN_R_PTS, px, py, ca, sa)
        fins_l = l2s_poly(_FIN_L_PTS, px, py, ca, sa)
        body   = l2s_poly(_BODY_PTS,  px, py, ca, sa)

        pygame.draw.polygon(surf, C_ACCENT, fins_r)
        pygame.draw.polygon(surf, C_ACCENT, fins_l)
        pygame.draw.polygon(surf, C_BODY,   body)

        # nozzle bell
        nl = l2s(*_NOZZLE_L, px, py, ca, sa)
        nr = l2s(*_NOZZLE_R, px, py, ca, sa)
        pygame.draw.line(surf, C_NOZZLE, nl, nr, 3)

        # cockpit window
        wx, wy = l2s(*_WIN_C, px, py, ca, sa)
        pygame.draw.circle(surf, C_ACCENT,  (wx,wy), _WIN_R+2)
        pygame.draw.circle(surf, C_WINDOW,  (wx,wy), _WIN_R)

        # debug dot at origin (centre of mass) — remove after confirming alignment:
        # pygame.draw.circle(surf, (255,0,0), (px,py), 3)

# ── minimap ───────────────────────────────────────────────────────────────────
MS = 190
def draw_map(surf, sun, planets, rocket, font):
    scale = MS / 80_000      # fits the full solar system (outermost orbit ~37k radius)
    ox, oy = WIDTH-MS-10, HEIGHT-MS-10
    ms = pygame.Surface((MS,MS), pygame.SRCALPHA)
    ms.fill((4,6,18,210))
    pygame.draw.rect(ms,(35,55,95),(0,0,MS,MS),1)

    def mp(wx, wy):
        return (int((wx-sun.x)*scale + MS//2),
                int((wy-sun.y)*scale + MS//2))

    for p in planets:
        pygame.draw.circle(ms,(18,28,50), mp(sun.x,sun.y),
                           max(1,int(p.orb_r*scale)), 1)
    pygame.draw.circle(ms, C_SUN, mp(sun.x,sun.y), max(3,int(sun.r*scale)))
    for p in planets:
        pr = max(2, int(p.r*scale))
        pygame.draw.circle(ms, p.col, mp(p.x,p.y), pr)
    if rocket.alive:
        pygame.draw.circle(ms,(255,255,255), mp(rocket.x,rocket.y), 3)

    surf.blit(ms,(ox,oy))
    # label
    lbl = font.render("MAP", True, (70,95,130))
    surf.blit(lbl,(ox+4, oy+3))

# ── HUD ───────────────────────────────────────────────────────────────────────
def draw_hud(surf, f1, f2, rocket):
    spd = math.hypot(rocket.vx, rocket.vy)
    panel = pygame.Surface((255,80), pygame.SRCALPHA)
    panel.fill(BG_PANEL)
    surf.blit(panel,(10,10))
    pygame.draw.rect(surf, C_HUD, (10,10,255,80), 1, border_radius=6)

    surf.blit(f1.render(f"SPEED  {spd:6.1f}  u/s", True, C_HUD),(20,18))
    if not rocket.alive:
        st, col = "** CRASHED **", C_CRASH
    elif rocket.thrusting:
        st, col = "THRUSTING", (255,200,65)
    else:
        st, col = "COASTING",  C_HUD
    surf.blit(f1.render(st, True, col),(20,48))

    if not rocket.alive:
        msg = f1.render("PRESS  R  TO RESPAWN", True, C_CRASH)
        surf.blit(msg,(WIDTH//2-msg.get_width()//2, HEIGHT//2+10))

    tip = f2.render("W/↑ Thrust    A/← D/→ Rotate    R Respawn    ESC Quit",
                    True,(65,90,125))
    surf.blit(tip,(WIDTH//2-tip.get_width()//2, HEIGHT-22))

# ── spawn on planet surface ───────────────────────────────────────────────────
def spawn_on(planet):
    """
    Place rocket exactly on planet surface, nose pointing radially outward,
    velocity = planet's current orbital velocity (rocket is stationary on surface).
    """
    # outward unit vector from orbit centre to planet
    nx = math.cos(planet.a)
    ny = math.sin(planet.a)
    # position: planet centre + outward * (planet radius + rocket half-height)
    # rocket half-height in local space = 22 (nose at +22, tail at -22)
    # COM is at local (0,0), so half-height from COM to tail = 22.
    # We want tail just touching surface, so COM is 22 px above surface:
    dist = planet.r + 22
    rx = planet.x + nx * dist
    ry = planet.y + ny * dist

    # nose angle: we want nose to point outward = away from planet = (nx, ny) world.
    # Thrust: vx += cos(ang)*T, vy -= sin(ang)*T → nose world = (cos(ang), -sin(ang)).
    # We want (cos(ang), -sin(ang)) = (nx, ny):
    #   cos(ang) = nx,  -sin(ang) = ny  →  sin(ang) = -ny
    #   ang = atan2(-ny, nx)
    nose_deg = math.degrees(math.atan2(-ny, nx))

    # velocity = planet's orbital velocity
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

    # Solar system centred at (WX, WY) in world space
    WX, WY  = 40_000, 40_000
    sun     = Sun(WX, WY)
    planets = [Planet(spec, WX, WY) for spec in _PLANETS]

    rocket  = spawn_on(planets[0])
    cam_x   = rocket.x
    cam_y   = rocket.y

    while True:
        dt = min(clock.tick(FPS) / 1000.0, 0.04)

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pygame.quit(); sys.exit()
            if ev.type == pygame.KEYDOWN:
                if ev.key == pygame.K_ESCAPE:
                    pygame.quit(); sys.exit()
                if ev.key == pygame.K_r:
                    rocket = spawn_on(planets[0])

        for p in planets:
            p.update(dt)
        rocket.update(dt, planets, sun)

        # smooth camera follow
        cam_x += (rocket.x - cam_x) * 7.0 * dt
        cam_y += (rocket.y - cam_y) * 7.0 * dt

        # ── draw ─────────────────────────────────────────────────────────────
        screen.fill(BG)
        _draw_stars(screen, cam_x - WIDTH//2, cam_y - HEIGHT//2)

        # orbit rings
        scx, scy = w2s(sun.x, sun.y, cam_x, cam_y)
        for p in planets:
            pygame.draw.circle(screen, (16,24,45), (scx,scy), p.orb_r, 1)

        sun.draw(screen, cam_x, cam_y)
        for p in planets:
            p.draw(screen, cam_x, cam_y, fl)
        rocket.draw(screen, cam_x, cam_y)
        draw_hud(screen, f1, f2, rocket)
        draw_map(screen, sun, planets, rocket, fl)

        pygame.display.flip()

if __name__ == "__main__":
    main()
