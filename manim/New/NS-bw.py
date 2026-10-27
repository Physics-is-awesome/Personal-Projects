#!/usr/bin/env python3

"""
ns_blowup_scenes.py
================================================================================
Manim animation for "Simulating a Navier-Stokes blow-up: a manufactured-solution
toy of OpenAI's finite-time singularity", synchronized to the narration track
(NS_blow-up.srt) and following the LaTeX script section by section.

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh ns_blowup_scenes.py N0
    ...                       or: ./render_all.sh NS_blow-up.wav   (renders all, joins, adds audio)

Scene map (script section -> scene -> narration cues in NS_blow-up.srt)
    N0     title, "I won't go into any of that"                cues   1-  5
    N1     Why simulate?  curiosity                              cues   6- 13
    N2     The Problem: the equations, the difficulty            cues  14- 19
    N2_1   finite-time singularity                               cues  20- 26
    N3     The Flow: the vortex picture, tau = 1 - t             cues  27- 32
    N3_1   scaling laws of the core                              cues  33- 41
    N4     key idea: force := residual                           cues  42- 46
    N4_1   how the paper cancels the divergence, in layers       cues  47- 54
    N4_2   what I can actually do: a manufactured solution       cues  55- 63
    N5     Model: axisymmetry, (u1, omega1, psi1)                cues  64- 70
    N5_1   the three equations and the exact toy flow            cues  71- 74
    N6     Conditions: switch-on, domain, boundaries             cues  75- 80
    N6_1   Numerics: grid, flux form, Cholesky, RK4              cues  81- 88
    N7     Validation 1 + 2: residual check, convergence         cues  89- 96
    N7_1   Validation 3: the exponents                           cues  97-103
    N7_2   Validation: the picture (similarity collapse)         cues 104-107
    N8     Results: four extra views, and the warning            cues 108-112
    N8_1   3D: spin the grid, 45% swirl surface, quarter cut     cues 113-118   (3D)
    N8_2   3D: physical vs similarity coordinates                cues 119-124   (3D)
    N9     tracer particles                                      cues 125-135
    N10    term balance                                          cues 136-152
    N11    switch the force off                                  cues 153-164
    N12    limits: the code fails, and that's fine               cues 165-168

--------------------------------------------------------------------------
AUDIO SYNC (identical machinery to metriplectic_1.py)
--------------------------------------------------------------------------
* Every scene has AUDIO_START / AUDIO_END = the absolute narration time at
  which the clip begins / ends in the final video.  Clips are laid end to end,
  so scene k ends exactly where scene k+1 starts.
* self.sync(t)       waits until the clip's own clock equals narration time t.
* self.at(n, *anims) = "sync to the START of cue n, then play".
* self.frac(n, f, *anims) = "sync to a fraction f (0..1) of cue n's duration,
  then play"  -- handy for the long cues that contain several beats.
* self.finish() fades the scene out *before* AUDIO_END and then syncs to
  AUDIO_END, so every rendered clip is exactly as long as its slice of audio.
* If an animation overruns its slot, sync() cannot rewind; it records the beat
  and tear_down() prints a "late beats" report, e.g.
      [sync] N10: clip 80.40s / audio 80.40s | late beats: cue 145 (+0.35s)
  Fix a late beat by shortening a run_time or moving the cue earlier.

Debug aid: set SNAP=1 and every self.snap("name") call writes
snaps/<scene>_<name>.png so layouts can be checked without watching.

--------------------------------------------------------------------------
WHERE THE DATA COMES FROM  (nothing here is hand-drawn "to look right")
--------------------------------------------------------------------------
The toy flow is the closed-form manufactured solution of the script,

    u1  = 8 tau^(-1-h)       exp(-rho^2 - zeta^2)
    psi1= 5 tau^(-1/2-h) zeta exp(-rho^2 - zeta^2)
    rho = r/tau^(1/2),  zeta = z/tau^(1/2-h),  tau = 1 - t,  h = 0.01

times the switch-on s(t).  Every field, streamline, tracer and term-balance
number in the animation is evaluated from it (Flow, tracers(), TERM_TABLE).
Checked independently with sympy: peak swirl 6.34 / 11.10 / 17.72 at
tau = 0.3 / 0.1 / 0.04 and 25.2 at t = 0.98; term maxima / tau^(-3/2-h) =
2.38 / 19.5 / 28.4 / 19.7 (acc / nonlinear / viscous / force), force/largest
= 0.69 -- i.e. the numbers you read out in the narration.  Two things that the
script does not pin down are ASSUMPTIONS:  the exact shape of the switch-on
ramp (a C2 smootherstep on [0.3, 0.5] is used) and the tracer seed radii.
Items that need the real PDE solver (force-off decay, convergence errors,
fitted exponents) use the numbers you quote, and say "schematic" when the
curve between two measured points is only a guide for the eye.

Theme: uses your emacs_theme module when importable (BG, FG, FG_DIM,
code_line, fit_width, starfield, BufferChrome).  If it is missing, a small
look-alike fallback is defined so the file still renders.
================================================================================
"""

import os
import re
import math
from collections import namedtuple
from functools import lru_cache
from pathlib import Path

import numpy as np
from manim import *

try:
    from emacs_theme import *  # noqa: F401,F403
    THEME = "emacs_theme"
except ImportError:  # ------------------------------------------------ fallback
    THEME = "fallback"
    BG, FG, FG_DIM, FONT = "#15171c", "#d9dce3", "#7a8294", "Monospace"

    def code_line(parts, font_size=28):
        items = [Text(s, font=FONT, color=c, font_size=font_size) for s, c in parts]
        return VGroup(*items).arrange(RIGHT, buff=0.06, aligned_edge=DOWN)

    def fit_width(m, max_width=None):
        max_width = max_width or (config.frame_width - 1.6)
        if m.width > max_width:
            m.set_width(max_width)
        return m

    def starfield(n=50, seed=0):
        rng = np.random.default_rng(seed)
        dots = VGroup()
        for _ in range(n):
            p = np.array([rng.uniform(-7, 7), rng.uniform(-4, 4), 0])
            dots.add(Dot(p, radius=float(rng.uniform(0.008, 0.025)),
                         color=WHITE).set_opacity(float(rng.uniform(0.15, 0.6))))
        return dots

    class BufferChrome(VGroup):
        def __init__(self, filename="x.tex", mode="LaTeX", position="0%", tab=None):
            super().__init__()
            w, h = config.frame_width, config.frame_height
            top = Rectangle(width=w, height=0.45, stroke_width=0, fill_color="#0d0f13",
                            fill_opacity=1).move_to([0, h / 2 - 0.225, 0])
            bot = Rectangle(width=w, height=0.45, stroke_width=0, fill_color="#0d0f13",
                            fill_opacity=1).move_to([0, -h / 2 + 0.225, 0])
            tl = Text(tab or filename, font=FONT, font_size=16, color=FG_DIM)
            tl.move_to(top).to_edge(LEFT, buff=0.4)
            bl = Text(f"-:---  {filename}    {position}    ({mode})", font=FONT,
                      font_size=15, color=FG_DIM)
            bl.move_to(bot).to_edge(LEFT, buff=0.4)
            self.add(top, bot, tl, bl)

# Our own TeX template (independent of the theme module) -- amsmath + amssymb.
_TPL = TexTemplate()
_TPL.add_to_preamble(r"\usepackage{amsmath,amssymb}")
MathTex.set_default(tex_template=_TPL)
Tex.set_default(tex_template=_TPL)

# --------------------------------------------------------------------------
# Layout constants (clear of the chrome bars)
# --------------------------------------------------------------------------
TOP_Y = config.frame_height / 2 - 0.9
BOT_Y = -config.frame_height / 2 + 0.75
LEFT_X = -config.frame_width / 2 + 0.8
RIGHT_X = config.frame_width / 2 - 0.8
CONTENT_MAX_WIDTH = RIGHT_X - LEFT_X - 0.4

# Colour roles used consistently through the video
C_SWIRL = ORANGE      # swirl / u_theta / angular momentum
C_AXIAL = RED         # flow up the axis (and "force")
C_DOWN = BLUE         # flow down the axis
C_RAD = TEAL          # radial inflow
C_FORCE = "#e2587a"   # the forcing
C_VISC = GREEN        # viscosity
C_NL = YELLOW         # nonlinear / advection
C_ACC = GREY_B        # acceleration

# --------------------------------------------------------------------------
# Narration timing
# --------------------------------------------------------------------------
Cue = namedtuple("Cue", "start end text")


def _find_srt():
    cands = [os.environ.get("NS_BLOWUP_SRT")]
    try:
        cands.append(Path(__file__).with_name("NS_blow-up.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "NS_blow-up.srt",
              Path("~/Music/NS_blow-up.srt").expanduser(),
              Path("/mnt/user-data/uploads/NS_blow-up.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError("NS_blow-up.srt not found (set NS_BLOWUP_SRT=/path/to/file)")


def _load_cues(path):
    block = re.compile(
        r"(\d+)\s*\n(\d\d):(\d\d):(\d\d),(\d\d\d)\s*-->\s*(\d\d):(\d\d):(\d\d),(\d\d\d)"
        r"\s*\n(.*?)(?=\n\s*\n|\Z)", re.S)
    sec = lambda h, m, s, ms: int(h) * 3600 + int(m) * 60 + int(s) + int(ms) / 1000
    cues = {}
    text = Path(path).read_text(encoding="utf-8")
    for m in block.finditer(text.strip() + "\n\n"):
        g = m.groups()
        cues[int(g[0])] = Cue(sec(*g[1:5]), sec(*g[5:9]), " ".join(g[9].split()))
    return cues


CUES = _load_cues(_find_srt())
TOTAL_DURATION = max(c.end for c in CUES.values())

# scene name -> first cue.  A scene runs from the start of its first cue to the
# start of the next scene's first cue (N0 starts at t = 0).
PLAN = [("N0", 1), ("N1", 6), ("N2", 14), ("N2_1", 20), ("N3", 27), ("N3_1", 33),
        ("N4", 42), ("N4_1", 47), ("N4_2", 55), ("N5", 64), ("N5_1", 71),
        ("N6", 75), ("N6_1", 81), ("N7", 89), ("N7_1", 97), ("N7_2", 104),
        ("N8", 108), ("N8_1", 113), ("N8_2", 119), ("N9", 125), ("N10", 136),
        ("N11", 153), ("N12", 165)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="ns_blowup.tex"):
    return BufferChrome(filename=tab, mode="LaTeX/Preview", position=position, tab=tab)


def audio_pct(t):
    return f"{round(100 * t / TOTAL_DURATION)}%"


def place_header(h, max_width=None):
    fit_width(h, max_width)
    h.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
    return h


def clamp_width(m, max_width=CONTENT_MAX_WIDTH):
    if m.width > max_width:
        m.set_width(max_width)
    return m


class _SyncMixin:
    """Cue-synchronised timing, shared by the 2D and 3D scene classes."""
    AUDIO_START = 0.0
    AUDIO_END = None
    STARS = None            # (count, seed) -> starfield backdrop

    # -- lifecycle ---------------------------------------------------------
    def _setup_sync(self):
        self.camera.background_color = BG
        self._late, self._persist = [], []
        if self.STARS:
            stars = starfield(*self.STARS)
            self.add(stars)
            self._persist.append(stars)
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self._add_chrome(chrome)
        self._persist.append(chrome)

    def _add_chrome(self, chrome):
        self.add(chrome)

    def _report(self):
        span = (self.AUDIO_END - self.AUDIO_START) if self.AUDIO_END else self.time
        late = ", ".join(f"cue {c} (+{d:.2f}s)" for c, d in self._late) or "none"
        print(f"[sync] {type(self).__name__}: clip {self.time:.2f}s / audio {span:.2f}s "
              f"| late beats: {late}")

    # -- timing ------------------------------------------------------------
    def sync(self, t, tag=None):
        dt = (t - self.AUDIO_START) - self.time
        if dt > 1e-6:
            self.wait(dt)
        elif dt < -0.12 and tag is not None:
            self._late.append((tag, -dt))

    def at(self, n, *anims, offset=0.0, **kw):
        """Wait for the start of cue n (+offset) then play anims."""
        self.sync(CUES[n].start + offset, tag=n)
        if anims:
            self.play(*anims, **kw)

    def frac(self, n, f, *anims, **kw):
        """Wait for a fraction f (0..1) of cue n's duration, then play anims."""
        c = CUES[n]
        self.sync(c.start + f * (c.end - c.start), tag=f"{n}@{f:g}")
        if anims:
            self.play(*anims, **kw)

    def finish(self, fade=0.4, keep=()):
        """Fade everything out except persistent chrome and `keep` (e.g. the
        section header, so the next clip of the same section can start with it
        already on screen)."""
        t_end = self.AUDIO_END
        self.sync(t_end - fade - 0.05)
        objs = [m for m in self.mobjects
                if m not in self._persist and m not in keep and not isinstance(m, ValueTracker)]
        for o in objs:
            for sub in o.get_family():
                sub.clear_updaters()
        if objs:
            self.play(FadeOut(*objs), run_time=fade)
        self.sync(t_end)

    def snap(self, label):
        if os.environ.get("SNAP"):
            os.makedirs("snaps", exist_ok=True)
            self.renderer.update_frame(self)
            self.renderer.camera.get_image().save(f"snaps/{type(self).__name__}_{label}.png")


class SyncedScene(_SyncMixin, Scene):
    def setup(self):
        super().setup()
        self._setup_sync()

    def tear_down(self):
        super().tear_down()
        self._report()


class Synced3D(_SyncMixin, ThreeDScene):
    """3D variant.  Everything that is a HUD element (header, labels, chrome)
    must be registered with self.hud(...) so it stays flat on the screen."""

    def setup(self):
        super().setup()
        self._setup_sync()

    def _add_chrome(self, chrome):
        self.add_fixed_in_frame_mobjects(chrome)

    def hud(self, *mobs, show=False):
        """Register flat-on-screen mobjects.  show=False leaves them off screen
        so that a later self.play(FadeIn(m)) brings them in."""
        self.add_fixed_in_frame_mobjects(*mobs)
        if not show:
            self.remove(*mobs)
        return mobs[0] if len(mobs) == 1 else mobs

    def tear_down(self):
        super().tear_down()
        self._report()


# --------------------------------------------------------------------------
# Small building blocks (same vocabulary as metriplectic_1.py)
# --------------------------------------------------------------------------
def hdr(text, size=34, kind="subsection"):
    h = code_line([("\\" + kind, BLUE), ("{", FG_DIM), (text, PURPLE), ("}", FG_DIM)],
                  font_size=size)
    return place_header(h)


def note(text, size=20, color=GREY_B, **kw):
    return Text(text, font_size=size, color=color, line_spacing=0.9, **kw)


def tex(s, size=34, color=None, **kw):
    m = MathTex(s, font_size=size, **kw)
    if color is not None:
        m.set_color(color)
    return m


def chip(label, color=BLUE, size=22, dashed=False, fill=0.0):
    t = Text(label, font_size=size)
    box = RoundedRectangle(corner_radius=0.12, width=t.width + 0.5, height=t.height + 0.38,
                           color=color, fill_color=color, fill_opacity=fill)
    if dashed:
        box = DashedVMobject(box, num_dashes=40)
    t.move_to(box.get_center())
    return VGroup(box, t)


def card(lines, color=BLUE, size=20, width=None, line_spacing=0.9):
    t = Text(lines, font_size=size, line_spacing=line_spacing)
    w = width or t.width + 0.5
    box = RoundedRectangle(corner_radius=0.12, width=w, height=t.height + 0.4, color=color)
    t.move_to(box.get_center())
    return VGroup(box, t)


def ramp_in(t):
    """Quadratic ease-in (slow start, fast finish) for rate_func."""
    return t * t


def at_left(m, y, x=LEFT_X):
    """Place m with its left edge at x and its centre line at y."""
    m.move_to([x, y, 0], aligned_edge=LEFT)
    return m


def poly(points, color=WHITE, sw=3, closed=False, fill=None, op=0.0):
    """Polyline through an (n, 2 or 3) array of scene coordinates."""
    pts = [np.array([p[0], p[1], 0.0]) for p in points]
    if closed:
        pts = pts + [pts[0]]
    m = VMobject(stroke_color=color, stroke_width=sw)
    if fill is not None:
        m.set_fill(fill, opacity=op)
    m.set_points_as_corners(pts)
    return m


def cmap_array(v, stops=("#0b1020", "#1b3a8a", "#2fa4a0", "#f3e04a", "#ffffff")):
    """Vectorised colormap: v in [0,1], any shape -> (..., 4) uint8."""
    rgb = np.array([color_to_rgb(s) for s in stops])
    xs = np.linspace(0, 1, len(stops))
    v = np.clip(v, 0, 1)
    out = np.zeros(v.shape + (4,), dtype=np.uint8)
    for c in range(3):
        out[..., c] = (255 * np.interp(v, xs, rgb[:, c])).astype(np.uint8)
    out[..., 3] = 255
    return out


def diverging_array(v, neg="#2b6cff", pos="#ff4a3d", mid="#12151c"):
    """v in [-1, 1] -> blue (down) .. dark .. red (up)."""
    n, m_, p = (np.array(color_to_rgb(c)) for c in (neg, mid, pos))
    v = np.clip(v, -1, 1)
    out = np.zeros(v.shape + (4,), dtype=np.uint8)
    for c in range(3):
        a = np.where(v < 0, m_[c] + (n[c] - m_[c]) * (-v), m_[c] + (p[c] - m_[c]) * v)
        out[..., c] = (255 * a).astype(np.uint8)
    out[..., 3] = 255
    return out


# ==========================================================================
# The manufactured flow (closed form) + data derived from it
# ==========================================================================
H_EXP = 0.01            # anisotropy exponent h
A_U, A_PSI = 8.0, 5.0   # swirl / stream-function amplitudes
A_Z = 0.5 - H_EXP       # core-height exponent 1/2 - h
T0, T1 = 0.3, 0.5       # switch-on window (the ramp shape is an assumption)


def switch(t):
    """C2 smootherstep from 0 (t <= 0.3) to 1 (t >= 0.5).  Stand-in for the
    script's 'approximately smooth function'."""
    x = np.clip((np.asarray(t, dtype=float) - T0) / (T1 - T0), 0.0, 1.0)
    return x * x * x * (10 - 15 * x + 6 * x * x)


def fields(r, z, t):
    """(u1, u_r, u_z) of the exact flow at switch = 1.  Closed forms, checked
    against sympy differentiation of psi1."""
    ta = 1.0 - t
    rho2 = np.asarray(r) ** 2 / ta
    zeta = np.asarray(z) / ta ** A_Z
    E = np.exp(-rho2 - zeta ** 2)
    u1 = A_U * ta ** (-1 - H_EXP) * E
    ur = -A_PSI * np.asarray(r) / ta * (1 - 2 * zeta ** 2) * E
    uz = 2 * A_PSI * ta ** (-0.5 - H_EXP) * zeta * (1 - rho2) * E
    return u1, ur, uz


def swirl_peak(t):
    """max over (r, z) of u_theta = r*u1, switch included."""
    ta = 1.0 - np.asarray(t, dtype=float)
    return switch(t) * A_U * math.sqrt(0.5) * math.exp(-0.5) * ta ** (-0.5 - H_EXP)


def swirl_field(r, z, t):
    u1, _, _ = fields(r, z, t)
    return switch(t) * np.asarray(r) * u1


def similarity(r, z, t):
    ta = 1.0 - t
    return r / math.sqrt(ta), z / ta ** A_Z


def physical(rho, zeta, t):
    ta = 1.0 - t
    return rho * math.sqrt(ta), zeta * ta ** A_Z


def streamline(seed, t, n=240, ds=0.012, sign=1.0, rmax=None, zmax=None):
    """Meridional streamline (r, z) of the exact flow, integrated in arc length."""
    p = np.array(seed, float)
    pts = [p.copy()]

    def vel(q):
        _, ur, uz = fields(max(q[0], 1e-9), q[1], t)
        v = np.array([ur, uz]) * sign
        nv = np.hypot(*v)
        return v / nv if nv > 1e-12 else v * 0

    for _ in range(n):
        k1 = vel(p); k2 = vel(p + ds / 2 * k1); k3 = vel(p + ds / 2 * k2); k4 = vel(p + ds * k3)
        p = p + ds / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        if p[0] < 0:
            p[0] = 0
        if (rmax and p[0] > rmax) or (zmax and abs(p[1]) > zmax):
            break
        pts.append(p.copy())
    return np.array(pts)


def heat_values(r_range, z_range, fn, nr=36, nz=64):
    """Samples fn(R, Z) on the (r, z) half plane: array (nz, nr), row 0 = top (+z), column 0 = r_min."""
    rr = np.linspace(r_range[0], r_range[1], nr)
    zz = np.linspace(z_range[1], z_range[0], nz)
    R, Z = np.meshgrid(rr, zz)
    return np.asarray(fn(R, Z), dtype=float)


class HeatGrid(VGroup):
    """Pixel-grid heat map built from Rectangles.  (Deliberately not an ImageMobject: that path
    depends on the installed Pillow version, this one does not.)"""

    def __init__(self, width, height, nr=36, nz=64, **kw):
        super().__init__(**kw)
        cw, ch = width / nr, height / nz
        for j in range(nz):
            for i in range(nr):
                c = Rectangle(width=cw, height=ch, stroke_width=0.5, fill_opacity=1)
                c.move_to([(i + 0.5) * cw - width / 2, height / 2 - (j + 0.5) * ch, 0])
                self.add(c)

    def set_values(self, arr, vmax, stops=None, opacity=1.0):
        rgba = cmap_array(arr / vmax, stops) if stops else cmap_array(arr / vmax)
        for c, px in zip(self.submobjects, rgba.reshape(-1, 4)):
            col = "#%02x%02x%02x" % (int(px[0]), int(px[1]), int(px[2]))
            c.set_fill(col, opacity=opacity).set_stroke(col, width=0.5, opacity=opacity)
        return self


# --------------------------------------------------------------------------
# Tracer particles: RK4 in the exact flow, like the solver's own stepper
# --------------------------------------------------------------------------
TRACER_RADII = np.linspace(0.15, 1.12, 7)   # all 21 midplane particles fall in: 3.2-4.4 turns
TRACER_HEIGHTS = np.linspace(-1.2, 1.2, 7)
TRACER_ANGLES = np.array([0.0, 2 * math.pi / 3, 4 * math.pi / 3])
T_REL, T_END = 0.4, 0.98


@lru_cache(maxsize=1)
def tracers(n_snap=420, dt=1e-4):
    """Release 7 radii x 7 heights x 3 angles = 147 particles at t = 0.4 and
    follow them to t = 0.98.  Returns (times, r, z, theta_unwrapped, is_midplane)
    with snapshots log-spaced in tau = 1 - t so the final collapse is resolved."""
    R0, Z0, TH0 = np.meshgrid(TRACER_RADII, TRACER_HEIGHTS, TRACER_ANGLES, indexing="ij")
    Y = np.stack([R0.ravel(), Z0.ravel(), TH0.ravel()], 1).astype(float)
    mid = np.isclose(Y[:, 1], 0.0)

    def rhs(t, Y):
        u1, ur, uz = fields(np.maximum(Y[:, 0], 1e-12), Y[:, 1], t)
        s = float(switch(t))
        return s * np.stack([ur, uz, u1], 1)

    n = int(round((T_END - T_REL) / dt))
    tau_snap = np.exp(np.linspace(math.log(1 - T_REL), math.log(1 - T_END), n_snap))
    snap_steps = np.unique(np.clip(np.round(((1 - tau_snap) - T_REL) / dt).astype(int), 0, n))
    out_t, out = [], []
    t = T_REL
    k_next = 0
    for k in range(n + 1):
        if k_next < len(snap_steps) and k == snap_steps[k_next]:
            out_t.append(t); out.append(Y.copy()); k_next += 1
        if k == n:
            break
        k1 = rhs(t, Y); k2 = rhs(t + dt / 2, Y + dt / 2 * k1)
        k3 = rhs(t + dt / 2, Y + dt / 2 * k2); k4 = rhs(t + dt, Y + dt * k3)
        Y = Y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        t += dt
    out = np.array(out)
    return np.array(out_t), out[:, :, 0], out[:, :, 1], out[:, :, 2], mid


# --------------------------------------------------------------------------
# Term balance of the u_theta equation for the exact flow.  Each row is
#   t, max|acc|, max|nonlinear|, max|viscous|, max|force|    (all x tau^(3/2+h))
# computed symbolically with sympy on a 600 x 1200 (r, z) grid (see header).
# The same four lines of the video are  c_i(t) * tau^(-3/2-h).
# --------------------------------------------------------------------------
TERM_TABLE = np.array([
    [0.60, 2.38, 19.52, 28.56, 19.89],
    [0.80, 2.38, 19.52, 28.48, 19.80],
    [0.90, 2.38, 19.52, 28.38, 19.71],
    [0.95, 2.38, 19.50, 28.29, 19.62],
    [0.98, 2.38, 19.51, 28.17, 19.50],
])


def term_consts(t):
    """Interpolated (acc, nonlinear, viscous, force) prefactors at time t."""
    return np.array([np.interp(t, TERM_TABLE[:, 0], TERM_TABLE[:, k]) for k in range(1, 5)])


# --------------------------------------------------------------------------
# Helpers used by several scenes
# --------------------------------------------------------------------------
def chevron(p, d, size=0.09, color=WHITE):
    """Small filled arrow head at point p pointing along direction d."""
    ang = math.atan2(d[1], d[0])
    tri = Triangle(fill_opacity=1, stroke_width=0, color=color).scale(size)
    tri.rotate(ang - PI / 2).move_to([p[0], p[1], 0])
    return tri


def flow_line(pts, color=BLUE_C, sw=2.5, heads=(0.5,), size=0.085):
    """A streamline polyline with arrow heads at the given fractions of its length."""
    m = poly(pts, color, sw)
    g = VGroup(m)
    n = len(pts)
    for f in heads:
        i = max(1, min(n - 1, int(f * n)))
        d = np.array(pts[i]) - np.array(pts[i - 1])
        g.add(chevron(pts[i], d, size, color))
    return g


def pipeline(labels, colors, y=0.0, size=22, gap=0.55, x0=0.0, fill=0.08):
    """A row of chips joined by arrows.  Returns (chips VGroup, arrows VGroup)."""
    chips = VGroup(*[chip(s, c, size, fill=fill) for s, c in zip(labels, colors)])
    chips.arrange(RIGHT, buff=gap + 0.5).move_to([x0, y, 0])
    clamp_width(chips)
    arrows = VGroup(*[Arrow(a.get_right(), b.get_left(), buff=0.08, color=GREY_B,
                            stroke_width=3, max_tip_length_to_length_ratio=0.35)
                      for a, b in zip(chips[:-1], chips[1:])])
    return chips, arrows


def clipped_curve(ax, fn, xs, ymax, color, sw=3):
    """Polyline of fn over xs on axes ax, cut where |y| > ymax."""
    pts = [ax.c2p(x, fn(x)) for x in xs if abs(fn(x)) <= ymax]
    m = VMobject(stroke_color=color, stroke_width=sw)
    m.set_points_as_corners(pts)
    return m


def ns_equation(size=42, with_f=True):
    parts = [r"\partial_t u", "+", r"(u\cdot\nabla)u", "=", r"-\nabla p", "+", r"\nu\Delta u"]
    if with_f:
        parts += ["+", "f"]
    eq = MathTex(*parts, font_size=size)
    cols = {0: C_ACC, 2: C_NL, 4: TEAL, 6: C_VISC}
    for i, c in cols.items():
        eq[i].set_color(c)
    if with_f:
        eq[8].set_color(C_FORCE)
    return eq


# ==========================================================================
# N0 -- title, "I won't go into any of that"   (cues 1-5)
# ==========================================================================
class N0(SyncedScene):
    AUDIO_START, AUDIO_END = START["N0"], END["N0"]
    STARS = (40, 3)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Simulating a Navier-Stokes blow-up: a manufactured-solution toy", FG)],
                          font_size=26)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: "...the drama and controversies relating to OpenAI's alleged proof of
        # singularities in solutions to the Navier-Stokes equation, brought by a forced system"
        self.at(1, Write(title), run_time=1.4)
        c_ai = chip("OpenAI's claimed finite-time singularity\nfor a forced Navier-Stokes system",
                    BLUE, 26, fill=0.08)
        c_dr = chip("the drama and the controversy", ORANGE, 26, fill=0.08)
        c_pde = chip("PDE theory: what the proof could mean for the field", TEAL, 26, fill=0.08)
        col = VGroup(c_ai, c_dr, c_pde).arrange(DOWN, buff=0.55).move_to([0, -0.2, 0])
        self.frac(1, 0.22, FadeIn(c_ai, shift=UP * 0.2), run_time=0.8)
        self.frac(1, 0.55, FadeIn(c_dr, shift=UP * 0.2), run_time=0.8)

        # cue 2: "I won't go into any of that."
        x_dr = Cross(c_dr, stroke_color=RED, stroke_width=6)
        self.at(2, Create(x_dr), c_dr.animate.set_opacity(0.35), run_time=0.7)

        # cue 3: a smaller subset understands the PDE theory ...
        self.at(3, FadeIn(c_pde, shift=UP * 0.2), run_time=0.8)

        # cue 4: "I am sadly not a part of this subset, so I will not be talking about that either."
        x_pde = Cross(c_pde, stroke_color=RED, stroke_width=6)
        self.at(4, Create(x_pde), c_pde.animate.set_opacity(0.35), run_time=0.7)
        self.snap("crossed")

        # cue 5: "I will be working on trying to numerically simulate this solution."
        self.at(5, FadeOut(x_dr), FadeOut(x_pde), FadeOut(c_dr), FadeOut(c_pde),
                run_time=0.6)
        goal = Text("numerically simulate this solution", font_size=40, color=YELLOW).move_to([0, -0.5, 0])
        self.play(Write(goal), run_time=1.2)
        # a little spiral: fluid winding in toward the axis
        spiral = ParametricFunction(lambda s: np.array([2.0 * math.exp(-0.45 * s) * math.cos(s),
                                                        0.6 * math.exp(-0.45 * s) * math.sin(s), 0]),
                                    t_range=[0, 4 * TAU], color=C_SWIRL, stroke_width=3)
        spiral.move_to([0, -2.1, 0])
        self.play(Create(spiral), run_time=2.4)
        self.finish()


# ==========================================================================
# N1 -- Why simulate?  (cues 6-13)
# ==========================================================================
class N1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N1"], END["N1"]

    def construct(self):
        header = hdr("Why Simulate")
        # cue 6: "Now you might be thinking, why simulate it?"
        self.at(6, Write(header), run_time=0.6)
        q = Text("why simulate it?", font_size=54, color=YELLOW).move_to([0, 0.4, 0])
        self.sync(CUES[6].start + 0.5)
        self.play(FadeIn(q, scale=0.9), run_time=0.7)
        # cue 7: "And that's a fair question."
        fair = note("a fair question", 28, GREY_B).next_to(q, DOWN, buff=0.4)
        self.at(7, FadeIn(fair), run_time=0.5)

        # cue 8: a numerical simulation is at best inferior / less accurate than the analytical one
        ax = Axes(x_range=[0.4, 1.0, 0.1], y_range=[0, 30, 10], x_length=6.4, y_length=3.2,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([-2.2, -0.3, 0])
        xl = note("t", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT)
        yl = note("peak swirl", 16).next_to(ax, UP, buff=0.05).align_to(ax, LEFT)
        exact = lambda t: float(swirl_peak(t))
        xs = np.linspace(0.4, 0.98, 200)
        c_ex = clipped_curve(ax, exact, xs, 30, BLUE_C)
        l_ex = note("analytic: exact", 18, BLUE_C).move_to(ax.c2p(0.55, 10))
        self.at(8, FadeOut(q), FadeOut(fair), Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(Create(c_ex), FadeIn(l_ex), run_time=1.6)
        # numerics: piecewise-linear on a coarse time grid, errors that grow
        tg = np.linspace(0.4, 0.98, 9)
        num = lambda t: float(swirl_peak(t)) * (1 - 0.0 - 0.18 * (t - 0.4) / 0.58)
        c_nu = clipped_curve(ax, lambda t: float(np.interp(t, tg, [num(x) for x in tg])), xs, 30, ORANGE)
        l_nu = note("numerical: at best an approximation", 18, ORANGE).shift(DOWN * 1.5 + RIGHT * 0.2)
        self.frac(8, 0.45, Create(c_nu), FadeIn(l_nu), run_time=1.6)

        # cue 9: "In reality, it will actually be much worse than that for reasons that I will get into."
        worse = chip("in reality: much worse, for reasons we will get to", RED, 24, fill=0.08)
        worse.move_to([4.2, -0.3, 0]); clamp_width(worse, 4.4)
        worse = card("in reality it is\nmuch worse than that,\nfor reasons we will get to", RED, 22, 4.2).move_to([4.3, -0.3, 0])
        self.at(9, FadeIn(worse, shift=LEFT * 0.2), run_time=0.7)

        # cue 10: "really curiosity is the only true motivating factor."
        self.at(10, FadeOut(VGroup(ax, xl, yl, c_ex, l_ex, c_nu, l_nu, worse)), run_time=0.5)
        cur = Text("curiosity", font_size=88, color=YELLOW).move_to([0, 0.3, 0])
        sub = note("the only real motivating factor", 26, GREY_B).next_to(cur, DOWN, buff=0.4)
        self.play(FadeIn(cur, scale=0.85), run_time=0.8)
        self.play(FadeIn(sub), run_time=0.5)

        # cue 11: a fun challenge, challenges I didn't count for, what I'll learn, hope you learn too
        self.at(11, FadeOut(cur), FadeOut(sub), run_time=0.4)
        items = [("a fun challenge", ORANGE), ("challenges I did not count on", RED),
                 ("what I will learn while researching it", GREEN),
                 ("and, I hope, something for you to learn too", BLUE)]
        rows = VGroup(*[chip(s, c, 28, fill=0.08) for s, c in items]).arrange(DOWN, buff=0.4).move_to([0, 0.0, 0])
        for i, (r_, f) in enumerate(zip(rows, (0.04, 0.27, 0.5, 0.78))):
            self.frac(11, f, FadeIn(r_, shift=RIGHT * 0.2), run_time=0.6)
        self.snap("motivation")

        # cue 12: "Obviously, all these results will be trivial and offer nothing to the researched mathematics."
        self.at(12, FadeOut(rows), run_time=0.4)
        triv = card("the results will be trivial\nand offer nothing new\nto the mathematics", GREY_B, 30, 6.4).move_to([0, 0.4, 0])
        self.play(FadeIn(triv), run_time=0.6)
        # cue 13: "It is just a little hobby of my own."
        hob = Text("just a little hobby", font_size=44, color=YELLOW).move_to([0, -2.0, 0])
        self.at(13, FadeIn(hob, shift=UP * 0.2), run_time=0.6)
        self.finish(keep=[header])


# ==========================================================================
# N2 -- The Problem: equations and the difficulty   (cues 14-19)
# ==========================================================================
class N2(SyncedScene):
    AUDIO_START, AUDIO_END = START["N2"], END["N2"]

    def construct(self):
        header = hdr("The Problem")
        self.add(header)
        # cue 14: "...the Navier-Stokes equations are Newton's second law for a fluid."
        newton = Text("Newton's second law, for a fluid", font_size=30, color=YELLOW).move_to([0, 1.9, 0])
        ma = tex(r"m\,a = F", 44, GREY_B).move_to([0, 1.9, 0])
        self.at(14, FadeIn(ma), run_time=0.6)
        eq = ns_equation(44).move_to([0, 0.5, 0])
        self.frac(14, 0.35, ReplacementTransform(ma, newton), Write(eq), run_time=1.8)

        # cue 15: the terms, one by one
        def tag(i, text, color, dy, dx=0.0):
            t = note(text, 19, color)
            t.move_to(eq[i].get_center() + [dx, dy, 0])
            ln = Line(eq[i].get_bottom() + DOWN * 0.08, t.get_top() + UP * 0.08, color=color, stroke_width=1.5)
            return VGroup(ln, t)

        t_acc = tag(0, "how u changes", C_ACC, -1.0)
        t_adv = tag(2, "advection: the fluid carries\nits own momentum", C_NL, -1.9)
        t_prs = tag(4, "pressure", TEAL, -1.0)
        t_vis = tag(6, "viscosity:\nsmooths things out", C_VISC, -1.9)
        t_frc = tag(8, "external force", C_FORCE, -1.0)
        inc = tex(r"\nabla\cdot u = 0", 44, GREY_B).move_to([0, -2.7, 0])
        t_inc = note("incompressible", 22, GREY_B).next_to(inc, RIGHT, buff=0.4)
        self.at(15, FadeIn(t_acc), run_time=0.5)
        self.frac(15, 0.10, FadeIn(t_adv), run_time=0.5)
        self.frac(15, 0.33, FadeIn(t_prs), run_time=0.5)
        self.frac(15, 0.46, FadeIn(t_vis), run_time=0.5)
        self.frac(15, 0.62, FadeIn(t_frc), run_time=0.5)
        self.frac(15, 0.76, Write(inc), FadeIn(t_inc), run_time=0.9)
        self.snap("terms")

        # cue 16: the essential difficulty: advection x pressure x vorticity stretching
        self.at(16, *[FadeOut(m) for m in [eq, newton, t_acc, t_adv, t_prs, t_vis, t_frc, inc, t_inc]],
                run_time=0.5)
        top = chip("advection", C_NL, 26, fill=0.1).move_to([0, 1.5, 0])
        left = chip("pressure", TEAL, 26, fill=0.1).move_to([-3.4, -1.0, 0])
        right = chip("vorticity stretch", C_SWIRL, 26, fill=0.1).move_to([3.4, -1.0, 0])
        tri = VGroup(*[DoubleArrow(a.get_center(), b.get_center(), buff=1.5, color=GREY_B, stroke_width=3,
                                   max_tip_length_to_length_ratio=0.12)
                       for a, b in ((top, left), (top, right), (left, right))])
        hard = note("the essential difficulty: their nonlinear interaction", 26, YELLOW).move_to([0, -2.5, 0])
        self.play(FadeIn(top), FadeIn(left), FadeIn(right), run_time=0.6)
        self.play(Create(tri), run_time=0.8)
        self.frac(16, 0.4, FadeIn(hard), run_time=0.5)

        # cue 17: nonlinearity moves energy to small scales / sharpens gradients; viscosity smooths
        self.at(17, *[FadeOut(m) for m in [top, left, right, tri, hard]], run_time=0.5)
        k = ValueTracker(0.3)
        a = ValueTracker(1.0)
        axL = Axes(x_range=[0, TAU, 1], y_range=[-1.3, 1.3, 1], x_length=5.4, y_length=2.4,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([-3.4, -0.2, 0])
        axR = Axes(x_range=[0, TAU, 1], y_range=[-1.3, 1.3, 1], x_length=5.4, y_length=2.4,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([3.4, -0.2, 0])
        curL = always_redraw(lambda: axL.plot(
            lambda x: math.tanh(k.get_value() * math.sin(x)) / math.tanh(k.get_value()),
            x_range=[0, TAU, 0.02], color=C_NL, use_smoothing=False))
        curR = always_redraw(lambda: axR.plot(
            lambda x: a.get_value() * math.sin(x), x_range=[0, TAU, 0.02], color=C_VISC))
        tL = note("nonlinearity: energy moves to small scales,\ngradients intensify", 20, C_NL).next_to(axL, UP, buff=0.3)
        tR = note("viscosity: dissipates kinetic energy,\nsmooths the flow", 20, C_VISC).next_to(axR, UP, buff=0.3)
        self.play(Create(axL), Create(axR), FadeIn(tL), FadeIn(tR), run_time=0.8)
        self.add(curL, curR)
        self.play(k.animate.set_value(6.0), a.animate.set_value(0.12), run_time=5.5, rate_func=smooth)
        self.snap("tug")

        # cue 18: do these mechanisms + incompressibility prevent singularities for all smooth data?
        self.at(18, *[FadeOut(m) for m in [axL, axR, curL, curR, tL, tR]], run_time=0.5)
        c1 = chip("any smooth initial data", BLUE, 28, fill=0.08)
        c2 = chip("stays smooth for all time?", GREEN, 28, fill=0.08)
        pr = VGroup(c1, c2).arrange(RIGHT, buff=1.5).move_to([0, 0.6, 0])
        ar = Arrow(c1.get_right(), c2.get_left(), buff=0.1, color=GREY_B)
        q = Text("?", font_size=56, color=YELLOW).next_to(ar, UP, buff=0.05)
        self.play(FadeIn(c1), run_time=0.5)
        self.play(GrowArrow(ar), FadeIn(c2), FadeIn(q), run_time=0.8)
        # cue 19: ...or can a solution become singular in finite time?
        c3 = chip("or does it become singular in finite time?", RED, 28, fill=0.1).move_to([0, -1.5, 0])
        self.at(19, FadeIn(c3, shift=UP * 0.2), run_time=0.7)
        self.finish(keep=[header])


# ==========================================================================
# N2_1 -- finite-time singularity   (cues 20-26)
# ==========================================================================
class N2_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N2_1"], END["N2_1"]

    def construct(self):
        header = hdr("The Problem")
        self.add(header)
        # cue 20: ...a finite-time singularity: the velocity becomes unbounded at a finite time T,
        # even though everything started smooth.
        ax = Axes(x_range=[0, 1.1, 0.5], y_range=[0, 10, 5], x_length=6.6, y_length=3.6,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([-3.0, -0.5, 0])
        xl = tex("t", 28, GREY_B).next_to(ax, DOWN, buff=0.1).align_to(ax, RIGHT)
        yl = tex(r"\max|u|", 28, GREY_B).next_to(ax, UP, buff=0.1).align_to(ax, LEFT)
        Tline = DashedLine(ax.c2p(1, 0), ax.c2p(1, 10), color=RED, dash_length=0.12)
        Tl = tex("T", 30, RED).next_to(ax.c2p(1, 0), DOWN, buff=0.12)
        curve = clipped_curve(ax, lambda t: 1.0 + 0.55 * (1 - t) ** -0.5 - 0.55, np.linspace(0, 0.9985, 600), 10, C_SWIRL)
        smooth = note("smooth start", 20, GREEN).move_to(ax.c2p(0.14, 1.9))
        self.at(20, Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(FadeIn(smooth), run_time=0.4)
        self.frac(20, 0.25, Create(Tline), FadeIn(Tl), run_time=0.6)
        self.play(Create(curve), run_time=5.0, rate_func=ramp_in)
        blow = note("unbounded at a finite time", 22, C_SWIRL).move_to([-3.0, 2.2, 0])
        self.play(FadeIn(blow), run_time=0.5)
        self.snap("blowup")

        # cue 21: "Now in real fluids, they simply don't do this."
        real = card("real fluids simply\ndon't do this", GREEN, 26, 3.6).move_to([3.7, 1.1, 0])
        self.at(21, FadeIn(real, shift=LEFT * 0.2), run_time=0.7)
        # cue 22: "And that's why the question matters."
        why = chip("that is why the question matters", YELLOW, 26, fill=0.1).move_to([3.7, -0.6, 0])
        clamp_width(why, 5.2)
        self.at(22, FadeIn(why, shift=UP * 0.2), run_time=0.6)

        # cue 23: what separates our fluid mechanics from our current models
        self.at(23, *[FadeOut(m) for m in [real, why]], run_time=0.4)
        a = card("real fluid", BLUE, 26, 2.8)
        b = card("our model:\nNavier-Stokes", ORANGE, 26, 2.8)
        pr = VGroup(a, b).arrange(RIGHT, buff=1.2).move_to([3.7, 1.1, 0])
        gap = note("what separates them?", 22, YELLOW).next_to(pr, DOWN, buff=0.3)
        self.play(FadeIn(pr), run_time=0.6)
        self.play(FadeIn(gap), run_time=0.4)
        # cue 24/25: and further, what aspects of PDEs create singularities, can we make better ones?
        q1 = chip("what about a PDE creates singularities?", TEAL, 24, fill=0.08)
        q2 = chip("can we design better ones?", PURPLE, 24, fill=0.08)
        qs = VGroup(q1, q2).arrange(DOWN, buff=0.35).move_to([3.7, -1.6, 0])
        clamp_width(qs, 6.0)
        self.at(25, FadeIn(q1, shift=UP * 0.15), run_time=0.6)
        self.frac(25, 0.55, FadeIn(q2, shift=UP * 0.15), run_time=0.6)
        # cue 26: I won't pretend to know all the influence of the problem
        hum = note("(I won't pretend to know the full influence of the Navier-Stokes problem)", 20, GREY_B)
        hum.move_to([0, -3.0, 0])
        clamp_width(hum, 11.0)
        self.at(26, FadeIn(hum), run_time=0.6)
        self.finish(keep=[header])


# ==========================================================================
# N3 -- The Flow: the vortex picture, tau = 1 - t   (cues 27-32)
# ==========================================================================
class N3(SyncedScene):
    AUDIO_START, AUDIO_END = START["N3"], END["N3"]

    def construct(self):
        header = hdr("The Flow")
        self.add(header)
        # meridional (r, z) half plane at t = 0.6 (tau = 0.4): the exact flow's streamlines
        SC = 1.35
        AX = np.array([-5.6, -0.35, 0.0])                 # the axis (r = 0) crosses here at z = 0
        P = lambda r, z: [AX[0] + SC * r, AX[1] + SC * z, 0]
        frame = Rectangle(width=SC * 2.2, height=SC * 3.4, color=GREY_C, stroke_width=2)
        frame.move_to([AX[0] + SC * 1.1, AX[1], 0])
        axis = DashedLine(P(0, -1.7), P(0, 1.7), color=GREY_B, dash_length=0.1)
        axl = note("axis", 16, GREY_B).move_to(P(0, -1.95))
        rl = tex("r", 24, GREY_B).move_to(P(2.35, 0.0))
        zl = tex("z", 24, GREY_B).move_to(P(0.0, 1.95))
        # cue 27: "Now to simulate it, we need to look at the actual physical flow."
        self.at(27, Create(frame), Create(axis), FadeIn(axl), FadeIn(rl), FadeIn(zl), run_time=1.0)
        tmid = Text("we need the physical flow", font_size=26, color=GREY_B).move_to([2.9, 2.45, 0])
        self.play(FadeIn(tmid), run_time=0.6)

        # streamlines of the exact flow
        T_VIS = 0.6
        seeds = [(0.30, 0.03), (0.55, 0.03), (0.85, 0.03), (1.20, 0.03),
                 (0.30, -0.03), (0.55, -0.03), (0.85, -0.03), (1.20, -0.03),
                 (1.00, 0.90), (1.00, -0.90), (1.9, 0.9), (1.9, -0.9)]
        lines, mids = VGroup(), VGroup()
        for sd in seeds:
            pts = streamline(sd, T_VIS, n=420, ds=0.014, rmax=2.15, zmax=1.68)
            g = flow_line([P(*q) for q in pts], BLUE_C, 2.0, heads=(0.3, 0.7))
            (mids if abs(sd[1]) < 0.05 else lines).add(g)
        # cue 28: "The solution is a vortex."
        vtx = Text("the solution is a vortex", font_size=36, color=YELLOW).move_to([2.9, 2.45, 0])
        self.at(28, ReplacementTransform(tmid, vtx), run_time=0.7)
        self.play(LaggedStart(*[Create(g) for g in lines], lag_ratio=0.1), run_time=1.0)

        # cue 29: in the middle plane the fluid spirals inward, picks up spin, flows away along the axis
        in_arrows = VGroup(*[Arrow(P(r_ + 0.35, 0.0), P(r_, 0.0), buff=0, color=C_RAD, stroke_width=5,
                                   max_tip_length_to_length_ratio=0.45) for r_ in (1.6, 1.05, 0.55)])
        # top view: a midplane particle winding in (from the same exact flow)
        ts, rr, zz, th, mid = tracers()
        j = int(np.where(mid)[0][12])
        CIR = np.array([3.9, -0.6, 0.0])
        S2 = 1.35
        circ = Circle(radius=S2 * 1.2, color=GREY_C, stroke_width=2).move_to(CIR)
        cx = Dot(CIR, radius=0.04, color=GREY_B)
        path_pts = [[CIR[0] + S2 * rr[i, j] * math.cos(th[i, j]), CIR[1] + S2 * rr[i, j] * math.sin(th[i, j])]
                    for i in range(0, len(ts), 2)]
        spiral = poly(path_pts, C_SWIRL, 3)
        top_t = note("top view of one midplane particle:\nspirals in and spins up", 18, C_SWIRL).move_to([3.9, 1.65, 0])
        self.at(29, FadeIn(mids), FadeIn(in_arrows), run_time=0.8)
        self.frac(29, 0.2, FadeIn(circ), FadeIn(cx), FadeIn(top_t), run_time=0.5)
        self.play(Create(spiral), run_time=4.5, rate_func=ramp_in)
        self.snap("midplane")

        # cue 30: "...up on one side, down on the other."
        up = Arrow(P(0.12, 0.35), P(0.12, 1.3), buff=0, color=C_AXIAL, stroke_width=7,
                   max_tip_length_to_length_ratio=0.3)
        dn = Arrow(P(0.12, -0.35), P(0.12, -1.3), buff=0, color=C_DOWN, stroke_width=7,
                   max_tip_length_to_length_ratio=0.3)
        ul = note("up", 20, C_AXIAL).move_to(P(0.55, 1.2)); dl = note("down", 20, C_DOWN).move_to(P(0.62, -1.2))
        self.at(30, FadeOut(circ), FadeOut(cx), FadeOut(top_t), FadeOut(spiral), run_time=0.8)
        
        # cue 31: "Incompressibility is what forces that axial outflow."
        inc = chip("incompressibility forces the axial outflow", YELLOW, 22, fill=0.1)
        inc.move_to([3.2, -2.5, 0]); clamp_width(inc, 6.4)
        inc_eq = tex(r"\nabla\cdot u=0", 34, YELLOW).move_to([3.9, 2.4, 0])
        self.at(31, FadeIn(inc, shift=UP * 0.15), run_time=0.6)

        # cue 32: "...the fluid that arrives has to go somewhere... define tau = time until the singularity"
        tau_eq = tex(r"\tau = 1 - t", 46, C_SWIRL).move_to([0.2, 0.6, 0])
        tau_n = note("time remaining until the singularity", 24, GREY_B).next_to(tau_eq, DOWN, buff=0.3)
        self.at(32, FadeOut(vtx), FadeOut(inc), run_time=0.4)
        self.play(Write(tau_eq), FadeIn(tau_n), run_time=1.0)
        bar = Line([-1.4, -0.9, 0], [3.2, -0.9, 0], color=GREY_B, stroke_width=3)
        tk = ValueTracker(0.0)
        mark = always_redraw(lambda: Dot([-1.4 + 4.6 * tk.get_value(), -0.9, 0], radius=0.09, color=C_SWIRL))
        t0l = tex("t=0", 22, GREY_B).next_to(bar.get_start(), DOWN, buff=0.15)
        t1l = tex("T=1", 22, RED).next_to(bar.get_end(), DOWN, buff=0.15)
        self.play(FadeIn(bar), FadeIn(t0l), FadeIn(t1l), run_time=0.5)
        self.add(mark)
        self.play(tk.animate.set_value(0.97), run_time=3.2, rate_func=smooth)
        self.finish(keep=[header])


# ==========================================================================
# N3_1 -- scaling laws of the core   (cues 33-41)
# ==========================================================================
class N3_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N3_1"], END["N3_1"]

    def construct(self):
        header = hdr("The Flow")
        self.add(header)
        # schematic core, with h exaggerated so the aspect change is visible
        H_VIS = 0.25
        tau = ValueTracker(1.0)
        C = np.array([-4.3, -0.4, 0.0])
        SC = 1.6
        frame = Rectangle(width=4.2, height=5.2, color=GREY_D, stroke_width=1.5).move_to(C)
        core = always_redraw(lambda: Ellipse(
            width=2 * SC * 0.75 * math.sqrt(tau.get_value()),
            height=2 * SC * 0.95 * tau.get_value() ** (0.5 - H_VIS),
            color=C_SWIRL, fill_color=C_SWIRL, fill_opacity=0.28, stroke_width=3).move_to(C))
        axis = DashedLine(C + [0, -2.5, 0], C + [0, 2.5, 0], color=GREY_C, dash_length=0.1)
        tau_lab = always_redraw(lambda: VGroup(
            tex(r"\tau=", 28, GREY_B),
            DecimalNumber(tau.get_value(), num_decimal_places=2, font_size=30, color=GREY_B)
        ).arrange(RIGHT, buff=0.1).move_to(C + [0, -2.8, 0]))
        hnote = note("h exaggerated to 0.25 here\n(the run uses h = 0.01)", 15, GREY_B).move_to(C + [0, 1.95, 0])
        self.add(frame, axis, core, tau_lab)
        self.add(hnote)

        X0 = -1.4
        rows = {}

        def line(key, s, y, color=FG, size=36):
            m = at_left(tex(s, size, color), y, X0)
            rows[key] = m
            return m

        # cue 33: "The core radius shrinks like tau to the one half."
        r1 = line("R", r"R_{\rm core}\;\sim\;\tau^{1/2}", 2.3, C_SWIRL)
        self.at(33, Write(r1), tau.animate.set_value(0.35), run_time=2.0)
        # cue 34: "Exactly the viscous diffusion scale."
        r2 = line("visc", r"=\ \text{viscous diffusion length}\ \sqrt{\nu\tau}", 1.55, C_VISC, 30)
        self.at(34, Write(r2), run_time=1.4)
        # cue 35: core height shrinks a bit more slowly, tau^(1/2-h), h < 1/100
        r3 = line("Z", r"Z_{\rm core}\;\sim\;\tau^{1/2-h},\qquad h<10^{-2}", 0.7, C_AXIAL, 34)
        self.at(35, Write(r3), run_time=1.6)
        # cue 36: a longer and longer, thinner and thinner column
        self.at(36, tau.animate.set_value(0.07), run_time=4.0, rate_func=smooth)
        col = note("a longer and thinner column", 22, YELLOW).move_to([2.6, 0.0, 0])
        self.play(FadeIn(col), run_time=0.5)

        # cue 37: volume ~ tau^(3/2-h);  speed^2 ~ tau^(-1-2h)
        r4 = line("V", r"V_{\rm core}\sim \tau^{3/2-h}", -0.5, BLUE_C, 34)
        r5 = line("U", r"|u|^2\sim \tau^{-1-2h}", -1.3, ORANGE, 34)
        self.at(37, FadeOut(col), Write(r4), run_time=1.2)
        self.frac(37, 0.45, Write(r5), run_time=1.2)
        # cue 38: multiply: kinetic energy ~ tau^(1/2-3h)
        r6 = line("E", r"E_{\rm core}\sim V|u|^2\sim \tau^{1/2-3h}", -2.1, YELLOW, 34)
        self.at(38, Write(r6), run_time=1.5)
        # cue 39: "...as long as h is small, that tends towards zero."
        r7 = at_left(note("-> 0  as tau -> 0   (whenever h < 1/6)", 22, YELLOW), -2.75, X0)
        self.at(39, FadeIn(r7), run_time=0.6)

        # cue 40: swirl Reynolds number ~ tau^(-h): unbounded, just slowly
        self.at(40, *[FadeOut(m) for m in [r1, r2, r3, r4, r5, r6, r7]], run_time=0.5)
        s1 = line("Re", r"\mathrm{Re}_{\rm swirl}=\frac{u_\theta R}{\nu}\sim \tau^{-h}", 1.7, C_SWIRL, 40)
        s2 = at_left(note("grows without bound ... just very slowly", 22, GREY_B), 0.7, X0)
        self.play(Write(s1), run_time=1.4)
        self.play(FadeIn(s2), run_time=0.5)
        # cue 41: radial Reynolds number stays order one: viscosity always matters for the inflow
        s3 = line("Rr", r"\mathrm{Re}_{\rm radial}=\frac{u_r R}{\nu}\sim O(1)", -0.5, C_RAD, 40)
        s4 = at_left(note("viscosity never stops mattering for the inflow,\nso the radial part of the flow can balance itself", 22, GREY_B), -1.5, X0)
        self.at(41, Write(s3), run_time=1.3)
        self.play(FadeIn(s4), run_time=0.6)
        self.snap("reynolds")
        self.finish(keep=[header])


# ==========================================================================
# N4 -- key idea: force := residual   (cues 42-46)
# ==========================================================================
class N4(SyncedScene):
    AUDIO_START, AUDIO_END = START["N4"], END["N4"]

    def construct(self):
        header = hdr("The Flow")
        self.add(header)
        # cue 42: "Now the key idea is a very simple one."
        key = Text("the key idea is a simple one", font_size=44, color=YELLOW).move_to([0, 0.4, 0])
        self.at(42, FadeIn(key, scale=0.95), run_time=0.8)

        # cue 43: pick ANY u and p, define the force as whatever is left over
        self.at(43, FadeOut(key), run_time=0.4)
        pick = chip("pick any velocity u and pressure p", BLUE, 28, fill=0.08).move_to([0, 2.0, 0])
        defn = MathTex("f", ":=", r"\partial_t u", "+", r"(u\cdot\nabla)u", "-", r"\nu\Delta u", "+", r"\nabla p",
                       font_size=46).move_to([0, 0.5, 0])
        defn[0].set_color(C_FORCE)
        defn[2].set_color(C_ACC); defn[4].set_color(C_NL); defn[6].set_color(C_VISC); defn[8].set_color(TEAL)
        left = note("whatever is left over when you plug them in", 24, GREY_B).move_to([0, -0.5, 0])
        self.play(FadeIn(pick, shift=DOWN * 0.15), run_time=0.6)
        self.frac(43, 0.3, Write(defn), run_time=2.0)
        self.frac(43, 0.7, FadeIn(left), run_time=0.5)
        # cue 44: "Then the equation holds by construction."
        held = ns_equation(44).move_to([0, -1.6, 0])
        ok = Text("holds by construction", font_size=26, color=GREEN).move_to([0, -2.5, 0])
        self.at(44, Write(held), run_time=1.4)
        self.play(FadeIn(ok), run_time=0.5)

        # cue 45: the challenge: choose a blowing-up u whose residual stays smooth
        self.at(45, *[FadeOut(m) for m in [pick, defn, left, held, ok]], run_time=0.5)
        ch1 = chip("choose a u that blows up", RED, 30, fill=0.08)
        ch2 = chip("whose residual f stays smooth", GREEN, 30, fill=0.08)
        pr = VGroup(ch1, ch2).arrange(DOWN, buff=0.5).move_to([0, 1.0, 0])
        self.play(FadeIn(ch1, shift=UP * 0.0), run_time=0.6)
        self.frac(45, 0.5, FadeIn(ch2, shift=UP * 0.0), run_time=0.6)

        # cue 46: each term diverges on its own: they have to cancel, term by term
        self.at(46, pr.animate.scale(0.7).move_to([0, 1.2, 0]), run_time=0.6)
        ax = Axes(x_range=[0.4, 1.0, 0.1], y_range=[-10, 10, 5], x_length=8.2, y_length=3.4,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([0, -0.7, 0])
        xs = np.linspace(0.4, 0.985, 400)
        pw = lambda t: (1 - t) ** -1.5
        fs = [(lambda t: 0.9 * pw(t), C_ACC, "u_t"),
              (lambda t: -1.4 * pw(t), C_NL, "advection"),
              (lambda t: 0.9 * pw(t), C_VISC, "viscous"),
              (lambda t: -0.4 * pw(t) + 1.0 + 0.5 * t, TEAL, "pressure")]
        total = lambda t: sum(f(t) for f, _, _ in fs)
        curves = [clipped_curve(ax, f, xs, 10, c, 3) for f, c, _ in fs]
        tot = clipped_curve(ax, total, xs, 10, WHITE, 5)
        labs = VGroup(*[note(nm, 18, c) for _, c, nm in fs]).arrange(RIGHT, buff=0.5).next_to(ax, DOWN, buff=0.25)
        tl = note("f = sum: smooth", 20, WHITE).move_to(ax.c2p(0.55, 2.8))
        sch = note("schematic: each diverges like tau^(-3/2); the sum does not", 18, GREY_B).move_to([0, -3.05, 0])
        self.play(Create(ax), FadeIn(labs), run_time=0.6)
        self.play(*[Create(c) for c in curves], run_time=1.6)
        self.frac(46, 0.65, Create(tot), FadeIn(tl), run_time=1.0)
        self.play(FadeIn(sch), run_time=0.4)
        self.finish(keep=[header])


# ==========================================================================
# N4_1 -- how the paper cancels the divergence, in layers   (cues 47-54)
# ==========================================================================
class N4_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N4_1"], END["N4_1"]

    def construct(self):
        header = hdr("The Flow")
        self.add(header)
        CC = np.array([-3.9, -0.45, 0.0])
        # top view: concentric layers around the axis
        outer = Annulus(inner_radius=1.75, outer_radius=2.35, color=TEAL, fill_color=TEAL, fill_opacity=0.22,
                        stroke_width=2).move_to(CC)
        ring = Annulus(inner_radius=1.1, outer_radius=1.75, color=RED, fill_color=RED, fill_opacity=0.25,
                       stroke_width=2).move_to(CC)
        inner = Circle(radius=1.1, color=BLUE, fill_color=BLUE, fill_opacity=0.3, stroke_width=3).move_to(CC)
        L = lambda s, c, y: at_left(note(s, 21, c), y, 0.2)

        # cue 47: the inner core of the profile solves the momentum balance
        l1 = L("inner core: solves the\nmomentum balance", BLUE_C, 2.3)
        self.at(47, FadeIn(inner), FadeIn(l1), run_time=0.9)
        # cue 48: exterior swirl solves the heat equation exactly, so needs no force
        l2 = L("exterior swirl: solves the heat\nequation exactly -> needs no force", TEAL, 1.35)
        self.at(48, FadeIn(outer), FadeIn(l2), run_time=0.9)
        # cue 49: joining them leaves a leftover ring, and the leftover blows up
        l3 = L("joining the two leaves a leftover\nin a ring ... and it blows up", RED, 0.4)
        self.at(49, FadeIn(ring), FadeIn(l3), run_time=0.9)
        self.play(ring.animate.set_fill(RED, opacity=0.6), rate_func=there_and_back, run_time=1.2)

        # cue 50: add many small, rapidly oscillating pulses on the ring
        ph = ValueTracker(0.0)
        ph.add_updater(lambda m, dt: m.increment_value(5.0 * dt))
        wav = always_redraw(lambda: poly(
            [CC + (1.43 + 0.12 * math.sin(16 * a + ph.get_value())) * np.array([math.cos(a), math.sin(a), 0])
             for a in np.linspace(0, TAU, 220)], YELLOW, 3, closed=True))
        l4 = L("add many small, rapidly\noscillating pulses", YELLOW, -0.55)
        self.at(50, FadeIn(l4), run_time=0.5)
        self.add(ph, wav)
        # cue 51: each oscillation averages to zero, but its momentum flux does not
        avg = tex(r"\langle u'\rangle=0,\qquad \langle u'\otimes u'\rangle\neq 0", 34, YELLOW)
        avg.move_to([3.4, -1.55, 0]); clamp_width(avg, 6.2)
        self.at(51, Write(avg), run_time=1.6)
        # cue 52: that flux supplies exactly the missing force
        l5 = L("that momentum flux supplies\nexactly the missing force", GREEN, -2.45)
        self.at(52, FadeIn(l5), ring.animate.set_fill(GREEN, opacity=0.25).set_stroke(GREEN), run_time=1.0)
        self.snap("layers")

        # cue 53: the background shear amplifies the pulses; viscosity eventually kills them
        self.at(53, *[FadeOut(m) for m in [l1, l2, l3, l4, l5, avg]], run_time=0.4)
        ax = Axes(x_range=[0, 1, 0.5], y_range=[0, 1.2, 0.5], x_length=5.4, y_length=2.4,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([3.4, 0.4, 0])
        amp = ax.plot(lambda t: 1.0 * (math.exp(5.5 * t) * math.exp(-9.0 * t * t)) / 1.45, x_range=[0, 1], color=YELLOW)
        al = note("pulse amplitude", 16, YELLOW).next_to(ax, UP, buff=0.05).align_to(ax, LEFT).shift(UP * 0.5+LEFT * 0.5)
        tl = note("time", 14, GREY_B).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT)
        sh = note("shear amplifies", 18, ORANGE).move_to(ax.c2p(0.2, 0.9)).shift(RIGHT * 0.5)
        vi = note("viscosity kills", 18, GREEN).move_to(ax.c2p(0.78, 0.8))
        self.play(Create(ax), FadeIn(al), FadeIn(tl), run_time=0.6)
        self.play(Create(amp), run_time=2.0)
        self.play(FadeIn(sh), FadeIn(vi), run_time=0.5)

        # cue 54: further corrections, order after order, until the leftover and all its derivatives are smooth
        self.at(54, *[FadeOut(m) for m in [ax, amp, al, tl, sh, vi]], run_time=0.4)
        corr = VGroup(*[chip(s, c, 22, fill=0.08) for s, c in
                        [("correction 1", PURPLE), ("correction 2", PURPLE), ("correction 3", PURPLE), ("...", GREY_B)]])
        corr.arrange(RIGHT, buff=0.3).move_to([3.3, 1.2, 0]); clamp_width(corr, 6.4)
        for i, c in enumerate(corr):
            self.frac(54, 0.05 + 0.13 * i, FadeIn(c, shift=UP * 0.15), run_time=0.4)
        fin = card("leftover and all its derivatives:\nsmooth through the singular time", GREEN, 22, 6.0).move_to([3.3, -0.6, 0])
        self.frac(54, 0.65, FadeIn(fin, shift=UP * 0.15), ring.animate.set_fill(GREEN, opacity=0.1), run_time=0.8)
        self.finish(keep=[header])


# ==========================================================================
# N4_2 -- what I can actually do: a manufactured solution   (cues 55-63)
# ==========================================================================
class N4_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["N4_2"], END["N4_2"]

    def construct(self):
        header = hdr("The Flow")
        self.add(header)
        # cue 55: actually simulating that is hard: the paper gives no closed formula
        q = tex(r"u(r,z,t)=\;?", 60, YELLOW).move_to([0, 1.6, 0])
        nf = Text("no closed formula in the paper", font_size=32, color=RED).move_to([0, 0.3, 0])
        self.at(55, Write(q), run_time=1.0)
        self.frac(55, 0.4, FadeIn(nf, shift=UP * 0.15), run_time=0.6)
        # cue 56: profiles built implicitly: matching conditions, an infinite correction cycle; force = what's left
        ch, ar = pipeline(["matching conditions", "infinite correction cycle", "force = whatever is left"],
                          [BLUE, PURPLE, C_FORCE], y=-1.5, size=22)
        self.at(56, FadeIn(ch[0]), run_time=0.5)
        self.frac(56, 0.25, GrowArrow(ar[0]), FadeIn(ch[1]), run_time=0.6)
        self.frac(56, 0.55, GrowArrow(ar[1]), FadeIn(ch[2]), run_time=0.6)

        # cue 57: what I can do is something more limited
        self.at(57, *[FadeOut(m) for m in [q, nf, ch, ar]], run_time=0.5)
        lim = Text("what I can do is more limited", font_size=40, color=YELLOW).move_to([0, 0.6, 0])
        self.play(FadeIn(lim), run_time=0.5)
        # cue 58: keep axisymmetry + the scaling laws, pick Gaussian profiles, define f as the residual
        self.at(58, FadeOut(lim), run_time=0.4)
        cs = [("keep the axisymmetric geometry", BLUE), ("keep the proposed scaling laws", TEAL),
              ("choose simple Gaussian profiles", ORANGE), ("define f as the Navier-Stokes residual", C_FORCE)]
        rows = VGroup(*[chip(s, c, 26, fill=0.08) for s, c in cs]).arrange(DOWN, buff=0.3).move_to([0, 0.5, 0])
        for r_, f in zip(rows, (0.04, 0.25, 0.47, 0.72)):
            self.frac(58, f, FadeIn(r_, shift=RIGHT * 0.2), run_time=0.5)

        # cue 59/60: a manufactured-solution test: does the solver reproduce the prescribed field and forcing?
        self.at(59, FadeOut(rows), run_time=0.4)
        mms = Text("a manufactured-solution test", font_size=42, color=YELLOW).move_to([0, 2.0, 0])
        self.play(FadeIn(mms, scale=0.95), run_time=0.6)
        ch2, ar2 = pipeline(["prescribe u, p", "f := residual", "solver(f)", "= prescribed u ?"],
                            [BLUE, C_FORCE, ORANGE, GREEN], y=0.2, size=22, gap=0.3)
        self.at(60, FadeIn(ch2[0]), run_time=0.4)
        for k in range(1, 4):
            self.frac(60, 0.2 + 0.2 * k, GrowArrow(ar2[k - 1]), FadeIn(ch2[k]), run_time=0.5)

        # cue 61: "...but this does not reproduce the paper's construction."
        nope = card("this is NOT the paper's construction", RED, 28, 8.0).move_to([0, -1.8, 0])
        self.at(61, FadeIn(nope, shift=UP * 0.15), run_time=0.6)
        # cue 62: in particular it does not guarantee that the forcing stays smooth as T is approached
        sm = Text("nothing guarantees that f stays smooth as t -> T", font_size=28, color=ORANGE).move_to([0, -2.8, 0])
        clamp_width(sm, 11.0)
        self.at(62, FadeIn(sm, shift=UP * 0.15), run_time=0.6)
        # cue 63: that is the property I need to measure rather than assume
        ms = chip("measure it, don't assume it", YELLOW, 30, fill=0.12).move_to([0, -0.9, 0])
        self.at(63, FadeIn(ms, scale=0.9), run_time=0.6)
        self.snap("measure")
        self.finish(keep=[header])


@lru_cache(maxsize=1)
def umax_curve():
    """max over (r, z) of the meridional speed |(u_r, u_z)| of the exact flow, t in [0.5, 0.98]."""
    R, Z = np.meshgrid(np.linspace(0, 1.6, 160), np.linspace(-1.6, 1.6, 320), indexing="ij")
    ts = np.linspace(0.5, 0.98, 25)
    out = []
    for t in ts:
        _, ur, uz = fields(R, Z, t)
        out.append(float(np.max(np.hypot(ur, uz))))
    return ts, np.array(out)


def domain_panel(origin, sc=0.78, rmax=3.0, zmax=3.0):
    """Frame of the (r, z) computational domain; origin = screen position of (r=0, z=0)."""
    O = np.array(origin, float)
    P = lambda r, z: [O[0] + sc * r, O[1] + sc * z, 0]
    rect = Rectangle(width=sc * rmax, height=sc * 2 * zmax, color=GREY_B, stroke_width=2)
    rect.move_to([O[0] + sc * rmax / 2, O[1], 0])
    return P, rect


# ==========================================================================
# N5 -- Model: axisymmetry and (u1, omega1, psi1)   (cues 64-70)
# ==========================================================================
class N5(SyncedScene):
    AUDIO_START, AUDIO_END = START["N5"], END["N5"]

    def construct(self):
        header = hdr("Model")
        # cue 64: "Now for the model, we start with the Navier-Stokes."
        ns = ns_equation(46).move_to([0, 1.3, 0])
        inc = tex(r"\nabla\cdot u=0", 42, GREY_B).move_to([0, -0.1, 0])
        self.at(64, Write(header), run_time=0.6)
        self.play(Write(ns), Write(inc), run_time=1.6)
        # cue 65: "Pretty obvious."
        ob = note("pretty obvious", 26, GREY_B).move_to([0, -1.3, 0])
        self.at(65, FadeIn(ob), run_time=0.4)

        # cue 66: assume axisymmetry, cylindrical coordinates: (r, z, t)
        O = np.array([-4.3, -1.3, 0.0])
        zax = Arrow(O, O + [0, 3.0, 0], buff=0, color=GREY_B, stroke_width=3, max_tip_length_to_length_ratio=0.1)
        rax = Arrow(O, O + [2.5, 0, 0], buff=0, color=GREY_B, stroke_width=3, max_tip_length_to_length_ratio=0.1)
        ell = ParametricFunction(lambda s: O + np.array([1.2 * math.cos(s), 0.42 * math.sin(s) + 1.5, 0]),
                                 t_range=[0, TAU], color=C_SWIRL, stroke_width=4)
        tip = chevron(O + np.array([1.2 * math.cos(2.0), 0.42 * math.sin(2.0) + 1.5, 0]),
                      np.array([-math.sin(2.0) * 1.2, math.cos(2.0) * 0.42, 0]), 0.12, C_SWIRL)
        lz, lr, lth = tex("z", 28, GREY_B).move_to(O + [0, 3.25, 0]), tex("r", 28, GREY_B).move_to(O + [2.75, 0, 0]), \
            tex(r"\theta", 28, C_SWIRL).move_to(O + [1.75, 2.1, 0])
        diag = VGroup(zax, rax, ell, tip, lz, lr, lth)
        sym = tex(r"\partial_\theta = 0", 46, C_SWIRL).move_to([2.2, 1.2, 0])
        cyl = note("cylindrical coordinates:  (r, z, t)", 26, GREY_B).move_to([2.2, 0.1, 0])
        self.at(66, *[FadeOut(m) for m in [ns, inc, ob]], run_time=0.5)
        self.play(FadeIn(diag), run_time=0.8)
        self.play(Write(sym), run_time=0.8)
        self.frac(66, 0.55, FadeIn(cyl), run_time=0.6)

        # cue 67: three unknowns remain: the swirl, the azimuthal vorticity, the stream function
        self.at(67, FadeOut(sym), FadeOut(cyl), run_time=0.4)
        c_a = chip("u_theta : the swirl", C_SWIRL, 26, fill=0.08)
        c_b = chip("omega_theta : the azimuthal vorticity", C_RAD, 26, fill=0.08)
        c_c = chip("psi : the stream function", PURPLE, 26, fill=0.08)
        chips = VGroup(c_a, c_b, c_c).arrange(DOWN, buff=0.45, aligned_edge=LEFT).move_to([2.3, 0.6, 0])
        clamp_width(chips, 7.2)
        self.play(FadeIn(c_a, shift=LEFT * 0.2), run_time=0.5)
        self.frac(67, 0.4, FadeIn(c_b, shift=LEFT * 0.2), run_time=0.5)
        self.frac(67, 0.7, FadeIn(c_c, shift=LEFT * 0.2), run_time=0.5)

        # cue 68: following Hou & Li, divide each by the appropriate power of r
        d1 = tex(r"u_1=\dfrac{u_\theta}{r}", 40, C_SWIRL)
        d2 = tex(r"\omega_1=\dfrac{\omega_\theta}{r}", 40, C_RAD)
        d3 = tex(r"\psi_1=\dfrac{\psi}{r^{2}}", 40, PURPLE)
        defs = VGroup(d1, d2, d3).arrange(DOWN, buff=0.5, aligned_edge=LEFT).move_to([2.3, 0.6, 0])
        hou = note("following earlier work of Hou and Li", 20, GREY_B).move_to([2.3, 3.0 - 0.9, 0]).shift(UP * 0.0)
        hou.next_to(defs, UP, buff=0.35)
        self.at(68, FadeOut(chips), run_time=0.4)
        self.play(FadeIn(hou), run_time=0.4)
        self.play(Write(d1), run_time=0.8)
        self.frac(68, 0.45, Write(d2), run_time=0.8)
        self.frac(68, 0.7, Write(d3), run_time=0.8)
        # cue 69: regular on the axis: no divide-by-zero errors
        reg = note("regular on the axis: no division by zero", 22, GREEN).next_to(defs, DOWN, buff=0.4)
        self.at(69, FadeIn(reg, shift=UP * 0.15), run_time=0.6)
        self.snap("defs")

        # cue 70: velocity recovered from psi_1; incompressibility is automatic
        self.at(70, *[FadeOut(m) for m in [defs, hou, reg]], run_time=0.5)
        v1 = tex(r"u_r=-r\,\partial_z\psi_1", 44, C_RAD)
        v2 = tex(r"u_z=2\psi_1+r\,\partial_r\psi_1", 44, C_AXIAL)
        vs = VGroup(v1, v2).arrange(DOWN, buff=0.55, aligned_edge=LEFT).move_to([2.3, 0.9, 0])
        auto = chip("incompressibility is then automatic:  div u = 0", GREEN, 24, fill=0.1).move_to([2.3, -1.2, 0])
        clamp_width(auto, 7.2)
        self.play(Write(v1), run_time=1.0)
        self.frac(70, 0.4, Write(v2), run_time=1.2)
        self.frac(70, 0.75, FadeIn(auto, shift=UP * 0.15), run_time=0.6)
        self.finish(keep=[header])


# ==========================================================================
# N5_1 -- the three equations, viscosity = 1, the exact toy flow   (cues 71-74)
# ==========================================================================
class N5_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N5_1"], END["N5_1"]

    def construct(self):
        header = hdr("Model")
        self.add(header)
        E1 = MathTex(r"\partial_t u_1+u_r\partial_r u_1+u_z\partial_z u_1", "-", r"2u_1\partial_z\psi_1", "=",
                     r"L_5 u_1", "+", "F_1", font_size=36)
        E2 = MathTex(r"\partial_t \omega_1+u_r\partial_r \omega_1+u_z\partial_z \omega_1", "-", r"\partial_z(u_1^2)",
                     "=", r"L_5 \omega_1", "+", "F_2", font_size=36)
        E3 = MathTex(r"-L_5\psi_1=\omega_1,\qquad L_5=\partial_r^2+\tfrac{3}{r}\partial_r+\partial_z^2", font_size=36)
        for E in (E1, E2, E3):
            clamp_width(E, 12.0)
        E1.move_to([0, 2.15, 0]); E2.move_to([0, 0.1, 0]); E3.move_to([0, -1.55, 0])
        E1[6].set_color(C_FORCE); E2[6].set_color(C_FORCE)
        # cue 71: "The first equation is conservation of angular momentum."
        n1 = note("conservation of angular momentum", 22, C_SWIRL).next_to(E1, DOWN, buff=0.2)
        self.at(71, Write(E1), run_time=1.6)
        self.play(FadeIn(n1), run_time=0.4)
        # cue 72: the vorticity equation; d_z(u1^2) is the centrifugal drive turning swirl into meridional flow
        n2 = note("vorticity equation", 22, C_RAD).next_to(E2, DOWN, buff=0.2)
        self.at(72, Write(E2), run_time=1.8)
        self.play(FadeIn(n2), run_time=0.4)
        box = SurroundingRectangle(E2[2], color=ORANGE, buff=0.08)
        cf = note("centrifugal drive: turns swirl into meridional flow", 20, ORANGE).next_to(box, UP, buff=0.55)
        cf.shift(RIGHT * 0.6)
        arrow_cf = Arrow(cf.get_bottom(), box.get_top(), buff=0.05, color=ORANGE, stroke_width=3,
                         max_tip_length_to_length_ratio=0.3)
        self.frac(72, 0.45, Create(box), FadeIn(cf), GrowArrow(arrow_cf), run_time=0.8)
        # cue 73: Poisson equation for psi_1; L5 is the Laplacian in five dimensions for radial functions
        n3 = note("Poisson equation for the stream function", 22, PURPLE).next_to(E3, DOWN, buff=0.2)
        n3b = note("L5: the Laplacian in five dimensions, acting on radial functions", 20, GREY_B).next_to(n3, DOWN, buff=0.2)
        self.at(73, Write(E3), run_time=1.6)
        self.play(FadeIn(n3), run_time=0.4)
        self.frac(73, 0.6, FadeIn(n3b), run_time=0.5)
        self.snap("equations")

        # cue 74: viscosity set to one; the force defined symbolically; exact form with tau = 1 - t
        self.at(74, *[FadeOut(m) for m in [E1, E2, E3, n1, n2, n3, n3b, box, cf, arrow_cf]], run_time=0.5)
        nu = chip("viscosity set to 1  (any other nu: rescale lengths by sqrt(nu))", C_VISC, 22, fill=0.08)
        nu.move_to([0, 2.15, 0]); clamp_width(nu, 11.0)
        ff = tex(r"F_1=\frac{f_\theta}{r},\qquad F_2=\frac{(\nabla\times f)_\theta}{r}", 34, C_FORCE).move_to([0, 1.1, 0])
        sol = MathTex(r"u_1\;\propto\;\tau^{-1-h}\,e^{-\rho^2-\zeta^2}", font_size=36).move_to([0, -0.15, 0])
        sol2 = MathTex(r"\psi_1\;\propto\;\tau^{-1/2-h}\,\zeta\,e^{-\rho^2-\zeta^2}", font_size=36).move_to([0, -1.1, 0])
        sol3 = MathTex(r"\rho=\frac{r}{\tau^{1/2}},\quad \zeta=\frac{z}{\tau^{1/2-h}},\quad \tau=1-t", font_size=36)
        sol3.move_to([0, -2.2, 0])
        sol[0].set_color(C_SWIRL); sol2[0].set_color(PURPLE)
        ex = note("the force follows exactly, from a symbolic calculator", 20, GREY_B).move_to([0, 0.45, 0])
        self.play(FadeIn(nu, shift=DOWN * 0.15), run_time=0.5)
        self.frac(74, 0.2, Write(ff), run_time=1.2)
        self.frac(74, 0.35, FadeIn(ex), run_time=0.4)
        self.frac(74, 0.5, Write(sol), run_time=1.3)
        self.frac(74, 0.64, Write(sol2), run_time=1.3)
        self.frac(74, 0.8, Write(sol3), run_time=1.3)
        self.snap("exact")
        self.finish(keep=[header])


# ==========================================================================
# N6 -- Conditions: switch-on, domain, boundaries   (cues 75-80)
# ==========================================================================
class N6(SyncedScene):
    AUDIO_START, AUDIO_END = START["N6"], END["N6"]

    def construct(self):
        header = hdr("Conditions and Numerics")
        self.add(header)
        # cue 75: fluid starts at rest until t0 = 0.3
        ax = Axes(x_range=[0, 1.05, 0.25], y_range=[0, 1.15, 0.5], x_length=6.4, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([-3.0, -0.3, 0])
        xl = tex("t", 26, GREY_B).next_to(ax, DOWN, buff=0.1).align_to(ax, RIGHT)
        yl = tex(r"s(t)", 28, C_FORCE).next_to(ax, UP, buff=0.1).align_to(ax, LEFT)
        flat = ax.plot(lambda t: 0.0, x_range=[0, 0.3], color=C_FORCE, stroke_width=5)
        rest = note("at rest until t0 = 0.3", 22, GREY_B).move_to(ax.c2p(0.15, 0.22)).shift(DOWN * 1.0)
        self.at(75, Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(Create(flat), FadeIn(rest), run_time=1.2)
        # cue 76: a smooth function multiplies the whole flow between 0.3 and 0.5; h, amplitudes
        ramp = ax.plot(lambda t: float(switch(t)), x_range=[0.3, 1.0, 0.004], color=C_FORCE, stroke_width=5)
        t1 = DashedLine(ax.c2p(0.3, 0), ax.c2p(0.3, 1.0), color=GREY_C, dash_length=0.08)
        t2 = DashedLine(ax.c2p(0.5, 0), ax.c2p(0.5, 1.0), color=GREY_C, dash_length=0.08)
        lab01 = note("0.3", 15, GREY_B).next_to(ax.c2p(0.3, 0), DOWN, buff=0.1)
        lab05 = note("0.5", 15, GREY_B).next_to(ax.c2p(0.5, 0), DOWN, buff=0.1)
        smooth = note("smooth switch-on\nmultiplies the whole flow", 20, C_FORCE).move_to(ax.c2p(0.78, 0.5))
        Tl = DashedLine(ax.c2p(1, 0), ax.c2p(1, 1.1), color=RED, dash_length=0.08)
        Tlab = tex("T", 24, RED).next_to(ax.c2p(1, 0), DOWN, buff=0.1)
        self.at(76, Create(t1), Create(t2), FadeIn(lab01), FadeIn(lab05), run_time=0.5)
        self.play(Create(ramp), FadeIn(smooth), run_time=1.6)
        self.play(Create(Tl), FadeIn(Tlab), run_time=0.4)
        ch = VGroup(chip("h = 0.01  (anisotropy exponent)", BLUE, 22, fill=0.08),
                    chip("swirl amplitude  8", C_SWIRL, 22, fill=0.08),
                    chip("stream-function amplitude  5", PURPLE, 22, fill=0.08)).arrange(DOWN, buff=0.35, aligned_edge=LEFT)
        ch.move_to([4.2, 0.1, 0])
        for c_, f in zip(ch, (0.45, 0.62, 0.8)):
            self.frac(76, f, FadeIn(c_, shift=LEFT * 0.2), run_time=0.5)
        self.snap("switch")

        # cue 77: domain r in [0, 3], z in [-3, 3]
        self.at(77, *[FadeOut(m) for m in [ax, xl, yl, flat, rest, ramp, t1, t2, lab01, lab05, smooth, Tl, Tlab, ch]],
                run_time=0.5)
        P, rect = domain_panel([-4.6, -0.35, 0])
        rl = VGroup(tex("0", 22, GREY_B).move_to(P(0, -3.25)), tex("3", 22, GREY_B).move_to(P(3, -3.25)),
                    tex("r", 26, GREY_B).move_to(P(1.5, -3.5)))
        zl = VGroup(tex("-3", 22, GREY_B).move_to(P(-0.4, -3)), tex("3", 22, GREY_B).move_to(P(-0.3, 3)),
                    tex("z", 26, GREY_B).move_to(P(-0.4, 0)))
        dom = at_left(note("domain:  r in [0, 3],  z in [-3, 3]", 22, YELLOW), 2.2, -1.5)
        self.play(Create(rect), FadeIn(rl), FadeIn(zl), FadeIn(dom), run_time=1.0)
        # cue 78: on the axis the fields are even in r
        axis = DashedLine(P(0, -3), P(0, 3), color=GREEN, dash_length=0.1, stroke_width=5)
        a_n = at_left(note("axis:  fields even in r", 22, GREEN), 1.2, -1.5)
        self.at(78, Create(axis), FadeIn(a_n), run_time=0.7)
        # cue 79: outer wall, top and bottom: zero
        walls = VGroup(Line(P(3, -3), P(3, 3)), Line(P(0, 3), P(3, 3)), Line(P(0, -3), P(3, -3)))
        walls.set_color(RED).set_stroke(width=6)
        w_n = at_left(note("outer wall, top, bottom:  fields = 0", 22, RED), 0.2, -1.5)
        self.at(79, Create(walls), FadeIn(w_n), run_time=0.7)

        # cue 80: Gaussian tails at the walls are below 1e-5 of the peak, so zero is consistent
        tau0 = 1 - T0
        lr = lambda r: -0.4343 * r * r / tau0
        lz = lambda z: -0.4343 * z * z / tau0 ** (2 * A_Z)
        ax2 = Axes(x_range=[0, 3, 1], y_range=[-7, 0, 1], x_length=4.4, y_length=2.6,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([3.7, -1.55, 0])
        cr = ax2.plot(lambda x: lr(x), x_range=[0, 3], color=C_SWIRL)
        cz = ax2.plot(lambda x: lz(x), x_range=[0, 3], color=C_RAD)
        thr = DashedLine(ax2.c2p(0, -5), ax2.c2p(3, -5), color=YELLOW, dash_length=0.1)
        thl = note("10^-5 of the peak", 14, YELLOW).move_to(ax2.c2p(0.9, -4.5))
        cl_r = note("in r (orange) and in z (teal)", 14, C_SWIRL).move_to(ax2.c2p(1.2, -2.6))
        ty = note("log10 of the Gaussian, at t0 = 0.3", 14, GREY_B).next_to(ax2, UP, buff=0.05)
        _m, _e = f"{math.exp(-9 / tau0):.1e}".split("e")
        val = tex(rf"e^{{-9/0.7}}\approx {_m}\times10^{{{int(_e)}}}", 30, YELLOW).move_to([4.5, 0.9, 0])
        self.at(80, FadeIn(ty), Create(ax2), run_time=0.6)
        self.play(Create(cr), Create(cz), Create(thr), FadeIn(thl), FadeIn(cl_r), run_time=1.4)
        self.frac(80, 0.55, Write(val), run_time=0.9)
        self.snap("domain")
        self.finish(keep=[header])


# ==========================================================================
# N6_1 -- Numerics: grid, flux form, Cholesky, RK4, step limits   (cues 81-88)
# ==========================================================================
class N6_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N6_1"], END["N6_1"]

    def construct(self):
        header = hdr("Conditions and Numerics")
        self.add(header)
        # cue 81: cell-centred uniform grid, 120 x 240
        P, rect = domain_panel([-5.2, -0.35, 0])
        vl = VGroup(*[Line(P(3 * i / 12, -3), P(3 * i / 12, 3), color=GREY_D, stroke_width=1) for i in range(1, 12)])
        hl = VGroup(*[Line(P(0, -3 + 6 * j / 24), P(3, -3 + 6 * j / 24), color=GREY_D, stroke_width=1) for j in range(1, 24)])
        gl = note("120 x 240 cells (every 10th line drawn)", 15, GREY_B).move_to(P(1.5, -3.4))
        self.at(81, Create(rect), run_time=0.5)
        self.play(Create(vl), Create(hl), FadeIn(gl), run_time=1.6)
        self.snap("grid")
        X = 0.0
        # cue 82: flux form with an r^3 weight
        f1 = tex(r"L_5u=\frac{1}{r^3}\,\partial_r\!\left(r^3\,\partial_r u\right)+\partial_z^2u", 34, C_NL)
        f1.move_to([2.3, 2.0, 0]); clamp_width(f1, 8.4)
        fl = note("flux form with an r^3 weight", 22, GREY_B).next_to(f1, DOWN, buff=0.25)
        self.at(82, Write(f1), run_time=1.6)
        self.play(FadeIn(fl), run_time=0.4)
        # cue 83: on the axis the flux vanishes automatically
        ax_hi = Rectangle(width=0.08, height=4.68, color=GREEN, fill_color=GREEN, fill_opacity=0.5, stroke_width=0)
        ax_hi.move_to(P(0.0, 0.0))
        z_n = note("r^3 -> 0 on the axis:\nthe flux vanishes automatically", 22, GREEN).move_to([2.3, 0.5, 0])
        self.at(83, FadeIn(ax_hi), FadeIn(z_n), run_time=0.6)
        # cue 84: everything else is second-order central differences
        c_n = note("everything else: second-order central differences", 22, TEAL).move_to([2.3, -0.7, 0])
        self.at(84, FadeIn(c_n), run_time=0.6)

        # cue 85: Poisson problem is SPD once multiplied by r^3
        self.at(85, *[FadeOut(m) for m in [f1, fl, z_n, c_n]], run_time=0.5)
        spd = tex(r"-L_5\psi_1=\omega_1\ \Longrightarrow\ A\psi=b,\quad A=A^{\!\top}\succ0\ \ (\text{after}\times r^3)", 32, PURPLE)
        spd.move_to([2.3, 2.0, 0]); clamp_width(spd, 8.4)
        self.play(Write(spd), run_time=1.6)
        # cue 86: banded Cholesky, factor once, each solve = two triangular sweeps
        M = lambda cx: np.array([cx, 0.2, 0.0])
        sq = lambda cx: Square(side_length=1.5, color=GREY_B, stroke_width=2).move_to(M(cx))

        def band(cx):
            c = M(cx)
            return Polygon(c + [-0.75, 0.75, 0], c + [-0.35, 0.75, 0], c + [0.75, -0.35, 0],
                           c + [0.75, -0.75, 0], c + [0.35, -0.75, 0], c + [-0.75, 0.35, 0],
                           color=PURPLE, fill_color=PURPLE, fill_opacity=0.4, stroke_width=1)

        def lower(cx):
            c = M(cx)
            return Polygon(c + [-0.75, 0.75, 0], c + [-0.75, -0.75, 0], c + [0.75, -0.75, 0], c + [0.75, -0.35, 0],
                           c + [-0.35, 0.75, 0], color=BLUE, fill_color=BLUE, fill_opacity=0.4, stroke_width=1)

        def upper(cx):
            c = M(cx)
            return Polygon(c + [-0.75, 0.75, 0], c + [0.75, 0.75, 0], c + [0.75, -0.75, 0], c + [0.35, -0.75, 0],
                           c + [-0.75, 0.35, 0], color=ORANGE, fill_color=ORANGE, fill_opacity=0.4, stroke_width=1)

        cxA, cxL, cxU = 0.4, 3.0, 5.2
        mA = VGroup(sq(cxA), band(cxA)); mL = VGroup(sq(cxL), lower(cxL)); mU = VGroup(sq(cxU), upper(cxU))
        eqs = VGroup(tex("A", 34, PURPLE).move_to(M(cxA) + [0, 1.2, 0]), tex("=", 34).move_to(M((cxA + cxL) / 2 + 0.1)),
                     tex("L", 34, BLUE).move_to(M(cxL) + [0, 1.2, 0]), tex(r"L^{\!\top}", 34, ORANGE).move_to(M(cxU) + [0, 1.2, 0]))
        t86 = note("banded Cholesky: factor once;\neach solve is two triangular sweeps", 22, GREY_B).move_to([2.8, -1.7, 0])
        self.at(86, FadeIn(mA), FadeIn(eqs[0]), FadeIn(eqs[1]), run_time=0.6)
        self.play(FadeIn(mL), FadeIn(eqs[2]), FadeIn(mU), FadeIn(eqs[3]), run_time=0.8)
        self.play(FadeIn(t86), run_time=0.5)

        # cue 87: classical RK4
        self.at(87, *[FadeOut(m) for m in [spd, mA, mL, mU, eqs, t86]], run_time=0.5)
        rk = tex(r"u^{n+1}=u^n+\frac{\Delta t}{6}\left(k_1+2k_2+2k_3+k_4\right)", 36, C_NL)
        rk.move_to([2.3, 1.6, 0]); clamp_width(rk, 8.4)
        rkn = note("classical RK4", 26, GREY_B).next_to(rk, DOWN, buff=0.3)
        self.play(Write(rk), FadeIn(rkn), run_time=1.4)

        # cue 88: explicit viscous stability (~1e-4) and the advective CFL as velocity grows
        self.at(88, rk.animate.scale(0.7).move_to([2.3, 2.25, 0]), FadeOut(rkn), run_time=0.5)
        ts, um = umax_curve()
        dt_cfl = 0.025 / um                       # Delta = 3/120 = 0.025
        lg = np.log10(dt_cfl)
        lo, hi = min(-4.0, lg.min()) - 0.5, lg.max() + 0.4
        ax = Axes(x_range=[0.5, 0.98, 0.1], y_range=[lo, hi, 1], x_length=6.0, y_length=2.5,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([2.5, -0.55, 0])
        visc = DashedLine(ax.c2p(0.5, -4), ax.c2p(0.98, -4), color=C_VISC, dash_length=0.12, stroke_width=4)
        cfl = poly([ax.c2p(t, y) for t, y in zip(ts, lg)], C_NL, 4)
        vl_ = note("explicit viscous stability:  about 1e-4", 18, C_VISC).move_to(ax.c2p(0.74, -4.0) + [0, -0.3, 0])
        cl_ = note("advective CFL: Delta/max|u|,  shrinks as |u| grows", 16, C_NL).move_to(ax.c2p(0.74, 0.5 * (lg.min() - 4.0)))
        yl_ = note("time-step limit (log scale)", 14, GREY_B).next_to(ax, UP, buff=0.05).align_to(ax, LEFT)
        xl_ = note("t", 14, GREY_B).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT)
        self.play(Create(ax), FadeIn(yl_), FadeIn(xl_), run_time=0.6)
        self.frac(88, 0.2, Create(visc), FadeIn(vl_), run_time=0.8)
        self.frac(88, 0.5, Create(cfl), FadeIn(cl_), run_time=1.6)
        self.snap("dt")
        self.finish(keep=[header])


# ==========================================================================
# N7 -- Validation: residual check and convergence   (cues 89-96)
# ==========================================================================
class N7(SyncedScene):
    AUDIO_START, AUDIO_END = START["N7"], END["N7"]

    def construct(self):
        header = hdr("Validation")
        # cue 89: "Alright, now we need to work on actually validating our model."
        self.at(89, Write(header), run_time=0.6)
        v = Text("validating the model", font_size=46, color=YELLOW).move_to([0, 0.4, 0])
        self.sync(CUES[89].start + 0.7)
        self.play(FadeIn(v, scale=0.95), run_time=0.7)

        # cue 90: first test: no time stepping at all
        t1 = chip("test 1: no time stepping at all", BLUE, 30, fill=0.08).move_to([0, 2.0, 0])
        self.at(90, FadeOut(v), FadeIn(t1, shift=DOWN * 0.15), run_time=0.6)
        # cue 91: build the same flow in Cartesian coordinates
        axis_box = card("axisymmetric flow\n(r, z)\nforce F from the code", C_SWIRL, 22, 4.2).move_to([-3.6, 0.2, 0])
        cart_box = card("same flow in Cartesian\n(x, y, z)\nfull 3D residual, symbolic", C_RAD, 22, 4.2).move_to([3.6, 0.2, 0])
        self.at(91, FadeIn(cart_box, shift=LEFT * 0.2), run_time=0.7)
        # cue 92: compute the full 3D NS residual symbolically; compare with the axisymmetric force
        self.at(92, FadeIn(axis_box, shift=RIGHT * 0.2), run_time=0.7)
        cmp_ = DoubleArrow(axis_box.get_right(), cart_box.get_left(), buff=0.15, color=YELLOW, stroke_width=4)
        cq = Text("compare", font_size=24, color=YELLOW).next_to(cmp_, UP, buff=0.12)
        self.play(GrowArrow(cmp_), FadeIn(cq), run_time=0.7)
        # cue 93: they agree to about 1e-13
        agree = tex(r"\max|F_{\rm axi}-F_{\rm 3D}|\ \sim\ 10^{-13}", 44, GREEN).move_to([0, -1.6, 0])
        self.at(93, Write(agree), run_time=1.4)
        # cue 94: that checks the equations and the code generation
        k1 = chip("checks the equations", GREEN, 24, fill=0.08); k2 = chip("checks the code generation", GREEN, 24, fill=0.08)
        kk = VGroup(k1, k2).arrange(RIGHT, buff=0.7).move_to([0, -2.7, 0])
        self.at(94, FadeIn(k1, shift=UP * 0.15), run_time=0.5)
        self.frac(94, 0.45, FadeIn(k2, shift=UP * 0.15), run_time=0.5)
        self.snap("residual")

        # cue 95: second test: run the solver, compare with the exact flow, 80x160 -> 120x240
        self.at(95, *[FadeOut(m) for m in [t1, axis_box, cart_box, cmp_, cq, agree, kk]], run_time=0.5)
        t2 = chip("test 2: run the solver, compare with the exact flow", BLUE, 28, fill=0.08).move_to([0, 2.1, 0])
        self.play(FadeIn(t2, shift=DOWN * 0.15), run_time=0.5)
        L = lambda n: math.log10(n)
        ax = Axes(x_range=[L(60), L(160), 0.1], y_range=[-0.35, 0.5, 0.25], x_length=6.0, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([-2.4, -0.7, 0])
        p80, p120 = ax.c2p(L(80), math.log10(1.7)), ax.c2p(L(120), math.log10(0.7))
        d80, d120 = Dot(p80, color=ORANGE, radius=0.09), Dot(p120, color=ORANGE, radius=0.09)
        ref = DashedLine(p80, ax.c2p(L(120), math.log10(1.7) - 2 * (L(120) - L(80))), color=GREY_B, dash_length=0.1)
        l80 = note("80 x 160:  1.7%", 18, ORANGE).next_to(d80, UR, buff=0.12)
        l120 = note("120 x 240:  0.7%", 18, ORANGE).next_to(d120, UR, buff=0.12)
        lref = note("slope -2\n(second order)", 16, GREY_B).next_to(ref, DOWN, buff=0.1).shift(RIGHT * 0.3)
        xl = note("grid resolution N_r (log)", 14, GREY_B).next_to(ax, DOWN, buff=0.05)
        yl = note("swirl error at the end (log)", 14, GREY_B).next_to(ax, UP, buff=0.05).align_to(ax, LEFT)
        self.play(Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.6)
        self.play(FadeIn(d80), FadeIn(l80), FadeIn(d120), FadeIn(l120), run_time=0.8)
        self.play(Create(ref), FadeIn(lref), run_time=0.6)
        # cue 96: error 1.7% -> 0.7%: factor ~2.4 where second order predicts 2.25
        r1 = tex(rf"\text{{measured ratio}}\ \frac{{1.7}}{{0.7}}\approx {1.7 / 0.7:.1f}", 32, ORANGE).move_to([4.1, 0.6, 0])
        r2 = tex(r"\text{second order:}\ \left(\frac{120}{80}\right)^{2}=2.25", 32, GREY_B).move_to([4.1, -0.7, 0])
        clamp_width(r1, 5.2); clamp_width(r2, 5.2)
        self.at(96, Write(r1), run_time=1.2)
        self.frac(96, 0.5, Write(r2), run_time=1.2)
        self.snap("convergence")
        self.finish(keep=[header])


# ==========================================================================
# N7_1 -- Validation: the exponents   (cues 97-103)
# ==========================================================================
class N7_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["N7_1"], END["N7_1"]

    def construct(self):
        header = hdr("Validation")
        self.add(header)
        # cue 97: "Third, the exponents."
        t3 = chip("test 3: the exponents", BLUE, 30, fill=0.08).move_to([0, 2.1, 0])
        self.at(97, FadeIn(t3, shift=DOWN * 0.15), run_time=0.6)
        cx = [-4.6, 1.0, 4.3]
        hd = VGroup(note("fitted slope in tau", 20, GREY_B).move_to([cx[0] + 1.3, 1.35, 0]),
                    note("expected", 20, GREY_B).move_to([cx[1], 1.35, 0]),
                    note("measured", 20, GREY_B).move_to([cx[2], 1.35, 0]))
        rule = Line([-6.2, 1.05, 0], [6.2, 1.05, 0], color=GREY_D, stroke_width=1.5)
        self.play(FadeIn(hd), Create(rule), run_time=0.6)
        rows = [("swirl peak", r"-0.51", r"-0.508", C_SWIRL, 98),
                ("core radius", r"+0.50", r"+0.500", C_RAD, 99),
                ("core height", r"+0.49", r"+0.494", C_AXIAL, 99),
                ("kinetic energy", r"+0.47\;(=\tfrac12-3h)", r"+0.470", YELLOW, 100),
                ("axial and radial speed", r"-0.51\ /\ -0.50", r"\approx -0.49", RED, 101)]
        grp = {}
        for k, (name, exp_, mea, col, cue) in enumerate(rows):
            y = 0.55 - 0.68 * k
            lab = at_left(Text(name, font_size=24, color=col), y, cx[0])
            e_ = tex(exp_, 30, GREY_B).move_to([cx[1], y, 0])
            m_ = tex(mea, 32, col).move_to([cx[2], y, 0])
            grp[k] = VGroup(lab, e_, m_)
            if k == 2:
                self.play(FadeIn(grp[k], shift=RIGHT * 0.15), run_time=0.5)   # same cue 99, second beat
            else:
                self.at(cue, FadeIn(grp[k], shift=RIGHT * 0.15), run_time=0.6)
        # cue 102: a bit off; they move toward the expected values with grid refinement
        ar = at_left(note("a bit off: moves toward the expected values as the grid is refined", 20, ORANGE), -2.95, cx[0])
        clamp_width(ar, 11.0)
        self.at(102, FadeIn(ar, shift=UP * 0.1), run_time=0.6)
        self.snap("table")

        # cue 103: by the end the core is only five cells wide
        self.at(103, *[FadeOut(m) for m in [t3, hd, rule, ar, *grp.values()]], run_time=0.5)
        w = 0.62
        cells = VGroup(*[Square(side_length=w, color=GREY_B, stroke_width=1.5).move_to([(i - 6.5) * w, -1.0, 0])
                         for i in range(14)])
        bump = ParametricFunction(lambda s: np.array([s, -1.0 + w / 2 + 1.7 * math.exp(-(s / (1.2 * w)) ** 2 * 0.9), 0]),
                                  t_range=[-6.5 * w, 6.5 * w], color=C_SWIRL, stroke_width=4)
        five = VGroup(*[cells[i] for i in range(4, 9)]).copy().set_stroke(YELLOW, width=4)
        five_l = tex(r"\approx 5\ \text{cells}", 36, YELLOW).move_to([0, 1.5, 0])
        end_l = note("the core by the end of the run", 22, GREY_B).move_to([0, 2.3, 0])
        self.play(Create(cells), run_time=0.6)
        self.play(Create(bump), FadeIn(end_l), run_time=0.8)
        self.play(Create(five), Write(five_l), run_time=0.8)
        self.finish(keep=[header])


# ==========================================================================
# N7_2 -- Validation: the picture (similarity collapse)   (cues 104-107)
# ==========================================================================
class N7_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["N7_2"], END["N7_2"]

    def construct(self):
        header = hdr("Validation")
        self.add(header)
        W, Hh = 2.3, 4.2
        CL, CR = np.array([-3.4, -0.45, 0.0]), np.array([3.4, -0.45, 0.0])
        tt = ValueTracker(0.45)
        VMAX = float(swirl_peak(0.98)) * 1.02

        NR, NZ = 36, 64
        show_r = ValueTracker(0.0)
        left = HeatGrid(W, Hh, NR, NZ).move_to(CL)
        right = HeatGrid(W, Hh, NR, NZ).move_to(CR)

        def upd_left(m):
            m.set_values(heat_values((0, 1.3), (-1.5, 1.5),
                                     lambda R, Z: swirl_field(R, Z, tt.get_value()), NR, NZ), VMAX)

        def upd_right(m):
            t_ = tt.get_value()
            pk_ = max(float(swirl_peak(t_)), 1e-9)

            def fn(R, Z):
                pr, pz = physical(R, Z, t_)
                return swirl_field(pr, pz, t_) / pk_
            m.set_values(heat_values((0, 2.4), (-2.4, 2.4), fn, NR, NZ), 1.0, opacity=show_r.get_value())
        upd_left(left); upd_right(right)
        left.add_updater(upd_left); right.add_updater(upd_right)
        fl = Rectangle(width=W, height=Hh, color=GREY_C, stroke_width=2).move_to(CL)
        fr = Rectangle(width=W, height=Hh, color=GREY_C, stroke_width=2).move_to(CR)
        ll = note("real coordinates (fixed window)", 20, GREY_B).next_to(fl, UP, buff=0.12)
        lr = note("rescaled by the blow-up rates", 20, GREY_B).next_to(fr, UP, buff=0.12)
        pk = always_redraw(lambda: VGroup(note("peak swirl", 18, GREY_B),
                                          DecimalNumber(float(swirl_peak(tt.get_value())), num_decimal_places=1,
                                                        font_size=30, color=C_SWIRL)
                                          ).arrange(RIGHT, buff=0.15).next_to(fl, DOWN, buff=0.12))
        # cue 104: "And here's the picture."
        self.add(left, pk)
        self.at(104, FadeIn(fl), FadeIn(ll), run_time=0.8)
        # cue 105: real coordinates: brighter, faster, thinner; rescaling by the blow-up rates
        self.add(right)
        self.sync(CUES[105].start)
        self.play(tt.animate(rate_func=linear).set_value(0.70), run_time=3.2)
        self.play(tt.animate(rate_func=linear).set_value(0.98), show_r.animate.set_value(1.0),
                  FadeIn(fr), FadeIn(lr), run_time=3.0)
        # cue 106: ... and it is the same shape at every time
        same = note("the same shape at every time", 26, YELLOW).move_to([0, 2.3, 0])
        self.at(106, FadeIn(same), run_time=0.5)
        # cue 107: largely validates the results
        ok = chip("largely validates\nthe solver", GREEN, 24, fill=0.1).move_to([0.45, 0.0, 0])
        self.at(107, FadeIn(ok, scale=0.9), run_time=0.6)
        self.snap("collapse")
        self.finish(keep=[header])


# --------------------------------------------------------------------------
# The 45%-of-peak swirl surface, in similarity coordinates (rho, zeta).
# u_theta = r*u1 = rho*exp(-rho^2 - zeta^2) (x tau^(-1/2-h)).  The level set at
# 45% of the maximum is a closed curve in the (rho, zeta) half plane; spinning
# it about the axis gives a torus-like shell (hollow: u_theta vanishes on the axis).
# --------------------------------------------------------------------------
@lru_cache(maxsize=1)
def shell_curve(frac=0.45, n=96):
    g = lambda x: x * np.exp(-x * x)
    gmax = g(1 / math.sqrt(2))
    c = frac * gmax
    zm = math.sqrt(math.log(gmax / c))
    th = np.linspace(-PI / 2, 3 * PI / 2, n + 1)
    zeta = zm * np.sin(th)
    target = c * np.exp(zeta ** 2)
    outer = np.cos(th) >= 0
    lo = np.where(outer, 1 / math.sqrt(2), 0.0)
    hi = np.where(outer, 6.0, 1 / math.sqrt(2))
    for _ in range(70):
        mid = 0.5 * (lo + hi)
        right = np.where(outer, g(mid) > target, g(mid) < target)
        lo = np.where(right, mid, lo)
        hi = np.where(right, hi, mid)
    rho = 0.5 * (lo + hi)
    uz = 2 * A_PSI * zeta * (1 - rho ** 2) * np.exp(-rho ** 2 - zeta ** 2)   # axial velocity on the shell (shape only)
    uz_n = uz / np.max(np.abs(uz))
    return th, rho, zeta, uz_n


def hexcol(rgb_row):
    return "#%02x%02x%02x" % tuple(int(x) for x in rgb_row[:3])


def make_shell(phi_range=(PI / 2, TAU), colored=True, opacity=0.92, nu=44, nv=40, gray="#8a93a6"):
    """Unit similarity shell (rho, zeta) -> (x, y, z) with z up; faces coloured by axial velocity
    (red up, blue down) or flat gray.  Scale / move it afterwards."""
    th, rho, zeta, uz_n = shell_curve()

    def func(u, v):
        R = np.interp(u, th, rho)
        Zz = np.interp(u, th, zeta)
        return np.array([R * math.cos(v), R * math.sin(v), Zz])

    surf = Surface(func, u_range=[th[0], th[-1]], v_range=list(phi_range), resolution=(nu, nv),
                   fill_opacity=opacity, stroke_width=0.25, stroke_color="#0b0d12", checkerboard_colors=False)
    for face in surf:
        um = 0.5 * (face.u1 + face.u2)
        if colored:
            col = hexcol(diverging_array(np.array([np.interp(um, th, uz_n)]))[0])
        else:
            col = gray
        face.set_fill(col, opacity=opacity)
    return surf


def shell_xform(sx, sz, center):
    """Matrix + shift mapping the unit shell to physical size (sx in r, sz in z)."""
    M = np.diag([sx, sx, sz])
    return M, np.array(center, float)


def placed_shell(base, sx, sz, center):
    M, c = shell_xform(sx, sz, center)
    return base.copy().apply_matrix(M).shift(c)


# ==========================================================================
# N8 -- Results: four extra views, and the warning   (cues 108-112)
# ==========================================================================
class N8(SyncedScene):
    AUDIO_START, AUDIO_END = START["N8"], END["N8"]

    def construct(self):
        header = hdr("Results")
        # cue 108: now to look at the results from some more angles
        self.at(108, Write(header), run_time=0.6)
        t108 = Text("once the solver matched the exact flow:\nlook at it from more angles", font_size=34,
                    color=YELLOW, line_spacing=0.95).move_to([0, 0.6, 0])
        self.sync(CUES[108].start + 0.9)
        self.play(FadeIn(t108, shift=UP * 0.15), run_time=0.8)

        # cue 109: four extra views
        cs = [("1   a 3D picture of the vortex", C_SWIRL), ("2   tracer particles", C_RAD),
              ("3   the balance of terms", C_VISC), ("4   switching the force off", C_FORCE)]
        cards = VGroup(*[card(s, c, 26, 5.4) for s, c in cs])
        cards.arrange_in_grid(2, 2, buff=(0.5, 0.45)).move_to([0, 0.4, 0])
        self.at(109, FadeOut(t108), run_time=0.4)
        for c_, f in zip(cards, (0.0, 0.25, 0.5, 0.75)):
            self.frac(109, f + 0.1, FadeIn(c_, shift=UP * 0.15), run_time=0.5)

        # cue 110-112: the warning
        warn = Text("these describe MY simplified flow", font_size=36, color=ORANGE).move_to([0, -1.7, 0])
        self.at(110, cards.animate.set_opacity(0.45), FadeIn(warn, shift=UP * 0.15), run_time=0.6)
        w2 = Text("the manufactured one", font_size=32, color=ORANGE).move_to([0, -2.3, 0])
        self.at(111, FadeIn(w2, shift=UP * 0.15), run_time=0.5)
        w3 = card("not claims about OpenAI's solution", RED, 30, 7.6).move_to([0, -3.0 + 0.0, 0])
        w3.move_to([0, -2.95, 0]).scale(0.85)
        self.at(112, FadeIn(w3, scale=0.95), run_time=0.6)
        self.snap("warning")
        self.finish(keep=[header])


# ==========================================================================
# N8_1 -- 3D: spin the grid, the 45% swirl surface, quarter cut   (cues 113-118)
# ==========================================================================
class N8_1(Synced3D):
    AUDIO_START, AUDIO_END = START["N8_1"], END["N8_1"]

    def construct(self):
        header = hdr("Results")
        self.hud(header, show=True)
        self.set_camera_orientation(phi=90 * DEGREES, theta=-90 * DEGREES, zoom=1.3)
        C3 = np.array([0.0, 0.0, -0.3])
        # the flat (r, z) grid: x = r, z up
        def grid_plane():
            g = VGroup()
            for i in range(9):
                g.add(Line(C3 + [0.25 * i, 0, -1.5], C3 + [0.25 * i, 0, 1.5], color=BLUE_D, stroke_width=1.5))
            for j in range(9):
                g.add(Line(C3 + [0, 0, -1.5 + 0.375 * j], C3 + [2.0, 0, -1.5 + 0.375 * j], color=BLUE_D, stroke_width=1.5))
            return g
        grid = grid_plane()
        axis = DashedLine(C3 + [0, 0, -1.65], C3 + [0, 0, 1.65], color=GREEN, dash_length=0.1, stroke_width=4)
        lr = self.hud(tex("r", 30, GREY_B).move_to([3.0, -0.45, 0]))
        lz = self.hud(tex("z", 30, GREY_B).move_to([-0.3, 1.95, 0]))
        t113 = self.hud(at_left(Text("the code is axisymmetric:\neverything lives on the flat (r, z) grid", font_size=20,
                                     color=GREY_B, line_spacing=0.9), 2.0, -6.4))
        # cue 113
        self.at(113, Create(grid), Create(axis), FadeIn(lr), FadeIn(lz), FadeIn(t113), run_time=1.6)

        # cue 114: spin the grid around the axis
        copies = [grid.copy().rotate(k * 45 * DEGREES, axis=OUT, about_point=C3) for k in range(1, 8)]
        t114 = self.hud(at_left(Text("spin the grid around the axis", font_size=20, color=BLUE_C), 0.9, -6.4))
        self.at(114, FadeIn(t114), run_time=0.4)
        self.move_camera(phi=66 * DEGREES, theta=-52 * DEGREES, run_time=2.4,
                         added_anims=[LaggedStart(*[FadeIn(c_) for c_ in copies], lag_ratio=0.3), FadeOut(lr), FadeOut(lz)])

        # cue 115: the surface where the swirl speed is 45% of its maximum
        SC = 1.5
        sx, sz = math.sqrt(0.5) * SC, 0.5 ** A_Z * SC
        shell_full = placed_shell(make_shell((0, TAU), colored=False, opacity=0.55), sx, sz, C3)
        t115 = self.hud(at_left(Text("surface where the swirl speed\nis 45% of its maximum", font_size=20, color=C_SWIRL,
                                     line_spacing=0.9), -0.2, -6.4))
        self.at(115, *[FadeOut(c_) for c_ in copies], FadeIn(shell_full), FadeIn(t115), run_time=1.2)

        # cue 116: a quarter cut away so you can see inside
        shell_cut = placed_shell(make_shell((PI / 2, TAU), colored=False, opacity=0.6), sx, sz, C3)
        t116 = self.hud(at_left(Text("a quarter cut away,\nso you can see inside", font_size=20, color=YELLOW,
                                     line_spacing=0.9), -1.3, -6.4))
        self.at(116, FadeOut(shell_full), FadeIn(shell_cut), FadeIn(t116), run_time=0.9)
        self.move_camera(phi=64 * DEGREES, theta=38 * DEGREES, run_time=1.2)

        # cue 117: coloured by the axial velocity
        shell_col = placed_shell(make_shell((PI / 2, TAU), colored=True, opacity=0.95), sx, sz, C3)
        self.at(117, FadeIn(shell_col), run_time=0.9)
        self.remove(shell_cut)
        # cue 118: red is up, blue is down
        k_up = self.hud(chip("red: flowing up", RED, 20, fill=0.25).move_to([4.7, 1.0, 0]))
        k_dn = self.hud(chip("blue: flowing down", BLUE, 20, fill=0.25).move_to([4.7, 0.2, 0]))
        t117 = self.hud(Text("coloured by the axial velocity", font_size=20, color=GREY_B).move_to([4.5, 1.8, 0]))
        self.at(118, FadeIn(k_up), FadeIn(k_dn), FadeIn(t117), run_time=0.6)
        self.begin_ambient_camera_rotation(rate=0.06)
        self.snap("shell")
        self.finish(keep=[header])
        self.stop_ambient_camera_rotation()


# ==========================================================================
# N8_2 -- 3D: physical vs similarity coordinates   (cues 119-124)
# ==========================================================================
class N8_2(Synced3D):
    AUDIO_START, AUDIO_END = START["N8_2"], END["N8_2"]

    def construct(self):
        header = hdr("Results")
        self.hud(header, show=True)
        self.set_camera_orientation(phi=66 * DEGREES, theta=38 * DEGREES)
        SC = 1.2
        base = make_shell((PI / 2, TAU), colored=True, opacity=0.95, nu=30, nv=30)
        TH = 38 * DEGREES                                   # camera azimuth
        RV = np.array([-math.sin(TH), math.cos(TH), 0.0])    # screen-right direction in the world
        CL, CR = -3.3 * RV + [0, 0, -0.1], 3.3 * RV + [0, 0, -0.1]
        s_tr = ValueTracker(-math.log(1 - T_REL))            # s = -ln(tau): log-time, so the last collapse is visible
        tau_of = lambda: math.exp(-s_tr.get_value())
        t_of = lambda: 1.0 - tau_of()

        # left panel: ONE persistent surface whose points are rescaled in place each frame
        # (cheap: no copying / rebuilding of the ~900 faces)
        left = placed_shell(base, math.sqrt(1 - T_REL) * SC, (1 - T_REL) ** A_Z * SC, CL)
        base_pts = [f.points.copy() for f in base]

        def phys(m):
            ta = tau_of()
            S = np.array([math.sqrt(ta) * SC, math.sqrt(ta) * SC, ta ** A_Z * SC])
            for f, p in zip(m, base_pts):
                f.points = p * S + CL
        left.add_updater(phys)
        # right panel: similarity coordinates -- the shape is the same at every time, drawn at the t = 0.4 size
        right = placed_shell(base, math.sqrt(1 - T_REL) * SC, (1 - T_REL) ** A_Z * SC, CR)

        lab_l = self.hud(Text("real coordinates, fixed window", font_size=20, color=GREY_B).move_to([-3.3, 2.2, 0]))
        lab_r = self.hud(Text("rescaled:  r/tau^(1/2),  z/tau^(1/2-h)", font_size=20, color=GREY_B).move_to([3.3, 2.2, 0]))
        pk_txt = self.hud(note("peak swirl", 20, C_SWIRL).move_to([-4.2, -2.1, 0]))
        pk_num = DecimalNumber(float(swirl_peak(T_REL)), num_decimal_places=1, font_size=34, color=C_SWIRL)
        pk_num.move_to([-2.8, -2.1, 0])
        def _upd(fn):
            def u(m):
                m.set_value(fn())
                self.camera.add_fixed_in_frame_mobjects(m)   # set_value rebuilds glyphs: re-pin them to the screen
            return u
        pk_num.add_updater(_upd(lambda: float(swirl_peak(t_of()))))
        self.hud(pk_num)
        cr_txt = self.hud(note("core radius shrinks by", 20, GREY_B).move_to([2.0, -2.1, 0]))
        cr_num = DecimalNumber(1.0, num_decimal_places=1, font_size=34, color=YELLOW).move_to([4.2, -2.1, 0])
        cr_num.add_updater(_upd(lambda: math.sqrt((1 - T_REL) / tau_of())))
        self.hud(cr_num)
        cr_x = self.hud(tex(r"\times", 30, YELLOW).move_to([4.85, -2.1, 0]))

        # cue 119: left panel, physical coordinates, fixed window
        self.add(left)
        self.at(119, FadeIn(lab_l), FadeIn(pk_txt), FadeIn(pk_num), run_time=0.8)
        # cue 120: the swirl at its peak goes from 2 to 25  (run 1: left only)
        self.at(120, s_tr.animate.set_value(-math.log(1 - T_END)), run_time=4.6, rate_func=linear)
        # cue 121: rescale the coordinates: the shape barely changes   (run 2: both panels)
        s_tr.set_value(-math.log(1 - T_REL))
        self.at(121, FadeIn(right), FadeIn(lab_r), FadeIn(cr_txt), FadeIn(cr_num), FadeIn(cr_x), run_time=0.8)
        self.play(s_tr.animate.set_value(-math.log(1 - T_END)), run_time=10.5, rate_func=linear)
        # cue 122: this is not a discovery
        nd = self.hud(Text("not a\ndiscovery", font_size=28, color=YELLOW, line_spacing=0.9).move_to([0, 0.9, 0]))
        self.at(122, FadeIn(nd), run_time=0.5)
        # cue 123: I built the flow to be self-similar, so the right-hand picture has to look like that
        nd2 = self.hud(note("I built the flow\nto be self-similar:\nthe right-hand picture\nhas to look like this", 17, GREY_B)
                       .move_to([0, -0.4, 0]))
        self.at(123, FadeIn(nd2), run_time=0.5)
        # cue 124: what it shows: the solver preserves the shape while the core radius shrinks 5-6 times
        ok = self.hud(chip("the solver preserves the shape while the core shrinks 5-6x", GREEN, 20, fill=0.12)
                      .move_to([0, -3.0, 0]))
        self.at(124, FadeIn(ok, shift=UP * 0.15), run_time=0.6)
        self.snap("sidebyside")
        self.finish(keep=[header])


# ==========================================================================
# N9 -- tracer particles   (cues 125-135)
# ==========================================================================
class N9(SyncedScene):
    AUDIO_START, AUDIO_END = START["N9"], END["N9"]

    def construct(self):
        header = hdr("Results")
        self.add(header)
        ts, rr, zz, th, mid = tracers()
        nS = len(ts)
        n_p = rr.shape[1]
        idx = ValueTracker(0.0)

        def row(arr):
            x = idx.get_value(); i = int(min(max(math.floor(x), 0), nS - 2)); f = x - i
            return (1 - f) * arr[i] + f * arr[i + 1]

        tau_arr = 1.0 - ts
        rho_arr = rr / np.sqrt(tau_arr)[:, None]
        zet_arr = zz / (tau_arr ** A_Z)[:, None]
        th0 = th[0]

        # ---- left: physical (r, z) half plane
        PL = lambda r, z: [-5.5 + 1.5 * r, -0.4 + 1.5 * z, 0]
        frL = Rectangle(width=1.5 * 1.4, height=1.5 * 3.2, color=GREY_C, stroke_width=2).move_to(PL(0.7, 0))
        axL = DashedLine(PL(0, -1.6), PL(0, 1.6), color=GREY_B, dash_length=0.1)
        tL = note("(r, z) plane", 18, GREY_B).next_to(frL, UP, buff=0.12)
        dotsL = VGroup(*[Dot(radius=0.045 if mid[k] else 0.03, color=YELLOW if mid[k] else BLUE_C).set_opacity(1.0 if mid[k] else 0.75)
                         for k in range(n_p)])

        def updL(g):
            r_, z_ = row(rr), row(zz)
            for k, d in enumerate(g):
                d.move_to(PL(min(r_[k], 1.4), max(min(z_[k], 1.6), -1.6)))
        dotsL.add_updater(updL)

        # ---- middle: top view
        C2 = np.array([-0.2, -0.4, 0.0]); S2 = 1.45
        circ = Circle(radius=S2 * 1.25, color=GREY_C, stroke_width=2).move_to(C2)
        tT = note("top view", 18, GREY_B).next_to(circ, UP, buff=0.12)
        dotsT = VGroup(*[Dot(radius=0.045 if mid[k] else 0.025, color=YELLOW if mid[k] else BLUE_C).set_opacity(1.0 if mid[k] else 0.5)
                         for k in range(n_p)])

        def updT(g):
            r_, a_ = row(rr), row(th)
            for k, d in enumerate(g):
                d.move_to(C2 + [S2 * r_[k] * math.cos(a_[k]), S2 * r_[k] * math.sin(a_[k]), 0])
        dotsT.add_updater(updT)
        mid_ids = [k for k in range(n_p) if mid[k]]
        trail_ids = mid_ids[::3]                              # one angle per radius
        trails = VGroup(*[always_redraw(lambda k=k: poly(
            [C2 + [S2 * rr[i, k] * math.cos(th[i, k]), S2 * rr[i, k] * math.sin(th[i, k]), 0]
             for i in range(0, int(idx.get_value()) + 1, 2)] or [C2, C2], YELLOW, 1.6).set_stroke(opacity=0.5))
                          for k in trail_ids])

        # ---- live counters (midplane turns)
        def turns_vals():
            a = row(th) - th0
            tt_ = a[mid] / TAU
            return float(tt_.min()), float(tt_.max())
        cnt_lab = note("midplane turns so far:", 18, YELLOW).move_to([-0.2, -3.0, 0])
        c_min = DecimalNumber(0.0, num_decimal_places=1, font_size=28, color=YELLOW)
        c_max = DecimalNumber(0.0, num_decimal_places=1, font_size=28, color=YELLOW)
        dash = tex(r"\ldots", 26, YELLOW)
        cnt = VGroup(c_min, dash, c_max).arrange(RIGHT, buff=0.15).next_to(cnt_lab, RIGHT, buff=0.2)
        c_min.add_updater(lambda m: m.set_value(turns_vals()[0]))
        c_max.add_updater(lambda m: m.set_value(turns_vals()[1]))
        tcur = VGroup(note("t =", 18, GREY_B), DecimalNumber(T_REL, num_decimal_places=3, font_size=26, color=GREY_B))
        tcur.arrange(RIGHT, buff=0.12).move_to([-5.5 + 1.5 * 0.7, -3.0, 0])
        tcur[1].add_updater(lambda m: m.set_value(float(np.interp(idx.get_value(), np.arange(nS), ts))))

        # cue 125: next, the tracer particles
        t125 = Text("next: tracer particles", font_size=44, color=YELLOW).move_to([0, 0.3, 0])
        self.at(125, FadeIn(t125, scale=0.95), run_time=0.7)
        # cue 126: 147 passive particles at t = 0.4 on a grid of radii, heights and angles, moved with the flow
        gridtxt = tex(r"7\ \text{radii}\times 7\ \text{heights}\times 3\ \text{angles}=147", 34, YELLOW).move_to([0, -2.3, 0])
        self.at(126, FadeOut(t125), FadeIn(frL), FadeIn(axL), FadeIn(tL), FadeIn(circ), FadeIn(tT), run_time=0.7)
        self.add(dotsL, dotsT)
        self.play(FadeIn(dotsL), FadeIn(dotsT), run_time=0.8)
        self.play(Write(gridtxt), run_time=1.2)
        rel = note("released at t = 0.4; moved with the flow by the same RK4 stepper as the fluid", 18, GREY_B).move_to([0.9, 2.25, 0])
        clamp_width(rel, 9.0)
        self.frac(126, 0.55, FadeIn(rel), run_time=0.5)

        # cue 127/128: my prediction was that particles near the axis spiral inward ... I was wrong
        pred = Text("prediction: near the axis they spiral inward", font_size=26, color=ORANGE).move_to([0.9, 2.25, 0])
        self.at(127, FadeOut(rel), FadeOut(gridtxt), FadeIn(pred), run_time=0.5)
        wrong = Text("I was wrong.", font_size=44, color=RED).move_to([2.9, 0.9, 0])
        self.at(128, FadeIn(wrong, scale=0.9), run_time=0.5)

        # cue 129: at z = 0 the axial velocity is exactly zero by symmetry, so the midplane fluid only moves inward
        sym = tex(r"z=0:\quad u_z=0\ \text{by symmetry}", 30, C_RAD).move_to([2.9, 2.25, 0])
        self.at(129, FadeOut(pred), FadeOut(wrong), FadeIn(sym), run_time=0.5)
        self.add(trails, cnt_lab, cnt, tcur)
        self.play(FadeIn(cnt_lab), FadeIn(cnt), FadeIn(tcur), run_time=0.4)
        run1 = 22.0
        self.play(idx.animate.set_value(nS - 1), run_time=run1, rate_func=linear)
        # cue 130/131 happen inside the run: all 21 midplane particles fall in and spin up; the count shoots up
        # (beats are narrated over the animation; the labels below are timed to the cue starts)
        self.snap("run1")

        # cue 132: everything else goes around closed loops in the (r, z) plane ...
        loops = VGroup()
        pick = [(1, 1, 0), (1, 5, 0), (3, 0, 0), (3, 6, 0), (4, 2, 0), (5, 5, 0)]
        for ir, iz, ia in pick:
            k = (ir * 7 + iz) * 3 + ia
            loops.add(poly([PL(rr[i, k], zz[i, k]) for i in range(nS)], BLUE_C, 2.4))
        l132 = note("everything else:\nclosed loops in the (r, z) plane", 20, BLUE_C).move_to([3.4, 1.4, 0])
        self.at(132, FadeOut(sym), FadeIn(l132), dotsT.animate.set_opacity(0.0), run_time=0.5)
        self.play(LaggedStart(*[Create(m) for m in loops], lag_ratio=0.2), run_time=2.4)
        # cue 133: ... or carried out along the axis
        l133 = note("or carried out along the axis", 20, BLUE_C).move_to([3.4, 0.4, 0])
        self.at(133, FadeIn(l133), run_time=0.5)

        # cue 134: similarity coordinates: those particles get left behind; median similarity radius ~ 5
        self.at(134, *[FadeOut(m) for m in [l132, l133, loops, trails, circ, tT, dotsT, cnt_lab, cnt]], run_time=0.5)
        S3 = 0.30
        P3 = lambda rho, zeta: [1.2 + S3 * rho, -0.4 + S3 * zeta, 0]
        fr3 = Rectangle(width=S3 * 12, height=S3 * 15, color=GREY_C, stroke_width=2).move_to(P3(6, 0))
        ax3 = DashedLine(P3(0, -7.5), P3(0, 7.5), color=GREY_B, dash_length=0.1)
        t3 = note("similarity coordinates  (r/tau^(1/2),  z/tau^(1/2-h))", 18, GREY_B).next_to(fr3, UP, buff=0.12)
        dots3 = VGroup(*[Dot(radius=0.045 if mid[k] else 0.03, color=YELLOW if mid[k] else BLUE_C).set_opacity(1.0 if mid[k] else 0.75)
                         for k in range(n_p)])

        def upd3(g):
            r_, z_ = row(rho_arr), row(zet_arr)
            for k, d in enumerate(g):
                d.move_to(P3(min(r_[k], 12), max(min(z_[k], 7.5), -7.5)))
        dots3.add_updater(upd3)
        med_lab = note("median rho = r/tau^(1/2):", 18, ORANGE).move_to([2.6, -3.0, 0])
        med = DecimalNumber(1.0, num_decimal_places=1, font_size=30, color=ORANGE).next_to(med_lab, RIGHT, buff=0.15)
        med.add_updater(lambda m: m.set_value(float(np.median(row(rho_arr)))))
        idx.set_value(0.0)
        self.play(FadeIn(fr3), FadeIn(ax3), FadeIn(t3), FadeIn(med_lab), FadeIn(med), run_time=0.6)
        self.add(dots3)
        self.play(idx.animate.set_value(nS - 1), run_time=7.6, rate_func=linear)
        left_beh = note("left behind: the core shrinks faster than they move", 20, ORANGE).move_to([-2.5, 1.8, 0])
        self.snap("similarity")

        # cue 135: this likely comes from my simplified flow, not from the true solution
        cav = card("likely a feature of my simplified flow,\nnot of what OpenAI's solution would do\nif simulated more faithfully",
                   YELLOW, 22, 6.4).move_to([-2.6, 1.0, 0])
        #self.at(135, FadeIn(left_beh), run_time=0.4)
        self.wait(0.4)
        self.play(FadeIn(cav, shift=UP * 0.15), run_time=0.6)
        self.finish(keep=[header])


# ==========================================================================
# N10 -- term balance   (cues 136-152)
# ==========================================================================
def term_val(k, t):
    """max|term_k| at time t for the exact flow (k: 0 acc, 1 nonlinear, 2 viscous, 3 force)."""
    return float(term_consts(t)[k]) * (1.0 - t) ** (-1.5 - H_EXP)


class N10(SyncedScene):
    AUDIO_START, AUDIO_END = START["N10"], END["N10"]

    def construct(self):
        header = hdr("Results")
        self.add(header)
        names = ["acceleration", "nonlinear", "viscous", "force"]
        cols = [C_ACC, C_NL, C_VISC, C_FORCE]
        # cue 136: third, the term balance
        t136 = Text("third: the term balance", font_size=46, color=YELLOW).move_to([0, 0.4, 0])
        self.at(136, FadeIn(t136, scale=0.95), run_time=0.7)

        # cue 137: swirl equation: acceleration = nonlinear + viscous + force
        chips_ = VGroup(*[chip(n_, c_, 24, fill=0.1) for n_, c_ in zip(names, cols)])
        ops = [tex(s, 36) for s in ("=", "+", "+")]
        row = VGroup(chips_[0], ops[0], chips_[1], ops[1], chips_[2], ops[2], chips_[3]).arrange(RIGHT, buff=0.22)
        row.move_to([0, 2.15, 0]); clamp_width(row, 12.0)
        self.at(137, FadeOut(t136), run_time=0.4)
        self.play(LaggedStart(*[FadeIn(m, shift=DOWN * 0.1) for m in row], lag_ratio=0.12), run_time=1.6)
        sw = note("the swirl equation", 18, GREY_B).next_to(row, UP, buff=0.12).align_to(row, RIGHT)
        self.play(FadeIn(sw), run_time=0.3)

        # cue 138-140: record the maximum of each term at every step; all four grow with the same power
        ax = Axes(x_range=[0.35, 1.75, 0.5], y_range=[0.2, 3.4, 1], x_length=7.6, y_length=3.6,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([-1.2, -0.85, 0])
        xl = note("-log10(tau)   ( -> singularity )", 15, GREY_B).next_to(ax, DOWN, buff=0.05)
        yl = note("log10( max |term| )", 15, GREY_B).next_to(ax, UP, buff=0.05).align_to(ax, LEFT)
        tgrid = np.linspace(0.6, 0.98, 30)
        lines = []
        for k in range(4):
            pts = [ax.c2p(-math.log10(1 - t), math.log10(term_val(k, t))) for t in tgrid]
            lines.append(VGroup(poly(pts, cols[k], 3.5), *[Dot(p, radius=0.035, color=cols[k]) for p in pts[::3]]))
        labs = [note(n_, 16, c_) for n_, c_ in zip(names, cols)]
        for lb, k in zip(labs, range(4)):
            lb.next_to(ax.c2p(1.7, math.log10(term_val(k, 0.98))), RIGHT, buff=0.12)
        labs[1].shift(DOWN * 0.18); labs[3].shift(UP * 0.1)
        self.at(138, Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(LaggedStart(*[Create(l_) for l_ in lines], lag_ratio=0.25), FadeIn(VGroup(*labs)), run_time=2.0)
        slope = tex(r"\text{slopes between }-1.49\text{ and }-1.51", 30, YELLOW).move_to([4.2, 0.2, 0])
        clamp_width(slope, 4.6)
        self.at(139, Indicate(VGroup(*lines)), run_time=1.2)
        self.at(140, Write(slope), run_time=1.4)

        # cue 141: that is tau^(-3/2-h); divide the power out and they become almost flat lines
        pw = tex(r"\tau^{-3/2-h}", 44, YELLOW).move_to([4.2, 1.4, 0])
        self.at(141, FadeIn(pw), run_time=0.5)
        ax2 = Axes(x_range=[0.35, 1.75, 0.5], y_range=[0, 34, 10], x_length=7.6, y_length=3.6,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([-1.2, -0.85, 0])
        yl2 = note("max |term|  /  tau^(-3/2-h)", 15, GREY_B).next_to(ax2, UP, buff=0.05).align_to(ax2, LEFT)
        flat = []
        for k in range(4):
            pts = [ax2.c2p(-math.log10(1 - t), float(term_consts(t)[k])) for t in tgrid]
            flat.append(poly(pts, cols[k], 4))
        self.frac(141, 0.35, FadeOut(VGroup(*lines)), FadeOut(VGroup(*labs)), FadeOut(slope), FadeOut(yl),
                  ReplacementTransform(ax, ax2), FadeIn(yl2), run_time=1.0)
        self.play(LaggedStart(*[Create(f_) for f_ in flat], lag_ratio=0.2), run_time=1.6)
        # cue 142/143: read off their sizes
        vals = ["2.5", "20", "28", "20"]
        ylev = [float(term_consts(0.9)[k]) for k in range(4)]
        tags = []
        for k in range(4):
            tg = tex(rf"\approx {vals[k]}", 28, cols[k]).next_to(ax2.c2p(1.7, ylev[k]), RIGHT, buff=0.12)
            tags.append(tg)
        tags[2].shift(UP * 0.15); tags[1].shift(DOWN * 0.22); tags[3].shift(UP * 0.0)
        tag_names = [note(n_, 15, c_).next_to(tg, RIGHT, buff=0.1) for n_, c_, tg in zip(names, cols, tags)]
        self.at(142, FadeOut(pw), run_time=0.3)
        for k, f in zip(range(4), (0.0, 0.22, 0.45, 0.7)):
            self.frac(143, f, FadeIn(tags[k], shift=LEFT * 0.1), FadeIn(tag_names[k]), run_time=0.4)
        self.snap("flat")

        # cue 144: the force is about 0.7 of the biggest term, at every time
        r144 = tex(r"\dfrac{\text{force}}{\text{largest term}}\approx 0.7", 34, C_FORCE).move_to([4.4, -2.0, 0])
        clamp_width(r144, 3.6)
        r144.move_to([4.5, -1.8, 0])
        self.at(144, FadeOut(VGroup(*tags)), FadeOut(VGroup(*tag_names)), run_time=0.3)
        self.play(Write(r144), run_time=1.2)
        # cue 145: acceleration is about ten times smaller than the nonlinear and viscous terms
        r145 = tex(r"\dfrac{\text{acceleration}}{\text{nonlinear, viscous}}\approx\dfrac1{10}", 30, C_ACC).move_to([4.5, 0.1, 0])
        clamp_width(r145, 3.9)
        self.at(145, Write(r145), run_time=1.4)
        # cue 146: the big terms mostly offset each other; what is left makes the fluid move
        off = note("the big terms mostly offset each other;\nwhat is left makes the fluid move", 20, YELLOW).move_to([4.4, 1.9, 0])
        clamp_width(off, 4.4)
        self.at(146, FadeIn(off, shift=UP * 0.1), run_time=0.6)

        # cue 147: why does this matter?
        self.at(147, *[FadeOut(m) for m in [ax2, xl, yl2, *flat, r144, r145, off, row, sw]], run_time=0.5)
        why = Text("why does this matter?", font_size=44, color=YELLOW).move_to([0, 0.3, 0])
        self.play(FadeIn(why, scale=0.95), run_time=0.6)
        # cue 148: in the paper the divergent pieces of the force cancel among themselves
        self.at(148, FadeOut(why), run_time=0.3)
        axP = Axes(x_range=[0.4, 1.0, 0.2], y_range=[-8, 8, 4], x_length=5.2, y_length=2.8,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([-3.4, -0.55, 0])
        xs = np.linspace(0.4, 0.985, 300)
        pw_ = lambda t: (1 - t) ** -1.5
        parts_ = [(lambda t: 0.9 * pw_(t), C_ACC), (lambda t: -1.4 * pw_(t), C_NL), (lambda t: 0.9 * pw_(t), C_VISC),
                  (lambda t: -0.4 * pw_(t) + 1.0 + 0.5 * t, TEAL)]
        cvs = [clipped_curve(axP, f_, xs, 8, c_, 2.5) for f_, c_ in parts_]
        sm = clipped_curve(axP, lambda t: sum(f_(t) for f_, _ in parts_), xs, 8, WHITE, 5)
        tP = note("the paper: divergent pieces cancel\namong themselves; what is left is smooth", 20, WHITE).next_to(axP, UP, buff=0.3)
        sch = note("schematic", 14, GREY_B).next_to(axP, DOWN, buff=0.05)
        self.play(Create(axP), FadeIn(tP), FadeIn(sch), run_time=0.7)
        self.play(*[Create(c_) for c_ in cvs], run_time=1.8)
        self.frac(148, 0.7, Create(sm), run_time=1.2)
        # cue 149-151: in my toy that does not happen: the force has the same divergent power and sustains the blow-up
        axT = Axes(x_range=[0.4, 1.0, 0.2], y_range=[0, 8, 4], x_length=5.2, y_length=2.8,
                   axis_config={"include_tip": False, "font_size": 12}).move_to([3.4, -0.55, 0])
        tT_ = note("my toy: the force is a divergent term itself", 20, C_FORCE).next_to(axT, UP, buff=0.3)
        fcv = clipped_curve(axT, lambda t: 0.9 * (1 - t) ** -1.5 * 0.5, xs, 8, C_FORCE, 5)
        self.at(149, Create(axT), FadeIn(tT_), run_time=0.7)
        self.at(150, Create(fcv), run_time=1.6)
        same = tex(r"f\sim\tau^{-3/2}:\ \text{the same divergent power}", 28, C_FORCE).move_to([3.4, -2.1, 0])
        clamp_width(same, 5.6)
        self.at(151, FadeIn(same, shift=UP * 0.1), run_time=0.6)
        # cue 152: the clearest picture of why my version is the cheap one
        cheap = chip("why my version is the cheap one", YELLOW, 24, fill=0.12).move_to([3.7, 2.5, 0])
        self.at(152, FadeIn(cheap, scale=0.92), run_time=0.6)
        self.snap("compare")
        self.finish(keep=[header])


# ==========================================================================
# N11 -- switch the force off   (cues 153-164)
# ==========================================================================
class N11(SyncedScene):
    AUDIO_START, AUDIO_END = START["N11"], END["N11"]

    def construct(self):
        header = hdr("Results")
        self.add(header)
        # cue 153: fourth, switch the force off
        t153 = Text("fourth: switch the force off", font_size=44, color=YELLOW).move_to([0, 0.4, 0])
        self.at(153, FadeIn(t153, scale=0.95), run_time=0.7)

        # main plot: peak swirl vs t
        ax = Axes(x_range=[0.3, 1.02, 0.1], y_range=[0, 27, 10], x_length=6.8, y_length=3.7,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([-2.9, -0.3, 0])
        xl = tex("t", 24, GREY_B).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT)
        yl = note("peak swirl", 16, GREY_B).next_to(ax, UP, buff=0.05).align_to(ax, LEFT)
        xs = np.linspace(0.3, 0.98, 300)
        forced = clipped_curve(ax, lambda t: float(swirl_peak(t)), xs, 27, GREY_B, 3)
        forced_dash = DashedVMobject(forced, num_dashes=70)
        lf = note("force kept on", 15, GREY_B).move_to(ax.c2p(0.5, 17))
        # cue 154: option to fade the force out smoothly at a chosen time and keep running past t = 1
        self.at(154, FadeOut(t153), Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(Create(forced_dash), FadeIn(lf), run_time=1.4)
        fa = Axes(x_range=[0.3, 1.02, 0.2], y_range=[0, 1.2, 1], x_length=4.0, y_length=1.1,
                  axis_config={"include_tip": False, "font_size": 10}).move_to([4.3, 1.75, 0])
        fcur = fa.plot(lambda t: 1 - float(smooth_step((t - 0.7) / 0.06)), x_range=[0.3, 1.0, 0.003], color=C_FORCE, stroke_width=4)
        fl_ = note("force f(t): faded out smoothly at a chosen time\n(schematic, shown for t_off = 0.7)", 14, C_FORCE).next_to(fa, UP, buff=0.1)
        self.frac(154, 0.45, Create(fa), FadeIn(fl_), Create(fcur), run_time=1.2)

        # cue 155: tau = 0.3, 0.1, 0.04
        offs = [(0.7, "0.3", TEAL), (0.9, "0.1", ORANGE), (0.96, "0.04", PURPLE)]
        vlines, vlabs = VGroup(), VGroup()
        for t_off, tl, c_ in offs:
            vlines.add(DashedLine(ax.c2p(t_off, 0), ax.c2p(t_off, 27), color=c_, dash_length=0.1))
            vlabs.add(tex(rf"\tau={tl}", 22, c_).next_to(ax.c2p(t_off, 27), UP, buff=0.5 if tl == "0.04" else 0.12))
        self.at(155, LaggedStart(*[Create(v_) for v_ in vlines], lag_ratio=0.3), FadeIn(vlabs), run_time=1.4)

        # cue 156: swirl peaks about 6, 11, 18 right at the moment of removal ...
        peaks = [(t_off, float(swirl_peak(t_off)), c_, lab) for (t_off, _, c_), lab in zip(offs, ("6", "11", "18"))]
        dots = VGroup(*[Dot(ax.c2p(t_, p_), radius=0.08, color=c_) for t_, p_, c_, _ in peaks])
        plabs = VGroup(*[tex(rf"\approx {lab}", 22, c_).next_to(ax.c2p(t_, p_), LEFT, buff=0.25).shift(UP * 0.18) for t_, p_, c_, lab in peaks])
        self.at(156, LaggedStart(*[FadeIn(d_, scale=1.6) for d_ in dots], lag_ratio=0.3), FadeIn(plabs), run_time=1.4)
        # cue 157: ... then decays; at t = 1 it is down to 0.27, 0.48, 0.80
        finals = [0.27, 0.48, 0.80]
        decays, endlabs = VGroup(), VGroup()
        for (t_off, p_, c_, _), fv in zip(peaks, finals):
            k = math.log(p_ / fv) / (1 - t_off)
            ts_ = np.linspace(t_off, 1.0, 50)
            decays.add(DashedVMobject(poly([ax.c2p(t_, p_ * math.exp(-k * (t_ - t_off))) for t_ in ts_], c_, 4), num_dashes=24))
            endlabs.add(tex(rf"{fv:.2f}", 24, c_))
        endrow = VGroup(note("peak swirl at t = 1:", 16, GREY_B), *endlabs).arrange(RIGHT, buff=0.4).move_to([-2.9, -2.65, 0])
        sch = note("decay shape between the measured points: schematic", 13, GREY_B).move_to([-2.9, -3.05, 0])
        self.at(157, LaggedStart(*[Create(d_) for d_ in decays], lag_ratio=0.3), run_time=1.8)
        self.play(FadeIn(endrow), FadeIn(sch), run_time=0.6)
        self.snap("decay")

        # cue 158: the core radius turns around and grows
        axR = Axes(x_range=[0.3, 1.02, 0.2], y_range=[0, 1.9, 1], x_length=4.0, y_length=1.5,
                   axis_config={"include_tip": False, "font_size": 10}).move_to([4.3, -0.6, 0])
        t_off = 0.7
        Rc = lambda t: math.sqrt(1 - t) if t <= t_off else math.sqrt((1 - t_off) + 4 * (t - t_off))
        rcur = axR.plot(lambda t: Rc(t), x_range=[0.3, 1.0, 0.003], color=C_SWIRL, stroke_width=4, use_smoothing=False)
        rl = note("core radius (t_off = 0.7): turns around\nand grows (schematic viscous spreading)", 14, C_SWIRL).next_to(axR, UP, buff=0.1)
        self.at(158, FadeOut(fa), FadeOut(fl_), FadeOut(fcur), Create(axR), FadeIn(rl), run_time=0.6)
        self.play(Create(rcur), run_time=1.6)

        # cue 159: viscosity doing what viscosity does to a vortex nobody is driving
        s159 = card("just viscosity doing what it does\nto a vortex nobody is driving", C_VISC, 24, 6.2).move_to([4.2, 1.9, 0])
        s159.scale(0.9).move_to([4.3, 1.9, 0])
        self.at(159, FadeIn(s159, shift=LEFT * 0.15), run_time=0.6)
        # cue 160: not evidence about the real solution
        s160 = card("not evidence about the real solution", RED, 24, 6.2).scale(0.9).move_to([4.3, 0.55, 0])
        self.at(160, FadeOut(axR), FadeOut(rl), FadeOut(rcur), run_time=0.3)
        self.play(FadeIn(s160, shift=LEFT * 0.15), run_time=0.5)
        # cue 161: in my toy the force is what keeps the vortex collapsing
        s161 = card("in my toy the force keeps the vortex\ncollapsing: remove it and the collapse stops", ORANGE, 22, 6.2).scale(0.9).move_to([4.3, -0.9, 0])
        self.at(161, FadeIn(s161, shift=LEFT * 0.15), run_time=0.6)
        # cue 162: whether the unforced equations can blow up from smooth data is the open Millennium problem
        s162 = card("unforced Navier-Stokes from smooth data:\nthe open Millennium problem", PURPLE, 22, 6.2).scale(0.9).move_to([4.3, -2.4, 0])
        self.at(162, FadeIn(s162, shift=LEFT * 0.15), run_time=0.6)
        # cue 163: nothing here touches it
        n163 = Text("nothing here touches it", font_size=26, color=YELLOW).move_to([-2.9, 2.35, 0])
        self.at(163, FadeIn(n163), run_time=0.5)
        # cue 164: OpenAI's theorem itself is still forced
        n164 = chip("OpenAI's theorem itself is the forced one", BLUE, 24, fill=0.1).move_to([-2.9, 2.35, 0])
        self.at(164, FadeOut(n163), FadeIn(n164, shift=UP * 0.1), run_time=0.6)
        self.finish(keep=[header])


def smooth_step(x):
    x = min(max(float(x), 0.0), 1.0)
    return x * x * (3 - 2 * x)


# ==========================================================================
# N12 -- limits: the code fails, and that is fine   (cues 165-168)
# ==========================================================================
class N12(SyncedScene):
    AUDIO_START, AUDIO_END = START["N12"], END["N12"]

    def construct(self):
        header = hdr("Limits")
        # cue 165: "I must admit, as I mentioned many times: my code fails."
        self.at(165, Write(header), run_time=0.6)
        fail = Text("my code fails", font_size=64, color=RED).move_to([0, 0.5, 0])
        self.sync(CUES[165].start + 0.8)
        self.play(FadeIn(fail, scale=0.9), run_time=0.8)

        # cue 166: the force grows like tau^(-3/2): it diverges
        ax = Axes(x_range=[0.3, 1.0, 0.1], y_range=[0, 8, 4], x_length=6.2, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([-3.0, -0.6, 0])
        xs = np.linspace(0.3, 0.985, 300)
        cv = clipped_curve(ax, lambda t: 1.0 * (1 - t) ** -1.5 * 0.5 + 0.2, xs, 8, C_FORCE, 5)
        Tl = DashedLine(ax.c2p(1, 0), ax.c2p(1, 8), color=RED, dash_length=0.1)
        fl = tex(r"f\sim\tau^{-3/2}\ \longrightarrow\ \infty", 40, C_FORCE).move_to([3.7, 0.5, 0])
        div = note("the force diverges", 24, C_FORCE).move_to([3.7, -0.4, 0])
        self.at(166, fail.animate.scale(0.5).move_to([-3.0, 2.2, 0]), Create(ax), run_time=0.7)
        self.play(Create(Tl), Create(cv), Write(fl), run_time=2.4)
        self.play(FadeIn(div), run_time=0.4)

        # cue 167: the very thing the research programme was trying to avoid
        avoid = chip("exactly what the paper's construction is built to avoid", YELLOW, 24, fill=0.1).move_to([3.4, -1.9, 0])
        clamp_width(avoid, 6.2)
        self.at(167, FadeIn(avoid, shift=UP * 0.12), run_time=0.6)

        # cue 168: not sure how to fix it; I had fun; I hope you learned something; a success
        self.at(168, *[FadeOut(m) for m in [ax, cv, Tl, fl, div, avoid, fail]], run_time=0.6)
        fun = Text("I had fun,", font_size=50, color=YELLOW).move_to([0, 1.7, 0])
        hope = Text("and I hope you learned something", font_size=40, color=FG).move_to([0, 0.7, 0])
        succ = Text("so I'm calling it a success", font_size=40, color=GREEN).move_to([0, -0.5, 0])
        self.play(FadeIn(fun, shift=UP * 0.15), run_time=0.7)
        self.frac(168, 0.3, FadeIn(hope, shift=UP * 0.15), run_time=0.7)
        self.frac(168, 0.55, FadeIn(succ, shift=UP * 0.15), run_time=0.7)
        ty = Text("thank you for watching", font_size=34, color=GREY_B).move_to([0, -2.1, 0])
        self.frac(168, 0.8, FadeIn(ty), run_time=0.6)
        self.finish()