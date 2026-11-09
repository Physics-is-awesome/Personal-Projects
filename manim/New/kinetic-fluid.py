#!/usr/bin/env python3

"""
kinetic_fluid_scenes.py
================================================================================
Manim animation for "From Hamiltonian dynamics to the Boltzmann equation to
Navier-Stokes" (Liouville -> BBGKY -> Boltzmann -> H-theorem -> Chapman-Enskog),
synchronized to the narration track (kinetic-fluid.srt).

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh kinetic_fluid_scenes.py K0
    ...                       or render them all:  for s in K0 K1 ...; do ...; done

Scene map (script section -> scene -> narration cues in kinetic-fluid.srt)
    K0     title, scope                                     cues   1-  3
    K1     Overview I: three levels of description          cues   4- 10
    K1_1   Overview II: d << lambda << L, Knudsen, times    cues  11- 16
    K1_2   Overview III: the route, closure is the theme    cues  17- 26
    K2     N-body Hamiltonian, f, normalization, continuity cues  27- 37
    K2_1   Liouville equation, Poisson bracket, f = f(H)    cues  38- 43
    K2_2   Expectation values: d<A>/dt = <{A,H}>            cues  44- 48
    K3     Marginalising: f_1, streaming + collision terms  cues  49- 66
    K3_1   The BBGKY hierarchy                              cues  67- 74
    K3_2   Closures: Vlasov vs Boltzmann                    cues  75- 79
    K4     The collision rate omega and its symmetries      cues  80- 96
    K4_1   Molecular chaos, the arrow of time, Boltzmann    cues  97-111
    K4_2   Velocity variables                               cues 112-116
    K5     The H-theorem: derivation                        cues 117-130
    K5_1   Equality case -> Maxwell-Boltzmann               cues 131-138
    K5_2   Loschmidt and Zermelo, hard-disk simulation      cues 139-159
    K6     Moments, collision invariants, conservation laws cues 160-169
    K6_1   The moment hierarchy does not close              cues 170-176
    K7     Local equilibrium -> Euler                       cues 177-183
    K7_1   Where did the arrow of time go?                  cues 184-190
    K8     Chapman-Enskog expansion, BGK model              cues 191-203
    K8_1   Computing D f_0                                  cues 204-213
    K9     Stress, heat flux -> Navier-Stokes-Fourier       cues 214-225
    K9_1   Incompressible limit, hard spheres, BGK's limits cues 226-237
    K10    Three descriptions: what each keeps and costs    cues 238-250
    K10_1  Knudsen regimes and where "fluid" fails          cues 251-260
    K11    Closing                                          cues 261-261

--------------------------------------------------------------------------
AUDIO SYNC (same machinery as the metriplectic / Kerr / DG projects)
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
      [sync] K5: clip 126.20s / audio 126.20s | late beats: cue 126 (+0.35s)
  Fix a late beat by shortening a run_time or moving the cue earlier.

Debug aid: set SNAP=1 and every self.snap("name") call writes
snaps/<scene>_<name>.png so layouts can be checked without watching.

Hard-disk gas (K5_2): an event-driven simulation (DiskGas below) is run once
and cached; it takes a few seconds the first time a scene that needs it renders.

Theme: uses your emacs_theme module when importable (BG, FG, FG_DIM,
code_line, fit_width, starfield, BufferChrome).  If it is missing, a small
look-alike fallback is defined so the file still renders.
================================================================================
"""

import os
import re
import math
import heapq
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

# Our own TeX template (independent of the theme module): amsmath + amssymb, plus the
# few physics-package-style macros the script uses (\va, \pdv, \dv, \pb).
_TPL = TexTemplate()
_TPL.add_to_preamble(
    r"\usepackage{amsmath,amssymb}"
    r"\newcommand{\va}[1]{\mathbf{#1}}"
    r"\newcommand{\pdv}[2]{\frac{\partial #1}{\partial #2}}"
    r"\newcommand{\dv}[2]{\frac{d #1}{d #2}}"
    r"\newcommand{\pb}[2]{\{#1,#2\}}"
    r"\newcommand{\kB}{k_{B}}"
)
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

# --------------------------------------------------------------------------
# Narration timing
# --------------------------------------------------------------------------
Cue = namedtuple("Cue", "start end text")


def _find_srt():
    cands = [os.environ.get("KINETIC_FLUID_SRT")]
    try:
        cands.append(Path(__file__).with_name("kinetic-fluid.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "kinetic-fluid.srt",
              Path("~/Music/kinetic-fluid.srt").expanduser(),
              Path("/mnt/user-data/uploads/kinetic-fluid.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError("kinetic-fluid.srt not found (set KINETIC_FLUID_SRT=/path/to/file)")


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
# start of the next scene's first cue (K0 starts at t = 0).
PLAN = [("K0", 1), ("K1", 4), ("K1_1", 11), ("K1_2", 17), ("K2", 27), ("K2_1", 38),
        ("K2_2", 44), ("K3", 49), ("K3_1", 67), ("K3_2", 75), ("K4", 80), ("K4_1", 97),
        ("K4_2", 112), ("K5", 117), ("K5_1", 131), ("K5_2", 139), ("K6", 160),
        ("K6_1", 170), ("K7", 177), ("K7_1", 184), ("K8", 191), ("K8_1", 204),
        ("K9", 214), ("K9_1", 226), ("K10", 238), ("K10_1", 251), ("K11", 261)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="kinetic_fluid.tex"):
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


class SyncedScene(Scene):
    AUDIO_START = 0.0
    AUDIO_END = None
    STARS = None            # (count, seed) -> starfield backdrop

    # -- lifecycle ---------------------------------------------------------
    def setup(self):
        super().setup()
        self.camera.background_color = BG
        self._late, self._persist = [], []
        if self.STARS:
            stars = starfield(*self.STARS)
            self.add(stars)
            self._persist.append(stars)
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        self._persist.append(chrome)

    def tear_down(self):
        super().tear_down()
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


# --------------------------------------------------------------------------
# Small building blocks
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


def at_left(m, y, x=LEFT_X):
    """Place m with its left edge at x and its centre line at y."""
    m.move_to([x, y, 0], aligned_edge=LEFT)
    return m


def fit_box(m, max_w, max_h=None):
    """Shrink m (never enlarge) to fit inside max_w x max_h."""
    if m.width > max_w:
        m.set_width(max_w)
    if max_h is not None and m.height > max_h:
        m.set_height(max_h)
    return m


def out(*mobs):
    """FadeOut a bunch of mobjects (skipping None)."""
    return [FadeOut(m) for m in mobs if m is not None]


# --------------------------------------------------------------------------
# Gas-in-a-box helpers
# --------------------------------------------------------------------------
def box_frame(center, w, h, color=GREY_B):
    return Rectangle(width=w, height=h, color=color, stroke_width=2).move_to(center)


def bouncing_dots(center, w, h, n=40, seed=0, speed=0.9, radius=0.045, color=BLUE_C,
                  region=None):
    """A group of dots that bounce around a w x h box (ideal gas, no collisions).
    `region` = (x0, x1, y0, y1) in box fractions restricts where they start."""
    rng = np.random.default_rng(seed)
    c = np.array(center, dtype=float)
    x0, x1, y0, y1 = region or (0, 1, 0, 1)
    P = np.stack([rng.uniform(x0, x1, n), rng.uniform(y0, y1, n)], 1)
    V = speed * rng.standard_normal((n, 2)) / max(w, h)
    g = VGroup(*[Dot(c + [(p[0] - .5) * w, (p[1] - .5) * h, 0], radius=radius, color=color)
                 for p in P])

    def upd(m, dt):
        for k, d in enumerate(m.submobjects):
            P[k] += V[k] * dt
            for ax in (0, 1):
                if P[k, ax] < 0 or P[k, ax] > 1:
                    P[k, ax] = min(max(P[k, ax], 0), 1)
                    V[k, ax] *= -1
            d.move_to(c + [(P[k, 0] - .5) * w, (P[k, 1] - .5) * h, 0])
    g.add_updater(upd)
    return g


def fixed_dots(center, w, h, n=40, seed=0, radius=0.045, color=BLUE_C, region=None):
    rng = np.random.default_rng(seed)
    c = np.array(center, dtype=float)
    x0, x1, y0, y1 = region or (0, 1, 0, 1)
    return VGroup(*[Dot(c + [(rng.uniform(x0, x1) - .5) * w, (rng.uniform(y0, y1) - .5) * h, 0],
                        radius=radius, color=color) for _ in range(n)])


def arrow_field(center, w, h, fn, nx=7, ny=5, scale=0.5, color=TEAL, sw=3):
    """Little arrows on a regular grid; fn(x, y) -> (ux, uy) with x, y in [-1, 1]."""
    c = np.array(center, dtype=float)
    g = VGroup()
    for i in range(nx):
        for j in range(ny):
            x, y = -1 + 2 * (i + .5) / nx, -1 + 2 * (j + .5) / ny
            ux, uy = fn(x, y)
            p = c + [x * w / 2, y * h / 2, 0]
            d = np.array([ux, uy, 0.0]) * scale
            if np.linalg.norm(d) < 0.05:
                continue
            g.add(Arrow(p - d / 2, p + d / 2, buff=0, color=color, stroke_width=sw,
                        max_tip_length_to_length_ratio=0.45, max_stroke_width_to_length_ratio=8))
    return g


# --------------------------------------------------------------------------
# Phase-space helpers (phase-space "fluid" drawings)
# --------------------------------------------------------------------------
def shoelace(P):
    x, y = P[:, 0], P[:, 1]
    return 0.5 * abs(float(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1))))


def blob_base(cx=0.0, cy=0.0, s=1.0, n=96):
    """A lumpy closed curve (so rotation / shear is visible), as an (n, 2) array."""
    th = np.linspace(0, TAU, n, endpoint=False)
    r = s * (0.55 + 0.14 * np.cos(3 * th) + 0.09 * np.sin(2 * th + 0.6))
    return np.stack([cx + r * np.cos(th), cy + r * np.sin(th)], 1)


def flow_rot(P, t, w=1.0):
    """Harmonic-oscillator flow in (q, p):  qdot = p,  pdot = -q  (area preserving)."""
    c, s = math.cos(w * t), math.sin(w * t)
    return np.stack([P[:, 0] * c + P[:, 1] * s, -P[:, 0] * s + P[:, 1] * c], 1)


def blob_poly(P, center, sc, color, op=0.35):
    pts = [np.array([center[0] + x * sc, center[1] + y * sc, 0.0]) for x, y in P]
    m = VMobject(stroke_color=color, stroke_width=3, fill_color=color, fill_opacity=op)
    m.set_points_as_corners(pts + [pts[0]])
    return m


def phase_frame(center, w, h, xl="q", yl="p", color=GREY_B, size=24, box=True):
    """A framed (xl, yl) plane with crossed axes through the centre."""
    c = np.array(center, dtype=float)
    g = VGroup()
    if box:
        g.add(Rectangle(width=w, height=h, color=color, stroke_width=2).move_to(c))
    g.add(Arrow(c + [-w / 2 + 0.15, 0, 0], c + [w / 2 - 0.1, 0, 0], buff=0, color=color,
                stroke_width=2, max_tip_length_to_length_ratio=0.06),
          Arrow(c + [0, -h / 2 + 0.15, 0], c + [0, h / 2 - 0.1, 0], buff=0, color=color,
                stroke_width=2, max_tip_length_to_length_ratio=0.06))
    g.add(tex(xl, size, GREY_B).move_to(c + [w / 2 - 0.3, -0.3, 0]),
          tex(yl, size, GREY_B).move_to(c + [0.3, h / 2 - 0.3, 0]))
    return g


# --------------------------------------------------------------------------
# Hard-disk gas: event-driven molecular dynamics for the H-theorem scene.
# Event-driven dynamics is exactly time-reversible up to floating-point
# round-off, which is the whole point of the Loschmidt demonstration.
# --------------------------------------------------------------------------
import heapq


class DiskGas:
    """N elastic hard disks of radius r in the unit box [0,1]^2."""

    def __init__(self, pos, vel, r):
        self.pos, self.vel, self.r = np.array(pos, float), np.array(vel, float), float(r)
        self.n = len(self.pos)
        self.t = 0.0
        self.cnt = np.zeros(self.n, dtype=np.int64)
        self.heap, self._seq, self.ncoll = [], 0, 0
        for i in range(self.n):
            self._predict(i)

    # event = (time, seq, i, j, cnt_i, cnt_j);  j < 0  means wall (-1: x, -2: y)
    def _push(self, t, i, j):
        self._seq += 1
        cj = int(self.cnt[j]) if j >= 0 else -1
        heapq.heappush(self.heap, (t, self._seq, i, j, int(self.cnt[i]), cj))

    def _predict(self, i):
        p, v, r = self.pos[i], self.vel[i], self.r
        with np.errstate(divide="ignore", invalid="ignore"):
            tw = np.where(v > 0, (1 - r - p) / v, np.where(v < 0, (r - p) / v, np.inf))
        ax = int(np.argmin(tw))
        if np.isfinite(tw[ax]):
            self._push(self.t + max(float(tw[ax]), 0.0), i, -1 - ax)
        dr, dv = self.pos - p, self.vel - v
        b = (dr * dv).sum(1)
        a = (dv * dv).sum(1)
        c = (dr * dr).sum(1) - 4 * r * r
        disc = b * b - a * c
        ok = (b < 0) & (disc > 0) & (a > 0)
        ok[i] = False
        if ok.any():
            dt = np.full(self.n, np.inf)
            dt[ok] = (-b[ok] - np.sqrt(disc[ok])) / a[ok]
            j = int(np.argmin(dt))
            if np.isfinite(dt[j]):
                self._push(self.t + max(float(dt[j]), 0.0), i, j)

    def _advance(self, T):
        if T > self.t:
            self.pos += self.vel * (T - self.t)
            self.t = T

    def run_to(self, T):
        while self.heap and self.heap[0][0] <= T:
            te, _, i, j, ci, cj = heapq.heappop(self.heap)
            if self.cnt[i] != ci:
                continue
            if j >= 0 and self.cnt[j] != cj:
                self._predict(i)          # partner changed course: look again
                continue
            self._advance(te)
            self.cnt[i] += 1
            if j < 0:
                self.vel[i, -1 - j] *= -1
                self._predict(i)
            else:
                nrm = self.pos[j] - self.pos[i]
                nrm /= np.linalg.norm(nrm)
                dvn = float((self.vel[i] - self.vel[j]) @ nrm)
                self.vel[i] -= dvn * nrm
                self.vel[j] += dvn * nrm
                self.cnt[j] += 1
                self.ncoll += 1
                self._predict(i)
                self._predict(j)
        self._advance(T)
        return self.pos.copy()

    def reverse(self, eps=0.0, who=None, seed=1):
        """Flip every velocity (Loschmidt), optionally nudging some of them by eps."""
        self.vel *= -1
        if eps:
            rng = np.random.default_rng(seed)
            idx = np.arange(self.n) if who is None else np.atleast_1d(who)
            self.vel[idx] += eps * rng.standard_normal((len(idx), 2))
        self.cnt += 1
        self.heap = []
        for i in range(self.n):
            self._predict(i)

    def copy(self):
        g = DiskGas.__new__(DiskGas)
        g.pos, g.vel, g.r, g.n, g.t = self.pos.copy(), self.vel.copy(), self.r, self.n, self.t
        g.cnt, g.heap, g._seq, g.ncoll = self.cnt.copy(), list(self.heap), self._seq, self.ncoll
        return g


GAS_N, GAS_R = 220, 0.010


def gas_initial(n=GAS_N, r=GAS_R, corner=0.36, sigma=0.55, seed=7):
    """Particles on a jittered lattice in the lower-left corner, Maxwellian velocities."""
    rng = np.random.default_rng(seed)
    k = int(math.ceil(math.sqrt(n)))
    sp = (corner - 2 * r) / k
    pts = [(r + sp * (a + 0.5) + rng.uniform(-0.2, 0.2) * (sp - 2 * r),
            r + sp * (b + 0.5) + rng.uniform(-0.2, 0.2) * (sp - 2 * r))
           for a in range(k) for b in range(k)]
    pos = np.array(pts)[rng.permutation(k * k)[:n]]
    vel = sigma * rng.standard_normal((n, 2))
    vel -= vel.mean(0)
    return pos, vel


def coarse_H(pos, cells=6):
    """Coarse-grained position part of Boltzmann's H = sum p ln(p/A) (A = cell area / box area)."""
    idx = np.clip((pos * cells).astype(int), 0, cells - 1)
    cnt = np.zeros((cells, cells))
    np.add.at(cnt, (idx[:, 0], idx[:, 1]), 1)
    p = cnt[cnt > 0] / len(pos)
    return float((p * np.log(p * cells * cells)).sum())


@lru_cache(maxsize=4)
def gas_history(t_rev=1.3, t_end=2.6, fps=12, eps=1e-5, seed=7):
    """Three runs sampled at `fps` per unit sim-time.
    forward : 0 .. t_end                      (free expansion from the corner)
    exact   : forward to t_rev, flip, continue to t_end     (Loschmidt, no nudge)
    nudged  : same flip, but every velocity is nudged by eps
    Returns dict of arrays: t, and for each run (pos[k,N,2], H[k])."""
    pos0, vel0 = gas_initial(seed=seed)
    ts = np.arange(0, t_end + 1e-9, 1.0 / fps)
    out = {"t": ts, "t_rev": t_rev}

    def sample(g, times):
        P = np.array([g.run_to(t) for t in times])
        return P, np.array([coarse_H(p) for p in P])

    fwd = DiskGas(pos0, vel0, GAS_R)
    out["fwd"] = sample(fwd, ts)
    for name, e in (("exact", 0.0), ("nudged", eps)):
        g = DiskGas(pos0, vel0, GAS_R)
        P1, H1 = sample(g, ts[ts <= t_rev + 1e-9])
        g.run_to(t_rev)
        g.reverse(eps=e, seed=seed + 1)
        later = ts[ts > t_rev + 1e-9]
        P2, H2 = sample(g, later)
        out[name] = (np.concatenate([P1, P2]), np.concatenate([H1, H2]))
    return out


# ==========================================================================
# K0 -- title, scope   (cues 1-3)
# ==========================================================================
class K0(SyncedScene):
    AUDIO_START, AUDIO_END = START["K0"], END["K0"]
    STARS = (40, 5)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Hamiltonian, Boltzmann, to Navier-Stokes", FG)], font_size=30)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: "Hello and welcome to another video on the connection between
        # Hamiltonian dynamics, Boltzmann equation and Navier-Stokes."
        self.at(1, Write(title), run_time=1.4)
        nH = card("Hamiltonian\nN-body dynamics", BLUE, 26, 3.6)
        nB = card("Boltzmann\nequation", TEAL, 26, 3.6)
        nN = card("Navier-Stokes\nequations", ORANGE, 26, 3.6)
        row = VGroup(nH, nB, nN).arrange(RIGHT, buff=1.5).move_to([0, 0.3, 0])
        clamp_width(row)
        a1 = Arrow(nH.get_right(), nB.get_left(), buff=0.1, color=GREY_B)
        a2 = Arrow(nB.get_right(), nN.get_left(), buff=0.1, color=GREY_B)
        self.frac(1, 0.25, FadeIn(nH, shift=UP * 0.2), run_time=0.6)
        self.frac(1, 0.5, FadeIn(nB, shift=UP * 0.2), GrowArrow(a1), run_time=0.6)
        self.frac(1, 0.75, FadeIn(nN, shift=UP * 0.2), GrowArrow(a2), run_time=0.6)

        # cue 2: derive Navier-Stokes from Boltzmann, and Boltzmann from the N-body Hamiltonian
        b2 = CurvedArrow(nN.get_bottom() + DOWN * 0.1, nB.get_bottom() + DOWN * 0.1,
                         angle=-TAU / 6, color=YELLOW)
        b1 = CurvedArrow(nB.get_bottom() + DOWN * 0.1, nH.get_bottom() + DOWN * 0.1,
                         angle=-TAU / 6, color=YELLOW)
        l2 = note("derive", 22, YELLOW).next_to(b2, DOWN, buff=0.1)
        l1 = note("derive", 22, YELLOW).next_to(b1, DOWN, buff=0.1)
        self.at(2, Create(b2), FadeIn(l2), run_time=0.9)
        self.frac(2, 0.5, Create(b1), FadeIn(l1), run_time=0.9)

        # cue 3: examine the connection, and when it is appropriate to use one and not the other
        q = chip("when is it appropriate to use one and not the other?", YELLOW, 26, fill=0.1)
        q.move_to([0, -2.7, 0])
        clamp_width(q)
        self.at(3, FadeIn(q, shift=UP * 0.2), run_time=0.7)
        self.snap("scope")
        self.finish()


# ==========================================================================
# K1 -- Overview I: three levels of description   (cues 4-10)
# ==========================================================================
def level_card(title, eq, tags, color, y, w=6.6, h=1.65, x=-3.45):
    box = RoundedRectangle(corner_radius=0.12, width=w, height=h, color=color).move_to([x, y, 0])
    t = Text(title, font_size=22, color=color).move_to([x, y + h / 2 - 0.29, 0])
    e = fit_box(tex(eq, 28), w - 0.6, 0.6).move_to([x, y + 0.02, 0])
    tg = Text(tags, font_size=17, color=GREY_B).move_to([x, y - h / 2 + 0.27, 0])
    return VGroup(box, t, e, tg)


class K1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K1"], END["K1"]

    def construct(self):
        header = hdr("Brief Overview")
        # cue 4: "So for a brief overview, we're going to take the route that we're going to pass through."
        self.at(4, Write(header), run_time=0.7)

        C, W, H = np.array([3.95, -0.35, 0.0]), 5.2, 4.3
        frame = box_frame(C, W, H)
        cA = level_card("1  Microscopic (Hamiltonian)",
                        r"f(\va r_1,\dots,\va r_N,\va p_1,\dots,\va p_N,t)",
                        "exact · reversible · 6N variables", BLUE, 1.5)
        cB = level_card("2  Kinetic (Boltzmann)", r"f_1(\va r,\va p,t)",
                        "approximate · irreversible · 6 variables + t", TEAL, -0.35)
        cC = level_card("3  Hydrodynamic (Euler, Navier-Stokes)",
                        r"\rho(\va r,t),\ \ \va u(\va r,t),\ \ T(\va r,t)",
                        "more approximate · dissipative · 5 fields", ORANGE, -2.2)

        # cue 5: microscopic -- probability density on a 6N-dimensional phase space
        dots = bouncing_dots(C, W - 0.15, H - 0.15, n=46, seed=1)
        self.at(5, FadeIn(cA, shift=RIGHT * 0.2), FadeIn(frame), FadeIn(dots), run_time=0.8)
        # cue 6: about 10^23 particles' worth of variables
        n23 = tex(r"N \sim 10^{23}", 30, BLUE_C).next_to(frame, DOWN, buff=0.12)
        self.at(6, FadeIn(n23, shift=UP * 0.1), run_time=0.5)
        self.snap("micro")

        # cue 7: kinetic -- a single-particle distribution; follow ONE particle
        dots.clear_updaters()
        pts = [(-2.1, -1.2), (-1.2, -0.5), (-0.5, -1.0), (0.3, 0.0), (0.9, -0.7),
               (1.7, 0.4), (1.2, 1.4), (0.2, 1.2)]
        P = [C + np.array([x, y, 0.0]) for x, y in pts]
        path = VMobject(color=YELLOW, stroke_width=4)
        path.set_points_as_corners(P)
        kinks = VGroup(*[Circle(radius=0.11, color=RED, stroke_width=3).move_to(p) for p in P[1:-1]])
        head = Dot(P[-1], radius=0.08, color=YELLOW)
        self.at(7, FadeOut(n23), FadeIn(cB, shift=RIGHT * 0.2), dots.animate.set_opacity(0.18),
                run_time=0.8)
        self.play(Create(path), run_time=2.4, rate_func=linear)
        self.play(LaggedStart(*[FadeIn(k, scale=2) for k in kinks], lag_ratio=0.15),
                  FadeIn(head), run_time=1.2)
        # cue 8: approximate and irreversible
        kl = note("collisions kink the path", 19, RED).next_to(frame, DOWN, buff=0.12)
        self.at(8, FadeIn(kl), run_time=0.4)
        self.snap("kinetic")

        # cue 9: hydrodynamics -- five smooth fields
        swirl = lambda x, y: (-0.9 * y + 0.35, 0.9 * x + 0.25 * math.sin(2.5 * y))
        vf = arrow_field(C, W - 0.5, H - 0.6, swirl, nx=7, ny=5, scale=0.55)
        self.at(9, FadeOut(dots), FadeOut(path), FadeOut(kinks), FadeOut(head), FadeOut(kl),
                FadeIn(cC, shift=RIGHT * 0.2), FadeIn(vf), run_time=1.0)
        # cue 10: a bit more approximate still, and dissipative
        sm = note("smooth fields  ρ, u, T", 19, ORANGE).next_to(frame, DOWN, buff=0.12)
        self.at(10, FadeIn(sm), run_time=0.4)
        self.snap("hydro")
        self.finish(keep=[header])


# ==========================================================================
# K1_1 -- Overview II: separation of scales   (cues 11-16)
# ==========================================================================
class K1_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K1_1"], END["K1_1"]

    def construct(self):
        header = hdr("Brief Overview")
        self.add(header)
        xs = [-4.4, 0.0, 4.4]
        yd = 1.45

        # cue 11: each step throws information away; legitimate only with separation of scales
        t11 = Text("each step throws away information...", font_size=32, color=YELLOW)
        t11.move_to([0, 0.9, 0])
        t11b = Text("...and is only legitimate when scales separate", font_size=32, color=ORANGE)
        t11b.next_to(t11, DOWN, buff=0.4)
        self.at(11, FadeIn(t11, shift=UP * 0.15), run_time=0.7)
        self.frac(11, 0.55, FadeIn(t11b, shift=UP * 0.15), run_time=0.7)

        # cue 12: d and lambda
        c_d = np.array([xs[0], yd, 0.0])
        d_dot = Dot(c_d, radius=0.07, color=BLUE_C)
        d_ring = DashedVMobject(Circle(radius=0.55, color=BLUE_C, stroke_width=3), num_dashes=24).move_to(c_d)
        d_arr = Arrow(c_d, c_d + [0.55, 0, 0], buff=0, color=YELLOW, stroke_width=4,
                      max_tip_length_to_length_ratio=0.3)
        d_lab = tex(r"d", 28, YELLOW).move_to(c_d + [0.28, 0.25, 0])
        diag_d = VGroup(d_ring, d_dot, d_arr, d_lab)
        sym_d = tex(r"d", 40, YELLOW).move_to([xs[0], 0.35, 0])
        def_d = note("range of the interaction\n(~ molecular diameter)", 18).move_to([xs[0], -0.45, 0])

        c_l = np.array([xs[1], yd, 0.0])
        zz = [(-1.5, -0.5), (-0.75, 0.45), (0.1, -0.45), (0.95, 0.5), (1.55, -0.1)]
        Z = [c_l + np.array([x, y, 0.0]) for x, y in zz]
        zpath = VMobject(color=GREY_B, stroke_width=3)
        zpath.set_points_as_corners(Z)
        seg = Line(Z[1], Z[2], color=YELLOW, stroke_width=6)
        zdots = VGroup(*[Dot(p, radius=0.06, color=RED) for p in Z[1:-1]])
        diag_l = VGroup(zpath, seg, zdots)
        sym_l = tex(r"\lambda \sim \frac{1}{n\,d^{2}}", 36, YELLOW).move_to([xs[1], 0.4, 0])
        def_l = note("mean free path:\ndistance between collisions", 18).move_to([xs[1], -0.45, 0])

        self.at(12, FadeOut(t11), FadeOut(t11b), FadeIn(diag_d), Write(sym_d), FadeIn(def_d), run_time=1.0)
        self.frac(12, 0.38, Create(zpath), FadeIn(zdots), run_time=1.4)
        self.play(Create(seg), Write(sym_l), FadeIn(def_l), run_time=1.0)

        # cue 13: L, the scale on which density, velocity, temperature change
        c_L = np.array([xs[2], yd, 0.0])
        bars = VGroup(*[Rectangle(width=0.28, height=0.8, stroke_width=0, fill_color=BLUE,
                                  fill_opacity=0.08 + 0.085 * k).move_to(c_L + [-1.4 + 0.28 * (k + .5), 0.1, 0])
                        for k in range(10)])
        Larr = DoubleArrow(c_L + [-1.4, -0.45, 0], c_L + [1.4, -0.45, 0], buff=0, color=YELLOW,
                           stroke_width=4, max_tip_length_to_length_ratio=0.05)
        diag_L = VGroup(bars, Larr)
        sym_L = tex(r"L", 40, YELLOW).move_to([xs[2], 0.35, 0])
        def_L = note("scale over which ρ, u, T\nchange appreciably", 18).move_to([xs[2], -0.45, 0])
        self.at(13, FadeIn(bars), GrowFromCenter(Larr), Write(sym_L), FadeIn(def_L), run_time=1.2)
        self.snap("lengths")

        # cue 14: d << lambda << L, and Knudsen number
        ineq = tex(r"d \ll \lambda \ll L", 42).move_to([-4.2, -1.5, 0])
        kn = tex(r"\mathrm{Kn}=\lambda/L", 38, ORANGE).move_to([0.2, -1.5, 0])
        kbox = SurroundingRectangle(kn, color=ORANGE, buff=0.15)
        knote = note("the control parameter for the second half", 16, ORANGE).next_to(kbox, DOWN, buff=0.08)
        self.at(14, Write(ineq), run_time=1.2)
        self.frac(14, 0.4, Write(kn), Create(kbox), run_time=1.0)
        self.frac(14, 0.75, FadeIn(knote), run_time=0.5)

        # cue 15: the matching hierarchy of times
        tt = [tex(r"\tau_c \sim d/\bar v", 28, BLUE_C),
              tex(r"\tau \sim \lambda/\bar v", 28, TEAL),
              tex(r"T_{\rm fl} \sim L/\bar v", 28, ORANGE)]
        tn = ["collision lasts", "time between collisions", "fluid changes"]
        grp = []
        for x, m, s in zip(xs, tt, tn):
            m.move_to([x, -2.8, 0])
            grp.append((m, note(s, 16).move_to([x, -3.3, 0])))
        self.at(15, FadeIn(grp[0][0]), FadeIn(grp[0][1]), run_time=0.7)
        self.frac(15, 0.4, FadeIn(grp[1][0]), FadeIn(grp[1][1]), run_time=0.7)
        self.frac(15, 0.72, FadeIn(grp[2][0]), FadeIn(grp[2][1]), run_time=0.7)

        # cue 16: v-bar is the typical particle speed
        vb = note("v̄ = typical particle speed", 20, YELLOW).move_to([4.3, -1.5, 0])
        self.at(16, FadeIn(vb), run_time=0.5)
        self.snap("times")
        self.finish(keep=[header])


# ==========================================================================
# K1_2 -- Overview III: the route and the closure thread   (cues 17-26)
# ==========================================================================
class K1_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K1_2"], END["K1_2"]

    def construct(self):
        header = hdr("Brief Overview")
        self.add(header)
        X = -3.9
        ys = [1.75, 0.3, -1.15, -2.6]
        nodes = [card("Liouville / BBGKY\nexact, reversible", BLUE, 22, 4.6),
                 card("Boltzmann\nirreversible", TEAL, 22, 4.6),
                 card("Euler\nreversible again", GREEN, 22, 4.6),
                 card("Navier-Stokes\ndissipative", ORANGE, 22, 4.6)]
        for nd, y in zip(nodes, ys):
            nd.move_to([X, y, 0])
        arrows = [Arrow(nodes[k].get_bottom(), nodes[k + 1].get_top(), buff=0.05, color=GREY_B)
                  for k in range(3)]
        alab = [tex(r"d \ll \lambda:\ \text{molecular chaos}", 22, YELLOW),
                tex(r"\mathrm{Kn}\to 0:\ \text{local equilibrium}", 22, YELLOW),
                tex(r"\text{first order in } \mathrm{Kn}", 22, YELLOW)]
        for lab, ar in zip(alab, arrows):
            lab.next_to(ar, RIGHT, buff=0.2)

        # cue 17: the exact, reversible starting point
        self.at(17, FadeIn(nodes[0], shift=DOWN * 0.15), run_time=0.7)
        # cue 18: d << lambda -> molecular chaos -> Boltzmann, irreversible
        self.at(18, GrowArrow(arrows[0]), FadeIn(alab[0]), FadeIn(nodes[1], shift=DOWN * 0.15), run_time=0.9)
        # cue 19: Kn -> 0, local equilibrium -> Euler
        self.at(19, GrowArrow(arrows[1]), FadeIn(alab[1]), FadeIn(nodes[2], shift=DOWN * 0.15), run_time=0.9)
        # cue 20: first order in Kn -> Navier-Stokes
        self.at(20, GrowArrow(arrows[2]), FadeIn(alab[2]), FadeIn(nodes[3], shift=DOWN * 0.15), run_time=0.9)
        self.snap("route")

        # cue 21-22: a common thread: both arrows are closure problems
        thread = chip("common thread:  closure problems", YELLOW, 22, fill=0.1).move_to([3.9, 1.9, 0])
        self.at(21, FadeIn(thread, shift=LEFT * 0.2), run_time=0.6)
        self.at(22, Indicate(arrows[0], color=YELLOW, scale_factor=1.3),
                Indicate(arrows[1], color=YELLOW, scale_factor=1.3), run_time=1.0)

        # cue 23: BBGKY never closes -> assume something about two-particle correlations
        k1 = card("BBGKY never closes on its own\n→ assume something about\n    two-particle correlations", BLUE, 20, 5.6)
        k1.move_to([3.9, 0.55, 0])
        self.at(23, FadeIn(k1, shift=LEFT * 0.2), Indicate(arrows[0], color=BLUE, scale_factor=1.3), run_time=0.8)
        # cues 24-25: the moment equations never close -> assume the shape of the velocity distribution
        k2 = card("moment equations never close\n→ assume something about the\n    shape of the velocity distribution", TEAL, 20, 5.6)
        k2.move_to([3.9, -1.0, 0])
        self.at(24, FadeIn(k2, shift=LEFT * 0.2), Indicate(arrows[1], color=TEAL, scale_factor=1.3), run_time=0.8)
        # cue 26: each closure is where new physics enters, but we lose information
        k3 = note("each closure: new physics comes in,\nbut information about the system is lost", 21, YELLOW)
        k3.move_to([3.9, -2.4, 0])
        self.at(26, FadeIn(k3, shift=UP * 0.15), run_time=0.7)
        self.snap("closure")
        self.finish()


# ==========================================================================
# K2 -- N-body Hamiltonian, probability density, continuity   (cues 27-37)
# ==========================================================================
def phase_blob(center, w, h, tr, color=TEAL, sc=0.9, base=None, xl="q", yl="p"):
    """Framed phase plane + a lumpy blob carried by the harmonic flow (area preserving)."""
    c = np.array(center, dtype=float)
    base = blob_base(0, 0, 1.0) if base is None else base
    frame = phase_frame(c, w, h, xl, yl)
    blob = always_redraw(lambda: blob_poly(flow_rot(base, tr.get_value()), c, sc, color, 0.35))
    return frame, blob


class K2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K2"], END["K2"]

    def construct(self):
        header = hdr("From Liouville to BBGKY")
        # cue 27: "we are going to start with the N-body Hamiltonian"
        self.at(27, Write(header), run_time=0.7)
        Hm = MathTex(r"H=", r"\frac{1}{2m}\sum_{i=1}^{N}|\va p_i|^{2}", r"+\sum_{i=1}^{N}V(\va r_i)",
                     r"+\sum_{i<j}U(\va r_i-\va r_j)", font_size=40)
        Hm[1].set_color(BLUE_C)
        Hm[2].set_color(GREEN)
        Hm[3].set_color(ORANGE)
        fit_box(Hm, CONTENT_MAX_WIDTH).move_to([0, 1.35, 0])
        self.sync(CUES[27].start + 1.0)
        self.play(Write(Hm[0]), Write(Hm[1]), run_time=1.5)
        b1 = Brace(Hm[1], DOWN, buff=0.1, color=BLUE_C)
        l1 = note("kinetic energy", 19, BLUE_C).next_to(b1, DOWN, buff=0.1)
        self.play(GrowFromCenter(b1), FadeIn(l1), run_time=0.5)

        # cue 28: external force F = -grad V, acting equally on every particle
        b2 = Brace(Hm[2], DOWN, buff=0.1, color=GREEN)
        l2 = VGroup(tex(r"\va F=-\nabla V", 26, GREEN),
                    note("same on every particle", 17, GREEN)).arrange(DOWN, buff=0.08).next_to(b2, DOWN, buff=0.1)
        self.at(28, Write(Hm[2]), run_time=1.0)
        self.play(GrowFromCenter(b2), FadeIn(l2), run_time=0.6)

        # cue 29: two-body interaction
        b3 = Brace(Hm[3], DOWN, buff=0.1, color=ORANGE)
        l3 = note("two-body interaction", 19, ORANGE).next_to(b3, DOWN, buff=0.1)
        self.at(29, Write(Hm[3]), run_time=1.2)
        self.play(GrowFromCenter(b3), FadeIn(l3), run_time=0.6)
        # cue 30: later we assume the potential is short ranged
        l3b = note("(short-ranged, later)", 17, YELLOW).next_to(l3, DOWN, buff=0.08)
        self.at(30, FadeIn(l3b), run_time=0.5)
        # cue 31: position is r rather than q, for convenience
        rq = note("notation: position is r, not q", 19, GREY_B).move_to([-3.6, -1.0, 0])
        self.at(31, FadeIn(rq), run_time=0.5)

        # cue 32-33: a probability density on phase space, normalized
        fdef = tex(r"f(\va r_1,\dots,\va r_N,\va p_1,\dots,\va p_N,\,t)", 36, YELLOW)
        fdef.move_to([0, -1.9, 0])
        fch = chip("probability density on phase space", YELLOW, 20).next_to(fdef, UP, buff=0.25)
        self.at(32, FadeOut(rq), FadeIn(fch), Write(fdef), run_time=1.2)
        norm = tex(r"\int dV\,f=1,\qquad dV=\prod_{i=1}^{N}d^3r_i\,d^3p_i", 34)
        norm.move_to([0, -2.85, 0])
        self.at(33, Write(norm), run_time=1.4)
        self.snap("setup")

        # cue 34: probability is locally conserved -> continuity.  Make room.
        old = [b1, l1, b2, l2, b3, l3, l3b, fch, fdef, norm]
        self.at(34, *[FadeOut(m) for m in old], Hm.animate.scale(0.8).move_to([0, 1.95, 0]), run_time=0.8)
        # cue 35: a change at one point of phase space is compensated by its neighbours
        tr = ValueTracker(0.0)
        C = np.array([-3.5, -0.6, 0.0])
        frame, blob = phase_blob(C, 5.2, 3.5, tr, TEAL, 0.95)
        fl = note("probability is neither created nor destroyed:\nit only flows through phase space", 19, TEAL)
        fl.next_to(frame, DOWN, buff=0.1)
        self.at(35, FadeIn(frame), FadeIn(fl), run_time=0.6)
        self.add(blob)
        self.play(tr.animate.set_value(2.6), run_time=4.5, rate_func=linear)
        # cue 36: the continuity equation
        cont = MathTex(r"\pdv{f}{t}&+\sum_{i}\nabla_{\va r_i}\!\cdot\!\big(\dot{\va r}_i f\big)\\"
                       r"&+\sum_{i}\nabla_{\va p_i}\!\cdot\!\big(\dot{\va p}_i f\big)=0", font_size=34)
        fit_box(cont, 6.4).move_to([3.5, 0.45, 0])
        self.at(36, Write(cont), tr.animate.set_value(5.0), run_time=2.6)
        # cue 37: use Hamilton's equations to turn this into a Hamiltonian equation
        ham = tex(r"\dot{\va r}_i=\pdv{H}{\va p_i},\qquad \dot{\va p}_i=-\pdv{H}{\va r_i}", 34, YELLOW)
        fit_box(ham, 6.4).move_to([3.5, -1.55, 0])
        self.at(37, Write(ham), run_time=1.6)
        self.snap("continuity")
        self.finish(keep=[header])


# ==========================================================================
# K2_1 -- Liouville's equation, Poisson bracket, equilibrium   (cues 38-43)
# ==========================================================================
class K2_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K2_1"], END["K2_1"]

    def construct(self):
        header = hdr("From Liouville to BBGKY")
        self.add(header)
        # cue 38: Liouville's equation
        liou = tex(r"\pdv{f}{t}+\sum_{i=1}^{N}\left(\pdv{f}{\va r_i}\cdot\pdv{H}{\va p_i}"
                   r"-\pdv{f}{\va p_i}\cdot\pdv{H}{\va r_i}\right)=0", 34)
        fit_box(liou, 9.4).move_to([-1.6, 1.55, 0])
        lbox = SurroundingRectangle(liou, color=YELLOW, buff=0.2)
        lname = note("Liouville's equation", 20, YELLOW).next_to(lbox, RIGHT, buff=0.3)
        self.at(38, Write(liou), run_time=1.8)
        self.play(Create(lbox), FadeIn(lname), run_time=0.6)

        # cue 39: it says f does not change along a trajectory -- total derivative
        tot = tex(r"\dv{f}{t}=0", 44, TEAL).move_to([-3.4, 0.2, 0])
        tr = ValueTracker(0.0)
        C = np.array([3.9, -0.7, 0.0])
        frame, blob = phase_blob(C, 5.0, 2.7, tr, TEAL, 0.75)
        p0 = np.array([[0.5, 0.15]])
        ring = DashedVMobject(Circle(radius=float(np.hypot(0.5, 0.15)) * 0.8, color=YELLOW, stroke_width=2),
                              num_dashes=36).move_to(C)
        tracer = always_redraw(lambda: Dot(
            C + np.append(flow_rot(p0, tr.get_value())[0] * 0.8, 0), radius=0.08, color=YELLOW))
        tl = note("follow one point: its f never changes", 18, YELLOW).next_to(frame, DOWN, buff=0.08)
        self.at(39, Write(tot), FadeIn(frame), FadeIn(ring), FadeIn(tl), run_time=1.2)
        self.add(blob, tracer)
        self.play(tr.animate.set_value(6.0), run_time=7.5, rate_func=linear)

        # cue 40: express it with the Poisson bracket
        pb = MathTex(r"\pb{A}{B}\equiv\sum_i\Big(\pdv{A}{\va r_i}\cdot\pdv{B}{\va p_i}\\"
                     r"&\qquad-\pdv{A}{\va p_i}\cdot\pdv{B}{\va r_i}\Big)", font_size=32)
        fit_box(pb, 6.6).move_to([-3.3, -0.4, 0])
        self.at(40, FadeOut(tot), run_time=0.4)
        self.play(Write(pb), run_time=1.8)
        # cue 41: df/dt = {H, f}; an equilibrium distribution is time independent
        dfdt = tex(r"\pdv{f}{t}=\pb{H}{f}", 40).move_to([-3.4, -1.95, 0])
        eqm = tex(r"\pdv{f}{t}=0\ \Longleftrightarrow\ \pb{H}{f}=0", 34, ORANGE).move_to([-1.7, -2.95, 0])
        self.at(41, Write(dfdt), run_time=1.0)
        self.frac(41, 0.4, Write(eqm), run_time=1.4)
        self.snap("poisson")

        # cue 42: f must be a function of H; the Boltzmann distribution is the famous example
        fH = tex(r"f=f(H)", 38, YELLOW).move_to([3.7, -1.75, 0])
        bz = tex(r"f\propto e^{-\beta H}", 38, GREEN).move_to([3.7, -2.75, 0])
        self.at(42, FadeOut(frame), FadeOut(blob), FadeOut(tracer), FadeOut(ring), FadeOut(tl),
                Write(fH), run_time=1.0)
        self.frac(42, 0.4, Write(bz), run_time=1.2)
        # cue 43: "This is just one."
        one = note("just one of many solutions", 20, GREY_B).next_to(bz, DOWN, buff=0.15)
        self.at(43, FadeIn(one), run_time=0.5)
        self.finish(keep=[header])


# ==========================================================================
# K2_2 -- expectation values:  d<A>/dt = <{A,H}>   (cues 44-48)
# ==========================================================================
class K2_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K2_2"], END["K2_2"]

    def construct(self):
        header = hdr("From Liouville to BBGKY")
        self.add(header)
        # cue 44: an observable A(r, p) and its expectation value
        L = lambda m, y, x=LEFT_X: at_left(fit_box(m, 12.0), y, x)
        e1 = L(tex(r"\langle A\rangle=\int dV\;A(\va r_i,\va p_i)\,f(\va r_i,\va p_i,t)", 38), 1.45)
        c1 = chip("any function A on phase space", BLUE, 20).move_to([4.6, 2.2, 0])
        self.at(44, Write(e1), run_time=1.6)
        self.play(FadeIn(c1, shift=LEFT * 0.2), run_time=0.5)

        # cue 45: its time dependence, integrating by parts
        e2 = L(tex(r"\dv{\langle A\rangle}{t}=\int dV\,A\,\pdv{f}{t}=\int dV\,A\,\pb{H}{f}", 38), 0.2)
        e3 = L(tex(r"=\int dV\,\pb{A}{H}\,f", 38, YELLOW), -0.95, x=LEFT_X + 0.5)
        self.at(45, Write(e2), run_time=2.0)
        self.frac(45, 0.55, Write(e3), run_time=1.6)
        by = note("integrated by parts", 19, GREY_B).next_to(e3, RIGHT, buff=0.5)
        self.play(FadeIn(by), run_time=0.4)

        # cue 46: boundary terms vanish because f is normalised, so f -> 0 far away in phase space
        bd = card("boundary terms vanish:\nf is normalised, so f → 0 in the far reaches of phase space",
                  RED, 19, 8.2).move_to([-0.9, -2.2, 0])
        self.at(46, FadeIn(bd, shift=UP * 0.15), run_time=0.7)

        # cue 47: the derivative of the expectation value is the expectation of the bracket
        res = tex(r"\dv{\langle A\rangle}{t}=\big\langle\pb{A}{H}\big\rangle", 46, YELLOW)
        res.move_to([0, -0.5, 0])
        rb = SurroundingRectangle(res, color=YELLOW, buff=0.25)
        self.at(47, FadeOut(e1), FadeOut(c1), FadeOut(e2), FadeOut(e3), FadeOut(by), FadeOut(bd), run_time=0.5)
        self.play(Write(res), Create(rb), run_time=1.4)
        # cue 48: this looks just like the quantum expectation value (Ehrenfest)
        qm = tex(r"\dv{\langle A\rangle}{t}=\frac{1}{i\hbar}\big\langle[A,H]\big\rangle", 40, PURPLE)
        qm.move_to([0, -2.1, 0])
        corr = tex(r"\pb{\cdot}{\cdot}\ \longleftrightarrow\ \frac{1}{i\hbar}[\cdot,\cdot]", 30, GREY_B)
        corr.next_to(qm, DOWN, buff=0.35)
        self.at(48, Write(qm), run_time=1.5)
        self.play(FadeIn(corr), run_time=0.6)
        self.snap("quantum")
        self.finish(keep=[header])


# ==========================================================================
# K3 -- marginalise to f_1: streaming + collision terms   (cues 49-66)
# ==========================================================================
class K3(SyncedScene):
    AUDIO_START, AUDIO_END = START["K3"], END["K3"]

    def construct(self):
        header = hdr("The BBGKY Hierarchy")
        # cue 49: we have a distribution, but we still have a problem
        self.at(49, Write(header), run_time=0.6)
        fN = fit_box(tex(r"f(\va r_1,\dots,\va r_N,\va p_1,\dots,\va p_N,t)", 38, YELLOW), 6.6)
        fN.move_to([-3.2, 1.35, 0])
        self.sync(CUES[49].start + 0.9)
        self.play(Write(fN), run_time=1.5)
        # cue 50: an obscene number of variables
        br = Brace(fN, DOWN, buff=0.1, color=RED)
        lab = tex(r"6N\ \text{variables}", 30, RED).next_to(br, DOWN, buff=0.1)
        self.at(50, GrowFromCenter(br), FadeIn(lab), run_time=0.7)
        # cue 51: N ~ 10^23
        nn = tex(r"N\sim 10^{23}", 40, RED).next_to(lab, DOWN, buff=0.3)
        C, W, H = np.array([3.9, 0.2, 0.0]), 4.8, 3.6
        frame = box_frame(C, W, H)
        dots = fixed_dots(C, W - 0.3, H - 0.3, n=70, seed=4, radius=0.05)
        self.at(51, Write(nn), FadeIn(frame), FadeIn(dots), run_time=0.9)
        # cue 52: focus on ONE particle
        one = dots[9]
        self.at(52, dots.animate.set_opacity(0.25), run_time=0.4)
        self.play(one.animate.set_color(YELLOW).set_opacity(1).scale(2.2), run_time=0.5)
        # cue 53: all particles are identical -- none is privileged
        self.at(53, LaggedStart(*[Indicate(dots[k], color=GREEN, scale_factor=2.0) for k in (3, 21, 34, 48, 60)],
                                lag_ratio=0.25), run_time=2.4)
        idn = note("identical particles: none is privileged", 19, GREEN).next_to(frame, DOWN, buff=0.12)
        self.play(FadeIn(idn), run_time=0.4)
        # cue 54: density, velocity, energy flux ... all need one function
        qs = note("density  ·  mean velocity  ·  energy flux  ·  ...", 22, GREY_B).move_to([-3.2, -1.2, 0])
        self.at(54, FadeIn(qs, shift=UP * 0.1), run_time=0.6)
        # cue 55: ... all require f_1
        f1 = tex(r"f_1(\va r,\va p,t)", 52, YELLOW).move_to([-3.2, -2.4, 0])
        ar = Arrow(qs.get_bottom(), f1.get_top(), buff=0.1, color=GREY_B)
        self.at(55, GrowArrow(ar), Write(f1), run_time=1.0)
        self.snap("f1")

        # cue 56: define the time derivative of f_1 by integrating {H, f} over particles 2..N
        old = [fN, br, lab, nn, frame, dots, idn, qs, f1, ar]
        L1 = fit_box(tex(r"\pdv{f_1}{t}=N\!\int\prod_{i=2}^{N}d^3r_i\,d^3p_i\;\pb{H}{f}", 34), 11.5)
        L1.move_to([0, 1.8, 0])
        self.at(56, *[FadeOut(m) for m in old], run_time=0.5)
        self.play(Write(L1), run_time=1.6)
        # cue 57: expand with the Hamiltonian
        L2 = MathTex(r"&=N\!\int\prod_{i=2}^{N}d^3r_i\,d^3p_i\sum_{j=1}^{N}\Big(-\frac{\va p_j}{m}\cdot\pdv{f}{\va r_j}"
                     r"+\nabla V(\va r_j)\cdot\pdv{f}{\va p_j}\\"
                     r"&\hspace{6em}+\sum_{k\ne j}\nabla U(\va r_j-\va r_k)\cdot\pdv{f}{\va p_j}\Big)", font_size=30)
        fit_box(L2, 12.0).move_to([0, 0.35, 0])
        self.at(57, Write(L2), run_time=2.4)
        # cue 58-59: for j = 2..N integrate by parts: every such term vanishes
        n58 = note("terms with j = 2, ..., N:  integrate by parts, moving the derivative off f", 21, ORANGE)
        n58.move_to([0, -1.1, 0])
        self.at(58, FadeIn(n58, shift=UP * 0.1), run_time=0.6)
        n59 = note("→ they vanish: what multiplies f depends only on p (or only on r) for that particle,\n"
                   "   and f dies off at the edge of phase space", 20, RED).move_to([0, -1.1, 0])
        self.at(59, FadeOut(n58), FadeIn(n59), run_time=0.5)
        # cue 60: only the r_1, p_1 derivatives survive
        L3 = MathTex(r"\pdv{f_1}{t}&=N\!\int\prod_{i=2}^{N}d^3r_i\,d^3p_i\Big(-\frac{\va p}{m}\cdot\pdv{f}{\va r}"
                     r"+\nabla V\cdot\pdv{f}{\va p}\\"
                     r"&\hspace{6em}+\sum_{k=2}^{N}\nabla_{\va r}U(\va r-\va r_k)\cdot\pdv{f}{\va p}\Big)", font_size=30)
        fit_box(L3, 12.0).move_to([0, 0.35, 0])
        self.at(60, FadeOut(n59), ReplacementTransform(L2, L3), run_time=1.4)
        # cue 61: rename r_1 -> r, p_1 -> p
        rn = tex(r"\va r_1\equiv\va r,\qquad \va p_1\equiv\va p", 30, YELLOW).move_to([0, -1.3, 0])
        self.at(61, FadeIn(rn, shift=UP * 0.1), run_time=0.6)
        # cue 62: ... a Liouville-like equation with an added collision term
        R = MathTex(r"\pdv{f_1}{t}=", r"\pb{H_1}{f_1}", r"+", r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}", font_size=46)
        R.move_to([0, -2.4, 0])
        self.at(62, Write(R), run_time=1.6)
        self.snap("result")

        # cue 63-65: streaming vs collision
        self.at(63, *[FadeOut(m) for m in (L1, L3, rn)], R.animate.move_to([0, 1.5, 0]), run_time=0.8)
        R[1].set_color(BLUE_C)
        R[3].set_color(ORANGE)
        bS = Brace(R[1], DOWN, buff=0.1, color=BLUE_C)
        lS = note("streaming term", 21, BLUE_C).next_to(bS, DOWN, buff=0.1)
        self.play(GrowFromCenter(bS), FadeIn(lS), run_time=0.6)
        # cue 64: how particles move in the absence of collisions
        P0 = np.array([-5.6, -1.5, 0.0])
        straight = Arrow(P0, P0 + [3.8, 0, 0], buff=0, color=BLUE_C, stroke_width=4)
        sdot = Dot(P0 + [1.0, 0, 0], radius=0.09, color=YELLOW)
        sl = note("free flight", 18, BLUE_C).next_to(straight, DOWN, buff=0.15)
        self.at(64, GrowArrow(straight), FadeIn(sdot), FadeIn(sl), run_time=1.0)
        # cue 65: the collision integral
        bC = Brace(R[3], DOWN, buff=0.1, color=ORANGE)
        lC = note("collision integral", 21, ORANGE).next_to(bC, DOWN, buff=0.1)
        Q = np.array([2.2, -1.5, 0.0])
        kp = VMobject(color=ORANGE, stroke_width=4)
        kp.set_points_as_corners([Q + [-2.4, -0.1, 0], Q + [0, 0, 0], Q + [1.9, 0.9, 0]])
        other = Line(Q + [-0.4, 1.0, 0], Q + [0.4, -1.0, 0], color=GREY_B, stroke_width=3)
        kd = Circle(radius=0.14, color=RED, stroke_width=3).move_to(Q)
        self.at(65, GrowFromCenter(bC), FadeIn(lC), Create(other), Create(kp), FadeIn(kd), run_time=1.4)
        # cue 66: all particles identical -> the N-1 terms are identical: one two-body integral
        cl = tex(r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}=\int d^3r_2\,d^3p_2\;\nabla_{\va r}U(\va r-\va r_2)\cdot\pdv{f_2}{\va p}",
                 34, ORANGE)
        fit_box(cl, 11.5).move_to([0, -2.8, 0])
        n66 = note("N−1 identical terms", 19, GREY_B).move_to([5.2, -2.0, 0])
        self.at(66, Write(cl), FadeIn(n66), run_time=1.8)
        self.finish()


# ==========================================================================
# K3_1 -- the BBGKY hierarchy   (cues 67-74)
# ==========================================================================
def hier_box(k, width=6.0):
    nxt = k + 1
    t = MathTex(r"\pdv{f_%d}{t}=\pb{H_%d}{f_%d}+\int\nabla U\cdot\nabla_{\va p}\," % (k, k, k),
                r"f_%d" % nxt, font_size=28)
    t[1].set_color(ORANGE)
    fit_box(t, width - 0.5)
    box = SurroundingRectangle(t, buff=0.2, color=BLUE, corner_radius=0.1)
    return VGroup(box, t)


class K3_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K3_1"], END["K3_1"]

    def construct(self):
        header = hdr("The BBGKY Hierarchy")
        self.add(header)
        # cue 67: we have not solved anything, just traded one problem for another
        tr_ = Text("we have not solved anything:\nwe traded one problem for another", font_size=34,
                   color=YELLOW, line_spacing=0.95).move_to([0, 0.3, 0])
        self.at(67, FadeIn(tr_, shift=UP * 0.2), run_time=0.8)
        # cue 68: the equation for f_1 depends on f_2
        B1, B2, B3 = hier_box(1), hier_box(2), hier_box(3)
        X = -3.3
        B1.move_to([X, 1.55, 0]); B2.move_to([X, 0.25, 0]); B3.move_to([X, -1.05, 0])
        a12 = Arrow(B1.get_bottom(), B2.get_top(), buff=0.05, color=ORANGE)
        a23 = Arrow(B2.get_bottom(), B3.get_top(), buff=0.05, color=ORANGE)
        self.at(68, FadeOut(tr_), FadeIn(B1, shift=RIGHT * 0.2), run_time=0.8)
        # cue 69: the same steps for f_2 bring in f_3, and so on
        self.at(69, GrowArrow(a12), FadeIn(B2, shift=RIGHT * 0.2), run_time=0.7)
        self.play(GrowArrow(a23), FadeIn(B3, shift=RIGHT * 0.2), run_time=0.7)
        vd = tex(r"\vdots", 40).move_to([X, -2.05, 0])
        fN = tex(r"f_N:\ \text{Liouville}", 30, BLUE).move_to([X, -2.85, 0])
        self.play(FadeIn(vd), FadeIn(fN), run_time=0.6)
        gen = MathTex(r"\pdv{f_s}{t}&=\pb{H_s}{f_s}\\"
                      r"&\;+\sum_{i=1}^{s}\!\int\! d^3r_{s+1}d^3p_{s+1}\,\nabla_{\va r_i}U(\va r_i-\va r_{s+1})"
                      r"\cdot\nabla_{\va p_i}f_{s+1}", font_size=28)
        fit_box(gen, 6.6).move_to([3.5, 1.3, 0])
        gl = note("for any s", 18, GREY_B).next_to(gen, UP, buff=0.1)
        self.frac(69, 0.55, Write(gen), FadeIn(gl), run_time=1.6)
        # cue 70: H_s is the Hamiltonian of the first s particles alone; f_s defined by integrating out the rest
        fs = tex(r"f_s\equiv\frac{N!}{(N-s)!}\int\prod_{i>s}d^3r_i\,d^3p_i\,f", 30, YELLOW)
        fit_box(fs, 6.4).move_to([3.5, -0.3, 0])
        hs = note("H_s: the Hamiltonian of the first s particles alone", 19, GREY_B).move_to([3.5, -1.15, 0])
        self.at(70, Write(fs), FadeIn(hs), run_time=1.4)
        # cue 71: named after Bogoliubov, Born, Green, Kirkwood, Yvon
        nm = chip("BBGKY:  Bogoliubov · Born · Green · Kirkwood · Yvon", YELLOW, 19, fill=0.1)
        fit_box(nm, 7.0).move_to([3.5, -2.0, 0])
        self.at(71, FadeIn(nm, shift=UP * 0.15), run_time=0.7)
        # cue 72-73: completely equivalent to Liouville: exact and reversible; we only reorganised
        eqv = note("≡ Liouville's equation:  still exact, still reversible", 20, GREEN).move_to([3.5, -2.85, 0])
        self.at(72, FadeIn(eqv, shift=UP * 0.1), run_time=0.6)
        # cue 74: to get anywhere we must close it: express f_2 in terms of f_1
        clo = tex(r"\text{close it:}\quad f_2\ \longrightarrow\ f_2[f_1]", 34, ORANGE)
        clo.move_to([0, -3.0, 0])
        self.at(74, FadeOut(eqv), FadeOut(nm), run_time=0.4)
        self.play(Write(clo.move_to([3.5, -2.3, 0])), Indicate(B1[1][1], color=ORANGE, scale_factor=1.6),
                  run_time=1.2)
        self.snap("hierarchy")
        self.finish()


# ==========================================================================
# K3_2 -- closures: Vlasov vs Boltzmann   (cues 75-79)
# ==========================================================================
class K3_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K3_2"], END["K3_2"]

    def construct(self):
        header = hdr("The BBGKY Hierarchy")
        self.add(header)
        top = card("BBGKY hierarchy\n(not closed)", BLUE, 22, 3.6).move_to([0.6, 1.9, 0])
        XL, XR, YP = -3.5, 3.9, 0.2
        # cue 75: it is worth pausing to see how much the closure matters
        self.at(75, FadeIn(top, shift=DOWN * 0.2), run_time=0.7)

        # cue 76: weak, long-range interactions: every particle feels many others at once
        fl = box_frame([XL, YP, 0], 5.6, 2.0)
        rng = np.random.default_rng(3)
        pts = [np.array([XL + rng.uniform(-2.5, 2.5), YP + rng.uniform(-0.8, 0.8), 0]) for _ in range(13)]
        web = VGroup(*[Line(pts[a], pts[b], stroke_width=1.2, color=GREEN, stroke_opacity=0.28)
                       for a in range(len(pts)) for b in range(a + 1, len(pts))])
        wd = VGroup(*[Dot(p, radius=0.06, color=GREEN) for p in pts])
        cap_l = note("weak, long-range: every particle feels many at once", 18, GREEN).move_to([XL, -1.25, 0])
        a_l = Arrow(top.get_bottom() + LEFT * 0.6, fl.get_top(), buff=0.1, color=GREEN)
        self.at(76, GrowArrow(a_l), FadeIn(fl), FadeIn(web), FadeIn(wd), FadeIn(cap_l), run_time=1.2)

        # cue 77: distinct particles uncorrelated -> Vlasov equation, mean field
        clos = tex(r"f_2\approx f_1 f_1\ \Rightarrow\ \pdv{f_1}{t}=\pb{H_1+\Phi}{f_1}", 28, GREEN)
        fit_box(clos, 6.0).move_to([XL, -2.6, 0])
        vl = chip("Vlasov equation: mean field", GREEN, 20).move_to([XL, -1.95, 0])
        self.at(77, Write(clos), FadeIn(vl), run_time=1.6)
        used = note("collisionless plasmas · stellar dynamics", 18, GREY_B).move_to([XL, -3.15, 0])
        self.frac(77, 0.55, FadeIn(used), run_time=0.5)

        # cue 78: still reversible; the same hierarchy with a different closure for the opposite regime
        rv = note("still reversible ✓", 21, GREEN).move_to([XL, -3.15, 0])
        self.at(78, FadeOut(used), FadeIn(rv), run_time=0.5)
        fr = box_frame([XR, YP, 0], 5.6, 2.0)
        sp = [np.array([XR + x, YP + y, 0]) for x, y in
              [(-2.2, 0.6), (-1.0, -0.55), (0.2, 0.7), (1.3, -0.45), (2.2, 0.5), (-0.4, -0.2), (-2.0, -0.6)]]
        sd = VGroup(*[Dot(p, radius=0.07, color=ORANGE) for p in sp])
        rings = VGroup(*[Circle(radius=0.2, color=ORANGE, stroke_width=1.5, stroke_opacity=0.6).move_to(p) for p in sp])
        hit = Line(sp[1], sp[5], color=RED, stroke_width=4)
        flash = Circle(radius=0.28, color=RED, stroke_width=3).move_to((sp[1] + sp[5]) / 2)
        a_r = Arrow(top.get_bottom() + RIGHT * 0.6, fr.get_top(), buff=0.1, color=ORANGE)
        self.play(GrowArrow(a_r), FadeIn(fr), FadeIn(sd), FadeIn(rings), run_time=1.0)
        # cue 79: short-range, strong interactions in a dilute gas -- and that closure is not reversible
        cap_r = note("strong, short-range, dilute: binary collisions", 18, ORANGE).move_to([XR, -1.25, 0])
        bz = chip("Boltzmann equation", ORANGE, 20).move_to([XR, -1.95, 0])
        self.at(79, FadeIn(cap_r), FadeIn(bz), Create(hit), FadeIn(flash), run_time=1.0)
        nr = note("not reversible ✗", 21, RED).move_to([XR, -2.6, 0])
        self.play(FadeIn(nr, shift=UP * 0.1), run_time=0.5)
        self.snap("closures")
        self.finish()


# ==========================================================================
# K4 -- the collision rate omega and its symmetries   (cues 80-96)
# ==========================================================================
def pt_on(poly, x):
    """y-value of a polyline [(x, y), ...] (increasing x) at abscissa x."""
    for (x0, y0), (x1, y1) in zip(poly[:-1], poly[1:]):
        if x0 <= x <= x1:
            return y0 + (y1 - y0) * (x - x0) / (x1 - x0)
    return poly[-1][1]


def polyline(pts, color, sw=4):
    m = VMobject(color=color, stroke_width=sw)
    m.set_points_as_corners([np.array([x, y, 0.0]) for x, y in pts])
    return m


class K4(SyncedScene):
    AUDIO_START, AUDIO_END = START["K4"], END["K4"]

    def construct(self):
        header = hdr("Motivating the Boltzmann Equation")
        # cue 80: we have the structure: streaming + a collision term
        E0 = MathTex(r"\pdv{f_1}{t}=\pb{H_1}{f_1}+", r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}", font_size=36)
        E0[1].set_color(ORANGE)
        E0.move_to([-1.9, 1.85, 0])
        self.at(80, Write(header), Write(E0), run_time=1.4)
        # cue 81: we need the collision integral in terms of f_1
        q = tex(r"=\ ?\ [f_1]", 36, ORANGE).next_to(E0, RIGHT, buff=0.2)
        self.at(81, Write(q), run_time=0.8)

        # cue 82-83: it should be built from the rate at which collisions occur
        E1 = MathTex(r"\text{Rate}=", r"\omega(\va p',\va p_2'\,|\,\va p,\va p_2)",
                     r"\,f_2(\va r,\va r,\va p,\va p_2)\,d^3p\,d^3p_2\,d^3p'\,d^3p_2'", font_size=30)
        fit_box(E1, 8.8).move_to([-2.2, 0.8, 0])
        E1[1].set_color(YELLOW)
        E1[2].set_color(BLUE_C)
        self.at(82, FadeOut(q), run_time=0.3)
        self.play(Write(E1), run_time=2.0)
        # cue 84: omega is the scattering function: a collision (p, p2) -> (p', p2')
        C = np.array([4.6, 0.35, 0.0])
        mk = lambda a, b, col: Arrow(C + a, C + b, buff=0, color=col, stroke_width=5,
                                     max_tip_length_to_length_ratio=0.2)
        i1, i2 = mk([-1.6, 1.0, 0], [-0.30, 0.19, 0], BLUE_C), mk([-1.6, -1.0, 0], [-0.30, -0.19, 0], BLUE_C)
        o1, o2 = mk([0.28, 0.22, 0], [1.6, 1.25, 0], ORANGE), mk([0.28, -0.22, 0], [1.6, -1.25, 0], ORANGE)
        labs = VGroup(tex(r"\va p", 28, BLUE_C).move_to(C + [-1.85, 1.15, 0]),
                      tex(r"\va p_2", 28, BLUE_C).move_to(C + [-1.9, -1.15, 0]),
                      tex(r"\va p'", 28, ORANGE).move_to(C + [1.95, 1.3, 0]),
                      tex(r"\va p_2'", 28, ORANGE).move_to(C + [2.0, -1.3, 0]))
        vert = Circle(radius=0.3, color=YELLOW, stroke_width=3).move_to(C)
        wl = tex(r"\omega", 30, YELLOW).next_to(vert, DOWN, buff=0.45).shift(RIGHT * 0.1)
        self.at(84, Create(i1), Create(i2), FadeIn(labs[:2]), run_time=1.0)
        self.play(Create(o1), Create(o2), FadeIn(labs[2:]), Create(vert), Write(wl), run_time=1.2)
        # cue 85-86: computable from the interatomic potential; related to the cross section
        n85 = note("ω: computed from the inter-atomic potential U(r)\n    (related to the differential cross-section; more later)", 19, YELLOW)
        n85.move_to([-2.9, -0.05, 0])
        self.at(85, FadeIn(n85, shift=UP * 0.1), run_time=0.6)
        # cue 87-88: omega comes multiplied by f_2: the chance two particles sit at (r,p), (r,p2)
        self.at(87, Indicate(E1[2], color=BLUE_C, scale_factor=1.1), run_time=1.2)
        n88 = note("f₂: probability that the two particles sit at (r, p) and (r, p₂)", 19, BLUE_C)
        n88.move_to([-2.9, -0.05, 0])
        self.at(88, FadeOut(n85), FadeIn(n88), run_time=0.5)

        # cue 89-90: momentum and energy must be conserved in a collision: an assumption
        k1 = chip("momentum conserved:  p + p₂ = p′ + p₂′", GREEN, 18)
        k2 = chip("kinetic energy conserved", GREEN, 18)
        kk = VGroup(k1, k2).arrange(RIGHT, buff=0.3).move_to([-2.5, -0.05, 0])
        fit_box(kk, 9.4)
        self.at(89, FadeOut(n88), FadeIn(kk, shift=UP * 0.1), run_time=0.6)
        n90 = note("(assumes no external force does work during a collision)", 17, GREY_B).next_to(kk, DOWN, buff=0.1)
        self.at(90, FadeIn(n90), run_time=0.4)

        # cue 91: the new collision integral: gain minus loss
        E2 = MathTex(r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}&=\int d^3p_2\,d^3p'\,d^3p_2'\\ &\quad\times\Big[",
                     r"\omega(\va p,\va p_2|\va p',\va p_2')\,f_2(\va p',\va p_2')",
                     r"-\omega(\va p',\va p_2'|\va p,\va p_2)\,f_2(\va p,\va p_2)",
                     r"\Big]", font_size=28)
        E2[1].set_color(GREEN)
        E2[2].set_color(RED)
        fit_box(E2, 9.0).move_to([-2.3, 1.1, 0])
        self.at(91, FadeOut(E0), FadeOut(kk), FadeOut(n90), FadeOut(E1), run_time=0.4)
        self.play(Write(E2), run_time=2.0)
        gl = note("gain: scattered in", 18, GREEN).next_to(E2[1], DOWN, buff=0.12)
        ll = note("loss: scattered out", 18, RED).next_to(E2[2], DOWN, buff=0.12)
        self.play(FadeIn(gl), FadeIn(ll), run_time=0.4)

        # cue 92-94: omega obeys conservation laws; time reversal invariance -> in/out symmetric
        c92 = chip("conserves momentum and energy", GREEN, 19).move_to([-4.3, -0.75, 0])
        c93 = chip("time-reversal invariant", BLUE, 19).move_to([0.6, -0.75, 0])
        self.at(92, FadeIn(c92, shift=UP * 0.1), run_time=0.5)
        self.at(93, FadeIn(c93, shift=UP * 0.1), run_time=0.5)
        sym = tex(r"\omega(\va p',\va p_2'\,|\,\va p,\va p_2)=\omega(\va p,\va p_2\,|\,\va p',\va p_2')", 32, YELLOW)
        fit_box(sym, 9.0).move_to([-2.3, -1.65, 0])
        self.at(94, Write(sym), run_time=1.6)
        # cue 95-96: these symmetries simplify the collision term
        E3 = MathTex(r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}=\int d^3p_2\,d^3p'\,d^3p_2'\;\omega(\va p',\va p_2'|\va p,\va p_2)"
                     r"\Big[f_2(\va p',\va p_2')-f_2(\va p,\va p_2)\Big]", font_size=28)
        fit_box(E3, 9.2).move_to([-2.3, -2.75, 0])
        e3b = SurroundingRectangle(E3, color=YELLOW, buff=0.15)
        self.at(95, FadeOut(gl), FadeOut(ll), run_time=0.3)
        self.at(96, Write(E3), run_time=1.6)
        self.play(Create(e3b), run_time=0.5)
        self.snap("collision_integral")
        self.finish()


# ==========================================================================
# K4_1 -- molecular chaos and the arrow of time   (cues 97-111)
# ==========================================================================
class K4_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K4_1"], END["K4_1"]

    def construct(self):
        header = hdr("Motivating the Boltzmann Equation")
        self.add(header)
        # cue 97: finish by writing the collision integral in terms of f_1 rather than f_2
        goal = tex(r"f_2\ \longrightarrow\ f_1", 42, ORANGE).move_to([0, 0.6, 0])
        self.at(97, FadeIn(goal, scale=0.9), run_time=0.7)
        # cue 98: assume the two particles' velocities are uncorrelated
        mc = tex(r"f_2(\va r,\va r,\va p,\va p_2)=f_1(\va r,\va p)\,f_1(\va r,\va p_2)", 42, YELLOW)
        fit_box(mc, 10.5).move_to([-1.2, 1.85, 0])
        self.at(98, FadeOut(goal), run_time=0.3)
        self.play(Write(mc), run_time=2.0)
        # cue 99-100: molecular chaos: it seems simple, but isn't
        chaos = chip("molecular chaos", RED, 26, fill=0.1).move_to([5.0, 1.85, 0])
        self.at(99, FadeIn(chaos, shift=LEFT * 0.2), run_time=0.6)
        self.at(100, Indicate(chaos, color=RED, scale_factor=1.2), run_time=0.8)
        # cue 101: the collision rate is taken proportional to f_2
        n101 = note("collision rate ∝ f₂", 24, BLUE_C).move_to([-4.0, 0.55, 0])
        self.at(101, FadeIn(n101), run_time=0.4)
        # cue 102: ... so we are assuming velocities are uncorrelated BEFORE the collision
        n102 = Text("so: uncorrelated  BEFORE  a collision", font_size=26, color=ORANGE).move_to([1.5, 0.55, 0])
        self.at(102, FadeIn(n102, shift=RIGHT * 0.2), run_time=0.5)

        # cue 103-108: why this is reasonable -- and where the arrow of time sneaks in
        V1, V2 = (-3.0, -0.6), (2.8, -1.0)
        p1 = [(-5.8, 0.15), V1, V2, (5.8, 0.05)]
        p2 = [(-5.8, -1.7), V1, (5.8, -2.3)]
        p3 = [(-0.8, -2.45), V2, (5.8, -1.45)]
        P1, P2, P3 = polyline(p1, YELLOW), polyline(p2, BLUE_C), polyline(p3, GREEN)
        tax = Arrow([-6.2, -3.05, 0], [6.2, -3.05, 0], buff=0, color=GREY_B, stroke_width=2)
        tl = tex(r"t", 26, GREY_B).move_to([6.35, -3.05, 0])
        self.at(103, FadeOut(n101), Create(P1), Create(P2), FadeIn(tax), FadeIn(tl), run_time=1.6)
        # cue 104: during the collision the two velocities become correlated
        col1 = Circle(radius=0.16, color=RED, stroke_width=3).move_to([V1[0], V1[1], 0])
        xl = -1.9
        link1 = DashedLine([xl, pt_on(p1, xl), 0], [xl, pt_on(p2, xl), 0], color=RED, stroke_width=4)
        l1 = note("correlated by the collision", 18, RED).next_to(link1, UP, buff=0.4).set_stroke(BG, width=5, background=True)
        self.at(104, FadeIn(col1, scale=2), Create(link1), FadeIn(l1), run_time=0.9)
        # cue 105-106: ... but then a long time tau passes before the next collision
        tau = DoubleArrow([V1[0], -2.8, 0], [V2[0], -2.8, 0], buff=0, color=YELLOW, stroke_width=3,
                          max_tip_length_to_length_ratio=0.03)
        taul = tex(r"\tau", 30, YELLOW).next_to(tau, UP, buff=0.05)
        self.at(105, GrowFromCenter(tau), Write(taul), run_time=0.9)
        # cue 107: the next collision is with a completely different particle
        col2 = Circle(radius=0.16, color=RED, stroke_width=3).move_to([V2[0], V2[1], 0])
        xr = 1.3
        link2 = DashedLine([xr, pt_on(p1, xr), 0], [xr, pt_on(p3, xr), 0], color=GREY_B, stroke_width=3)
        l2 = note("no connection", 18, GREY_B).next_to(link2, UP, buff=0.4).set_stroke(BG, width=5, background=True)
        self.at(107, Create(P3), FadeIn(col2, scale=2), run_time=1.2)
        self.frac(107, 0.55, Create(link2), FadeIn(l2), run_time=0.7)
        # cue 108: ... assuming uncorrelated BEFORE (not after) quietly creates an arrow of time
        verdict = Text("uncorrelated before, correlated after:  an arrow of time", font_size=26, color=RED)
        verdict.move_to([0, 0.85, 0])
        self.at(108, FadeOut(n102), FadeIn(verdict, shift=UP * 0.15), run_time=0.8)
        self.snap("arrow")

        # cue 109: the Boltzmann equation, and its cross-section form
        keep = [header]
        self.at(109, *[FadeOut(m) for m in self.mobjects
                       if m not in self._persist and m not in keep], run_time=0.6)
        B = MathTex(r"\pdv{f_1}{t}=\pb{H_1}{f_1}+\Big(\pdv{f_1}{t}\Big)_{\rm coll}", font_size=40)
        B.move_to([0, 1.8, 0])
        Cf = MathTex(r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}=\int d^3p_2\,d^3p'\,d^3p_2'\;\omega(\va p',\va p_2'|\va p,\va p_2)"
                     r"\Big[f_1(\va r,\va p')f_1(\va r,\va p_2')-f_1(\va r,\va p)f_1(\va r,\va p_2)\Big]", font_size=28)
        fit_box(Cf, 12.0).move_to([0, 0.5, 0])
        Cs = MathTex(r"\Big(\pdv{f_1}{t}\Big)_{\rm coll}=\int d^3p_2\,d\Omega\;\frac{|\va p-\va p_2|}{m}\,\frac{d\sigma}{d\Omega}"
                     r"\Big[f_1(\va r,\va p')f_1(\va r,\va p_2')-f_1(\va r,\va p)f_1(\va r,\va p_2)\Big]", font_size=28)
        fit_box(Cs, 12.0).move_to([0, -1.0, 0])
        bb = SurroundingRectangle(B, color=YELLOW, buff=0.2)
        self.play(Write(B), Create(bb), run_time=1.4)
        self.frac(109, 0.3, Write(Cf), run_time=2.2)
        self.frac(109, 0.62, Write(Cs), run_time=2.2)
        sl = note("cross-section form of ω", 18, GREY_B).next_to(Cs, DOWN, buff=0.12)
        self.play(FadeIn(sl), run_time=0.4)
        # cue 110-111: many (more rigorous) derivations exist; we move on
        mv = note("many derivations exist, most far more rigorous than this one: we move on", 22, YELLOW)
        mv.move_to([0, -2.6, 0])
        self.at(110, FadeIn(mv, shift=UP * 0.1), run_time=0.7)
        self.finish(keep=[header])


# ==========================================================================
# K4_2 -- velocity variables   (cues 112-116)
# ==========================================================================
class K4_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K4_2"], END["K4_2"]

    def construct(self):
        header = hdr("Velocity Variables", kind="subsubsection*")
        self.at(112, Write(header), run_time=0.7)
        # cue 112: fluid mechanics is more naturally written with velocity than momentum
        sw = tex(r"\va p\ \to\ m\va v,\qquad f(\va r,\va v,t)\equiv m^3 f_1(\va r,m\va v,t),\qquad \va a=\va F/m", 32)
        fit_box(sw, 12.0).move_to([0, 1.7, 0])
        self.sync(CUES[112].start + 1.5)
        self.play(Write(sw), run_time=2.0)
        # cue 113: Newton's second law, velocity instead of momentum -> the Boltzmann equation
        BE = MathTex(r"\pdv{f}{t}+\va v\cdot\nabla f+\va a\cdot\nabla_{\va v}f=C[f]", font_size=50)
        BE.move_to([0, 0.35, 0])
        bx = SurroundingRectangle(BE, color=YELLOW, buff=0.25)
        Cc = MathTex(r"C[f]=\int d^3v_2\,d\Omega\,|\va v-\va v_2|\,\frac{d\sigma}{d\Omega}"
                     r"\Big[f(\va v')f(\va v_2')-f(\va v)f(\va v_2)\Big]", font_size=32)
        fit_box(Cc, 11.5).move_to([0, -1.05, 0])
        self.at(113, Write(BE), run_time=1.6)
        self.play(Create(bx), FadeIn(Cc, shift=UP * 0.1), run_time=1.0)
        # cue 114: equivalently, with the symmetric rate W
        Cw = MathTex(r"C[f]=\int d^3v_2\,d^3v'\,d^3v_2'\;W\,\big[f'f_2'-ff_2\big],\qquad "
                     r"W(\va v',\va v_2'|\va v,\va v_2)=W(\va v,\va v_2|\va v',\va v_2')", font_size=32)
        fit_box(Cw, 12.2).move_to([0, -2.3, 0])
        self.at(114, Write(Cw), run_time=2.4)
        # cue 115: shorthand f' = f(v'), f_2 = f(v_2), ...
        sh = tex(r"f'\equiv f(\va v'),\quad f_2\equiv f(\va v_2),\quad f_2'\equiv f(\va v_2')", 26, GREY_B)
        sh.move_to([0, -3.1, 0])
        self.at(115, FadeIn(sh, shift=UP * 0.1), run_time=0.6)
        # cue 116: this is the form we use next
        wb = SurroundingRectangle(Cw, color=ORANGE, buff=0.15)
        self.at(116, Create(wb), run_time=0.6)
        self.snap("velocity")
        self.finish()


# ==========================================================================
# K5 -- the H-theorem: derivation   (cues 117-130)
# ==========================================================================
DGAMMA = r"d\Gamma"


class K5(SyncedScene):
    AUDIO_START, AUDIO_END = START["K5"], END["K5"]

    def construct(self):
        header = hdr("H-Theorem")
        # cue 117: the payoff of the molecular-chaos assumption
        self.at(117, Write(header), run_time=0.6)
        pay = Text("the payoff of molecular chaos", font_size=40, color=YELLOW).move_to([0, 0.3, 0])
        self.sync(CUES[117].start + 0.8)
        self.play(FadeIn(pay, scale=0.95), run_time=0.7)
        # cue 118: Boltzmann's H functional; -k_B H is the entropy; claim dH/dt <= 0
        Hd = tex(r"H[f](t)=\int d^3r\,d^3v\;f\ln f", 46, YELLOW).move_to([-1.2, 1.95, 0])
        warn = note("not the Hamiltonian!", 20, RED).next_to(Hd, RIGHT, buff=0.5)
        self.at(118, FadeOut(pay), Write(Hd), run_time=1.4)
        self.play(FadeIn(warn), run_time=0.4)
        claim = tex(r"S=-k_B H\,,\qquad \dv{H}{t}\le 0", 48).move_to([0, 0.75, 0])
        cb = SurroundingRectangle(claim, color=ORANGE, buff=0.22)
        self.frac(118, 0.45, Write(claim), run_time=1.4)
        self.play(Create(cb), run_time=0.5)

        # cue 119: differentiate under the integral
        s1 = tex(r"\dv{H}{t}=\int d^3r\,d^3v\;(1+\ln f)\,\pdv{f}{t}", 36).move_to([0, -0.65, 0])
        self.at(119, Write(s1), run_time=1.6)
        # cue 120: substitute the Boltzmann equation
        eB = MathTex(r"\pdv{f}{t}=", r"-\va v\cdot\nabla f-\va a\cdot\nabla_{\va v}f", r"+\,C[f]", font_size=34)
        eB[1].set_color(RED)
        eB[2].set_color(YELLOW)
        eB.move_to([0, -1.75, 0])
        self.at(120, Write(eB), run_time=1.6)
        # cue 121: streaming and force terms are total derivatives -> boundary terms vanish
        n121 = note("streaming and force terms are total derivatives, v·∇(f ln f) and a·∇ᵥ(f ln f):\n"
                    "they integrate to boundary terms, which vanish for a box with reflecting or periodic walls",
                    18, RED).move_to([0, -2.75, 0])
        self.at(121, FadeIn(n121, shift=UP * 0.1), run_time=0.6)
        # cue 122: only the collision integral survives
        r1 = tex(r"\dv{H}{t}=\int d^3r\,d^3v\;(1+\ln f)\,C[f]", 36).move_to([-1.0, -1.75, 0])
        self.at(122, FadeOut(eB), FadeOut(n121), run_time=0.4)
        self.play(Write(r1), run_time=1.2)
        # cue 123: collisions do not create particles: the "1" drops out
        n123 = note("collisions conserve particle number:\n∫ C[f] d³v = 0, so the “1” drops", 18, GREEN)
        n123.move_to([4.6, -1.75, 0])
        r2 = tex(r"\dv{H}{t}=\int d^3r\,d^3v\;\ln f\;C[f]=\int d\Gamma\;W\,\big(f'f_2'-ff_2\big)\ln f", 34, YELLOW)
        fit_box(r2, 12.0).move_to([0, -2.75, 0])
        dg = tex(r"d\Gamma\equiv d^3r\,d^3v\,d^3v_2\,d^3v'\,d^3v_2'", 22, GREY_B).move_to([0, -3.25, 0])
        self.at(123, FadeIn(n123), Write(r2), FadeIn(dg), run_time=1.8)
        self.snap("collision_only")

        # cue 124: dummy labels: we are free to relabel the integration variables
        wipe = [m for m in self.mobjects if m not in self._persist and m is not header]
        ea = at_left(fit_box(tex(r"\dv{H}{t}=\int d\Gamma\;W\,\big(f'f_2'-ff_2\big)\ln f", 34), 8.8), 1.75)
        n124 = note("the integration variables are dummy labels:\nrelabel them as we please", 18, GREY_B).move_to([4.7, 1.75, 0])
        self.at(124, *[FadeOut(m) for m in wipe], run_time=0.5)
        self.play(Write(ea), FadeIn(n124), run_time=1.2)
        # cue 125-126: swap v <-> v2 and v' <-> v2' together; W is unchanged for identical particles
        n125 = note("swap v ↔ v₂ and v′ ↔ v₂′ together:\nW unchanged (identical particles)\n→ same integral with ln f₂", 17, ORANGE)
        n125.move_to([4.7, 1.75, 0])
        self.at(125, FadeOut(n124), FadeIn(n125), run_time=0.5)
        # cue 127: average the two forms
        eb = at_left(fit_box(tex(r"\dv{H}{t}=\frac12\int d\Gamma\;W\,\big(f'f_2'-ff_2\big)\ln(ff_2)", 34), 8.8), 0.45)
        n127 = note("average the two:", 18, GREY_B).move_to([4.7, 0.45, 0])
        self.at(127, Write(eb), FadeIn(n127), run_time=1.6)
        # cue 128: now swap primed and unprimed variables
        n128 = note("swap primed ↔ unprimed:\nW unchanged (time reversal),\nbracket flips sign", 18, ORANGE)
        n128.move_to([4.7, -0.85, 0])
        self.at(128, FadeIn(n128), run_time=0.5)
        ec = at_left(fit_box(tex(r"\dv{H}{t}=-\frac12\int d\Gamma\;W\,\big(f'f_2'-ff_2\big)\ln(f'f_2')", 34), 8.8), -0.85)
        self.frac(129, 0.1, Write(ec), run_time=1.6)
        # cue 129: average the last two forms
        ed = at_left(fit_box(tex(r"\dv{H}{t}=-\frac14\int d\Gamma\;W\,\big(f'f_2'-ff_2\big)\ln\frac{f'f_2'}{ff_2}\ \le 0", 34, YELLOW), 9.0), -2.3)
        n129 = note("average the last two forms:", 18, GREY_B).move_to([4.7, -2.3, 0])
        self.frac(129, 0.55, Write(ed), FadeIn(n129), run_time=2.0)
        self.snap("symmetrised")

        # cue 130: W >= 0 and (a - b) ln(a/b) >= 0: this is the H-theorem
        ax = Axes(x_range=[0.02, 3.0, 1], y_range=[0, 2.2, 1], x_length=3.4, y_length=1.9,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([5.0, -0.2, 0])
        gcurve = ax.plot(lambda x: (x - 1) * math.log(x), x_range=[0.05, 3.0], color=GREEN)
        gl = tex(r"(a-b)\ln\frac{a}{b}\ \ge 0", 24, GREEN).next_to(ax, UP, buff=0.12)
        gx = tex(r"a/b", 20, GREY_B).next_to(ax, DOWN, buff=0.05)
        self.at(130, FadeOut(n125), FadeOut(n127), FadeOut(n128), FadeOut(n129), FadeOut(ea), FadeOut(eb),
                FadeOut(ec), run_time=0.5)
        self.play(ed.animate.move_to([-1.9, 0.4, 0]), run_time=0.6)
        eb_box = SurroundingRectangle(ed, color=ORANGE, buff=0.15)
        self.play(Create(eb_box), Create(ax), Create(gcurve), Write(gl), FadeIn(gx), run_time=1.4)
        thm = Text("H-theorem", font_size=44, color=ORANGE).move_to([-1.9, -1.2, 0])
        why = note("W ≥ 0  and  (a − b) ln(a/b) ≥ 0   ⇒   H never increases", 22, YELLOW).move_to([-1.9, -2.1, 0])
        self.play(FadeIn(thm, scale=0.9), FadeIn(why), run_time=0.8)
        self.snap("htheorem")
        self.finish(keep=[header])


# ==========================================================================
# K5_1 -- when does H stop decreasing?  Maxwell-Boltzmann   (cues 131-138)
# ==========================================================================
def _gauss(v, mu, s):
    return math.exp(-0.5 * ((v - mu) / s) ** 2) / (s * math.sqrt(TAU))


def f_mix(v, s):
    """(1-s) * bimodal + s * Maxwellian, all with n = 1, u = 0, <v^2> = 1."""
    bi = 0.5 * _gauss(v, -0.8, 0.6) + 0.5 * _gauss(v, 0.8, 0.6)
    return (1 - s) * bi + s * _gauss(v, 0.0, 1.0)


def H_of(s):
    v = np.linspace(-6, 6, 1201)
    f = np.array([f_mix(x, s) for x in v])
    g = np.where(f > 1e-300, f * np.log(np.maximum(f, 1e-300)), 0.0)
    integrate = getattr(np, "trapezoid", None) or np.trapz
    return float(integrate(g, v))


class K5_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K5_1"], END["K5_1"]

    def construct(self):
        header = hdr("H-Theorem")
        self.add(header)
        # cue 131: when does H stop decreasing?  (watch f relax, H fall)
        tr = ValueTracker(0.0)
        ax = Axes(x_range=[-4, 4, 2], y_range=[0, 0.7, 0.35], x_length=5.2, y_length=2.3,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([3.9, 0.7, 0])
        maxw = ax.plot(lambda v: _gauss(v, 0, 1), x_range=[-4, 4], color=GREEN, stroke_width=2)
        maxw = DashedVMobject(maxw, num_dashes=40)
        fcur = always_redraw(lambda: ax.plot(lambda v: f_mix(v, tr.get_value()), x_range=[-4, 4],
                                             color=YELLOW, stroke_width=4))
        fl = tex(r"f(v)", 26, YELLOW).next_to(ax, UP, buff=0.1).align_to(ax, LEFT)
        Hlab = tex(r"H=", 30).move_to([3.1, -1.25, 0])
        Hnum = always_redraw(lambda: DecimalNumber(H_of(tr.get_value()), num_decimal_places=3, font_size=32,
                                                   color=YELLOW).next_to(Hlab, RIGHT, buff=0.15))
        self.at(131, FadeIn(ax), FadeIn(maxw), FadeIn(fl), FadeIn(Hlab), run_time=0.8)
        self.add(fcur, Hnum)
        q = Text("When does H stop decreasing?", font_size=32, color=YELLOW).move_to([-3.0, 1.75, 0])
        # f relaxes over ~29 s of narration; an updater lets the equations below sync meanwhile
        tr.add_updater(lambda m, dt: m.set_value(min(1.0, m.get_value() + dt / 29.0)))
        self.play(FadeIn(q), run_time=0.6)

        # cue 132: when f'f2' = f f2 for every allowed collision
        # (the animation above runs long; the equations are placed by narration time below)
        e1 = at_left(tex(r"f'f_2'=ff_2\quad\text{for every allowed collision}", 32), 1.85)
        self.at(132, ReplacementTransform(q, e1), run_time=0.8)
        # cue 133: i.e. ln f is a collision invariant
        e2 = at_left(fit_box(tex(r"\ln f(\va v)+\ln f(\va v_2)=\ln f(\va v')+\ln f(\va v_2')", 30), 7.4), 0.95)
        c2 = chip("ln f is a collision invariant", ORANGE, 19).move_to([-3.0, 0.35, 0])
        self.at(133, Write(e2), FadeIn(c2), run_time=1.6)
        # cue 134: only combinations of the conserved quantities -> Maxwell-Boltzmann
        e3 = at_left(tex(r"\ln f=\alpha+\va\beta\cdot\va v+\gamma\,v^2", 34), -0.45)
        e4 = at_left(fit_box(tex(r"f_{\rm M}=n\Big(\frac{m}{2\pi k_BT}\Big)^{3/2}"
                                 r"\exp\Big[-\frac{m|\va v-\va u|^2}{2k_BT}\Big]", 30, GREEN), 7.2), -1.85)
        c3 = note("only conserved quantities (1, v, v²) are collision invariants", 17, GREY_B).move_to([-3.0, -0.95, 0])
        self.at(134, Write(e3), FadeIn(c3), run_time=1.4)
        self.frac(134, 0.5, Write(e4), run_time=1.8)
        # cue 135-136: with an external potential, the streaming term must vanish too
        e5 = at_left(fit_box(tex(r"f\propto e^{-\beta H_1}\ \Rightarrow\ n(\va r)\propto e^{-V(\va r)/k_BT}", 32, ORANGE), 7.4), -3.0)
        n5 = note("streaming term must vanish too", 18, GREY_B).move_to([-3.0, -2.5, 0])
        self.at(135, FadeIn(n5), run_time=0.4)
        self.at(136, Write(e5), run_time=1.6)
        # cue 137: the Boltzmann distribution we assumed at the start is now derived
        ch = chip("assumed for the N-body system at the start  →  derived for f₁", GREEN, 19, fill=0.1)
        fit_box(ch, 6.2).move_to([3.6, -2.6, 0])
        self.at(137, FadeIn(ch, shift=UP * 0.1), run_time=0.7)
        # cue 138: -k_B H plays the role of entropy: the H-theorem is the second law of a dilute gas
        sl = tex(r"S=-k_B H:\ \ \text{second law for a dilute gas}", 26, YELLOW).move_to([3.7, -3.2, 0])
        fit_box(sl, 6.6)
        self.at(138, Write(sl), run_time=1.2)
        self.snap("maxwell")
        self.finish(keep=[header])


# ==========================================================================
# K5_2 -- Loschmidt and Zermelo, hard-disk simulation   (cues 139-159)
# ==========================================================================
class K5_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["K5_2"], END["K5_2"]

    def construct(self):
        header = hdr("The Paradoxes", kind="subsubsection*")
        FPS = 30
        hist = gas_history(1.0, 2.3, FPS, 1e-4)
        ts, T_REV = hist["t"], hist["t_rev"]
        P_ex, H_ex = hist["exact"]
        P_nu, H_nu = hist["nudged"]
        nfr = len(ts)

        # ---- cue 139-141: reversible mechanics, irreversible H
        self.at(139, Write(header), run_time=0.7)
        par = Text("a bit of a paradox", font_size=44, color=YELLOW).move_to([0, 0.7, 0])
        self.sync(CUES[139].start + 0.9)
        self.play(FadeIn(par, scale=0.95), run_time=0.7)
        cA = chip("reversible mechanics + energy conservation", BLUE, 24)
        cB = chip("a quantity that only goes one way", ORANGE, 24)
        pair = VGroup(cA, cB).arrange(DOWN, buff=0.55).move_to([0, 0.3, 0])
        ar = Arrow(cA.get_bottom(), cB.get_top(), buff=0.08, color=GREY_B)
        self.at(141, FadeOut(par), FadeIn(cA, shift=RIGHT * 0.2), run_time=0.6)
        self.play(GrowArrow(ar), FadeIn(cB, shift=RIGHT * 0.2), run_time=0.7)

        # ---- build the simulation panel (left) and H(t) plot (right)
        S = 4.3
        origin = np.array([-3.8, -0.15, 0.0])
        frame = Square(side_length=S, color=GREY_B, stroke_width=2).move_to(origin)
        cols = [interpolate_color(BLUE_C, RED_C, float(np.clip(p[0] / 0.36, 0, 1))) for p in P_ex[0]]
        gas = VGroup(*[Dot(radius=0.045, color=c) for c in cols])
        tau, tau_n, mode = ValueTracker(0.0), ValueTracker(T_REV), ["ex"]

        def frame_at(P, t):
            x = min(max(t, 0.0), ts[-1]) * FPS
            k = min(int(math.floor(x)), nfr - 2)
            w = x - k
            return (1 - w) * P[k] + w * P[k + 1]

        def upd(m):
            P, t = (P_ex, tau.get_value()) if mode[0] == "ex" else (P_nu, tau_n.get_value())
            pos = frame_at(P, t)
            for d, p in zip(m.submobjects, pos):
                d.move_to(origin + np.array([(p[0] - 0.5) * S, (p[1] - 0.5) * S, 0.0]))
        upd(gas)
        gas.add_updater(upd)

        ax = Axes(x_range=[0, 2.3, 0.5], y_range=[0, 2.0, 1.0], x_length=5.6, y_length=3.0,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([3.6, 0.35, 0])
        xl = tex(r"t", 24, GREY_B).next_to(ax, DOWN, buff=0.08).align_to(ax, RIGHT)
        yl = tex(r"H", 26, GREY_B).next_to(ax, UP, buff=0.08).align_to(ax, LEFT)

        def make_curve(Hs, upto, color, k0=0):
            x = min(max(upto, 0.0), ts[-1]) * FPS
            k = min(int(math.floor(x)), nfr - 2)
            w = x - k
            pts = [ax.c2p(ts[i], Hs[i]) for i in range(k0, k + 1)]
            pts.append(ax.c2p(ts[k] + w / FPS, (1 - w) * Hs[k] + w * Hs[k + 1]))
            m = VMobject(color=color, stroke_width=4)
            if len(pts) >= 2:
                m.set_points_as_corners(pts)
            return m

        k_rev = int(round(T_REV * FPS))
        curve_ex = always_redraw(lambda: make_curve(H_ex, tau.get_value(), BLUE_C))
        curve_nu = always_redraw(lambda: make_curve(H_nu, tau_n.get_value(), ORANGE, k0=k_rev))
        def caption(txt, color=GREY_B, y=-3.0, size=22):
            return Text(txt, font_size=size, color=color, line_spacing=0.9).move_to([0, y, 0])

        # cue 142: many famous objections: start the gas expanding from a corner
        self.at(142, FadeOut(cA), FadeOut(cB), FadeOut(ar), FadeIn(frame), FadeIn(gas),
                FadeIn(ax), FadeIn(xl), FadeIn(yl), run_time=0.7)
        self.add(curve_ex, curve_nu)
        cap = caption("hard-disk gas released in a corner:  H(t) falls", BLUE_C)
        self.play(FadeIn(cap), tau.animate.set_value(0.55), run_time=3.9, rate_func=linear)
        # cue 143-144: Loschmidt: take the gas at some time and flip every velocity
        c143 = caption("Loschmidt's reversibility objection", YELLOW, size=26)
        self.at(143, FadeOut(cap), FadeIn(c143), tau.animate.set_value(T_REV), run_time=3.1, rate_func=linear)
        flip = tex(r"\va v\ \to\ -\va v", 64, YELLOW).move_to(origin)
        vline = DashedLine(ax.c2p(T_REV, 0), ax.c2p(T_REV, 2.0), color=YELLOW, stroke_width=2)
        self.at(144, FadeIn(flip, scale=1.3), Create(vline), run_time=0.5)
        self.play(FadeOut(flip), run_time=0.5)
        # cue 145-146: the dynamics carries it back to the low-entropy state: H must increase
        c145 = caption("run it on: the dynamics retraces its steps", BLUE_C)
        self.at(145, FadeOut(c143), FadeIn(c145), run_time=0.3)
        self.play(tau.animate.set_value(1.67), run_time=4.6, rate_func=linear)
        c146 = caption("H increases!", RED, size=30)
        self.at(146, FadeOut(c145), FadeIn(c146), run_time=0.3)
        self.play(tau.animate.set_value(2.0), run_time=2.6, rate_func=linear)
        self.snap("loschmidt")
        # cue 147-149: where does the proof break? nothing wrong with the proof, but with its input
        c147 = caption("where does the proof break?", YELLOW, size=28)
        self.at(147, FadeOut(c146), FadeIn(c147), run_time=0.3)
        c148 = caption("nothing in the proof is wrong, but its input is", ORANGE, size=26)
        self.at(148, FadeOut(c147), FadeIn(c148), run_time=0.3)
        c149 = caption("molecular chaos holds for the typical state a gas finds itself in", GREEN, size=24)
        self.at(149, FadeOut(c148), FadeIn(c149), run_time=0.3)
        # cue 150-151: the velocity-reversed state is exquisitely atypical; a tiny nudge ruins it
        c150 = caption("the velocity-reversed state is exquisitely atypical", RED, size=26)
        self.at(150, FadeOut(c149), FadeIn(c150), run_time=0.3)
        leg_ex = note("exact reversal", 17, BLUE_C).move_to([2.4, 2.1, 0])
        leg_nu = note("nudged by 10⁻⁴", 17, ORANGE).move_to([5.2, 2.1, 0])
        c151 = caption("every velocity is correlated with the particles it is about to meet:\n"
                       "nudge them by 10⁻⁴ and the collisions no longer undo the earlier ones", ORANGE, size=22)
        self.at(151, FadeOut(c150), FadeIn(c151), FadeIn(leg_ex), FadeIn(leg_nu), run_time=0.5)
        mode[0] = "nu"
        tau_n.set_value(T_REV)
        self.play(tau_n.animate.set_value(2.3), run_time=11.0, rate_func=linear)
        # cue 152: such states exist, but they are an incredibly small fraction of phase space
        c152 = caption("such states exist, but they are an incredibly small fraction of phase space", YELLOW, size=22)
        self.at(152, FadeOut(c151), FadeIn(c152), run_time=0.4)
        self.snap("nudged")

        # cue 153: Zermelo's recurrence objection (clear the simulation)
        self.at(153, *[FadeOut(m) for m in (frame, gas, ax, xl, yl, curve_ex, curve_nu, vline, leg_ex, leg_nu, c152)],
                run_time=0.6)
        gas.clear_updaters()
        curve_ex.clear_updaters()
        curve_nu.clear_updaters()
        zt = chip("Zermelo's recurrence objection", YELLOW, 28, fill=0.1).move_to([0, 1.9, 0])
        self.play(FadeIn(zt, shift=DOWN * 0.15), run_time=0.5)
        # cue 154: Poincare recurrence: a bounded Hamiltonian system returns arbitrarily close to its start
        C = np.array([-3.5, -0.5, 0.0])
        pf = phase_frame(C, 5.0, 3.2, "q", "p")
        orb = ParametricFunction(lambda t: C + np.array([1.9 * math.sin(3 * t + 0.4), 1.2 * math.sin(5.1 * t), 0.0]),
                                 t_range=[0, 19.5], color=TEAL, stroke_width=2.5)
        start = Dot(C + np.array([1.9 * math.sin(0.4), 0.0, 0.0]), radius=0.09, color=YELLOW)
        near = Circle(radius=0.22, color=YELLOW, stroke_width=3).move_to(start)
        pc = note("a bounded Hamiltonian system returns\narbitrarily close to its initial state", 19, TEAL).next_to(pf, DOWN, buff=0.1)
        self.at(154, FadeIn(pf), FadeIn(start), FadeIn(pc), run_time=0.6)
        self.play(Create(orb, rate_func=linear), run_time=5.0)
        self.play(Create(near), run_time=0.4)
        # cue 155: so H must eventually go back up
        up = tex(r"\Rightarrow\quad H\ \text{must eventually go back up}", 34, ORANGE).move_to([3.4, 0.9, 0])
        fit_box(up, 6.4)
        self.at(155, Write(up), run_time=1.4)
        # cue 156: true, but the recurrence time is e^{O(N)}
        rec = tex(r"T_{\rm rec}\sim e^{O(N)},\qquad N\sim10^{23}", 36, YELLOW).move_to([3.4, -0.3, 0])
        fit_box(rec, 6.4)
        gg = tex(r"T_{\rm rec}\ \gg\ t_{\rm universe}\approx 4\times10^{17}\ \mathrm{s}", 34, RED).move_to([3.4, -1.45, 0])
        fit_box(gg, 6.4)
        self.at(156, Write(rec), run_time=1.4)
        self.frac(156, 0.5, Write(gg), run_time=1.4)
        # cue 157: the H-theorem is a statement about the Boltzmann equation, not the N-body system
        self.at(157, *[FadeOut(m) for m in (pf, orb, start, near, pc, up, rec, gg, zt)], run_time=0.5)
        a = card("H-theorem", ORANGE, 30, 3.6)
        b = card("Boltzmann equation", TEAL, 30, 4.6)
        c = card("N-body system", BLUE, 30, 4.2)
        a.move_to([-4.0, 0.9, 0]); b.move_to([1.1, 0.9, 0]); c.move_to([1.1, -1.0, 0])
        a1 = Arrow(a.get_right(), b.get_left(), buff=0.1, color=ORANGE)
        a2 = Arrow(a.get_right() + DOWN * 0.3, c.get_left(), buff=0.1, color=RED)
        ok = note("a statement about this ✓", 20, ORANGE).next_to(b, RIGHT, buff=0.3)
        no = note("not about this ✗", 20, RED).next_to(c, RIGHT, buff=0.3)
        self.play(FadeIn(a), FadeIn(b), FadeIn(c), run_time=0.7)
        self.play(GrowArrow(a1), FadeIn(ok), run_time=0.6)
        self.play(GrowArrow(a2), FadeIn(no), run_time=0.6)
        # cue 158: valid in a statistical sense for almost all initial conditions that start chaotic
        st = note("valid in a statistical sense: for almost all initial conditions that start out chaotic", 22, GREEN)
        st.move_to([0, -2.4, 0])
        self.at(158, FadeIn(st, shift=UP * 0.15), run_time=0.7)
        # cue 159: mathematicians have since made this precise
        mp = note("(mathematicians have made this precise: more at the end)", 20, GREY_B).move_to([0, -3.0, 0])
        self.at(159, FadeIn(mp), run_time=0.5)
        self.finish(keep=[header])


# ==========================================================================
# K6 -- moments, collision invariants, conservation laws   (cues 160-169)
# ==========================================================================
class K6(SyncedScene):
    AUDIO_START, AUDIO_END = START["K6"], END["K6"]

    def construct(self):
        header = hdr("Conservation Laws from the Boltzmann Equation")
        # cue 160: Boltzmann has far too much information for everyday fluid mechanics
        self.at(160, Write(header), run_time=0.7)
        big = tex(r"f(\va r,\va v,t)", 64, YELLOW).move_to([-3.2, 0.5, 0])
        far = Text("far too much information\nfor everyday fluid mechanics", font_size=30, color=RED,
                   line_spacing=0.95).next_to(big, DOWN, buff=0.5)
        self.sync(CUES[160].start + 0.9)
        self.play(FadeIn(big, scale=0.9), FadeIn(far), run_time=0.9)
        # cue 161: a fluid needs only a few fields: the moments of f
        defs = VGroup(
            tex(r"n=\int d^3v\,f,\qquad \rho=mn", 28),
            tex(r"\va u=\frac1n\int d^3v\,\va v\,f", 28),
            tex(r"\tfrac32 nk_BT=\int d^3v\,\tfrac12 mc^2 f", 28)).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        defs.move_to([-3.4, 1.2, 0])
        ladder = VGroup(chip("0th moment", BLUE, 18), chip("1st moment", TEAL, 18), chip("2nd moment", GREEN, 18))
        for ch, d in zip(ladder, defs):
            ch.move_to([3.5, d.get_center()[1], 0])
        self.at(161, FadeOut(big), FadeOut(far), run_time=0.4)
        self.play(LaggedStart(*[FadeIn(d, shift=RIGHT * 0.2) for d in defs], lag_ratio=0.35),
                  LaggedStart(*[FadeIn(c) for c in ladder], lag_ratio=0.35), run_time=1.8)
        # cue 162: peculiar velocity c = v - u; pressure tensor, heat flux, ideal-gas law
        d2 = VGroup(
            tex(r"\va c=\va v-\va u", 28, YELLOW),
            tex(r"P_{ij}=m\int d^3v\,c_ic_j\,f", 28),
            tex(r"q_i=\int d^3v\,\tfrac12 mc^2\,c_i\,f", 28),
            tex(r"p=\tfrac13P_{ii}=nk_BT", 28, ORANGE)).arrange(DOWN, buff=0.22, aligned_edge=LEFT)
        d2.move_to([-3.4, -1.55, 0])
        lad2 = VGroup(chip("2nd moment: pressure tensor", GREEN, 17), chip("3rd moment: heat flux", PURPLE, 17))
        lad2[0].move_to([3.6, d2[1].get_center()[1], 0]); lad2[1].move_to([3.6, d2[2].get_center()[1], 0])
        self.at(162, FadeIn(d2[0]), run_time=0.5)
        self.play(LaggedStart(*[FadeIn(m, shift=RIGHT * 0.2) for m in d2[1:]], lag_ratio=0.3),
                  FadeIn(lad2), run_time=1.6)
        self.snap("moments")

        # cue 163: multiply Boltzmann by any psi(v) and integrate; symmetrise the collision term
        keep_hdr = header
        wipe = [m for m in self.mobjects if m not in self._persist and m is not keep_hdr]
        w1 = MathTex(r"\pdv{}{t}\!\int\! d^3v\,\psi f+\nabla\cdot\!\int\! d^3v\,\va v\,\psi f"
                     r"-\int\! d^3v\,f\,\va a\cdot\nabla_{\va v}\psi=\int\! d^3v\,\psi\,C[f]", font_size=30)
        fit_box(w1, 12.4).move_to([0, 1.8, 0])
        w2 = MathTex(r"\int d^3v\,\psi\,C[f]=\frac14\int d^3v\,d^3v_2\,d^3v'\,d^3v_2'\,W\,\big(f'f_2'-ff_2\big)"
                     r"\big(\psi+\psi_2-\psi'-\psi_2'\big)", font_size=30)
        fit_box(w2, 12.4).move_to([0, 0.35, 0])
        self.at(163, *[FadeOut(m) for m in wipe], run_time=0.4)
        self.play(Write(w1), run_time=2.4)
        self.frac(163, 0.5, Write(w2), run_time=2.6)
        # cue 164: vanishes whenever psi + psi_2 = psi' + psi_2' in every collision
        inv = tex(r"\psi+\psi_2=\psi'+\psi_2'\ \ \text{in every collision}\ \Longrightarrow\ \int\psi\,C[f]\,d^3v=0",
                  30, ORANGE)
        fit_box(inv, 12.0).move_to([0, -1.0, 0])
        self.at(164, Write(inv), run_time=2.0)
        # cue 165: the collision invariants: 1, v, v^2 -- mass, momentum, energy, nothing else
        ci = VGroup(chip("ψ = 1:  mass", BLUE, 22), chip("ψ = v:  momentum", TEAL, 22),
                    chip("ψ = v²:  energy", GREEN, 22)).arrange(RIGHT, buff=0.4).move_to([0, -2.2, 0])
        fit_box(ci, 12.0)
        self.at(165, LaggedStart(*[FadeIn(c, shift=UP * 0.15) for c in ci], lag_ratio=0.3), run_time=1.4)
        nn = note("and nothing else", 20, YELLOW).next_to(ci, DOWN, buff=0.15)
        self.frac(165, 0.6, FadeIn(nn), run_time=0.4)

        # cue 166: so the right-hand side drops out for exactly these: three conservation laws
        wipe2 = [m for m in self.mobjects if m not in self._persist and m is not header]
        c1 = tex(r"\pdv{\rho}{t}+\nabla\cdot(\rho\va u)=0\qquad(\psi=m)", 34)
        c2 = tex(r"\rho\,D_tu_i=-\partial_jP_{ij}+nF_i\qquad(\psi=mv_i)", 34)
        c3 = tex(r"\tfrac32 nk_B\,D_tT=-P_{ij}\,\partial_ju_i-\partial_iq_i\qquad(\psi=\tfrac12mv^2)", 34)
        cs = VGroup(c1, c2, c3).arrange(DOWN, buff=0.5, aligned_edge=LEFT).move_to([0, 0.5, 0])
        fit_box(cs, 12.2)
        self.at(166, *[FadeOut(m) for m in wipe2], run_time=0.4)
        self.play(LaggedStart(*[Write(c) for c in cs], lag_ratio=0.5), run_time=3.2)
        # cue 167: the material derivative
        dt = tex(r"D_t=\partial_t+\va u\cdot\nabla", 34, YELLOW).move_to([0, -1.55, 0])
        self.at(167, Write(dt), run_time=1.0)
        # cue 168-169: momentum equation: m ∫ v_i v_j f = rho u_i u_j + P_ij, since ∫ c f = 0, plus continuity
        box = SurroundingRectangle(c2, color=ORANGE, buff=0.12)
        step = tex(r"m\!\int\! d^3v\,v_iv_jf=\rho u_iu_j+P_{ij}\quad(\text{since }\textstyle\int d^3v\,\va c\,f=0)", 30, ORANGE)
        fit_box(step, 12.0).move_to([0, -2.4, 0])
        self.at(168, Create(box), Write(step), run_time=1.8)
        n169 = note("+ continuity turns  ∂ₜ(ρuᵢ) + ∂ⱼ(ρuᵢuⱼ)  into  ρ Dₜuᵢ;   the energy equation takes a few more lines of the same kind",
                    17, GREY_B)
        fit_box(n169, 12.0).move_to([0, -3.1, 0])
        self.at(169, FadeIn(n169), run_time=0.5)
        self.snap("conservation")
        self.finish()


# ==========================================================================
# K6_1 -- the moment hierarchy does not close   (cues 170-176)
# ==========================================================================
class K6_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K6_1"], END["K6_1"]

    def construct(self):
        header = hdr("Conservation Laws from the Boltzmann Equation")
        self.add(header)
        # cue 170-171: exact consequences of Boltzmann -- and of the underlying mechanics
        e1 = chip("exact consequences of the Boltzmann equation", BLUE, 24).move_to([0, 1.7, 0])
        e2 = chip("and of the underlying mechanics: only mass, momentum, energy conservation", GREEN, 22)
        fit_box(e2, 12.0).move_to([0, 0.8, 0])
        self.at(170, FadeIn(e1, shift=DOWN * 0.15), run_time=0.6)
        self.at(171, FadeIn(e2, shift=DOWN * 0.15), run_time=0.6)
        # cue 172: ... but they are not a closed system
        no = Text("but not a closed system", font_size=34, color=RED).move_to([0, -0.4, 0])
        self.at(172, FadeIn(no, scale=0.95), run_time=0.6)
        # cue 173: five equations, but unknowns depend on the whole f
        eqn = card("5 equations\n(ρ, u, T)", BLUE, 24, 3.4).move_to([-4.2, -2.0, 0])
        unk = card("unknowns: ρ, u, T  and  P_ij, q_i\nwhich depend on all of f", RED, 22, 5.8).move_to([2.6, -2.0, 0])
        ne = tex(r"\neq", 44).move_to([-0.6, -2.0, 0])
        self.at(173, FadeOut(e1), FadeOut(e2), no.animate.move_to([0, 1.6, 0]), FadeIn(eqn), FadeIn(unk),
                FadeIn(ne), run_time=0.9)
        # cue 174-175: the BBGKY problem again, in a new place: each moment's equation involves the next one up
        chain = VGroup(*[chip(s, c, 20) for s, c in [("n  (0th moment)", BLUE), ("u  (1st)", TEAL),
                                                       ("P_ij, T  (2nd)", GREEN), ("q_i  (3rd)", PURPLE),
                                                       ("4th moment ...", RED)]])
        chain.arrange(RIGHT, buff=0.55).move_to([0, 0.0, 0])
        fit_box(chain, 12.4)
        arrs = VGroup(*[Arrow(chain[k].get_right(), chain[k + 1].get_left(), buff=0.05, color=ORANGE,
                              stroke_width=3, max_tip_length_to_length_ratio=0.35) for k in range(4)])
        self.at(174, FadeOut(no), run_time=0.3)
        bb = note("the same problem as BBGKY, in a new place", 22, YELLOW).move_to([0, 1.6, 0])
        self.play(FadeIn(bb), run_time=0.4)
        self.at(175, LaggedStart(*[FadeIn(c) for c in chain], lag_ratio=0.2), run_time=1.6)
        self.play(LaggedStart(*[GrowArrow(a) for a in arrs], lag_ratio=0.25), run_time=1.2)
        nd = note("the equation for each moment involves the next one up", 20, ORANGE).next_to(chain, DOWN, buff=0.3)
        self.play(FadeIn(nd), run_time=0.4)
        # cue 176: to close it we must say what f looks like
        cl = Text("to close it: say what f looks like", font_size=30, color=YELLOW).move_to([0, -3.1, 0])
        self.at(176, FadeIn(cl, shift=UP * 0.15), run_time=0.7)
        self.snap("closure")
        self.finish()


# ==========================================================================
# K7 -- local equilibrium -> Euler   (cues 177-183)
# ==========================================================================
def local_maxwellians(center, w, h):
    """Three local Maxwellians (different n, u, T) at three places in the gas."""
    ax = Axes(x_range=[-4, 4, 2], y_range=[0, 0.75, 0.25], x_length=w, y_length=h,
              axis_config={"include_tip": False, "stroke_width": 2}).move_to(center)
    specs = [(0.30, -1.2, 0.6, TEAL), (0.5, 0.2, 1.0, YELLOW), (0.25, 1.4, 1.4, ORANGE)]
    curves = VGroup(*[ax.plot(lambda v, n=n, u=u, s=s: n * math.exp(-0.5 * ((v - u) / s) ** 2) / (s * math.sqrt(TAU)),
                              x_range=[-4, 4], color=c, stroke_width=4) for n, u, s, c in specs])
    return ax, curves


class K7(SyncedScene):
    AUDIO_START, AUDIO_END = START["K7"], END["K7"]

    def construct(self):
        header = hdr("Local Equilibrium and the Euler Equations")
        self.at(177, Write(header), run_time=0.7)
        # cue 177: the H-theorem says what f looks like after many collisions: a Maxwellian
        m1 = chip("H-theorem: after many collisions, f is a Maxwellian", GREEN, 24)
        fit_box(m1, 11.0).move_to([0, 1.6, 0])
        self.sync(CUES[177].start + 0.8)
        self.play(FadeIn(m1, shift=DOWN * 0.15), run_time=0.7)
        # cue 178-179: collisions fast compared with changes of the fluid: Kn -> 0, local Maxwellian
        self.at(178, FadeOut(m1), run_time=0.3)
        f0 = MathTex(r"f\approx f_0=n\Big(\frac{m}{2\pi k_BT}\Big)^{3/2}\exp\Big[-\frac{m|\va v-\va u|^2}{2k_BT}\Big]",
                     r",\quad n,\va u,T\ \text{functions of}\ (\va r,t)", font_size=32)
        f0[0].set_color(YELLOW)
        fit_box(f0, 12.4).move_to([0, 1.85, 0])
        kn = tex(r"\mathrm{Kn}\to0", 40, ORANGE).move_to([-6.0, 0.75, 0])
        self.sync(CUES[178].start + 2.0)
        self.play(Write(kn), run_time=0.8)
        ax, curves = local_maxwellians([3.6, 0.2, 0], 5.6, 2.4)
        lab = note("local n, u, T at different places", 18, GREY_B).next_to(ax, DOWN, buff=0.1)
        self.at(179, Write(f0), run_time=2.2)
        self.play(Create(ax), LaggedStart(*[Create(c) for c in curves], lag_ratio=0.3), FadeIn(lab), run_time=1.6)
        # cue 180: this is local equilibrium: the closure for the moment equations
        cl = chip("local equilibrium = the closure", YELLOW, 22, fill=0.1).move_to([-2.0, 0.75, 0])
        self.at(180, FadeIn(cl, shift=UP * 0.15), run_time=0.6)
        # cue 181: compute the unknown moments with f0 (even Gaussian in c): odd integrals vanish
        mom = VGroup(tex(r"P_{ij}=m\int d^3v\,c_ic_jf_0=nk_BT\,\delta_{ij}=p\,\delta_{ij}", 30),
                     tex(r"q_i=\int d^3v\,\tfrac12mc^2c_if_0=0", 30)).arrange(DOWN, buff=0.35, aligned_edge=LEFT)
        mom.move_to([-3.1, -0.6, 0])
        odd = note("odd in c: integrates to zero", 18, GREY_B).next_to(mom[1], DOWN, buff=0.12).align_to(mom[1], LEFT)
        self.at(181, Write(mom[0]), run_time=1.6)
        self.play(Write(mom[1]), FadeIn(odd), run_time=1.2)
        # ... substituting: the Euler equations for an ideal gas
        eu = VGroup(tex(r"\pdv{\rho}{t}+\nabla\cdot(\rho\va u)=0", 30),
                    tex(r"\rho\,D_t\va u=-\nabla p+n\va F", 30),
                    tex(r"\tfrac32nk_B\,D_tT=-p\,\nabla\cdot\va u,\qquad p=nk_BT", 30))
        eu.arrange(DOWN, buff=0.25, aligned_edge=LEFT).move_to([3.4, -2.0, 0])
        fit_box(eu, 6.6)
        ename = Text("Euler equations", font_size=22, color=ORANGE).next_to(eu, UP, buff=0.12).align_to(eu, LEFT)
        self.frac(181, 0.5, FadeOut(ax), FadeOut(curves), FadeOut(lab), run_time=0.4)
        self.play(FadeIn(ename), LaggedStart(*[Write(e) for e in eu], lag_ratio=0.4), run_time=2.6)
        # cue 182-183: combine with continuity: adiabatic with gamma = 5/3
        ad = tex(r"D_t\big(p\,\rho^{-5/3}\big)=0", 36, YELLOW).move_to([-3.1, -2.7, 0])
        self.at(182, Write(ad), run_time=1.2)
        gm = note("adiabatic with γ = 5/3, as it should be for a monatomic ideal gas", 19, GREEN)
        fit_box(gm, 6.0).move_to([-3.1, -3.2, 0])
        self.at(183, FadeIn(gm), run_time=0.5)
        self.snap("euler")
        self.finish()


# ==========================================================================
# K7_1 -- where did the arrow of time go?   (cues 184-190)
# ==========================================================================
class K7_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K7_1"], END["K7_1"]

    def construct(self):
        header = hdr("Local Equilibrium and the Euler Equations")
        self.add(header)
        # cue 184: notice what has happened to the arrow of time
        q = Text("what happened to the arrow of time?", font_size=36, color=YELLOW).move_to([0, 1.7, 0])
        self.at(184, FadeIn(q, shift=DOWN * 0.15), run_time=0.7)
        # cue 185-187: Euler has no dissipation: reversible again
        ax = Axes(x_range=[-6, 6, 2], y_range=[0, 1.2, 1], x_length=10.0, y_length=1.5,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([0, 0.15, 0])
        x0 = ValueTracker(-3.5)
        pulse = always_redraw(lambda: ax.plot(lambda x: math.exp(-1.2 * (x - x0.get_value()) ** 2),
                                              x_range=[-6, 6], color=TEAL, stroke_width=4))
        lbl = note("a density pulse in an Euler flow", 18, GREY_B).next_to(ax, UP, buff=0.1)
        self.at(185, FadeIn(ax), FadeIn(lbl), run_time=0.5)
        self.add(pulse)
        no_diss = Text("no dissipation: the pulse never smears out", font_size=26, color=TEAL).move_to([0, -1.4, 0])
        # cue 186 (2.1 s): the pulse travels; cue 187 (1.4 s): run it backwards
        self.at(186, FadeIn(no_diss), x0.animate.set_value(3.5), run_time=2.5, rate_func=linear)
        rev = tex(r"\va u\to-\va u,\quad t\to-t", 36, ORANGE).move_to([0, -2.3, 0])
        rv2 = note("reversible again", 24, GREEN).next_to(rev, DOWN, buff=0.2)
        self.at(187, FadeIn(rev, shift=UP * 0.1), FadeIn(rv2), x0.animate.set_value(-3.5),
                run_time=1.3, rate_func=linear)
        # cue 188: the irreversibility of Boltzmann is hiding in the local-equilibrium assumption
        pulse.clear_updaters()
        self.at(188, *[FadeOut(m) for m in (ax, pulse, lbl, no_diss, rev, rv2)], FadeOut(q), run_time=0.5)
        cB = card("Boltzmann\nirreversible", TEAL, 24, 3.2).move_to([-4.6, 0.5, 0])
        cL = card("local equilibrium\nmax-entropy state at fixed\nconserved densities", ORANGE, 22, 4.4).move_to([0.0, 0.5, 0])
        cE = card("Euler\nreversible", GREEN, 24, 3.0).move_to([4.8, 0.5, 0])
        a1 = Arrow(cB.get_right(), cL.get_left(), buff=0.1, color=GREY_B)
        a2 = Arrow(cL.get_right(), cE.get_left(), buff=0.1, color=GREY_B)
        hide = note("the irreversibility is hiding in this assumption", 21, ORANGE).next_to(cL, DOWN, buff=0.3)
        self.play(FadeIn(cB), GrowArrow(a1), FadeIn(cL), GrowArrow(a2), FadeIn(cE), run_time=1.4)
        self.play(FadeIn(hide), Indicate(cL, color=ORANGE, scale_factor=1.06), run_time=0.8)
        # cue 189: nothing in Euler produces entropy (except shocks, which Euler cannot itself describe)
        ns = note("nothing in Euler produces entropy\n(except shocks, which Euler cannot describe by itself)", 22, RED).move_to([0, -1.7, 0])
        self.at(189, FadeIn(ns, shift=UP * 0.15), run_time=0.7)
        # cue 190: to see viscosity and heat conduction, look at the first correction to local equilibrium
        nxt = chip("viscosity, heat conduction:  first correction to local equilibrium", YELLOW, 24, fill=0.1)
        fit_box(nxt, 11.5).move_to([0, -2.9, 0])
        self.at(190, FadeIn(nxt, shift=UP * 0.15), run_time=0.7)
        self.snap("arrow")
        self.finish()


# ==========================================================================
# K8 -- Chapman-Enskog expansion, BGK model   (cues 191-203)
# ==========================================================================
class K8(SyncedScene):
    AUDIO_START, AUDIO_END = START["K8"], END["K8"]

    def construct(self):
        header = hdr("Chapman-Enskog")
        # cue 191-192: write the Boltzmann equation to show the small parameter epsilon ~ Kn
        self.at(191, Write(header), run_time=0.7)
        eq = MathTex(r"\mathcal D f\equiv\Big(\pdv{}{t}+\va v\cdot\nabla+\va a\cdot\nabla_{\va v}\Big)f=",
                     r"\frac{1}{\varepsilon}", r"C[f]", font_size=44)
        eq[1].set_color(ORANGE)
        eq.move_to([0, 1.7, 0])
        fit_box(eq, 12.0)
        self.sync(CUES[191].start + 1.0)
        self.play(Write(eq), run_time=1.8)
        n192 = tex(r"\text{units of } L \text{ and } L/\bar v:\quad \mathcal D f=O(1),\quad C[f]=O(1/\varepsilon),\quad"
                   r"\varepsilon\sim\mathrm{Kn}=\frac{\lambda}{L}", 28, GREY_B)
        fit_box(n192, 12.2).move_to([0, 0.55, 0])
        self.at(192, Write(n192), run_time=3.0)
        # cue 193: expand f in epsilon: all the time dependence enters through the five fields
        ex = tex(r"f=f^{(0)}+\varepsilon f^{(1)}+\varepsilon^2f^{(2)}+\cdots", 44, YELLOW).move_to([0, -0.7, 0])
        note193 = note("time dependence only through the five hydrodynamic fields; solve order by order", 20, GREY_B)
        note193.next_to(ex, DOWN, buff=0.25)
        self.at(193, Write(ex), run_time=1.6)
        self.play(FadeIn(note193), run_time=0.5)
        # cue 194-196: order eps^-1: C[f0] = 0, so f0 is the local Maxwellian: the Euler closure
        self.at(194, FadeOut(n192), FadeOut(note193), ex.animate.move_to([0, 0.6, 0]).scale(0.8), run_time=0.6)
        o1 = VGroup(tex(r"\varepsilon^{-1}:", 34, ORANGE), tex(r"C[f^{(0)}]=0", 34),
                    tex(r"\Rightarrow\ f^{(0)}=f_0\ \text{(local Maxwellian)}", 30, GREEN),
                    note("Euler", 24, GREEN)).arrange(RIGHT, buff=0.35)
        fit_box(o1, 12.4).move_to([0, -0.5, 0])
        self.play(Write(o1[0]), Write(o1[1]), run_time=1.0)
        self.at(195, Write(o1[2]), run_time=1.2)
        self.at(196, FadeIn(o1[3], shift=LEFT * 0.2), run_time=0.5)
        # cue 197-198: order eps^0: linearise C about f0, solve for f^(1)
        o2 = VGroup(tex(r"\varepsilon^{0}:", 34, ORANGE), tex(r"\mathcal Df_0=\mathcal L f^{(1)}", 34),
                    tex(r"\Rightarrow\ \text{solve for } f^{(1)}", 30, GREEN)).arrange(RIGHT, buff=0.35)
        o2.move_to([-1.0, -1.55, 0])
        lin = note("𝓛: collision operator linearised about f₀", 19, GREY_B).next_to(o2, DOWN, buff=0.15)
        self.at(197, Write(o2[0]), Write(o2[1]), FadeIn(lin), run_time=1.6)
        self.at(198, Write(o2[2]), run_time=1.0)
        # cue 199-200: insert f0 + f1 into the exact conservation laws -> Navier-Stokes
        o3 = VGroup(tex(r"f_0+\varepsilon f^{(1)}\ \longrightarrow\ \text{extra } P_{ij},\ q_i", 30),
                    tex(r"\Rightarrow\ \text{Navier-Stokes}", 34, ORANGE)).arrange(RIGHT, buff=0.4)
        fit_box(o3, 12.0).move_to([0, -2.65, 0])
        self.at(199, Write(o3[0]), run_time=1.4)
        self.at(200, Write(o3[1]), run_time=1.0)
        self.snap("ordering")
        # cue 201: BGK -- the simplest collision model with the right properties
        wipe = [m for m in self.mobjects if m not in self._persist and m is not header]
        self.at(201, *[FadeOut(m) for m in wipe], run_time=0.5)
        bg = tex(r"C[f]\ \to\ -\frac{f-f_0}{\tau}", 56, YELLOW).move_to([0, 1.3, 0])
        bgl = Text("BGK model (Bhatnagar-Gross-Krook)", font_size=26, color=ORANGE).move_to([0, 2.25, 0])
        self.play(FadeIn(bgl), Write(bg), run_time=1.6)
        # cue 202: f0 has the same n, u, T as f: conservation survives; tau ~ mean free time
        pr = VGroup(chip("f₀: same n, u, T as f", GREEN, 22), chip("mass, momentum, energy still conserved", GREEN, 22),
                    chip("τ ~ mean free time", TEAL, 22)).arrange(DOWN, buff=0.3).move_to([0, -0.5, 0])
        self.at(202, LaggedStart(*[FadeIn(c, shift=UP * 0.12) for c in pr], lag_ratio=0.5), run_time=2.4)
        # cue 203: so f^(1) = -tau D f0 and the whole problem is computing D f0
        f1 = tex(r"f^{(1)}=-\tau\,\mathcal Df_0", 50, YELLOW).move_to([0, -2.3, 0])
        fb = SurroundingRectangle(f1, color=YELLOW, buff=0.2)
        self.at(203, Write(f1), Create(fb), run_time=1.2)
        self.finish()


# ==========================================================================
# K8_1 -- computing D f_0   (cues 204-213)
# ==========================================================================
class K8_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K8_1"], END["K8_1"]

    def construct(self):
        header = hdr("Chapman-Enskog")
        self.add(header)
        # cue 204: ln f0
        l0 = tex(r"\ln f_0=\ln n-\tfrac32\ln T-\frac{mc^2}{2k_BT}+\text{const}", 36).move_to([0, 1.95, 0])
        self.at(204, Write(l0), run_time=2.4)
        # cue 205: c = v - u, so grad_v ln f0 = -m c / kT
        l1 = tex(r"\va c=\va v-\va u,\qquad \nabla_{\va v}\ln f_0=-\frac{m\va c}{k_BT}", 34, YELLOW).move_to([0, 1.0, 0])
        self.at(205, Write(l1), run_time=1.8)
        # cue 206: D ln f0: the time derivatives are not independent
        D = MathTex(r"\mathcal D\ln f_0=", r"\frac{D_tn+\va c\cdot\nabla n}{n}",
                    r"+\frac{mc_j}{k_BT}\big(D_tu_j+\va c\cdot\nabla u_j\big)",
                    r"+\Big(\frac{mc^2}{2k_BT}-\frac32\Big)\frac{D_tT+\va c\cdot\nabla T}{T}",
                    r"-\frac{m}{k_BT}\,\va a\cdot\va c", font_size=30)
        fit_box(D, 12.4).move_to([0, -0.35, 0])
        D[1].set_color(BLUE_C)
        D[2].set_color(TEAL)
        D[3].set_color(GREEN)
        D[4].set_color(ORANGE)
        self.at(206, Write(D), run_time=3.2)
        # cue 207: to this order the time derivatives are given by the Euler equations
        eu = tex(r"D_tn=-n\nabla\!\cdot\!\va u,\qquad D_tu_j=a_j-\frac{1}{mn}\partial_j(nk_BT),\qquad D_tT=-\tfrac23T\,\nabla\!\cdot\!\va u",
                 28, YELLOW)
        fit_box(eu, 12.4).move_to([0, -1.6, 0])
        eub = note("time derivatives replaced using the Euler equations", 18, GREY_B).next_to(eu, DOWN, buff=0.12)
        self.at(207, Write(eu), FadeIn(eub), run_time=2.4)
        # cue 208-210: two things happen at once: acceleration terms cancel; c.grad n terms cancel
        self.at(208, Indicate(D[2], color=YELLOW, scale_factor=1.05), Indicate(D[4], color=YELLOW, scale_factor=1.05),
                run_time=1.2)
        n209 = note("acceleration terms cancel against −(m/kT) a·c", 21, ORANGE).move_to([-2.6, -2.75, 0])
        self.at(209, FadeIn(n209), run_time=0.5)
        n210 = note("c·∇n terms cancel among themselves", 21, BLUE_C).move_to([3.6, -2.75, 0])
        self.at(210, FadeIn(n210), Indicate(D[1], color=BLUE_C, scale_factor=1.05), run_time=0.8)
        # cue 211-212: no density gradient and no force survive: a uniform gas in a uniform force field stays in equilibrium
        n211 = Text("no density gradient and no external force survive", font_size=26, color=YELLOW).move_to([0, -3.3, 0])
        self.at(211, FadeIn(n211, shift=UP * 0.1), run_time=0.6)
        n212 = Text("a uniform gas in a uniform force field stays in equilibrium", font_size=26, color=GREEN).move_to([0, -3.3, 0])
        self.at(212, FadeOut(n211), FadeIn(n212), run_time=0.5)
        # cue 213: what is left depends only on velocity and temperature gradients
        wipe = [m for m in self.mobjects if m not in self._persist and m is not header]
        res = MathTex(r"\mathcal Df_0=f_0\Big[\frac{m}{2k_BT}\,c_ic_j\,\sigma_{ij}+\Big(\frac{mc^2}{2k_BT}-\frac52\Big)\frac{\va c\cdot\nabla T}{T}\Big]",
                      font_size=40)
        fit_box(res, 12.4).move_to([0, 0.7, 0])
        sg = tex(r"\sigma_{ij}\equiv\partial_iu_j+\partial_ju_i-\tfrac23\delta_{ij}\nabla\!\cdot\!\va u", 32, GREY_B).move_to([0, -0.7, 0])
        tags = VGroup(chip("velocity gradients", TEAL, 22), chip("temperature gradient", ORANGE, 22))
        tags[0].move_to([-3.2, -1.9, 0]); tags[1].move_to([3.2, -1.9, 0])
        two = note("the two things we expect to drive transport", 22, YELLOW).move_to([0, -2.9, 0])
        self.at(213, *[FadeOut(m) for m in wipe], run_time=0.5)
        self.play(Write(res), run_time=2.2)
        self.play(FadeIn(sg), FadeIn(tags), FadeIn(two), run_time=1.0)
        self.snap("Df0")
        self.finish()


# ==========================================================================
# K9 -- stress, heat flux -> Navier-Stokes-Fourier   (cues 214-225)
# ==========================================================================
class K9(SyncedScene):
    AUDIO_START, AUDIO_END = START["K9"], END["K9"]

    def construct(self):
        header = hdr("Chapman-Enskog")
        self.add(header)
        # cue 214: the Gaussian moments we need
        g4 = tex(r"\int d^3v\,f_0\,c_ic_jc_kc_l=n\Big(\frac{k_BT}{m}\Big)^{2}"
                 r"\big(\delta_{ij}\delta_{kl}+\delta_{ik}\delta_{jl}+\delta_{il}\delta_{jk}\big)", 34)
        fit_box(g4, 12.4).move_to([0, 1.7, 0])
        self.at(214, Write(g4), run_time=2.6)
        # cue 215: averages of x = m c^2 / 2 k_B T
        av = tex(r"x=\frac{mc^2}{2k_BT}:\qquad \langle x\rangle=\tfrac32,\quad\langle x^2\rangle=\tfrac{15}{4},"
                 r"\quad\langle x^3\rangle=\tfrac{105}{8}", 34, YELLOW)
        av.move_to([0, 0.2, 0])
        self.at(215, Write(av), run_time=2.0)
        # cue 216-217: stress -- only the c_i c_j sigma term contributes; sigma symmetric and traceless
        st = tex(r"P_{ij}-p\,\delta_{ij}=-\tau\,m\!\int\! d^3v\,c_ic_j\,f_0\,\frac{m}{2k_BT}\,c_kc_l\,\sigma_{kl}"
                 r"=-\tau\,nk_BT\,\sigma_{ij}", 30)
        fit_box(st, 12.4).move_to([0, 1.85, 0])
        n216 = note("only the c_i c_j σ term contributes: the temperature term is odd in c", 19, ORANGE).move_to([0, 1.1, 0])
        self.at(216, FadeOut(g4), FadeOut(av), run_time=0.4)
        self.play(Write(st), FadeIn(n216), run_time=1.8)
        n217 = tex(r"\sigma_{kl}\ \text{symmetric and traceless}", 24, GREY_B).move_to([0, 1.1, 0])
        self.at(217, FadeOut(n216), FadeIn(n217), run_time=0.5)
        # cue 218-219: heat flux -- only the temperature term; isotropy
        hf = tex(r"q_i=-\frac{\tau}{3T}\,\partial_iT\!\int\! d^3v\,f_0\,\tfrac12mc^2\,c^2\Big(\frac{mc^2}{2k_BT}-\frac52\Big)"
                 r"=-\frac52\,\frac{nk_B^2T\tau}{m}\,\partial_iT", 30)
        fit_box(hf, 12.4).move_to([0, 0.0, 0])
        n218 = note("only the temperature term contributes: the stress term is even in c", 19, GREEN).move_to([0, -0.8, 0])
        self.at(218, Write(hf), FadeIn(n218), run_time=2.2)
        n219 = tex(r"\text{isotropy:}\ \int f_0\,c_ic_j\,g(c^2)=\tfrac13\delta_{ij}\int f_0\,c^2g", 24, GREY_B).move_to([0, -0.8, 0])
        self.at(219, FadeOut(n218), FadeIn(n219), run_time=0.5)
        # cue 220: exactly Newton's law of viscosity and Fourier's law of heat conduction
        nw = tex(r"P_{ij}=p\,\delta_{ij}-\mu\,\sigma_{ij}", 38, ORANGE).move_to([-3.5, -1.75, 0])
        fo = tex(r"\va q=-\kappa\,\nabla T", 38, GREEN).move_to([3.5, -1.75, 0])
        nwl = note("Newton's law of viscosity", 19, ORANGE).next_to(nw, DOWN, buff=0.12)
        fol = note("Fourier's law of heat conduction", 19, GREEN).next_to(fo, DOWN, buff=0.12)
        co = tex(r"\mu=nk_BT\,\tau=p\tau,\qquad \kappa=\frac52\,\frac{k_B}{m}\,\mu", 32, YELLOW).move_to([0, -3.0, 0])
        self.at(220, FadeOut(n217), FadeOut(n219), Write(nw), Write(fo), FadeIn(nwl), FadeIn(fol), run_time=1.6)
        self.play(Write(co), run_time=1.2)
        self.snap("laws")

        # cue 221: plug into the exact conservation laws: Navier-Stokes-Fourier
        wipe = [m for m in self.mobjects if m not in self._persist and m is not header]
        N1 = tex(r"\pdv{\rho}{t}+\nabla\cdot(\rho\va u)=0", 36)
        N2 = MathTex(r"\rho\,D_t\va u=-\nabla p", r"+\nabla\cdot(\mu\,\sigma)", r"+n\va F", font_size=36)
        N3 = MathTex(r"\tfrac32nk_B\,D_tT=-p\,\nabla\cdot\va u", r"+\tfrac12\mu\,\sigma_{ij}\sigma_{ij}",
                     r"+\nabla\cdot(\kappa\nabla T)", font_size=36)
        eqs = VGroup(N1, N2, N3).arrange(DOWN, buff=0.75, aligned_edge=LEFT).move_to([-0.3, 1.0, 0])
        N2[1].set_color(ORANGE)
        N3[1].set_color(RED)
        N3[2].set_color(GREEN)
        ttl = Text("Navier-Stokes-Fourier", font_size=26, color=YELLOW).move_to([0, 2.45, 0])
        self.at(221, *[FadeOut(m) for m in wipe], run_time=0.5)
        self.play(FadeIn(ttl), LaggedStart(*[Write(e) for e in eqs], lag_ratio=0.4), run_time=3.0)
        # cue 222: the new terms: viscous stress, viscous heating, heat conduction
        l1 = note("viscous stress", 19, ORANGE).next_to(N2[1], DOWN, buff=0.12)
        l2 = note("viscous heating  ≥ 0", 19, RED).next_to(N3[1], DOWN, buff=0.12)
        l3 = note("heat conduction", 19, GREEN).next_to(N3[2], DOWN, buff=0.12)
        self.at(222, LaggedStart(FadeIn(l1), FadeIn(l2), FadeIn(l3), lag_ratio=0.4), run_time=1.6)
        # cue 223: dissipative terms, responsible for entropy production
        d1 = chip("dissipative terms: they produce entropy", RED, 22, fill=0.08).move_to([0, -2.0, 0])
        self.at(223, FadeIn(d1, shift=UP * 0.15), run_time=0.6)
        # cue 224: the H-theorem shows up in the transport coefficients
        d2 = tex(r"\mu\ge0,\quad\kappa\ge0\qquad\longleftarrow\qquad\text{the H-theorem}", 30, YELLOW).move_to([0, -2.85, 0])
        self.at(224, Write(d2), run_time=1.4)
        # cue 225: the arrow of time enters at first order in Kn, through f^(1)
        d3 = note("Euler lacked the arrow of time: it enters at first order in Kn, through the deviation f⁽¹⁾",
                  19, ORANGE)
        fit_box(d3, 12.2).move_to([0, -3.4, 0])
        self.at(225, FadeIn(d3, shift=UP * 0.1), run_time=0.6)
        self.finish()


# ==========================================================================
# K9_1 -- incompressible limit, hard spheres, what BGK gets wrong   (cues 226-237)
# ==========================================================================
def hard_sphere_mu(T=300.0, d=3.7e-10, mass_u=28.0134):
    kB, u = 1.380649e-23, 1.66053907e-27
    m = mass_u * u
    return 5.0 / (16.0 * d * d) * math.sqrt(m * kB * T / math.pi)


class K9_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K9_1"], END["K9_1"]

    def construct(self):
        header = hdr("Chapman-Enskog")
        self.add(header)
        # cue 226: low Mach number, nearly constant density: incompressible equations
        inc = tex(r"\pdv{\va u}{t}+(\va u\cdot\nabla)\va u=-\frac{\nabla p}{\rho}+\nu\nabla^2\va u,\qquad\nabla\cdot\va u=0", 38)
        fit_box(inc, 12.4).move_to([0, 1.7, 0])
        nu = tex(r"\nu=\mu/\rho", 32, YELLOW).move_to([0, 0.6, 0])
        self.at(226, Write(inc), run_time=2.4)
        self.play(FadeIn(nu, shift=UP * 0.1), run_time=0.5)
        # cue 227: scaling: Ma ~ Kn -> 0 keeps Re of order one
        sc = tex(r"\mathrm{Ma}\sim\mathrm{Kn}\to0\ \ \text{with}\ \ \mathrm{Re}=O(1):\qquad \mathrm{Kn}\sim\frac{\mathrm{Ma}}{\mathrm{Re}}", 36, ORANGE)
        fit_box(sc, 12.2).move_to([0, -0.9, 0])
        sn = note("(up to numerical factors)", 19, GREY_B).next_to(sc, DOWN, buff=0.15)
        self.at(227, Write(sc), FadeIn(sn), run_time=2.4)

        # cue 228-229: the structure is robust: isotropy leaves no other possibility, only coefficients change
        self.at(228, *[FadeOut(m) for m in (inc, nu, sc, sn)], run_time=0.5)
        rb = chip("the structure is robust", GREEN, 30, fill=0.08).move_to([0, 1.8, 0])
        self.play(FadeIn(rb, scale=0.95), run_time=0.6)
        iso = note("true Boltzmann collision operator: the leading correction has the same tensor form,\n"
                   "because isotropy leaves no other possibility; only the coefficients change", 21, GREY_B)
        iso.move_to([0, 0.6, 0])
        self.at(229, FadeIn(iso, shift=UP * 0.1), run_time=0.7)
        # cue 230: hard spheres, first Chapman-Enskog approximation
        hs = tex(r"\mu=\frac{5}{16\,d^2}\sqrt{\frac{mk_BT}{\pi}},\qquad \kappa=\frac{15}{4}\frac{k_B}{m}\,\mu", 40, YELLOW)
        fit_box(hs, 12.0).move_to([0, -1.0, 0])
        hb = SurroundingRectangle(hs, color=YELLOW, buff=0.2)
        hl = note("hard spheres, first Chapman-Enskog approximation", 20, GREY_B).next_to(hb, DOWN, buff=0.12)
        self.at(230, Write(hs), run_time=2.2)
        self.play(Create(hb), FadeIn(hl), run_time=0.6)

        # cue 231-233: two remarkable things
        self.at(231, FadeOut(rb), FadeOut(iso), FadeOut(hl), run_time=0.4)
        self.play(VGroup(hs, hb).animate.scale(0.7).move_to([0, 1.8, 0]), run_time=0.7)
        two = Text("two remarkable things", font_size=26, color=ORANGE).move_to([0, 0.95, 0])
        self.play(FadeIn(two), run_time=0.4)
        # cue 232: mu independent of density (Maxwell), grows like sqrt(T)
        a1 = Axes(x_range=[0, 4, 1], y_range=[0, 2, 1], x_length=3.4, y_length=1.8,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([-4.6, -0.55, 0])
        c1 = a1.plot(lambda x: 1.0, x_range=[0.05, 4], color=TEAL, stroke_width=4)
        t1 = VGroup(tex(r"\mu", 24, GREY_B).next_to(a1, UP, buff=0.05).align_to(a1, LEFT),
                    tex(r"n\ \text{(or } p)", 22, GREY_B).next_to(a1, DOWN, buff=0.05).align_to(a1, RIGHT),
                    note("independent of density", 18, TEAL).next_to(a1, DOWN, buff=0.4))
        a2 = Axes(x_range=[0, 4, 1], y_range=[0, 2.2, 1], x_length=3.4, y_length=1.8,
                  axis_config={"include_tip": False, "stroke_width": 2}).move_to([-0.6, -0.55, 0])
        c2 = a2.plot(lambda x: math.sqrt(x), x_range=[0, 4], color=ORANGE, stroke_width=4)
        t2 = VGroup(tex(r"\mu", 24, GREY_B).next_to(a2, UP, buff=0.05).align_to(a2, LEFT),
                    tex(r"T", 22, GREY_B).next_to(a2, DOWN, buff=0.05).align_to(a2, RIGHT),
                    note("grows like √T", 18, ORANGE).next_to(a2, DOWN, buff=0.4))
        self.at(232, FadeOut(two), Create(a1), Create(c1), FadeIn(t1), run_time=1.2)
        self.play(Create(a2), Create(c2), FadeIn(t2), run_time=1.2)
        mx = note("Maxwell's famous prediction:\nρ in μ ~ ρ v̄ λ is cancelled by λ ∝ 1/ρ", 16, GREY_B).move_to([-3.6, -3.0, 0])
        self.frac(232, 0.6, FadeIn(mx), run_time=0.5)
        # cue 233: check with nitrogen at 300 K, d = 3.7 angstrom
        mu_val = hard_sphere_mu()
        mant = mu_val * 1e5
        chk = VGroup(Text("nitrogen, 300 K, d ≈ 3.7 Å", font_size=22, color=YELLOW),
                     tex(r"\mu\approx%.2f\times10^{-5}\ \mathrm{Pa\,s}" % mant, 38, YELLOW),
                     note("measured viscosity of air: within a few percent\n(d is a fitted diameter: a consistency check,\nnot a prediction)", 17, GREY_B)
                     ).arrange(DOWN, buff=0.25)
        fit_box(chk, 4.6).move_to([4.0, -0.7, 0])
        self.at(233, FadeIn(chk, shift=LEFT * 0.2), run_time=0.9)
        self.snap("viscosity")

        # cue 234-237: what BGK gets wrong
        wipe = [m for m in self.mobjects if m not in self._persist and m is not header]
        cB = VGroup(Text("BGK (relaxation time)", font_size=24, color=ORANGE),
                    tex(r"\kappa=\frac52\,\frac{k_B}{m}\,\mu", 34),
                    tex(r"\mathrm{Pr}=\frac{c_p\mu}{\kappa}=1", 34, ORANGE)).arrange(DOWN, buff=0.4)
        cH = VGroup(Text("true Boltzmann operator (hard spheres)", font_size=24, color=GREEN),
                    tex(r"\kappa=\frac{15}{4}\,\frac{k_B}{m}\,\mu", 34),
                    tex(r"\mathrm{Pr}=\frac23\ \ \text{(real monatomic gases)}", 30, GREEN)).arrange(DOWN, buff=0.4)
        bx1 = SurroundingRectangle(cB, color=ORANGE, buff=0.3)
        bx2 = SurroundingRectangle(cH, color=GREEN, buff=0.3)
        both = VGroup(VGroup(bx1, cB).move_to([-3.6, 0.3, 0]), VGroup(bx2, cH).move_to([3.4, 0.3, 0]))
        self.at(234, *[FadeOut(m) for m in wipe], run_time=0.5)
        self.play(FadeIn(both[0], shift=RIGHT * 0.2), FadeIn(both[1], shift=LEFT * 0.2), run_time=1.0)
        fac = tex(r"\kappa/\mu\ \text{off by a factor}\ \tfrac23", 34, RED).move_to([0, -1.9, 0])
        self.play(Write(fac), run_time=1.2)
        self.at(235, Indicate(cB[2], color=ORANGE, scale_factor=1.2), Indicate(cH[2], color=GREEN, scale_factor=1.2),
                run_time=1.4)
        self.at(236, FadeIn(note("a known limitation of BGK, fixed by modified models", 22, YELLOW).move_to([0, -2.7, 0])),
                run_time=0.6)
        fin = Text("a model collision term is good for structure, bad for coefficients", font_size=26, color=YELLOW)
        fin.move_to([0, -3.25, 0])
        self.at(237, FadeIn(fin, shift=UP * 0.1), run_time=0.7)
        self.finish()


# ==========================================================================
# K10 -- three descriptions: what each keeps and what it costs   (cues 238-250)
# ==========================================================================
class K10(SyncedScene):
    AUDIO_START, AUDIO_END = START["K10"], END["K10"]

    def construct(self):
        header = hdr("When to Use Which Description")
        cols =[("Hamiltonian / MD", BLUE, -4.5), ("Boltzmann", TEAL, 0.0), ("Navier-Stokes", ORANGE, 4.5)]
        boxes = VGroup(*[RoundedRectangle(corner_radius=0.12, width=4.2, height=4.55, color=c).move_to([x, -0.6, 0])
                         for _, c, x in cols])
        titles = VGroup(*[Text(n, font_size=26, color=c).move_to([x, 1.2, 0]) for n, c, x in cols])
        # cue 238-239: three descriptions; each trades how much it keeps against what it costs
        sub = note("how much it keeps   vs.   how much it costs", 22, YELLOW).move_to([0, 2.05, 0])
        self.at(238, Write(header), FadeIn(boxes), FadeIn(titles), run_time=0.9)
        self.at(239, FadeIn(sub), run_time=0.5)

        def item(x, y, s, color=WHITE, size=17):
            return Text(s, font_size=size, color=color, line_spacing=0.9).move_to([x, y, 0])

        X0, X1, X2 = (c[2] for c in cols)
        # cue 240: MD is always correct for classical particles, with no assumption about density/correlations
        i1 = item(X0, 0.45, "always correct for classical\nparticles: no assumption about\ndensity or correlations", BLUE_C)
        self.at(240, FadeIn(i1, shift=UP * 0.1), run_time=0.6)
        # cue 241-242: but you follow every particle: small systems, short times
        i2 = item(X0, -0.65, "cost: follow every particle", RED)
        self.at(241, FadeIn(i2, shift=UP * 0.1), run_time=0.5)
        i3 = item(X0, -1.2, "small systems, short times", RED)
        self.at(242, FadeIn(i3, shift=UP * 0.1), run_time=0.5)
        # cue 243: the tool for dense gases, nanoscale systems, where correlations matter
        i4 = item(X0, -2.15, "use it when: dense gas,\nnanoscale, correlations matter", GREEN)
        self.at(243, FadeIn(i4, shift=UP * 0.1), run_time=0.6)

        # cue 244: Boltzmann keeps the velocity distribution: non-equilibrium states; needs a dilute gas
        j1 = item(X1, 0.55, "keeps the velocity distribution:\nnon-equilibrium states are fine", TEAL)
        self.at(244, FadeIn(j1, shift=UP * 0.1), run_time=0.6)
        j2 = item(X1, -0.3, "needs: dilute gas, short-range\ninteractions, molecular chaos", ORANGE)
        self.frac(244, 0.55, FadeIn(j2, shift=UP * 0.1), run_time=0.5)
        # cue 245: f lives in 6D + time; the collision integral is 5D at each point: expensive
        j3 = item(X1, -1.2, "f lives in 6D + time; the collision\nintegral is 5D at every point", RED)
        self.at(245, FadeIn(j3, shift=UP * 0.1), run_time=0.6)
        # cue 246-247: in practice Monte Carlo particle methods: DSMC, or discrete-velocity methods
        j4 = item(X1, -2.15, "in practice: Monte Carlo\n(DSMC) or discrete-\nvelocity methods", GREEN)
        self.at(246, FadeIn(j4, shift=UP * 0.1), run_time=0.6)

        # cue 248: Navier-Stokes: five fields in three dimensions
        k1 = item(X2, 0.45, "five fields in three dimensions", ORANGE)
        self.at(248, FadeIn(k1, shift=UP * 0.1), run_time=0.5)
        # cue 249: cheap, but needs local equilibrium
        k2 = item(X2, -0.35, "cheap", GREEN, 22)
        k3 = item(X2, -1.1, "but needs local equilibrium", RED)
        self.at(249, FadeIn(k2, shift=UP * 0.1), run_time=0.4)
        self.play(FadeIn(k3, shift=UP * 0.1), run_time=0.4)
        # cue 250: f close to a Maxwellian everywhere: Kn << 1
        k4 = tex(r"f\approx\text{Maxwellian everywhere}\ \Leftrightarrow\ \mathrm{Kn}\ll1", 22, YELLOW)
        fit_box(k4, 3.9).move_to([X2, -2.15, 0])
        self.at(250, Write(k4), run_time=1.2)
        self.snap("tradeoffs")
        self.finish()


# ==========================================================================
# K10_1 -- the Knudsen number organises the choice   (cues 251-260)
# ==========================================================================
def kx(logk):
    """x-position of log10(Kn) on the regime bar (Kn = 1e-5 ... 1e3)."""
    return -6.0 + 12.0 * (logk + 5.0) / 8.0


class K10_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["K10_1"], END["K10_1"]

    def construct(self):
        header = hdr("When to Use Which Description")
        self.add(header)
        # cue 251: the Knudsen number organises the choice, with the usual rules of thumb
        bands = [(-5, -3, BLUE, "continuum", "Navier-Stokes\n(Euler outside layers)"),
                 (-3, -1, TEAL, "slip flow", "Navier-Stokes + slip and\ntemperature jump"),
                 (-1, 1, ORANGE, "transitional", "Boltzmann\n(e.g. DSMC)"),
                 (1, 3, RED, "free molecular", "collisionless\nBoltzmann")]
        yb = 0.85
        rects, names, models = VGroup(), VGroup(), VGroup()
        for a, b, c, nm, md in bands:
            r = Rectangle(width=kx(b) - kx(a), height=0.85, stroke_width=2, color=c, fill_color=c, fill_opacity=0.22)
            r.move_to([(kx(a) + kx(b)) / 2, yb, 0])
            rects.add(r)
            names.add(Text(nm, font_size=21, color=c).move_to(r))
            models.add(Text(md, font_size=15, color=GREY_B, line_spacing=0.9).move_to([(kx(a) + kx(b)) / 2, yb - 0.95, 0]))
        ticks = VGroup(*[VGroup(Line([kx(lg), yb - 0.5, 0], [kx(lg), yb - 0.4, 0], color=GREY_B),
                                tex(lab, 22, GREY_B).move_to([kx(lg), yb + 0.7, 0]))
                         for lg, lab in [(-3, r"10^{-3}"), (-1, r"10^{-1}"), (1, r"10")]])
        axis = Arrow([kx(-5) - 0.3, 1.95, 0], [kx(3) + 0.3, 1.95, 0], buff=0, color=GREY_B, stroke_width=2)
        axl = tex(r"\mathrm{Kn}=\lambda/L", 26, YELLOW).move_to([0, 2.3, 0])
        self.at(251, FadeIn(axis), FadeIn(axl), LaggedStart(*[FadeIn(r) for r in rects], lag_ratio=0.2), run_time=1.4)
        self.play(FadeIn(names), FadeIn(ticks), run_time=0.6)
        self.play(FadeIn(models), run_time=0.7)
        # cue 252: the boundaries are not sharp; they depend on what you want
        ns = note("boundaries are not sharp: they depend on the quantity you want", 20, YELLOW).move_to([0, -1.2, 0])
        self.at(252, FadeIn(ns, shift=UP * 0.1), run_time=0.6)
        # cue 253-254: re-entry at 100 km: mean free path ~ 0.1 m ~ the size of the spacecraft
        e1 = chip("re-entry at 100 km: λ ~ 0.1 m,\ncomparable to the spacecraft", ORANGE, 17)
        e1.move_to([3.5, -2.2, 0])
        self.at(253, FadeOut(ns), run_time=0.3)
        self.play(FadeIn(e1, shift=UP * 0.15), run_time=0.6)
        self.at(254, Indicate(rects[2], color=ORANGE, scale_factor=1.04), run_time=1.0)
        # cue 255: microchannels / nanofluidics: 1 micron channel at 1 atm sits in the slip regime
        e2 = chip("1 μm channel at 1 atm: Kn ≈ 0.07\nno-slip fails", TEAL, 17).move_to([-3.5, -2.2, 0])
        self.at(255, FadeIn(e2, shift=UP * 0.15), Indicate(rects[1], color=TEAL, scale_factor=1.04), run_time=0.9)
        # cue 256: shock waves are only a few mean free paths thick
        e3 = chip("shock wave: a few mean free paths thick\nNavier-Stokes closure unreliable", RED, 17).move_to([3.5, -3.05, 0])
        self.at(256, FadeIn(e3, shift=UP * 0.15), run_time=0.7)
        # cue 257: high-frequency sound is another example
        e4 = chip("high-frequency sound:\nperiod ~ collision time", PURPLE, 17).move_to([-3.5, -3.05, 0])
        self.at(257, FadeIn(e4, shift=UP * 0.15), run_time=0.7)
        # cue 258: the relevant L is the local gradient length, not the size of the container
        self.at(258, *[FadeOut(m) for m in (e1, e2, e3, e4)], run_time=0.4)
        gl = tex(r"L=\frac{Q}{|\nabla Q|}\qquad Q=\rho,\ T,\ \va u", 40, YELLOW).move_to([-2.4, -1.9, 0])
        gn = note("the local gradient length,\nnot the size of the container", 20, GREY_B).move_to([3.7, -1.9, 0])
        self.play(Write(gl), FadeIn(gn), run_time=1.4)
        # cue 259-260: continuum equations begin to fail when lambda |grad Q| / Q exceeds a few percent
        rule = tex(r"\frac{\lambda\,|\nabla Q|}{Q}\ \gtrsim\ \text{a few percent}\ \Longrightarrow\ \text{continuum equations begin to fail}", 32, RED)
        fit_box(rule, 12.2).move_to([0, -3.0, 0])
        self.at(259, Write(rule), run_time=2.4)
        self.at(260, Indicate(rule, color=RED, scale_factor=1.04), run_time=1.0)
        self.snap("regimes")
        self.finish()


# ==========================================================================
# K11 -- closing   (cue 261)
# ==========================================================================
class K11(SyncedScene):
    AUDIO_START, AUDIO_END = START["K11"], END["K11"]
    STARS = (40, 9)

    def construct(self):
        nH = card("Hamiltonian\nN-body", BLUE, 26, 3.4)
        nB = card("Boltzmann", TEAL, 26, 3.4)
        nN = card("Navier-Stokes", ORANGE, 26, 3.4)
        row = VGroup(nH, nB, nN).arrange(RIGHT, buff=1.6).move_to([0, 0.9, 0])
        clamp_width(row)
        a1 = Arrow(nH.get_right(), nB.get_left(), buff=0.1, color=GREY_B)
        a2 = Arrow(nB.get_right(), nN.get_left(), buff=0.1, color=GREY_B)
        l1 = note("molecular chaos", 18, YELLOW).next_to(a1, UP, buff=0.1)
        l2 = note("first order in Kn", 18, YELLOW).next_to(a2, UP, buff=0.1)
        # cue 261: so much more to say; a more rigorous derivation; thanks for watching
        self.at(261, FadeIn(nH), FadeIn(nB), GrowArrow(a1), FadeIn(l1), FadeIn(nN), GrowArrow(a2), FadeIn(l2),
                run_time=1.6)
        more = Text("there is much more to say, and a far more rigorous derivation,\nas some mathematicians have recently managed",
                    font_size=24, color=GREY_B, line_spacing=0.95).move_to([0, -1.2, 0])
        self.frac(261, 0.3, FadeIn(more, shift=UP * 0.15), run_time=0.8)
        thx = Text("Thank you for watching", font_size=44, color=YELLOW).move_to([0, -2.6, 0])
        self.frac(261, 0.65, FadeIn(thx, scale=0.95), run_time=0.9)
        self.finish()


if __name__ == "__main__":
    print("Scenes:", ", ".join(ALL_SCENES))
    print(f"Narration length: {TOTAL_DURATION:.1f} s ({len(CUES)} cues)")
    for n in ALL_SCENES:
        print(f"  {n:6s} {START[n]:8.2f} -> {END[n]:8.2f}  ({END[n]-START[n]:6.1f} s)")