#!/usr/bin/env python3

"""
metriplectic_scenes.py
================================================================================
Manim animation for "Metriplectic Dynamics, part 1: why Hamiltonian dynamics
forbids dissipation", synchronized to the narration track (metriplectic_1.srt).

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh metriplectic_scenes.py M0
    ...                       or: ./render_all.sh

Scene map (script section -> scene -> narration cues in metriplectic_1.srt)
    M0     title card + outline + the puzzle                cues   1-  2
    M1     Example: baseball with linear drag               cues   3-  8
    M1_1   p = m xdot, p(t), the "Hamiltonian", mismatch     cues   9- 12
    M1_2   H != E, the reverse-engineering problem          cues  13- 17
    M2     Why?  Hamiltonian = function + structure         cues  18- 21
    M3     (M, omega), Darboux, X_H, J                       cues  22- 25
    M3_1   Poisson bracket properties, Jacobi <-> d omega    cues  26- 28
    M3_2   Poisson manifolds, Casimirs, symplectic leaves    cues  29- 37
    M4     Obstruction 1: energy                             cues  38- 42
    M4_1   Obstruction 2: volume (Cartan, Liouville)         cues  43- 45
    M4_2   Obstruction 3: spectrum                           cues  46- 50
    M5     Loophole 1: hide it in time (Caldirola-Kanai)     cues  51- 61
    M6     Loophole 2: first integral, Lie-Poisson           cues  62- 69
    M6_1   ... and its price; "the real lesson"              cues  70- 83
    M7     Reversibility: Theta, parity split                cues  84- 93
    M7_1   group vs semigroup                                cues  94- 98
    M8     Entropy I: Caldeira-Leggett, GLE, FDT             cues  99-109
    M8_1   Entropy II: Gibbs entropy, coarse graining        cues 110-119
    M9     Metriplectic: two brackets, requirements          cues 120-128
    M9_1   degeneracy conditions, E and S laws               cues 129-135
    M9_2   geometric picture, parity, the name               cues 136-141
    M10    The baseball, metriplectic version                cues 142-148
    M10_1  entropy production, LaSalle, attractor            cues 149-156
    M11    Conclusion                                        cues 157-162
    M11_1  Look ahead                                        cues 163-167

--------------------------------------------------------------------------
AUDIO SYNC (same machinery as the Kerr / DG projects)
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
      [sync] M4: clip 34.48s / audio 34.48s | late beats: cue 40 (+0.35s)
  Fix a late beat by shortening a run_time or moving the cue earlier.

Debug aid: set SNAP=1 and every self.snap("name") call writes
snaps/<scene>_<name>.png so layouts can be checked without watching.

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

# --------------------------------------------------------------------------
# Narration timing
# --------------------------------------------------------------------------
Cue = namedtuple("Cue", "start end text")


def _find_srt():
    cands = [os.environ.get("METRIPLECTIC_SRT")]
    try:
        cands.append(Path(__file__).with_name("metriplectic_1.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "metriplectic_1.srt",
              Path("~/Music/metriplectic_1.srt").expanduser(),
              Path("/mnt/user-data/uploads/metriplectic_1.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError("metriplectic_1.srt not found (set METRIPLECTIC_SRT=/path/to/file)")


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
# start of the next scene's first cue (M0 starts at t = 0).
PLAN = [("M0", 1), ("M1", 3), ("M1_1", 9), ("M1_2", 13), ("M2", 18), ("M3", 22),
        ("M3_1", 26), ("M3_2", 29), ("M4", 38), ("M4_1", 43), ("M4_2", 46),
        ("M5", 51), ("M6", 62), ("M6_1", 70), ("M7", 84), ("M7_1", 94),
        ("M8", 99), ("M8_1", 110), ("M9", 120), ("M9_1", 129), ("M9_2", 136),
        ("M10", 142), ("M10_1", 149), ("M11", 157), ("M11_1", 163)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="metriplectic_1.tex"):
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


def make_colormap(stops):
    """stops: list of hex colours -> function f(x in [0,1]) -> uint8 RGB."""
    rgb = [np.array(color_to_rgb(s)) for s in stops]

    def f(x):
        x = float(np.clip(x, 0, 1)) * (len(rgb) - 1)
        i = min(int(x), len(rgb) - 2)
        return (255 * ((1 - (x - i)) * rgb[i] + (x - i) * rgb[i + 1])).astype(np.uint8)
    return f


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


# --------------------------------------------------------------------------
# Phase-space helpers
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


def flow_drag(P, t, b=0.6):
    """Drag flow (m = 1):  xdot = p,  pdot = -b p   (area shrinks as e^{-bt})."""
    e = math.exp(-b * t)
    return np.stack([P[:, 0] + P[:, 1] * (1 - e) / b, P[:, 1] * e], 1)


def flow_ck(P, t, b=0.6):
    """Caldirola-Kanai flow in (x, p_c):  xdot = p_c e^{-bt},  p_c = const (area preserving)."""
    e = math.exp(-b * t)
    return np.stack([P[:, 0] + P[:, 1] * (1 - e) / b, P[:, 1]], 1)


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


def blob_panel(center, w, h, tracker, flow, base, color, sc=1.0, xl="q", yl="p", op=0.35):
    """Framed plane + a live blob transported by `flow(base, tracker value)`.
    Returns (static_group, blob_mobject, area_fn)."""
    c = np.array(center, dtype=float)
    frame = phase_frame(c, w, h, xl, yl)
    blob = always_redraw(lambda: blob_poly(flow(base, tracker.get_value()), c, sc, color, op))
    area = lambda: shoelace(flow(base, tracker.get_value()))
    return frame, blob, area


def area_label(text, fn, color, pos, size=24, places=2):
    t = Text(text, font_size=size, color=color).move_to(pos)
    num = always_redraw(lambda: DecimalNumber(fn(), num_decimal_places=places, font_size=size + 4,
                                              color=color).next_to(t, RIGHT, buff=0.15))
    return t, num


def orbit_dot(center, rx, ry, rate=1.6, color=YELLOW, radius=0.07, phase0=0.0):
    ph = ValueTracker(phase0)
    ph.add_updater(lambda m, dt: m.increment_value(rate * dt))
    c = np.array(center, dtype=float)
    dot = always_redraw(lambda: Dot(c + [rx * math.cos(ph.get_value()), ry * math.sin(ph.get_value()), 0],
                                    radius=radius, color=color))
    return ph, dot


def baseball(r=0.22):
    b = Circle(radius=r, color=WHITE, stroke_width=2, fill_color=WHITE, fill_opacity=1)
    s1 = Arc(radius=r * 0.78, start_angle=2.2, angle=1.8, color=RED, stroke_width=2)
    s2 = Arc(radius=r * 0.78, start_angle=-0.9, angle=1.8, color=RED, stroke_width=2)
    return VGroup(b, s1, s2)


def warped_grid(center, half=1.6, n=7, warp=None, color=BLUE_D, samples=40, sw=2):
    """Lines of a square grid pushed through warp(x, y) -> (x', y')."""
    warp = warp or (lambda x, y: (x, y))
    g = VGroup()
    ts = np.linspace(-half, half, samples)
    for v in np.linspace(-half, half, n):
        for horizontal in (True, False):
            pts = []
            for t in ts:
                x, y = (t, v) if horizontal else (v, t)
                wx, wy = warp(x, y)
                pts.append(np.array([center[0] + wx, center[1] + wy, 0]))
            m = VMobject(stroke_color=color, stroke_width=sw)
            m.set_points_as_corners(pts)
            g.add(m)
    return g


def lattice(center, radius=1.7, step=0.38, color=BLUE_D):
    pts = VGroup()
    k = int(radius / step)
    for i in range(-k, k + 1):
        for j in range(-k, k + 1):
            x, y = i * step, j * step
            if x * x + y * y <= radius * radius:
                pts.add(Dot([center[0] + x, center[1] + y, 0], radius=0.035, color=color))
    return pts


def patch(center, rx=2.2, ry=1.5):
    """A smooth blobby 'manifold patch'."""
    th = np.linspace(0, TAU, 60, endpoint=False)
    r = 1 + 0.10 * np.cos(3 * th + 0.5) + 0.07 * np.sin(5 * th)
    pts = [np.array([center[0] + rx * rr * math.cos(t), center[1] + ry * rr * math.sin(t), 0.0])
           for rr, t in zip(r, th)]
    m = VMobject(color=BLUE_C, stroke_width=3, fill_color=BLUE_E, fill_opacity=0.25)
    m.set_points_smoothly(pts + [pts[0]])
    return m


def leaf_stack(center, ys=(-1.6, -0.55, 0.5, 1.55), w=3.7, d=0.6, h=0.6, color=BLUE_D, op=0.2):
    """Stack of skewed parallelograms (symplectic leaves seen in perspective)."""
    leaves = VGroup()
    for y in ys:
        c = np.array([center[0], center[1] + y, 0.0])
        poly = Polygon(c + [-w / 2, -h / 2, 0], c + [w / 2 - d * 0.2, -h / 2, 0],
                       c + [w / 2 + d * 0.4, h / 2, 0], c + [-w / 2 + d * 0.4, h / 2, 0],
                       color=color, stroke_width=2, fill_color=color, fill_opacity=op)
        leaves.add(poly)
    return leaves


# ==========================================================================
# M0 -- title card + outline + the puzzle   (cues 1-2)
# ==========================================================================
class M0(SyncedScene):
    AUDIO_START, AUDIO_END = START["M0"], END["M0"]
    STARS = (40, 3)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Metriplectic dynamics, part 1: why Hamiltonians forbid dissipation", FG)],
                          font_size=26)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: "Hello and welcome to this first video in the series on
        # Metriplectic Dynamics."
        self.at(1, Write(title), run_time=1.2)

        names = ["Introduction", "Example", "Why?", "What a Hamiltonian system is",
                 "Why this disallows dissipation", "Loophole 1: hide it in time",
                 "Loophole 2: give up canonical structure", "Reversibility", "Entropy",
                 "Metriplectic dynamics", "Conclusion and a look ahead"]
        mk = lambda s: code_line([("** ", FG_DIM), (s, FG)], font_size=20)
        colL = VGroup(*[mk(s) for s in names[:6]]).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        colR = VGroup(*[mk(s) for s in names[6:]]).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        cols = VGroup(colL, colR).arrange(RIGHT, buff=0.9, aligned_edge=UP)
        cols.next_to(title, DOWN, buff=0.6, aligned_edge=LEFT).shift(RIGHT * 0.4)
        clamp_width(cols)
        self.sync(CUES[1].start + 1.6)
        self.play(LaggedStart(*[FadeIn(r, shift=RIGHT * 0.15) for r in [*colL, *colR]],
                              lag_ratio=0.1), run_time=2.4)

        # cue 2: "...motivate them by looking at why Hamiltonian dynamics generally
        # does not permit dissipative or irreversible dynamics and how modern
        # advances get around this."
        self.at(2, FadeOut(cols), run_time=0.5)
        cH = card("Hamiltonian dynamics\nconserves energy\nreversible", BLUE, 24, 4.6)
        cD = card("dissipation\nair resistance, friction\nirreversible", ORANGE, 24, 4.6)
        pair = VGroup(cH, cD).arrange(RIGHT, buff=1.8).move_to([0, 0.5, 0])
        q = Text("?", font_size=80, color=YELLOW).move_to(pair.get_center())
        self.play(FadeIn(cH, shift=RIGHT * 0.2), FadeIn(cD, shift=LEFT * 0.2), run_time=0.8)
        self.play(FadeIn(q, scale=0.5), run_time=0.5)
        mp = chip("metriplectic dynamics", YELLOW, 28, fill=0.12).move_to([0, -2.1, 0])
        a1 = Arrow(cH.get_bottom() + DOWN * 0.05, mp.get_left() + LEFT * 0.05, buff=0.1, color=BLUE)
        a2 = Arrow(cD.get_bottom() + DOWN * 0.05, mp.get_right() + RIGHT * 0.05, buff=0.1, color=ORANGE)
        self.frac(2, 0.6, FadeIn(mp, shift=UP * 0.2), GrowArrow(a1), GrowArrow(a2), run_time=0.9)
        self.snap("puzzle")
        self.finish()


# ==========================================================================
# M1 -- Example: a baseball with linear drag  (cues 3-8)
# ==========================================================================
class M1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M1"], END["M1"]

    def construct(self):
        header = hdr("Example")
        # cue 3: "I mean this seems like a very strange thing."
        self.at(3, Write(header), run_time=0.6)
        q = Text("?", font_size=110, color=YELLOW).move_to([0, 0.2, 0])
        self.sync(CUES[3].start + 0.7)
        self.play(FadeIn(q, scale=0.6), run_time=0.6)

        # cue 4: Hamiltonian dynamics is a foundation; dissipation is basic physics too
        c_h = chip("Hamiltonian dynamics: a foundation of physics", BLUE, 28)
        c_d = chip("air resistance, dissipation: some of the most basic physics", ORANGE, 28)
        pair = VGroup(c_h, c_d).arrange(DOWN, buff=0.7).move_to([0, 0.2, 0])
        clamp_width(pair)
        self.at(4, FadeOut(q), FadeIn(c_h, shift=RIGHT * 0.2), run_time=0.7)
        self.frac(4, 0.5, FadeIn(c_d, shift=RIGHT * 0.2), run_time=0.7)

        # cue 5: simplify -- linear drag, no gravity
        self.at(5, FadeOut(pair), run_time=0.5)
        c_lin = chip("linear drag", ORANGE, 28)
        c_ng = chip("no gravity", TEAL, 28)
        row = VGroup(c_lin, c_ng).arrange(RIGHT, buff=0.9).move_to([0, 1.3, 0])
        self.play(FadeIn(c_lin, shift=UP * 0.2), run_time=0.5)
        self.frac(5, 0.6, FadeIn(c_ng, shift=UP * 0.2), run_time=0.5)

        # cue 6: a baseball released at speed v;  force = -b * velocity
        y0 = -1.9
        track = Line([-6.2, y0 - 0.32, 0], [6.2, y0 - 0.32, 0], color=GREY_B, stroke_width=2)
        xpos = lambda t: -5.4 + 9.6 * (1 - math.exp(-0.5 * t))
        ticks = VGroup(*[Line([xpos(k), y0 - 0.4, 0], [xpos(k), y0 - 0.24, 0], color=GREY_C, stroke_width=2)
                         for k in range(0, 9)])
        tr = ValueTracker(0.0)
        ball = always_redraw(lambda: baseball().move_to([xpos(tr.get_value()), y0, 0]))
        varrow = always_redraw(lambda: Arrow(
            ball.get_center() + RIGHT * 0.3,
            ball.get_center() + RIGHT * (0.3 + max(1.8 * math.exp(-0.5 * tr.get_value()), 0.08)),
            buff=0, color=YELLOW, stroke_width=4, max_tip_length_to_length_ratio=0.5))
        v_lab = tex(r"v", 34, YELLOW).move_to([xpos(0) + 1.2, y0 + 0.4, 0])
        eq1 = tex(r"m\ddot{x} = -b\,\dot{x}", 46)
        eq1.move_to([0, 1.3, 0])
        self.at(6, FadeOut(row), FadeIn(track), FadeIn(ticks), run_time=0.5)
        self.add(ball, varrow)
        self.play(FadeIn(v_lab), Write(eq1), run_time=1.2)
        self.frac(6, 0.3, FadeOut(v_lab), tr.animate.set_value(7.0), run_time=6.0, rate_func=linear)
        self.snap("ball")

        # cue 7: the rest is trivial -- solve it
        eq2 = tex(r"v(t) = v_0\,e^{-bt/m}", 44)
        eq2.next_to(eq1, DOWN, buff=0.5)
        self.at(7, Write(eq2), run_time=1.4)

        # cue 8: velocity -> kinetic energy over time, total energy
        eq3 = tex(r"E(t) = \tfrac12 m\,v(t)^2 = \tfrac12 m v_0^2\,e^{-2bt/m}", 40, ORANGE)
        eq3.next_to(eq2, DOWN, buff=0.5)
        self.at(8, Write(eq3), run_time=1.8)
        self.snap("energy")
        self.finish(keep=[header])


# ==========================================================================
# M1_1 -- momentum, solution, the "Hamiltonian", the mismatch  (cues 9-12)
# ==========================================================================
class M1_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M1_1"], END["M1_1"]

    def construct(self):
        header = hdr("Example")
        self.add(header)

        # cue 9: define p = m xdot, so pdot = -(b/m) p
        e1 = at_left(tex(r"p = m\dot{x}", 38), 1.8)
        e2 = at_left(tex(r"\dot{p} = -\frac{b}{m}\,p", 38), 0.85)
        self.at(9, Write(e1), run_time=1.0)
        self.frac(9, 0.5, Write(e2), run_time=1.2)

        # cue 10: solution p(t); no potential energy -> the Hamiltonian
        e3 = at_left(tex(r"p(t) = p_0\,e^{-bt/m}", 38), -0.1)
        e4 = at_left(tex(r"H(q,p,t) = \frac{p^2}{2m}\,e^{-bt/m}", 38, YELLOW), -1.15)
        n4 = note("no potential energy:  V(x) = 0", 20).next_to(e4, DOWN, buff=0.25, aligned_edge=LEFT)
        self.at(10, Write(e3), run_time=1.2)
        self.frac(10, 0.5, Write(e4), FadeIn(n4), run_time=1.6)
        self.snap("hamiltonian")

        # cue 11: Hamilton's equations: position is trivial, but p never changes
        r1 = tex(r"\dot{x} = \frac{\partial H}{\partial p} = \frac{p}{m}\,e^{-bt/m}", 30)
        r2 = tex(r"\dot{p} = -\frac{\partial H}{\partial q} = 0", 30, RED)
        rg = VGroup(r1, r2).arrange(DOWN, buff=0.4, aligned_edge=LEFT).move_to([3.5, 1.1, 0])
        r3 = note("momentum never changes?!\nbut the ball clearly slows down", 20, RED).next_to(rg, DOWN, buff=0.4)
        self.at(11, Write(r1), run_time=1.2)
        self.play(Write(r2), run_time=1.0)
        self.play(FadeIn(r3), run_time=0.5)

        # cue 12: compare with the Newtonian picture
        self.at(12, FadeOut(rg), FadeOut(r3), run_time=0.4)
        ax1 = Axes(x_range=[0, 6, 1], y_range=[0, 1.25, 0.5], x_length=5.2, y_length=2.0,
                   axis_config={"include_tip": False, "font_size": 14}).move_to([3.7, 1.05, 0])
        ax2 = Axes(x_range=[0, 6, 1], y_range=[0, 1.25, 0.5], x_length=5.2, y_length=2.0,
                   axis_config={"include_tip": False, "font_size": 14}).move_to([3.7, -1.9, 0])
        t1 = note("momentum", 17).next_to(ax1, UP, buff=0.05).align_to(ax1, LEFT)
        t2 = note("energy", 17).next_to(ax2, UP, buff=0.05).align_to(ax2, LEFT)
        pc = ax1.plot(lambda t: 1.0, x_range=[0, 6], color=ORANGE)
        pp = ax1.plot(lambda t: math.exp(-0.5 * t), x_range=[0, 6], color=GREEN)
        l_pc = note("p in H: constant", 16, ORANGE).next_to(ax1.c2p(6, 1.0), DOWN, buff=0.12).shift(LEFT * 1.1)
        l_pp = note("m x-dot: decays", 16, GREEN).next_to(ax1.c2p(3.0, math.exp(-1.5)), UP, buff=0.2).shift(RIGHT * 0.4)
        hh = ax2.plot(lambda t: math.exp(-0.5 * t), x_range=[0, 6], color=ORANGE)
        ee = ax2.plot(lambda t: math.exp(-1.0 * t), x_range=[0, 6], color=GREEN)
        l_hh = note("H(t)", 16, ORANGE).move_to(ax2.c2p(2.0, math.exp(-1.0)) + [0.55, 0.3, 0])
        l_ee = note("Newtonian energy", 16, GREEN).move_to(ax2.c2p(3.0, math.exp(-3.0)) + [1.0, 0.45, 0])
        mm1 = note("✗ mismatch", 20, RED).move_to([5.7, 2.45, 0])
        mm2 = note("✗ mismatch", 20, RED).move_to([5.7, -0.45, 0])
        self.play(Create(ax1), FadeIn(t1), Create(pc), Create(pp), FadeIn(l_pc), FadeIn(l_pp), run_time=1.6)
        self.frac(12, 0.3, FadeIn(mm1), run_time=0.3)
        self.frac(12, 0.4, Create(ax2), FadeIn(t2), Create(hh), Create(ee), FadeIn(l_hh), FadeIn(l_ee), run_time=1.6)
        self.frac(12, 0.8, FadeIn(mm2), run_time=0.3)
        self.snap("mismatch")
        self.finish()


# ==========================================================================
# M1_2 -- H != E, reverse-engineering, fluids  (cues 13-17)
# ==========================================================================
class M1_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["M1_2"], END["M1_2"]

    def construct(self):
        header = hdr("Example")
        self.add(header)

        # cue 13: "Now, when we look at these mismatches,"
        t13 = Text("look at these mismatches...", font_size=34, color=YELLOW).move_to([0, 1.5, 0])
        self.at(13, FadeIn(t13), run_time=0.5)

        # cue 14: the Hamiltonian is not the energy, the conjugate momentum is not momentum
        l1 = tex(r"H \;\neq\; E_{\rm Newton}", 46, ORANGE)
        l2 = tex(r"p_{\rm conj} \;\neq\; m\dot{x}", 46, ORANGE)
        ls = VGroup(l1, l2).arrange(RIGHT, buff=1.4).move_to([0, 1.5, 0])
        self.at(14, ReplacementTransform(t13, ls), run_time=0.9)
        sub = note("the Hamiltonian is not the energy, and the conjugate momentum is not the momentum", 20)
        sub.next_to(ls, DOWN, buff=0.35)
        self.play(FadeIn(sub), run_time=0.5)

        # cue 15: true -- but we only got it right because we already knew the answer
        bH = chip("Hamiltonian H", BLUE, 24)
        bE = chip("equations of motion", BLUE, 24)
        bS = chip("solution  p(t)", GREEN, 24)
        flow = VGroup(bH, bE, bS).arrange(RIGHT, buff=1.1).move_to([0, -0.7, 0])
        fa = VGroup(Arrow(bH.get_right(), bE.get_left(), buff=0.08, color=GREY_B),
                    Arrow(bE.get_right(), bS.get_left(), buff=0.08, color=GREY_B))
        self.at(15, FadeIn(flow), FadeIn(fa), run_time=0.8)
        back = CurvedArrow(bS.get_top() + DOWN * 0.05, bH.get_top() + DOWN * 0.05, angle=TAU / 8,
                           color=ORANGE, stroke_width=5)
        bl = note("how we actually got H: we already knew the solution", 20, ORANGE).next_to(back, DOWN, buff=1.0)
        self.frac(15, 0.4, Create(back), FadeIn(bl), run_time=1.0)

        # cue 16: so using the Hamiltonian this way is useless
        use = chip("unphysical phase space  →  Hamiltonian mostly useless", RED, 24)
        use.move_to([0, -2.95, 0])
        self.at(16, FadeIn(use, shift=UP * 0.2), run_time=0.6)
        self.snap("useless")

        # cue 17: in fluid dynamics this goes much further and breaks the whole structure
        self.at(17, *[FadeOut(m) for m in [ls, sub, flow, fa, back, bl, use]], run_time=0.5)
        cA, cB = np.array([-3.4, -0.3, 0.0]), np.array([3.4, -0.3, 0.0])
        gA = warped_grid(cA, half=1.4, n=7, warp=lambda x, y: (x + 0.12 * math.sin(2 * y), y + 0.1 * math.sin(2.3 * x)),
                         color=TEAL)
        gB = warped_grid(cB, half=1.4, n=7,
                         warp=lambda x, y: (x + 0.55 * math.sin(3 * y) * math.cos(2 * x),
                                            y + 0.5 * math.sin(3 * x + y)), color=RED)
        tA = note("drag: a little confusing", 22, TEAL).next_to(gA, UP, buff=0.3)
        tB = note("fluids: the whole Hamiltonian\nstructure breaks", 22, RED).next_to(gB, UP, buff=0.3)
        self.play(Create(gA), FadeIn(tA), run_time=1.2)
        self.frac(17, 0.45, Create(gB), FadeIn(tB), run_time=1.6)
        self.snap("fluids")
        self.finish(keep=[header])


# ==========================================================================
# M2 -- Why?  (cues 18-21)
# ==========================================================================
class M2(SyncedScene):
    AUDIO_START, AUDIO_END = START["M2"], END["M2"]

    def construct(self):
        header = hdr("Why?")
        # cue 18: "Now explain why this actually happens."
        self.at(18, Write(header), run_time=0.6)
        big = Text("Why does this happen?", font_size=50, color=YELLOW).move_to([0, 0.4, 0])
        self.sync(CUES[18].start + 0.5)
        self.play(FadeIn(big, scale=0.9), run_time=0.8)

        # cue 19: a Hamiltonian is not just a function
        fn = tex(r"H:\ M \to \mathbb{R}", 56).move_to([0, 0.8, 0])
        fl = note("just a function?  no.", 26, RED).next_to(fn, DOWN, buff=0.5)
        self.at(19, FadeOut(big), FadeIn(fn), run_time=0.5)
        self.play(FadeIn(fl), run_time=0.4)

        # cue 20: a function TOGETHER WITH a geometric structure -- which cannot accommodate dissipation
        self.at(20, FadeOut(fn), FadeOut(fl), run_time=0.4)
        cH = card("a function\nH", BLUE, 28, 3.2)
        cS = card("a geometric structure\non phase space\n(M, ω)", TEAL, 28, 4.4)
        plus = tex(r"+", 56)
        trio = VGroup(cH, plus, cS).arrange(RIGHT, buff=0.6).move_to([0, 0.9, 0])
        self.play(FadeIn(cH), run_time=0.5)
        self.play(FadeIn(plus), FadeIn(cS), run_time=0.7)
        bad = Text("✗  this structure cannot accommodate dissipation", font_size=28, color=RED)
        bad.next_to(trio, DOWN, buff=0.7)
        self.frac(20, 0.55, FadeIn(bad, shift=UP * 0.2), run_time=0.6)

        # cue 21: energy conservation and phase-space volume: two shadows of one fact
        self.at(21, *[FadeOut(m) for m in [trio, bad]], run_time=0.5)
        core = card("one structure", YELLOW, 30, 3.6).move_to([0, 1.5, 0])
        c1 = chip("energy is conserved", ORANGE, 26).move_to([-3.6, -1.2, 0])
        c2 = chip("phase-space volume is conserved", BLUE, 26).move_to([3.4, -1.2, 0])
        a1 = Arrow(core.get_bottom() + LEFT * 0.7, c1.get_top(), buff=0.1, color=ORANGE)
        a2 = Arrow(core.get_bottom() + RIGHT * 0.7, c2.get_top(), buff=0.1, color=BLUE)
        sh = note("two separate-looking facts: shadows of the same one", 22, GREY_B).move_to([0, -2.5, 0])
        self.play(FadeIn(core), run_time=0.5)
        self.play(GrowArrow(a1), FadeIn(c1), run_time=0.6)
        self.play(GrowArrow(a2), FadeIn(c2), run_time=0.6)
        self.frac(21, 0.7, FadeIn(sh), run_time=0.5)
        self.snap("shadows")
        self.finish()


# ==========================================================================
# M3 -- manifold, symplectic form, Darboux, X_H, J  (cues 22-25)
# ==========================================================================
class M3(SyncedScene):
    AUDIO_START, AUDIO_END = START["M3"], END["M3"]

    def construct(self):
        header = hdr("What a Hamiltonian System Actually Is")
        # cue 22: smooth manifold M of dimension 2n with a symplectic form omega,
        # a closed and non-degenerate two-form
        self.at(22, Write(header), run_time=0.6)
        C = np.array([3.7, -0.35, 0.0])
        pat = patch(C)
        wgrid = warped_grid(C, half=1.2, n=5, warp=lambda x, y: (x + 0.18 * math.sin(2 * y), y + 0.15 * math.sin(2.3 * x)),
                            color=TEAL)
        m_lab = tex(r"M^{2n}", 36, BLUE_C).move_to(C + [-1.9, 1.5, 0])
        e1 = at_left(tex(r"(M^{2n},\ \omega)", 40), 1.75)
        n1 = at_left(note("ω: a closed, non-degenerate two-form", 20, GREY_B), 1.1)
        self.sync(CUES[22].start + 1.0)
        self.play(Create(pat), FadeIn(m_lab), Write(e1), run_time=1.6)
        self.play(Create(wgrid), run_time=1.2)
        self.frac(22, 0.5, FadeIn(n1), run_time=0.5)
        c1 = tex(r"\mathrm{d}\omega = 0", 30, YELLOW)
        c2 = tex(r"\omega\ \text{non-degenerate}", 30, YELLOW)
        cg = VGroup(c1, c2).arrange(RIGHT, buff=0.8)
        at_left(cg, 0.4)
        self.frac(22, 0.7, FadeIn(c1, shift=UP * 0.15), run_time=0.4)
        self.frac(22, 0.85, FadeIn(c2, shift=UP * 0.15), run_time=0.4)

        # cue 23: Darboux -- local coordinates in which omega is the standard sum
        sgrid = warped_grid(C, half=1.2, n=5, color=TEAL)
        e2 = at_left(tex(r"\omega = \sum_{i=1}^{n}\mathrm{d}q^i\wedge\mathrm{d}p_i", 34), -0.55)
        n2 = at_left(note("Darboux: always possible locally", 18, GREY_B), -1.3)
        ql = tex(r"q", 28, TEAL).move_to(C + [1.75, -0.3, 0])
        pl = tex(r"p", 28, TEAL).move_to(C + [0.3, 1.45, 0])
        self.at(23, ReplacementTransform(wgrid, sgrid), Write(e2), run_time=1.6)
        self.play(FadeIn(n2), FadeIn(ql), FadeIn(pl), run_time=0.5)
        self.snap("darboux")

        # cue 24: H generates a vector field X_H
        arrows = VGroup()
        for q_ in np.linspace(-1.0, 1.0, 5):
            for p_ in np.linspace(-1.0, 1.0, 5):
                if abs(q_) < 1e-9 and abs(p_) < 1e-9:
                    continue
                s = C + [q_, p_, 0]
                arrows.add(Arrow(s, s + 0.3 * np.array([p_, -q_, 0.0]), buff=0, color=ORANGE, stroke_width=3,
                                 max_tip_length_to_length_ratio=0.45))
        e3a = at_left(tex(r"\iota_{X_H}\,\omega = \mathrm{d}H", 32), -1.85)
        e3b = at_left(tex(r"\dot q^i = \frac{\partial H}{\partial p_i},\qquad \dot p_i = -\frac{\partial H}{\partial q^i}", 30), -2.65)
        self.at(24, Write(e3a), LaggedStart(*[GrowArrow(a) for a in arrows], lag_ratio=0.02), run_time=1.6)
        self.frac(24, 0.45, Write(e3b), run_time=1.6)
        self.snap("xh")

        # cue 25: the inverse of omega is an antisymmetric bivector J -> Poisson bracket
        self.at(25, FadeOut(e3a), FadeOut(e3b), FadeOut(arrows), run_time=0.5)
        j1 = at_left(tex(r"J^{ij} = (\omega^{-1})^{ij}\quad\text{antisymmetric bivector}", 30), -1.8)
        j2 = at_left(tex(r"\{f,g\} = \partial_i f\,J^{ij}\,\partial_j g", 36, YELLOW), -2.6)
        j3 = tex(r"\dot f = \{f,H\}", 36, YELLOW).move_to([3.7, -2.6, 0])
        self.play(Write(j1), run_time=1.6)
        self.frac(25, 0.4, Write(j2), run_time=1.6)
        self.frac(25, 0.75, Write(j3), run_time=1.2)
        self.snap("bracket")
        self.finish(keep=[header])


# ==========================================================================
# M3_1 -- properties of the bracket  (cues 26-28)
# ==========================================================================
class M3_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M3_1"], END["M3_1"]

    def construct(self):
        header = hdr("What a Hamiltonian System Actually Is")
        self.add(header)

        # cue 26: the bracket has three properties
        t = Text("the bracket has three properties", font_size=30, color=YELLOW)
        t.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(26, FadeIn(t), run_time=0.5)

        rows = [
            ("antisymmetric", BLUE, r"\{f,g\} = -\{g,f\}"),
            ("Leibniz rule", TEAL, r"\{f,gh\} = \{f,g\}\,h + g\,\{f,h\}"),
            ("Jacobi identity", ORANGE, r"\{f,\{g,h\}\} + \{g,\{h,f\}\} + \{h,\{f,g\}\} = 0"),
        ]
        grp = VGroup()
        for name, col, eq in rows:
            r = VGroup(Text(name, font_size=26, color=col), tex(eq, 36))
            r[0].set_width(2.7) if r[0].width > 2.7 else None
            grp.add(r)
        ys = [1.1, -0.15, -1.4]
        for r, y in zip(grp, ys):
            r[0].move_to([-4.6, y, 0])
            r[1].next_to(r[0], RIGHT, buff=0.6)
        # cue 27: antisymmetric, Leibniz, Jacobi in turn
        self.at(27, FadeIn(grp[0][0]), Write(grp[0][1]), run_time=1.0)
        self.frac(27, 0.35, FadeIn(grp[1][0]), Write(grp[1][1]), run_time=1.2)
        self.frac(27, 0.65, FadeIn(grp[2][0]), Write(grp[2][1]), run_time=1.6)

        # cue 28: when J is invertible, Jacobi is exactly d omega = 0
        eq = tex(r"J\ \text{invertible:}\qquad \text{Jacobi}\ \Longleftrightarrow\ \mathrm{d}\omega = 0", 38, YELLOW)
        eq.move_to([0, -2.7, 0])
        box = SurroundingRectangle(grp[2], color=ORANGE, buff=0.15)
        self.at(28, Create(box), Write(eq), run_time=1.6)
        self.snap("jacobi")
        self.finish(keep=[header])


# ==========================================================================
# M3_2 -- Poisson manifolds, Casimirs, symplectic leaves  (cues 29-37)
# ==========================================================================
class M3_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["M3_2"], END["M3_2"]

    def construct(self):
        header = hdr("What a Hamiltonian System Actually Is")
        self.add(header)

        # cue 29: relax one assumption
        t29 = at_left(Text("now relax one assumption", font_size=30, color=YELLOW), 1.8)
        self.at(29, FadeIn(t29), run_time=0.5)

        # cues 30-31: let J be degenerate, as long as it stays antisymmetric + Jacobi
        j = at_left(tex(r"J^{ij}(z)\ \text{may be degenerate}", 36), 0.9)
        self.at(30, Write(j), run_time=1.2)
        mat = tex(r"J=\begin{pmatrix}0&1&0\\-1&0&0\\0&0&0\end{pmatrix}", 36).move_to([3.9, 0.9, 0])
        mn = note("rank 2 < 3: degenerate", 18, RED).next_to(mat, DOWN, buff=0.2)
        self.play(Write(mat), FadeIn(mn), run_time=1.2)
        keep = at_left(note("…still antisymmetric, still satisfying Jacobi", 22, GREY_B), 0.15)
        self.at(31, FadeIn(keep), run_time=0.6)

        # cue 32: a Poisson manifold
        pm = chip("Poisson manifold", YELLOW, 28, fill=0.1)
        at_left(pm, -0.9, x=LEFT_X)
        self.at(32, FadeIn(pm, shift=RIGHT * 0.2), run_time=0.6)
        self.snap("poisson")

        # cue 33: Casimir functions: {C, f} = 0 for every f
        self.at(33, *[FadeOut(m) for m in [t29, j, keep, pm, mat, mn]], run_time=0.5)
        cl = at_left(Text("Casimir functions C", font_size=30, color=ORANGE), 1.8)
        ce = at_left(tex(r"\{C, f\} = 0\quad\text{for every } f", 38), 0.85)
        self.play(FadeIn(cl), run_time=0.4)
        self.play(Write(ce), run_time=1.4)
        # cue 34: equivalently J grad C = 0
        ce2 = at_left(tex(r"\Longleftrightarrow\quad J\nabla C = 0", 38, YELLOW), -0.05)
        self.at(34, Write(ce2), run_time=1.2)

        # cue 35: constant along Hamiltonian motion; level sets slice M into leaves
        k = at_left(tex(r"\dot C = \{C,H\} = 0", 36), -1.1)
        lv = at_left(note("level sets of C slice M into\nsymplectic leaves", 22), -2.0)
        C0 = np.array([3.8, -0.15, 0.0])
        leaves = leaf_stack(C0)
        labs = VGroup(*[tex(rf"C=c_{i + 1}", 22, GREY_B).move_to(l.get_right() + [0.65, -0.05, 0])
                        for i, l in enumerate(leaves)])
        self.at(35, Write(k), FadeIn(lv), LaggedStart(*[FadeIn(l, shift=UP * 0.15) for l in leaves], lag_ratio=0.2),
                FadeIn(labs), run_time=1.8)

        # cue 36: each leaf is a genuine symplectic manifold; flow never leaves its leaf
        sel = leaves[1]
        ph, dot = orbit_dot(sel.get_center() + [0.1, 0, 0], 1.05, 0.2, rate=1.7)
        esc = DashedLine(sel.get_center() + [0.9, 0.1, 0], sel.get_center() + [0.9, 1.05, 0], color=RED)
        esc_x = Text("✗", font_size=30, color=RED).move_to(esc.get_end() + [0, 0.25, 0])
        self.at(36, sel.animate.set_fill(BLUE, opacity=0.45), run_time=0.6)
        self.add(ph, dot)
        self.play(Create(esc), FadeIn(esc_x), run_time=0.7)
        sn = note("each leaf: a genuine symplectic manifold\nthe flow never leaves it", 20, GREEN)
        sn.scale(0.9).move_to([3.9, -2.55, 0])
        self.play(FadeIn(sn), run_time=0.4)

        # cue 37: keep this picture
        keepc = chip("keep this picture: metriplectic is built from it", YELLOW, 20)
        keepc.move_to([-2.9, -3.0, 0])
        self.at(37, FadeIn(keepc, shift=UP * 0.2), run_time=0.6)
        self.snap("leaves")
        self.finish()


# ==========================================================================
# M4 -- obstruction 1: energy  (cues 38-42)
# ==========================================================================
class M4(SyncedScene):
    AUDIO_START, AUDIO_END = START["M4"], END["M4"]

    def construct(self):
        header = hdr("Why This Disallows Dissipation")
        # cue 38: three independent obstructions, all following from the structure above
        self.at(38, Write(header), run_time=0.6)
        chips = VGroup(chip("1  Energy", ORANGE, 30), chip("2  Volume", BLUE, 30),
                       chip("3  Spectrum", TEAL, 30)).arrange(RIGHT, buff=0.9).move_to([0, 0.7, 0])
        self.sync(CUES[38].start + 1.2)
        self.play(LaggedStart(*[FadeIn(c, shift=UP * 0.2) for c in chips], lag_ratio=0.35), run_time=1.6)
        under = note("three independent obstructions - all three follow from (M, ω, J)", 24, YELLOW)
        under.next_to(chips, DOWN, buff=0.6)
        self.frac(38, 0.6, FadeIn(under), run_time=0.6)

        # cue 39-40: antisymmetry => dH/dt = 0
        self.at(39, FadeOut(chips), FadeOut(under), run_time=0.5)
        lab = at_left(Text("1  Energy", font_size=32, color=ORANGE), 1.8)
        e1 = at_left(tex(r"\dot H = \{H,H\}", 40), 0.85)
        self.play(FadeIn(lab), Write(e1), run_time=1.2)
        e2 = at_left(tex(r"= \nabla H\cdot J\,\nabla H = 0", 40), -0.1)
        n2 = at_left(note("J antisymmetric:  vᵀ J v = 0 for every v", 20), -0.8)
        self.at(40, Write(e2), run_time=1.4)
        self.frac(40, 0.6, FadeIn(n2), run_time=0.5)

        # cue 41: H is generally the energy => energy conserved
        e3 = at_left(tex(r"H = E\ \Rightarrow\ E\ \text{is conserved}", 36, YELLOW), -1.7)
        ax = Axes(x_range=[0, 6, 1], y_range=[0, 1.3, 0.5], x_length=5.4, y_length=3.2,
                  axis_config={"include_tip": False, "font_size": 16}).move_to([3.7, -0.35, 0])
        axl = VGroup(note("time", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     note("energy", 16).next_to(ax, UP, buff=0.05).align_to(ax, LEFT))
        flat = ax.plot(lambda t: 1.0, x_range=[0, 6], color=GREEN)
        fl = note("E(t) = const", 18, GREEN).next_to(ax.c2p(6, 1.0), DOWN, buff=0.15).shift(LEFT * 0.9)
        self.at(41, Write(e3), Create(ax), FadeIn(axl), run_time=1.2)
        self.play(Create(flat), FadeIn(fl), run_time=0.9)

        # cue 42: dissipation would need energy to drop => impossible
        dec = ax.plot(lambda t: math.exp(-0.5 * t), x_range=[0, 6], color=RED)
        dl = note("dissipation: E(t) falls", 18, RED).move_to(ax.c2p(3.5, 0.5))
        cross = Text("✗", font_size=44, color=RED).move_to(ax.c2p(5.4, 0.55))
        e4 = at_left(note("dissipation would need dE/dt < 0  -  impossible", 22, RED), -2.5)
        self.at(42, Create(dec), FadeIn(dl), run_time=1.0)
        self.play(FadeIn(cross), FadeIn(e4), run_time=0.5)
        self.snap("energy")
        self.finish(keep=[header])


# ==========================================================================
# M4_1 -- obstruction 2: volume  (cues 43-45)
# ==========================================================================
class M4_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M4_1"], END["M4_1"]

    def construct(self):
        header = hdr("Why This Disallows Dissipation")
        self.add(header)

        # right panel: a blob under the Hamiltonian flow -- area stays constant
        PC = np.array([3.9, -0.6, 0.0])
        t_ham = ValueTracker(0.0)
        t_ham.add_updater(lambda m, dt: m.increment_value(0.8 * dt))
        base_h = blob_base(0.9, 0.0, 1.0)
        fr_h, bl_h, ar_h = blob_panel(PC, 4.8, 4.2, t_ham, flow_rot, base_h, BLUE, sc=1.0)
        lab_h, num_h = area_label("area =", ar_h, BLUE, PC + [-0.4, -2.45, 0])

        lab = at_left(Text("2  Volume", font_size=32, color=BLUE), 1.8)
        # cue 43: Cartan's formula
        cartan = at_left(tex(r"\mathcal{L}_{X_H}\omega=\mathrm{d}\,\iota_{X_H}\omega+\iota_{X_H}\mathrm{d}\omega"
                             r"=\mathrm{d}^2H+0=0", 26), 0.8)
        clamp_width(cartan, 7.4)
        self.at(43, FadeIn(lab), Write(cartan), run_time=2.0)
        self.add(t_ham)
        self.play(Create(fr_h), FadeIn(bl_h), FadeIn(lab_h), FadeIn(num_h), run_time=1.0)
        flow_t = note("Hamiltonian flow: area stays constant", 18, BLUE).move_to(PC + [0, 2.35, 0])
        self.play(FadeIn(flow_t), run_time=0.4)

        # cue 44: flow preserves omega -> omega^n (Liouville), and every Poincare invariant
        r1 = at_left(tex(r"\phi_t^*\omega=\omega\ \Rightarrow\ \phi_t^*\,\omega^n=\omega^n", 32), -0.4)
        r2 = at_left(note("Liouville's theorem", 20, YELLOW), -1.0)
        r3 = at_left(tex(r"\int_\Sigma\omega^k\ \text{preserved: every Poincaré integral invariant}", 26), -1.8)
        clamp_width(r3, 7.4)
        self.at(44, Write(r1), run_time=1.4)
        self.play(FadeIn(r2), run_time=0.4)
        self.frac(44, 0.55, Write(r3), run_time=1.6)
        self.snap("hamiltonian")

        # cue 45: the drag flow is only conformally symplectic
        self.at(45, *[FadeOut(m) for m in [cartan, r1, r2, r3, flow_t, fr_h, bl_h, lab_h, num_h]], run_time=0.5)
        t_ham.clear_updaters()
        t_dr = ValueTracker(0.0)
        t_dr.add_updater(lambda m, dt: m.set_value((m.get_value() + 0.55 * dt) % 3.2))
        base_d = blob_base(-1.6, 0.9, 0.6)
        fr_d, bl_d, ar_d = blob_panel(PC, 4.8, 4.2, t_dr, flow_drag, base_d, ORANGE, sc=1.0, xl="x")
        lab_d, num_d = area_label("area =", ar_d, ORANGE, PC + [-0.4, -2.45, 0])
        flow_d = note("drag flow: area shrinks like e^(-bt/m)", 18, ORANGE).move_to(PC + [0, 2.35, 0])
        self.add(t_dr)
        d1 = at_left(tex(r"X=\frac pm\,\partial_x-\frac bm\,p\,\partial_p\quad\text{on}\quad\omega=\mathrm{d}x\wedge\mathrm{d}p", 26), 0.8)
        d2 = at_left(tex(r"\iota_X\omega=\tfrac pm\,\mathrm{d}p+\tfrac bm\,p\,\mathrm{d}x", 30), -0.1)
        d3 = at_left(tex(r"\mathcal{L}_X\omega=-\frac bm\,\omega\ \Rightarrow\ \phi_t^*\omega=e^{-bt/m}\,\omega", 30, ORANGE), -1.1)
        for d in (d1, d2, d3):
            clamp_width(d, 7.4)
        self.play(Create(fr_d), FadeIn(bl_d), FadeIn(lab_d), FadeIn(num_d), FadeIn(flow_d), Write(d1), run_time=1.6)
        self.frac(45, 0.35, Write(d2), run_time=1.2)
        self.frac(45, 0.6, Write(d3), run_time=1.6)
        cs = chip("only conformally symplectic - not symplectic", RED, 22)
        at_left(cs, -2.3)
        self.frac(45, 0.85, FadeIn(cs, shift=UP * 0.15), run_time=0.5)
        self.snap("drag")
        self.finish(keep=[header])


# ==========================================================================
# M4_2 -- obstruction 3: spectrum  (cues 46-50)
# ==========================================================================
class M4_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["M4_2"], END["M4_2"]

    def construct(self):
        header = hdr("Why This Disallows Dissipation")
        self.add(header)
        lab = at_left(Text("3  Spectrum", font_size=32, color=TEAL), 1.8)

        # complex plane for the eigenvalues
        ax = Axes(x_range=[-2.5, 2.5, 1], y_range=[-2, 2, 1], x_length=5.4, y_length=4.2,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([3.8, -0.45, 0])
        axl = VGroup(tex(r"\mathrm{Re}\,\lambda", 22, GREY_B).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     tex(r"\mathrm{Im}\,\lambda", 22, GREY_B).next_to(ax, UP, buff=0.05).align_to(ax, LEFT))

        # cue 46: linearise about a fixed point: xi' = A xi, A = J S, S = Hess H
        a1 = at_left(tex(r"\dot\xi = A\,\xi,\qquad A = J\,S", 34), 0.85)
        a2 = at_left(tex(r"S = \mathrm{Hess}\,H\ \ \text{(symmetric)}", 30), 0.05)
        self.at(46, FadeIn(lab), Write(a1), run_time=1.4)
        self.frac(46, 0.45, Write(a2), run_time=1.4)
        self.frac(46, 0.8, Create(ax), FadeIn(axl), run_time=0.8)

        # cue 47: J^-1 A J = S J = -A^T  -> spectrum symmetric under lambda -> -lambda
        a3 = at_left(tex(r"J^{-1}AJ = SJ = -A^{T}", 34), -0.85)
        a4 = at_left(tex(r"A\ \sim\ -A^T\ \Rightarrow\ \mathrm{spec}(A)=-\mathrm{spec}(A)", 28), -1.6)
        clamp_width(a4, 7.4)
        pairs = [(1.4, 0.0), (0.0, 1.0), (0.8, 0.9), (0.8, -0.9)]
        dots, dashes, labs = VGroup(), VGroup(), VGroup()
        for re_, im_ in pairs:
            p1, p2 = ax.c2p(re_, im_), ax.c2p(-re_, -im_)
            if abs(re_) + abs(im_) < 1e-6:
                continue
            dots.add(Dot(p1, color=BLUE, radius=0.08), Dot(p2, color=BLUE, radius=0.08))
            dashes.add(DashedLine(p1, p2, color=GREY_B, stroke_width=2))
        self.at(47, Write(a3), run_time=1.6)
        self.frac(47, 0.3, Write(a4), run_time=1.6)
        self.frac(47, 0.55, LaggedStart(*[Create(d) for d in dashes], lag_ratio=0.2),
                  LaggedStart(*[FadeIn(d, scale=0.5) for d in dots], lag_ratio=0.1), run_time=2.0)
        sym = note("λ and −λ always come in pairs", 18, BLUE).next_to(ax, DOWN, buff=0.1)
        self.play(FadeIn(sym), run_time=0.4)
        self.snap("spectrum")

        # cue 48: in particular tr A = 0
        a5 = at_left(tex(r"\mathrm{tr}\,A = 0", 40, YELLOW), -2.45)
        self.at(48, Write(a5), run_time=1.0)

        # cue 49: every decaying mode has a growing partner; drag: tr A = -b/m != 0
        l_dec = note("decaying", 16, GREEN).next_to(ax.c2p(-1.4, 0), UP, buff=0.12)
        l_gro = note("growing partner", 16, RED).next_to(ax.c2p(1.4, 0), UP, buff=0.12)
        self.at(49, Indicate(dots[0], color=GREEN, scale_factor=1.8), Indicate(dots[1], color=RED, scale_factor=1.8),
                FadeIn(l_dec), FadeIn(l_gro), run_time=1.0)
        none = chip("no asymptotically stable fixed points", RED, 22)
        none.move_to([-2.6, -3.0, 0])
        self.frac(49, 0.3, FadeIn(none, shift=UP * 0.15), run_time=0.5)
        self.frac(49, 0.55, *[FadeOut(m) for m in [dots, dashes, sym, l_dec, l_gro]], run_time=0.5)
        drag_pts = VGroup(Dot(ax.c2p(-0.8, 0), color=ORANGE, radius=0.09), Dot(ax.c2p(0, 0), color=ORANGE, radius=0.09))
        ghost = Circle(radius=0.09, color=GREY_B, stroke_width=2).move_to(ax.c2p(0.8, 0))
        gx = Text("✗", font_size=26, color=RED).next_to(ghost, UP, buff=0.05)
        dl = tex(r"-b/m", 24, ORANGE).next_to(drag_pts[0], UP, buff=0.15)
        trd = tex(r"\text{drag: } \mathrm{tr}\,A=-\frac bm\neq 0", 32, ORANGE).move_to([0.2, 0.85, 0])
        trd.set_x(-3.0)
        self.play(FadeIn(drag_pts), FadeIn(ghost), FadeIn(gx), FadeIn(dl), FadeOut(a1), FadeOut(a2), FadeOut(a3),
                  FadeOut(a4), FadeOut(a5), run_time=0.8)
        trd.move_to([-2.9, 0.4, 0])
        self.play(Write(trd), run_time=1.0)

        # cue 50: with Poincare recurrence on a bounded phase space: no attractors
        self.at(50, *[FadeOut(m) for m in [drag_pts, ghost, gx, dl, ax, axl, trd, none]], run_time=0.5)
        C = np.array([3.6, -1.0, 0.0])
        loops = VGroup(*[Ellipse(width=2 * r, height=1.4 * r, color=BLUE, stroke_width=3).move_to(C) for r in (0.5, 0.95, 1.4, 1.85)])
        arr = [Arrow(C + [r, 0.01, 0], C + [r, 0.3, 0], buff=0, color=BLUE, stroke_width=3, max_tip_length_to_length_ratio=0.8)
               for r in (0.95, 1.85)]
        bnd = DashedVMobject(Circle(radius=2.1, color=GREY_B).move_to(C), num_dashes=50)
        rec = note("bounded phase space + Poincaré recurrence:\norbits keep coming back", 20, BLUE).move_to([3.6, 1.9, 0])
        att = Text("✗", font_size=60, color=RED).move_to(C)
        concl = chip("Hamiltonian flows have no attractors", YELLOW, 28, fill=0.1)
        at_left(concl, -0.3)
        self.play(Create(bnd), LaggedStart(*[Create(l) for l in loops], lag_ratio=0.2), FadeIn(rec), run_time=1.4)
        self.play(FadeIn(att), FadeIn(concl, shift=UP * 0.2), run_time=0.7)
        self.snap("noattractor")
        self.finish()


# ==========================================================================
# M5 -- Loophole 1: hide the dissipation in time  (cues 51-61)
# ==========================================================================
class M5(SyncedScene):
    AUDIO_START, AUDIO_END = START["M5"], END["M5"]

    def construct(self):
        header = hdr("Loophole 1: Hide the Dissipation in Time", 32, "subsubsection")
        # cue 51: "Now there are some loopholes to hide the dissipation in time."
        self.at(51, Write(header), run_time=0.6)
        hide = chip("hide the dissipation in explicit time dependence", ORANGE, 28)
        hide.move_to([0, 0.4, 0])
        self.sync(CUES[51].start + 0.8)
        self.play(FadeIn(hide, shift=UP * 0.2), run_time=0.6)

        # cue 52: Caldirola-Kanai Hamiltonian: a perfectly good symplectic system in (x, p_c)
        self.at(52, FadeOut(hide), run_time=0.4)
        e1 = at_left(tex(r"H = \frac{p_c^{\,2}}{2m}\,e^{-bt/m}", 42, YELLOW), 1.75)
        c1 = chip("a perfectly good symplectic system in (x, p_c)", GREEN, 22)
        at_left(c1, 0.85)
        e2 = at_left(tex(r"p_c = m\,e^{bt/m}\,\dot x", 40), -0.05)
        self.play(Write(e1), run_time=1.4)
        self.frac(52, 0.35, FadeIn(c1, shift=UP * 0.15), run_time=0.5)
        self.frac(52, 0.6, Write(e2), run_time=1.4)

        # cue 53-54: the physical momentum
        e3 = at_left(tex(r"p = e^{-bt/m}\,p_c", 40, ORANGE), -0.95)
        n3 = at_left(note("the physical momentum", 20, ORANGE), -1.55)
        self.at(53, FadeIn(n3), run_time=0.4)
        self.at(54, Write(e3), run_time=1.2)

        # cue 55: the map psi_t satisfies the pullback identity
        e4 = at_left(tex(r"\psi_t(x,p_c) = (x,\ e^{-bt/m}p_c)", 34), -2.2)
        e5 = at_left(tex(r"\psi_t^*(\mathrm{d}x\wedge\mathrm{d}p) = e^{-bt/m}\,\mathrm{d}x\wedge\mathrm{d}p_c", 30, YELLOW), -2.95)
        self.at(55, Write(e4), run_time=1.4)
        self.frac(55, 0.45, Write(e5), run_time=1.8)
        self.snap("map")

        # cue 56-57: canonical area conserved, physical area shrinks
        self.at(56, *[FadeOut(m) for m in [e1, c1, e2, e3, n3, e4]], e5.animate.scale(0.8).move_to([0, 2.5, 0]), run_time=0.5)
        tau = ValueTracker(0.0)
        tau.add_updater(lambda m, dt: m.set_value(min(m.get_value() + 0.3 * dt, 3.6)))
        b = 0.6
        base = blob_base(-1.2, 0.7, 0.62)
        PL, PR = np.array([-3.4, 0.5, 0.0]), np.array([3.4, 0.5, 0.0])
        fL, bL, aL = blob_panel(PL, 4.8, 3.0, tau, flow_ck, base, GREEN, sc=1.0, xl="x", yl="p_c")
        fR, bR, aR = blob_panel(PR, 4.8, 3.0, tau, flow_drag, base, RED, sc=1.0, xl="x", yl="p")
        tL = note("canonical (x, p_c)", 20, GREEN).move_to(PL + [0, -1.8, 0])
        tR = note("physical (x, p)", 20, RED).move_to(PR + [0, -1.8, 0])
        psi = VGroup(Arrow([-0.75, 0.5, 0], [0.75, 0.5, 0], buff=0, color=YELLOW), tex(r"\psi_t", 28, YELLOW).move_to([0, 0.9, 0]))
        lL, nL = area_label("area =", aL, GREEN, PL + [-0.4, -2.35, 0])
        lR, nR = area_label("area =", aR, RED, PR + [-0.4, -2.35, 0])
        self.add(tau)
        self.play(Create(fL), Create(fR), FadeIn(bL), FadeIn(bR), FadeIn(tL), FadeIn(tR), FadeIn(psi),
                  FadeIn(lL), FadeIn(nL), FadeIn(lR), FadeIn(nR), run_time=0.9)
        cn = note("conserved ✓", 20, GREEN).move_to(PL + [0, -2.9, 0])
        self.at(56, FadeIn(cn), offset=1.5, run_time=0.4)
        sh = note("shrinks ✗", 20, RED).move_to(PR + [0, -2.9, 0])
        self.at(57, FadeIn(sh), run_time=0.4)
        disc = note("the discrepancy lives entirely in a time-dependent, non-symplectic change of variables", 18, YELLOW)
        disc.move_to([0, -3.1, 0])
        self.frac(57, 0.45, FadeIn(disc), run_time=0.5)
        self.snap("panels")

        # cue 58-60: the cost shows up on quantisation
        self.at(58, *[FadeOut(m) for m in [fL, fR, bL, bR, tL, tR, psi, lL, nL, lR, nR, cn, sh, disc, e5]], run_time=0.5)
        tau.clear_updaters()
        q1 = at_left(tex(r"[x,p_c] = i\hbar", 44), 1.5)
        self.play(Write(q1), run_time=1.0)
        q2 = at_left(tex(r"[x,p] = i\hbar\,e^{-bt/m}\ \longrightarrow\ 0", 44, ORANGE), 0.45)
        self.at(59, Write(q2), run_time=1.6)
        ax = Axes(x_range=[0, 6, 1], y_range=[0, 1.2, 0.5], x_length=3.4, y_length=2.2,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([4.3, -0.5, 0])
        dec = ax.plot(lambda t: math.exp(-0.5 * t), x_range=[0, 6], color=RED)
        unc = at_left(tex(r"\Delta x\,\Delta p \ \ge\ \tfrac{\hbar}{2}\,e^{-bt/m}\ \to\ 0", 36, RED), -0.65)
        un = at_left(note("the uncertainty bound for the physical\nvariables decays away (Dekker)", 18, GREY_B), -1.4)
        self.at(60, Write(unc), Create(ax), Create(dec), FadeIn(un), run_time=1.6)

        # cue 61: the Hamiltonian is not describing the physical phase space
        self.at(61, FadeOut(un), run_time=0.3)
        fin = chip("the Hamiltonian is not describing the physical phase space", RED, 24, fill=0.12)
        at_left(fin, -2.6)
        self.play(FadeIn(fin, shift=UP * 0.2), run_time=0.6)
        self.snap("cost")
        self.finish()


# ==========================================================================
# M6 -- Loophole 2: first integral and Lie-Poisson structure  (cues 62-69)
# ==========================================================================
B_DRAG = 1.0
PL6 = np.array([-3.4, -1.5, 0.0])
PR6 = np.array([3.4, -1.5, 0.0])
SX6, SP6, PMID6 = 5.6 / 6.0, 2.8 / 2.15, 0.725


def xp_pt(x, p):
    """(x, p) -> scene point in the left panel."""
    return np.array([PL6[0] + x * SX6, PL6[1] + (p - PMID6) * SP6, 0.0])


def cu_pt(c, u):
    """(c, u) -> scene point in the right panel."""
    return np.array([PR6[0] + c * 1.12, PR6[1] + (u + 1.2) * 0.64, 0.0])


C_LINES = [-1.5, -0.5, 0.5, 1.5]


def c_line_p0(c):
    return min(1.6, B_DRAG * (c + 3.0))


class M6(SyncedScene):
    AUDIO_START, AUDIO_END = START["M6"], END["M6"]

    def construct(self):
        header = hdr("Loophole 2: Give Up the Canonical Structure", 32, "subsubsection")
        # cue 62: just give up the canonical structure
        self.at(62, Write(header), run_time=0.6)
        t62 = Text("give up the canonical structure", font_size=34, color=YELLOW).move_to([0, 1.4, 0])
        self.sync(CUES[62].start + 1.0)
        self.play(FadeIn(t62), run_time=0.5)
        # cue 63: time dependence is not necessary
        t63 = Text("time dependence is not even necessary", font_size=34, color=YELLOW).move_to([0, 1.4, 0])
        self.at(63, ReplacementTransform(t62, t63), run_time=0.6)

        # panels
        frL = Rectangle(width=5.8, height=3.0, color=GREY_B, stroke_width=2).move_to(PL6)
        frR = Rectangle(width=5.8, height=3.0, color=GREY_B, stroke_width=2).move_to(PR6)
        att = DashedLine(xp_pt(-3.0, 0), xp_pt(3.0, 0), color=RED, stroke_width=3)
        attl = note("p = 0  (the attractor)", 16, RED).move_to(xp_pt(1.6, -0.2))
        xl_ = tex(r"x", 24, GREY_B).move_to(xp_pt(2.8, 1.65))
        pl_ = tex(r"p", 24, GREY_B).move_to(xp_pt(-2.8, 1.65))
        cl_ = tex(r"c", 24, GREY_B).move_to(PR6 + [2.65, -1.25, 0])
        ul_ = tex(r"u=\ln|p|", 24, GREY_B).move_to(PR6 + [-1.8, 1.25, 0])
        lines = VGroup()
        for c in C_LINES:
            p0 = c_line_p0(c)
            pts = [xp_pt(c - p / B_DRAG, p) for p in np.linspace(p0, 0, 30)]
            m = VMobject(stroke_color=TEAL, stroke_width=3)
            m.set_points_as_corners(pts)
            lines.add(m)

        # cue 64: first integral c = x + p/b
        eq_c = tex(r"c = x + \frac{p}{b}\qquad\text{(a first integral)}", 40)
        eq_c.move_to([0, 1.45, 0])
        self.at(64, ReplacementTransform(t63, eq_c), Create(frL), FadeIn(xl_), FadeIn(pl_), Create(att), FadeIn(attl),
                run_time=1.0)
        self.play(LaggedStart(*[Create(l) for l in lines], lag_ratio=0.25), run_time=1.6)
        lc = note("level sets of c: straight lines", 16, TEAL).move_to(xp_pt(-1.4, 1.65))
        self.play(FadeIn(lc), run_time=0.4)

        # cue 65: cdot = p/m - p/m = 0 ;  new coordinate u = ln|p|
        eq_cd = tex(r"\dot c = \frac pm-\frac pm = 0,\qquad u=\ln|p|", 38)
        eq_cd.move_to([0, 1.45, 0])
        tau = ValueTracker(0.0)
        tau.add_updater(lambda m, dt: m.set_value((m.get_value() + 0.8 * dt) % 3.3))

        def ptsL():
            g = VGroup()
            for c in C_LINES:
                p0 = c_line_p0(c)
                p = p0 * math.exp(-B_DRAG * tau.get_value())
                g.add(Dot(xp_pt(c - p / B_DRAG, p), radius=0.08, color=YELLOW))
            return g

        def ptsR():
            g = VGroup()
            for c in C_LINES:
                p0 = c_line_p0(c)
                u = math.log(p0) - B_DRAG * tau.get_value()
                g.add(Dot(cu_pt(c, u), radius=0.08, color=YELLOW))
            return g
        dL, dR = always_redraw(ptsL), always_redraw(ptsR)
        axR = VGroup(Line(cu_pt(-2.5, -3.4), cu_pt(2.5, -3.4), color=GREY_B, stroke_width=2),
                     Line(cu_pt(0, -3.4), cu_pt(0, 1.0), color=GREY_B, stroke_width=2))
        self.at(65, ReplacementTransform(eq_c, eq_cd), FadeOut(lc), Create(frR), Create(axR), FadeIn(cl_), FadeIn(ul_),
                run_time=1.0)
        self.add(tau)
        self.play(FadeIn(dL), FadeIn(dR), run_time=0.5)

        # cue 66: cdot = 0, udot = -b/m : a constant vector field
        eq_u = tex(r"\dot c = 0,\qquad \dot u = -\frac bm", 40)
        eq_u.move_to([0, 1.45, 0])
        vf = VGroup(*[Arrow(cu_pt(c, u), cu_pt(c, u - 0.5), buff=0, color=ORANGE, stroke_width=2.5,
                            max_tip_length_to_length_ratio=0.5)
                      for c in np.linspace(-2.2, 2.2, 5) for u in (0.2, -1.0, -2.2)])
        vfl = note("a constant vector field", 18, ORANGE).move_to(PR6 + [0.9, 1.75, 0])
        ul_.set_opacity(0.9)
        self.at(66, ReplacementTransform(eq_cd, eq_u), FadeIn(vf), FadeIn(vfl), run_time=1.0)
        self.snap("cu")

        # cue 67: it preserves dc ^ du; in the original variables omega' = dx^dp / p
        self.at(67, FadeOut(vf), FadeOut(vfl), run_time=0.4)
        eq_w = tex(r"\mathrm{d}c\wedge\mathrm{d}u=\frac{\mathrm{d}x\wedge\mathrm{d}p}{p}=:\omega'", 34)
        eq_w.move_to([0, 1.95, 0])
        eq_b = tex(r"\{x,p\}'=p,\qquad \tilde H=\frac{bx+p}{m}", 34, YELLOW)
        eq_b.move_to([0, 1.05, 0])
        self.play(ReplacementTransform(eq_u, eq_w), run_time=1.0)
        self.frac(67, 0.4, Write(eq_b), run_time=1.6)

        # cue 68: check xdot = {x, H~}', pdot = {p, H~}'
        eq_chk = tex(r"\dot x=\{x,\tilde H\}'=\frac pm\ \checkmark\qquad \dot p=\{p,\tilde H\}'=-\frac bm\,p\ \checkmark", 30, GREEN)
        clamp_width(eq_chk, 12.0)
        eq_chk.move_to([0, 1.05, 0])
        self.at(68, FadeOut(eq_b), run_time=0.3)
        self.play(Write(eq_chk), run_time=1.8)

        # cue 69: drag IS an autonomous Hamiltonian system on the Lie-Poisson structure of the affine algebra
        fin = chip("drag is an autonomous Hamiltonian system on the Lie–Poisson structure of the affine algebra",
                   YELLOW, 20, fill=0.1)
        clamp_width(fin, 12.2)
        fin.move_to([0, 0.35, 0])
        self.at(69, FadeIn(fin, shift=UP * 0.15), run_time=0.7)
        self.snap("loophole2")
        self.finish()


# ==========================================================================
# M6_1 -- the price, and the real lesson  (cues 70-83)
# ==========================================================================
class M6_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M6_1"], END["M6_1"]

    def construct(self):
        header = hdr("Loophole 2: Give Up the Canonical Structure", 32, "subsubsection")
        self.add(header)

        # cue 70: but look at the price
        t70 = Text("but look at the price", font_size=38, color=RED).move_to([0, 0.3, 0])
        self.at(70, FadeIn(t70, scale=0.9), run_time=0.6)

        # panel (x,p) with both half-planes
        PC = np.array([-3.4, -0.85, 0.0])
        sp = 1.1
        P = lambda x, p: np.array([PC[0] + x * 0.9, PC[1] + p * sp, 0.0])
        up_ = Rectangle(width=5.4, height=1.8 * sp, stroke_width=0, fill_color=BLUE, fill_opacity=0.18).move_to(P(0, 0.9))
        dn_ = Rectangle(width=5.4, height=1.8 * sp, stroke_width=0, fill_color=TEAL, fill_opacity=0.18).move_to(P(0, -0.9))
        axis_ = DashedLine(P(-3.0, 0), P(3.0, 0), color=RED, stroke_width=4)
        frame = Rectangle(width=5.4, height=3.6 * sp, color=GREY_B, stroke_width=2).move_to(PC)
        lp = note("leaf  p > 0", 18, BLUE).move_to(P(-1.6, 1.5))
        ln_ = note("leaf  p < 0", 18, TEAL).move_to(P(-1.6, -1.5))
        l0 = note("p = 0: every point is its own leaf", 16, RED).move_to(P(0.1, 0.28))
        pan = VGroup(frame, up_, dn_, axis_)

        # J = p plot
        axj = Axes(x_range=[-1.5, 1.5, 0.5], y_range=[-1.5, 1.5, 0.5], x_length=4.2, y_length=2.6,
                   axis_config={"include_tip": False, "font_size": 14}).move_to([3.7, -0.6, 0])
        jl = axj.plot(lambda p: p, x_range=[-1.5, 1.5], color=WHITE)
        zero = Dot(axj.c2p(0, 0), color=RED, radius=0.11)
        jlab = tex(r"J(p)=p", 28).next_to(axj, UP, buff=0.1)
        plab = tex(r"p", 22, GREY_B).next_to(axj, RIGHT, buff=0.05)

        # cue 71: the bivector J = p vanishes exactly on p = 0
        t71 = tex(r"\text{the bivector } J = p\ \text{ vanishes exactly on } p=0", 32)
        t71.move_to([0, 1.95, 0])
        self.at(71, FadeOut(t70), Write(t71), Create(axj), Create(jl), FadeIn(jlab), FadeIn(plab), run_time=1.4)
        self.play(FadeIn(zero, scale=2), run_time=0.4)
        # cue 72: which is the attractor
        att = note("which is the attractor", 22, RED).move_to(axj.c2p(0.95, -0.75))
        self.at(72, FadeIn(att), Indicate(zero, color=RED, scale_factor=2.0), run_time=0.9)

        # cue 73: rank drops to zero; the leaves are two half-planes plus a line of points
        self.at(73, Create(frame), FadeIn(up_), FadeIn(dn_), run_time=0.8)
        self.play(Create(axis_), FadeIn(lp), FadeIn(ln_), run_time=0.8)
        self.frac(73, 0.6, FadeIn(l0), run_time=0.5)
        self.snap("leaves")

        # cue 74-76: Liouville not violated: the strip eps < p < 1 has infinite area
        self.at(74, *[FadeOut(m) for m in [axj, jl, zero, jlab, plab, att, l0, lp, ln_]], FadeOut(t71), run_time=0.5)
        t74 = Text("Liouville is not violated", font_size=32, color=GREEN).move_to([0, 1.95, 0])
        self.play(FadeIn(t74), run_time=0.5)
        eps = ValueTracker(0.8)

        def strip():
            e = eps.get_value()
            r = Rectangle(width=2.4, height=(1.0 - e) * sp, stroke_width=2, color=YELLOW, fill_color=YELLOW,
                          fill_opacity=0.4)
            r.move_to(P(0, (1.0 + e) / 2))
            return r
        st = always_redraw(strip)
        e_lab = tex(r"\varepsilon<p<1", 30, YELLOW).move_to([2.2, 0.9, 0])
        area_tex = tex(r"\omega'\text{-area}=\int_\varepsilon^1\frac{\mathrm{d}p}{p}=\ln\frac1\varepsilon", 32)
        area_tex.move_to([3.6, 0.1, 0])
        clamp_width(area_tex, 6.4)
        self.at(75, FadeIn(st), FadeIn(e_lab), Write(area_tex), run_time=1.6)
        val = always_redraw(lambda: DecimalNumber(math.log(1.0 / eps.get_value()), num_decimal_places=2, font_size=44,
                                                  color=YELLOW).move_to([3.6, -0.8, 0]))
        self.add(val)
        self.at(76, eps.animate.set_value(0.004), run_time=7.0, rate_func=rate_functions.ease_out_sine)
        inf = note("→ ∞: the attractor sits infinitely far away", 22, GREEN).move_to([3.6, -1.7, 0])
        self.play(FadeIn(inf), run_time=0.5)

        # cue 77-79: H~ is just c in disguise; it knows nothing about kinetic energy, heat, or why the ball stops
        self.at(77, *[FadeOut(m) for m in [pan, st, e_lab, area_tex, val, inf, t74]], run_time=0.5)
        self.remove(val, st)
        h1 = tex(r"\tilde H=\frac{bx+p}{m}=\frac bm\,c", 46, YELLOW).move_to([0, 1.5, 0])
        h2 = note("just the conserved quantity c in disguise", 26, GREY_B).next_to(h1, DOWN, buff=0.4)
        self.play(Write(h1), FadeIn(h2), run_time=1.4)
        x1, x2, x3 = (chip("kinetic energy", RED, 24), chip("heat", RED, 24), chip("why the ball stops", RED, 24))
        xs = VGroup(x1, x2, x3).arrange(RIGHT, buff=0.7).move_to([0, -0.6, 0])
        no = note("it knows nothing about:", 22).move_to([0, 0.15, 0])
        self.at(78, FadeIn(no), run_time=0.4)
        self.play(LaggedStart(*[FadeIn(c, shift=UP * 0.15) for c in xs], lag_ratio=0.5), run_time=2.0)
        nt = chip("not Hamiltonian in the sense of being the total energy", RED, 24, fill=0.1).move_to([0, -2.0, 0])
        self.at(79, FadeIn(nt, shift=UP * 0.15), run_time=0.6)

        # cue 80: the real lesson
        self.at(80, *[FadeOut(m) for m in [h1, h2, xs, no, nt]], run_time=0.4)
        les = Text("This is the real lesson.", font_size=46, color=YELLOW).move_to([0, 0.3, 0])
        self.play(FadeIn(les, scale=0.9), run_time=0.6)

        # cue 81: any planar flow with no fixed points is locally Hamiltonian (flow-box)
        self.at(81, FadeOut(les), run_time=0.4)
        CL, CR = np.array([-3.4, -0.9, 0.0]), np.array([3.4, -0.9, 0.0])
        curv = VGroup(*[ParametricFunction(lambda t, k=k: CL + [t, k * 0.55 + 0.35 * math.sin(1.3 * t + 0.5 * k), 0],
                                           t_range=[-2.2, 2.2], color=ORANGE, stroke_width=3) for k in range(-2, 3)])
        stra = VGroup(*[Line(CR + [-2.2, k * 0.55, 0], CR + [2.2, k * 0.55, 0], color=GREEN, stroke_width=3) for k in range(-2, 3)])
        tips_l = VGroup(*[Arrow(c.point_from_proportion(0.9), c.point_from_proportion(1.0), buff=0, color=ORANGE, stroke_width=3,
                                max_tip_length_to_length_ratio=1.5) for c in curv])
        tips_r = VGroup(*[Arrow(c.get_end() + [-0.15, 0, 0], c.get_end(), buff=0, color=GREEN, stroke_width=3,
                                max_tip_length_to_length_ratio=1.5) for c in stra])
        mp = Arrow([-0.9, -0.9, 0], [0.9, -0.9, 0], buff=0, color=YELLOW)
        mpl = note("flow-box coordinates", 18, YELLOW).next_to(mp, UP, buff=0.1)
        fb = tex(r"\dot c = 0,\qquad \dot s = 1", 36, GREEN).next_to(stra, DOWN, buff=0.4)
        t81 = note("any planar flow without fixed points is locally Hamiltonian", 24).move_to([0, 2.0, 0])
        self.play(FadeIn(t81), Create(curv), FadeIn(tips_l), run_time=1.2)
        self.frac(81, 0.35, GrowArrow(mp), FadeIn(mpl), Create(stra), FadeIn(tips_r), run_time=1.4)
        self.play(Write(fb), run_time=0.9)
        cheap = chip("so the existence of a Hamiltonian is cheap", GREEN, 24).move_to([0, -3.0, 0])
        self.frac(81, 0.8, FadeIn(cheap, shift=UP * 0.15), run_time=0.5)

        # cue 82: what is expensive: the physical energy on a canonical phase space, non-degenerate bracket
        self.at(82, *[FadeOut(m) for m in [curv, stra, tips_l, tips_r, mp, mpl, fb, t81, cheap]], run_time=0.4)
        ex = Text("what is expensive: a Hamiltonian that is", font_size=30, color=YELLOW).move_to([0, 1.6, 0])
        r1 = chip("the physical energy", ORANGE, 26)
        r2 = chip("on the canonical phase space", BLUE, 26)
        r3 = chip("with a non-degenerate bracket", TEAL, 26)
        rg = VGroup(r1, r2, r3).arrange(DOWN, buff=0.4).move_to([0, -0.5, 0])
        self.play(FadeIn(ex), run_time=0.4)
        self.frac(82, 0.2, FadeIn(r1, shift=RIGHT * 0.2), run_time=0.4)
        self.frac(82, 0.5, FadeIn(r2, shift=RIGHT * 0.2), run_time=0.4)
        self.frac(82, 0.75, FadeIn(r3, shift=RIGHT * 0.2), run_time=0.4)

        # cue 83: that combination is what dissipation forbids
        forb = chip("that combination is what dissipation forbids", RED, 30, fill=0.12).move_to([0, -2.8, 0])
        self.at(83, FadeIn(forb, shift=UP * 0.2), run_time=0.6)
        self.snap("forbid")
        self.finish()


# ==========================================================================
# M7 -- Reversibility  (cues 84-93)
# ==========================================================================
class M7(SyncedScene):
    AUDIO_START, AUDIO_END = START["M7"], END["M7"]

    def construct(self):
        header = hdr("Reversibility")
        # cue 84: next thing to define is reversibility
        self.at(84, Write(header), run_time=0.6)
        rt = Text("time reversal", font_size=44, color=YELLOW).move_to([0, 0.3, 0])
        self.sync(CUES[84].start + 0.6)
        self.play(FadeIn(rt, scale=0.9), run_time=0.6)

        # cue 85: Theta: anti-symplectic involution, Theta^2 = 1
        self.at(85, FadeOut(rt), run_time=0.3)
        C = np.array([3.7, -0.3, 0.0])
        fr = phase_frame(C, 5.0, 4.2)
        circ = Circle(radius=1.4, color=GREY_B, stroke_width=2).move_to(C)
        L1 = at_left(tex(r"\Theta^2 = 1", 40), 1.8)
        n1 = at_left(note("an involution", 20, GREY_B), 1.2)
        self.play(Write(L1), FadeIn(n1), Create(fr), Create(circ), run_time=1.2)
        # cue 86
        L2 = at_left(tex(r"\Theta^*\omega = -\omega", 40), 0.4)
        n2 = at_left(note("anti-symplectic", 20, GREY_B), -0.2)
        self.at(86, Write(L2), FadeIn(n2), run_time=1.2)

        # cue 87: in canonical coordinates Theta(q,p) = (q,-p)
        L3 = at_left(tex(r"\Theta(q,p) = (q,\,-p)", 40, YELLOW), -1.2)
        ang = lambda d: math.radians(d)
        pt = lambda d: C + 1.4 * np.array([math.cos(ang(d)), math.sin(ang(d)), 0])
        arcA = Arc(radius=1.4, start_angle=ang(140), angle=-ang(100), color=BLUE, stroke_width=5).move_arc_center_to(C)
        arcA.add_tip(tip_length=0.22)
        arcB = Arc(radius=1.4, start_angle=ang(220), angle=ang(100), color=GREEN, stroke_width=5).move_arc_center_to(C)
        arcB.add_tip(tip_length=0.22)
        P_, tP_ = Dot(pt(140), color=BLUE), Dot(pt(220), color=GREEN)
        link = DashedLine(pt(140), pt(220), color=GREY_B)
        lP = tex(r"(q,p)", 22, BLUE).next_to(P_, LEFT, buff=0.1)
        lT = tex(r"(q,-p)", 22, GREEN).next_to(tP_, LEFT, buff=0.1)
        self.at(87, Write(L3), run_time=1.2)
        self.play(FadeIn(P_), FadeIn(tP_), Create(link), FadeIn(lP), FadeIn(lT), run_time=0.8)
        self.play(Create(arcA), Create(arcB), run_time=1.2)
        rv = note("Θ turns a solution into a time-reversed solution", 18, YELLOW).move_to(C + [0, -2.45, 0])
        self.play(FadeIn(rv), run_time=0.4)
        self.snap("theta")

        # cue 88: if H∘Θ = H then Θ_* X_H = -X_H and Θ φ_t = φ_{-t} Θ
        self.at(88, *[FadeOut(m) for m in [fr, circ, arcA, arcB, P_, tP_, link, lP, lT, rv]], run_time=0.5)
        M1_ = at_left(tex(r"H\circ\Theta = H\ \Rightarrow\ \Theta_*X_H = -X_H", 30), -2.15)
        M2_ = at_left(tex(r"\Theta\circ\phi_t = \phi_{-t}\circ\Theta", 34, YELLOW), -2.95)
        sq_c = np.array([3.4, -0.1, 0.0])
        dx_, dy_ = 1.7, 1.45
        nz = tex(r"z", 30).move_to(sq_c + [-dx_, dy_, 0])
        npz = tex(r"\phi_t(z)", 30).move_to(sq_c + [dx_, dy_, 0])
        nTz = tex(r"\Theta z", 30).move_to(sq_c + [-dx_, -dy_, 0])
        nTpz = tex(r"\Theta\phi_t(z)=\phi_{-t}(\Theta z)", 22, YELLOW).move_to(sq_c + [dx_ - 0.3, -dy_, 0])
        ar_t = Arrow(nz.get_right() + [0.1, 0, 0], npz.get_left() + [-0.1, 0, 0], buff=0, color=BLUE)
        ar_l = Arrow(nz.get_bottom() + [0, -0.1, 0], nTz.get_top() + [0, 0.1, 0], buff=0, color=GREEN)
        ar_r = Arrow(npz.get_bottom() + [0, -0.1, 0], nTpz.get_top() + [dx_ * 0 + 0.3, 0.1, 0], buff=0, color=GREEN)
        ar_b = Arrow(nTz.get_right() + [0.1, 0, 0], nTpz.get_left() + [-0.1, 0, 0], buff=0, color=BLUE)
        lt = tex(r"\phi_t", 24, BLUE).next_to(ar_t, UP, buff=0.05)
        ll = tex(r"\Theta", 24, GREEN).next_to(ar_l, LEFT, buff=0.1)
        lr = tex(r"\Theta", 24, GREEN).next_to(ar_r, RIGHT, buff=0.1)
        lb = tex(r"\phi_{-t}", 24, BLUE).next_to(ar_b, UP, buff=0.05)
        sq = VGroup(nz, npz, nTz, nTpz, ar_t, ar_l, ar_r, ar_b, lt, ll, lr, lb)
        conj = note("Hamiltonian flows are conjugate to their own inverse", 18, YELLOW).move_to([3.3, -2.7, 0])
        self.play(Write(M1_), run_time=1.2)
        self.frac(88, 0.3, Write(M2_), FadeIn(nz), FadeIn(npz), FadeIn(nTz), FadeIn(nTpz), run_time=1.4)
        self.frac(88, 0.55, *[GrowArrow(a) for a in (ar_t, ar_l, ar_r, ar_b)], FadeIn(lt), FadeIn(ll), FadeIn(lr), FadeIn(lb),
                  run_time=1.2)
        self.frac(88, 0.85, FadeIn(conj), run_time=0.4)
        self.snap("square")

        # cue 89: split the drag field into reversible + dissipative parts
        self.at(89, *[FadeOut(m) for m in [L1, n1, L2, n2, L3, M1_, M2_, sq, conj]], run_time=0.5)
        X1 = at_left(tex(r"X_{\rm rev} = \frac pm\,\partial_x", 40, BLUE), 1.8)
        X2 = at_left(tex(r"X_{\rm diss} = -\frac bm\,p\,\partial_p", 40, ORANGE), 0.7)
        t_r = note("reversible (conservative)", 18, BLUE).next_to(X1, RIGHT, buff=0.5)
        t_d = note("dissipative (irreversible)", 18, ORANGE).next_to(X2, RIGHT, buff=0.5)
        self.play(Write(X1), run_time=1.2)
        self.frac(89, 0.45, FadeIn(t_r), Write(X2), run_time=1.4)
        self.frac(89, 0.75, FadeIn(t_d), run_time=0.4)

        # cue 90: reversible terms odd, dissipative terms even
        P1 = at_left(tex(r"\Theta_*X_{\rm rev} = -X_{\rm rev}", 36, BLUE), -0.5)
        P2 = at_left(tex(r"\Theta_*X_{\rm diss} = +X_{\rm diss}", 36, ORANGE), -1.4)

        def demo(cc, col, a0, a1, text):
            b4 = Arrow(cc + [-2.0, 0, 0] + a0[0], cc + [-2.0, 0, 0] + a0[1], buff=0, color=col, stroke_width=6)
            aft = Arrow(cc + [1.0, 0, 0] + a1[0], cc + [1.0, 0, 0] + a1[1], buff=0, color=col, stroke_width=6)
            mid = VGroup(note("t → −t", 20, GREY_B), Arrow([-0.35, 0, 0], [0.35, 0, 0], buff=0, color=GREY_B, stroke_width=2))
            mid[0].move_to(cc + [-0.45, 0.35, 0])
            mid[1].move_to(cc + [-0.45, -0.05, 0])
            tt = note(text, 20, col).move_to(cc + [-0.5, 0.95, 0])
            return VGroup(b4, mid, aft, tt)
        D1 = demo(np.array([3.9, 1.2, 0.0]), BLUE, ([-0.5, 0, 0], [0.5, 0, 0]), ([0.5, 0, 0], [-0.5, 0, 0]), "odd: flips sign")
        D2 = demo(np.array([3.9, -1.0, 0.0]), ORANGE, ([0, 0.5, 0], [0, -0.5, 0]), ([0, 0.5, 0], [0, -0.5, 0]), "even: unchanged")
        self.at(90, Write(P1), FadeIn(D1), run_time=1.4)
        self.frac(90, 0.5, Write(P2), FadeIn(D2), run_time=1.4)
        self.snap("parity")

        # cue 91: a reversed trajectory solves xddot = + (b/m) xdot
        self.at(91, *[FadeOut(m) for m in [D1, D2, X1, X2, t_r, t_d, P1, P2]], run_time=0.5)
        ax = Axes(x_range=[0, 2, 0.5], y_range=[0, 5, 1], x_length=5.2, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([3.8, -0.4, 0])
        fw = ax.plot(lambda t: math.exp(-0.8 * t), x_range=[0, 2], color=GREEN)
        bw = ax.plot(lambda t: math.exp(0.8 * t), x_range=[0, 2], color=RED)
        axl = VGroup(note("time", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     note("speed |x-dot|", 16).next_to(ax, UP, buff=0.05).align_to(ax, LEFT))
        lf = note("forward: damped", 18, GREEN).move_to(ax.c2p(1.35, 0.8) + [0, 0.45, 0])
        lbk = note("time-reversed:\ngrows", 18, RED).move_to(ax.c2p(0.55, 3.4))
        Q1 = at_left(tex(r"\ddot x = +\frac bm\,\dot x", 46, RED), 1.0)
        self.play(Write(Q1), Create(ax), FadeIn(axl), run_time=1.2)
        self.frac(91, 0.45, Create(fw), FadeIn(lf), Create(bw), FadeIn(lbk), run_time=1.6)
        # cue 92: anti-damping
        ad = chip("anti-damping", RED, 30, fill=0.12)
        at_left(ad, -0.5)
        self.at(92, FadeIn(ad, shift=UP * 0.15), run_time=0.5)
        # cue 93: remember this parity split
        rem = chip("remember the parity split: reversible = odd, dissipative = even", YELLOW, 22, fill=0.1)
        rem.move_to([0, -2.95, 0])
        self.at(93, FadeIn(rem, shift=UP * 0.15), run_time=0.6)
        self.snap("antidamp")
        self.finish()


# ==========================================================================
# M7_1 -- group vs semigroup  (cues 94-98)
# ==========================================================================
class M7_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M7_1"], END["M7_1"]

    def construct(self):
        header = hdr("Reversibility")
        self.add(header)
        # cue 94: there is also an algebraic version
        t94 = Text("an algebraic version of the same fact", font_size=30, color=YELLOW)
        t94.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(94, FadeIn(t94), run_time=0.5)

        tau = ValueTracker(0.0)
        tau.add_updater(lambda m, dt: m.increment_value(dt))
        N = 9
        xs = np.linspace(-3.6, 3.6, N)
        y1, y2 = 0.3, -1.8
        line1 = Line([-5.2, y1, 0], [5.2, y1, 0], color=GREY_B, stroke_width=2)
        line2 = Line([-5.2, y2, 0], [5.2, y2, 0], color=GREY_B, stroke_width=2)
        d1 = always_redraw(lambda: VGroup(*[Dot([x * 0.85 + 1.3 * math.sin(0.9 * tau.get_value()), y1, 0], radius=0.09,
                                                color=BLUE) for x in xs]))
        tau2 = ValueTracker(0.0)
        d2 = always_redraw(lambda: VGroup(*[Dot([x * 0.85 * math.exp(-0.45 * tau2.get_value()), y2, 0], radius=0.09,
                                                color=ORANGE) for x in xs]))

        # cue 95: a Hamiltonian flow is a group of diffeomorphisms: injective
        l1 = tex(r"\{\phi_t\}_{t\in\mathbb{R}}", 36, BLUE).move_to([-4.4, y1 + 0.9, 0])
        n1 = note("a group of diffeomorphisms: injective, distinct states never merge", 20, BLUE)
        n1.next_to(l1, RIGHT, buff=0.4)
        self.at(95, Create(line1), FadeIn(d1), FadeIn(l1), run_time=0.8)
        self.add(tau)
        self.play(FadeIn(n1), run_time=0.5)

        # cue 96-97: the drag flow is only a semigroup on t >= 0
        l2 = tex(r"p\mapsto e^{-bt/m}\,p,\quad t\ge 0", 34, ORANGE).move_to([-3.4, y2 + 0.9, 0])
        n2 = note("only a semigroup", 20, ORANGE).next_to(l2, RIGHT, buff=0.4)
        tau2.add_updater(lambda m, dt: m.set_value(min(m.get_value() + dt, 9.0)))
        self.at(96, Create(line2), FadeIn(d2), FadeIn(l2), run_time=0.8)
        self.add(tau2)
        self.at(97, FadeIn(n2), run_time=0.5)

        # cue 98: all initial momenta collapse to the same point: information is lost
        coll = chip("all initial momenta collapse to p = 0: information about the initial condition is lost", RED, 22, fill=0.1)
        clamp_width(coll, 12.2)
        coll.move_to([0, -3.0, 0])
        self.at(98, FadeIn(coll, shift=UP * 0.15), run_time=0.6)
        self.snap("semigroup")
        self.finish()


# ==========================================================================
# M8 -- Entropy I: Caldeira-Leggett, GLE, FDT, order of limits  (cues 99-109)
# ==========================================================================
def spring(a, b, n=7, amp=0.09):
    a, b = np.array(a, float), np.array(b, float)
    d = b - a
    L = np.linalg.norm(d)
    u = d / L
    nrm = np.array([-u[1], u[0], 0.0])
    pts = [a, a + u * 0.15 * L]
    for i in range(n):
        f = 0.15 + 0.7 * (i + 0.5) / n
        pts.append(a + u * f * L + nrm * amp * (1 if i % 2 == 0 else -1))
    pts += [a + u * 0.85 * L, b]
    m = VMobject(stroke_color=GREY_B, stroke_width=2)
    m.set_points_as_corners(pts)
    return m


class M8(SyncedScene):
    AUDIO_START, AUDIO_END = START["M8"], END["M8"]

    def construct(self):
        header = hdr("Entropy")
        # cue 99: microscopic laws are reversible, yet friction exists
        self.at(99, Write(header), run_time=0.6)
        ca = chip("microscopic laws are reversible", BLUE, 26)
        cb = chip("friction exists", ORANGE, 26)
        yet = note("yet", 24, YELLOW)
        row = VGroup(ca, yet, cb).arrange(RIGHT, buff=0.5).move_to([0, 0.6, 0])
        self.sync(CUES[99].start + 0.9)
        self.play(FadeIn(ca, shift=RIGHT * 0.2), run_time=0.5)
        self.frac(99, 0.55, FadeIn(yet), FadeIn(cb, shift=LEFT * 0.2), run_time=0.6)

        # cue 100: the ball is not a closed system
        BC = np.array([2.6, 0.9, 0.0])
        ball = baseball(0.45).move_to(BC)
        closed = DashedVMobject(Circle(radius=0.95, color=GREY_B).move_to(BC), num_dashes=30)
        bl = note("the ball", 18).next_to(closed, DOWN, buff=0.1)
        t100 = at_left(Text("the ball is not a closed system", font_size=30, color=YELLOW), 1.7)
        self.at(100, FadeOut(row), run_time=0.4)
        self.play(FadeIn(ball), Create(closed), FadeIn(bl), Write(t100), run_time=1.2)

        # cue 101-102: Caldeira-Leggett: the ball couples linearly to N harmonic oscillators
        ys = np.linspace(2.0, -0.2, 6)
        osc = VGroup(*[Circle(radius=0.13, color=TEAL, stroke_width=3, fill_color=TEAL, fill_opacity=0.3)
                       .move_to([5.7, y, 0]) for y in ys])
        springs = VGroup(*[spring(BC + [0.5, 0, 0], [5.55, y, 0]) for y in ys])
        bath_l = note("bath: N oscillators", 18, TEAL).move_to([5.1, 2.55, 0])
        t101 = at_left(Text("Caldeira-Leggett model", font_size=30, color=YELLOW), 1.7)
        self.at(101, ReplacementTransform(t100, t101), FadeOut(closed),
                LaggedStart(*[Create(s) for s in springs], lag_ratio=0.12), FadeIn(osc), FadeIn(bath_l), run_time=1.6)
        n102 = at_left(note("the ball couples linearly to\nN harmonic oscillators", 22), 0.95)
        self.at(102, FadeIn(n102), run_time=0.6)

        # cue 103: the total Hamiltonian
        htot = tex(r"H_{\rm tot}=\frac{p^2}{2m}+V(x)+\sum_{k=1}^{N}\left[\frac{p_k^2}{2m_k}"
                   r"+\frac{m_k\omega_k^2}{2}\left(q_k-\frac{c_k}{m_k\omega_k^2}\,x\right)^2\right]", 30)
        clamp_width(htot, 12.2)
        htot.move_to([0, -1.5, 0])
        self.at(103, Write(htot), run_time=1.5)

        # cue 104: exactly Hamiltonian; Mori-Zwanzig projection onto the ball's variables
        c104 = at_left(chip("exactly Hamiltonian: symplectic, energy-conserving", GREEN, 20), 0.05)
        c104b = at_left(chip("Mori-Zwanzig: project out the bath", YELLOW, 20), -0.6)
        proj = DashedVMobject(Rectangle(width=1.0, height=3.0, color=YELLOW).move_to([5.7, 0.9, 0]), num_dashes=40)
        plab = tex(r"\mathcal P\ \text{onto the ball}", 24, YELLOW).move_to([5.1, -1.0, 0])
        self.at(104, FadeIn(c104, shift=UP * 0.15), run_time=0.6)
        self.frac(104, 0.3, FadeIn(c104b, shift=UP * 0.15), Create(proj), FadeIn(plab),
                  osc.animate.set_opacity(0.35), springs.animate.set_opacity(0.35), run_time=1.0)

        # cue 105: generalised Langevin equation
        gle = MathTex(r"m\ddot x=-V'(x)-", r"\int_0^t\gamma(t-s)\,\dot x(s)\,\mathrm{d}s", r"+\,\xi(t)", font_size=40)
        gle[1].set_color(ORANGE)
        gle[2].set_color(PURPLE)
        gle.move_to([0, -1.5, 0])
        memo = note("memory = friction", 18, ORANGE).next_to(gle[1], DOWN, buff=0.15)
        nz = note("noise", 18, PURPLE).next_to(gle[2], DOWN, buff=0.15)
        self.at(105, FadeOut(htot), run_time=0.3)
        self.play(Write(gle), run_time=1.2)
        self.play(FadeIn(memo), FadeIn(nz), run_time=0.4)

        # cue 106: fluctuation-dissipation theorem
        fdt = tex(r"\langle\xi(t)\xi(s)\rangle=k_BT\,\gamma(|t-s|)", 34, YELLOW).move_to([-1.0, -3.0, 0])
        fdtn = note("fluctuation-dissipation theorem", 20, YELLOW).next_to(fdt, RIGHT, buff=0.4)
        box = SurroundingRectangle(fdt, color=YELLOW, buff=0.12, stroke_width=2)
        self.at(106, Write(fdt), run_time=1.2)
        self.play(Create(box), FadeIn(fdtn), run_time=0.6)
        self.snap("fdt")

        # cue 107: Ohmic bath, N -> infinity: the kernel collapses to a delta
        junk = [ball, bl, springs, osc, bath_l, proj, plab, t101, n102, c104, c104b, memo, nz, fdt, fdtn, box]
        oh = at_left(tex(r"J(\omega)\propto\omega\ \ \text{(Ohmic)}", 34), 1.9)
        nn = at_left(tex(r"N\to\infty", 44, YELLOW), 0.85)
        w = ValueTracker(1.4)
        axk = Axes(x_range=[0, 3, 1], y_range=[0, 1.3, 1], x_length=4.6, y_length=2.4,
                   axis_config={"include_tip": False, "font_size": 14}).move_to([3.9, 1.0, 0])
        curve = always_redraw(lambda: axk.plot(lambda t: math.exp(-(t / w.get_value()) ** 2), x_range=[0, 3, 0.005],
                                               color=ORANGE, use_smoothing=False))
        kl = tex(r"\gamma(t)", 26, ORANGE).move_to(axk.c2p(1.9, 1.05))
        kt = tex(r"t", 22, GREY_B).next_to(axk, DOWN, buff=0.05).align_to(axk, RIGHT)
        self.at(107, *[FadeOut(m) for m in junk], FadeIn(oh), FadeIn(nn), Create(axk), FadeIn(kl), FadeIn(kt), run_time=0.8)
        self.add(curve)
        self.frac(107, 0.2, w.animate.set_value(0.04), run_time=5.5, rate_func=rate_functions.ease_in_out_sine)
        spike = Arrow(axk.c2p(0, 0), axk.c2p(0, 1.25), buff=0, color=ORANGE, stroke_width=6)
        dl = tex(r"\gamma(t)\to 2b\,\delta(t)", 28, ORANGE).move_to(axk.c2p(1.7, 0.75))
        gle2 = MathTex(r"m\ddot x=-V'(x)-", r"b\,\dot x", r"+\,\xi(t)", font_size=40)
        gle2[1].set_color(YELLOW)
        gle2.move_to([0, -1.5, 0])
        self.frac(107, 0.55, FadeOut(curve), GrowArrow(spike), FadeIn(dl), run_time=0.8)
        self.frac(107, 0.7, ReplacementTransform(gle, gle2), run_time=0.9)
        rec = chip("we recover  -b x-dot", GREEN, 24)
        rec.move_to([0, -2.6, 0])
        self.frac(107, 0.85, FadeIn(rec, shift=UP * 0.15), run_time=0.5)
        self.snap("ohmic")

        # cue 108: the order of limits matters
        order = chip("the order of limits matters", ORANGE, 28, fill=0.1).move_to([0, -2.6, 0])
        self.at(108, FadeOut(rec), FadeIn(order, shift=UP * 0.15), run_time=0.5)

        # cue 109: finite N has Poincare recurrence; dissipation emergent only for N -> inf, then t -> inf
        self.at(109, *[FadeOut(m) for m in [oh, nn, axk, kl, kt, spike, dl, gle2, order]], run_time=0.5)
        r1 = at_left(Text("finite N: Poincaré recurrence", font_size=28, color=RED), 1.8)
        r2 = at_left(note("dissipation is emergent only for", 22), 0.95)
        r3 = at_left(chip("N → ∞,  then  t → ∞", YELLOW, 28, fill=0.1), 0.2)
        om5 = np.array([0.31, 0.55, 0.83, 1.12, 1.4])
        om80 = np.random.default_rng(3).uniform(0.1, 1.6, 80)

        def small_ax(y):
            return Axes(x_range=[0, 40, 10], y_range=[-1, 1.1, 1], x_length=5.0, y_length=1.45,
                        axis_config={"include_tip": False, "font_size": 12}).move_to([3.9, y, 0])
        a5, a80 = small_ax(1.5), small_ax(-1.0)
        c5 = a5.plot(lambda t: float(np.mean(np.cos(om5 * t))), x_range=[0, 40, 0.1], color=RED, use_smoothing=False)
        c80 = a80.plot(lambda t: float(np.mean(np.cos(om80 * t))), x_range=[0, 40, 0.1], color=GREEN, use_smoothing=False)
        l5 = note("N = 5: fluctuates, revives", 16, RED).next_to(a5, UP, buff=0.05).align_to(a5, LEFT)
        l80 = note("N = 80: decays and stays decayed", 16, GREEN).next_to(a80, UP, buff=0.05).align_to(a80, LEFT)
        tl = note("time", 14, GREY_B).next_to(a80, DOWN, buff=0.05).align_to(a80, RIGHT)
        self.play(FadeIn(r1), run_time=0.5)
        self.frac(109, 0.2, Create(a5), Create(c5), FadeIn(l5), run_time=1.6)
        self.frac(109, 0.45, Create(a80), Create(c80), FadeIn(l80), FadeIn(tl), FadeIn(r2), run_time=1.6)
        self.frac(109, 0.7, FadeIn(r3, shift=UP * 0.15), run_time=0.6)
        self.snap("recurrence")
        self.finish(keep=[header])


# ==========================================================================
# M8_1 -- Entropy II: Gibbs entropy, coarse graining  (cues 110-119)
# ==========================================================================
class M8_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M8_1"], END["M8_1"]

    def construct(self):
        header = hdr("Entropy")
        self.add(header)
        q = Text("what does entropy do in this picture?", font_size=36, color=YELLOW).move_to([0, 0.4, 0])
        self.at(110, FadeIn(q, scale=0.95), run_time=0.6)

        # --- the phase-space cloud (density rho), transported by a twist flow ----------
        PC = np.array([3.9, -0.15, 0.0])
        CELL = 0.2
        NPT = 320
        rng = np.random.default_rng(7)
        rr = 0.85 * np.sqrt(rng.uniform(0, 1, NPT))
        aa = rng.uniform(0, TAU, NPT)
        P0 = np.stack([0.95 + rr * np.cos(aa), rr * np.sin(aa)], 1)
        r0, th0 = np.hypot(P0[:, 0], P0[:, 1]), np.arctan2(P0[:, 1], P0[:, 0])

        def pos(tv):
            ang = th0 + tv * 0.9 * (1.0 + 1.5 * r0 ** 2)
            return np.stack([PC[0] + r0 * np.cos(ang), PC[1] + r0 * np.sin(ang)], 1)

        tau = ValueTracker(0.0)
        rate = [0.12]
        tau.add_updater(lambda m, dt: m.set_value(min(m.get_value() + rate[0] * dt, 6.0)))
        dots = VGroup(*[Dot(radius=0.03, color=BLUE) for _ in range(NPT)])

        def upd(g):
            P = pos(tau.get_value())
            for d, (x, y) in zip(g, P):
                d.move_to([x, y, 0])
        upd(dots)
        dots.add_updater(upd)
        cells_on = [False]

        def occupied():
            P = pos(tau.get_value()) - PC[:2]
            return set(map(tuple, np.floor(P / CELL).astype(int)))

        def cells():
            if not cells_on[0]:
                return VGroup()
            return VGroup(*[Square(side_length=CELL, stroke_width=0, fill_color=ORANGE, fill_opacity=0.28)
                            .move_to([PC[0] + (i + 0.5) * CELL, PC[1] + (j + 0.5) * CELL, 0]) for i, j in occupied()])
        cells_m = always_redraw(cells)
        frame = phase_frame(PC, 5.0, 4.0, "q", "p")
        cap = note("density ρ, carried by the flow", 18, BLUE).move_to(PC + [0, 2.3, 0])
        sg0 = math.log(math.pi * 0.85 ** 2)
        s_g = Text("S_G (fine) =", font_size=22, color=BLUE).move_to(PC + [-0.5, -2.45, 0])
        s_gn = DecimalNumber(sg0, num_decimal_places=2, font_size=26, color=BLUE).next_to(s_g, RIGHT, buff=0.15)
        s_c = Text("S (coarse) =", font_size=22, color=ORANGE).move_to(PC + [-0.5, -2.9, 0])
        s_cn = always_redraw(lambda: DecimalNumber(math.log(max(len(occupied()), 1) * CELL * CELL), num_decimal_places=2,
                                                   font_size=26, color=ORANGE).next_to(s_c, RIGHT, buff=0.15))

        # cue 111: Liouville => rho transported, Gibbs entropy exactly constant
        liou = at_left(tex(r"\partial_t\rho=\{H,\rho\}", 42), 1.7)
        ln = at_left(note("ρ is transported by a volume-preserving flow", 20), 0.95)
        gibbs = at_left(tex(r"S_G[\rho]=-k_B\!\int\rho\ln\rho\ \frac{\omega^n}{n!}", 34), -0.1)
        clamp_width(gibbs, 7.4)
        const = at_left(chip("exactly constant", GREEN, 26), -1.0)
        self.at(111, FadeOut(q), Write(liou), Create(frame), FadeIn(cap), run_time=1.2)
        self.add(tau, cells_m)
        self.play(FadeIn(dots), run_time=0.5)
        self.frac(111, 0.25, FadeIn(ln), run_time=0.5)
        self.frac(111, 0.45, Write(gibbs), FadeIn(s_g), FadeIn(s_gn), run_time=1.4)
        self.frac(111, 0.75, FadeIn(const, shift=UP * 0.15), run_time=0.5)
        self.snap("fine")

        # cue 112: entropy growth requires coarse graining
        cg = Text("Entropy growth requires\ncoarse-graining", font_size=34, color=YELLOW, line_spacing=0.9)
        at_left(cg, 1.2)
        self.at(112, *[FadeOut(m) for m in [liou, ln, gibbs, const]], run_time=0.4)
        cells_on[0] = True
        rate[0] = 0.28
        self.play(FadeIn(cg), FadeIn(s_c), FadeIn(s_cn), run_time=0.6)
        self.snap("coarse")

        # cue 113: Pythagorean identity
        pyth = MathTex(r"S_G[\mathcal P\rho]", r"=S_G[\rho]+", r"k_B\,D_{\rm KL}(\rho\,\|\,\mathcal P\rho)", font_size=34)
        pyth[2].set_color(ORANGE)
        clamp_width(pyth, 7.4)
        at_left(pyth, 0.1)
        ge = at_left(tex(r"\ge\ S_G[\rho]", 34), -0.6)
        pn = at_left(note("P ρ: the projection of ρ onto the reduced description", 18, GREY_B), -1.25)
        self.at(113, Write(pyth), run_time=1.6)
        self.frac(113, 0.5, Write(ge), FadeIn(pn), run_time=1.0)

        # cue 114: the entropy gain = relative entropy of what we discard
        sr = SurroundingRectangle(pyth[2], color=ORANGE, buff=0.1, stroke_width=2)
        d1 = at_left(note("the gain is the relative entropy of what we discard:", 20, ORANGE), -2.0)
        d2 = at_left(chip("information about the bath", ORANGE, 24, fill=0.1), -2.65)
        self.at(114, Create(sr), FadeIn(d1), run_time=0.7)
        self.frac(114, 0.45, FadeIn(d2, shift=UP * 0.15), run_time=0.5)

        # cue 115: S(z) = k_B ln Vol(microstates consistent with z)
        for o in list(dots.get_family()):
            o.clear_updaters()
        tau.clear_updaters()
        cells_m.clear_updaters()
        self.at(115, *[FadeOut(m) for m in [pyth, ge, pn, sr, d1, d2, cg, dots, cells_m, cap, s_g, s_gn, s_c, s_cn, frame]],
                run_time=0.5)
        fiber = Rectangle(width=2.0, height=2.0, color=ORANGE, stroke_width=3, fill_color=ORANGE, fill_opacity=0.15)
        fiber.move_to(PC + [0, 0.3, 0])
        micro = VGroup(*[Dot(PC + [0, 0.3, 0] + [rng.uniform(-0.9, 0.9), rng.uniform(-0.9, 0.9), 0], radius=0.04, color=ORANGE)
                         for _ in range(40)])
        zpt = Dot(PC + [0, -1.6, 0], color=YELLOW, radius=0.1)
        zl = tex(r"z", 30, YELLOW).next_to(zpt, DOWN, buff=0.1)
        zar = DashedLine(zpt.get_top(), fiber.get_bottom(), color=YELLOW)
        fl = note("microstates consistent with z", 18, ORANGE).next_to(fiber, UP, buff=0.15)
        S1 = at_left(tex(r"S(z)=k_B\ln\mathrm{Vol}\{\text{microstates consistent with }z\}", 34), 1.5)
        clamp_width(S1, 7.4)
        S1n = at_left(note("Boltzmann: the log-volume of the microstates behind a reduced state", 20, GREY_B), 0.6)
        self.play(Create(fiber), FadeIn(micro), FadeIn(zpt), FadeIn(zl), Create(zar), FadeIn(fl), Write(S1), run_time=1.6)
        self.frac(115, 0.45, FadeIn(S1n), run_time=0.5)

        # cue 116: second law with exact energy conservation
        sd = at_left(tex(r"\dot S\ \ge\ 0", 54, GREEN), 1.5)
        ec = at_left(tex(r"E=\text{const}", 50, ORANGE), 0.4)
        sn = at_left(note("along the reduced dynamics, with the total energy exactly conserved", 20), -0.55)
        clamp_width(sn, 7.4)
        axp = Axes(x_range=[0, 6, 1], y_range=[0, 1.3, 1], x_length=4.8, y_length=3.0,
                   axis_config={"include_tip": False, "font_size": 14}).move_to(PC + [0, 0.2, 0])
        sc_ = axp.plot(lambda t: 1.0 - 0.7 * math.exp(-0.7 * t), x_range=[0, 6], color=GREEN)
        ec_ = axp.plot(lambda t: 0.6, x_range=[0, 6], color=ORANGE)
        sl = tex(r"S(t)", 26, GREEN).move_to(axp.c2p(5.3, 1.15))
        el = tex(r"E(t)", 26, ORANGE).move_to(axp.c2p(5.3, 0.42))
        self.at(116, *[FadeOut(m) for m in [S1, S1n, fiber, micro, zpt, zl, zar, fl]], run_time=0.4)
        self.play(Write(sd), Create(axp), run_time=1.0)
        self.frac(116, 0.35, Create(sc_), FadeIn(sl), Write(ec), run_time=1.4)
        self.frac(116, 0.6, Create(ec_), FadeIn(el), FadeIn(sn), run_time=1.0)

        # cue 117: the design target on the reduced space
        tgt = at_left(chip("the design target on the reduced space", YELLOW, 24, fill=0.1), -1.7)
        self.at(117, FadeIn(tgt, shift=UP * 0.15), run_time=0.5)

        # cue 118-119: we need Edot = 0, Sdot >= 0 for ANY E and S with the right compatibility
        self.at(118, *[FadeOut(m) for m in [sd, ec, sn, tgt, axp, sc_, ec_, sl, el]], run_time=0.5)
        nd = at_left(note("we need dynamics with", 26, GREY_B), 1.6)
        ee = at_left(tex(r"\dot E=0,\qquad \dot S\ge 0", 56, YELLOW), 0.5)
        self.play(FadeIn(nd), Write(ee), run_time=1.4)
        anyc = at_left(chip("for ANY E and S satisfying the right compatibility conditions", ORANGE, 22, fill=0.1), -1.0)
        clamp_width(anyc, 12.0)
        self.at(119, FadeIn(anyc, shift=UP * 0.15), run_time=0.6)
        self.snap("target")
        self.finish()


# ==========================================================================
# M9 -- Metriplectic dynamics: two brackets and the requirements  (cues 120-128)
# ==========================================================================
class M9(SyncedScene):
    AUDIO_START, AUDIO_END = START["M9"], END["M9"]

    def construct(self):
        header = hdr("Metriplectic Dynamics")
        self.at(120, Write(header), run_time=0.6)
        cards = [card("Kaufman & Morrison\n1984", BLUE, 24), card("Morrison\n1986, the full paradigm", PURPLE, 24),
                 card("Grmela & Öttinger\nGENERIC, independently", ORANGE, 24)]
        cg = VGroup(*cards).arrange(RIGHT, buff=0.5).move_to([0, 0.5, 0])
        self.frac(120, 0.15, FadeIn(cards[0], shift=UP * 0.2), run_time=0.6)
        self.frac(120, 0.4, FadeIn(cards[1], shift=UP * 0.2), run_time=0.6)
        self.frac(120, 0.7, FadeIn(cards[2], shift=UP * 0.2), run_time=0.6)

        # cue 121: two brackets on the same space
        pb = chip("Poisson bracket  {·,·}\nreversible physics", BLUE, 24)
        mb = chip("symmetric, positive-semidefinite\nbracket  (·,·):  irreversible physics", ORANGE, 24)
        plus = tex(r"+", 44)
        both = VGroup(pb, plus, mb).arrange(RIGHT, buff=0.4).move_to([0, 0.7, 0])
        self.at(121, FadeOut(cg), run_time=0.4)
        self.play(FadeIn(pb, shift=RIGHT * 0.2), FadeIn(plus), FadeIn(mb, shift=LEFT * 0.2), run_time=0.9)
        sp = note("two brackets on the same space", 24, YELLOW).next_to(both, DOWN, buff=0.5)
        self.play(FadeIn(sp), run_time=0.4)

        # cue 122: matrix form
        eq1 = MathTex(r"\dot z=", r"L(z)\nabla E", r"+", r"M(z)\nabla S", font_size=50)
        eq1[1].set_color(BLUE)
        eq1[3].set_color(ORANGE)
        eq1.move_to([0, 1.2, 0])
        self.at(122, FadeOut(sp), both.animate.scale(0.7).move_to([0, 2.1, 0]), run_time=0.6)
        self.play(Write(eq1), run_time=1.6)
        # cue 123: bracket form
        eq2 = MathTex(r"\dot f=", r"\{f,E\}", r"+", r"(f,S)", font_size=46)
        eq2[1].set_color(BLUE)
        eq2[3].set_color(ORANGE)
        eq2.move_to([0, 0.4, 0])
        self.at(123, Write(eq2), run_time=1.2)

        # cue 124-128: the requirements
        lab = at_left(note("the requirements", 22, YELLOW), -0.2)
        R1 = at_left(tex(r"L^{T}=-L,\qquad L\ \text{satisfies the Jacobi identity}", 32, BLUE), -0.75)
        R2 = at_left(tex(r"M^{T}=M\succeq 0", 32, ORANGE), -1.4)
        R2n = at_left(note("Onsager reciprocity (microscopic reversibility) + positivity (second law)", 18, GREY_B), -1.85)
        R3 = at_left(tex(r"M^{+}\ \text{is a Riemannian metric on}\ \mathrm{range}\,M", 28, ORANGE), -2.35)
        R4 = at_left(tex(r"L\nabla S=0,\qquad M\nabla E=0\qquad\text{(degeneracy conditions)}", 32, YELLOW), -3.05)
        for r in (R1, R2, R3, R4):
            clamp_width(r, 12.0)
        self.at(124, FadeIn(lab), run_time=0.4)
        self.at(125, Write(R1), run_time=1.6)
        self.at(126, Write(R2), run_time=1.0)
        self.frac(126, 0.4, FadeIn(R2n), run_time=0.6)
        self.at(127, Write(R3), run_time=1.6)
        self.at(128, Write(R4), run_time=1.6)
        self.snap("requirements")
        self.finish(keep=[header])


# ==========================================================================
# M9_1 -- degeneracy conditions; E and S laws  (cues 129-135)
# ==========================================================================
class M9_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M9_1"], END["M9_1"]

    def construct(self):
        header = hdr("Metriplectic Dynamics")
        self.add(header)
        t129 = Text("the degeneracy conditions have a clean interpretation", font_size=30, color=YELLOW).move_to([0, 0.4, 0])
        self.at(129, FadeIn(t129), run_time=0.6)

        CL, CR = np.array([-3.4, -0.1, 0.0]), np.array([3.4, -0.1, 0.0])
        # left: symplectic leaves of constant entropy
        leaves = leaf_stack(CL, ys=(-1.3, -0.45, 0.4, 1.25), w=3.2, d=0.5, h=0.55)
        hi = leaves[2]
        hi_c = hi.get_center()
        ph = ValueTracker(0.0)
        ph.add_updater(lambda m, dt: m.increment_value(1.4 * dt))
        ldot = always_redraw(lambda: Dot(hi_c + [1.0 * math.sin(ph.get_value()), 0, 0], radius=0.09, color=BLUE))
        lt = tex(r"L\nabla S=0", 40, BLUE).move_to([-3.4, 2.0, 0])
        lc1 = note("S is a Casimir of the Poisson bracket", 18, BLUE).move_to([-3.4, -2.3, 0])
        lc2 = note("reversible motion stays on a leaf\nof constant entropy", 18, BLUE).move_to([-3.4, -2.9, 0])
        sax = Arrow(CL + [-2.2, -1.5, 0], CL + [-2.2, 1.6, 0], buff=0, color=GREY_B, stroke_width=3)
        sl = tex(r"S", 24, GREY_B).next_to(sax, UP, buff=0.05)
        # right: constant-energy surfaces for the metric bracket
        rings = VGroup(*[Ellipse(width=2 * r, height=1.5 * r, color=GREY_B, stroke_width=2).move_to(CR) for r in (0.5, 1.0, 1.5)])
        hring = Ellipse(width=2.0, height=1.5, color=ORANGE, stroke_width=4).move_to(CR)
        ph2 = ValueTracker(0.0)
        ph2.add_updater(lambda m, dt: m.increment_value(1.4 * dt))
        rdot = always_redraw(lambda: Dot(CR + [1.0 * math.cos(ph2.get_value()), 0.75 * math.sin(ph2.get_value()), 0],
                                         radius=0.09, color=ORANGE))
        rt = tex(r"M\nabla E=0", 40, ORANGE).move_to([3.4, 2.0, 0])
        rc1 = note("E is a Casimir of the metric bracket", 18, ORANGE).move_to([3.4, -2.3, 0])
        rc2 = note("irreversible motion stays on a\nsurface of constant energy", 18, ORANGE).move_to([3.4, -2.9, 0])
        # cue 130-131
        self.at(130, FadeOut(t129), FadeIn(lt), Create(leaves), FadeIn(sax), FadeIn(sl), run_time=1.0)
        self.add(ph)
        self.play(FadeIn(lc1), run_time=0.4)
        self.at(131, hi.animate.set_fill(BLUE, opacity=0.5), FadeIn(ldot), FadeIn(lc2), run_time=0.8)
        # cue 132-133
        self.at(132, FadeIn(rt), Create(rings), FadeIn(rc1), run_time=1.0)
        self.add(ph2)
        self.at(133, Create(hring), FadeIn(rdot), FadeIn(rc2), run_time=0.8)
        self.snap("casimirs")

        # cue 134: equivalently, generator F = E + S, because the cross terms vanish
        panel = [lt, leaves, sax, sl, ldot, lc1, lc2, rt, rings, hring, rdot, rc1, rc2]
        self.at(134, *[FadeOut(m) for m in panel], run_time=0.5)
        ph.clear_updaters()
        ph2.clear_updaters()
        F1 = tex(r"\dot f=\{f,F\}+(f,F),\qquad F=E+S", 44, YELLOW).move_to([0, 1.6, 0])
        clamp_width(F1, 12.0)
        F2 = MathTex(r"\{f,S\}=0", r"\quad\text{and}\quad", r"(f,E)=0", font_size=38)
        F2[0].set_color(BLUE)
        F2[2].set_color(ORANGE)
        F2.move_to([0, 0.7, 0])
        F2n = note("the cross terms vanish", 22, GREY_B).move_to([0, 0.0, 0])
        self.play(Write(F1), run_time=1.4)
        self.frac(134, 0.5, Write(F2), FadeIn(F2n), run_time=1.4)

        # cue 135: the two laws, one line each
        self.at(135, FadeOut(F2n), F1.animate.scale(0.8).move_to([0, 2.2, 0]), F2.animate.scale(0.8).move_to([0, 1.5, 0]),
                run_time=0.6)
        eE = tex(r"\dot E=\underbrace{\{E,E\}}_{0}+\underbrace{(E,S)}_{=\,\nabla E\cdot M\nabla S\,=\,0}=0", 36, ORANGE)
        eS = tex(r"\dot S=\underbrace{\{S,E\}}_{0}+(S,S)=\nabla S\cdot M\nabla S\ \ge\ 0", 36, GREEN)
        for e_ in (eE, eS):
            clamp_width(e_, 12.0)
        eE.move_to([0, 0.2, 0])
        eS.move_to([0, -1.5, 0])
        self.play(Write(eE), run_time=1.6)
        self.frac(135, 0.45, Write(eS), run_time=1.6)
        fin = chip("first and second laws, by construction", YELLOW, 24, fill=0.1).move_to([0, -2.7, 0])
        self.frac(135, 0.8, FadeIn(fin, shift=UP * 0.15), run_time=0.5)
        self.snap("laws")
        self.finish(keep=[header])


# ==========================================================================
# M9_2 -- geometric picture, parity, the name  (cues 136-141)
# ==========================================================================
class M9_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["M9_2"], END["M9_2"]

    def construct(self):
        header = hdr("Metriplectic Dynamics")
        self.add(header)
        C = np.array([3.9, -0.2, 0.0])
        leaves = leaf_stack(C, ys=(-1.6, -0.55, 0.5, 1.55), w=3.7, d=0.6, h=0.6)
        shell = Polygon(C + [-0.6, -2.1, 0], C + [0.7, -2.1, 0], C + [1.1, 2.15, 0], C + [-0.2, 2.15, 0],
                        color=YELLOW, stroke_width=2, fill_color=YELLOW, fill_opacity=0.12)
        shl = note("energy shell E = const", 16, YELLOW).move_to(C + [0.5, -2.55, 0])
        sax = Arrow(C + [-2.5, -2.0, 0], C + [-2.5, 2.1, 0], buff=0, color=GREY_B, stroke_width=3)
        sl = tex(r"s", 26, GREY_B).next_to(sax, UP, buff=0.05)
        a_rev = Arrow(C + [-0.5, -0.55, 0], C + [0.9, -0.55, 0], buff=0, color=BLUE, stroke_width=6)
        a_irr = Arrow(C + [0.0, -1.5, 0], C + [0.55, 1.6, 0], buff=0, color=ORANGE, stroke_width=6)
        tL = at_left(Text("reversible motion slides along\nthe symplectic leaves", font_size=26, color=BLUE, line_spacing=0.9), 1.5)
        tM = at_left(Text("irreversible motion slides across leaves,\nwithin an energy shell", font_size=26, color=ORANGE, line_spacing=0.9), 0.2)
        tS = at_left(note("S increases monotonically\nin the crossing direction", 22, GREEN), -0.85)
        self.at(136, Create(leaves), FadeIn(sax), FadeIn(sl), Write(tL), run_time=1.4)
        self.play(GrowArrow(a_rev), run_time=0.7)
        self.at(137, Create(shell), FadeIn(shl), Write(tM), run_time=1.2)
        self.play(GrowArrow(a_irr), run_time=0.7)
        self.at(138, FadeIn(tS), Indicate(a_irr, color=GREEN, scale_factor=1.15), run_time=0.9)
        self.snap("geometry")

        # cue 139-140: parity
        pL = at_left(tex(r"L\text{-term:}\ \ \Theta_*\ \mapsto\ -1\quad\text{odd}", 32, BLUE), -1.6)
        pM = at_left(tex(r"M\text{-term:}\ \ \Theta_*\ \mapsto\ +1\quad\text{even}", 32, ORANGE), -2.3)
        clamp_width(pL, 7.4)
        clamp_width(pM, 7.4)
        self.at(139, Write(pL), run_time=1.0)
        self.frac(139, 0.5, Write(pM), run_time=1.0)
        same = chip("exactly the parity split we found for drag", YELLOW, 22, fill=0.1)
        at_left(same, -3.0)
        self.at(140, FadeIn(same, shift=UP * 0.15), run_time=0.5)

        # cue 141: metric + symplectic = metriplectic
        self.at(141, *[FadeOut(m) for m in [leaves, shell, shl, sax, sl, a_rev, a_irr, tL, tM, tS, pL, pM, same]], run_time=0.5)
        w1 = Text("metric", font_size=56, color=ORANGE)
        w2 = Text("+", font_size=56)
        w3 = Text("symplectic", font_size=56, color=BLUE)
        ws = VGroup(w1, w2, w3).arrange(RIGHT, buff=0.4).move_to([0, 0.5, 0])
        self.play(FadeIn(ws, shift=UP * 0.2), run_time=0.7)
        name = Text("metriplectic", font_size=72, color=YELLOW).move_to([0, 0.5, 0])
        self.frac(141, 0.5, ReplacementTransform(ws, name), run_time=0.9)
        self.snap("name")
        self.finish(keep=[header])


# ==========================================================================
# M10 -- the baseball, metriplectic version  (cues 142-148)
# ==========================================================================
class M10(SyncedScene):
    AUDIO_START, AUDIO_END = START["M10"], END["M10"]

    def construct(self):
        header = hdr("Metriplectic Dynamics")
        self.add(header)
        # cue 142: now the baseball
        t142 = Text("Now the baseball.", font_size=46, color=YELLOW).move_to([0, 0.9, 0])
        ball = baseball(0.4).move_to([0, -0.4, 0])
        self.at(142, FadeIn(t142, scale=0.9), FadeIn(ball), run_time=0.6)

        # cue 143: add one variable s (entropy of ball + air), U(s), T = U'(s)
        L1 = at_left(tex(r"z=(x,p,s)", 44, YELLOW), 1.9)
        L2 = at_left(note("s:  the entropy of the ball\nand the surrounding air", 22), 1.0)
        L3 = at_left(tex(r"U(s):\ \text{internal energy}", 32), 0.0)
        L4 = at_left(tex(r"T=U'(s)>0", 40, ORANGE), -0.9)
        axU = Axes(x_range=[0, 2.2, 1], y_range=[0, 6, 2], x_length=4.4, y_length=3.4,
                   axis_config={"include_tip": False, "font_size": 14}).move_to([4.0, 0.2, 0])
        U = lambda s_: 0.5 * math.exp(1.1 * s_)
        cu = axU.plot(U, x_range=[0, 2.2], color=BLUE)
        s0 = 1.2
        sl0 = 0.55 * math.exp(1.1 * s0)
        tg = axU.plot(lambda s_: U(s0) + sl0 * (s_ - s0), x_range=[s0 - 0.8, s0 + 0.8], color=ORANGE)
        Ul = tex(r"U(s)", 28, BLUE).next_to(axU, UP, buff=0.05).align_to(axU, LEFT)
        sxl = tex(r"s", 24, GREY_B).next_to(axU, RIGHT, buff=0.05)
        Tl = tex(r"T=\text{slope}", 24, ORANGE).move_to(axU.c2p(1.75, 1.0))
        self.at(143, FadeOut(t142), FadeOut(ball), Write(L1), run_time=0.8)
        self.frac(143, 0.2, FadeIn(L2), run_time=0.6)
        self.frac(143, 0.42, Write(L3), Create(axU), Create(cu), FadeIn(Ul), FadeIn(sxl), run_time=1.4)
        self.frac(143, 0.7, Write(L4), Create(tg), FadeIn(Tl), run_time=1.2)

        # cue 144: E, S, gradients, L and M
        eS = at_left(tex(r"E=\frac{p^2}{2m}+U(s),\qquad S=s", 36), 1.9)
        grad = at_left(tex(r"\nabla E=\begin{pmatrix}0\\p/m\\T\end{pmatrix},\quad\nabla S=\begin{pmatrix}0\\0\\1\end{pmatrix}", 32), 0.3)
        Lm = tex(r"L=\begin{pmatrix}0&1&0\\-1&0&0\\0&0&0\end{pmatrix}", 34, BLUE).move_to([3.9, 1.5, 0])
        Mm = tex(r"M=\frac bT\,vv^{T},\quad v=\begin{pmatrix}0\\T\\-p/m\end{pmatrix}", 32, ORANGE)
        clamp_width(Mm, 5.2)
        Mm.move_to([3.9, -0.5, 0])
        self.at(144, *[FadeOut(m) for m in [L1, L2, L3, L4, axU, cu, tg, Ul, sxl, Tl]], run_time=0.3)
        self.play(Write(eS), Write(grad), run_time=0.8)
        self.play(Write(Lm), Write(Mm), run_time=0.9)

        # cue 145: degeneracy conditions hold
        ck1 = at_left(tex(r"L\nabla S=0\quad(s\ \text{is a Casimir of }L)", 30, BLUE), -1.3)
        ck2 = at_left(tex(r"M\nabla E=\frac bT\,v\,(v\cdot\nabla E)=0", 30, ORANGE), -2.1)
        ck3 = at_left(tex(r"v\cdot\nabla E=\frac{Tp}{m}-\frac{Tp}{m}=0", 28), -2.85)
        ok = chip("degeneracy conditions hold", GREEN, 24, fill=0.1).move_to([3.9, -2.0, 0])
        self.at(145, Write(ck1), run_time=1.2)
        self.frac(145, 0.3, Write(ck2), run_time=1.4)
        self.frac(145, 0.6, Write(ck3), run_time=1.2)
        self.frac(145, 0.8, FadeIn(ok, shift=UP * 0.15), run_time=0.5)
        self.snap("setup")

        # cue 146: the equations of motion
        e1 = at_left(tex(r"\dot x=\frac pm", 44), 1.95)
        e2 = at_left(tex(r"\dot p=-\frac bm\,p", 44), 0.7)
        e3 = at_left(tex(r"\dot s=\frac{b\,p^2}{m^2T}", 44, ORANGE), -0.75)
        tau = ValueTracker(0.0)
        rate = [0.0]
        tau.add_updater(lambda m, dt: m.set_value(min(m.get_value() + rate[0] * dt, 6.0)))
        track = Line([1.6, 1.85, 0], [6.2, 1.85, 0], color=GREY_B, stroke_width=2)
        bb = baseball(0.22)
        bb.add_updater(lambda m: m.move_to([1.9 + 3.9 * (1 - math.exp(-tau.get_value())), 2.15, 0]))
        H = 2.3
        base_y = -2.8
        pbar = always_redraw(lambda: Rectangle(width=0.7, height=max(H * math.exp(-tau.get_value()), 0.01), stroke_width=2,
                                               color=BLUE, fill_color=BLUE, fill_opacity=0.5)
                             .move_to([2.9, base_y, 0], aligned_edge=DOWN))
        sbar = always_redraw(lambda: Rectangle(width=0.7, height=max(H * (1 - math.exp(-2 * tau.get_value())), 0.01),
                                               stroke_width=2, color=ORANGE, fill_color=ORANGE, fill_opacity=0.5)
                             .move_to([5.0, base_y, 0], aligned_edge=DOWN))
        pfr = Rectangle(width=0.7, height=H, color=GREY_B, stroke_width=1).move_to([2.9, base_y, 0], aligned_edge=DOWN)
        sfr = Rectangle(width=0.7, height=H, color=GREY_B, stroke_width=1).move_to([5.0, base_y, 0], aligned_edge=DOWN)
        pl = tex(r"p", 28, BLUE).next_to(pfr, DOWN, buff=0.08)
        sl = tex(r"s", 28, ORANGE).next_to(sfr, DOWN, buff=0.08)
        pt = note("momentum drains", 16, BLUE).next_to(pfr, UP, buff=0.1)
        st = note("entropy grows", 16, ORANGE).next_to(sfr, UP, buff=0.1)
        self.at(146, *[FadeOut(m) for m in [eS, grad, Lm, Mm, ck1, ck2, ck3, ok]], run_time=0.4)
        self.add(tau)
        self.play(Write(e1), Write(e2), FadeIn(track), FadeIn(bb), Create(pfr), Create(sfr), FadeIn(pl), FadeIn(sl),
                  FadeIn(pbar), FadeIn(sbar), FadeIn(pt), FadeIn(st), run_time=1.0)
        rate[0] = 0.28
        self.play(Write(e3), run_time=1.0)
        orig = SurroundingRectangle(VGroup(e1, e2), color=YELLOW, buff=0.15, stroke_width=2)
        on = note("the original drag equations", 20, YELLOW).move_to([-0.7, 1.35, 0])
        self.frac(146, 0.7, Create(orig), FadeIn(on), run_time=0.6)

        # cue 147: ... now with the physical momentum
        phys = note("now with the physical momentum", 20, GREEN).move_to([-0.5, 0.7, 0])
        self.at(147, FadeIn(phys), run_time=0.5)

        # cue 148: no explicit time dependence, nondegenerate bracket on the mechanical block
        c1 = at_left(chip("no explicit time dependence", GREEN, 24, fill=0.1), -2.1)
        c2 = at_left(chip("non-degenerate bracket on the (x, p) block", GREEN, 24, fill=0.1), -2.9)
        self.at(148, FadeIn(c1, shift=UP * 0.15), run_time=0.5)
        self.frac(148, 0.5, FadeIn(c2, shift=UP * 0.15), run_time=0.5)
        self.snap("eom")
        bb.clear_updaters()
        self.finish(keep=[header])


# ==========================================================================
# M10_1 -- entropy production, LaSalle, the attractor exists  (cues 149-156)
# ==========================================================================
class M10_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M10_1"], END["M10_1"]

    def construct(self):
        header = hdr("Metriplectic Dynamics")
        self.add(header)
        # cue 149: dS = dQ / T
        s1 = at_left(tex(r"\dot s=\frac{b\,p^2}{m^2T}=\frac{b\,\dot x^2}{T}", 46, ORANGE), 1.6)
        c1 = at_left(chip("dS = δQ / T", GREEN, 28, fill=0.1), 0.5)
        BC = np.array([4.1, 1.2, 0.0])
        ball = baseball(0.35).move_to(BC)
        air = DashedVMobject(Ellipse(width=3.2, height=2.2, color=TEAL).move_to(BC + [0.9, 0, 0]), num_dashes=40)
        al = note("ball + surrounding air", 16, TEAL).move_to(BC + [0.9, -1.4, 0])
        heat = Arrow(BC + [0.4, 0, 0], BC + [1.7, 0, 0], buff=0, color=ORANGE, stroke_width=5)
        hl = tex(r"\delta Q", 26, ORANGE).next_to(heat, UP, buff=0.05)
        self.at(149, Write(s1), FadeIn(ball), Create(air), FadeIn(al), run_time=1.6)
        self.frac(149, 0.45, FadeIn(c1, shift=UP * 0.15), GrowArrow(heat), FadeIn(hl), run_time=0.8)
        # cue 150: dissipated power b xdot^2
        q1 = at_left(tex(r"\text{dissipated power}\ \ \dot Q=b\,\dot x^{2}", 40, ORANGE), -0.55)
        self.at(150, Write(q1), run_time=1.4)
        # cue 151: energy conservation, explicitly
        e1 = at_left(tex(r"\dot E=\frac{p\,\dot p}{m}+T\dot s=-\frac{b\,p^2}{m^2}+\frac{b\,p^2}{m^2}=0", 34, GREEN), -1.7)
        clamp_width(e1, 12.0)
        c2 = at_left(chip("energy conservation checks out explicitly", GREEN, 24, fill=0.1), -2.55)
        self.at(151, Write(e1), run_time=1.8)
        self.frac(151, 0.7, FadeIn(c2, shift=UP * 0.15), run_time=0.5)
        self.snap("heat")

        # cue 152: Sdot vanishes only at p = 0; LaSalle
        PCr = np.array([3.9, 0.7, 0.0])
        fr = phase_frame(PCr, 5.2, 2.8, "x", "p")
        att = DashedLine(PCr + [-2.5, 0, 0], PCr + [2.5, 0, 0], color=RED, stroke_width=4)
        attl = note("p = 0", 16, RED).move_to(PCr + [-2.0, -0.25, 0])
        starts = [(-2.0, 1.0), (-1.4, -1.0), (-0.3, 1.2), (0.2, -0.7), (-0.9, 0.5), (0.8, 0.9)]
        px = lambda x, p: PCr + [x * 0.55, p * 0.95, 0]
        trajs = VGroup(*[ParametricFunction(lambda t, x0=x0, p0=p0: px(x0 + p0 * (1 - math.exp(-t)), p0 * math.exp(-t)),
                                            t_range=[0, 4], color=TEAL, stroke_width=3) for x0, p0 in starts])
        tau = ValueTracker(0.0)
        rate = [0.0]
        tau.add_updater(lambda m, dt: m.set_value(min(m.get_value() + rate[0] * dt, 4.0)))
        movers = always_redraw(lambda: VGroup(*[Dot(px(x0 + p0 * (1 - math.exp(-tau.get_value())),
                                                          p0 * math.exp(-tau.get_value())), radius=0.08, color=YELLOW)
                                                for x0, p0 in starts]))
        d1 = at_left(tex(r"\dot S=\frac bT\left(\frac pm\right)^2\ \ge\ 0", 44, GREEN), 1.8)
        d2 = at_left(note("vanishes only at p = 0", 24, RED), 0.85)
        d3 = at_left(chip("LaSalle's invariance principle", PURPLE, 26, fill=0.1), 0.0)
        self.at(152, *[FadeOut(m) for m in [s1, c1, q1, e1, c2, ball, air, al, heat, hl]], run_time=0.5)
        self.play(Write(d1), Create(fr), Create(att), FadeIn(attl), run_time=1.2)
        self.frac(152, 0.5, FadeIn(d2), FadeIn(d3, shift=UP * 0.15), run_time=0.8)

        # cue 153: every trajectory converges to {p = 0}
        d4 = at_left(chip("every trajectory converges to {p = 0}", RED, 26, fill=0.1), -0.9)
        self.at(153, Create(trajs), FadeIn(d4, shift=UP * 0.15), run_time=1.2)
        self.add(tau, movers)
        rate[0] = 0.5
        self.wait(0.2)

        # cue 154: the final entropy, fixed by energy conservation
        ax = Axes(x_range=[0, 5, 1], y_range=[0, 1.3, 1], x_length=4.6, y_length=1.5,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([3.9, -2.05, 0])
        scurve = ax.plot(lambda t: 0.25 + 0.7 * (1 - math.exp(-1.2 * t)), x_range=[0, 5], color=GREEN)
        sinf = DashedLine(ax.c2p(0, 0.95), ax.c2p(5, 0.95), color=YELLOW, stroke_width=2)
        sil = tex(r"s_\infty", 24, YELLOW).next_to(sinf, RIGHT, buff=0.05)
        stl = tex(r"s(t)", 24, GREEN).move_to(ax.c2p(1.0, 0.95))
        u1 = at_left(tex(r"U(s_\infty)=E_0", 46, YELLOW), -1.9)
        u2 = at_left(note("the final entropy is fixed by energy conservation", 20, GREY_B), -2.6)
        self.at(154, Write(u1), Create(ax), Create(scurve), Create(sinf), FadeIn(sil), FadeIn(stl), run_time=1.4)
        self.play(FadeIn(u2), run_time=0.5)
        self.snap("lasalle")

        # cue 155: the attractor that Hamiltonian mechanics forbids now exists
        tau.clear_updaters()
        self.remove(movers)
        self.at(155, *[FadeOut(m) for m in [d1, d2, d3, d4, u1, u2, fr, att, attl, trajs, ax, scurve, sinf, sil, stl]],
                run_time=0.4)
        big = Text("The attractor that Hamiltonian mechanics\nforbids now exists.", font_size=36, color=YELLOW,
                   line_spacing=0.9).move_to([0, 0.4, 0])
        self.play(FadeIn(big, scale=0.95), run_time=0.6)

        # cue 156: the phase-space volume lost in (x,p) has moved into s
        t2 = ValueTracker(0.0)
        t2.add_updater(lambda m, dt: m.set_value(min(m.get_value() + 0.5 * dt, 4.5)))
        base = blob_base(-1.2, 0.7, 0.62)
        PL = np.array([-3.4, 0.1, 0.0])
        fL, bL, aL = blob_panel(PL, 4.8, 3.2, t2, flow_drag, base, ORANGE, sc=1.0, xl="x", yl="p")
        a0 = shoelace(base)
        lL, nL = area_label("area =", aL, ORANGE, PL + [-0.4, -2.2, 0])
        tL = note("(x, p): volume lost", 20, ORANGE).move_to(PL + [0, 1.95, 0])
        arr = Arrow([-0.4, 0.1, 0], [0.7, 0.1, 0], buff=0, color=YELLOW, stroke_width=5)
        PR = np.array([3.6, 0.1, 0.0])
        Hh = 2.8
        sfr = Rectangle(width=1.0, height=Hh, color=GREY_B, stroke_width=2).move_to(PR + [0, -Hh / 2 + 0.3 + 1.1, 0])
        sfr.move_to(PR + [0, 0, 0])
        sbar = always_redraw(lambda: Rectangle(width=1.0, height=max(Hh * (1 - aL() / a0), 0.01), stroke_width=2,
                                               color=GREEN, fill_color=GREEN, fill_opacity=0.5)
                             .move_to(sfr.get_bottom(), aligned_edge=DOWN))
        tR = note("s: entropy gained", 20, GREEN).move_to(PR + [0, 1.95, 0])
        keep = chip("moved into s, not destroyed", YELLOW, 28, fill=0.1).move_to([0, -3.05, 0])
        self.at(156, FadeOut(big), run_time=0.4)
        self.add(t2)
        self.play(Create(fL), FadeIn(bL), FadeIn(lL), FadeIn(nL), FadeIn(tL), GrowArrow(arr), Create(sfr), FadeIn(sbar),
                  FadeIn(tR), run_time=1.0)
        self.frac(156, 0.55, FadeIn(keep, shift=UP * 0.15), run_time=0.6)
        self.snap("moved")
        self.finish()


# ==========================================================================
# M11 -- conclusion  (cues 157-162)
# ==========================================================================
class M11(SyncedScene):
    AUDIO_START, AUDIO_END = START["M11"], END["M11"]

    def construct(self):
        header = hdr("Conclusion and a Look Ahead")
        self.at(157, Write(header), run_time=0.6)
        c0 = at_left(chip("Hamiltonian system  =  Poisson structure + a generator", BLUE, 26), 1.7)
        self.sync(CUES[157].start + 0.9)
        self.play(FadeIn(c0, shift=UP * 0.15), run_time=0.6)
        r1 = at_left(tex(r"\text{antisymmetric bracket}\ \Rightarrow\ \dot H=0", 38), 0.5)
        self.frac(157, 0.5, Write(r1), run_time=1.4)
        r2 = at_left(tex(r"\text{Cartan's formula}\ \Rightarrow\ \mathcal L_{X_H}\omega=0", 38), -0.6)
        self.at(158, Write(r2), run_time=1.6)
        r3 = at_left(tex(r"\text{spectrum symmetric:}\ \ \lambda\leftrightarrow-\lambda", 38), -1.7)
        self.frac(158, 0.5, Write(r3), run_time=1.4)
        self.snap("recap")

        # cue 159: every attempt to write drag as Hamiltonian pays somewhere
        t159 = at_left(Text("every attempt to write drag as a\nHamiltonian system pays somewhere:", font_size=28,
                            color=YELLOW, line_spacing=0.9), 1.6)
        p1 = at_left(chip("explicit time dependence + non-symplectic coordinates", RED, 24, fill=0.08), 0.2)
        p2 = at_left(chip("a Poisson structure that goes singular on the attractor", RED, 24, fill=0.08), -0.8)
        self.at(159, *[FadeOut(m) for m in [c0, r1, r2, r3]], run_time=0.5)
        self.play(FadeIn(t159), run_time=0.6)
        self.frac(159, 0.35, FadeIn(p1, shift=UP * 0.15), run_time=0.6)
        self.frac(159, 0.65, FadeIn(p2, shift=UP * 0.15), run_time=0.6)

        # cue 160: real dissipation is a projection: information flowing into untracked degrees of freedom
        BC = np.array([-3.3, -1.7, 0.0])
        ball = baseball(0.4).move_to(BC)
        rng = np.random.default_rng(11)
        bath = VGroup(*[Dot([rng.uniform(1.6, 5.4), rng.uniform(-2.7, -0.7), 0], radius=0.05, color=TEAL) for _ in range(46)])
        bfr = DashedVMobject(Rectangle(width=4.2, height=2.4, color=TEAL).move_to([3.5, -1.7, 0]), num_dashes=44)
        fl = VGroup(*[Arrow(BC + [0.6, dy, 0], [1.5, -1.7 + 1.2 * dy, 0], buff=0, color=ORANGE, stroke_width=3,
                            max_tip_length_to_length_ratio=0.12) for dy in (-0.5, 0.0, 0.5)])
        il = note("information", 18, ORANGE).move_to([-0.2, -0.65, 0])
        bl = note("degrees of freedom we do not track", 16, TEAL).move_to([3.5, -3.1, 0])
        t160 = at_left(Text("real dissipation is a projection", font_size=34, color=YELLOW), 1.5)
        n160 = at_left(note("information flowing into degrees of freedom we do not track", 22), 0.7)
        self.at(160, *[FadeOut(m) for m in [t159, p1, p2]], run_time=0.5)
        self.play(Write(t160), FadeIn(ball), run_time=0.9)
        self.frac(160, 0.35, FadeIn(n160), Create(bfr), FadeIn(bath), FadeIn(bl), run_time=0.9)
        self.frac(160, 0.65, LaggedStart(*[GrowArrow(a) for a in fl], lag_ratio=0.25), FadeIn(il), run_time=1.2)

        # cue 161: entropy is a ledger
        led = chip("entropy is the ledger", YELLOW, 34, fill=0.1).move_to([0, 0.0, 0])
        self.at(161, FadeIn(led, scale=0.9), run_time=0.5)

        # cue 162: metriplectic dynamics joins the two brackets
        self.at(162, *[FadeOut(m) for m in [t160, n160, ball, bath, bfr, fl, il, bl, led]], run_time=0.5)
        f1 = tex(r"\dot f=\{f,E\}+(f,S)", 54).move_to([0, 1.5, 0])
        f1.set_color_by_tex(r"\{f,E\}", BLUE)
        f1.set_color_by_tex(r"(f,S)", ORANGE)
        f2 = tex(r"L\nabla S=0,\qquad M\nabla E=0", 44, YELLOW).move_to([0, 0.15, 0])
        f3 = tex(r"\dot E=0,\qquad \dot S\ge 0", 54, GREEN).move_to([0, -1.25, 0])
        fin = chip("the first and second laws follow by construction", YELLOW, 26, fill=0.1).move_to([0, -2.5, 0])
        self.play(Write(f1), run_time=1.2)
        self.frac(162, 0.3, Write(f2), run_time=1.4)
        self.frac(162, 0.55, Write(f3), run_time=1.4)
        self.frac(162, 0.8, FadeIn(fin, shift=UP * 0.15), run_time=0.6)
        self.snap("final")
        self.finish(keep=[header])


# ==========================================================================
# M11_1 -- look ahead  (cues 163-167)
# ==========================================================================
class M11_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["M11_1"], END["M11_1"]

    def construct(self):
        header = hdr("Conclusion and a Look Ahead")
        self.add(header)
        t163 = Text("several directions open up from here", font_size=38, color=YELLOW).move_to([0, 0.4, 0])
        self.at(163, FadeIn(t163, scale=0.95), run_time=0.6)

        # cue 164: Morrison's four-bracket
        h1 = at_left(Text("1   Morrison's 4-bracket", font_size=32, color=BLUE), 1.8)
        s1 = at_left(note("replaces the symmetric bracket with a curvature-like object", 22), 1.0)
        f4 = tex(r"(f,g;k,n)", 52, BLUE).move_to([-2.5, -0.1, 0])
        sy = tex(r"(f,g;k,n)=-(g,f;k,n)=-(f,g;n,k)=(k,n;f,g)", 32)
        clamp_width(sy, 12.0)
        sy.move_to([0, -1.2, 0])
        syn = note("Riemann-tensor symmetries: a more systematic structure", 20, GREY_B).move_to([0, -2.0, 0])
        self.at(164, FadeOut(t163), FadeIn(h1), run_time=0.6)
        self.play(FadeIn(s1), run_time=0.5)
        self.frac(164, 0.35, Write(f4), run_time=1.0)
        self.frac(164, 0.55, Write(sy), run_time=2.0)
        self.frac(164, 0.8, FadeIn(syn), run_time=0.5)

        # cue 165: continuum systems, then numerics
        self.at(165, *[FadeOut(m) for m in [h1, s1, f4, sy, syn]], run_time=0.5)
        h2 = at_left(Text("2   Continuum systems", font_size=32, color=ORANGE), 1.8)
        a1 = at_left(tex(r"L\ \longrightarrow\ \text{Lie--Poisson bracket}", 34, BLUE), 0.9)
        a2 = at_left(tex(r"M\ \longrightarrow\ \text{Landau collision operator (plasmas)}", 34, ORANGE), 0.1)
        a3 = at_left(tex(r"M\ \longrightarrow\ \text{viscosity and heat conduction (fluids)}", 34, ORANGE), -0.7)
        for a in (a1, a2, a3):
            clamp_width(a, 12.0)
        self.play(FadeIn(h2), run_time=0.4)
        self.frac(165, 0.1, Write(a1), run_time=1.2)
        self.frac(165, 0.22, Write(a2), run_time=1.4)
        self.frac(165, 0.34, Write(a3), run_time=1.4)
        # numerics: structure preserving discretisations
        self.frac(165, 0.5, *[FadeOut(m) for m in [h2, a1, a2, a3]], run_time=0.4)
        h3 = at_left(Text("3   Structure-preserving numerics", font_size=32, color=GREEN), 1.8)
        n1 = at_left(note("discrete-gradient methods:", 24), 1.0)
        n2 = at_left(tex(r"E\ \text{conserved,}\quad S\ \text{monotone, at the discrete level}", 34, GREEN), 0.2)
        clamp_width(n2, 6.6)
        # an explicit-Euler oscillator drifts in energy; the discrete-gradient (midpoint) one does not
        steps, h = 60, 0.12
        q, p = 1.0, 0.0
        Eeu = [0.5]
        for _ in range(steps):
            q, p = q + h * p, p - h * q
            Eeu.append(0.5 * (q * q + p * p))
        ax = Axes(x_range=[0, steps, 20], y_range=[0, 1.6, 0.5], x_length=4.8, y_length=2.6,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([3.9, -0.4, 0])
        c_bad = ax.plot(lambda k: Eeu[int(round(k))], x_range=[0, steps, 1], color=RED, use_smoothing=False)
        c_ok = ax.plot(lambda k: 0.5, x_range=[0, steps], color=GREEN)
        lb = note("standard scheme: E drifts", 16, RED).move_to(ax.c2p(22, 1.35))
        lg = note("discrete gradient: E conserved", 16, GREEN).move_to(ax.c2p(33, 0.3))
        tl = note("time step", 14, GREY_B).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT)
        self.play(FadeIn(h3), FadeIn(n1), run_time=0.5)
        self.frac(165, 0.62, Write(n2), Create(ax), FadeIn(tl), run_time=1.4)
        self.frac(165, 0.78, Create(c_bad), FadeIn(lb), Create(c_ok), FadeIn(lg), run_time=1.6)
        self.snap("numerics")

        # cue 166: next videos
        nxt = chip("in the next videos: these ideas, explored further", YELLOW, 28, fill=0.1)
        self.at(166, *[FadeOut(m) for m in [h3, n1, n2, ax, c_bad, c_ok, lb, lg, tl]], run_time=0.5)
        nxt.move_to([0, 0.4, 0])
        self.play(FadeIn(nxt, shift=UP * 0.15), run_time=0.6)

        # cue 167: thank you
        ty = Text("thank you for watching", font_size=46, color=YELLOW).move_to([0, 0.4, 0])
        self.at(167, FadeOut(nxt), FadeIn(ty, scale=0.95), run_time=0.6)
        self.finish()
