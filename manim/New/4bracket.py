#!/usr/bin/env python3

"""
metriplectic_4bracket.py
================================================================================
Manim animation for "Metriplectic Dynamics, part 2: the metriplectic 4-bracket",
synchronized to the narration track (4bracket.srt).  Built on the same machinery
as metriplectic_1.py (SyncedScene, cue-driven PLAN, Emacs-buffer chrome).

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh metriplectic_4bracket.py F0
    ...                       or: ./render_all.sh

Scene map (script section -> scene -> narration cues in 4bracket.srt)
    F0     title card, prerequisite (part 1)                   cues   1-  2
    F1     2-bracket -> 4-bracket, scope, the paper            cues   3-  8
    F2     review: Hamiltonian + metric, two laws              cues   9- 13
    F2_1   Casimir, free energy, metriplectic equations        cues  14- 22
    F3     structure of the dissipative bracket                cues  23- 27
    F3_1   why the 2-bracket falls short                       cues  28- 37
    F4     curvature tensor, metric, Riemann symmetries        cues  38- 46
    F4_1   the metriplectic 4-bracket (f,k;g,n)                cues  47- 52
    F4_2   algebraic properties                                cues  53- 56
    F4_3   contravariant sectional curvature, K >= 0           cues  57- 62
    F5     dissipative dynamics, E conserved, S increases      cues  63- 71
    F6     Lie-metriplectic manifolds                          cues  72- 80
    F7     Kulkarni-Nomizu construction                        cues  81- 88
    F8     UTA: algorithm, dynamical variables                 cues  89- 93
    F8_1   UTA: energy/entropy, Poisson, K-N, general solution cues  94-100
    F9     next directions                                     cues 101-106

Audio sync, SNAP debugging, the late-beat report and the emacs_theme fallback
all work exactly as in metriplectic_1.py (see its docstring).
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
_TPL.add_to_preamble(r"\newcommand{\KN}{\mathbin{\bigcirc\mkern-15mu\wedge}}")
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
    cands = [os.environ.get("BRACKET4_SRT")]
    try:
        cands.append(Path(__file__).with_name("4bracket.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "4bracket.srt",
              Path("~/Music/4bracket.srt").expanduser(),
              Path("/mnt/user-data/uploads/4bracket.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError("4bracket.srt not found (set BRACKET4_SRT=/path/to/file)")


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
# start of the next scene's first cue (F0 starts at t = 0).
PLAN = [("F0", 1), ("F1", 3), ("F2", 9), ("F2_1", 14), ("F3", 23), ("F3_1", 28),
        ("F4", 38), ("F4_1", 47), ("F4_2", 53), ("F4_3", 57), ("F5", 63),
        ("F6", 72), ("F7", 81), ("F8", 89), ("F8_1", 94), ("F9", 101)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="4bracket.tex"):
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

# --------------------------------------------------------------------------
# 4-bracket specific building blocks
# --------------------------------------------------------------------------
C_F, C_K, C_G, C_N = BLUE, GREEN, ORANGE, PURPLE     # the four slots (f,k;g,n)
C_POI, C_MET = BLUE, ORANGE                          # Poisson / metric (as in part 1)


def br4(size=46, args=("f", "k", "g", "n")):
    """(f,k;g,n) with the four slots colour-coded."""
    m = MathTex("(", args[0], ",", args[1], ";", args[2], ",", args[3], ")", font_size=size)
    for i, c in ((1, C_F), (3, C_K), (5, C_G), (7, C_N)):
        m[i].set_color(c)
    return m


def tick():
    return Text("✓", font_size=30, color=GREEN)


def cross():
    return Text("✗", font_size=30, color=RED)


# ==========================================================================
# F0 -- title card + prerequisite   (cues 1-2)
# ==========================================================================
class F0(SyncedScene):
    AUDIO_START, AUDIO_END = START["F0"], END["F0"]
    STARS = (40, 7)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Metriplectic dynamics, part 2: the 4-bracket", FG)], font_size=26)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: "Welcome back to our series on Metriplectic Dynamics."
        self.at(1, Write(title), run_time=1.2)
        big = br4(96).move_to([0, -0.2, 0])
        self.sync(CUES[1].start + 1.2)
        self.play(Write(big), run_time=1.4)

        # cue 2: watch the previous video; we assume that background
        self.at(2, FadeOut(big), run_time=0.5)
        prev = card("Part 1\nwhy Hamiltonian dynamics forbids dissipation", BLUE, 26, 8.2)
        prev.move_to([0, 0.9, 0])
        c1 = chip("Poisson bracket", BLUE, 22)
        c2 = chip("Casimirs", BLUE, 22)
        c3 = chip("two-bracket idea", BLUE, 22)
        row = VGroup(c1, c2, c3).arrange(RIGHT, buff=0.5).move_to([0, -1.3, 0])
        self.play(FadeIn(prev, shift=UP * 0.2), run_time=0.7)
        self.frac(2, 0.4, LaggedStart(*[FadeIn(c, shift=UP * 0.15) for c in row], lag_ratio=0.25),
                  run_time=1.2)
        a = note("assumed background for this video", 22, YELLOW).move_to([0, -2.5, 0])
        self.frac(2, 0.7, FadeIn(a), run_time=0.5)
        self.snap("prereq")
        self.finish()


# ==========================================================================
# F1 -- from a 2-bracket to a 4-bracket; scope  (cues 3-8)
# ==========================================================================
class F1(SyncedScene):
    AUDIO_START, AUDIO_END = START["F1"], END["F1"]

    def construct(self):
        header = hdr("Introduction")
        # cue 3: extend from a binary bracket into a four bracket to expand the geometry
        self.at(3, Write(header), run_time=0.6)
        two = tex(r"(f,g)", 72)
        two_l = note("binary bracket", 24)
        four = br4(72)
        four_l = note("four-bracket", 24, YELLOW)
        A = VGroup(two, two_l).arrange(DOWN, buff=0.35).move_to([-3.6, 0.4, 0])
        B = VGroup(four, four_l).arrange(DOWN, buff=0.35).move_to([3.6, 0.4, 0])
        arr = Arrow(A.get_right() + RIGHT * 0.2, B.get_left() + LEFT * 0.2, buff=0, color=WHITE)
        geo = note("expands the geometry we can use", 26, TEAL).move_to([0, -1.7, 0])
        self.sync(CUES[3].start + 0.8)
        self.play(FadeIn(A, shift=UP * 0.2), run_time=0.6)
        self.frac(3, 0.35, GrowArrow(arr), FadeIn(B, shift=LEFT * 0.3), run_time=1.0)
        self.frac(3, 0.7, FadeIn(geo), run_time=0.5)

        # cue 4-7: theoretical examination, based on the Morrison & Updike paper
        self.at(4, *[FadeOut(m) for m in [A, B, arr, geo]], run_time=0.5)
        t4 = Text("a theoretical examination of the 4-bracket", font_size=34, color=YELLOW).move_to([0, 1.4, 0])
        self.play(FadeIn(t4, shift=UP * 0.15), run_time=0.6)
        paper = card("Morrison & Updike\nInclusive curvaturelike framework for describing dissipation:\n"
                     "Metriplectic 4-bracket dynamics", TEAL, 27, 11.4)
        paper.move_to([0, -0.5, 0])
        self.at(5, FadeIn(paper, shift=UP * 0.2), run_time=0.8)
        self.at(6, Indicate(paper, color=YELLOW, scale_factor=1.03), run_time=0.8)
        rd = note("closely based on the paper -- worth reading", 24, GREY_B).move_to([0, -2.4, 0])
        self.at(7, FadeIn(rd), run_time=0.5)

        # cue 8: finite-dimensional systems + minimal UTA introduction
        self.at(8, *[FadeOut(m) for m in [t4, paper, rd]], run_time=0.5)
        c1 = chip("finite-dimensional metriplectic systems", C_F, 30, fill=0.08)
        c2 = chip("minimal intro: Unified Thermodynamics Algorithm (UTA)", C_G, 30, fill=0.08)
        pair = VGroup(c1, c2).arrange(DOWN, buff=0.6).move_to([0, 0.3, 0])
        clamp_width(pair)
        self.play(FadeIn(c1, shift=UP * 0.2), run_time=0.6)
        self.frac(8, 0.5, FadeIn(c2, shift=UP * 0.2), run_time=0.6)
        self.snap("scope")
        self.finish()


# ==========================================================================
# F2 -- quick review of metriplectic systems  (cues 9-13)
# ==========================================================================
class F2(SyncedScene):
    AUDIO_START, AUDIO_END = START["F2"], END["F2"]

    def construct(self):
        header = hdr("Review of Metriplectic")
        # cue 9: expand Hamiltonian dynamics into dissipative / irreversible systems
        ham = chip("Hamiltonian dynamics", C_POI, 28, fill=0.08)
        dis = chip("dissipative, irreversible systems", C_MET, 28, fill=0.08)
        row = VGroup(ham, dis).arrange(RIGHT, buff=2.2).move_to([0, 1.2, 0])
        clamp_width(row)
        arr = Arrow(ham.get_right(), dis.get_left(), buff=0.12, color=WHITE)
        self.at(9, Write(header), FadeIn(ham), run_time=0.6)
        self.frac(9, 0.4, GrowArrow(arr), FadeIn(dis), run_time=0.9)

        # cue 10: traditional Hamiltonians are limited because they are fundamentally symplectic
        sy = note("limited: fundamentally symplectic", 24, C_POI).next_to(ham, DOWN, buff=0.3)
        self.at(10, FadeIn(sy), run_time=0.6)
        # cue 11: expand into Riemannian geometry, hence the metric
        rg = note("expand into Riemannian geometry  →  a metric", 24, C_MET).next_to(dis, DOWN, buff=0.3)
        gij = tex(r"g_{ij}", 44, C_MET).next_to(rg, DOWN, buff=0.3)
        self.at(11, FadeIn(rg), Write(gij), run_time=0.9)

        # cue 12: antisymmetric Poisson bracket + symmetric metric bracket
        self.at(12, *[FadeOut(m) for m in [row, arr, sy, rg, gij]], run_time=0.5)
        cP = card("Poisson bracket\nantisymmetric\nconservative Hamiltonian flow", C_POI, 26, 5.6)
        cM = card("metric bracket\nsymmetric\ndissipation and entropy generation", C_MET, 28, 6.6)
        plus = tex(r"+", 56)
        trio = VGroup(cP, plus, cM).arrange(RIGHT, buff=0.5).move_to([0, 0.5, 0])
        clamp_width(trio)
        self.play(FadeIn(cP, shift=RIGHT * 0.2), FadeIn(plus), FadeIn(cM, shift=LEFT * 0.2), run_time=0.9)
        # cue 13: together: first and second laws
        laws = chip("first and second laws, together", YELLOW, 28, fill=0.1).move_to([0, -2.2, 0])
        self.at(13, FadeIn(laws, shift=UP * 0.15), run_time=0.6)
        self.snap("review")
        self.finish()


# ==========================================================================
# F2_1 -- deriving the metriplectic equations  (cues 14-22)
# ==========================================================================
class F2_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["F2_1"], END["F2_1"]

    def construct(self):
        header = hdr("Review of Metriplectic")
        self.add(header)
        X = LEFT_X
        # cue 14: Poisson bracket whose Casimir is the entropy
        e1 = at_left(tex(r"\dot f=\{f,g\}", 36), 1.9, X)
        e2 = at_left(tex(r"\{S,H\}=0", 36), 1.2, X)
        e3 = at_left(tex(r"\{C,f\}=0\quad\forall f", 36, YELLOW), 0.5, X)
        n3 = at_left(note("Casimir functionals of the Poisson bracket", 20), 0.5, X + 4.8)
        n2 = at_left(note("here the Casimir plays the role of entropy", 20, C_MET), 1.2, X + 4.8)
        self.at(14, Write(e1), run_time=0.8)
        self.frac(14, 0.35, Write(e2), FadeIn(n2), run_time=0.8)
        self.frac(14, 0.65, Write(e3), FadeIn(n3), run_time=0.9)
        # cue 15: Hamiltonian free energy
        e4 = at_left(tex(r"F=H+\lambda C", 40, GREEN), -0.5, X)
        n4 = at_left(note("Hamiltonian free energy", 20, GREEN), -0.5, X + 4.8)
        self.at(15, Write(e4), FadeIn(n4), run_time=1.0)
        # cue 16-17: lambda is a left-over constant; in MSTDOF it is minus the temperature
        n5 = at_left(note("lambda: an arbitrary constant, simply left over", 20, GREY_B), -1.05, X + 4.8)
        n6 = at_left(note("in the MSTDOF model:  lambda = −temperature", 20, YELLOW), -1.05, X + 4.8)
        self.at(16, FadeIn(n5), run_time=0.5)
        self.at(17, FadeOut(n5), FadeIn(n6), run_time=0.5)
        self.snap("freeenergy")

        # cue 18: expand the system:  <<f,g>> = {f,H} + (f,g), (f,g) symmetric, bilinear, semi-definite
        self.at(18, *[FadeOut(m) for m in [e1, e2, e3, n2, n3, n6, e4, n4]], run_time=0.5)
        f1 = at_left(tex(r"\langle\langle f,g\rangle\rangle=\{f,g\}+(f,g)", 40), 1.7, X)
        f2 = at_left(tex(r"(f,g)\ \text{symmetric,\ bilinear,\ semi-definite}", 32, C_MET), 0.8, X)
        # cue 19: evolution; (f,g) chosen to cancel the lambda coefficient
        f3 = at_left(tex(r"\dot f=\langle\langle f,F\rangle\rangle", 40, YELLOW), -0.2, X)
        f3n = at_left(note("(f,g) is defined so the lambda in F drops out of the equation of motion", 20, GREY_B),
                      -0.85, X)
        self.play(Write(f1), run_time=1.2)
        self.frac(18, 0.6, Write(f2), run_time=1.2)
        self.at(19, Write(f3), run_time=1.0)
        self.frac(19, 0.5, FadeIn(f3n), run_time=0.6)
        # cue 20: H conserved
        f4 = at_left(tex(r"(H,f)=0\quad\forall f", 38, C_MET), -1.8, X)
        f4n = at_left(note("so that H is conserved", 20, GREY_B), -1.8, X + 4.4)
        self.at(20, Write(f4), FadeIn(f4n), run_time=1.0)
        # cue 21: metriplectic evolution
        self.at(21, *[FadeOut(m) for m in [f1, f2, f3, f3n, f4, f4n]], run_time=0.5)
        g1 = tex(r"\dot f=\{f,H\}+\lambda\,(f,C)", 56)
        g1[0].set_color(WHITE)
        g1.move_to([0, 1.0, 0])
        g1l = note("metriplectic evolution", 24, YELLOW).next_to(g1, UP, buff=0.4)
        self.play(FadeIn(g1l), Write(g1), run_time=1.4)
        # cue 22: the Casimir mimics entropy
        g2 = tex(r"\dot C=\lambda\,(C,C)", 56, C_MET).move_to([0, -1.1, 0])
        g2l = note("the Casimir mimics entropy", 24, C_MET).next_to(g2, UP, buff=0.4)
        self.at(22, FadeIn(g2l), Write(g2), run_time=1.4)
        self.snap("evolution")
        self.finish()


# ==========================================================================
# F3 -- structure of the dissipative bracket  (cues 23-27)
# ==========================================================================
class F3(SyncedScene):
    AUDIO_START, AUDIO_END = START["F3"], END["F3"]

    def construct(self):
        header = hdr("Motivating the Metriplectic 4-bracket")
        # cue 23: with this bracket system, look at the structure of the dissipative bracket
        self.at(23, Write(header), run_time=0.6)
        t23 = Text("the structure of the dissipative bracket", font_size=36, color=YELLOW).move_to([0, 0.5, 0])
        self.sync(CUES[23].start + 0.6)
        self.play(FadeIn(t23, shift=UP * 0.15), run_time=0.6)

        # cue 24: symmetric, positive semi-definite, energy degenerate
        self.at(24, FadeOut(t23), run_time=0.4)
        k1 = card("1  symmetric", C_MET, 24, 3.6)
        k2 = card("2  positive\nsemi-definite", C_MET, 24, 3.6)
        k3 = card("3  energy\ndegenerate", YELLOW, 24, 3.6)
        cs = VGroup(k1, k2, k3).arrange(RIGHT, buff=0.5).move_to([0, 1.2, 0])
        q1 = tex(r"(F,G)=(G,F)", 34).next_to(k1, DOWN, buff=0.4)
        q2 = tex(r"(F,F)\ \geq\ 0", 34).next_to(k2, DOWN, buff=0.4)
        q3 = tex(r"(H,F)=0", 34, YELLOW).next_to(k3, DOWN, buff=0.4)
        self.play(FadeIn(k1, shift=UP * 0.2), Write(q1), run_time=0.7)
        self.frac(24, 0.35, FadeIn(k2, shift=UP * 0.2), Write(q2), run_time=0.7)
        self.frac(24, 0.7, FadeIn(k3, shift=UP * 0.2), Write(q3), run_time=0.7)

        # cue 25: the third condition is particularly important
        self.at(25, Indicate(k3, color=YELLOW, scale_factor=1.1), run_time=1.0)
        # cue 26: the bracket does not produce a change in total energy
        e = tex(r"\dot H=(H,H)=0", 40, YELLOW).move_to([0, -1.6, 0])
        en = note("the dissipative bracket does not change the total energy", 22, GREY_B).next_to(e, DOWN, buff=0.3)
        self.at(26, Write(e), FadeIn(en), run_time=1.0)
        # cue 27: it just produces entropy, which can mimic energy dissipation
        s = tex(r"\dot S\ \geq\ 0", 40, GREEN).move_to([3.4, -1.6, 0])
        e.generate_target()
        e.target.move_to([-3.4, -1.6, 0])
        self.at(27, FadeOut(en), MoveToTarget(e), Write(s), run_time=0.9)
        sn = note("entropy production -- in some cases mimics energy dissipation", 22, GREEN).move_to([0, -2.6, 0])
        self.play(FadeIn(sn), run_time=0.5)
        self.snap("conditions")
        self.finish()


# ==========================================================================
# F3_1 -- why the two-bracket formulation is not enough  (cues 28-37)
# ==========================================================================
class F3_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["F3_1"], END["F3_1"]

    def construct(self):
        header = hdr("Motivating the Metriplectic 4-bracket")
        self.add(header)
        # cue 28: you can't just add arbitrary dissipative operators
        t28 = Text("you can't just add arbitrary dissipative operators", font_size=34,
                   color=YELLOW).move_to([0, 1.6, 0])
        self.at(28, FadeIn(t28, shift=UP * 0.15), run_time=0.7)
        # cue 29: fluid dynamics
        fl = chip("fluid dynamics", TEAL, 28, fill=0.1).move_to([0, 0.2, 0])
        self.at(29, FadeIn(fl, shift=UP * 0.15), run_time=0.6)
        # cue 30: viscosity or collisional relaxation, individually
        eq = MathTex(r"(F,S)", r"=", r"(F,S)_{\rm visc}", r"+", r"(F,S)_{\rm heat}", font_size=46)
        eq[2].set_color(C_F)
        eq[4].set_color(C_G)
        eq.move_to([0, -0.9, 0])
        self.at(30, FadeOut(t28), fl.animate.move_to([0, 1.8, 0]), Write(eq), run_time=1.4)
        tk1, tk2 = tick().next_to(eq[2], DOWN, buff=0.3), tick().next_to(eq[4], DOWN, buff=0.3)
        self.frac(30, 0.7, FadeIn(tk1), FadeIn(tk2), run_time=0.5)
        ind = note("individually, each can be done", 22, GREEN).move_to([0, -2.4, 0])
        self.play(FadeIn(ind), run_time=0.4)
        # cue 31: coupling them breaks the mathematical structure
        bad = cross().next_to(eq[3], DOWN, buff=0.3)
        brk = note("coupled together, the mathematical structure generally breaks", 24, RED).move_to([0, -2.4, 0])
        self.at(31, FadeOut(ind), FadeIn(bad), FadeIn(brk), run_time=0.7)
        self.snap("coupling")

        # cue 32: workarounds exist but are complicated, no general way
        self.at(32, *[FadeOut(m) for m in [fl, eq, bad, brk, tk1, tk2]], run_time=0.5)
        w1 = chip("mathematicians found ways around it", GREY_B, 26)
        w2 = chip("extremely complicated", ORANGE, 26)
        w3 = chip("no general method to extend it", RED, 26)
        col = VGroup(w1, w2, w3).arrange(DOWN, buff=0.45).move_to([0, 0.6, 0])
        self.play(FadeIn(w1, shift=RIGHT * 0.2), run_time=0.5)
        self.frac(32, 0.4, FadeIn(w2, shift=RIGHT * 0.2), run_time=0.5)
        self.frac(32, 0.7, FadeIn(w3, shift=RIGHT * 0.2), run_time=0.5)
        # cue 33-34: construct the operators consistently; cross-coupling adds constraints
        r1 = note("the operators must be constructed consistently", 26, WHITE).move_to([0, -1.5, 0])
        r2 = note("cross-coupling between processes: further constraints", 26, WHITE).move_to([0, -2.2, 0])
        self.at(33, FadeIn(r1), run_time=0.5)
        self.at(34, FadeIn(r2), run_time=0.5)
        # cue 35: two-bracket gives no general construction procedure
        self.at(35, *[FadeOut(m) for m in [w1, w2, w3, r1, r2]], run_time=0.4)
        two = card("2-bracket\nno general construction procedure", RED, 28, 7.2).move_to([0, 0.6, 0])
        self.play(FadeIn(two, shift=UP * 0.2), run_time=0.6)
        self.snap("twobracket")
        # cue 36: but the four bracket actually does
        four = card("4-bracket\na general construction procedure", GREEN, 28, 7.2).move_to([0, 0.6, 0])
        self.at(36, FadeOut(two), FadeIn(four, shift=UP * 0.2), run_time=0.7)
        # cue 37: and that's what we're going to show
        shw = br4(60).move_to([0, -1.8, 0])
        self.at(37, Write(shw), run_time=1.0)
        self.finish()


# ==========================================================================
# F4 -- the curvature tensor and the metric  (cues 38-46)
# ==========================================================================
class F4(SyncedScene):
    AUDIO_START, AUDIO_END = START["F4"], END["F4"]

    def construct(self):
        header = hdr("Constructing the 4-bracket")
        X = LEFT_X
        # cue 38: construct our metriplectic four-bracket
        self.at(38, Write(header), run_time=0.6)
        # cue 39: phase space Z is a Riemannian manifold with curvature tensor R
        l1 = at_left(Text("phase space  Z  is a Riemannian manifold with a curvature tensor  R", font_size=26), 1.9, X)
        clamp_width(l1, 12.0)
        r1 = at_left(tex(r"R:\ \mathfrak{X}(\mathcal{Z})\times\mathfrak{X}(\mathcal{Z})\times\mathfrak{X}(\mathcal{Z})"
                         r"\ \rightarrow\ \mathfrak{X}(\mathcal{Z})", 34, YELLOW), 1.05, X)
        clamp_width(r1, 12.0)
        self.at(39, FadeIn(l1), run_time=0.6)
        self.frac(39, 0.4, Write(r1), run_time=1.6)
        # cue 41: for X,Y,Z vector fields, R(X,Y)Z is a vector field
        r2 = at_left(tex(r"\mathbf{X},\mathbf{Y},\mathbf{Z}\in\mathfrak{X}(\mathcal{Z})\ \Rightarrow\ "
                         r"R(\mathbf{X},\mathbf{Y})\mathbf{Z}\in\mathfrak{X}(\mathcal{Z})", 32), 0.2, X)
        clamp_width(r2, 12.0)
        self.at(41, Write(r2), run_time=1.6)
        # cue 42: coordinate form
        r3 = at_left(tex(r"R^{i}{}_{jkl},\qquad i,j,k,l=1,\dots,\dim\mathcal{Z}", 34, GREEN), -0.65, X)
        self.at(42, Write(r3), run_time=1.4)
        # cue 43: a metric g with covariant tensor g_ij
        r4 = at_left(tex(r"g:\ \mathfrak{X}(\mathcal{Z})\times\mathfrak{X}(\mathcal{Z})\rightarrow\Lambda^0(\mathcal{Z})"
                         r"\qquad g_{ij}", 32, ORANGE), -1.5, X)
        clamp_width(r4, 12.0)
        self.at(43, Write(r4), run_time=1.4)
        # cue 44: totally covariant tensor
        r5 = at_left(tex(r"R(\mathbf{X},\mathbf{Y},\mathbf{Z},\mathbf{W})=g\big(R(\mathbf{X},\mathbf{Y})\mathbf{Z},\mathbf{W}\big)",
                         34, YELLOW), -2.35, X)
        clamp_width(r5, 12.0)
        self.at(44, Write(r5), run_time=1.6)
        # cue 45: index form
        r6 = at_left(tex(r"R_{ijkl}=g_{im}R^{m}{}_{jkl}", 34, GREEN), -3.1, X)
        self.at(45, Write(r6), run_time=1.0)
        self.snap("curvature")
        # cue 46: the five symmetries -- hand off to a clean slate
        self.at(46, *[FadeOut(m) for m in [l1, r1, r2, r3, r4, r5, r6]], run_time=0.5)
        sym = [(r"R_{ijkl}=-R_{jikl}", "1  antisymmetric in the first pair", BLUE),
               (r"R_{ijkl}=-R_{ijlk}", "2  antisymmetric in the second pair", BLUE),
               (r"R_{ijkl}=R_{klij}", "3  pair symmetry", BLUE),
               (r"R_{ijkl}+R_{iklj}+R_{iljk}=0", "4  cyclic / algebraic: first Bianchi", YELLOW),
               (r"R_{ijkl;m}+R_{ijlm;k}+R_{ijmk;l}=0", "5  differential: second Bianchi (the \"Jacobi\" identity)", YELLOW)]
        ys = [1.9, 1.0, 0.1, -0.8, -1.7]
        for (e, lab, col), y in zip(sym, ys):
            eq = at_left(tex(e, 34, col), y, X)
            clamp_width(eq, 6.4)
            lb = at_left(note(lab, 20, GREY_B), y, X + 6.8)
            clamp_width(lb, 5.4)
            self.play(Write(eq), FadeIn(lb), run_time=0.8)
            self.wait(0.4)
        self.snap("symmetries")
        self.finish()


# ==========================================================================
# F4_1 -- the 4-bracket itself  (cues 47-52)
# ==========================================================================
class F4_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["F4_1"], END["F4_1"]

    def construct(self):
        header = hdr("Constructing the 4-bracket")
        self.add(header)
        # cue 47: suppose a fully contravariant tensor with the same symmetries
        r = tex(r"R:\ \Lambda^1(\mathcal{Z})\times\Lambda^1(\mathcal{Z})\times\Lambda^1(\mathcal{Z})"
                r"\times\Lambda^1(\mathcal{Z})\rightarrow\Lambda^0(\mathcal{Z})", 36, YELLOW)
        clamp_width(r, 12.0)
        r.move_to([0, 1.8, 0])
        rn = note("fully contravariant, with the same symmetries as the Riemann tensor", 22, GREY_B).next_to(r, DOWN, buff=0.3)
        self.at(47, Write(r), run_time=1.6)
        self.play(FadeIn(rn), run_time=0.5)
        # cue 49: how to make one: raise indices of R with the metric
        how = tex(r"R_{ijkl}\ \xrightarrow{\ \text{raise with } g\ }\ R^{ijkl}", 40, ORANGE).move_to([0, 0.0, 0])
        hn = note("compose the covariant curvature with any tangent-cotangent isomorphism; the metric is one obvious choice",
                  20, GREY_B).next_to(how, DOWN, buff=0.3)
        clamp_width(hn, 12.0)
        self.at(49, Write(how), run_time=1.4)
        self.play(FadeIn(hn), run_time=0.5)
        # cue 50: natural bracket on four functions
        self.at(50, *[FadeOut(m) for m in [r, rn, how, hn]], run_time=0.5)
        d = MathTex("(", "f", ",", "k", ";", "g", ",", "n", ")", ":=", "R(", "df", ",", "dk", ",", "dg", ",", "dn", ")",
                    font_size=52)
        for i, c in ((1, C_F), (3, C_K), (5, C_G), (7, C_N), (11, C_F), (13, C_K), (15, C_G), (17, C_N)):
            d[i].set_color(c)
        d.move_to([0, 1.2, 0])
        self.play(Write(d), run_time=1.6)
        # cue 51: this is our metriplectic 4-bracket
        lab = chip("the metriplectic 4-bracket", YELLOW, 30, fill=0.1).move_to([0, -0.4, 0])
        self.at(51, FadeIn(lab, shift=UP * 0.15), Indicate(d[:9], color=YELLOW), run_time=0.9)
        # cue 52: coordinate form
        cf = MathTex(r"(f,k;g,n)", r"=", r"R^{ijkl}(z)", r"\frac{\partial f}{\partial z^i}", r"\frac{\partial k}{\partial z^j}",
                     r"\frac{\partial g}{\partial z^k}", r"\frac{\partial n}{\partial z^l}", font_size=44)
        for i, c in ((3, C_F), (4, C_K), (5, C_G), (6, C_N)):
            cf[i].set_color(c)
        cf.move_to([0, -1.9, 0])
        cn = note("in a coordinate neighborhood", 20, GREY_B).next_to(cf, UP, buff=0.3)
        self.at(52, FadeIn(cn), Write(cf), run_time=1.8)
        self.snap("bracket")
        self.finish()


# ==========================================================================
# F4_2 -- algebraic properties of the 4-bracket  (cues 53-56)
# ==========================================================================
class F4_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["F4_2"], END["F4_2"]

    def construct(self):
        header = hdr("Constructing the 4-bracket")
        self.add(header)
        X = LEFT_X
        # cue 53: from this construction the algebraic properties are immediately evident
        t53 = Text("algebraic properties: immediately evident", font_size=30, color=YELLOW).move_to([0, 2.3, 0])
        self.at(53, FadeIn(t53, shift=UP * 0.15), run_time=0.6)
        # cue 54: linearity; identities/symmetries; derivation
        h1 = at_left(Text("(i)  linearity in all arguments", font_size=22, color=TEAL), 1.5, X)
        e1 = at_left(tex(r"(f+h,k;g,n)=(f,k;g,n)+(h,k;g,n)", 32), 0.9, X)
        h2 = at_left(Text("(ii)  algebraic identities / symmetries", font_size=22, color=TEAL), 0.15, X)
        e2 = VGroup(tex(r"(f,k;g,n)=-(k,f;g,n)", 28), tex(r"(f,k;g,n)=-(f,k;n,g)", 28),
                    tex(r"(f,k;g,n)=(g,n;f,k)", 28),
                    tex(r"(f,k;g,n)+(f,g;n,k)+(f,n;k,g)=0", 28, YELLOW)).arrange(
            DOWN, aligned_edge=LEFT, buff=0.2)
        e2.move_to([0, 0, 0])
        e2.set_y(-1.0)
        e2.align_to(h2, LEFT)
        h3 = at_left(Text("(iii)  derivation in all arguments", font_size=22, color=TEAL), -2.3, X)
        e3 = at_left(tex(r"(fh,k;g,n)=f\,(h,k;g,n)+(f,k;g,n)\,h", 32), -2.9, X)
        self.at(54, FadeIn(h1), Write(e1), run_time=1.2)
        self.frac(54, 0.3, FadeIn(h2), LaggedStart(*[Write(m) for m in e2], lag_ratio=0.3), run_time=2.4)
        self.frac(54, 0.7, FadeIn(h3), Write(e3), run_time=1.4)
        self.snap("properties")
        # cue 55: all manifest when writing in coordinates
        cf = tex(r"R^{ijkl}\,\partial_i f\,\partial_j k\,\partial_k g\,\partial_l n", 34, GREEN).move_to([4.4, 0.4, 0])
        cfn = note("manifest in coordinates", 22, GREEN).next_to(cf, UP, buff=0.3)
        clamp_width(cf, 5.4)
        self.at(55, FadeIn(cfn), Write(cf), run_time=1.4)
        # cue 56: fh is pointwise multiplication
        pw = note("fh  =  pointwise multiplication", 22, GREY_B).next_to(e3, RIGHT, buff=0.5)
        self.at(56, FadeIn(pw), Indicate(e3, color=YELLOW, scale_factor=1.04), run_time=0.8)
        self.finish()


# ==========================================================================
# F4_3 -- contravariant sectional curvature  (cues 57-62)
# ==========================================================================
class F4_3(SyncedScene):
    AUDIO_START, AUDIO_END = START["F4_3"], END["F4_3"]

    def construct(self):
        header = hdr("Sectional Curvature")
        # cue 57: contravariant analogs of Riemannian constructions
        self.at(57, Write(header), run_time=0.6)
        an = chip("contravariant analogs of Riemannian constructions", TEAL, 28, fill=0.08).move_to([0, 1.8, 0])
        clamp_width(an)
        self.play(FadeIn(an, shift=UP * 0.2), run_time=0.7)
        # cue 58: contravariant sectional curvature on 1-forms sigma, eta
        O = np.array([-3.6, -0.6, 0.0])
        s = Arrow(O, O + [2.0, 0.35, 0], buff=0, color=C_F, stroke_width=6)
        e = Arrow(O, O + [0.7, 1.9, 0], buff=0, color=C_K, stroke_width=6)
        para = Polygon(O, O + [2.0, 0.35, 0], O + [2.7, 2.25, 0], O + [0.7, 1.9, 0],
                       color=YELLOW, stroke_width=1.5, fill_color=YELLOW, fill_opacity=0.18)
        sl = tex(r"\sigma", 40, C_F).next_to(s.get_end(), RIGHT, buff=0.1)
        el = tex(r"\eta", 40, C_K).next_to(e.get_end(), UP, buff=0.1)
        K = MathTex(r"K(\sigma,\eta)", r":=", r"R(\sigma,\eta,\sigma,\eta)", font_size=46)
        K[0].set_color(YELLOW)
        K.move_to([3.0, 0.6, 0])
        s_in = tex(r"\sigma,\ \eta\ \in\ \Lambda^1(\mathcal{Z})", 32).next_to(K, DOWN, buff=0.4)
        self.at(58, FadeOut(an), FadeIn(para), GrowArrow(s), GrowArrow(e), FadeIn(sl), FadeIn(el), run_time=1.0)
        self.frac(58, 0.35, Write(K), run_time=1.4)
        self.frac(58, 0.7, FadeIn(s_in), run_time=0.6)
        # cue 59: second equality if sigma = df, eta = dg; drop the usual normalization
        eq2 = MathTex(r"=\ (f,g;f,g)", font_size=46, color=GREEN).next_to(K, DOWN, buff=0.4)
        sub = tex(r"\sigma=df,\qquad \eta=dg", 34, GREEN).next_to(eq2, DOWN, buff=0.3)
        nrm = note("normalization by |σ ∧ η| dropped, for simplicity", 20, GREY_B).next_to(sub, DOWN, buff=0.3)
        self.at(59, FadeOut(s_in), Write(eq2), run_time=1.0)
        self.play(Write(sub), run_time=0.8)
        self.frac(59, 0.6, FadeIn(nrm), run_time=0.5)
        self.snap("sectional")
        # cue 60: assume K >= 0 throughout
        self.at(60, *[FadeOut(m) for m in [eq2, sub, nrm]], run_time=0.5)
        pos = tex(r"K(\sigma,\eta)\ \geq\ 0", 64, YELLOW).move_to([3.0, -1.4, 0])
        box = SurroundingRectangle(pos, color=YELLOW, buff=0.25, corner_radius=0.12)
        pn = note("assumed throughout", 22, YELLOW).next_to(box, DOWN, buff=0.3)
        self.play(Write(pos), Create(box), FadeIn(pn), run_time=1.2)
        # cue 61: we have created the metriplectic 4-bracket: a very broad structure
        self.at(61, *[FadeOut(m) for m in [para, s, e, sl, el, K, pos, box, pn]], run_time=0.5)
        big = br4(100).move_to([0, 0.9, 0])
        bl = note("a very broad structure that encompasses many things", 28, WHITE).next_to(big, DOWN, buff=0.5)
        self.play(Write(big), FadeIn(bl), run_time=1.2)
        # cue 62: first constructed in 2023, so work so far is limited
        tl = Line([-4.5, -2.3, 0], [4.5, -2.3, 0], color=GREY_B)
        d23 = Dot([2.6, -2.3, 0], radius=0.12, color=YELLOW)
        l23 = Text("2023: first constructed", font_size=22, color=YELLOW).next_to(d23, UP, buff=0.2)
        dn = Dot([4.5, -2.3, 0], radius=0.08, color=WHITE)
        ln = note("now", 18).next_to(dn, DOWN, buff=0.15)
        lim = note("work so far is fairly limited", 22, GREY_B).move_to([0, -3.1, 0])
        self.at(62, Create(tl), FadeIn(d23), FadeIn(l23), FadeIn(dn), FadeIn(ln), run_time=1.0)
        self.frac(62, 0.5, FadeIn(lim), run_time=0.5)
        self.finish()


# ==========================================================================
# F5 -- dissipative dynamics from the 4-bracket  (cues 63-71)
# ==========================================================================
class F5(SyncedScene):
    AUDIO_START, AUDIO_END = START["F5"], END["F5"]

    def construct(self):
        header = hdr("Dissipative Dynamics")
        X = LEFT_X
        # cue 63-64: the construction allows dissipative structure; here we prove it
        self.at(63, Write(header), run_time=0.6)
        cl = Text("claim: the 4-bracket construction allows dissipative structure", font_size=28,
                  color=YELLOW).move_to([0, 2.0, 0])
        clamp_width(cl)
        self.play(FadeIn(cl, shift=UP * 0.15), run_time=0.7)
        pf = chip("proof", GREEN, 26, fill=0.1).move_to([0, 0.8, 0])
        self.at(64, FadeIn(pf, shift=UP * 0.15), run_time=0.6)
        # cue 65: symmetric but degenerate bracket
        self.at(65, FadeOut(cl), FadeOut(pf), run_time=0.4)
        d1 = tex(r"(f,g)_H\ :=\ (f,H;g,H)=R^{ijkl}\,\partial_i f\,\partial_j H\,\partial_k g\,\partial_l H", 34, YELLOW)
        clamp_width(d1, 12.0)
        d1.move_to([0, 1.9, 0])
        d1n = note("symmetric, yet degenerate", 22, GREY_B).next_to(d1, DOWN, buff=0.3)
        self.play(Write(d1), run_time=1.8)
        self.frac(65, 0.7, FadeIn(d1n), run_time=0.5)
        # cue 66: swap f<->g swaps i<->k; contraction in j,l symmetric
        d2 = tex(r"f\leftrightarrow g\ \Longleftrightarrow\ i\leftrightarrow k,\qquad j,l\ \text{contracted symmetrically}",
                 30, C_G)
        clamp_width(d2, 12.0)
        d2.move_to([0, 0.5, 0])
        d3 = tex(r"(f,g)_H=(g,f)_H\qquad\text{by the pair symmetry}", 34, GREEN).move_to([0, -0.4, 0])
        self.at(66, FadeOut(d1n), Write(d2), run_time=1.4)
        self.frac(66, 0.65, Write(d3), run_time=1.2)
        # cue 67: so the G-metric follows
        d4 = tex(r"G^{ik}=R^{ijkl}\,\partial_j H\,\partial_l H", 40, ORANGE).move_to([0, -1.5, 0])
        self.at(67, Write(d4), run_time=1.2)
        # cue 68: dissipative dynamics generated by it
        self.at(68, *[FadeOut(m) for m in [d2, d3]], d4.animate.move_to([0, 0.5, 0]), run_time=0.5)
        d5 = tex(r"\dot z^i=(z^i,H;S,H)=(z^i,S)_H=G^{ij}\,\partial_j S", 40, YELLOW)
        clamp_width(d5, 12.0)
        d5.move_to([0, -0.7, 0])
        self.play(Write(d5), run_time=1.8)
        # cue 69: for full metriplectic dynamics, add the Poisson bracket
        d6 = tex(r"+\ \{z^i,H\}\quad\text{(the Poisson-bracket term, for full metriplectic dynamics)}", 28, BLUE)
        clamp_width(d6, 12.0)
        d6.move_to([0, -1.7, 0])
        self.at(69, FadeIn(d6, shift=UP * 0.15), run_time=0.8)
        self.snap("dynamics")

        # cue 70: energy conserved; entropy never decreases
        self.at(70, *[FadeOut(m) for m in [d1, d4, d5, d6]], run_time=0.5)
        en = tex(r"(f,H)_H=(H,f)_H=0\quad\forall f", 36, YELLOW)
        enn = note("energy conservation: automatically", 20, YELLOW).next_to(en, DOWN, buff=0.2)
        eg = VGroup(en, enn).arrange(DOWN, buff=0.2, aligned_edge=LEFT).move_to([-3.4, 1.6, 0])
        ent = tex(r"\dot S=(S,S)_H=(S,H;S,H)\ \geq\ 0", 36, GREEN)
        entn = note("from K ≥ 0: entropy production", 20, GREEN).next_to(ent, DOWN, buff=0.2)
        eg2 = VGroup(ent, entn).arrange(DOWN, buff=0.2, aligned_edge=LEFT).move_to([-2.4, -0.7, 0])
        ax = Axes(x_range=[0, 5, 1], y_range=[0, 3.2, 1], x_length=4.4, y_length=2.8,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([4.2, -0.4, 0])
        Hc = ax.plot(lambda x: 2.0, x_range=[0, 5], color=YELLOW)
        Sc = ax.plot(lambda x: 0.4 + 2.3 * (1 - math.exp(-0.8 * x)), x_range=[0, 5], color=GREEN)
        Hl = tex(r"H", 28, YELLOW).next_to(ax.c2p(5, 2.0), RIGHT, buff=0.1)
        Sl = tex(r"S", 28, GREEN).next_to(ax.c2p(5, 2.6), RIGHT, buff=0.1)
        self.play(Write(en), FadeIn(enn), Create(ax), Create(Hc), FadeIn(Hl), run_time=1.6)
        self.frac(70, 0.45, Write(ent), FadeIn(entn), Create(Sc), FadeIn(Sl), run_time=2.0)
        self.snap("laws")
        # cue 71: 2-brackets arise as special cases of 4-brackets
        self.at(71, *[FadeOut(m) for m in [en, enn, ent, entn, ax, Hc, Sc, Hl, Sl]], run_time=0.5)
        a = VGroup(tex(r"(f,H;g,H)", 56, YELLOW), note("4-bracket", 24, YELLOW)).arrange(DOWN, buff=0.3).move_to([-3.6, 0.4, 0])
        b = VGroup(tex(r"(f,g)", 56, ORANGE), note("metriplectic 2-bracket", 24, ORANGE)).arrange(DOWN, buff=0.3).move_to([3.6, 0.4, 0])
        arr = Arrow(a.get_right() + RIGHT * 0.2, b.get_left() + LEFT * 0.2, buff=0, color=WHITE)
        sp = note("2-brackets arise as special cases of 4-brackets", 26, WHITE).move_to([0, -1.6, 0])
        self.play(FadeIn(a, shift=UP * 0.2), run_time=0.5)
        self.play(GrowArrow(arr), FadeIn(b, shift=LEFT * 0.2), FadeIn(sp), run_time=0.9)
        self.finish()


# ==========================================================================
# F6 -- Lie-metriplectic manifolds  (cues 72-80)
# ==========================================================================
class F6(SyncedScene):
    AUDIO_START, AUDIO_END = START["F6"], END["F6"]

    def construct(self):
        header = hdr("Lie-Metriplectic Manifolds")
        # cue 72: expand on the structure via the underlying manifold
        self.at(72, Write(header), run_time=0.6)
        t72 = Text("the underlying manifold: Lie-metriplectic manifolds", font_size=32,
                   color=YELLOW).move_to([0, 0.6, 0])
        self.play(FadeIn(t72, shift=UP * 0.15), run_time=0.7)
        # cue 73: reading from the Morrison & Updike paper, slightly condensed
        pp = card("Morrison & Updike\n(condensed readout from the paper)", TEAL, 24, 7.0).move_to([0, -1.0, 0])
        self.at(73, FadeIn(pp, shift=UP * 0.2), run_time=0.7)

        # cue 74: configuration space a Lie group G -> reduced phase space Z = T*G/G = g*
        self.at(74, *[FadeOut(m) for m in [t72, pp]], run_time=0.5)

        def node(label, color, y, w=None):
            m = tex(label, 30)
            r = SurroundingRectangle(m, color=color, buff=0.2, corner_radius=0.1)
            g = VGroup(r, m)
            g.move_to([LEFT_X + 1.9, y, 0])
            return g
        n1 = node(r"G\ \ \text{(Lie group)}", BLUE, 1.9)
        n2 = node(r"\mathcal{Z}=T^{*}G/G\cong\mathfrak{g}^{*}", TEAL, 0.7)
        n3 = node(r"g\ \ \text{left-invariant metric}", GREEN, -0.5)
        n4 = node(r"R_G\big|_e\ \ \text{constant tensor on}\ \mathfrak{g}", ORANGE, -1.7)
        for n in (n1, n2, n3, n4):
            n.align_to([LEFT_X, 0, 0], LEFT)
        ar = VGroup(*[Arrow(a.get_bottom(), b.get_top(), buff=0.06, stroke_width=3, color=GREY_B,
                            max_tip_length_to_length_ratio=0.3)
                      for a, b in ((n1, n2), (n2, n3), (n3, n4))])
        nt = [note("configuration space of a Hamiltonian system", 20),
              note("reduced phase space;  g* is the dual of the Lie algebra g of G", 20),
              note("globally defined by left translation from its action at the identity", 20),
              note("entirely encodes the left-invariant curvature tensor", 20)]
        for n, t, y in zip((n1, n2, n3, n4), nt, (1.9, 0.7, -0.5, -1.7)):
            t.move_to([LEFT_X + 5.6, y, 0], aligned_edge=LEFT)
        nt[1].set_x(LEFT_X + 5.9, LEFT)
        self.play(FadeIn(n1), FadeIn(nt[0]), run_time=0.6)
        self.frac(74, 0.4, GrowArrow(ar[0]), FadeIn(n2), FadeIn(nt[1]), run_time=1.0)
        # cue 75-76: a metric is needed; left-invariant metrics are the only ones that respect Z
        self.at(75, GrowArrow(ar[1]), FadeIn(n3), run_time=0.9)
        self.frac(75, 0.5, FadeIn(nt[2]), run_time=0.6)
        ok = note("the only metrics that respect the reduced phase space", 22, GREEN).move_to([0, -2.7, 0])
        self.at(76, FadeIn(ok), Indicate(n3, color=GREEN, scale_factor=1.05), run_time=0.8)
        # cue 77: left-invariant curvature tensor encoded by its value at the identity
        self.at(77, FadeOut(ok), GrowArrow(ar[2]), FadeIn(n4), run_time=0.9)
        self.frac(77, 0.5, FadeIn(nt[3]), run_time=0.6)
        self.snap("chain")

        # cue 78: raise indices -> 4-bracket on g*
        self.at(78, *[FadeOut(m) for m in [n1, n2, n3, n4, *ar, *nt]], run_time=0.5)
        r1 = tex(r"R^{ijkl}=R_G\big|_e\,(z^i,z^j,z^k,z^l)", 48, YELLOW).move_to([0, 1.5, 0])
        r1n = note("metrically raising the indices of R_G: the 4-bracket on g*, with z^i coordinates on g*", 22, GREY_B)
        r1n.next_to(r1, DOWN, buff=0.35)
        clamp_width(r1n, 12.0)
        self.play(Write(r1), run_time=1.4)
        self.play(FadeIn(r1n), run_time=0.5)
        # cue 79: independent of z -> Lie-metriplectic 4-brackets
        box = SurroundingRectangle(r1, color=GREEN, buff=0.25, corner_radius=0.1)
        lm = chip("R independent of z   ⇒   Lie-metriplectic 4-bracket", GREEN, 26, fill=0.08).move_to([0, -0.8, 0])
        self.at(79, Create(box), FadeIn(lm, shift=UP * 0.15), run_time=1.0)
        # cue 80: z-dependence of the 2-bracket is determined by H; quadratic H -> quadratic form
        dep = tex(r"(\cdot,\cdot)_H\ \text{depends on}\ z\ \text{only through}\ H", 34).move_to([0, -1.9, 0])
        quad = tex(r"H=H_{ij}z^iz^j\ \Longrightarrow\ (\cdot,\cdot)_H\ \text{is a quadratic form}", 34, YELLOW)
        clamp_width(quad, 12.0)
        quad.move_to([0, -2.8, 0])
        self.at(80, Write(dep), run_time=1.2)
        self.frac(80, 0.5, Write(quad), run_time=1.4)
        self.snap("lie")
        self.finish()


# ==========================================================================
# F7 -- Kulkarni-Nomizu construction  (cues 81-88)
# ==========================================================================
class F7(SyncedScene):
    AUDIO_START, AUDIO_END = START["F7"], END["F7"]

    def construct(self):
        header = hdr("Kulkarni-Nomizu Construction")
        X = LEFT_X
        # cue 81: look at specific ways to construct four brackets
        self.at(81, Write(header), run_time=0.6)
        t81 = Text("specific ways to construct 4-brackets", font_size=34, color=YELLOW).move_to([0, 0.6, 0])
        self.play(FadeIn(t81, shift=UP * 0.15), run_time=0.6)
        # cue 82: K-N gains the most research attention
        kn = chip("Kulkarni-Nomizu  (K-N)", ORANGE, 34, fill=0.12).move_to([0, -0.8, 0])
        most = note("the one that gains the most research attention", 24, GREY_B).next_to(kn, DOWN, buff=0.3)
        self.at(82, FadeIn(kn, shift=UP * 0.15), FadeIn(most), run_time=0.8)
        # cue 83: others exist; read the paper
        oth = note("others exist -- see the paper", 22, GREY_B).move_to([0, -2.4, 0])
        self.at(83, FadeIn(oth), run_time=0.5)

        # cue 84: the K-N product
        self.at(84, *[FadeOut(m) for m in [t81, kn, most, oth]], run_time=0.3)
        L = [MathTex(r"(\sigma\KN\mu)(df,dk,dg,dn)=", r"\sigma(df,dg)\,\mu(dk,dn)", font_size=34),
             MathTex(r"-\,\sigma(df,dn)\,\mu(dk,dg)", font_size=34),
             MathTex(r"+\,\mu(df,dg)\,\sigma(dk,dn)", font_size=34),
             MathTex(r"-\,\mu(df,dn)\,\sigma(dk,dg)", font_size=34)]
        L[0][1].set_color(C_F)
        L[1].set_color(C_K)
        L[2].set_color(C_G)
        L[3].set_color(C_N)
        g = VGroup(*L).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        for k in (1, 2, 3):
            L[k].align_to(L[0][1], LEFT)
        g.move_to([0, 1.2, 0])
        self.play(Write(L[0]), run_time=0.8)
        self.frac(84, 0.5, LaggedStart(*[Write(m) for m in L[1:]], lag_ratio=0.5), run_time=1.6)
        # cue 85: the 4-bracket defined as the K-N product
        br = MathTex(r"(f,k;g,n)=(\sigma\KN\mu)(df,dk,dg,dn)", font_size=44, color=YELLOW).move_to([0, -1.2, 0])
        self.at(85, Write(br), run_time=1.4)
        # cue 86: simple to construct a minimal metriplectic
        mm = note("a simple way to construct a minimal metriplectic system", 24, GREY_B).move_to([0, -2.1, 0])
        self.at(86, FadeIn(mm), run_time=0.6)
        self.snap("kn")
        # cue 87: Cauchy-Schwarz: K >= 0 if sigma and mu positive semi-definite
        self.at(87, *[FadeOut(m) for m in [g, br, mm]], run_time=0.5)
        cs = tex(r"\sigma,\ \mu\ \text{positive semi-definite}\ \overset{\text{Cauchy--Schwarz}}{\Longrightarrow}\ K(\sigma,\eta)\geq0",
                 38, GREEN)
        clamp_width(cs, 12.0)
        cs.move_to([0, 1.0, 0])
        self.play(Write(cs), run_time=1.8)
        # cue 88: one positive definite (an inner product): equality iff sigma proportional to eta
        pd = tex(r"\sigma\ \text{or}\ \mu\ \text{positive definite (an inner product)}", 34, ORANGE).move_to([0, -0.3, 0])
        eqc = tex(r"K(\sigma,\eta)\geq0,\qquad K(\sigma,\eta)=0\ \Longleftrightarrow\ \sigma\propto\eta", 38, YELLOW)
        clamp_width(eqc, 12.0)
        eqc.move_to([0, -1.5, 0])
        self.at(88, Write(pd), run_time=1.4)
        self.frac(88, 0.4, Write(eqc), run_time=1.8)
        self.snap("cs")
        self.finish()


# ==========================================================================
# Unified Thermodynamics Algorithm: step tracker shared by F8 / F8_1
# ==========================================================================
UTA_STEPS = ["Identify dynamical variables", "Propose energy and entropy",
             "Find the Poisson bracket", "Build the 4-bracket (K-N)"]


def uta_tracker(done=0, active=None):
    """Left-hand column of four steps.  Steps < done are green, `active` is yellow."""
    rows = VGroup()
    for i, name in enumerate(UTA_STEPS):
        col = GREEN if i < done else (YELLOW if i == active else GREY_B)
        c = Circle(radius=0.22, color=col, stroke_width=3)
        n = Text(str(i + 1), font_size=20, color=col).move_to(c)
        t = Text(name, font_size=20, color=col).next_to(c, RIGHT, buff=0.2)
        rows.add(VGroup(c, n, t))
    rows.arrange(DOWN, aligned_edge=LEFT, buff=0.5)
    rows.move_to([LEFT_X + 2.0, 0.2, 0], aligned_edge=LEFT)
    rows.align_to([LEFT_X, 0, 0], LEFT)
    return rows


PANEL_X = 3.5


# ==========================================================================
# F8 -- UTA: setup and dynamical variables  (cues 89-93)
# ==========================================================================
class F8(SyncedScene):
    AUDIO_START, AUDIO_END = START["F8"], END["F8"]

    def construct(self):
        header = hdr("Unified Thermodynamics Algorithm")
        # cue 89: an algorithm built on the K-N product to generate systems from 4-brackets
        self.at(89, Write(header), run_time=0.6)
        a = chip("K-N product", ORANGE, 28, fill=0.1)
        b = chip("generalized dissipative systems", GREEN, 28, fill=0.1)
        row = VGroup(a, Arrow(LEFT, RIGHT, buff=0, color=WHITE).scale(0.7), b).arrange(RIGHT, buff=0.4)
        row.move_to([0, 0.8, 0])
        alg = note("a recent algorithm using the K-N product to build systems from 4-brackets", 24, GREY_B)
        alg.next_to(row, DOWN, buff=0.5)
        clamp_width(alg, 12.0)
        self.sync(CUES[89].start + 0.7)
        self.play(FadeIn(row, shift=UP * 0.2), FadeIn(alg), run_time=0.9)
        # cue 90: in depth field; general steps, with Navier-Stokes-Fourier as the example
        ns = chip("example: Navier-Stokes-Fourier", TEAL, 28, fill=0.1).move_to([0, -1.5, 0])
        self.at(90, FadeIn(ns, shift=UP * 0.15), run_time=0.7)
        # cue 91: first, identify the dynamical variables
        self.at(91, *[FadeOut(m) for m in [row, alg, ns]], run_time=0.5)
        tr = uta_tracker(done=0, active=0)
        self.play(FadeIn(tr), run_time=0.7)
        # cue 92-93: rho, m, sigma
        xi = tex(r"\boldsymbol{\xi}(\mathbf{x},t)=\big(\rho,\ \mathbf{m},\ \sigma\big)", 46).move_to([PANEL_X, 1.5, 0])
        d1 = note("ρ : mass density", 24)
        d2 = note("m = ρ v : momentum density,  v the Eulerian velocity field", 24)
        d3 = note("σ : entropy density", 24, ORANGE)
        ds = VGroup(d1, d2, d3).arrange(DOWN, aligned_edge=LEFT, buff=0.3).next_to(xi, DOWN, buff=0.5)
        clamp_width(ds, 7.2)
        self.at(92, Write(xi), FadeIn(d1), run_time=1.2)
        self.at(93, FadeIn(d2), run_time=0.6)
        self.frac(93, 0.6, FadeIn(d3), run_time=0.6)
        self.snap("variables")
        self.finish(keep=[header])


# ==========================================================================
# F8_1 -- UTA: steps 2-4 and the general solution  (cues 94-100)
# ==========================================================================
class F8_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["F8_1"], END["F8_1"]

    def construct(self):
        header = hdr("Unified Thermodynamics Algorithm")
        self.add(header)
        tr = uta_tracker(done=0, active=0)
        self.add(tr)
        xi = tex(r"\boldsymbol{\xi}=\big(\rho,\ \mathbf{m},\ \sigma\big)", 46).move_to([PANEL_X, 1.5, 0])
        hl = note("the entropy density σ is singled out as the last variable", 24, ORANGE).next_to(xi, DOWN, buff=0.5)
        self.add(xi)
        # cue 94: entropy density singled out
        self.at(94, FadeIn(hl), Indicate(xi, color=ORANGE, scale_factor=1.05), run_time=0.8)

        def step(k, *mobs, rt=0.8, outs=()):
            """Advance tracker to step k, swap the right-hand panel."""
            new = uta_tracker(done=k, active=k)
            self.play(Transform(tr, new), *[FadeOut(o) for o in outs],
                      *[FadeIn(m, shift=UP * 0.12) for m in mobs], run_time=rt)

        # cue 95: second, propose energy and entropy functionals
        S = tex(r"S=\int_\Omega \sigma", 44, ORANGE)
        H = tex(r"H=\int_\Omega \frac{|\mathbf{m}|^{2}}{2\rho}+\rho\,u\!\left(\rho,\frac{\sigma}{\rho}\right)", 40, YELLOW)
        SH = VGroup(S, H).arrange(DOWN, buff=0.7).move_to([PANEL_X, 0.4, 0])
        clamp_width(SH, 7.4)
        self.sync(CUES[95].start)
        step(1, SH, outs=[xi, hl], rt=0.9)
        # cue 96: third, the Poisson bracket for which S is a Casimir
        cas = tex(r"\{F,S\}=0\quad\forall F\qquad(S\ \text{is a Casimir})", 34, ORANGE)
        pb = VGroup(
            tex(r"\{F,G\}=-\!\int_\Omega \mathbf{m}\cdot\big[F_{\mathbf m}\cdot\nabla G_{\mathbf m}"
                r"-G_{\mathbf m}\cdot\nabla F_{\mathbf m}\big]", 26),
            tex(r"\qquad+\,\rho\big[F_{\mathbf m}\cdot\nabla G_\rho-G_{\mathbf m}\cdot\nabla F_\rho\big]", 26),
            tex(r"\qquad+\,\sigma\big[F_{\mathbf m}\cdot\nabla G_\sigma-G_{\mathbf m}\cdot\nabla F_\sigma\big]", 26),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.2)
        c3 = VGroup(cas, pb).arrange(DOWN, buff=0.5).move_to([PANEL_X, 0.4, 0])
        clamp_width(c3, 7.4)
        self.at(96)
        step(2, c3, outs=[SH], rt=0.9)
        self.snap("poisson")
        # cue 97: fourth, K-N construction that separates local thermodynamics from phenomenology
        M = tex(r"M(dF,dG)=F_\sigma\,G_\sigma", 38, ORANGE)
        Sg = tex(r"\Sigma(dF,dG)=\mathcal{L}_*^{(\alpha)}\!\big(F_{\xi^\alpha}\big)\,\frac{L^{\alpha\beta}}{H_\sigma}\,"
                 r"\mathcal{L}_*^{(\beta)}\!\big(G_{\xi^\beta}\big)", 30, GREEN)
        ms = VGroup(M, Sg).arrange(DOWN, buff=0.45).move_to([PANEL_X, 1.4, 0])
        clamp_width(ms, 7.4)
        eom = tex(r"\dot F=\{F,H\}+(F,H;S,H),\qquad R=M\KN\Sigma", 32, YELLOW)
        clamp_width(eom, 7.4)
        eom.next_to(ms, DOWN, buff=0.5)
        sep = note("separates local thermodynamics (M) from phenomenological quantities (Σ)", 20, GREY_B)
        clamp_width(sep, 7.4)
        sep.next_to(eom, DOWN, buff=0.3)
        sep.move_to(RIGHT * 2.5+ DOWN *1.0)
        self.at(97)
        step(3, ms, outs=[c3], rt=0.9)
        self.frac(97, 0.4, Write(eom), run_time=1.4)
        self.frac(97, 0.7, FadeIn(sep), run_time=0.6)
        self.snap("kn_step")
        # cue 98-99: the most general solution possible
        gen = chip("about the most general solution possible", GREEN, 26, fill=0.1).move_to([PANEL_X, -2.5, 0.0])
        self.at(98, FadeIn(gen, shift=UP * 0.15), run_time=0.7)
        self.at(99, Indicate(gen, color=GREEN, scale_factor=1.05), run_time=0.8)
        # cue 100: narrow it down by defining the dissipative functions L* and L
        box = SurroundingRectangle(Sg, color=YELLOW, buff=0.12, corner_radius=0.08)
        todo = note("to specialize: define the maps  L*  and the matrix  L  -- a future video", 22, YELLOW)
        clamp_width(todo, 7.4)
        todo.move_to([PANEL_X, -2.5, 0.0])
        todo.shift(LEFT * 1.0)
        self.at(100, Create(box), FadeOut(gen), FadeIn(todo), run_time=0.9)
        self.finish()


# ==========================================================================
# F9 -- next directions  (cues 101-106)
# ==========================================================================
class F9(SyncedScene):
    AUDIO_START, AUDIO_END = START["F9"], END["F9"]

    def construct(self):
        header = hdr("Conclusion and Next Steps")
        # cue 101: basics of the 4-bracket are sketched; look at next directions
        self.at(101, Write(header), run_time=0.6)
        t = Text("the basics of the 4-bracket, sketched", font_size=34, color=YELLOW).move_to([0, 0.2, 0])
        bb = br4(80).next_to(t, UP, buff=0.5)
        self.play(FadeIn(t, shift=UP * 0.15), Write(bb), run_time=1.0)
        t2 = Text("where do we go from here?", font_size=34, color=YELLOW).move_to(t)
        self.frac(101, 0.55, Transform(t, t2), run_time=0.5)
        # four direction cards (arranged in a 2x2 grid)
        cs = [card("Unified Thermodynamics\nAlgorithm, in depth", C_G, 24, 5.0),
              card("infinite-dimensional\n4-brackets", C_F, 24, 5.0),
              card("numerical integrators\nfor metriplectic systems", C_K, 24, 5.0),
              card("beyond simple fluids:\nmulti-fluids, kinetic systems", C_N, 24, 5.0)]
        grid = VGroup(*cs).arrange_in_grid(rows=2, cols=2, buff=(0.5, 0.45)).move_to([0, -0.5, 0])
        # cue 102: UTA in depth (a future video)
        self.at(102, FadeOut(t), FadeOut(bb), run_time=0.4)
        self.play(FadeIn(cs[0], shift=UP * 0.2), run_time=0.7)
        # cue 103: infinite-dimensional 4-brackets (only finite-dimensional so far)
        self.at(103, FadeIn(cs[1], shift=UP * 0.2), run_time=0.7)
        # cue 104-105: numerical integrators, as symplectic systems have symplectic integrators
        self.at(104, FadeIn(cs[2], shift=UP * 0.2), run_time=0.7)
        sy = note("just as symplectic systems have symplectic integrators", 20, GREEN)
        sy.next_to(cs[2], DOWN, buff=0.15)
        self.at(105, FadeIn(sy), run_time=0.5)
        # cue 106: beyond simple fluids
        self.at(106, FadeIn(cs[3], shift=UP * 0.2), run_time=0.7)
        self.snap("next")
        self.finish()