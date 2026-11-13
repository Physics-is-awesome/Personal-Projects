#!/usr/bin/env python3

"""
metriplectic_integration.py
================================================================================
Manim animation for "Metriplectic Dynamics, part 3: structure-preserving
integrators for metriplectic 4-brackets", synchronized to the narration track
(metriplectic_integration.srt).  Built on the same machinery as
metriplectic_4bracket.py (SyncedScene, cue-driven PLAN, Emacs-buffer chrome).

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh metriplectic_integration.py G0
    ...  or all of them:
    for s in G0 G1 G2 G2_1 G3 G3_1 G3_2 G3_3 G4 G5 G5_1 G6 G6_1 G7 G7_1 G7_2 \
             G8 G8_1 G9 G9_1 G10; do python3 -m manim -qh metriplectic_integration.py $s; done

Scene map (script section -> scene -> narration cues in the .srt)
    G0     title card, prerequisites (parts 1 & 2)                cues   1-  3
    G1     scope: the Barham-Morrison-Zaidni paper, NSF example    cues   4-  5
    G2     review: Hamiltonian + metric, Casimir, free energy      cues   6- 12
    G2_1   review: <<f,g>>, evolution, 4-bracket                   cues  13- 18
    G3     general theory: why numerics, entropy variable          cues  19- 25
    G3_1   what a discretization must preserve                     cues  26- 29
    G3_2   method of lines, mimetic methods                        cues  30- 34
    G3_3   FEEC, DEC, Galerkin                                     cues  35- 44
    G4     temporal discretization: discrete gradients, Itoh-Abe/AVF cues 45- 48
    G5     setting up: notation, the NSF 4-bracket                 cues  49- 51
    G5_1   2-bracket, dissipative flow, Poisson part, weak form    cues  52- 56
    G6     spatial discretization: periodic domain, V_h, H and S   cues  57- 62
    G6_1   discrete antisymmetric bracket, Jacobi identity         cues  63- 66
    G7     discrete 4-bracket, L2 projections, delta s_h           cues  67- 72
    G7_1   observable, variational problem, discrete brackets      cues  73- 75
    G7_2   the three main features                                 cues  76- 83
    G8     DAE form, interpretation of dH, momentum equation       cues  84- 89
    G8_1   thermodynamic consistency (semi-discrete)               cues  90- 92
    G9     AVF scheme, implicit midpoint, quadrature               cues  93- 98
    G9_1   energy conservation, entropy production, order          cues  99-103
    G10    numerical experiments, thanks                           cues 104-107

Audio sync, SNAP debugging, the late-beat report and the emacs_theme fallback
all work exactly as in metriplectic_4bracket.py:
    SNAP=1 python3 -m manim -ql metriplectic_integration.py G7_1   -> snaps/*.png
    METRI_INT_SRT=/path/to/file.srt                                  -> custom SRT

Optional result images for G10 (shown if present, otherwise a labelled
placeholder is drawn -- no fake data is ever plotted):
    energy_error.png   (|H^n - H^0| vs t)      entropy.png   (S^n vs t)
    or point at them with  METRI_INT_IMG_ENERGY=...  METRI_INT_IMG_ENTROPY=...
================================================================================
"""

import os
import re
import math
from collections import namedtuple
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

# Our own TeX template (independent of the theme module): amsmath + amssymb
# plus short macros for the (long) discrete-problem formulas.
_TPL = TexTemplate()
_TPL.add_to_preamble(r"\usepackage{amsmath,amssymb}")
_TPL.add_to_preamble(r"\newcommand{\KN}{\mathbin{\bigcirc\mkern-15mu\wedge}}")
_TPL.add_to_preamble(r"\newcommand{\bu}{\boldsymbol{u}_h}")
_TPL.add_to_preamble(r"\newcommand{\bv}{\boldsymbol{v}_h}")
_TPL.add_to_preamble(r"\newcommand{\bw}{\boldsymbol{w}_h}")
_TPL.add_to_preamble(r"\newcommand{\dbh}{\delta\boldsymbol{h}_h}")
_TPL.add_to_preamble(r"\newcommand{\dbs}{\delta\boldsymbol{s}_h}")
_TPL.add_to_preamble(r"\newcommand{\dx}{\partial_x}")
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
    cands = [os.environ.get("METRI_INT_SRT")]
    try:
        cands.append(Path(__file__).with_name("metriplectic_integration.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "metriplectic_integration.srt",
              Path("~/Music/metriplectic_integration.srt").expanduser(),
              Path("/mnt/user-data/uploads/metriplectic_integration.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError(
        "metriplectic_integration.srt not found (set METRI_INT_SRT=/path/to/file)")


def _load_cues(path):
    block = re.compile(
        r"(\d+)\s*\n(\d\d):(\d\d):(\d\d),(\d\d\d)\s*-->\s*(\d\d):(\d\d):(\d\d),(\d\d\d)"
        r"\s*\n(.*?)(?=\n\s*\n|\Z)", re.S)
    sec = lambda h, m, s, ms: int(h) * 3600 + int(m) * 60 + int(s) + int(ms) / 1000
    cues = {}
    text = Path(path).read_text(encoding="utf-8").replace("\r\n", "\n")
    for m in block.finditer(text.strip() + "\n\n"):
        g = m.groups()
        cues[int(g[0])] = Cue(sec(*g[1:5]), sec(*g[5:9]), " ".join(g[9].split()))
    return cues


CUES = _load_cues(_find_srt())
TOTAL_DURATION = max(c.end for c in CUES.values())

# scene name -> first cue.  A scene runs from the start of its first cue to the
# start of the next scene's first cue (G0 starts at t = 0).
PLAN = [("G0", 1), ("G1", 4), ("G2", 6), ("G2_1", 13), ("G3", 19), ("G3_1", 26),
        ("G3_2", 30), ("G3_3", 35), ("G4", 45), ("G5", 49), ("G5_1", 52),
        ("G6", 57), ("G6_1", 63), ("G7", 67), ("G7_1", 73), ("G7_2", 76),
        ("G8", 84), ("G8_1", 90), ("G9", 93), ("G9_1", 99), ("G10", 104)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="integrators.tex"):
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
        """Fade everything out except persistent chrome and `keep`."""
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

    def clear_content(self, *keep, run_time=0.4):
        """Fade out everything on screen except chrome, the header and `keep`."""
        objs = [m for m in self.mobjects
                if m not in self._persist and m not in keep and not isinstance(m, ValueTracker)]
        if objs:
            self.play(FadeOut(*objs), run_time=run_time)


# --------------------------------------------------------------------------
# Small building blocks (shared with the 4-bracket file)
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


def eq(s, y, size=32, color=None, x=LEFT_X, maxw=12.2):
    """Left-aligned, width-clamped equation line."""
    m = tex(s, size, color)
    clamp_width(m, maxw)
    return at_left(m, y, x)


# --------------------------------------------------------------------------
# Integrator-specific building blocks
# --------------------------------------------------------------------------
C_F, C_K, C_G, C_N = BLUE, GREEN, ORANGE, PURPLE      # the four 4-bracket slots
C_POI, C_MET = BLUE, ORANGE                           # Poisson / metric (as in parts 1-2)
C_H, C_S = YELLOW, GREEN                              # energy / entropy


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


def hat_axes(center, width=5.0, height=2.0, nodes=5, colors=None):
    """Piecewise-linear 'hat' basis functions on a uniform 1-D grid (p = 1 CG)."""
    ax = Axes(x_range=[0, nodes - 1, 1], y_range=[0, 1.2, 1], x_length=width, y_length=height,
              axis_config={"include_tip": False, "include_numbers": False, "stroke_width": 2})
    ax.move_to(center)
    cols = colors or [BLUE, TEAL, GREEN, YELLOW, ORANGE, RED]
    plots = VGroup()
    for i in range(nodes):
        lo, hi = max(0, i - 1), min(nodes - 1, i + 1)
        g = ax.plot(lambda x, i=i: max(0.0, 1 - abs(x - i)), x_range=[lo, hi],
                    color=cols[i % len(cols)], stroke_width=4)
        plots.add(g)
    return ax, plots


def lattice(origin, nx, ny, d, color, shift=(0.0, 0.0)):
    """Thin square grid lines starting at `origin` (lower left)."""
    g = VGroup()
    ox, oy = origin[0] + shift[0], origin[1] + shift[1]
    for i in range(nx + 1):
        g.add(Line([ox + i * d, oy, 0], [ox + i * d, oy + ny * d, 0], color=color, stroke_width=2))
    for j in range(ny + 1):
        g.add(Line([ox, oy + j * d, 0], [ox + nx * d, oy + j * d, 0], color=color, stroke_width=2))
    return g


def maybe_image(env_name, default_name, width, placeholder_text):
    """ImageMobject if the file exists, otherwise a dashed placeholder frame."""
    cands = [os.environ.get(env_name), default_name, Path("results") / default_name]
    try:
        cands.append(Path(__file__).with_name(default_name))
    except NameError:
        pass
    for c in cands:
        if c and Path(c).exists():
            im = ImageMobject(str(c))
            im.set_width(width)
            return im
    box = DashedVMobject(RoundedRectangle(corner_radius=0.12, width=width, height=width * 0.62,
                                          color=GREY_B), num_dashes=60)
    t = note(placeholder_text, 20, GREY_B)
    clamp_width(t, width - 0.5)
    t.move_to(box.get_center())
    return VGroup(box, t)


# ==========================================================================
# G0 -- title card + prerequisites   (cues 1-3)
# ==========================================================================
class G0(SyncedScene):
    AUDIO_START, AUDIO_END = START["G0"], END["G0"]
    STARS = (40, 11)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Metriplectic dynamics, part 3: integrators for 4-brackets", FG)],
                          font_size=26)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: welcome to another video on metriplectic dynamics
        self.at(1, Write(title), run_time=1.4)

        # cue 2: numerical methods for 4-brackets (2-brackets too)
        big = br4(84).move_to([-4.0, 0.3, 0])
        self.sync(CUES[1].start + 1.5)
        self.play(Write(big), run_time=1.2)
        zs = [tex(r"z^{n}", 52), tex(r"z^{n+1}", 52), tex(r"z^{n+2}", 52)]
        row = VGroup(*zs).arrange(RIGHT, buff=1.1).move_to([3.3, 0.3, 0])
        arrs = VGroup(*[Arrow(a.get_right(), b.get_left(), buff=0.1, color=WHITE, stroke_width=4)
                        for a, b in zip(zs[:-1], zs[1:])])
        lab = note("numerical methods for metriplectic 4-brackets", 26, YELLOW).move_to([0, -1.5, 0])
        two = note("(abstracts to 2-brackets as well)", 22, GREY_B).move_to([0, -2.1, 0])
        self.at(2, FadeIn(lab, shift=UP * 0.15), run_time=0.6)
        self.frac(2, 0.25, LaggedStart(FadeIn(zs[0]), GrowArrow(arrs[0]), FadeIn(zs[1]),
                                       GrowArrow(arrs[1]), FadeIn(zs[2]), lag_ratio=0.3),
                  run_time=1.6)
        self.frac(2, 0.7, FadeIn(two), run_time=0.5)

        # cue 3: watch the previous two videos; this review is brief
        self.at(3, *[FadeOut(m) for m in [big, row, arrs, lab, two]], run_time=0.5)
        c1 = chip("Part 1: why Hamiltonian dynamics forbids dissipation", BLUE, 24)
        c2 = chip("Part 2: the metriplectic 4-bracket", BLUE, 24)
        col = VGroup(c1, c2).arrange(DOWN, buff=0.45).move_to([0, 0.6, 0])
        self.play(FadeIn(c1, shift=UP * 0.2), run_time=0.6)
        self.frac(3, 0.3, FadeIn(c2, shift=UP * 0.2), run_time=0.6)
        a = note("recommended first -- the review in this video is extremely brief", 22,
                 YELLOW).move_to([0, -1.4, 0])
        self.frac(3, 0.6, FadeIn(a), run_time=0.5)
        self.snap("prereq")
        self.finish()


# ==========================================================================
# G1 -- scope  (cues 4-5)
# ==========================================================================
class G1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G1"], END["G1"]

    def construct(self):
        header = hdr("Scope")
        # cue 4: mimic the Barham-Morrison-Zaidni paper
        self.at(4, Write(header), run_time=0.6)
        paper = card("A thermodynamically consistent discretization of 1D thermal-fluid\n"
                     "models using their metriplectic 4-bracket structure\n\n"
                     "W. Barham, P. J. Morrison, A. Zaidni", TEAL, 28, 12.6)
        paper.move_to([0, 0.6, 0])
        self.sync(CUES[4].start + 0.8)
        self.play(FadeIn(paper, shift=UP * 0.2), run_time=0.8)
        self.frac(4, 0.5, Indicate(paper, color=YELLOW, scale_factor=1.03), run_time=0.8)
        rd = note("this video closely follows the paper", 24, GREY_B).move_to([0, -1.6, 0])
        self.frac(4, 0.75, FadeIn(rd), run_time=0.5)

        # cue 5: general ideas of metriplectic integrators, then the simplest concrete example
        self.at(5, *[FadeOut(m) for m in [paper, rd]], run_time=0.5)
        c1 = chip("general ideas of metriplectic integrators", C_F, 30, fill=0.08)
        c2 = chip("concrete example: Navier-Stokes-Fourier, a thermal fluid", C_G, 30, fill=0.08)
        pair = VGroup(c1, c2).arrange(DOWN, buff=0.9).move_to([0, 0.3, 0])
        clamp_width(pair)
        arr = Arrow(c1.get_bottom(), c2.get_top(), buff=0.12, color=WHITE)
        self.play(FadeIn(c1, shift=UP * 0.2), run_time=0.6)
        self.frac(5, 0.45, GrowArrow(arr), FadeIn(c2, shift=UP * 0.2), run_time=0.8)
        s = note("the simplest example I could find", 22, GREY_B).next_to(c2, DOWN, buff=0.3)
        self.frac(5, 0.7, FadeIn(s), run_time=0.5)
        self.snap("scope")
        self.finish()


# ==========================================================================
# G2 -- quick review of metriplectic systems  (cues 6-12)
# ==========================================================================
class G2(SyncedScene):
    AUDIO_START, AUDIO_END = START["G2"], END["G2"]

    def construct(self):
        header = hdr("Review of Metriplectic")
        # cue 6: expand Hamiltonian dynamics into dissipative / irreversible systems
        ham = chip("Hamiltonian dynamics", C_POI, 28, fill=0.08)
        dis = chip("dissipative, irreversible systems", C_MET, 28, fill=0.08)
        row = VGroup(ham, dis).arrange(RIGHT, buff=2.2).move_to([0, 1.2, 0])
        clamp_width(row)
        arr = Arrow(ham.get_right(), dis.get_left(), buff=0.12, color=WHITE)
        self.at(6, Write(header), FadeIn(ham), run_time=0.6)
        self.frac(6, 0.4, GrowArrow(arr), FadeIn(dis), run_time=0.9)
        # cue 7: traditional Hamiltonians are limited: fundamentally symplectic
        sy = note("limited: fundamentally symplectic", 24, C_POI).next_to(ham, DOWN, buff=0.3)
        self.at(7, FadeIn(sy), run_time=0.6)
        # cue 8: Riemannian geometry, hence the metric
        rg = note("expand into Riemannian geometry  →  a metric", 24, C_MET).next_to(dis, DOWN, buff=0.3)
        gij = tex(r"g_{ij}", 44, C_MET).next_to(rg, DOWN, buff=0.3)
        self.at(8, FadeIn(rg), Write(gij), run_time=0.9)

        # cue 9: antisymmetric Poisson bracket + symmetric metric bracket
        self.at(9, *[FadeOut(m) for m in [row, arr, sy, rg, gij]], run_time=0.5)
        cP = card("Poisson bracket\nantisymmetric\nconservative Hamiltonian flow", C_POI, 26, 5.6)
        cM = card("metric bracket\nsymmetric\ndissipation and entropy generation", C_MET, 28, 6.6)
        plus = tex(r"+", 56)
        trio = VGroup(cP, plus, cM).arrange(RIGHT, buff=0.5).move_to([0, 0.5, 0])
        clamp_width(trio)
        self.play(FadeIn(cP, shift=RIGHT * 0.2), FadeIn(plus), FadeIn(cM, shift=LEFT * 0.2), run_time=0.9)

        # cue 10: together: first and second laws; start from the Poisson bracket, Casimir = entropy
        laws = chip("first and second laws, together", YELLOW, 26, fill=0.1).move_to([0, -2.2, 0])
        self.at(10, FadeIn(laws, shift=UP * 0.15), run_time=0.6)
        self.frac(10, 0.2, FadeOut(trio), FadeOut(laws), run_time=0.6)
        X = LEFT_X
        e1 = eq(r"\dot f=\{f,g\}", 1.9, 40, x=X)
        e1n = note("start with the Poisson bracket", 20, C_POI).next_to(e1, RIGHT, buff=0.6)
        e2 = eq(r"\{C,f\}=0\quad\forall f", 0.9, 40, YELLOW, x=X)
        e2n = note("Casimir functionals of the Poisson bracket", 20).next_to(e2, RIGHT, buff=0.6)
        e3 = eq(r"0=\{S,H\}", -0.1, 40, x=X)
        e3n = note("here the Casimir is the entropy", 20, C_MET).next_to(e3, RIGHT, buff=0.6)
        self.frac(10, 0.4, Write(e1), FadeIn(e1n), run_time=0.9)
        self.frac(10, 0.6, Write(e2), FadeIn(e2n), run_time=0.9)
        self.frac(10, 0.8, Write(e3), FadeIn(e3n), run_time=0.9)

        # cue 11: Hamiltonian free energy, lambda arbitrary constant left over
        e4 = eq(r"F=H+\lambda C", -1.2, 44, GREEN, x=X)
        e4n = note("Hamiltonian free energy", 20, GREEN).next_to(e4, RIGHT, buff=0.6)
        self.at(11, Write(e4), FadeIn(e4n), run_time=1.0)
        e4m = note("lambda: an arbitrary constant, simply left over", 20, GREY_B).move_to([0, -2.2, 0])
        self.frac(11, 0.55, FadeIn(e4m), run_time=0.5)
        # cue 12: in the MSTDOF model lambda is minus the temperature
        e5 = eq(r"\lambda=-T", -2.2, 40, YELLOW, x=X)
        e5n = note("in the MSTDOF model: minus the temperature", 20, YELLOW).next_to(e5, RIGHT, buff=0.6)
        self.at(12, FadeOut(e4m), Write(e5), FadeIn(e5n), run_time=0.9)
        self.snap("review")
        self.finish()


# ==========================================================================
# G2_1 -- the metriplectic equations and the 4-bracket  (cues 13-18)
# ==========================================================================
class G2_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G2_1"], END["G2_1"]

    def construct(self):
        header = hdr("Review of Metriplectic")
        self.add(header)
        X = LEFT_X
        # cue 13: expand the system <<f,g>> = {f,g} + (f,g), symmetric bilinear semi-definite
        f1 = eq(r"\langle\langle f,g\rangle\rangle=\{f,g\}+(f,g)", 1.8, 44, x=X)
        f1n = note("expand the system via F", 20, GREY_B).next_to(f1, RIGHT, buff=0.6)
        f2 = eq(r"(f,g)\ \text{symmetric,\ bilinear,\ semi-definite}", 0.9, 34, C_MET, x=X)
        self.at(13, Write(f1), FadeIn(f1n), run_time=1.4)
        self.frac(13, 0.45, Write(f2), run_time=1.4)
        # cue 14: evolution given by this equation
        f3 = eq(r"\dot f=\langle\langle f,F\rangle\rangle", -0.2, 44, YELLOW, x=X)
        self.at(14, Write(f3), run_time=0.9)
        # cue 15: (f,g) defined so that lambda drops out of the equations of motion
        f3n = note("(f,g) is defined so that the lambda in F cancels out of the equation of motion",
                   20, GREY_B).move_to([0, -1.3, 0])
        clamp_width(f3n, 12.0)
        self.at(15, FadeIn(f3n), run_time=0.7)
        # cue 16: H conserved -> metriplectic evolution
        f4 = eq(r"(H,f)=0\quad\forall f", -2.1, 38, C_MET, x=X)
        f4n = note("so that H is conserved", 20, GREY_B).next_to(f4, RIGHT, buff=0.6)
        self.at(16, Write(f4), FadeIn(f4n), run_time=1.0)

        # cue 17: metriplectic evolution, Casimir mimics entropy, push to a 4-bracket
        self.at(17, *[FadeOut(m) for m in [f1, f1n, f2, f3, f3n, f4, f4n]], run_time=0.5)
        g1 = tex(r"\dot f=\{f,H\}+\lambda\,(f,C)", 52).move_to([0, 1.3, 0])
        g1l = note("metriplectic evolution", 22, YELLOW).next_to(g1, UP, buff=0.2)
        g2 = tex(r"\dot C=\lambda\,(C,C)", 48, C_MET).move_to([0, 0.2, 0])
        g2n = note("the Casimir mimics entropy", 22, C_MET).next_to(g2, RIGHT, buff=0.6)
        self.play(FadeIn(g1l), Write(g1), run_time=1.0)
        self.frac(17, 0.25, Write(g2), FadeIn(g2n), run_time=1.0)
        g3 = tex(r"\dot f=\{f,H\}+(f,H;S,H)", 52, YELLOW).move_to([0, -1.2, 0])
        g3l = note("push further: binary bracket  →  four-bracket", 22, GREEN).next_to(g3, UP, buff=0.3)
        self.frac(17, 0.55, FadeIn(g3l), Write(g3), run_time=1.4)
        # cue 18: generalize an algorithm to define the 4-bracket
        uta = chip("an algorithm (UTA) can now define the 4-bracket systematically", C_G, 26, fill=0.1)
        uta.move_to([0, -2.6, 0])
        clamp_width(uta)
        self.at(18, FadeIn(uta, shift=UP * 0.15), run_time=0.7)
        self.snap("fourbracket")
        self.finish()


# ==========================================================================
# G3 -- general theory: why numerics, entropy variable  (cues 19-25)
# ==========================================================================
class G3(SyncedScene):
    AUDIO_START, AUDIO_END = START["G3"], END["G3"]

    def construct(self):
        header = hdr("General Theory")
        # cue 19: metriplectic is primarily used in fluid and kinetic equations
        self.at(19, Write(header), run_time=0.6)
        fl = chip("fluid equations", TEAL, 30, fill=0.1)
        ki = chip("kinetic equations", PURPLE, 30, fill=0.1)
        row = VGroup(fl, ki).arrange(RIGHT, buff=1.2).move_to([0, 0.9, 0])
        self.sync(CUES[19].start + 0.8)
        self.play(FadeIn(fl, shift=UP * 0.2), run_time=0.6)
        self.frac(19, 0.45, FadeIn(ki, shift=UP * 0.2), run_time=0.6)
        # cue 20: like most fluid or kinetic equations, solved numerically
        pde = tex(r"\partial_t\boldsymbol{\xi}=\mathcal{F}[\boldsymbol{\xi}]", 44).move_to([-3.4, -1.0, 0])
        cmp_ = chip("computer", GREY_B, 26).move_to([3.4, -1.0, 0])
        arr = Arrow(pde.get_right() + RIGHT * 0.2, cmp_.get_left() + LEFT * 0.2, buff=0, color=WHITE)
        num = note("must be solved numerically", 24, YELLOW).move_to([0, -2.1, 0])
        self.at(20, FadeIn(pde), GrowArrow(arr), FadeIn(cmp_), FadeIn(num), run_time=1.0)
        # cue 21: geometric properties we would like to conserve
        self.at(21, *[FadeOut(m) for m in [row, pde, cmp_, arr, num]], run_time=0.5)
        a = chip("symplectic geometry / Hamiltonian mechanics", C_POI, 26, fill=0.08)
        b = chip("special geometric properties", YELLOW, 26, fill=0.08)
        c = chip("worth conserving in the numerics", GREEN, 26, fill=0.08)
        col = VGroup(a, b, c).arrange(DOWN, buff=0.55).move_to([0, 0.3, 0])
        self.play(FadeIn(a, shift=UP * 0.15), run_time=0.5)
        self.frac(21, 0.35, FadeIn(b, shift=UP * 0.15), run_time=0.5)
        self.frac(21, 0.7, FadeIn(c, shift=UP * 0.15), run_time=0.5)

        # cue 22: entropy, not internal energy, is the prognostic variable
        self.at(22, FadeOut(col), run_time=0.4)
        usual = tex(r"(\rho,\ \mathbf{m},\ e)", 52).move_to([-3.6, 0.9, 0])
        usual_l = note("usual: internal energy e", 22, GREY_B).next_to(usual, DOWN, buff=0.3)
        metr = tex(r"(\rho,\ \mathbf{m},\ \sigma)", 52, C_S).move_to([3.6, 0.9, 0])
        metr_l = note("metriplectic: entropy density σ", 22, C_S).next_to(metr, DOWN, buff=0.3)
        vs = note("vs.", 24).move_to([0, 0.9, 0])
        self.play(FadeIn(usual), FadeIn(usual_l), run_time=0.7)
        self.frac(22, 0.4, FadeIn(vs), FadeIn(metr), FadeIn(metr_l), run_time=0.8)
        pv = note("entropy is the prognostic variable", 24, YELLOW).move_to([0, -1.2, 0])
        self.frac(22, 0.7, FadeIn(pv), run_time=0.5)
        # cue 23: incompatible with common tools for hyperbolic conservation laws
        tool = chip("standard tools for hyperbolic conservation laws", GREY_B, 26).move_to([-2.0, -2.4, 0])
        bad = cross().next_to(tool, RIGHT, buff=0.3)
        inc = note("incompatible", 24, RED).next_to(bad, RIGHT, buff=0.3)
        self.at(23, FadeIn(tool), FadeIn(bad), FadeIn(inc), run_time=0.8)

        # cue 24-25: many unique properties we wish to satisfy in discretization and integration
        self.at(24, *[FadeOut(m) for m in [usual, usual_l, metr, metr_l, vs, pv, tool, bad, inc]],
                run_time=0.5)
        prop = chip("several unique properties of metriplectic systems", YELLOW, 30, fill=0.1).move_to([0, 1.5, 0])
        self.play(FadeIn(prop, shift=UP * 0.15), run_time=0.6)
        d1 = chip("space: discretization", C_F, 26, fill=0.08)
        d2 = chip("time: integration", C_K, 26, fill=0.08)
        dd = VGroup(d1, d2).arrange(RIGHT, buff=1.4).move_to([0, -0.7, 0])
        l1 = Arrow(prop.get_bottom(), d1.get_top(), buff=0.12, color=WHITE)
        l2 = Arrow(prop.get_bottom(), d2.get_top(), buff=0.12, color=WHITE)
        self.at(25, GrowArrow(l1), GrowArrow(l2), FadeIn(d1), FadeIn(d2), run_time=0.9)
        w = note("we want to satisfy many of these in both", 24, GREY_B).move_to([0, -2.1, 0])
        self.frac(25, 0.5, FadeIn(w), run_time=0.5)
        self.snap("props")
        self.finish()


# ==========================================================================
# G3_1 -- what a discretization must preserve  (cues 26-29)
# ==========================================================================
class G3_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G3_1"], END["G3_1"]

    def construct(self):
        header = hdr("Properties to Preserve")
        # cue 26: algebraic bracket properties, in both space and time
        self.at(26, Write(header), run_time=0.6)
        t26 = note("when discretizing, both space and time must preserve:", 26, YELLOW).move_to([0, 2.0, 0])
        self.play(FadeIn(t26), run_time=0.5)
        k1 = card("skew-symmetry\nof the bracket", C_POI, 24, 3.9)
        k2 = card("symmetry of the\nmetric bracket", C_MET, 24, 3.9)
        k3 = card("positive\nsemi-definiteness", C_MET, 24, 3.9)
        cs = VGroup(k1, k2, k3).arrange(RIGHT, buff=0.45).move_to([0, 0.6, 0])
        q1 = tex(r"\{F,G\}=-\{G,F\}", 32).next_to(k1, DOWN, buff=0.4)
        q2 = tex(r"[F,G]=[G,F]", 32).next_to(k2, DOWN, buff=0.4)
        q3 = tex(r"[F,F]\ \geq\ 0", 32).next_to(k3, DOWN, buff=0.4)
        self.frac(26, 0.2, FadeIn(k1, shift=UP * 0.2), Write(q1), run_time=0.8)
        self.frac(26, 0.45, FadeIn(k2, shift=UP * 0.2), Write(q2), run_time=0.8)
        self.frac(26, 0.7, FadeIn(k3, shift=UP * 0.2), Write(q3), run_time=0.8)

        # cue 27: degeneracy conditions: energy as Poisson generator & metric Casimir
        self.at(27, *[FadeOut(m) for m in [t26, cs, q1, q2, q3]], run_time=0.5)
        t27 = note("and the degeneracy conditions", 26, YELLOW).move_to([0, 2.0, 0])
        self.play(FadeIn(t27), run_time=0.4)
        en = card("energy H_d:  Poisson generator  &  metric Casimir\n"
                  "drives the reversible dynamics, invisible to the dissipative bracket",
                  YELLOW, 22, 11.4).move_to([0, 0.9, 0])
        qe = tex(r"[H_d,F]=0\quad\forall F", 36, YELLOW).move_to([0, -0.2, 0])
        self.frac(27, 0.2, FadeIn(en, shift=UP * 0.2), run_time=0.8)
        self.frac(27, 0.55, Write(qe), run_time=1.0)
        # cue 28: entropy as metric generator & Poisson Casimir
        sn = card("entropy S_d:  metric generator  &  Poisson Casimir\n"
                  "drives the dissipative dynamics, invisible to the reversible bracket",
                  GREEN, 22, 11.4).move_to([0, -1.5, 0])
        qs = tex(r"\{S_d,F\}=0\quad\forall F", 36, GREEN).move_to([0, -2.6, 0])
        self.at(28, FadeIn(sn, shift=UP * 0.2), Write(qs), run_time=1.2)
        # cue 29: other Casimir invariants, e.g. mass, must hold in both discrete brackets
        self.at(29, *[FadeOut(m) for m in [t27, en, qe, sn, qs]], run_time=0.5)
        ms = card("any other Casimir invariant, such as mass,\nmust hold in the discrete form of both brackets",
                  TEAL, 26, 9.6).move_to([0, 0.8, 0])
        mq = tex(r"\{M_d,F\}=0,\qquad [M_d,F]=0", 38, TEAL).move_to([0, -1.0, 0])
        self.play(FadeIn(ms, shift=UP * 0.2), run_time=0.7)
        self.frac(29, 0.4, Write(mq), run_time=1.0)
        self.snap("degeneracy")
        self.finish()


# ==========================================================================
# G3_2 -- method of lines, mimetic idea  (cues 30-34)
# ==========================================================================
class G3_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["G3_2"], END["G3_2"]

    def construct(self):
        header = hdr("Method of Lines")
        # cue 30: discretize space: infinite-dimensional PDE -> finite-dimensional ODE system
        self.at(30, Write(header), run_time=0.6)
        pde = card("PDE\n(infinite-dim.)", C_F, 24, 2.9).move_to([-5.0, 0.8, 0])
        ode = card("ODE system\n(finite-dim.)", C_K, 24, 2.9).move_to([0.3, 0.8, 0])
        a1 = Arrow(pde.get_right(), ode.get_left(), buff=0.12, color=WHITE)
        a1l = note("discretize space", 20, YELLOW).next_to(a1, UP, buff=0.12)
        self.sync(CUES[30].start + 0.8)
        self.play(FadeIn(pde, shift=UP * 0.2), run_time=0.6)
        self.frac(30, 0.4, GrowArrow(a1), FadeIn(a1l), FadeIn(ode, shift=LEFT * 0.2), run_time=1.0)
        # cue 31: then discretize time
        sol = card("states u^n\n(discrete)", C_G, 24, 2.9).move_to([5.3, 0.8, 0])
        a2 = Arrow(ode.get_right(), sol.get_left(), buff=0.12, color=WHITE)
        a2l = note("discretize time", 20, YELLOW).next_to(a2, UP, buff=0.12)
        self.at(31, GrowArrow(a2), FadeIn(a2l), FadeIn(sol, shift=LEFT * 0.2), run_time=0.9)
        # cue 32: both steps require structure-preserving methods
        br = Brace(VGroup(pde, sol), DOWN, buff=0.3, color=GREEN)
        brl = note("both steps require structure-preserving methods", 24, GREEN).next_to(br, DOWN, buff=0.15)
        self.at(32, GrowFromCenter(br), FadeIn(brl), run_time=0.7)

        # cue 33: spatial discretization: a couple of commonly used methods
        self.at(33, *[FadeOut(m) for m in [pde, ode, sol, a1, a1l, a2, a2l, br, brl]], run_time=0.4)
        t33 = note("space: a couple of commonly used methods", 28, YELLOW).move_to([0, 1.6, 0])
        self.play(FadeIn(t33, shift=UP * 0.15), run_time=0.5)
        # cue 34: artificial dissipation destroys energy; mimetic methods mimic the geometry
        art = chip("artificial dissipation", RED, 28)
        mim = chip("mimetic methods: mimic differential geometry", GREEN, 28, fill=0.08)
        pair = VGroup(art, mim).arrange(DOWN, buff=0.8).move_to([-0.5, -0.4, 0])
        clamp_width(pair, 10.0)
        bad = cross().next_to(art, RIGHT, buff=0.3)
        bn = note("destroys energy", 22, RED).next_to(bad, RIGHT, buff=0.3)
        ok = tick().next_to(mim, RIGHT, buff=0.3)
        self.at(34, FadeIn(art), FadeIn(bad), FadeIn(bn), run_time=0.7)
        self.frac(34, 0.45, FadeIn(mim, shift=UP * 0.15), FadeIn(ok), run_time=0.8)
        self.snap("mol")
        self.finish()


# ==========================================================================
# G3_3 -- FEEC, DEC, Galerkin  (cues 35-44)
# ==========================================================================
class G3_3(SyncedScene):
    AUDIO_START, AUDIO_END = START["G3_3"], END["G3_3"]

    def construct(self):
        header = hdr("Spatial Discretization Methods")
        self.add(header)
        # cue 35: finite element exterior calculus
        t35 = chip("Finite Element Exterior Calculus  (FEEC)", C_F, 32, fill=0.1).move_to([0, 1.5, 0])
        self.at(35, FadeIn(t35, shift=UP * 0.2), run_time=0.7)
        # cue 36: not random grid points, differential forms
        self.at(36, FadeOut(t35), run_time=0.4)
        rng = np.random.default_rng(3)
        pts = VGroup(*[Dot([rng.uniform(-6.0, -2.0), rng.uniform(-1.4, 1.6), 0], radius=0.06,
                           color=GREY_B) for _ in range(14)])
        pn = note("values at grid points", 22, GREY_B).move_to([-4.0, -1.9, 0])
        pb = cross().move_to([-4.0, 2.0, 0])
        d0 = Dot([1.2, 0.6, 0], radius=0.1, color=C_F)
        d1 = Line([2.4, 0.6, 0], [4.0, 0.6, 0], color=C_K, stroke_width=6)
        d2 = Polygon([4.8, 0.0, 0], [6.2, 0.0, 0], [5.5, 1.3, 0], color=C_G, stroke_width=3,
                     fill_color=C_G, fill_opacity=0.25)
        fl = VGroup(note("0-form", 20, C_F).next_to(d0, DOWN, buff=0.5),
                    note("1-form", 20, C_K).next_to(d1, DOWN, buff=0.5),
                    note("2-form", 20, C_G).next_to(d2, DOWN, buff=0.5))
        fn = note("differential forms", 22, GREEN).move_to([3.7, -1.9, 0])
        fk = tick().move_to([3.7, 2.0, 0])
        self.play(FadeIn(pts), FadeIn(pn), FadeIn(pb), run_time=0.8)
        self.frac(36, 0.4, FadeIn(d0), Create(d1), FadeIn(d2), FadeIn(fl), FadeIn(fn), FadeIn(fk),
                  run_time=1.2)
        # cue 37: observables mapped onto geometric elements
        obs = note("mass, energy, momentum ... mapped onto geometric elements", 24, YELLOW).move_to([0, -2.8, 0])
        self.at(37, FadeIn(obs), run_time=0.6)
        # cue 38: vector calculus identities hold exactly -> skew-symmetry of the Poisson bracket
        self.at(38, *[FadeOut(m) for m in [pts, pn, pb, d0, d1, d2, fl, fn, fk, obs]], run_time=0.4)
        dd = tex(r"d\circ d=0", 64, YELLOW).move_to([0, 1.2, 0])
        vc = tex(r"\nabla\times\nabla f=0,\qquad \nabla\cdot(\nabla\times\mathbf{A})=0", 38).move_to([0, -0.2, 0])
        sk = note("identities hold exactly  →  skew-symmetry of the Poisson bracket is preserved",
                  24, GREEN).move_to([0, -1.5, 0])
        clamp_width(sk, 12.0)
        self.play(Write(dd), run_time=0.9)
        self.frac(38, 0.35, Write(vc), run_time=1.3)
        self.frac(38, 0.7, FadeIn(sk), run_time=0.6)

        # cue 39: mimetic finite difference / DEC: two grids
        self.at(39, *[FadeOut(m) for m in [dd, vc, sk]], run_time=0.4)
        t39 = chip("Mimetic finite differences / Discrete Exterior Calculus  (DEC)", C_K, 28, fill=0.08)
        t39.move_to([0, 1.7, 0])
        clamp_width(t39)
        self.play(FadeIn(t39, shift=UP * 0.15), run_time=0.6)
        g1 = lattice([-2.4, -2.6], 6, 3, 1.0, C_POI)
        g2 = lattice([-2.4, -2.6], 5, 2, 1.0, C_MET, shift=(0.5, 0.5))
        l1 = note("one grid:\nreversible (Poisson)", 20, C_POI).move_to([-5.2, -1.0, 0])
        l2 = note("a second grid:\nirreversible (metric)", 20, C_MET).move_to([5.3, -1.0, 0])
        self.frac(39, 0.2, Create(g1), FadeIn(l1), run_time=1.0)
        self.frac(39, 0.5, Create(g2), FadeIn(l2), run_time=1.0)
        self.snap("dec")

        # cue 40-43: Galerkin: discontinuous (done) vs continuous (here)
        self.at(40, *[FadeOut(m) for m in [t39, g1, g2, l1, l2]], run_time=0.4)
        t40 = chip("Galerkin methods", C_G, 32, fill=0.1).move_to([0, 1.7, 0])
        self.play(FadeIn(t40, shift=UP * 0.15), run_time=0.5)
        dg = chip("discontinuous Galerkin: already covered in an earlier video", GREY_B, 24)
        cg = chip("continuous Galerkin: this video", GREEN, 28, fill=0.1)
        self.at(41, FadeIn(dg.move_to([0, 0.5, 0])), run_time=0.6)
        ax, hats = hat_axes([0, -1.2, 0], 7.0, 2.0)
        self.at(42, dg.animate.set_opacity(0.35), FadeIn(cg.move_to([0, -0.3, 0])), run_time=0.6)
        cg.generate_target()
        cg.target.move_to([0, 1.0, 0])
        self.play(MoveToTarget(cg), FadeOut(dg), Create(ax), run_time=0.7)
        self.play(LaggedStart(*[Create(h) for h in hats], lag_ratio=0.25), run_time=1.0)
        e = note("because it is easier -- and this video is already hard enough", 22, YELLOW).move_to([0, -3.0, 0])
        self.at(43, FadeIn(e), run_time=0.5)
        # cue 44: other established methods exist
        oth = note("other methods exist, but these are the most established", 22, GREY_B).move_to([0, -3.0, 0])
        self.at(44, FadeOut(e), FadeIn(oth), run_time=0.5)
        self.snap("galerkin")
        self.finish()


# ==========================================================================
# G4 -- temporal discretization  (cues 45-48)
# ==========================================================================
class G4(SyncedScene):
    AUDIO_START, AUDIO_END = START["G4"], END["G4"]

    def construct(self):
        header = hdr("Temporal Discretization")
        # cue 45: almost all metriplectic integrators are implicit, like symplectic ones
        self.at(45, Write(header), run_time=0.6)
        a = chip("almost all metriplectic integrators are implicit", C_F, 30, fill=0.08).move_to([0, 1.3, 0])
        b = note("just like symplectic ones", 26, GREEN).next_to(a, DOWN, buff=0.35)
        imp = tex(r"\boldsymbol{u}^{n+1}=\boldsymbol{u}^{n}+\Delta t\,\Phi(\boldsymbol{u}^{n},\boldsymbol{u}^{n+1})", 40)
        imp.move_to([0, -1.0, 0])
        clamp_width(imp, 11.0)
        self.sync(CUES[45].start + 0.8)
        self.play(FadeIn(a, shift=UP * 0.2), run_time=0.6)
        self.frac(45, 0.4, FadeIn(b), run_time=0.5)
        self.frac(45, 0.6, Write(imp), run_time=1.2)
        # cue 46: replace partial derivatives by discrete gradients
        self.at(46, *[FadeOut(m) for m in [a, b, imp]], run_time=0.5)
        t46 = note("replace continuous partial derivatives with discrete gradients", 26, YELLOW).move_to([0, 1.8, 0])
        dg = tex(r"F(z_{n+1})-F(z_n)=\bar\nabla F(z_n,z_{n+1})\cdot(z_{n+1}-z_n)", 44)
        clamp_width(dg, 12.0)
        dg.move_to([0, 0.5, 0])
        self.play(FadeIn(t46), run_time=0.5)
        self.frac(46, 0.25, Write(dg), run_time=1.8)
        # cue 47-48: two primary methods: Itoh-Abe and AVF
        self.at(47, *[FadeOut(m) for m in [t46, dg]], run_time=0.4)
        P0 = np.array([-5.4, -1.4, 0.0])
        P1 = np.array([-1.4, 1.0, 0.0])
        Q0 = np.array([1.6, -1.4, 0.0])
        Q1 = np.array([5.6, 1.0, 0.0])
        ia_t = note("Itoh-Abe", 30, ORANGE).move_to([-3.4, 1.9, 0])
        avf_t = note("AVF  (this video)", 30, GREEN).move_to([3.0, 1.9, 0])
        self.play(FadeIn(ia_t), FadeIn(avf_t), run_time=0.5)
        z0a, z1a = Dot(P0, color=WHITE), Dot(P1, color=WHITE)
        z0b, z1b = Dot(Q0, color=WHITE), Dot(Q1, color=WHITE)
        zl = VGroup(tex(r"z_n", 30).next_to(z0a, DOWN, buff=0.15), tex(r"z_{n+1}", 30).next_to(z1a, UP, buff=0.15),
                    tex(r"z_n", 30).next_to(z0b, DOWN, buff=0.15), tex(r"z_{n+1}", 30).next_to(z1b, UP, buff=0.15))
        self.play(FadeIn(z0a), FadeIn(z1a), FadeIn(z0b), FadeIn(z1b), FadeIn(zl), run_time=0.5)
        # staircase: one coordinate at a time
        mid1 = np.array([P1[0], P0[1], 0.0])
        stair = VGroup(Arrow(P0, mid1, buff=0.08, color=ORANGE, stroke_width=5),
                       Arrow(mid1, P1, buff=0.08, color=ORANGE, stroke_width=5))
        ia_n = note("update one coordinate at a time,\nrest held fixed", 20, ORANGE)
        ia_n.move_to([-3.4, -2.7, 0])
        self.sync(CUES[47].start + 0.9)
        self.play(Create(stair[0]), run_time=0.7)
        self.play(Create(stair[1]), FadeIn(ia_n), run_time=0.7)
        # cue 48: AVF = average vector field
        line = Arrow(Q0, Q1, buff=0.08, color=GREEN, stroke_width=5)
        avf_n = note("average vector field\nalong the straight segment", 20, GREEN).move_to([3.6, -2.7, 0])
        self.at(48, Create(line), FadeIn(avf_n), run_time=1.0)
        self.snap("discgrad")
        self.finish()


# ==========================================================================
# G5 -- setting up the problem  (cues 49-51)
# ==========================================================================
class G5(SyncedScene):
    AUDIO_START, AUDIO_END = START["G5"], END["G5"]

    def construct(self):
        header = hdr("Setting up the Problem")
        # cue 49: not deriving the reference equations; read the paper
        self.at(49, Write(header), run_time=0.6)
        t = card("the reference equations are not derived here\nsee the paper for the derivations",
                 TEAL, 28, 9.4).move_to([0, 0.7, 0])
        self.sync(CUES[49].start + 0.8)
        self.play(FadeIn(t, shift=UP * 0.2), run_time=0.8)
        # cue 50: functional-derivative shorthand
        sh = tex(r"\frac{\delta F}{\delta u}=F_u", 64, YELLOW).move_to([0, -1.6, 0])
        shn = note("functional derivative shorthand, as in the paper", 22, GREY_B).next_to(sh, DOWN, buff=0.3)
        self.at(50, FadeIn(sh, shift=UP * 0.2), FadeIn(shn), run_time=0.9)
        # cue 51: the 4-bracket from the UTA
        self.at(51, *[FadeOut(m) for m in [t, sh, shn]], run_time=0.4)
        l0 = eq(r"(F,K;G,N)=\int_\Omega(\Sigma\KN M)(F,K,G,N)\,dx", 1.9, 36, YELLOW)
        l1 = eq(r"=\int_\Omega\frac1T\Big[\mu\big(K_\sigma\dx F_m-F_\sigma\dx K_m\big)"
                r"\big(N_\sigma\dx G_m-G_\sigma\dx N_m\big)", 0.7, 34, C_F)
        l2 = eq(r"\qquad+\frac{\kappa}{T}\big(K_\sigma\dx F_\sigma-F_\sigma\dx K_\sigma\big)"
                r"\big(N_\sigma\dx G_\sigma-G_\sigma\dx N_\sigma\big)\Big]dx", -0.6, 34, C_G)
        ln = note("4-bracket of Navier-Stokes-Fourier, built with the UTA", 22, GREY_B).move_to([0, -1.9, 0])
        vl = note("μ: viscosity", 20, C_F).move_to([-4.2, -2.7, 0])
        hl = note("κ: thermal conductivity", 20, C_G).move_to([-0.6, -2.7, 0])
        self.play(Write(l0), run_time=1.4)
        self.frac(51, 0.4, Write(l1), run_time=1.6)
        self.frac(51, 0.7, Write(l2), FadeIn(ln), FadeIn(vl), FadeIn(hl), run_time=1.6)
        self.snap("nsf4")
        self.finish()


# ==========================================================================
# G5_1 -- 2-bracket, dissipative flow, conservative part, weak form  (cues 52-56)
# ==========================================================================
class G5_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G5_1"], END["G5_1"]

    def construct(self):
        header = hdr("Setting up the Problem")
        self.add(header)
        # cue 52: the 2-bracket is a 4-bracket with H in two slots -> dissipative flow
        e1 = eq(r"(F,G)_H:=(F,H;G,H)", 1.9, 38, YELLOW)
        e1n = note("2-bracket from the 4-bracket", 20, GREY_B).next_to(e1, RIGHT, buff=0.6)
        e2 = eq(r"(F,S)_H=\int_\Omega\Big(-\Sigma(F,H)\,M(S,H)+M(F,S)\,\Sigma(H,H)\Big)dx", 0.9, 32)
        e3 = eq(r"=\int_\Omega\Big(-\Big[(\dx F_m)\,\mu\,\dx u+(\dx F_\sigma)\tfrac{\kappa}{T}\dx T\Big]"
                r"+F_\sigma\Big[\tfrac{\mu}{T}(\dx u)^2+\tfrac{\kappa}{T^2}(\dx T)^2\Big]\Big)dx",
                -0.2, 30, C_MET)
        e3n = note("dissipative flow", 20, C_MET).move_to([0, -0.95, 0])
        self.at(52, Write(e1), FadeIn(e1n), run_time=1.2)
        self.frac(52, 0.3, Write(e2), run_time=1.8)
        self.frac(52, 0.65, Write(e3), FadeIn(e3n), run_time=2.0)
        # cue 53: conservative portion
        c1 = eq(r"\{F,H\}=-\int_\Omega\Big[m\big(F_m\dx u-u\dx F_m\big)+\rho\big(F_m\dx\eta-u\dx F_\rho\big)"
                r"+\sigma\big(F_m\dx T-u\dx F_\sigma\big)\Big]dx", -1.8, 30, C_POI)
        c1n = note("conservative portion", 20, C_POI).move_to([0, -2.55, 0])
        self.at(53, Write(c1), FadeIn(c1n), run_time=2.0)
        # cue 54: combine -> weak evolution equation
        self.at(54, *[FadeOut(m) for m in [e1, e1n, e2, e3, e3n, c1, c1n]], run_time=0.5)
        w = tex(r"\dot F\ =\ \{F,H\}\ +\ (F,S)_H", 64).move_to([0, 0.6, 0])
        wl = note("weak evolution equation of Navier-Stokes-Fourier", 24, YELLOW).next_to(w, UP, buff=0.5)
        b1 = note("conservative", 22, C_POI).move_to([-0.3, -0.6, 0])
        b2 = note("dissipative", 22, C_MET).move_to([3.0, -0.6, 0])
        self.play(FadeIn(wl), Write(w), run_time=1.3)
        self.frac(54, 0.6, FadeIn(b1), FadeIn(b2), run_time=0.5)
        # cue 55-56: strong form recoverable but not needed; the structure is where FEM is derived
        self.at(55, *[FadeOut(m) for m in [w, wl, b1, b2]], run_time=0.4)
        s1 = chip("the strong form of Navier-Stokes-Fourier can be recovered", GREY_B, 26).move_to([0, 1.0, 0])
        self.play(FadeIn(s1, shift=UP * 0.15), run_time=0.6)
        s2 = chip("but we don't need it: the finite-element method is derived\n"
                  "from the metriplectic structure, not the evolution equations", GREEN, 26, fill=0.08)
        s2.move_to([0, -1.2, 0])
        clamp_width(s2)
        self.at(56, FadeIn(s2, shift=UP * 0.15), run_time=0.8)
        self.snap("weak")
        self.finish()


# ==========================================================================
# G6 -- spatial discretization: domain, V_h, H and S  (cues 57-62)
# ==========================================================================
class G6(SyncedScene):
    AUDIO_START, AUDIO_END = START["G6"], END["G6"]

    def construct(self):
        header = hdr("Spatial Discretization")
        # cue 57: periodic domain Omega = [0,L]/~
        self.at(57, Write(header), run_time=0.6)
        seg = Line([-4.5, 1.6, 0], [4.5, 1.6, 0], color=WHITE, stroke_width=5)
        e0 = Dot(seg.get_start(), color=YELLOW)
        eL = Dot(seg.get_end(), color=YELLOW)
        l0 = tex(r"0", 30).next_to(e0, DOWN, buff=0.2)
        lL = tex(r"L", 30).next_to(eL, DOWN, buff=0.2)
        om = tex(r"\Omega=[0,L]/\!\sim,\qquad L>0", 40).move_to([0, 0.3, 0])
        self.sync(CUES[57].start + 0.8)
        self.play(Create(seg), FadeIn(e0), FadeIn(eL), FadeIn(l0), FadeIn(lL), run_time=0.9)
        self.frac(57, 0.45, Write(om), run_time=1.2)
        # cue 58: equivalence identifies endpoints; V_h in H^1; CG on a uniform grid
        glue = CurvedArrow(eL.get_center() + UP * 0.15, e0.get_center() + UP * 0.15, angle=TAU / 3.2,
                           color=YELLOW, stroke_width=3)
        gl = note("endpoints identified", 20, YELLOW).next_to(seg, UP, buff=0.55)
        ticks = VGroup(*[Line([x, 1.45, 0], [x, 1.75, 0], color=TEAL, stroke_width=3)
                         for x in np.linspace(-4.5, 4.5, 13)])
        self.at(58, Create(glue), FadeIn(gl), run_time=0.8)
        self.frac(58, 0.5, Create(ticks), run_time=0.8)
        tg = note("uniform grid  T_h  on Ω", 20, TEAL).next_to(seg, DOWN, buff=0.75)
        tg.shift(RIGHT * 2.2)
        self.frac(58, 0.75, FadeIn(tg), run_time=0.4)
        # cue 59: V_h definition
        vh = eq(r"V_h=\big\{v_h\in H^1(\Omega):\ v_h|_K\in\mathbb{P}^p(K),\ \forall K\in\mathcal{T}_h\big\}",
                -1.0, 40, YELLOW, maxw=12.2)
        vhn = note("degree-p continuous Galerkin finite-element space", 22, GREY_B).move_to([0, -1.9, 0])
        self.at(59, FadeOut(om), Write(vh), FadeIn(vhn), run_time=1.8)
        # cue 60: P^p(K) = degree-p polynomials on K
        k_x = (ticks[5].get_center()[0] + ticks[6].get_center()[0]) / 2
        kbox = Rectangle(width=ticks[6].get_center()[0] - ticks[5].get_center()[0], height=0.5,
                         color=YELLOW, stroke_width=3, fill_color=YELLOW, fill_opacity=0.2).move_to([k_x, 1.6, 0])
        kl = tex(r"K", 34, YELLOW).next_to(kbox, UP, buff=0.3)
        pk = note("P^p(K): polynomials of degree p on the cell K", 22, YELLOW).move_to([0, -2.7, 0])
        self.at(60, FadeIn(kbox), FadeIn(kl), FadeIn(pk), run_time=0.8)

        # cue 61: method of lines; fields modelled in the Galerkin subspace
        self.at(61, *[FadeOut(m) for m in [seg, e0, eL, l0, lL, glue, gl, ticks, tg, vh, vhn, kbox, kl, pk]],
                run_time=0.5)
        fl = tex(r"\rho_h,\ \ m_h,\ \ \sigma_h\ \in\ V_h", 56).move_to([-1.8, 1.2, 0])
        fln = note("all dynamical fields are modelled in the Galerkin subspace", 22, GREY_B).next_to(fl, DOWN, buff=0.4)
        ax, hats = hat_axes([3.6, -1.4, 0], 5.4, 2.0)
        self.play(Write(fl), FadeIn(fln), run_time=1.2)
        self.frac(61, 0.35, Create(ax), run_time=0.5)
        self.play(LaggedStart(*[Create(h) for h in hats], lag_ratio=0.2), run_time=1.2)
        mol = note("method of lines: space first", 22, YELLOW).move_to([-3.6, -1.7, 0])
        self.frac(61, 0.7, FadeIn(mol), run_time=0.5)
        # cue 62: Hamiltonian and entropy, then discretize them
        self.at(62, *[FadeOut(m) for m in [fl, fln, ax, hats, mol]], run_time=0.5)
        H = eq(r"H[\rho,m,\sigma]=\int_\Omega\Big(\frac{m^2}{2\rho}+\rho\,U\big(\rho,\tfrac{\sigma}{\rho}\big)\Big)dx",
               1.9, 34, C_H)
        S = eq(r"S[\sigma]=\int_\Omega\sigma\,dx", 0.8, 34, C_S)
        ar = Arrow([0, 0.2, 0], [0, -0.45, 0], buff=0, color=WHITE)
        arl = note("restrict to V_h", 20, YELLOW).next_to(ar, RIGHT, buff=0.2)
        Hh = eq(r"H^h[\rho_h,m_h,\sigma_h]=\int_\Omega\Big(\frac{m_h^2}{2\rho_h}+\rho_h\,U\big(\rho_h,\tfrac{\sigma_h}{\rho_h}\big)\Big)dx",
                -1.2, 34, C_H)
        Sh = eq(r"S^h[\sigma_h]=\int_\Omega\sigma_h\,dx", -2.3, 34, C_S)
        self.play(Write(H), run_time=1.4)
        self.play(Write(S), run_time=0.8)
        self.frac(62, 0.5, GrowArrow(ar), FadeIn(arl), Write(Hh), run_time=1.6)
        self.frac(62, 0.8, Write(Sh), run_time=0.9)
        self.snap("HS")
        self.finish()


# ==========================================================================
# G6_1 -- discrete antisymmetric bracket, Jacobi identity  (cues 63-66)
# ==========================================================================
class G6_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G6_1"], END["G6_1"]

    def construct(self):
        header = hdr("Spatial Discretization")
        self.add(header)
        # cue 63: same method on the antisymmetric bracket
        b0 = eq(r"\{F^h,G^h\}_h(\rho_h,m_h,\sigma_h)=-\int_\Omega\Big[", 1.9, 34, C_POI)
        b1 = eq(r"m_h\big(F^h_{m_h}\dx G^h_{m_h}-G^h_{m_h}\dx F^h_{m_h}\big)", 1.0, 32, x=LEFT_X + 1.0)
        b2 = eq(r"+\ \rho_h\big(F^h_{m_h}\dx G^h_{\rho_h}-G^h_{m_h}\dx F^h_{\rho_h}\big)", 0.1, 32, x=LEFT_X + 1.0)
        b3 = eq(r"+\ \sigma_h\big(F^h_{m_h}\dx G^h_{\sigma_h}-G^h_{m_h}\dx F^h_{\sigma_h}\big)\Big]dx",
                -0.8, 32, x=LEFT_X + 1.0)
        self.at(63, Write(b0), run_time=1.0)
        self.frac(63, 0.3, Write(b1), run_time=0.9)
        self.frac(63, 0.55, Write(b2), run_time=0.9)
        self.frac(63, 0.75, Write(b3), run_time=0.9)
        # cue 64: antisymmetric bracket, not a Poisson bracket: Jacobi identity fails
        self.at(64, *[FadeOut(m) for m in [b0, b1, b2, b3]], run_time=0.5)
        nm = card("an antisymmetric bracket, not a Poisson bracket", ORANGE, 28, 10.0).move_to([0, 1.8, 0])
        jac = tex(r"\{F,\{G,H\}\}+\{H,\{F,G\}\}+\{G,\{H,F\}\}\ \neq\ 0", 42).move_to([0, 0.2, 0])
        jn = note("the Jacobi identity fails", 24, RED).move_to([0, -0.9, 0])
        cr = cross().next_to(jn, LEFT, buff=0.3)
        self.play(FadeIn(nm, shift=UP * 0.2), run_time=0.7)
        self.frac(64, 0.3, Write(jac), run_time=1.6)
        self.frac(64, 0.65, FadeIn(jn), FadeIn(cr), run_time=0.6)
        # cue 65: no known method in fluid mechanics preserves it
        nk = note("no known method in fluid mechanics preserves this property when discretizing",
                  24, GREY_B).move_to([0, -2.0, 0])
        clamp_width(nk, 12.0)
        self.at(65, FadeIn(nk), run_time=0.7)
        # cue 66: but antisymmetry, energy and entropy are conserved -> sufficient
        self.at(66, *[FadeOut(m) for m in [nm, jac, jn, cr, nk]], run_time=0.5)
        r1 = chip("antisymmetry", C_POI, 28, fill=0.08)
        r2 = chip("energy", C_H, 28, fill=0.08)
        r3 = chip("entropy", C_S, 28, fill=0.08)
        row = VGroup(r1, r2, r3).arrange(RIGHT, buff=0.8).move_to([-0.6, 0.8, 0])
        ticks = VGroup(*[tick().next_to(r, DOWN, buff=0.25) for r in (r1, r2, r3)])
        self.play(LaggedStart(*[FadeIn(r, shift=UP * 0.15) for r in row], lag_ratio=0.25), run_time=1.0)
        self.frac(66, 0.4, FadeIn(ticks), run_time=0.5)
        suf = note("all conserved by our method  →  sufficient for our work", 26, GREEN).move_to([0, -1.5, 0])
        self.frac(66, 0.65, FadeIn(suf), run_time=0.6)
        self.snap("antisym")
        self.finish()


# ==========================================================================
# G7 -- discrete 4-bracket, projections, delta s_h  (cues 67-72)
# ==========================================================================
class G7(SyncedScene):
    AUDIO_START, AUDIO_END = START["G7"], END["G7"]

    def construct(self):
        header = hdr("The Discrete Brackets")
        self.add(header)
        # cue 67: the discrete 4-bracket
        g0 = eq(r"(F^h,K^h;G^h,N^h)_h=\frac{1}{\mathrm{Re}}\int_\Omega\frac{1}{T_h}\Big[\big(K^h_{\sigma_h}\dx F^h_{m_h}"
                r"-F^h_{\sigma_h}\dx K^h_{m_h}\big)\big(N^h_{\sigma_h}\dx G^h_{m_h}-G^h_{\sigma_h}\dx N^h_{m_h}\big)",
                1.9, 28, YELLOW)
        g1 = eq(r"\qquad+\frac{1}{\mathrm{Pr}}\frac{\gamma}{\gamma-1}\frac{1}{T_h}\big(K^h_{\sigma_h}\dx F^h_{\sigma_h}"
                r"-F^h_{\sigma_h}\dx K^h_{\sigma_h}\big)\big(N^h_{\sigma_h}\dx G^h_{\sigma_h}-G^h_{\sigma_h}\dx N^h_{\sigma_h}\big)\Big]dx",
                0.8, 28, YELLOW)
        gn = note("the discrete metriplectic 4-bracket", 22, YELLOW).move_to([0, -0.3, 0])
        self.at(67, Write(g0), run_time=1.6)
        self.play(Write(g1), FadeIn(gn), run_time=1.4)
        # cue 68: L2 projection onto V_h gives the functional derivatives of H
        self.at(68, *[FadeOut(m) for m in [g0, g1, gn]], run_time=0.5)
        q0 = eq(r"Q_{V_h}:\ L^2\text{-projection onto }V_h", 1.95, 32, TEAL)
        h1 = eq(r"H^h_{\rho_h}=Q_{V_h}\Big(-\frac{m_h^2}{2\rho_h^2}+U+\rho_h\,\partial_1U-\frac{\rho_h}{\sigma_h}\,\partial_2U\Big)",
                1.0, 32, C_H)
        h2 = eq(r"H^h_{m_h}=Q_{V_h}\Big(\frac{m_h}{\rho_h}\Big)", 0.0, 32, C_H)
        h3 = eq(r"H^h_{\sigma_h}=Q_{V_h}\big(\partial_2U(\rho_h,\sigma_h/\rho_h)\big)", -0.8, 32, C_H)
        hn = note("functional derivatives of the Hamiltonian, found by projection", 20, GREY_B).move_to([0, -1.5, 0])
        self.play(Write(q0), run_time=1.0)
        self.frac(68, 0.3, Write(h1), run_time=1.6)
        self.frac(68, 0.6, Write(h2), run_time=0.8)
        self.frac(68, 0.8, Write(h3), FadeIn(hn), run_time=1.2)
        # cue 69: entropy derivatives wrt rho and m are zero ...
        s1 = eq(r"S^h_{\rho_h}=S^h_{m_h}=0", -2.2, 36, C_S)
        s1n = note("(rho: mass density, m: momentum density)", 20, GREY_B).next_to(s1, RIGHT, buff=0.6)
        self.at(69, Write(s1), FadeIn(s1n), run_time=1.2)
        # cue 70: ... but wrt sigma it is one
        s2 = eq(r"S^h_{\sigma_h}=1", -2.9, 36, C_S)
        s2n = note("σ: the entropy density itself", 20, GREY_B).next_to(s2, RIGHT, buff=0.6)
        self.at(70, Write(s2), FadeIn(s2n), run_time=0.9)
        # cue 71: V_h interpolates constants exactly -> delta s_h = (0,0,1)
        self.at(71, *[FadeOut(m) for m in [q0, h1, h2, h3, hn, s1, s1n, s2, s2n]], run_time=0.5)
        why = note("V_h interpolates constant functions exactly", 26, TEAL).move_to([0, 1.8, 0])
        ds = tex(r"\delta\boldsymbol{s}_h=(0,\ 0,\ 1)", 64, C_S).move_to([0, 0.4, 0])
        self.play(FadeIn(why), run_time=0.6)
        self.frac(71, 0.35, Write(ds), run_time=1.4)
        # cue 72: vector of derivatives of S wrt the three dynamical fields
        lb = VGroup(note("ρ_h\nmass density", 22, GREY_B), note("m_h\nmomentum density", 22, GREY_B),
                    note("σ_h\nentropy density", 22, C_S)).arrange(RIGHT, buff=1.4).move_to([0, -1.8, 0])
        vn = note("notation matching the paper: derivatives of the entropy with respect to the three dynamical fields",
                  20, YELLOW).move_to([0, -2.9, 0])
        clamp_width(vn, 12.2)
        self.at(72, FadeIn(lb, shift=UP * 0.15), FadeIn(vn), run_time=0.9)
        self.snap("ds")
        self.finish()


# ==========================================================================
# G7_1 -- observable, variational problem, discrete brackets  (cues 73-75)
# ==========================================================================
class G7_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G7_1"], END["G7_1"]

    def construct(self):
        header = hdr("The Variational Problem")
        self.add(header)
        # cue 73: an observable of the form F^h = (phi_m,m_h)+(phi_rho,rho_h)+(phi_sigma,sigma_h)
        o = eq(r"F^h=(\phi_m,m_h)_{L^2}+(\phi_\rho,\rho_h)_{L^2}+(\phi_\sigma,\sigma_h)_{L^2}", 1.0, 40, YELLOW)
        on = note("assume an observable of this form", 22, GREY_B).move_to([0, 2.0, 0])
        self.at(73, FadeIn(on), Write(o), run_time=1.6)
        # cue 74: variational problem + discrete Poisson + discrete metriplectic bracket
        self.at(74, *[FadeOut(m) for m in [on, o]], run_time=0.5)
        fp = eq(r"\text{find}\ (\bu,\dbh)=\big((\rho_h,m_h,\sigma_h),(\eta_h,u_h,T_h)\big)\in V_h^3\times V_h^3:", 2.0, 28, GREY_B)
        mv = eq(r"(\bv,\partial_t\bu)_{L^2}-\{\bv,\dbh\}(\bu)-(\bv,\dbs)_H(\dbh)+(\dbh-DH(\bu),\bw)_{L^2}=0",
                1.15, 34, YELLOW)
        mvn = note("for all test functions (v_h, w_h)", 20, GREY_B).move_to([0, 0.5, 0])
        self.play(FadeIn(fp), run_time=0.6)
        self.frac(74, 0.1, Write(mv), FadeIn(mvn), run_time=2.0)
        p1 = eq(r"\{\bv,\dbh\}(\bu)=-(m_h\dx u_h,\phi_m)+(m_hu_h,\dx\phi_m)-(\rho_h\dx\eta_h,\phi_m)", -0.2, 28, C_POI)
        p2 = eq(r"\qquad+(\rho_hu_h,\dx\phi_\rho)-(\sigma_h\dx T_h,\phi_m)+(\sigma_hu_h,\dx\phi_\sigma)", -0.9, 28, C_POI)
        pn = note("discrete Poisson bracket   ( ·,· ) = L² inner product", 20, C_POI).move_to([0, -1.45, 0])
        self.frac(74, 0.4, Write(p1), run_time=1.3)
        self.play(Write(p2), FadeIn(pn), run_time=1.3)
        m1 = eq(r"(\bv,\dbs)_H(\dbh)=-\frac{1}{\mathrm{Re}}\Big[(\dx u_h,\dx\phi_m)-\Big(\frac{(\dx u_h)^2}{T_h},\phi_\sigma\Big)",
                -2.05, 28, C_MET)
        m2 = eq(r"\qquad+\frac{1}{\mathrm{Pr}}\frac{\gamma}{\gamma-1}\Big[\Big(\frac{\dx T_h}{T_h},\dx\phi_\sigma\Big)"
                r"-\Big(\frac{(\dx T_h)^2}{T_h^2},\phi_\sigma\Big)\Big]\Big]", -2.8, 28, C_MET)
        self.frac(74, 0.7, Write(m1), run_time=1.3)
        self.play(Write(m2), run_time=1.3)
        # cue 75: L2 projections impose the derivatives of H
        self.at(75, *[FadeOut(m) for m in [fp, mv, mvn, p1, p2, pn, m1, m2]], run_time=0.5)
        pr = eq(r"(\dbh-DH(\bu),\bw)_{L^2}=\Big(\eta_h-\tfrac{\delta H^h}{\delta\rho_h},\phi_{\eta_h}\Big)"
                r"+\Big(u_h-\tfrac{\delta H^h}{\delta m_h},\phi_{u_h}\Big)+\Big(T_h-\tfrac{\delta H^h}{\delta\sigma_h},\phi_{T_h}\Big)",
                0.4, 30, TEAL)
        prn = note("the L² projections of the derivatives of H are imposed this way", 22, GREY_B).move_to([0, -0.6, 0])
        self.play(Write(pr), FadeIn(prn), run_time=1.8)
        self.snap("variational")
        self.finish()


# ==========================================================================
# G7_2 -- the three main features  (cues 76-83)
# ==========================================================================
class G7_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["G7_2"], END["G7_2"]

    def construct(self):
        header = hdr("Three Main Features")
        # cue 76: three main features
        self.at(76, Write(header), run_time=0.6)
        t = note("three main features", 38, YELLOW).move_to([0, 0.8, 0])
        num = VGroup(*[Text(str(i), font_size=40, color=c) for i, c in ((1, C_F), (2, C_G), (3, C_K))])
        num.arrange(RIGHT, buff=1.2).move_to([0, -0.6, 0])
        self.sync(CUES[76].start + 0.4)
        self.play(FadeIn(t, shift=UP * 0.15), FadeIn(num), run_time=0.6)

        # cue 77-78: derivatives computed by projection; additional degrees of freedom
        self.at(77, FadeOut(t), FadeOut(num), run_time=0.4)
        c1 = chip("1   derivatives as additional degrees of freedom", C_F, 28, fill=0.08).move_to([0, 2.0, 0])
        u = tex(r"\bu=(\rho_h,\,m_h,\,\sigma_h)", 46).move_to([-3.6, 0.4, 0])
        ul = note("evolving state vector", 22, GREY_B).next_to(u, DOWN, buff=0.3)
        dh = tex(r"\dbh=(\eta_h,\,u_h,\,T_h)", 46, C_H).move_to([3.6, 0.4, 0])
        dl = note("derivatives of H, computed by projection", 22, C_H).next_to(dh, DOWN, buff=0.3)
        ne = tex(r"\neq", 56, RED).move_to([0, 0.4, 0])
        self.play(FadeIn(c1, shift=UP * 0.15), run_time=0.6)
        self.frac(77, 0.3, FadeIn(u), FadeIn(ul), run_time=0.7)
        self.frac(77, 0.6, FadeIn(ne), FadeIn(dh), FadeIn(dl), run_time=0.8)
        dist = note("distinct from the evolving state vector", 24, YELLOW).move_to([0, -1.5, 0])
        self.frac(77, 0.85, FadeIn(dist), run_time=0.5)
        dof = note("so we track  δh_h  as extra degrees of freedom", 24, GREEN).move_to([0, -2.4, 0])
        self.at(78, FadeIn(dof), run_time=0.6)

        # cue 79-80: bilinear brackets; one slot is a generating-function derivative, the other the test function
        self.at(79, *[FadeOut(m) for m in [c1, u, ul, dh, dl, ne, dist, dof]], run_time=0.4)
        c2 = chip("2   bilinear two-brackets", C_G, 28, fill=0.08).move_to([0, 2.0, 0])
        br1 = tex(r"\{\ \bv\ ,\ \dbh\ \}(\bu)", 50)
        br1[0].set_color(C_POI)
        br2 = tex(r"(\ \bv\ ,\ \dbs\ )_H(\dbh)", 50)
        br2[0].set_color(C_MET)
        b1l = note("conservative dynamics", 22, C_POI)
        b2l = note("dissipative dynamics", 22, C_MET)
        A = VGroup(br1, b1l).arrange(DOWN, buff=0.35).move_to([-3.6, 0.4, 0])
        B = VGroup(br2, b2l).arrange(DOWN, buff=0.35).move_to([3.6, 0.4, 0])
        self.play(FadeIn(c2, shift=UP * 0.15), run_time=0.5)
        self.frac(79, 0.2, FadeIn(A, shift=UP * 0.15), FadeIn(B, shift=UP * 0.15), run_time=0.8)
        s1 = note("one argument: derivative of the generating function  (δh_h  or  δs_h)", 22, YELLOW).move_to([0, -1.4, 0])
        s2 = note("the other: an arbitrary test function  v_h", 22, WHITE).move_to([0, -2.1, 0])
        self.at(80, FadeIn(s1), run_time=0.6)
        self.frac(80, 0.5, FadeIn(s2), run_time=0.6)

        # cue 81-83: nonlinear field dependence
        self.at(81, *[FadeOut(m) for m in [c2, A, B, s1, s2]], run_time=0.4)
        c3 = chip("3   nonlinear field dependence", C_K, 28, fill=0.08).move_to([0, 2.0, 0])
        self.play(FadeIn(c3, shift=UP * 0.15), run_time=0.5)
        a1 = tex(r"\{\cdot,\cdot\}", 50, C_POI).move_to([-3.6, 0.5, 0])
        a1d = tex(r"\text{depends on the state}\ \bu", 34, C_POI).next_to(a1, DOWN, buff=0.4)
        a2 = tex(r"(\cdot,\cdot)_H", 50, C_MET).move_to([3.6, 0.5, 0])
        a2d = tex(r"\text{depends on}\ \dbh", 34, C_MET).next_to(a2, DOWN, buff=0.4)
        self.at(82, FadeIn(a1), FadeIn(a1d), run_time=0.8)
        self.frac(82, 0.4, FadeIn(a2), FadeIn(a2d), run_time=0.8)
        # cue 83: dependence on dh (not u) dictated by the 4-bracket formalism, essential for energy conservation
        box = SurroundingRectangle(a2d, color=YELLOW, buff=0.15, corner_radius=0.1)
        why = note("dictated by the 4-bracket formalism,\nand essential for energy conservation", 24, YELLOW)
        why.move_to([3.6, -2.2, 0])
        self.at(83, Create(box), FadeIn(why), run_time=0.9)
        self.snap("features")
        self.finish()


# ==========================================================================
# G8 -- DAE form, interpretation, momentum equation  (cues 84-89)
# ==========================================================================
class G8(SyncedScene):
    AUDIO_START, AUDIO_END = START["G8"], END["G8"]

    def construct(self):
        header = hdr("Differential-Algebraic Form")
        # cue 84: L2 projections as additional fields -> semi-discrete DAE
        self.at(84, Write(header), run_time=0.6)
        d1 = eq(r"(\bv,\partial_t\bu)_{L^2}=\{\bv,\dbh\}(\bu)+(\bv,\dbs)_H(\dbh)", 1.6, 38)
        d1n = note("differential part: evolves the state", 20, GREY_B).move_to([0, 0.75, 0])
        d2 = eq(r"(\dbh-DH(\bu),\bw)_{L^2}=0", -0.3, 38, TEAL)
        d2n = note("algebraic part: the projected derivatives of H", 20, TEAL).move_to([0, -1.15, 0])
        dae = chip("a differential-algebraic equation (DAE)", YELLOW, 28, fill=0.1).move_to([0, -2.4, 0])
        self.sync(CUES[84].start + 0.6)
        self.play(Write(d1), FadeIn(d1n), run_time=1.6)
        self.frac(84, 0.4, Write(d2), FadeIn(d2n), run_time=1.6)
        self.frac(84, 0.75, FadeIn(dae, shift=UP * 0.15), run_time=0.6)
        # cue 85: physical meaning: velocity and temperature
        self.at(85, *[FadeOut(m) for m in [d1, d1n, d2, d2n, dae]], run_time=0.5)
        r1 = eq(r"\frac{\delta H}{\delta m}=u", 1.7, 46, C_H)
        r1n = note("velocity", 26, C_H).next_to(r1, RIGHT, buff=0.8)
        r2 = eq(r"\frac{\delta H}{\delta\sigma}=T", 0.2, 46, C_H)
        r2n = note("temperature", 26, C_H).next_to(r2, RIGHT, buff=0.8)
        self.play(Write(r1), FadeIn(r1n), run_time=1.1)
        self.frac(85, 0.5, Write(r2), FadeIn(r2n), run_time=1.1)
        # cue 86: density derivative ~ enthalpy; pressure gradient via a Bernoulli-like equation
        r3 = eq(r"\frac{\delta H}{\delta\rho}=\eta", -1.3, 46, C_H)
        r3n = note("related to the enthalpy", 26, C_H).next_to(r3, RIGHT, buff=0.8)
        self.at(86, Write(r3), FadeIn(r3n), run_time=1.0)
        bern = eq(r"\Big(\dx p_h+\tfrac12\dx(u_h^2)-\rho_h\dx\eta_h+\sigma_h\dx T_h,\ \phi\Big)_{L^2}=0\quad\forall\phi\in V_h",
                  -2.6, 30, YELLOW)
        bn = note("Bernoulli-like equation: recovers the pressure gradient", 20, GREY_B).move_to([0, -3.15, 0])
        self.frac(86, 0.45, Write(bern), FadeIn(bn), run_time=1.8)
        # cue 87: phi_rho = phi_sigma = 0 -> momentum equation
        self.at(87, *[FadeOut(m) for m in [r1, r1n, r2, r2n, r3, r3n, bern, bn]], run_time=0.5)
        t87 = note("letting  φ_ρ = φ_σ = 0,  the momentum equation", 24, YELLOW).move_to([0, 2.0, 0])
        mo1 = eq(r"(\phi_m,\partial_tm_h)+(m_h\dx u_h,\phi_m)-(m_hu_h,\dx\phi_m)", 1.0, 34)
        mo2 = eq(r"\qquad+(\rho_h\dx\eta_h,\phi_m)+(\sigma_h\dx T_h,\phi_m)+\frac{1}{\mathrm{Re}}(\dx u_h,\dx\phi_m)=0\quad\forall\phi_m\in V_h",
                 0.0, 34)
        self.play(FadeIn(t87), run_time=0.5)
        self.frac(87, 0.15, Write(mo1), run_time=1.6)
        self.frac(87, 0.45, Write(mo2), run_time=2.0)
        # cue 88: the projection equations for eta, u, T
        c1 = eq(r"\eta_h=-\frac{m_h^2}{2\rho_h^2}+U+\rho_h\partial_1U-\frac{\rho_h}{\sigma_h}\partial_2U", -1.3, 30, C_H)
        c2 = eq(r"u_h=\frac{m_h}{\rho_h},\qquad T_h=\partial_2U\Big(\rho_h,\frac{\sigma_h}{\rho_h}\Big)", -2.4, 30, C_H)
        cn = note("each projected: tested against φ_η, φ_u, φ_T", 20, GREY_B).move_to([0, -3.1, 0])
        self.at(88, Write(c1), run_time=1.4)
        self.frac(88, 0.4, Write(c2), FadeIn(cn), run_time=1.4)
        # cue 89: continuity and entropy equations similarly
        self.at(89, *[FadeOut(m) for m in [t87, mo1, mo2, c1, c2, cn]], run_time=0.5)
        sim = chip("continuity and entropy equations follow in a similar fashion", GREEN, 28, fill=0.08)
        sim.move_to([0, 0.4, 0])
        clamp_width(sim)
        self.play(FadeIn(sim, shift=UP * 0.15), run_time=0.7)
        self.snap("dae")
        self.finish()


# ==========================================================================
# G8_1 -- thermodynamic consistency (semi-discrete)  (cues 90-92)
# ==========================================================================
class G8_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G8_1"], END["G8_1"]

    def construct(self):
        header = hdr("Thermodynamic Consistency")
        # cue 90: spatially semi-discretized equations are thermodynamically consistent
        self.at(90, Write(header), run_time=0.6)
        c = chip("the semi-discrete equations are thermodynamically consistent", GREEN, 30, fill=0.1)
        c.move_to([0, 1.9, 0])
        clamp_width(c)
        self.sync(CUES[90].start + 0.7)
        self.play(FadeIn(c, shift=UP * 0.15), FadeIn(tick().next_to(c, RIGHT, buff=0.2)), run_time=0.7)
        ck = note("check it with two choices of test function", 24, GREY_B).move_to([0, 0.8, 0])
        self.frac(90, 0.5, FadeIn(ck), run_time=0.5)
        # cue 91: v_h = dh -> dH/dt = 0
        ax = Axes(x_range=[0, 5, 1], y_range=[0, 3.2, 1], x_length=3.6, y_length=2.4,
                  axis_config={"include_tip": False, "font_size": 12}).move_to([4.7, -1.2, 0])
        Hc = ax.plot(lambda x: 2.0, x_range=[0, 5], color=C_H)
        Sc = ax.plot(lambda x: 0.4 + 2.3 * (1 - math.exp(-0.8 * x)), x_range=[0, 5], color=C_S)
        Hl = tex(r"H", 26, C_H).next_to(ax.c2p(5, 2.0), RIGHT, buff=0.1)
        Sl = tex(r"S", 26, C_S).next_to(ax.c2p(5, 2.6), RIGHT, buff=0.1)
        e1 = eq(r"\bv=\dbh:\qquad \dot H^h=(\dbh,\partial_t\bu)_{L^2}=0", -0.6, 36, C_H, maxw=8.2)
        self.at(91, Write(e1), Create(ax), Create(Hc), FadeIn(Hl), run_time=1.6)
        # cue 92: v_h = ds -> entropy production >= 0
        e2 = eq(r"\bv=\dbs:\qquad \dot S^h=(\dbs,\partial_t\bu)_{L^2}", -1.6, 36, C_S, maxw=8.2)
        e3 = eq(r"=\frac{1}{\mathrm{Re}}\Big[\Big(\frac{(\dx u_h)^2}{T_h},1\Big)+\frac{1}{\mathrm{Pr}}\frac{\gamma}{\gamma-1}"
                r"\Big(\frac{(\dx T_h)^2}{T_h^2},1\Big)\Big]\ \geq\ 0", -2.6, 28, C_S, maxw=8.2)
        self.at(92, Write(e2), Create(Sc), FadeIn(Sl), run_time=1.2)
        self.play(Write(e3), run_time=1.6)
        self.snap("consistent")
        self.finish()


# ==========================================================================
# G9 -- AVF scheme, implicit midpoint, quadrature  (cues 93-98)
# ==========================================================================
class G9(SyncedScene):
    AUDIO_START, AUDIO_END = START["G9"], END["G9"]

    def construct(self):
        header = hdr("Temporal Discretization")
        # cue 93: time stepping based on the average-vector-field discrete gradient
        self.at(93, Write(header), run_time=0.6)
        t93 = chip("time stepping: average-vector-field (AVF) discrete gradient", GREEN, 30, fill=0.1)
        t93.move_to([0, 0.8, 0])
        clamp_width(t93)
        self.sync(CUES[93].start + 0.8)
        self.play(FadeIn(t93, shift=UP * 0.2), run_time=0.7)
        # cue 94: take the FE model and average the discrete vector field
        self.at(94, FadeOut(t93), run_time=0.4)
        a1 = eq(r"\Big(\frac{\bu^{n+1}-\bu^n}{\Delta t},\bv\Big)+\{\dbh^n,\bv\}\Big(\frac{\bu^{n+1}+\bu^n}{2}\Big)"
                r"+(\dbs^n,\bv)_H(\dbh^n)", 1.8, 32)
        a2 = eq(r"\qquad+\big(\overline{DH}(\bu^n,\bu^{n+1})-\dbh^n,\bw\big)_{L^2}=0\quad\forall(\bv,\bw)\in V_h^3\times V_h^3",
                0.8, 32)
        an = note("the finite-element model with the vector field averaged", 20, GREY_B).move_to([0, -0.05, 0])
        self.play(Write(a1), run_time=1.8)
        self.frac(94, 0.45, Write(a2), FadeIn(an), run_time=1.8)
        # cue 95: DH-bar as an integral; midpoint rule gives implicit midpoint
        ax = Axes(x_range=[0, 1, 0.25], y_range=[0, 2.2, 1], x_length=4.8, y_length=2.1,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([3.9, -1.9, 0])
        f = lambda t: 1.0 + 0.6 * math.sin(2.6 * t) + 0.5 * t * t
        gr = ax.plot(f, x_range=[0, 1], color=BLUE)
        area = ax.get_area(gr, x_range=(0, 1), color=BLUE, opacity=0.2)
        tl = tex(r"t", 22).next_to(ax.c2p(1, 0), RIGHT, buff=0.1)
        dh = eq(r"\overline{DH}(\bu^n,\bu^{n+1})=\int_0^1 DH\big((1-t)\bu^n+t\bu^{n+1}\big)\,dt", -1.2, 30, YELLOW, maxw=7.4)
        self.at(95, FadeOut(an), Write(dh), Create(ax), Create(gr), FadeIn(area), FadeIn(tl), run_time=1.6)
        mid = ax.get_riemann_rectangles(gr, x_range=[0, 1], dx=1.0, input_sample_type="center",
                                        stroke_width=2, fill_opacity=0.35, color=ORANGE)
        mn = eq(r"\text{midpoint rule}\ \Longrightarrow\ \text{implicit midpoint method}", -2.5, 28, ORANGE, maxw=7.4)
        self.frac(95, 0.55, FadeIn(mid), Write(mn), run_time=1.4)
        # cue 96: in general the integral must be approximated by quadrature
        qn = note("in general: approximate the integral by quadrature", 22, YELLOW).move_to([-2.4, -3.1, 0])
        self.at(96, FadeOut(mn), FadeOut(mid), FadeIn(qn), run_time=0.6)
        # cue 97-98: Gauss-Legendre with >= 4 points -> energy conserved to machine precision
        xs, _ = np.polynomial.legendre.leggauss(4)
        ts = (xs + 1) / 2
        nodes = VGroup(*[Dot(ax.c2p(t, f(t)), radius=0.07, color=GREEN) for t in ts])
        stems = VGroup(*[ax.get_vertical_line(ax.c2p(t, f(t)), color=GREEN, line_func=Line) for t in ts])
        self.at(97, Create(stems), FadeIn(nodes), run_time=0.9)
        gl = tex(r"\text{Gauss--Legendre quadrature},\ \geq 4\ \text{points}", 32, GREEN).move_to([-2.4, -3.1, 0])
        clamp_width(gl, 7.4)
        self.play(FadeOut(qn), run_time=0.3)
        self.play(FadeIn(gl), run_time=0.4)
        mp = note("energy conserved to machine precision\nin the tests of Barham et al.", 22, GREEN).move_to([-3.0, -2.2, 0])
        self.at(98, FadeOut(dh), FadeIn(mp), run_time=0.6)
        self.snap("avf")
        self.finish()


# ==========================================================================
# G9_1 -- energy conservation, entropy production, order  (cues 99-103)
# ==========================================================================
class G9_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["G9_1"], END["G9_1"]

    def construct(self):
        header = hdr("Temporal Discretization")
        self.add(header)
        # cue 99: v_h = dh, fundamental theorem of calculus -> energy conservation
        t99 = note("take  v_h = δh_h  and use the fundamental theorem of calculus", 24, YELLOW).move_to([0, 2.0, 0])
        l1 = eq(r"\Big(\frac{\bu^{n+1}-\bu^n}{\Delta t},\dbh\Big)_{L^2}=\Big(\frac{\bu^{n+1}-\bu^n}{\Delta t},\overline{DH}(\bu^n,\bu^{n+1})\Big)_{L^2}",
                1.0, 30)
        l2 = eq(r"=\frac{1}{\Delta t}\int_0^1 DH\big((1-t)\bu^n+t\bu^{n+1}\big)\cdot(\bu^{n+1}-\bu^n)\,dt", 0.0, 30)
        l3 = eq(r"=\frac{1}{\Delta t}\int_0^1\frac{d}{dt}H\big((1-t)\bu^n+t\bu^{n+1}\big)\,dt", -0.9, 30)
        l4 = eq(r"=\frac{H(\bu^{n+1})-H(\bu^n)}{\Delta t}=0", -1.9, 36, C_H)
        self.at(99, FadeIn(t99), run_time=0.5)
        self.frac(99, 0.1, Write(l1), run_time=2.0)
        self.frac(99, 0.35, Write(l2), run_time=1.6)
        self.frac(99, 0.6, Write(l3), run_time=1.6)
        self.frac(99, 0.82, Write(l4), run_time=1.4)

        # cue 100: energy conserved; entropy production from v_h = ds^n; consistent
        self.at(100, *[FadeOut(m) for m in [t99, l1, l2, l3, l4]], run_time=0.5)
        ec = eq(r"H^{n+1}=H^n", 1.8, 44, C_H)
        ecn = note("energy conservation verified", 22, C_H).next_to(ec, RIGHT, buff=0.7)
        ent = eq(r"\frac{S^{n+1}-S^n}{\Delta t}=(\dbs^n,\dbs^n)_H(\dbh)", 0.5, 34, C_S)
        ent2 = eq(r"=\frac{1}{\mathrm{Re}}\Big[\Big(\frac{(\dx u_h^n)^2}{T_h^n},1\Big)+\frac{1}{\mathrm{Pr}}\frac{\gamma}{\gamma-1}"
                  r"\Big(\frac{(\dx T_h^n)^2}{(T_h^n)^2},1\Big)\Big]\ \geq\ 0", -0.6, 30, C_S)
        entn = note("positive entropy production:  v_h = δs_h^n", 22, C_S).move_to([0, -1.5, 0])
        cons = chip("the fully discrete method is thermodynamically consistent", GREEN, 28, fill=0.1)
        cons.move_to([0, -2.5, 0])
        clamp_width(cons)
        self.play(Write(ec), FadeIn(ecn), run_time=1.2)
        self.frac(100, 0.25, Write(ent), run_time=1.6)
        self.frac(100, 0.45, Write(ent2), FadeIn(entn), run_time=2.0)
        self.frac(100, 0.8, FadeIn(cons, shift=UP * 0.15), run_time=0.7)

        # cue 101: AVF is second order
        self.at(101, *[FadeOut(m) for m in [ec, ecn, ent, ent2, entn, cons]], run_time=0.5)
        o2 = tex(r"\text{AVF discrete gradient:}\quad O(\Delta t^2)", 48, YELLOW).move_to([0, 1.5, 0])
        ax = Axes(x_range=[-3, 0, 1], y_range=[-6, 0, 2], x_length=4.2, y_length=2.4,
                  axis_config={"include_tip": False, "font_size": 14}).move_to([0, -1.3, 0])
        ref = ax.plot(lambda x: 2 * x, x_range=[-3, -0.5], color=YELLOW)
        xl = tex(r"\log\Delta t", 24).next_to(ax.c2p(0, -6), DOWN, buff=0.15)
        yl = tex(r"\log(\mathrm{error})", 24).next_to(ax.c2p(-3, 0), LEFT, buff=0.15)
        rl = note("slope 2  (reference)", 18, YELLOW).next_to(ax.c2p(-1.4, -2.8), RIGHT, buff=0.15)
        self.play(Write(o2), run_time=1.0)
        self.frac(101, 0.35, Create(ax), Create(ref), FadeIn(xl), FadeIn(yl), FadeIn(rl), run_time=1.4)
        # cue 102-103: further work: Gauss-Legendre IRK and energy-conserving methods fit one framework
        self.at(102, *[FadeOut(m) for m in [o2, ax, ref, xl, yl, rl]], run_time=0.5)
        fw = note("further work: a general framework", 28, YELLOW).move_to([0, 1.7, 0])
        k1 = chip("Gauss-Legendre implicit Runge-Kutta methods", C_F, 26, fill=0.08)
        k2 = chip("energy-conserving methods", C_G, 26, fill=0.08)
        col = VGroup(k1, k2).arrange(DOWN, buff=0.5).move_to([-3.0, -0.4, 0])
        gf = card("general framework", GREEN, 28, 4.2).move_to([4.2, -0.4, 0])
        ar = VGroup(Arrow(k1.get_right(), gf.get_left(), buff=0.12, color=WHITE),
                    Arrow(k2.get_right(), gf.get_left(), buff=0.12, color=WHITE))
        self.play(FadeIn(fw), run_time=0.5)
        self.frac(102, 0.3, FadeIn(k1, shift=UP * 0.15), run_time=0.5)
        self.at(103, FadeIn(k2, shift=UP * 0.15), run_time=0.5)
        self.frac(103, 0.4, *[GrowArrow(a) for a in ar], FadeIn(gf), run_time=0.9)
        n = note("both were recently shown to fit into a single framework", 22, GREY_B).move_to([0, -2.4, 0])
        self.frac(103, 0.7, FadeIn(n), run_time=0.5)
        self.finish()


# ==========================================================================
# G10 -- numerical experiments, thanks  (cues 104-107)
# ==========================================================================
class G10(SyncedScene):
    AUDIO_START, AUDIO_END = START["G10"], END["G10"]
    STARS = (40, 5)

    def construct(self):
        header = hdr("Numerical Experiments")
        # cue 104: authors tested these methods; I replicate to confirm my understanding
        self.at(104, Write(header), run_time=0.6)
        c1 = chip("the authors already tested these methods numerically", GREY_B, 28)
        c2 = chip("to confirm my own understanding (and for fun): replicate their work", GREEN, 28, fill=0.08)
        col = VGroup(c1, c2).arrange(DOWN, buff=0.7).move_to([0, 0.6, 0])
        clamp_width(col)
        arr = Arrow(c1.get_bottom(), c2.get_top(), buff=0.12, color=WHITE)
        self.sync(CUES[104].start + 0.8)
        self.play(FadeIn(c1, shift=UP * 0.2), run_time=0.6)
        self.frac(104, 0.5, GrowArrow(arr), FadeIn(c2, shift=UP * 0.2), run_time=0.9)
        # cue 105: similar results, errors near machine precision
        self.at(105, *[FadeOut(m) for m in [c1, c2, arr]], run_time=0.5)
        pa = maybe_image("METRI_INT_IMG_ENERGY", "energy_error.png", 5.8,
                         "[ insert your energy-error plot here\n  |H^n - H^0| vs t ]").move_to([-3.4, 0.1, 0])
        pb = maybe_image("METRI_INT_IMG_ENTROPY", "entropy.png", 5.8,
                         "[ insert your entropy plot here\n  S^n vs t ]").move_to([3.4, 0.1, 0])
        la = note("energy error", 22, C_H).next_to(pa, DOWN, buff=0.3)
        lb = note("entropy", 22, C_S).next_to(pb, DOWN, buff=0.3)
        self.play(FadeIn(pa), FadeIn(pb), FadeIn(la), FadeIn(lb), run_time=0.9)
        res = note("very similar results to the paper, with errors near machine precision", 24, YELLOW)
        res.move_to([0, -2.6, 0])
        clamp_width(res, 12.0)
        self.frac(105, 0.4, FadeIn(res), run_time=0.6)
        # cue 106: Python + Firedrake
        self.at(106, *[FadeOut(m) for m in [pa, pb, la, lb, res]], run_time=0.5)
        p1 = chip("Python", BLUE, 34, fill=0.1)
        p2 = chip("Firedrake", ORANGE, 34, fill=0.1)
        pr = VGroup(p1, p2).arrange(RIGHT, buff=1.2).move_to([0, 0.5, 0])
        pn = note("used to carry out the discretization", 24, GREY_B).move_to([0, -0.9, 0])
        self.play(FadeIn(p1, shift=UP * 0.2), run_time=0.5)
        self.frac(106, 0.4, FadeIn(p2, shift=UP * 0.2), FadeIn(pn), run_time=0.6)
        # cue 107: thank you
        self.at(107, *[FadeOut(m) for m in [p1, p2, pn]], run_time=0.3)
        ty = Text("Thank you for watching", font_size=44, color=FG).move_to([0, 0.3, 0])
        self.play(FadeIn(ty, shift=UP * 0.2), run_time=0.6)
        self.finish()