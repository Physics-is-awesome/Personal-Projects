"""
kerr_bssn_scenes.py
================================================================================
Manim animation for "Developing a numerical GR code, episode 1: a vacuum Kerr
black hole", synchronized to the narration track (kerr_bssn.srt).

Render one scene at a time (each clip starts at self.time == 0):

    python3 -m manim -pqh kerr_bssn_scenes.py S0
    ...                       or: ./render_all.sh

Scene map (script section -> scene -> narration cues in kerr_bssn.srt)
    S0     title card + outline                          cues   1-  2
    S1     Scope (roadmap, quasi-star, G = c = 1)         cues   3- 13
    S2     Kerr: what is spinning? frame dragging         cues  14- 21
    S2_1   Boyer-Lindquist metric, horizons, coord. sing. cues  22- 26
    S2_2   Kerr-Schild form (g = eta + 2H l l)            cues  27- 30
    S2_3   a = J/M, geometric units                       cues  31- 37
    S3     3+1 split (lapse, shift, gamma, K)             cues  38- 52
    S4     ADM equations + constraint propagation         cues  53- 65
    S4_1   counting components, ADM instability           cues  66- 73
    S5     BSSN (linearised argument)                     cues  74- 83
    S6     gauge freedom, bad slicing / bad shift         cues  84- 89
    S6_1   moving-puncture gauge, frozen-gauge test       cues  90- 98
    S7     initial data (Kerr-Schild -> ADM -> BSSN)      cues  99-107
    S8     cartoon method                                 cues 108-117
    S9     why excision, apparent vs event horizon        cues 118-129
    S9_1   excision: causality, horizon, r_exc, mask      cues 130-135
    S9_2   excision boundary: leakage, Jacobi refill      cues 136-143
    S10    method of lines, stencils, RK4, CFL            cues 144-152
    S10_1  Kreiss-Oliger dissipation                      cues 153-155
    S11    outer boundary: Sommerfeld                     cues 156-166
    S12    code flow chart                                cues 167-178

--------------------------------------------------------------------------
AUDIO SYNC (same idea as the DG project's SyncedScene, with two changes)
--------------------------------------------------------------------------
* Every scene has AUDIO_START / AUDIO_END = the absolute narration time at
  which the clip begins / ends in the final video.  Clips are laid end to
  end, so scene k ends exactly where scene k+1 starts.
* self.sync(t) waits until the clip's own clock equals narration time t.
  self.at(n, *anims) is shorthand for "sync to the START of cue n, then play".
* self.finish() fades the scene out *before* AUDIO_END and then syncs to
  AUDIO_END, so every rendered clip is exactly as long as its slice of audio
  and the concatenation can never drift.
* If an animation overruns its slot, sync() cannot rewind; instead it records
  the beat and tear_down() prints a "late beats" report, e.g.
      [sync] S3: clip 116.97s / audio 116.96s | late beats: cue 48 (+0.35s)
  Fix a late beat by shortening a run_time or moving the cue earlier.

Debug aid: set SNAP=1 and every self.snap("name") call writes
snaps/<scene>_<name>.png so layouts can be checked without watching.

Theme: uses your emacs_theme module when importable (BG, FG, FG_DIM,
code_line, fit_width, starfield, BufferChrome, Packages).  If it is missing,
a small look-alike fallback is defined so the file still renders.
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

    class Packages:
        def __init__(self):
            t = TexTemplate()
            t.add_to_preamble(r"\usepackage{amsmath,amssymb}")
            self.tex_template = t

# --------------------------------------------------------------------------
# Layout constants (clear of the chrome bars)
# --------------------------------------------------------------------------
TOP_Y = config.frame_height / 2 - 0.9
BOT_Y = -config.frame_height / 2 + 0.75
LEFT_X = -config.frame_width / 2 + 0.8
RIGHT_X = config.frame_width / 2 - 0.8
CONTENT_MAX_WIDTH = RIGHT_X - LEFT_X - 0.4

pack = Packages()
MathTex.set_default(tex_template=pack.tex_template)
Tex.set_default(tex_template=pack.tex_template)

# --------------------------------------------------------------------------
# Narration timing
# --------------------------------------------------------------------------
Cue = namedtuple("Cue", "start end text")


def _find_srt():
    cands = [os.environ.get("KERR_SRT")]
    try:
        cands.append(Path(__file__).with_name("kerr_bssn.srt"))
    except NameError:
        pass
    cands += [Path.cwd() / "kerr_bssn.srt",
              Path("~/Music/kerr_bssn.srt").expanduser(),
              Path("/mnt/user-data/uploads/kerr_bssn.srt")]
    for c in cands:
        if c and Path(c).exists():
            return Path(c)
    raise FileNotFoundError("kerr_bssn.srt not found (set KERR_SRT=/path/to/file)")


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
# start of the next scene's first cue (S0 starts at t = 0).
PLAN = [("S0", 1), ("S1", 3), ("S2", 14), ("S2_1", 22), ("S2_2", 27), ("S2_3", 31),
        ("S3", 38), ("S4", 53), ("S4_1", 66), ("S5", 74), ("S6", 84), ("S6_1", 90),
        ("S7", 99), ("S8", 108), ("S9", 118), ("S9_1", 130), ("S9_2", 136),
        ("S10", 144), ("S10_1", 153), ("S11", 156), ("S12", 167)]
START, END = {}, {}
for _i, (_n, _c) in enumerate(PLAN):
    START[_n] = 0.0 if _i == 0 else CUES[_c].start
for _i, (_n, _c) in enumerate(PLAN):
    END[_n] = START[PLAN[_i + 1][0]] if _i + 1 < len(PLAN) else TOTAL_DURATION
ALL_SCENES = [n for n, _ in PLAN]


def chrome_for(position, tab="kerr_bssn.tex"):
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

    def finish(self, fade=0.4, keep=()):
        """Fade everything out except persistent chrome and `keep` (e.g. the
        section header, so the next clip of the same section can start with it
        already on screen)."""
        t_end = self.AUDIO_END
        self.sync(t_end - fade - 0.05)
        objs = [m for m in self.mobjects if m not in self._persist and m not in keep]
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


def make_colormap(stops):
    """stops: list of hex colours -> function f(x in [0,1]) -> uint8 RGB."""
    rgb = [np.array(color_to_rgb(s)) for s in stops]

    def f(x):
        x = float(np.clip(x, 0, 1)) * (len(rgb) - 1)
        i = min(int(x), len(rgb) - 2)
        return (255 * ((1 - (x - i)) * rgb[i] + (x - i) * rgb[i + 1])).astype(np.uint8)
    return f


CMAP = make_colormap(["#1b2a6b", "#2a7fb8", "#2fc4a7", "#f3e04a", "#fff6c8"])


# --------------------------------------------------------------------------
# Kerr geometry helpers (x-z cross-section, M = 1)
# --------------------------------------------------------------------------
def kerr_r_plus(a, M=1.0):
    return M + math.sqrt(max(M * M - a * a, 0.0))


def kerr_section(a, scale=1.0, M=1.0, ring=True):
    """Horizon (r_+), ergosphere and ring singularity in the x-z plane of
    Kerr-Schild coordinates.  A point of Boyer-Lindquist (r, theta) sits at
    rho = sqrt(r^2 + a^2) sin(theta), z = r cos(theta)."""
    rp = kerr_r_plus(a, M)

    def curve(rfun):
        def f(th):
            r = rfun(th)
            return np.array([math.sqrt(r * r + a * a) * math.sin(th) * scale,
                             r * math.cos(th) * scale, 0])
        return f

    hor = ParametricFunction(curve(lambda th: rp), t_range=[0, TAU], color=RED, stroke_width=4)
    ergo = ParametricFunction(
        curve(lambda th: M + math.sqrt(max(M * M - a * a * math.cos(th) ** 2, 0.0))),
        t_range=[0, TAU], color=YELLOW, stroke_width=3)
    hor.set_fill(BLACK, opacity=0.9)
    g = VGroup(ergo, hor)
    if ring:
        g.add(Dot([a * scale, 0, 0], radius=0.06, color=WHITE),
              Dot([-a * scale, 0, 0], radius=0.06, color=WHITE))
    return g


def spheroids(a, rs, scale=1.0, color=BLUE_D, ring=True):
    """Surfaces of constant Kerr-Schild r:  x^2/(r^2+a^2) + z^2/r^2 = 1."""
    g = VGroup()
    for r in rs:
        g.add(Ellipse(width=2 * math.sqrt(r * r + a * a) * scale, height=2 * r * scale,
                      color=color, stroke_width=2))
    if ring:
        g.add(Line([-a * scale, 0, 0], [a * scale, 0, 0], color=WHITE, stroke_width=5),
              Dot([a * scale, 0, 0], radius=0.06, color=WHITE),
              Dot([-a * scale, 0, 0], radius=0.06, color=WHITE))
    return g


def ks_fields(a, x, z, M=1.0):
    """r and H of Kerr-Schild on the y = 0 plane (arrays)."""
    R2 = x * x + z * z
    r2 = 0.5 * ((R2 - a * a) + np.sqrt((R2 - a * a) ** 2 + 4 * a * a * z * z))
    r = np.sqrt(np.maximum(r2, 1e-12))
    H = M * r ** 3 / (r ** 4 + a * a * z * z + 1e-12)
    return r, H


def lapse_image(a, half=4.0, n=200, mask_r=None, height=4.6):
    """Colour map of the Kerr-Schild lapse alpha = 1/sqrt(1+2H) on y = 0."""
    xs = np.linspace(-half, half, n)
    zs = np.linspace(half, -half, n)
    X, Z = np.meshgrid(xs, zs)
    r, H = ks_fields(a, X, Z)
    alpha = 1.0 / np.sqrt(1.0 + 2.0 * H)
    img = np.zeros((n, n, 4), dtype=np.uint8)
    for i in range(n):
        for j in range(n):
            img[i, j, :3] = CMAP((alpha[i, j] - 0.55) / 0.45)
            img[i, j, 3] = 255
    if mask_r is not None:
        img[r < mask_r] = [20, 20, 24, 255]
    im = ImageMobject(img)
    im.set_height(height)
    return im


# ==========================================================================
# S0 -- title card + outline
# ==========================================================================
class S0(SyncedScene):
    AUDIO_START, AUDIO_END = START["S0"], END["S0"]
    STARS = (60, 2)

    def construct(self):
        title = code_line([("* ", FG_DIM),
                           ("Numerical GR, episode 1: a vacuum Kerr black hole", FG)],
                          font_size=28)
        fit_width(title)
        title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        # cue 1: "Welcome to my first video on developing a numerical general
        # relativistic code."
        self.at(1, Write(title), run_time=0.9)

        names = ["Introduction", "Scope", "Kerr spacetime", "3+1 split", "ADM formalism",
                 "BSSN formalism", "Gauge conditions", "Initial data", "Cartoon method",
                 "Excision", "Method of lines + RK4", "Boundary conditions", "Code"]
        mk = lambda s: code_line([("** ", FG_DIM), (s, FG)], font_size=22)
        colL = VGroup(*[mk(s) for s in names[:7]]).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        colR = VGroup(*[mk(s) for s in names[7:]]).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        cols = VGroup(colL, colR).arrange(RIGHT, buff=1.2, aligned_edge=UP)
        cols.next_to(title, DOWN, buff=0.6, aligned_edge=LEFT).shift(RIGHT * 0.4)
        rows = VGroup(*colL, *colR)
        self.sync(CUES[1].start + 2.0)
        self.play(LaggedStart(*[FadeIn(r, shift=RIGHT * 0.15) for r in rows],
                              lag_ratio=0.12), run_time=1.6)

        # cue 2: "Here we are starting with a vacuum Kerr black hole."
        bh = VGroup(Circle(radius=0.55, color=WHITE, stroke_width=2, fill_color=BLACK,
                           fill_opacity=1),
                    Ellipse(width=2.3, height=0.55, color=ORANGE, stroke_width=3),
                    Ellipse(width=3.0, height=0.8, color=YELLOW, stroke_width=1.5))
        bh.to_edge(RIGHT, buff=1.3).shift(DOWN * 0.3)
        self.at(2, Indicate(colL[2], color=TEAL, scale_factor=1.08), run_time=0.8)
        self.snap("outline")
        self.finish()


# ==========================================================================
# S1 -- Scope
# ==========================================================================
def quasi_star(radius=1.9):
    layers = VGroup(*[
        Circle(radius=radius * (1 - k / 7), stroke_width=0,
               fill_color=interpolate_color(ManimColor("#7a1f1f"), ManimColor("#ffd27a"), k / 7),
               fill_opacity=0.40) for k in range(7)])
    ring = Ellipse(width=0.8, height=0.2, color=ORANGE, stroke_width=3)
    bh = Circle(radius=0.17, color=WHITE, stroke_width=2, fill_color=BLACK, fill_opacity=1)
    return VGroup(layers, ring, bh)


class S1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S1"], END["S1"]
    STARS = (50, 5)

    def construct(self):
        header = hdr("Scope", 38, "section")
        # cue 3: "Before getting onto the scope of the video, I will mention my
        # overall scope of the broader project."
        self.at(3, Write(header), run_time=0.6)

        proj = note("The broader project", 24, FG)
        proj.next_to(header, DOWN, aligned_edge=LEFT, buff=0.5)
        self.play(FadeIn(proj), run_time=0.4)

        # cue 4 (16.9-29.9): FORTRAN code with GR, fluids, maybe B-fields / EOS
        c_for = chip("FORTRAN code", ORANGE, 24)
        c_gr = chip("general relativity", BLUE)
        c_fl = chip("relativistic fluids", TEAL)
        c_ex = chip("maybe: magnetic fields, nuclear EOS", GREY_B, 20, dashed=True)
        stack = VGroup(c_for, c_gr, c_fl, c_ex).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        stack.next_to(proj, DOWN, aligned_edge=LEFT, buff=0.5)
        self.at(4, FadeIn(c_for, shift=RIGHT * 0.2), run_time=0.5)
        self.sync(CUES[4].start + 3.2)
        self.play(FadeIn(c_gr, shift=RIGHT * 0.2), run_time=0.5)
        self.sync(CUES[4].start + 6.0)
        self.play(FadeIn(c_fl, shift=RIGHT * 0.2), run_time=0.5)
        self.sync(CUES[4].start + 9.8)
        self.play(FadeIn(c_ex, shift=RIGHT * 0.2), run_time=0.5)

        # cue 5-8: the quasi-star goal
        goal = note("Goal: a toy model of a quasi-star", 24, YELLOW)
        goal.next_to(stack, DOWN, aligned_edge=LEFT, buff=0.55)
        self.at(5, Write(goal), run_time=0.9)
        qs = quasi_star().move_to([3.6, -0.25, 0])
        self.at(6, FadeIn(qs, scale=0.8), run_time=0.9)
        l1 = note("extremely large star", 18).next_to(qs, UP, buff=0.15)
        l2 = Arrow(qs.get_center() + [1.9, -1.2, 0], qs.get_center() + [0.2, -0.1, 0],
                   buff=0.05, color=WHITE, stroke_width=2)
        l2t = note("black hole at the centre", 18).next_to(l2.get_start(), DOWN, buff=0.1)
        self.play(FadeIn(l1), GrowArrow(l2), FadeIn(l2t), run_time=0.7)

        # cue 7: accretion powers the star, just like fusion powers a normal one
        acc = VGroup(*[Arrow(qs.get_center() + 1.6 * np.array([math.cos(t), math.sin(t), 0]),
                             qs.get_center() + 0.45 * np.array([math.cos(t + 0.5), math.sin(t + 0.5), 0]),
                             buff=0, color=ORANGE, stroke_width=3, max_tip_length_to_length_ratio=0.2)
                       for t in np.linspace(0, TAU, 7, endpoint=False)])
        pw = note("accretion powers the star\n(the role fusion plays in a normal star)", 18, ORANGE)
        pw.next_to(qs, DOWN, buff=0.35).shift(LEFT * 0.4)
        self.at(7, FadeOut(l2), FadeOut(l2t), LaggedStart(*[GrowArrow(a) for a in acc], lag_ratio=0.08),
                FadeIn(pw), run_time=1.4)

        # cue 8: combines extreme relativistic and fluid tools
        tag_gr = chip("GR", BLUE, 18).move_to(qs.get_center() + [-2.4, 1.1, 0])
        tag_fl = chip("fluid dynamics", TEAL, 18).move_to(qs.get_center() + [2.2, 1.6, 0])
        self.at(8, FadeIn(tag_gr), FadeIn(tag_fl), run_time=0.6)
        self.snap("quasistar")

        # cue 9-12: do it in steps
        self.at(9, FadeOut(qs), FadeOut(acc), FadeOut(pw), FadeOut(l1), FadeOut(tag_gr),
                FadeOut(tag_fl), FadeOut(stack), FadeOut(goal), FadeOut(proj), run_time=0.5)
        s1 = card("Step 1\nGR only:\nKerr black hole\nin vacuum", BLUE, 20, 3.0)
        s2 = card("Step 2\n+ relativistic\nfluids", TEAL, 20, 3.0)
        s3 = card("Step 3\n+ magnetic fields,\nnuclear EOS", GREY_B, 20, 3.0)
        s4 = card("Goal\nquasi-star\ntoy model", YELLOW, 20, 3.0)
        steps = VGroup(s1, s2, s3, s4).arrange(RIGHT, buff=0.55).shift(DOWN * 0.3)
        fit_width(steps, CONTENT_MAX_WIDTH)
        arrows = VGroup(*[Arrow(steps[i].get_right(), steps[i + 1].get_left(), buff=0.05)
                          for i in range(3)])
        self.play(LaggedStart(FadeIn(s2), FadeIn(s3), FadeIn(s4), lag_ratio=0.3),
                  FadeIn(arrows), run_time=1.2)
        self.at(10, FadeIn(s1, shift=DOWN * 0.2), run_time=0.6)
        self.at(11, s1[0].animate.set_fill(BLUE, opacity=0.25), run_time=0.5)
        here = note("this video", 22, YELLOW)
        here.next_to(s1, DOWN, buff=0.7)
        up = Arrow(here.get_top(), s1.get_bottom(), buff=0.08, color=YELLOW)
        self.at(12, FadeIn(here), GrowArrow(up), run_time=0.6)
        self.snap("steps")

        # cue 13: geometrized units
        self.at(13, FadeOut(steps), FadeOut(arrows), FadeOut(here), FadeOut(up), run_time=0.5)
        units = tex(r"G = c = 1", 72)
        cap = note("geometrized units: every quantity becomes a power of length", 22)
        cap.next_to(units, DOWN, buff=0.5)
        self.play(Write(units), run_time=0.8)
        self.play(FadeIn(cap), run_time=0.5)
        self.snap("units")
        self.finish()


# ==========================================================================
# S2 -- Kerr: what is spinning?  (cues 14-21)
# ==========================================================================
def swirl_rings(radii=(0.95, 1.3, 1.65, 2.0, 2.35, 2.7), center=ORIGIN):
    """Rings of dots (+ one tangential arrow each) that co-rotate with the
    frame-dragging angular velocity omega(r) ~ 2Ma / r^3."""
    rings = []
    for r in radii:
        n = int(11 * r)
        dots = VGroup(*[Dot(center + r * np.array([math.cos(t), math.sin(t), 0]),
                            radius=0.03, color=BLUE_C)
                        for t in np.linspace(0, TAU, n, endpoint=False)])
        arrows = VGroup(*[
            Arrow(center + r * np.array([math.cos(t), math.sin(t), 0]),
                  center + r * np.array([math.cos(t), math.sin(t), 0])
                  + 0.42 * np.array([-math.sin(t), math.cos(t), 0]),
                  buff=0, color=ORANGE, stroke_width=3, max_tip_length_to_length_ratio=0.35)
            for t in np.linspace(0, TAU, 6, endpoint=False)]).set_opacity(0)
        ring = VGroup(dots, arrows)
        ring.omega = 2.4 / r ** 3
        rings.append(ring)
    return rings


class S2(SyncedScene):
    AUDIO_START, AUDIO_END = START["S2"], END["S2"]
    STARS = (50, 7)

    def construct(self):
        header = hdr("Kerr Spacetime")
        # cue 14: "...explain what a Kerr black hole is through its metric."
        self.at(14, Write(header), run_time=0.6)

        # cue 15: "a Kerr black hole is a spinning black hole."
        c = np.array([0.8, -0.25, 0.0])
        bh = VGroup(Circle(radius=0.8, color=WHITE, stroke_width=2, fill_color=BLACK,
                           fill_opacity=1)).move_to(c)
        spin = Arc(radius=1.35, start_angle=0.2, angle=TAU * 0.78, color=ORANGE,
                   stroke_width=5).move_arc_center_to(c)
        spin.add_tip(tip_length=0.25)
        spin.add_updater(lambda m, dt: m.rotate(dt * 1.4, about_point=c))
        J = tex(r"J", 40, ORANGE).move_to(c + [1.9, 1.5, 0])
        self.at(15, FadeIn(bh, scale=0.7), Create(spin), FadeIn(J), run_time=1.0)

        # cues 16-17: "...I need to ask you a question. What's spinning?"
        q = Text("What's spinning?", font_size=44, color=YELLOW).to_edge(UP, buff=1.0).shift(RIGHT * 0.5)
        q.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(17, Write(q), run_time=0.8)

        # cue 18: singularity = a point; infinitely small, so how can it spin?
        dot = Dot(c, radius=0.07, color=WHITE)
        sing = note("the singularity is a single point\n- infinitely small -\nso what is there to rotate?", 22, RED)
        sing.next_to(header, DOWN, buff=1.1, aligned_edge=LEFT)
        self.at(18, FadeIn(sing), spin.animate.set_color(RED), FadeIn(dot), run_time=0.8)
        self.snap("point")

        # cue 19: "the thing that's spinning is spacetime, called frame dragging."
        rings = swirl_rings(center=c)
        for ring in rings:
            ring.add_updater(lambda m, dt: m.rotate(dt * m.omega, about_point=c))
        fd = Text("frame dragging", font_size=34, color=TEAL)
        fd.move_to(c + [3.9, 1.6, 0])
        self.at(19, FadeOut(sing), FadeOut(spin), FadeOut(J), FadeOut(q),
                LaggedStart(*[FadeIn(r) for r in rings], lag_ratio=0.1), FadeIn(fd),
                run_time=1.4)
        self.add(bh, dot)

        # cue 20: Newtonian vocabulary -- a gravitational pull in a swirl
        newt = note("in Newtonian words: gravity has a\nswirling component around the hole", 22, ORANGE)
        newt.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.at(20, *[r[1].animate.set_opacity(1) for r in rings], FadeIn(newt), run_time=1.0)
        self.snap("swirl")

        # cue 21: this changes the shape of the horizon
        self.at(21, *[FadeOut(r) for r in rings], FadeOut(fd), FadeOut(newt), FadeOut(bh),
                FadeOut(dot), run_time=0.6)
        sc = 0.95
        pos = np.array([-3.0, -0.3, 0.0])
        s0 = kerr_section(0.0, sc, ring=False).move_to(pos)
        s9 = kerr_section(0.9, sc).move_to([3.0, -0.3, 0])
        l0 = VGroup(note("a = 0: spherical horizon,\nr = 2M", 20, WHITE)).next_to(s0, DOWN, buff=0.35)
        l9 = note("a = 0.9M: flattened horizon,\nplus an ergosphere", 20, WHITE).next_to(s9, DOWN, buff=0.35)
        self.play(Create(s0), FadeIn(l0), run_time=0.8)
        self.play(Create(s9), FadeIn(l9), run_time=1.2)
        key = VGroup(
            VGroup(Line(ORIGIN, RIGHT * 0.5, color=RED, stroke_width=4), note("event horizon", 18)).arrange(RIGHT, buff=0.2),
            VGroup(Line(ORIGIN, RIGHT * 0.5, color=YELLOW, stroke_width=3), note("ergosphere", 18)).arrange(RIGHT, buff=0.2),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.15).to_corner(UR, buff=0.9).shift(DOWN*0.5)
        self.play(FadeIn(key), run_time=0.5)
        self.snap("horizon")
        self.finish(keep=[header])


# ==========================================================================
# S2_1 -- Boyer-Lindquist metric, horizons, coordinate singularity (22-26)
# ==========================================================================
BL_METRIC = (
    r"\begin{aligned}ds^2 = {}& -\Big(1-\frac{2Mr}{\Sigma}\Big)dt^2"
    r"-\frac{4Mar\sin^2\theta}{\Sigma}\,dt\,d\phi+\frac{\Sigma}{\Delta}dr^2+\Sigma\,d\theta^2\\"
    r"&+\Big(r^2+a^2+\frac{2Ma^2r\sin^2\theta}{\Sigma}\Big)\sin^2\theta\,d\phi^2\end{aligned}"
)


class S2_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S2_1"], END["S2_1"]
    STARS = (50, 7)

    def construct(self):
        header = hdr("Kerr Spacetime")
        self.add(header)

        # cue 22: "...Boyer-Lindquist coordinates ... this relatively long but
        # pretty simple equation, where sigma is ... and delta is ..."
        metric = tex(BL_METRIC, 30)
        clamp_width(metric, CONTENT_MAX_WIDTH)
        metric.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        bl = note("Boyer-Lindquist coordinates (t, r, θ, φ)", 20, TEAL)
        bl.next_to(metric, DOWN, buff=0.3, aligned_edge=LEFT)
        self.at(22, Write(metric), run_time=4.0)
        self.play(FadeIn(bl), run_time=0.4)

        sd = tex(r"\Sigma = r^2 + a^2\cos^2\theta,\qquad \Delta = r^2 - 2Mr + a^2", 32)
        sd.next_to(bl, DOWN, buff=0.4, aligned_edge=LEFT)
        self.sync(CUES[22].start + 12.0)
        self.play(Write(sd[0][:sd[0].submobjects.__len__() // 2]), run_time=1.0)
        self.sync(CUES[22].start + 17.5)
        self.play(Write(sd[0][sd[0].submobjects.__len__() // 2:]), run_time=1.0)
        self.snap("metric")

        # cue 23: horizon at Delta = 0
        self.at(23, FadeOut(metric), FadeOut(bl), run_time=0.5)
        self.play(sd.animate.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT), run_time=0.6)
        sec = kerr_section(0.9, 0.85).move_to([4.2, 0.2, 0])
        hor_eq = tex(r"\Delta = 0 \;\Rightarrow\; r_\pm = M \pm \sqrt{M^2 - a^2}", 32)
        hor_eq.next_to(sd, DOWN, buff=0.6, aligned_edge=LEFT)
        lab_h = note("horizon  r₊", 18, RED).next_to(sec, DOWN, buff=0.15).shift(LEFT * 0.8)
        self.play(Write(hor_eq), Create(sec[1]), FadeIn(lab_h), run_time=1.4)

        # cue 24: ergosphere
        erg_eq = tex(r"r_E = M + \sqrt{M^2 - a^2\cos^2\theta}", 32, YELLOW)
        erg_eq.next_to(hor_eq, DOWN, buff=0.5, aligned_edge=LEFT)
        lab_e = note("ergosphere", 18, YELLOW).next_to(sec, UP, buff=0.1)
        self.at(24, Write(erg_eq), Create(sec[0]), FadeIn(lab_e), run_time=1.4)
        self.play(Indicate(sec[0], color=YELLOW, scale_factor=1.04), run_time=0.8)
        self.snap("horizons")

        # cue 25: coordinate singularity
        grr = tex(r"g_{rr} = \frac{\Sigma}{\Delta} \;\longrightarrow\; \infty \quad (r \to r_+)", 32, RED)
        grr.next_to(erg_eq, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(25, Write(grr), Indicate(sec[1], color=RED, scale_factor=1.06), run_time=1.5)
        cs = note("a coordinate singularity – not a singularity of the spacetime", 22, RED)
        cs.next_to(grr, DOWN, buff=0.35, aligned_edge=LEFT)
        self.play(FadeIn(cs), run_time=0.6)

        # cue 26: troublesome numerically
        bad = chip("a simulation cannot cross r₊ in these coordinates", RED, 22)
        bad.next_to(cs, DOWN, buff=0.35, aligned_edge=LEFT)
        self.at(26, FadeIn(bad, shift=UP * 0.2), run_time=0.6)
        self.snap("coordsing")
        self.finish(keep=[header])


# ==========================================================================
# S2_2 -- Kerr-Schild form (27-30)
# ==========================================================================
class S2_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["S2_2"], END["S2_2"]
    STARS = (50, 7)

    def construct(self):
        header = hdr("Kerr Spacetime")
        self.add(header)

        # cue 27: "instead we are going to put it in a different formalism
        # called the Kerr-Schild form"
        sub = chip("Kerr–Schild form", YELLOW, 24)
        sub.next_to(header, RIGHT, buff=0.7)
        self.at(27, FadeIn(sub, shift=RIGHT * 0.2), run_time=0.6)

        # cue 28: g = eta + 2 H l l
        g = tex(r"g_{\mu\nu} = \eta_{\mu\nu} + 2H\,l_\mu l_\nu", 38)
        g.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        flat = note("flat Minkowski metric", 17, GREY_B).next_to(g[0][5:9], DOWN, buff=0.8)
        self.at(28, Write(g), run_time=1.2)
        l_eq = tex(r"l_\mu = \Big(1,\ \frac{rx+ay}{r^2+a^2},\ \frac{ry-ax}{r^2+a^2},\ \frac{z}{r}\Big)", 30)
        l_eq.next_to(g, DOWN, buff=0.35, aligned_edge=LEFT)
        self.sync(CUES[28].start + 5.2)
        self.play(Write(l_eq), run_time=1.6)

        # cue 29: H = M r^3 / (r^4 + a^2 z^2)
        h_eq = tex(r"H = \frac{M r^3}{r^4 + a^2 z^2}", 36)
        h_eq.next_to(l_eq, DOWN, buff=0.35, aligned_edge=LEFT)
        self.at(29, Write(h_eq), run_time=1.5)

        # cue 30: r implicit
        imp = tex(r"\frac{x^2+y^2}{r^2+a^2} + \frac{z^2}{r^2} = 1", 36, YELLOW)
        imp.next_to(h_eq, DOWN, buff=0.35, aligned_edge=LEFT)
        expl = tex(r"r^2 = \tfrac12\Big[(R^2-a^2)+\sqrt{(R^2-a^2)^2+4a^2z^2}\Big],\ R^2=x^2+y^2+z^2", 24)
        expl.next_to(imp, DOWN, buff=0.3, aligned_edge=LEFT)
        clamp_width(expl, 7.4)
        a_tr = ValueTracker(0.001)
        sph = always_redraw(lambda: spheroids(a_tr.get_value(), [0.8, 1.4, 2.0, 2.6], 0.8)
                            .move_to([4.1, -0.5, 0]))
        cap = note("surfaces of constant r", 18, GREY_B).move_to([4.1, -3.05, 0])
        self.sync(CUES[30].start)
        self.add(sph)
        self.play(Write(imp), FadeIn(cap), run_time=1.5)
        self.sync(CUES[30].start + 5.0)
        self.play(Write(expl), run_time=1.5)
        self.play(a_tr.animate.set_value(0.9), run_time=4.0)
        ring = note("a > 0: oblate spheroids and a ring singularity\n(a = 0 gives ordinary spheres r² = x²+y²+z²)",
                    16, GREY_B).move_to([3.9, 2.0, 0])
        self.play(FadeIn(ring), run_time=0.5)
        self.snap("spheroids")
        self.finish(keep=[header])


# ==========================================================================
# S2_3 -- a = J/M and geometric units (31-37)
# ==========================================================================
class S2_3(SyncedScene):
    AUDIO_START, AUDIO_END = START["S2_3"], END["S2_3"]
    STARS = (50, 7)

    def construct(self):
        header = hdr("Kerr Spacetime")
        self.add(header)

        # cue 31: "define this a that we've been using ... our unit for
        # angular momentum"
        a_big = tex(r"a", 90, YELLOW).move_to([-3.5, 0.9, 0])
        what = note("the spin parameter used in every formula so far", 22).next_to(a_big, DOWN, buff=0.4)
        self.at(31, Write(a_big), FadeIn(what), run_time=1.0)

        # cue 32: a = J / M
        a_def = tex(r"a = \frac{J}{M}", 80)
        a_def[0][0].set_color(YELLOW)
        a_def.move_to([-3.5, 0.9, 0])
        jl = note("J: traditional angular momentum", 20, ORANGE).next_to(a_def, DOWN, buff=0.5)
        ml = note("M: mass", 20, BLUE).next_to(jl, DOWN, buff=0.15, aligned_edge=LEFT)
        self.at(32, ReplacementTransform(a_big, a_def), FadeOut(what), run_time=0.9)
        self.play(FadeIn(jl), FadeIn(ml), run_time=0.6)

        # cue 33: allowed because of geometric units
        gu = chip("G = c = 1", TEAL, 26).move_to([3.6, 2.0, 0])
        gcap = note("we can do this because of geometrized units", 18, TEAL).next_to(gu, DOWN, buff=0.2)
        self.at(33, FadeIn(gu), FadeIn(gcap), run_time=0.6)

        # cue 34: every unit is a length
        dims = VGroup(
            tex(r"[M] = L", 40, BLUE),
            tex(r"[J] = L^2", 40, ORANGE),
            tex(r"[a] = [J]/[M] = L", 40, YELLOW),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.35).move_to([3.6, -0.5, 0])
        self.at(34, FadeIn(dims[0]), run_time=0.5)
        self.sync(CUES[34].start + 3.5)
        self.play(FadeIn(dims[1]), run_time=0.5)
        self.sync(CUES[34].start + 7.0)
        self.play(FadeIn(dims[2]), run_time=0.5)
        self.snap("dims")

        # cue 35-36: three conventions
        self.at(35, FadeOut(a_def), FadeOut(jl), FadeOut(ml), FadeOut(gu), FadeOut(gcap), FadeOut(dims), run_time=0.5)
        c1 = card("J\ntraditional angular\nmomentum", ORANGE, 20, 3.4)
        c2 = card("a = J / M\na length (used here)", YELLOW, 20, 3.4)
        c3 = card("j = J / M²\ndimensionless", TEAL, 20, 3.4)
        row = VGroup(c1, c2, c3).arrange(RIGHT, buff=0.6).move_to([0, -0.2, 0])
        self.play(FadeIn(c2, scale=0.9), run_time=0.5)
        easy = note("a length is much easier to use in the equations", 20).next_to(c2, DOWN, buff=0.4)
        self.play(FadeIn(easy), run_time=0.5)
        self.sync(CUES[35].start + 8.5)
        self.play(FadeIn(c1, shift=RIGHT * 0.3), run_time=0.5)
        self.at(36, FadeIn(c3, shift=LEFT * 0.3), run_time=0.5)
        rel = tex(r"j = \frac{a}{M}", 34, TEAL).next_to(c3, DOWN, buff=0.4)
        self.play(Write(rel), run_time=0.7)

        # cue 37: back to our spacetime
        self.at(37, FadeOut(row), FadeOut(easy), FadeOut(rel), run_time=0.5)
        recap = tex(r"g_{\mu\nu} = \eta_{\mu\nu} + 2H\,l_\mu l_\nu,\qquad H = \frac{Mr^3}{r^4+a^2z^2}", 36)
        recap.move_to([0, 0.2, 0])
        sec = kerr_section(0.7, 0.8).move_to([0, -1.9, 0]) if False else None
        spin_note = note("same spacetime as before – now with a = J/M", 20, GREY_B).next_to(recap, DOWN, buff=0.5)
        self.play(Write(recap), run_time=1.2)
        self.play(FadeIn(spin_note), run_time=0.4)
        self.snap("recap")
        self.finish(keep=[header])


# ==========================================================================
# S3 -- the 3+1 split  (cues 38-52)
# ==========================================================================
def light_cone(center, tilt=0.0, size=0.55, color=YELLOW, flip=False):
    """Two null lines forming a cone in a (r, t) diagram, tilted by `tilt` rad.
    flip=True mirrors the cone vertically (time and space swap roles)."""
    c = np.array(list(center)[:2] + [0.0], dtype=float)
    ang = [PI / 4, 3 * PI / 4]
    lines = VGroup()
    for a in ang:
        d = np.array([math.cos(a), math.sin(a), 0]) * size
        lines.add(Line(c - d, c + d, color=color, stroke_width=3))
    fill = Polygon(c, c + size * np.array([math.cos(ang[0]), math.sin(ang[0]), 0]),
                   c + size * np.array([math.cos(ang[1]), math.sin(ang[1]), 0]),
                   color=color, fill_opacity=0.25, stroke_width=0)
    g = VGroup(lines, fill)
    g.rotate(tilt, about_point=c)
    return g


def slice_curve(y0, amp=0.35, width=6.0, phase=0.0, x0=0.0, color=BLUE_C):
    f = lambda x: np.array([x0 + x, y0 + amp * math.sin(0.9 * x + phase), 0])
    return ParametricFunction(f, t_range=[-width / 2, width / 2], color=color, stroke_width=3)


class S3(SyncedScene):
    AUDIO_START, AUDIO_END = START["S3"], END["S3"]
    STARS = (50, 11)

    def construct(self):
        header = hdr("3+1 Split")
        # cue 38: "...we derive them as initial value problems as a function
        # of time and then numerically integrate them over said time."
        self.at(38, Write(header), run_time=0.6)
        ivp = VGroup(
            chip("u(t₀)", BLUE, 28),
            tex(r"\partial_t u = F(u)", 36),
            chip("u(t)", GREEN, 28),
        ).arrange(RIGHT, buff=1.0).move_to([0, 0.3, 0])
        ar = VGroup(Arrow(ivp[0].get_right(), ivp[1].get_left(), buff=0.1),
                    Arrow(ivp[1].get_right(), ivp[2].get_left(), buff=0.1))
        iv_note = note("initial value problem: data now, rule for how it changes,\nintegrate forward in time", 22)
        iv_note.next_to(ivp, DOWN, buff=0.7)
        self.sync(CUES[38].start + 1.5)
        self.play(FadeIn(ivp[0]), run_time=0.5)
        self.sync(CUES[38].start + 3.8)
        self.play(GrowArrow(ar[0]), Write(ivp[1]), run_time=1.0)
        self.sync(CUES[38].start + 6.8)
        self.play(GrowArrow(ar[1]), FadeIn(ivp[2]), run_time=0.8)
        self.play(FadeIn(iv_note), run_time=0.5)

        # cues 39-41: but in GR space and time are the same kind of thing...
        self.at(39, FadeOut(ivp), FadeOut(ar), FadeOut(iv_note), run_time=0.5)
        trouble = Text("But in GR, time is not special:", font_size=28, color=YELLOW)
        trouble.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.play(FadeIn(trouble), run_time=0.5)
        n40 = note("space and time are two dimensions of the same kind", 24)
        n40.next_to(trouble, DOWN, buff=0.35, aligned_edge=LEFT)
        self.at(40, FadeIn(n40), run_time=0.6)
        # cue 41: inside a black hole space behaves timelike, time spacelike
        base_y = -1.3
        axis = Line([-5.5, base_y - 0.62, 0], [5.5, base_y - 0.62, 0], color=GREY_B, stroke_width=2)
        hor = DashedLine([0.5, base_y - 1.3, 0], [0.5, base_y + 1.7, 0], color=RED)
        hl = note("horizon", 18, RED).next_to(hor, UP, buff=0.1)
        out_cone = light_cone([-3.5, base_y], 0.0)
        near_cone = light_cone([-0.6, base_y], 0.45)
        in_cone = light_cone([3.4, base_y], 1.45)
        lbls = VGroup(note("far away:\ntime is up", 17).next_to(out_cone, DOWN, buff=0.6),
                      note("near the horizon:\ncones tilt", 17).next_to(near_cone, DOWN, buff=0.6),
                      note("inside: \"up\" (future)\npoints along −r", 17, ORANGE).next_to(in_cone, DOWN, buff=0.6))
        self.at(41, Create(axis), Create(hor), FadeIn(hl), run_time=0.6)
        self.play(FadeIn(out_cone), FadeIn(near_cone), FadeIn(in_cone), FadeIn(lbls), run_time=1.0)
        sw = note("space becomes time-like, time becomes space-like", 22, ORANGE)
        sw.move_to(n40)
        self.sync(CUES[41].start + 6.5)
        self.play(FadeOut(n40), FadeIn(sw), run_time=0.5)
        self.snap("cones")

        # cue 42-43: foliate spacetime -- 3+1 formalism
        self.at(42, *[FadeOut(m) for m in [trouble, axis, hor, hl, out_cone, near_cone, in_cone, lbls, sw]],
                run_time=0.5)
        slices = VGroup(*[slice_curve(y, 0.28, 5.6, ph, x0=-1.7) for y, ph in
                          zip([-1.8, -0.9, 0.0, 0.9, 1.8], [0, 0.5, 1.0, 1.5, 2.0])])
        self.play(Create(slices[0]), run_time=0.3)
        self.play(LaggedStart(*[Create(s) for s in slices[1:]], lag_ratio=0.25), run_time=1.5)
        tarr = Arrow([1.6, -2.3, 0], [1.6, 2.5, 0], buff=0, color=WHITE).shift(LEFT * 0.0)
        tl = tex(r"t", 34).next_to(tarr, UP, buff=0.1)
        sig = tex(r"\Sigma_t", 34, BLUE_C).next_to(slices[2], RIGHT, buff=0.15).shift(UP * 0.15)
        self.play(GrowArrow(tarr), FadeIn(tl), FadeIn(sig), run_time=0.8)
        # cue 43: the 3+1 formalism
        f31 = Text("3 + 1  formalism", font_size=34, color=YELLOW).move_to([4.2, 0.9, 0])
        self.at(43, FadeIn(f31, shift=UP * 0.2), run_time=0.6)
        # cue 44: foliating the space into space-like hypersurfaces
        f_note = note("foliate spacetime into a stack of\nspace-like hypersurfaces Σₜ", 22)
        f_note.next_to(f31, DOWN, buff=0.4)
        self.at(44, FadeIn(f_note), run_time=0.6)
        self.snap("foliation")

        # cue 45: define the variables -- zoom to two neighbouring slices
        self.at(45, FadeOut(f31), FadeOut(f_note), FadeOut(tarr), FadeOut(tl), FadeOut(sig),
                *[FadeOut(s) for s in slices], run_time=0.5)
        lo = slice_curve(-1.1, 0.18, 6.2, 0.4, x0=-1.2)
        hi = slice_curve(1.2, 0.18, 6.2, 0.9, x0=-1.2)
        p0 = lo.point_from_proportion(0.38)
        nvec = np.array([0.0, 1.0, 0.0])
        p_n = p0 + nvec * 2.3
        p_beta = p_n + np.array([1.15, 0.0, 0.0])
        lo_lbl = tex(r"\Sigma_t", 30, BLUE_C).next_to(lo, LEFT, buff=0.15)
        hi_lbl = tex(r"\Sigma_{t+dt}", 30, BLUE_C).next_to(hi, LEFT, buff=0.15)
        P = Dot(p0, color=WHITE)
        self.play(Create(lo), Create(hi), FadeIn(lo_lbl), FadeIn(hi_lbl), FadeIn(P), run_time=1.0)

        # list of variables (right side)
        vars_col = VGroup(
            VGroup(tex(r"\gamma_{ij}", 36, GREEN), note("spatial metric: geometry\nof each slice", 18)),
            VGroup(tex(r"\alpha", 36, ORANGE), note("lapse: proper time\nbetween slices", 18)),
            VGroup(tex(r"\beta^i", 36, PURPLE), note("shift: how the spatial\ncoordinates move", 18)),
            VGroup(tex(r"K_{ij}", 36, RED), note("extrinsic curvature:\nhow a slice sits in spacetime", 18)),
        )
        for v in vars_col:
            v.arrange(RIGHT, buff=0.35, aligned_edge=UP)
        vars_col.arrange(DOWN, aligned_edge=LEFT, buff=0.35).to_edge(RIGHT, buff=0.7).shift(DOWN * 0.15)

        # cue 46: gamma
        gam_pts = VGroup(*[Dot(lo.point_from_proportion(p), radius=0.05, color=GREEN) for p in np.linspace(0.12, 0.9, 8)])
        self.at(46, FadeIn(vars_col[0]), FadeIn(gam_pts), run_time=0.7)
        # cue 47: lapse -- normal separation
        nv = Arrow(p0, p_n, buff=0, color=ORANGE, stroke_width=5)
        nv_l = tex(r"\alpha\,dt", 30, ORANGE).next_to(nv, LEFT, buff=0.12)
        nn = tex(r"n^\mu", 30, ORANGE).next_to(nv, RIGHT, buff=0.1).shift(DOWN * 0.5)
        self.at(47, FadeIn(vars_col[1]), GrowArrow(nv), FadeIn(nv_l), FadeIn(nn), run_time=0.9)
        # cue 48: shift
        bv = Arrow(p_n, p_beta, buff=0, color=PURPLE, stroke_width=5)
        bv_l = tex(r"-\beta^i dt", 28, PURPLE).next_to(bv, UP, buff=0.1)
        same = DashedLine(p0, [p_beta[0], p0[1], 0], color=GREY_B)
        tgt = Dot(p_beta, color=PURPLE)
        self.at(48, FadeIn(vars_col[2]), GrowArrow(bv), FadeIn(bv_l), FadeIn(tgt), run_time=0.9)
        # cue 49: extrinsic curvature -- how normals fan out
        fan = VGroup(*[Arrow(lo.point_from_proportion(p),
                             lo.point_from_proportion(p) + 0.8 * np.array([math.sin((p - 0.5) * 1.6), math.cos((p - 0.5) * 1.6), 0]),
                             buff=0, color=RED, stroke_width=3, max_tip_length_to_length_ratio=0.3)
                       for p in np.linspace(0.12, 0.9, 7)])
        self.at(49, FadeIn(vars_col[3]), LaggedStart(*[GrowArrow(a) for a in fan], lag_ratio=0.1), run_time=1.2)
        self.snap("variables")

        # cue 49 (cont.): the metric becomes ...
        self.sync(CUES[49].start + 7.5)
        self.play(*[FadeOut(m) for m in [lo, hi, lo_lbl, hi_lbl, P, gam_pts, nv, nv_l, nn, bv, bv_l, tgt, fan]],
                  vars_col.animate.scale(0.8).to_corner(UR, buff=0.9).shift(DOWN * 0.7), run_time=0.7)
        ds = tex(r"ds^2 = -\alpha^2 dt^2 + \gamma_{ij}\,(dx^i + \beta^i dt)(dx^j + \beta^j dt)", 36)
        ds.move_to([-2.0, 0.3, 0])
        clamp_width(ds, 7.0)
        ds.move_to([-2.2, 0.3, 0])
        self.play(Write(ds), run_time=2.5)

        # cue 50: the four variables
        four = tex(r"\alpha,\ \ \beta^i,\ \ \gamma_{ij},\ \ K_{ij}", 44, YELLOW).next_to(ds, DOWN, buff=0.8)
        self.at(50, FadeIn(four, shift=UP * 0.2), run_time=0.7)
        # cue 51-52: easy part; the hard part is evolving them
        easy = note("describing the spacetime: the easy part", 24, GREEN).next_to(four, DOWN, buff=0.5)
        self.at(51, FadeIn(easy), run_time=0.5)
        hard = tex(r"\partial_t\,\gamma_{ij} = \,?\qquad \partial_t\,K_{ij} = \,?", 40, RED)
        hard.next_to(four, DOWN, buff=0.5)
        hardn = note("how do we evolve them?  –  that is the hard part", 24, RED).next_to(hard, DOWN, buff=0.35)
        self.at(52, FadeOut(easy), FadeIn(hard), FadeIn(hardn), run_time=0.7)
        self.snap("hard")
        self.finish()


# ==========================================================================
# S4 -- ADM formalism: constraints + evolution  (cues 53-65)
# ==========================================================================
class S4(SyncedScene):
    AUDIO_START, AUDIO_END = START["S4"], END["S4"]
    STARS = (50, 13)

    def construct(self):
        header = hdr("ADM Formalism")
        # cue 53: "The Arnowitt-Deser-Misner, or ADM formalism, is the
        # Hamiltonian or canonical formulation of general relativity,
        # attained from the 3+1 split."
        self.at(53, Write(header), run_time=0.6)
        big = Text("Arnowitt – Deser – Misner", font_size=44, color=YELLOW).move_to([0, 0.9, 0])
        self.sync(CUES[53].start + 1.2)
        self.play(FadeIn(big, shift=UP * 0.2), run_time=0.8)
        c1 = chip("Hamiltonian (canonical) formulation of GR", BLUE, 26)
        c2 = chip("obtained from the 3+1 split", TEAL, 26)
        VGroup(c1, c2).arrange(DOWN, buff=0.35).next_to(big, DOWN, buff=0.6)
        self.sync(CUES[53].start + 5.5)
        self.play(FadeIn(c1, shift=UP * 0.15), run_time=0.6)
        self.sync(CUES[53].start + 9.0)
        self.play(FadeIn(c2, shift=UP * 0.15), run_time=0.6)

        # cue 54: constrained initial-value problem, ideally suited to numerics
        self.at(54, FadeOut(c1), FadeOut(c2), big.animate.scale(0.6).next_to(header, DOWN, buff=0.45, aligned_edge=LEFT),
                run_time=0.6)
        left = card("Einstein's equations", BLUE, 24, 4.0)
        right = card("constrained\ninitial-value problem", GREEN, 24, 4.0)
        pair = VGroup(left, right).arrange(RIGHT, buff=1.4).move_to([0, -0.3, 0])
        arr = Arrow(left.get_right(), right.get_left(), buff=0.1)
        self.play(FadeIn(left), run_time=0.5)
        self.play(GrowArrow(arr), FadeIn(right), run_time=0.8)
        suit = note("ideally suited to numerical evolution", 22, GREEN).next_to(pair, DOWN, buff=0.5)
        self.sync(CUES[54].start + 6.0)
        self.play(FadeIn(suit), run_time=0.5)

        # cue 55: induced metric and unit normal
        self.at(55, FadeOut(pair), FadeOut(arr), FadeOut(suit), FadeOut(big), run_time=0.5)
        lo = slice_curve(-0.9, 0.18, 6.0, 0.4, x0=-3.6)
        sig = tex(r"\Sigma_t", 30, BLUE_C).next_to(lo, LEFT, buff=0.15)
        p0 = lo.point_from_proportion(0.5)
        nv = Arrow(p0, p0 + [0, 1.8, 0], buff=0, color=ORANGE, stroke_width=5)
        nl = tex(r"n^\mu", 32, ORANGE).next_to(nv, RIGHT, buff=0.1)
        gm = tex(r"\gamma_{ij}", 34, GREEN).next_to(lo, DOWN, buff=0.25)
        gml = note("induced metric", 18, GREEN).next_to(gm, DOWN, buff=0.1)
        nl2 = note("unit normal, pointing to the future", 18, ORANGE).next_to(nl, RIGHT, buff=0.2).shift(UP * 0.5)
        self.play(Create(lo), FadeIn(sig), run_time=0.6)
        self.play(FadeIn(gm), FadeIn(gml), run_time=0.5)
        self.sync(CUES[55].start + 2.5)
        self.play(GrowArrow(nv), FadeIn(nl), FadeIn(nl2), run_time=0.8)

        # cue 56: extrinsic curvature; Einstein tensor projected two ways
        fan = VGroup(*[Arrow(lo.point_from_proportion(p),
                             lo.point_from_proportion(p) + 0.7 * np.array([math.sin((p - 0.5) * 1.8), math.cos((p - 0.5) * 1.8), 0]),
                             buff=0, color=RED, stroke_width=3, max_tip_length_to_length_ratio=0.3)
                       for p in np.linspace(0.1, 0.9, 7) if abs(p - 0.5) > 0.05])
        kl = tex(r"K_{ij}", 34, RED).next_to(lo, DOWN, buff=0.25).shift(RIGHT * 2.2)
        self.at(56, FadeOut(nl2), LaggedStart(*[GrowArrow(a) for a in fan], lag_ratio=0.1), FadeIn(kl), run_time=1.2)
        proj_g = tex(r"G_{\mu\nu}", 48).move_to([0.9, 0.9, 0])
        proj_c = VGroup(tex(r"G_{\mu\nu}\,n^\mu n^\nu,\ \ G_{\mu\nu}\,n^\mu\gamma^{\nu}{}_{i}", 26, ORANGE),
                        note("orthogonal to the slice \u2192 constraints", 18, ORANGE)).arrange(DOWN, buff=0.1)
        proj_e = VGroup(tex(r"G_{\mu\nu}\,\gamma^{\mu}{}_{i}\gamma^{\nu}{}_{j}", 26, GREEN),
                        note("onto the slice \u2192 evolution equations", 18, GREEN)).arrange(DOWN, buff=0.1)
        projs = VGroup(proj_c, proj_e).arrange(DOWN, buff=0.7).move_to([4.5, 0.9, 0])
        pa = VGroup(Arrow(proj_g.get_right(), proj_c.get_left(), buff=0.15, stroke_width=3, color=ORANGE),
                    Arrow(proj_g.get_right(), proj_e.get_left(), buff=0.15, stroke_width=3, color=GREEN))
        self.sync(CUES[56].start + 4.0)
        self.play(Write(proj_g), run_time=0.7)
        self.play(FadeIn(proj_c), GrowArrow(pa[0]), run_time=0.8)
        self.play(FadeIn(proj_e), GrowArrow(pa[1]), run_time=0.8)
        self.snap("projections")

        # cue 57: Hamiltonian constraint
        self.at(57, *[FadeOut(m) for m in [lo, sig, nv, nl, gm, gml, fan, kl, proj_g, projs, pa]], run_time=0.5)
        lab_c = Text("Constraints", font_size=26, color=ORANGE).next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        lab_e = Text("Evolution", font_size=26, color=GREEN).move_to([1.4, lab_c.get_center()[1], 0]).align_to([0.6, 0, 0], LEFT)
        ham_t = note("Hamiltonian constraint", 20, ORANGE)
        ham = tex(r"R + K^2 - K_{ij}K^{ij} = 16\pi\rho", 30)
        ham_n = note("ρ: energy density measured by\nthe Eulerian observer (nᵘ)", 18)
        hamg = VGroup(ham_t, ham, ham_n).arrange(DOWN, aligned_edge=LEFT, buff=0.18)
        hamg.next_to(lab_c, DOWN, buff=0.35, aligned_edge=LEFT)
        self.play(FadeIn(lab_c), run_time=0.4)
        self.play(FadeIn(ham_t), Write(ham), run_time=1.4)
        self.sync(CUES[57].start + 5.5)
        self.play(FadeIn(ham_n), run_time=0.5)

        # cue 58: momentum constraint
        mom_t = note("momentum constraint", 20, ORANGE)
        mom = tex(r"D_j\big(K^{ij} - \gamma^{ij}K\big) = 8\pi S^i", 30)
        mom_n = note("Sⁱ: momentum density, same\nkind of quantity as ρ", 18)
        momg = VGroup(mom_t, mom, mom_n).arrange(DOWN, aligned_edge=LEFT, buff=0.18)
        momg.next_to(hamg, DOWN, buff=0.45, aligned_edge=LEFT)
        self.at(58, FadeIn(mom_t), Write(mom), run_time=1.4)
        self.sync(CUES[58].start + 5.0)
        self.play(FadeIn(mom_n), run_time=0.5)
        self.snap("constraints")

        # cue 59: evolution of gamma
        ev_x = 0.4
        evg = tex(r"\partial_t\gamma_{ij} = -2\alpha K_{ij} + \mathcal{L}_\beta\gamma_{ij}", 28)
        evg_t = note("advances the geometry slice to slice", 18, GREEN)
        evg_n = note("𝒥_β: Lie derivative along the shift", 18)
        lie = note("L_β : Lie derivative along the shift", 18)
        eg = VGroup(evg_t, evg, lie).arrange(DOWN, aligned_edge=LEFT, buff=0.18)
        lab_e.next_to(header, DOWN, buff=0.5).align_to([ev_x, 0, 0], LEFT)
        eg.next_to(lab_e, DOWN, buff=0.35, aligned_edge=LEFT)
        self.at(59, FadeIn(lab_e), FadeIn(evg_t), Write(evg), run_time=1.6)
        self.sync(CUES[59].start + 6.0)
        self.play(FadeIn(lie), run_time=0.5)

        # cue 60: evolution of K, six components
        evk = tex(r"\begin{aligned}\partial_t K_{ij} = {}& -D_iD_j\alpha + \alpha\big(R_{ij} + K K_{ij} - 2K_{il}K^l{}_j\big)\\"
                  r"&- \mathcal{L}_\beta K_{ij} + 4\pi\alpha\big((S-\rho)\gamma_{ij} - 2S_{ij}\big)\end{aligned}", 22)
        evk.next_to(eg, DOWN, buff=0.45, aligned_edge=LEFT)
        six = note("together: 6 + 6 evolved components", 18, YELLOW).next_to(evk, DOWN, buff=0.25, aligned_edge=LEFT)
        self.at(60, Write(evk), run_time=3.0)
        self.play(FadeIn(six), run_time=0.5)
        self.snap("evolution")

        # cue 61-62: too many components?  -- No.
        eqs_all = VGroup(lab_c, hamg, momg, lab_e, evg_t, evg, lie, evk, six)
        q = Text("too many equations?", font_size=34, color=YELLOW).move_to([3.3, -2.2, 0])
        self.at(61, FadeIn(q), Indicate(hamg, color=YELLOW), Indicate(momg, color=YELLOW), run_time=1.0)
        red = note("the constraints look redundant – they only use γᵢⱼ and Kᵢⱼ,\nthe same variables we evolve", 18, GREY_B)
        red.next_to(q, UP, buff=0.2)
        self.sync(CUES[61].start + 6.0)
        self.play(FadeIn(red), run_time=0.5)
        self.at(62, FadeOut(q), FadeOut(red), run_time=0.4)
        no = Text("that isn't actually the case", font_size=30, color=GREEN).move_to([3.3, -2.2, 0])
        self.play(FadeIn(no), run_time=0.5)

        # cue 63: constraint propagation
        self.at(63, FadeOut(eqs_all), FadeOut(no), run_time=0.6)
        yC = -0.4
        surf = Line([-4.6, yC, 0], [4.6, yC, 0], color=GREEN, stroke_width=6)
        surf_l = tex(r"\text{constraint surface:}\ \ \mathcal{C}=0", 30, GREEN).move_to([0, yC - 0.55, 0])
        traj = Arrow([-4.0, yC, 0], [4.0, yC, 0], buff=0, color=YELLOW, stroke_width=5)
        t0 = Dot([-4.0, yC, 0], color=YELLOW)
        t0l = note("initial data", 18, YELLOW).next_to(t0, UP, buff=0.15)
        stt = note("space of (γᵢⱼ, Kᵢⱼ)", 18, GREY_B).move_to([0, 1.7, 0])
        self.play(Create(surf), FadeIn(surf_l), FadeIn(stt), run_time=0.8)
        self.play(FadeIn(t0), FadeIn(t0l), run_time=0.4)
        self.play(GrowArrow(traj), run_time=1.6)
        stay = note("exact evolution stays on the constraint surface\nif the initial data start on it", 22, YELLOW)
        stay.move_to([0, 1.0, 0])
        self.play(FadeIn(stay), run_time=0.5)

        # cue 64: "always" = the analytic solution
        num = ParametricFunction(lambda x: np.array([x, yC + 0.0075 * (x + 4.0) ** 2.2, 0]),
                                 t_range=[-4.0, 4.0], color=RED, stroke_width=5)
        numl = note("numerical solution", 20, RED).move_to([2.6, 0.85, 0])
        self.at(64, FadeOut(stay), run_time=0.4)
        self.play(Create(num), FadeIn(numl), run_time=3.0)

        # cue 65: numerically the constraints must be enforced separately
        self.at(65, FadeOut(stt), run_time=0.3)
        need = note("\"always\" = analytically.  Numerically the constraints do not\nenforce themselves – even for the simplest schemes.", 22, RED)
        need.move_to([0, 2.0, 0])
        self.play(FadeIn(need), run_time=0.6)
        self.snap("drift")
        self.finish(keep=[header])


# ==========================================================================
# S4_1 -- counting components + ADM's weakness  (cues 66-73)
# ==========================================================================
class S4_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S4_1"], END["S4_1"]
    STARS = (50, 13)

    def construct(self):
        header = hdr("ADM Formalism")
        self.add(header)

        # cue 66: "Now that we have these 12 components"
        sq = VGroup(*[Square(0.62, color=BLUE_C, stroke_width=3) for _ in range(12)]).arrange(RIGHT, buff=0.12)
        sq.move_to([0, 0.3, 0])
        gam = tex(r"\gamma_{ij}:\ 6", 28, GREEN).next_to(sq[:6], UP, buff=0.35)
        kay = tex(r"K_{ij}:\ 6", 28, RED).next_to(sq[6:], UP, buff=0.35)
        tot = tex(r"6 + 6 = 12\ \text{components}", 32).next_to(sq, DOWN, buff=1.4)
        self.at(66, LaggedStart(*[Create(s) for s in sq], lag_ratio=0.08), run_time=1.5)
        self.play(FadeIn(gam), FadeIn(kay), Write(tot), run_time=0.9)

        # cue 67: four constrained
        b1 = Brace(sq[:4], DOWN, color=ORANGE)
        t1 = note("4 fixed by the constraints\n(1 Hamiltonian + 3 momentum)", 18, ORANGE).next_to(b1, DOWN, buff=0.1)
        self.at(67, FadeOut(tot), *[sq[i].animate.set_fill(ORANGE, 0.5).set_color(ORANGE) for i in range(4)],
                GrowFromCenter(b1), FadeIn(t1), run_time=1.0)
        # cue 68: four gauge
        b2 = Brace(sq[4:8], DOWN, color=PURPLE)
        t2 = note("4 gauge freedoms\n(lapse + 3 shift components)", 18, PURPLE).next_to(b2, DOWN, buff=0.1)
        self.at(68, *[sq[i].animate.set_fill(PURPLE, 0.5).set_color(PURPLE) for i in range(4, 8)],
                GrowFromCenter(b2), FadeIn(t2), run_time=1.0)
        # cue 69: four physical
        b3 = Brace(sq[8:], DOWN, color=GREEN)
        t3 = note("4 left: the true\ndynamical content", 18, GREEN).next_to(b3, DOWN, buff=0.1)
        self.at(69, *[sq[i].animate.set_fill(GREEN, 0.5).set_color(GREEN) for i in range(8, 12)],
                GrowFromCenter(b3), FadeIn(t3), run_time=1.0)
        self.snap("count")

        # cue 70: GR is hard even for hyperbolic equations
        self.at(70, *[FadeOut(m) for m in [sq, gam, kay, b1, b2, b3, t1, t2, t3]], run_time=0.5)
        hard = Text("Even in linear form, GR is harder than\na plain hyperbolic (wave) problem", font_size=28, color=YELLOW)
        hard.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.play(FadeIn(hard), run_time=0.6)

        # cue 71: linearised Einstein equation
        lin = tex(r"\gamma_{ij} = \eta_{ij} + h_{ij},\qquad |h_{ij}| \ll 1", 34)
        lin.next_to(hard, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(71, Write(lin), run_time=1.5)
        wave = MathTex(r"\ddot h_{ij}", r"=", r"\Delta h_{ij}", r"\;-\,h_{ik,kj} - h_{jk,ki} + h_{kk,ij}", font_size=40)
        wave[2].set_color(BLUE_C)
        wave[3].set_color(ORANGE)
        wave.next_to(lin, DOWN, buff=0.7, aligned_edge=LEFT)
        wl = note("the ordinary wave equation", 20, BLUE_C).next_to(wave, DOWN, buff=0.45, aligned_edge=LEFT)
        el = note("extra terms from the\nconstraint structure", 20, ORANGE).next_to(wl, RIGHT, buff=0.9)
        self.sync(CUES[71].start + 7.0)
        self.play(Write(wave), run_time=2.0)
        self.sync(CUES[71].start + 12.5)
        self.play(FadeIn(wl), FadeIn(el), run_time=0.6)
        self.snap("linear")

        # cue 72: constraint violations grow without bound
        self.at(72, *[FadeOut(m) for m in [hard, lin, wave, wl, el]], run_time=0.5)
        ax = Axes(x_range=[0, 6, 1], y_range=[0, 5, 1], x_length=7.0, y_length=3.4,
                  axis_config={"include_tip": False, "font_size": 18}).move_to([-0.8, -0.5, 0])
        xl = note("time", 18).next_to(ax, DOWN, buff=0.1).align_to(ax, RIGHT)
        yl = note("constraint violation", 18).next_to(ax, UP, buff=0.1).align_to(ax, LEFT)
        curve = ax.plot(lambda t: 0.08 * math.exp(0.95 * t), x_range=[0, 5.9], color=RED)
        stat = ax.plot(lambda t: 0.08, x_range=[0, 5.9], color=GREEN)
        sl = note("what we want", 18, GREEN).next_to(ax.c2p(5.9, 0.08), UP, buff=0.12).shift(LEFT * 0.4)
        self.play(Create(ax), FadeIn(xl), FadeIn(yl), run_time=0.8)
        self.play(Create(stat), FadeIn(sl), run_time=0.6)
        self.sync(CUES[72].start + 3.0)
        self.play(Create(curve), run_time=6.0)
        gl = note("ADM: a tiny violation grows without limit,\neven in nearly flat spacetime", 20, RED)
        gl.next_to(ax, RIGHT, buff=0.4).shift(UP * 0.6)
        self.play(FadeIn(gl), run_time=0.6)
        self.snap("growth")

        # cue 73: we need something beyond this
        self.at(73, *[FadeOut(m) for m in [ax, xl, yl, curve, stat, sl, gl]], run_time=0.5)
        beyond = Text("we need something beyond ADM", font_size=40, color=YELLOW)
        self.play(FadeIn(beyond, scale=0.9), run_time=0.8)
        self.finish()


# ==========================================================================
# S5 -- BSSN: the linearised argument  (cues 74-83)
# ==========================================================================
def wave_eq(extra, size=38):
    w = MathTex(r"\ddot h_{ij}", r"=", r"\Delta h_{ij}", extra, font_size=size)
    w[2].set_color(BLUE_C)
    w[3].set_color(ORANGE)
    return w


class S5(SyncedScene):
    AUDIO_START, AUDIO_END = START["S5"], END["S5"]
    STARS = (50, 17)

    def construct(self):
        header = hdr("BSSN Formalism")
        # cue 74: "For that, we have the BSSN formalism."
        self.at(74, Write(header), run_time=0.6)
        big = Text("BSSN", font_size=70, color=YELLOW).move_to([0, 0.5, 0])
        names = note("Baumgarte – Shapiro – Shibata – Nakamura", 24).next_to(big, DOWN, buff=0.35)
        self.sync(CUES[74].start + 0.5)
        self.play(FadeIn(big, scale=0.9), FadeIn(names), run_time=0.8)

        # cues 75-76: the wave equation must be recovered, at least linearly
        wave0 = wave_eq(r"-h_{ik,kj}-h_{jk,ki}+h_{kk,ij}")
        wave0.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        goal = note("goal: recover a pure wave equation – at least at the linear level", 22, YELLOW)
        goal.next_to(wave0, DOWN, buff=0.3, aligned_edge=LEFT)
        self.at(76, FadeOut(big), FadeOut(names), run_time=0.4)
        self.play(Write(wave0), run_time=1.5)
        self.play(FadeIn(goal), run_time=0.5)

        # cue 77: two new variables F_i and Phi
        d1 = tex(r"F_i = h_{ij,j}", 38, TEAL)
        d2 = tex(r"\Phi = h_{ii}", 38, PURPLE)
        defs = VGroup(d1, d2).arrange(RIGHT, buff=1.0).next_to(goal, DOWN, buff=0.6, aligned_edge=LEFT)
        self.at(77, Write(d1), run_time=1.2)
        self.sync(CUES[77].start + 4.0)
        self.play(Write(d2), run_time=1.2)

        # cue 78: rewrite the earlier equation
        wave1 = wave_eq(r"-F_{i,j}-F_{j,i}+\Phi_{,ij}")
        wave1.move_to(wave0, aligned_edge=LEFT)
        self.at(78, ReplacementTransform(wave0, wave1), run_time=1.2)
        self.snap("rewrite")

        # cue 79: momentum constraint
        mom_t = note("momentum constraint", 18, ORANGE)
        mom = tex(r"\dot F_i - \dot\Phi_{,i} = 0", 34)
        # cue 80: Hamiltonian constraint
        ham_t = note("Hamiltonian constraint", 18, ORANGE)
        ham = tex(r"\Delta\Phi - F_{i,i} = 0", 34)
        mg = VGroup(mom_t, mom).arrange(DOWN, aligned_edge=LEFT, buff=0.12)
        hg = VGroup(ham_t, ham).arrange(DOWN, aligned_edge=LEFT, buff=0.12)
        cons = VGroup(mg, hg).arrange(RIGHT, buff=1.2).next_to(defs, DOWN, buff=0.6, aligned_edge=LEFT)
        self.at(79, FadeIn(mom_t), Write(mom), run_time=1.6)
        self.at(80, FadeIn(ham_t), Write(ham), run_time=1.6)

        # cue 81: ... so the second derivative is the wave equation again
        final = tex(r"\ddot h_{ij} = \Delta h_{ij}", 52, BLUE_C)
        final.next_to(cons, DOWN, buff=0.6, aligned_edge=LEFT)
        box = SurroundingRectangle(final, color=YELLOW, buff=0.2)
        fn = note("a pure wave equation again – errors propagate away\ninstead of growing", 20, YELLOW)
        fn.next_to(box, RIGHT, buff=0.4)
        self.at(81, Write(final), run_time=1.4)
        self.play(Create(box), FadeIn(fn), run_time=0.8)
        self.snap("linear")

        # cue 82: similar definitions exist for the non-linear case
        self.at(82, *[FadeOut(m) for m in [wave1, goal, defs, cons, final, box, fn]], run_time=0.5)
        rows = [
            (r"\phi = \tfrac1{12}\ln\det\gamma", "conformal factor"),
            (r"\tilde\gamma_{ij} = e^{-4\phi}\gamma_{ij}", "conformal metric  (det = 1)"),
            (r"K = \gamma^{ij}K_{ij}", "trace of the extrinsic curvature"),
            (r"\tilde A_{ij} = e^{-4\phi}\big(K_{ij}-\tfrac13\gamma_{ij}K\big)", "trace-free, conformally rescaled K"),
            (r"\tilde\Gamma^i = -\partial_j\tilde\gamma^{ij}", "conformal connection functions  (the Fᵢ of the non-linear case)"),
        ]
        tbl = VGroup()
        for t, d in rows:
            tbl.add(VGroup(tex(t, 28), note(d, 18, GREY_B)).arrange(RIGHT, buff=0.5, aligned_edge=DOWN))
        tbl.arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        for r in tbl:
            r[1].align_to(tbl[0][0], LEFT).shift(RIGHT * 5.2)
        tbl.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        clamp_width(tbl)
        self.play(LaggedStart(*[FadeIn(r, shift=RIGHT * 0.2) for r in tbl], lag_ratio=0.5), run_time=3.5)
        self.snap("variables")

        # cue 83: the full proof is linked below
        link = chip("full proof: linked in the video description", GREY_B, 22, dashed=True)
        link.next_to(tbl, DOWN, buff=0.6, aligned_edge=LEFT)
        self.at(83, FadeIn(link, shift=UP * 0.2), run_time=0.6)
        self.finish()


# ==========================================================================
# S6 -- gauge freedom  (cues 84-89)
# ==========================================================================
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


class S6(SyncedScene):
    AUDIO_START, AUDIO_END = START["S6"], END["S6"]
    STARS = (50, 19)

    def construct(self):
        header = hdr("Gauge Conditions")
        # cue 84: general covariance => freedom to choose coordinates
        self.at(84, Write(header), run_time=0.6)
        cL, cR = np.array([-3.4, -0.5, 0.0]), np.array([3.4, -0.5, 0.0])
        warp = lambda x, y: (x + 0.35 * math.sin(1.7 * y), y + 0.3 * math.sin(1.5 * x))
        gL = warped_grid(cL)
        gR = warped_grid(cR, warp=warp, color=TEAL)
        evs = [(-0.8, 0.5), (0.9, -0.6), (0.2, 1.0)]
        dL = VGroup(*[Dot([cL[0] + x, cL[1] + y, 0], color=YELLOW, radius=0.07) for x, y in evs])
        dR = VGroup(*[Dot([cR[0] + warp(x, y)[0], cR[1] + warp(x, y)[1], 0], color=YELLOW, radius=0.07) for x, y in evs])
        cap = note("same spacetime, same events – different coordinate labels", 22).move_to([0, 2.2, 0])
        self.sync(CUES[84].start + 1.0)
        self.play(Create(gL), run_time=1.0)
        self.play(FadeIn(dL), run_time=0.4)
        self.sync(CUES[84].start + 3.5)
        self.play(Create(gR), FadeIn(dR), FadeIn(cap), run_time=1.5)
        self.play(gR.animate.set_stroke(opacity=0.9), run_time=0.1)

        # cue 85: gauge freedom
        gf = Text("gauge freedom", font_size=40, color=YELLOW).move_to([0, -3.0 + 0.0, 0]).shift(UP * 0.2)
        self.at(85, FadeIn(gf, shift=UP * 0.2), run_time=0.6)
        self.snap("grids")

        # cue 86: slicing (lapse) and shift
        self.at(86, *[FadeOut(m) for m in [gL, gR, dL, dR, cap, gf]], run_time=0.5)
        slc = VGroup(Text("slicing condition", font_size=26, color=ORANGE),
                     tex(r"\alpha(t,x^i) = \,?", 36, ORANGE),
                     note("how far apart are the slices?", 18)).arrange(DOWN, buff=0.2)
        slc.move_to([-3.4, 1.4, 0])
        sh = VGroup(Text("shift condition", font_size=26, color=PURPLE),
                    tex(r"\beta^i(t,x^j) = \,?", 36, PURPLE),
                    note("how do coordinates move\nfrom slice to slice?", 18)).arrange(DOWN, buff=0.2)
        sh.move_to([3.4, 1.4, 0])
        self.play(FadeIn(slc), run_time=0.6)
        # mini picture: slices with uneven spacing
        ys = [-2.4, -1.9, -1.2, -0.3]
        sl_lines = VGroup(*[Line([-5.6, y, 0], [-1.2, y, 0], color=ORANGE, stroke_width=3) for y in ys])
        self.play(LaggedStart(*[Create(l) for l in sl_lines], lag_ratio=0.2), run_time=1.2)
        self.sync(CUES[86].start + 7.0)
        self.play(FadeIn(sh), run_time=0.6)
        # coordinate lines leaning with the shift
        lean = VGroup(*[Line([1.4 + 0.9 * i, -2.5, 0], [1.4 + 0.9 * i + 0.9 * (1 + 0.0 * i), -0.2, 0],
                             color=PURPLE, stroke_width=3) for i in range(5)])
        lean_n = note("coordinate lines lean", 16, PURPLE).move_to([4.2, -2.85, 0])
        self.play(LaggedStart(*[Create(l) for l in lean], lag_ratio=0.15), FadeIn(lean_n), run_time=1.2)

        # cue 87: Einstein's equations don't say; choose to make things easier
        self.at(87, *[FadeOut(m) for m in [slc, sh, sl_lines, lean, lean_n]], run_time=0.5)
        free = VGroup(Text("Einstein's equations say nothing about the gauge", font_size=30, color=GREY_B),
                      Text("→ so we choose whatever makes the numerics easier", font_size=30, color=GREEN)
                      ).arrange(DOWN, buff=0.5).move_to([0, 0.2, 0])
        self.play(FadeIn(free[0]), run_time=0.5)
        self.sync(CUES[87].start + 2.0)
        self.play(FadeIn(free[1]), run_time=0.5)

        # cue 88: bad slicing makes singularities appear without any black hole
        self.at(88, FadeOut(free), run_time=0.4)
        c = ValueTracker(0.0)
        ysl = [-1.5, -0.75, 0.0, 0.75, 1.5]

        def slices_now():
            g = VGroup()
            for y in ysl:
                xs = np.linspace(-4.5, 4.5, 90)
                pts = [np.array([x, -0.3 + y * (1 - c.get_value() * math.exp(-x * x / 1.2)), 0]) for x in xs]
                m = VMobject(stroke_color=BLUE_C, stroke_width=3)
                m.set_points_as_corners(pts)
                g.add(m)
            return g
        sl_now = always_redraw(slices_now)
        self.add(sl_now)
        bad = note("poorly chosen slicing: the slices focus and pile up\n→ a coordinate singularity – with no black hole at all", 22, RED)
        bad.move_to([0, 2.2, 0])
        self.play(FadeIn(bad), run_time=0.5)
        self.sync(CUES[88].start + 2.5)
        self.play(c.animate.set_value(0.97), run_time=5.0)
        crash = Dot([0, -0.3 + 0.03, 0], color=RED, radius=0.1)
        self.play(FadeIn(crash, scale=2), run_time=0.4)

        # cue 89: a good shift lets us follow the binary without the frame-dragging cost
        self.at(89, FadeOut(sl_now), FadeOut(bad), FadeOut(crash), run_time=0.5)
        self.remove(sl_now)
        pL, pR = np.array([-3.4, -0.95, 0.0]), np.array([3.4, -0.95, 0.0])
        latL, latR = lattice(pL), lattice(pR, color=PURPLE)
        omega = 1.0
        binL = VGroup(Dot(pL + [0.8, 0, 0], radius=0.13, color=WHITE), Dot(pL + [-0.8, 0, 0], radius=0.13, color=WHITE))
        binR = VGroup(Dot(pR + [0.8, 0, 0], radius=0.13, color=WHITE), Dot(pR + [-0.8, 0, 0], radius=0.13, color=WHITE))
        binL.add_updater(lambda m, dt: m.rotate(dt * omega, about_point=pL))
        latR.add_updater(lambda m, dt: m.rotate(-dt * omega, about_point=pR))
        tL = note("fixed coordinates:\nthe binary orbits across the grid", 18).move_to([-3.4, 1.65, 0])
        tR = note("co-rotating shift:\nthe binary sits still, the grid turns", 18, PURPLE).move_to([3.4, 1.65, 0])
        self.play(FadeIn(latL), FadeIn(latR), FadeIn(binL), FadeIn(binR), FadeIn(tL), FadeIn(tR), run_time=0.8)
        self.sync(CUES[89].start + 9.5)
        self.snap("binary")
        self.finish(keep=[header])


# ==========================================================================
# S6_1 -- the moving-puncture gauge  (cues 90-98)
# ==========================================================================
def v_light(r, M=1.0):
    """Outgoing coordinate speed of a light ray, Schwarzschild in ingoing
    Kerr-Schild coordinates:  (1-2M/r)/(1+2M/r)."""
    return (1 - 2 * M / r) / (1 + 2 * M / r)


def v_gauge(r, M=1.0):
    """Outgoing speed of the 1+log gauge mode,  -beta^r + sqrt(2 alpha gamma^rr)
    with alpha = (1+2M/r)^(-1/2),  gamma^rr = 1/(1+2M/r),  beta^r = (2M/r)/(1+2M/r)."""
    f = 1 + 2 * M / r
    return -(2 * M / r) / f + math.sqrt(2.0) * f ** (-0.75)


def bisect_zero(fn, lo, hi, n=60):
    flo = fn(lo)
    for _ in range(n):
        mid = 0.5 * (lo + hi)
        fm = fn(mid)
        if (fm > 0) == (flo > 0):
            lo, flo = mid, fm
        else:
            hi = mid
    return 0.5 * (lo + hi)


R_GAUGE_ZERO = bisect_zero(v_gauge, 0.3, 2.0)


class S6_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S6_1"], END["S6_1"]
    STARS = (50, 19)

    def construct(self):
        header = hdr("Gauge Conditions")
        self.add(header)

        # cue 90: the standard moving-puncture pair
        mp = Text("the standard \"moving puncture\" pair", font_size=32, color=YELLOW)
        mp.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        c_sl = chip("1+log slicing", ORANGE, 28)
        c_sh = chip("Γ-driver shift", PURPLE, 28)
        pair = VGroup(c_sl, c_sh).arrange(RIGHT, buff=1.0).move_to([0, 0.3, 0])
        self.at(90, FadeIn(mp), run_time=0.5)
        self.sync(CUES[90].start + 1.8)
        self.play(FadeIn(c_sl, shift=UP * 0.2), FadeIn(c_sh, shift=UP * 0.2), run_time=0.7)

        # cues 91-92: 1+log
        self.at(91, FadeOut(c_sh), c_sl.animate.next_to(mp, DOWN, buff=0.4, aligned_edge=LEFT), run_time=0.6)
        eq1 = tex(r"\partial_t\alpha = -2\alpha K + \beta^k\partial_k\alpha", 36, ORANGE)
        eq1.next_to(c_sl, RIGHT, buff=0.7)
        self.play(Write(eq1), run_time=1.5)
        ax = Axes(x_range=[0, 4, 1], y_range=[0, 1.1, 0.5], x_length=5.4, y_length=2.6,
                  axis_config={"include_tip": False, "font_size": 16}).move_to([-2.2, -1.3, 0])
        axl = VGroup(note("r", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     note("lapse α", 16).next_to(ax, UP, buff=0.05).align_to(ax, LEFT))
        cols = [BLUE_C, YELLOW, RED]
        curves = [ax.plot(lambda r, w=w: r / math.sqrt(r * r + w * w), x_range=[0, 4], color=c)
                  for w, c in zip((0.12, 0.8, 1.6), cols)]
        self.at(92, Create(ax), FadeIn(axl), run_time=0.6)
        self.play(Create(curves[0]), run_time=0.8)
        self.play(ReplacementTransform(curves[0].copy(), curves[1]), run_time=1.2)
        self.play(ReplacementTransform(curves[1].copy(), curves[2]), run_time=1.2)
        sl_n = note("the lapse collapses where the curvature is strong,\nslowing the slices down before the singularity\n(schematic profiles)", 18, GREY_B)
        sl_n.next_to(ax, RIGHT, buff=0.5)
        self.play(FadeIn(sl_n), run_time=0.5)
        self.snap("onepluslog")

        # cues 93-94: Gamma driver
        self.at(93, *[FadeOut(m) for m in [ax, axl, curves[0], curves[1], curves[2], sl_n, eq1, c_sl]], run_time=0.5)
        c_sh2 = chip("Γ-driver shift", PURPLE, 28).next_to(mp, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(FadeIn(c_sh2), run_time=0.4)
        eq2 = tex(r"\partial_t\beta^i = \tfrac34\,\tilde\Gamma^i - \eta\,\beta^i", 36, PURPLE)
        eq2.next_to(c_sh2, RIGHT, buff=0.7)
        self.at(94, Write(eq2), run_time=1.5)
        pull = lambda x, y: (x * (1 - 0.65 * math.exp(-(x * x + y * y) / 1.1)), y * (1 - 0.65 * math.exp(-(x * x + y * y) / 1.1)))
        gl = warped_grid(np.array([-3.2, -1.95, 0.0]), half=1.25, n=7, warp=pull, color=RED)
        gr = warped_grid(np.array([3.2, -1.95, 0.0]), half=1.25, n=7, color=GREEN)
        tl_ = note("shift not controlled:\ncoordinates get stretched toward the hole", 17, RED).next_to(gl, UP, buff=0.2)
        tr_ = note("Γ-driver: keeps Γ̃ⁱ roughly constant,\nso coordinates stay regular", 17, GREEN).next_to(gr, UP, buff=0.2)
        self.sync(CUES[94].start + 3.0)
        self.play(Create(gl), Create(gr), FadeIn(tl_), FadeIn(tr_), run_time=1.6)
        self.snap("gammadriver")

        # cue 95: damping eta
        self.at(95, *[FadeOut(m) for m in [gl, gr, tl_, tr_]], run_time=0.5)
        ax2 = Axes(x_range=[0, 6, 1], y_range=[0, 4.6, 1], x_length=5.6, y_length=2.7,
                   axis_config={"include_tip": False, "font_size": 16}).move_to([-2.0, -1.4, 0])
        und = ax2.plot(lambda t: 0.75 * t, x_range=[0, 6], color=RED)
        dmp = ax2.plot(lambda t: 0.75 * (1 - math.exp(-t)), x_range=[0, 6], color=GREEN)
        ax2l = VGroup(note("time", 16).next_to(ax2, DOWN, buff=0.05).align_to(ax2, RIGHT),
                      note("|β|", 16).next_to(ax2, UP, buff=0.05).align_to(ax2, LEFT))
        self.play(Create(ax2), FadeIn(ax2l), run_time=0.5)
        self.play(Create(und), run_time=1.2)
        self.play(Create(dmp), run_time=1.2)
        lab_u = note("η = 0: shift grows without bound", 16, RED).next_to(ax2.c2p(3.2, 2.4), UP, buff=0.1).shift(LEFT * 0.6)
        lab_d = note("η ∼ 1/M: shift settles", 16, GREEN).next_to(ax2.c2p(4.2, 0.75), UP, buff=0.15)
        eta_n = note("the damping η stops the shift from growing\nwithout bound and keeps the system\nfrom oscillating", 18).next_to(ax2, RIGHT, buff=0.5)
        self.play(FadeIn(lab_u), FadeIn(lab_d), FadeIn(eta_n), run_time=0.6)

        # cue 96: freeze the gauge for a first test
        self.at(96, *[FadeOut(m) for m in [ax2, ax2l, und, dmp, lab_u, lab_d, eta_n, eq2, c_sh2, mp]], run_time=0.5)
        t1 = Text("first test: freeze the gauge", font_size=30, color=YELLOW).next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        f1 = card("α, βⁱ held at the\nanalytic Kerr–Schild values", ORANGE, 22, 4.2)
        f2 = card("numerics isolated from\nthe gauge dynamics", TEAL, 22, 4.2)
        f3 = card("spacetime should stay static\nto within truncation error", GREEN, 22, 4.2)
        fl = VGroup(f1, f2, f3).arrange(RIGHT, buff=0.7).move_to([0, -0.4, 0])
        fit_width(fl, CONTENT_MAX_WIDTH)
        fa = VGroup(Arrow(f1.get_right(), f2.get_left(), buff=0.05), Arrow(f2.get_right(), f3.get_left(), buff=0.05))
        self.play(FadeIn(t1), run_time=0.4)
        self.sync(CUES[96].start + 1.5)
        self.play(FadeIn(f1), run_time=0.5)
        self.sync(CUES[96].start + 5.5)
        self.play(GrowArrow(fa[0]), FadeIn(f2), run_time=0.6)
        self.sync(CUES[96].start + 9.5)
        self.play(GrowArrow(fa[1]), FadeIn(f3), run_time=0.6)

        # cue 97: caution -- gauge waves can outrun light; characteristics must point into the hole
        self.at(97, *[FadeOut(m) for m in [t1, f1, f2, f3, fa]], run_time=0.5)
        ax3 = Axes(x_range=[0.5, 3.5, 0.5], y_range=[-1.0, 1.5, 0.5], x_length=6.6, y_length=3.3,
                   axis_config={"include_tip": False, "font_size": 16, "include_numbers": False}).move_to([-1.6, -0.9, 0])
        ax3.add(DashedLine(ax3.c2p(0.5, 0), ax3.c2p(3.5, 0), color=GREY_B, stroke_width=2))
        lc = ax3.plot(v_light, x_range=[0.5, 3.5], color=YELLOW)
        gc = ax3.plot(v_gauge, x_range=[0.5, 3.5], color=ORANGE)
        lcl = note("light", 16, YELLOW).next_to(ax3.c2p(3.5, v_light(3.5)), UP, buff=0.1).shift(LEFT * 0.2)
        gcl = note("1+log gauge mode", 16, ORANGE).next_to(ax3.c2p(3.5, v_gauge(3.5)), UP, buff=0.12).shift(LEFT * 0.8)
        axl3 = VGroup(note("r / M", 16).next_to(ax3, DOWN, buff=0.05).align_to(ax3, RIGHT),
                      note("outgoing coordinate speed  dr/dt   (a = 0)", 16).next_to(ax3, UP, buff=0.05).align_to(ax3, LEFT))
        self.play(Create(ax3), FadeIn(axl3), run_time=0.6)
        self.play(Create(lc), FadeIn(lcl), run_time=1.0)
        self.play(Create(gc), FadeIn(gcl), run_time=1.0)
        xl = ax3.c2p(2.0, 0)
        xg = ax3.c2p(R_GAUGE_ZERO, 0)
        dl = Dot(xl, color=YELLOW)
        dg = Dot(xg, color=ORANGE)
        tl2 = note("r₊ = 2M\n(light turns inward)", 16, YELLOW).next_to(dl, DOWN, buff=0.3).shift(RIGHT * 0.5)
        tg2 = note(f"r ≈ {R_GAUGE_ZERO:.2f}M\n(gauge mode turns inward)", 16, ORANGE).next_to(dg, DOWN, buff=0.95).shift(RIGHT * 1.3)
        self.sync(CUES[97].start + 6.5)
        self.play(FadeIn(dl), FadeIn(tl2), FadeIn(dg), FadeIn(tg2), run_time=0.8)
        safe = Rectangle(width=abs(xl[0] - ax3.c2p(0.5, 0)[0]) if False else abs(xg[0] - ax3.c2p(0.5, 0)[0]),
                         height=3.3, color=GREEN, fill_opacity=0.15, stroke_width=0)
        safe.move_to([(xg[0] + ax3.c2p(0.5, 0)[0]) / 2, ax3.get_center()[1], 0])
        sn = note("the excision surface must lie where\nevery characteristic points inward,\nincluding the gauge modes", 18, GREEN)
        sn.next_to(ax3, RIGHT, buff=0.4)
        self.sync(CUES[97].start + 10.5)
        self.play(FadeIn(safe), FadeIn(sn), run_time=0.8)
        self.snap("speeds")

        # cue 98: check numerically
        chk = chip("✓ check this numerically – don't just assume it", GREEN, 24)
        chk.to_edge(DOWN, buff=0.9)
        self.at(98, FadeIn(chk, shift=UP * 0.2), run_time=0.6)
        self.finish(keep=[header])


# ==========================================================================
# S7 -- initial data: Kerr-Schild -> ADM -> BSSN  (cues 99-107)
# ==========================================================================
class S7(SyncedScene):
    AUDIO_START, AUDIO_END = START["S7"], END["S7"]
    STARS = (50, 23)

    def construct(self):
        header = hdr("Initial Data")
        # cue 99: BSSN keeps the constraints only if the initial data satisfy them
        self.at(99, Write(header), run_time=0.6)
        yC = 0.0
        surf = Line([-3.5, yC, 0], [3.5, yC, 0], color=GREEN, stroke_width=6)
        d0 = Dot([-3.0, yC, 0], color=YELLOW, radius=0.1)
        sl = tex(r"\text{constraint surface: } \mathcal{H}=0,\ \mathcal{M}^i=0", 28, GREEN).next_to(surf, DOWN, buff=0.3)
        tr = Arrow([-3.0, yC, 0], [3.0, yC, 0], buff=0, color=YELLOW, stroke_width=5)
        need = note("BSSN keeps the constraints satisfied – provided the initial data\nalready satisfy the constraints and Einstein's equations", 22, YELLOW)
        need.move_to([0, 1.5, 0])
        self.sync(CUES[99].start + 1.5)
        self.play(Create(surf), FadeIn(sl), run_time=0.8)
        self.play(FadeIn(d0), GrowArrow(tr), run_time=1.5)
        self.play(FadeIn(need), run_time=0.6)

        # cue 100: normally hard (elliptic constraint solve) -- but Kerr is analytic
        self.at(100, *[FadeOut(m) for m in [surf, d0, sl, tr, need]], run_time=0.5)
        hardc = card("generic spacetime:\nsolve the constraint equations\nfor valid initial data\n(a hard problem)", RED, 22, 4.6)
        easyc = card("Kerr: one of the few exact\nanalytic solutions of GR\n→ just use it", GREEN, 22, 4.6)
        pair = VGroup(hardc, easyc).arrange(RIGHT, buff=0.9).move_to([0, 0.2, 0])
        self.play(FadeIn(hardc), run_time=0.6)
        self.sync(CUES[100].start + 7.0)
        self.play(FadeIn(easyc), run_time=0.6)
        self.sync(CUES[100].start + 12.0)
        self.play(*[FadeOut(m) for m in [hardc, easyc]], run_time=0.4)
        img = lapse_image(0.7, half=3.5, n=150, mask_r=kerr_r_plus(0.7) - 0.0, height=3.4)
        img.move_to([4.3, -0.6, 0])
        img_l = note("Kerr–Schild lapse α\n(x–z plane, a = 0.7M)", 16, GREY_B).next_to(img, DOWN, buff=0.15)
        self.play(FadeIn(img), FadeIn(img_l), run_time=0.6)

        # cue 101: read the ADM variables off analytically
        adm = tex(r"""\begin{aligned}
            \alpha &= \frac{1}{\sqrt{1+2H}}, \qquad \beta^i = \frac{2H}{1+2H}\,l^i\\[4pt]
            \gamma_{ij} &= \delta_{ij} + 2H\,l_il_j,\qquad \gamma^{ij} = \delta^{ij} - \frac{2H}{1+2H}\,l^il^j\\[4pt]
            \det\gamma &= 1+2H,\qquad l^i = \delta^{ij}l_j
            \end{aligned}""", 30)
        clamp_width(adm, 8.2)
        adm.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        k_note = note("Kerr–Schild coordinates are already in 3+1 form:\nread every ADM variable off analytically", 20, TEAL)
        k_note.next_to(adm, DOWN, buff=0.4, aligned_edge=LEFT)
        self.at(101, Write(adm), run_time=5.0)
        self.play(FadeIn(k_note), run_time=0.5)
        self.snap("adm")

        # cues 102-104: stationary => K_ij from the gamma evolution equation
        self.at(102, FadeOut(adm), FadeOut(k_note), run_time=0.4)
        st = tex(r"\partial_t\gamma_{ij} = 0 \qquad\text{(stationary solution)}", 34)
        st.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        self.play(Write(st), run_time=1.4)
        evo = tex(r"0 = -2\alpha K_{ij} + D_i\beta_j + D_j\beta_i", 34)
        evo.next_to(st, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(103, Write(evo), run_time=1.6)
        kij = tex(r"K_{ij} = \frac{1}{2\alpha}\big(D_i\beta_j + D_j\beta_i\big)", 40, YELLOW)
        kij.next_to(evo, DOWN, buff=0.6, aligned_edge=LEFT)
        self.at(104, Write(kij), run_time=1.6)
        self.snap("kij")

        # cue 105: conformal decomposition feeding the BSSN variables
        self.at(105, *[FadeOut(m) for m in [st, evo]], kij.animate.scale(0.75).next_to(header, DOWN, buff=0.45, aligned_edge=LEFT), run_time=0.5)
        dec = tex(r"""\begin{aligned}
            \phi &= \tfrac{1}{12}\ln\det\gamma, & \tilde\gamma_{ij} &= e^{-4\phi}\gamma_{ij}\\[4pt]
            K &= \gamma^{ij}K_{ij}, & \tilde A_{ij} &= e^{-4\phi}\big(K_{ij}-\tfrac13\gamma_{ij}K\big)\\[4pt]
            \tilde\Gamma^i &= -\partial_j\tilde\gamma^{ij}
            \end{aligned}""", 26)
        clamp_width(dec, 8.4)
        dec.next_to(kij, DOWN, buff=0.35, aligned_edge=LEFT)
        dn = note("conformal decomposition → the BSSN variables", 20, TEAL).next_to(dec, DOWN, buff=0.35, aligned_edge=LEFT)
        self.play(Write(dec), run_time=3.0)
        self.play(FadeIn(dn), run_time=0.4)

        # cue 106: exact vacuum solution => constraints hold analytically
        self.at(106, FadeOut(img), FadeOut(img_l), FadeOut(dn), run_time=0.4)
        cons = tex(r"\mathcal{H} = 0,\qquad \mathcal{M}^i = 0\qquad\text{(analytically)}", 34, GREEN)
        cons.next_to(dec, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(Write(cons), run_time=1.4)
        # cue 107: on the grid only to truncation error: baseline for validation
        grid_c = tex(r"|\mathcal{H}|,\ |\mathcal{M}^i| \;\sim\; \mathcal{O}(\Delta x^2)\qquad\text{(on the grid)}", 34, ORANGE)
        grid_c.next_to(cons, DOWN, buff=0.3, aligned_edge=LEFT)
        base = chip("this residual is the baseline for the validation section", YELLOW, 22)
        base.next_to(grid_c, DOWN, buff=0.3, aligned_edge=LEFT)
        self.at(107, Write(grid_c), run_time=1.4)
        self.play(FadeIn(base, shift=UP * 0.2), run_time=0.6)
        self.snap("baseline")
        self.finish()


# ==========================================================================
# S8 -- the cartoon method  (cues 108-117)
# ==========================================================================
class S8(SyncedScene):
    AUDIO_START, AUDIO_END = START["S8"], END["S8"]
    STARS = (50, 29)

    def construct(self):
        header = hdr("Cartoon Method")
        # cue 108: run an axisymmetric problem on a 2D Cartesian grid
        self.at(108, Write(header), run_time=0.6)
        cL, cR = np.array([-3.6, -0.5, 0.0]), np.array([3.0, -0.5, 0.0])
        body = Ellipse(width=1.9, height=1.4, color=ORANGE, fill_color=ORANGE, fill_opacity=0.3).move_to(cL)
        zax = Arrow(cL + [0, -1.5, 0], cL + [0, 1.7, 0], buff=0, color=WHITE, stroke_width=3)
        zl = tex(r"z", 28).next_to(zax, UP, buff=0.05)
        spin = Arc(radius=1.2, start_angle=PI * 1.05, angle=PI * 0.9, color=TEAL, stroke_width=3).move_arc_center_to(cL + [0, 0.65, 0])
        spin.add_tip(tip_length=0.2)
        lab3 = note("axisymmetric 3D problem", 18).next_to(zax, DOWN, buff=0.15)
        self.sync(CUES[108].start + 1.0)
        self.play(FadeIn(body), GrowArrow(zax), FadeIn(zl), Create(spin), FadeIn(lab3), run_time=1.2)
        grid2 = warped_grid(cR, half=1.5, n=7, color=BLUE_D)
        x_l = tex(r"x", 28).next_to(cR + [1.5, 0, 0], RIGHT, buff=0.05)
        z_l = tex(r"z", 28).next_to(cR + [0, 1.5, 0], UP, buff=0.05)
        lab2 = note("one 2D Cartesian (x, z) grid", 18).next_to(grid2, DOWN, buff=0.2)
        arr = Arrow(cL + [1.5, 0, 0], cR + [-1.8, 0, 0], buff=0.1, color=YELLOW)
        self.sync(CUES[108].start + 4.0)
        self.play(GrowArrow(arr), Create(grid2), FadeIn(x_l), FadeIn(z_l), FadeIn(lab2), run_time=1.5)

        # cue 109: traditionally cylindrical / spherical coordinates
        self.at(109, *[FadeOut(m) for m in [body, zax, zl, spin, lab3, grid2, x_l, z_l, lab2, arr]], run_time=0.5)
        cyl = card("cylindrical\n(ρ, φ, z)", BLUE, 24, 3.4)
        sph = card("spherical\n(r, θ, φ)", BLUE, 24, 3.4)
        trad = VGroup(cyl, sph).arrange(RIGHT, buff=1.0).move_to([0, 0.9, 0])
        tl_ = note("the traditional way to remove a dimension", 20).next_to(trad, UP, buff=0.3)
        self.play(FadeIn(tl_), FadeIn(cyl), FadeIn(sph), run_time=0.7)

        # cues 110-111: but it costs extra work and creates coordinate singularities
        cost1 = note("• every equation must be rewritten in the new coordinates", 22, RED)
        cost2 = note("• more coordinate singularities than Cartesian\n   (on the axis ρ = 0, at the origin, at the poles)", 22, RED)
        costs = VGroup(cost1, cost2).arrange(DOWN, aligned_edge=LEFT, buff=0.35).next_to(trad, DOWN, buff=0.6)
        self.at(111, FadeIn(cost1), run_time=0.5)
        self.sync(CUES[111].start + 7.0)
        self.play(FadeIn(cost2), run_time=0.5)

        # cue 112: the trick -- rotate all vectors and tensors about the z-axis
        self.at(112, *[FadeOut(m) for m in [trad, tl_, costs]], run_time=0.5)
        o = np.array([-3.4, -0.7, 0.0])
        ax_x = Arrow(o + [-0.3, 0, 0], o + [3.3, 0, 0], buff=0, color=GREY_B, stroke_width=2)
        ax_y = Arrow(o + [0, -0.3, 0], o + [0, 2.9, 0], buff=0, color=GREY_B, stroke_width=2)
        xl_ = tex(r"x", 26).next_to(ax_x, RIGHT, buff=0.05)
        yl_ = tex(r"y", 26).next_to(ax_y, UP, buff=0.05)
        top = note("looking down the z-axis", 16, GREY_B).move_to(o + [1.6, -1.45, 0])
        trick = note("rotate every vector and tensor around the z-axis\ninto the x–z plane", 22, TEAL).move_to([3.0, 1.9, 0])
        self.play(Create(ax_x), Create(ax_y), FadeIn(xl_), FadeIn(yl_), FadeIn(top), FadeIn(trick), run_time=0.8)

        # cue 113: (x,y,z) -> (rho,0,z)
        x0, y0 = 1.9, 1.3
        rho = math.hypot(x0, y0)
        P = Dot(o + [x0, y0, 0], color=YELLOW, radius=0.09)
        vec = Arrow(o + [x0, y0, 0], o + [x0 + 0.45, y0 - 0.35, 0], buff=0, color=ORANGE, stroke_width=4, max_tip_length_to_length_ratio=0.35)
        Pg = VGroup(P, vec)
        Pl = tex(r"(x,y)", 24, YELLOW).next_to(P, UP, buff=0.1)
        Q = Dot(o + [rho, 0, 0], color=GREEN, radius=0.09)
        rl = tex(r"\rho", 28, GREEN).next_to(Q, DOWN, buff=0.12)
        arc = Arc(radius=rho, start_angle=math.atan2(y0, x0), angle=-math.atan2(y0, x0), color=TEAL, stroke_width=3).move_arc_center_to(o)
        arc.add_tip(tip_length=0.18)
        self.at(113, FadeIn(Pg), FadeIn(Pl), run_time=0.6)
        self.sync(CUES[113].start + 2.0)
        self.play(Create(arc), Rotate(Pg, angle=-math.atan2(y0, x0), about_point=o), run_time=1.6)
        self.play(FadeIn(Q), FadeIn(rl), FadeOut(Pl), run_time=0.4)
        mapeq = VGroup(tex(r"(x,y,z)\ \longrightarrow\ (\rho,0,z)", 34),
                       tex(r"\rho = \sqrt{x^2+y^2}", 34, GREEN)).arrange(DOWN, aligned_edge=LEFT, buff=0.3).move_to([3.3, 0.2, 0])
        clamp_width(mapeq, 5.2)
        self.sync(CUES[113].start + 5.0)
        self.play(Write(mapeq), run_time=1.5)
        self.snap("rotate")

        # cue 114: the y-derivatives take their traditional form ...
        self.at(114, *[FadeOut(m) for m in [Pg, Q, rl, arc, trick, mapeq]], run_time=0.5)
        dx_ = 0.8
        Pp = Dot(o + [1.3, dx_, 0], color=YELLOW, radius=0.08)
        Pm = Dot(o + [1.3, -dx_, 0], color=YELLOW, radius=0.08)
        Pc = Dot(o + [1.3, 0, 0], color=GREY_B, radius=0.05)
        bp = Arrow(Pp.get_center(), Pp.get_center() + [0.4, 0.15, 0], buff=0, color=ORANGE, stroke_width=3, max_tip_length_to_length_ratio=0.4)
        bm = Arrow(Pm.get_center(), Pm.get_center() + [0.4, -0.15, 0], buff=0, color=ORANGE, stroke_width=3, max_tip_length_to_length_ratio=0.4)
        lp = tex(r"(x,+\Delta y)", 22, YELLOW).next_to(Pp, LEFT, buff=0.12)
        lm = tex(r"(x,-\Delta y)", 22, YELLOW).next_to(Pm, LEFT, buff=0.12)
        self.play(FadeIn(Pp), FadeIn(Pm), FadeIn(Pc), FadeIn(bp), FadeIn(bm), FadeIn(lp), FadeIn(lm), run_time=0.7)
        dby = tex(r"\partial_y\beta^x = \frac{\beta^x(x,+\Delta y,z)-\beta^x(x,-\Delta y,z)}{2\Delta y}", 32)
        clamp_width(dby, 6.6)
        dby.move_to([3.0, 1.2, 0])
        self.play(Write(dby), run_time=2.5)

        # cues 115-116: take the matching point on the computational plane
        rp = math.hypot(1.3, dx_)
        self.at(115, run_time=0.01)
        Qp = Dot(o + [rp, 0, 0], color=GREEN, radius=0.09)
        arcp = Arc(radius=rp, start_angle=math.atan2(dx_, 1.3), angle=-math.atan2(dx_, 1.3), color=TEAL, stroke_width=3).move_arc_center_to(o)
        arcm = Arc(radius=rp, start_angle=-math.atan2(dx_, 1.3), angle=math.atan2(dx_, 1.3), color=TEAL, stroke_width=3).move_arc_center_to(o)
        comp = tex(r"(\rho,0,z)", 24, GREEN).next_to(Qp, DOWN, buff=0.2).shift(RIGHT * 0.3)
        self.play(Create(arcp), Create(arcm), FadeIn(Qp), FadeIn(comp), run_time=1.4)
        rho2 = tex(r"\rho = \sqrt{x^2+\Delta y^2}", 34, GREEN).move_to([3.0, -0.4, 0])
        self.at(116, Write(rho2), run_time=1.4)
        self.snap("fd")

        # cue 117: go backwards to get the y-derivative
        back = note("evaluate the stored values at ρ, rotate the components back,\nthen difference – this gives the ∂_y derivative", 20, YELLOW)
        back.move_to([3.0, -1.7, 0])
        self.at(117, FadeIn(back), run_time=0.6)
        self.finish()


# ==========================================================================
# S9 -- why excise?  apparent vs event horizon  (cues 118-129)
# ==========================================================================
class S9(SyncedScene):
    AUDIO_START, AUDIO_END = START["S9"], END["S9"]
    STARS = (60, 31)

    def construct(self):
        header = hdr("Excision")
        # cue 118: Kerr-Schild already keeps us away from coordinate singularities
        self.at(118, Write(header), run_time=0.6)
        ok = note("✓  Kerr–Schild coordinates are regular across the horizon:\n    no coordinate singularity there", 22, GREEN)
        ok.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.sync(CUES[118].start + 1.0)
        self.play(FadeIn(ok), run_time=0.6)
        # cue 119: but it doesn't do the whole thing
        no = note("✗  but that is not the whole story", 24, RED)
        no.next_to(ok, DOWN, buff=0.4, aligned_edge=LEFT)
        self.at(119, FadeIn(no), run_time=0.5)

        # cue 120: physical problems remain inside the hole -- curvature diverges
        ax = Axes(x_range=[0.5, 3.0, 0.5], y_range=[-1.0, 3.6, 1.0], x_length=6.2, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 16}).move_to([2.6, -1.0, 0])
        kre = ax.plot(lambda r: math.log10(48.0) - 6.0 * math.log10(r), x_range=[0.5, 3.0], color=RED)
        hor = DashedLine(ax.c2p(2.0, -1.0), ax.c2p(2.0, 3.6), color=YELLOW, stroke_width=2)
        hl = note("r = 2M", 16, YELLOW).next_to(hor, UP, buff=0.05)
        axl = VGroup(note("r / M", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     note("log₁₀ of the curvature invariant 48M²/r⁶  (a = 0)", 16).next_to(ax, UP, buff=0.35).align_to(ax, LEFT))
        phys = note("the physical singularity is still there:\ncurvature → ∞ as r → 0", 20, RED).move_to([-3.4, -1.2, 0])
        self.at(120, Create(ax), FadeIn(axl), FadeIn(phys), run_time=0.8)
        self.play(Create(kre), Create(hor), FadeIn(hl), run_time=2.5)
        self.snap("curvature")

        # cue 121: possible remedies
        self.at(121, *[FadeOut(m) for m in [ok, no, ax, axl, kre, hor, hl, phys]], run_time=0.5)
        names = ["puncture", "horizon penetration", "hyperbolic penetrating", "characteristic evolution",
                 "black-hole membrane", "excision"]
        chips = VGroup(*[chip(n, GREY_B, 24) for n in names]).arrange_in_grid(rows=3, cols=2, buff=(0.7, 0.45))
        chips.move_to([0, -0.2, 0])
        sub = note("several ways to deal with the inside of the hole", 22).next_to(chips, UP, buff=0.5)
        self.play(FadeIn(sub), run_time=0.4)
        for i, c in enumerate(chips):
            self.sync(CUES[121].start + 1.0 + 2.1 * i)
            self.play(FadeIn(c, shift=RIGHT * 0.2), run_time=0.4)

        # cue 122: excision -- cut the black hole out of the spacetime
        self.at(122, *[c.animate.set_opacity(0.3) for c in chips[:5]], FadeOut(sub), run_time=0.5)
        ex = chips[5]
        self.play(ex[0].animate.set_color(YELLOW), run_time=0.3)
        self.play(ex.animate.move_to([-3.6, 1.5, 0]), *[FadeOut(c) for c in chips[:5]], run_time=0.8)
        cen = np.array([3.0, -0.4, 0.0])
        lat = lattice(cen, radius=2.1, step=0.3)
        circ = DashedVMobject(Circle(radius=0.95, color=RED).move_to(cen), num_dashes=30)
        self.play(FadeIn(lat), run_time=0.5)
        inner = VGroup(*[d for d in lat if np.linalg.norm(d.get_center() - cen) < 0.95])
        self.play(Create(circ), inner.animate.set_color(GREY_D).set_opacity(0.4), run_time=1.2)
        cutn = note("cut the black hole out of the spacetime:\nstop evolving the points inside", 20, YELLOW).move_to([-3.1, -0.2, 0])
        self.play(FadeIn(cutn), run_time=0.5)
        self.snap("cut")

        # cues 123-125: apparent vs event horizon
        self.at(123, *[FadeOut(m) for m in [lat, circ, cutn, ex]], run_time=0.5)
        t123 = note("we excise the apparent horizon – note: apparent, not event", 24, YELLOW)
        t123.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(FadeIn(t123), run_time=0.5)
        app = card("apparent horizon\n\nthe horizon as seen on one slice,\nwith our gauge conditions\n(outgoing null expansion Θ = 0)", ORANGE, 20, 5.0)
        evt = card("event horizon\n\nthe horizon for all possible\nobservers: the boundary of what\ncan never signal to infinity", BLUE, 20, 5.0)
        app.move_to([-3.2, -0.9, 0])
        evt.move_to([3.2, -0.9, 0])
        self.at(124, FadeIn(app, shift=UP * 0.2), run_time=0.6)
        self.at(125, FadeIn(evt, shift=UP * 0.2), run_time=0.6)

        # cues 126-128: do we need a horizon finder?
        self.at(126, *[FadeOut(m) for m in [t123, app, evt]], run_time=0.5)
        need = Text("to excise it, we need to know where it is", font_size=30, color=YELLOW)
        need.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.play(FadeIn(need), run_time=0.5)
        finder = card("horizon finder\n(solve Θ = 0 on the slice)", GREY_B, 22, 4.4).move_to([-3.2, -0.6, 0])
        analytic = card("here: the solution is analytic\nr = r₊ is known exactly", GREEN, 22, 4.4).move_to([3.2, -0.6, 0])
        self.at(127, FadeIn(finder), run_time=0.5)
        cross = VGroup(Line(finder.get_corner(UL), finder.get_corner(DR), color=RED, stroke_width=5),
                       Line(finder.get_corner(DL), finder.get_corner(UR), color=RED, stroke_width=5))
        self.sync(CUES[127].start + 2.0)
        self.play(Create(cross), FadeIn(analytic), run_time=0.8)
        fut = note("in future problems it will be needed –\nso I compute it anyway", 22, ORANGE).move_to([0, -2.6, 0])
        self.at(128, FadeIn(fut), run_time=0.6)

        # cue 129: it's also nicer to look at
        self.at(129, *[FadeOut(m) for m in [finder, cross, analytic, fut, need]], run_time=0.5)
        sec = kerr_section(0.7, 1.1).move_to([3.4, -0.6, 0])
        nice = note("and a horizon surface is much nicer to look at\nand animate than abstract curvature components", 22).move_to([-2.6, -0.3, 0])
        self.play(FadeIn(nice), Create(sec), run_time=1.5)
        self.snap("nice")
        self.finish(keep=[header])


# ==========================================================================
# S9_1 -- excision: causality, horizon, r_exc, mask  (cues 130-135)
# ==========================================================================
class S9_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S9_1"], END["S9_1"]
    STARS = (60, 31)

    def construct(self):
        header = hdr("Excision")
        self.add(header)

        # cue 130: stop evolving points inside a surface
        t1 = note("excision: stop evolving every point inside a surface r < r_exc", 24, YELLOW)
        t1.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        cen = np.array([0.0, -1.0, 0.0])
        lat = lattice(cen, radius=2.1, step=0.3)
        circ = DashedVMobject(Circle(radius=0.95, color=RED).move_to(cen), num_dashes=30)
        inner = VGroup(*[d for d in lat if np.linalg.norm(d.get_center() - cen) < 0.95])
        self.at(130, FadeIn(t1), run_time=0.5)
        self.sync(CUES[130].start + 1.2)
        self.play(FadeIn(lat), run_time=0.6)
        self.sync(CUES[130].start + 3.5)
        self.play(Create(circ), inner.animate.set_color(GREY_D).set_opacity(0.4), run_time=1.2)

        # cue 131: inside the horizon information only flows inward
        self.at(131, *[FadeOut(m) for m in [lat, circ]], run_time=0.4)
        base = -1.4
        axis = Arrow([-5.6, base, 0], [5.6, base, 0], buff=0, color=GREY_B, stroke_width=2)
        rl = tex(r"r", 28).next_to(axis, RIGHT, buff=0.05)
        xe, xh = 0.0, 1.7
        l_exc = DashedLine([xe, base - 0.3, 0], [xe, base + 2.6, 0], color=ORANGE)
        l_hor = DashedLine([xh, base - 0.3, 0], [xh, base + 2.6, 0], color=RED)
        tl_e = tex(r"r_{\rm exc}", 26, ORANGE).next_to(l_exc, UP, buff=0.05)
        tl_h = tex(r"r_+", 26, RED).next_to(l_hor, UP, buff=0.05)
        cones = VGroup(light_cone([-3.9, base + 0.9], 1.25, 0.6), light_cone([-1.9, base + 0.9], 1.1, 0.6),
                       light_cone([xe, base + 0.9], 0.95, 0.6), light_cone([3.7, base + 0.9], 0.1, 0.6))
        inn = note("all characteristics point inward:\nwhat we do inside cannot reach the outside", 20, GREEN).move_to([-3.4, base - 0.95, 0])
        out = note("outside:\nsignals can escape", 18, GREY_B).move_to([3.7, base - 0.85, 0])
        self.play(Create(axis), FadeIn(rl), Create(l_exc), Create(l_hor), FadeIn(tl_e), FadeIn(tl_h), run_time=0.8)
        self.play(FadeIn(cones), run_time=0.8)
        self.play(FadeIn(inn), FadeIn(out), run_time=0.6)
        prov = note("provided every characteristic at the surface points into the hole", 20, YELLOW).move_to([0, 2.0, 0])
        self.sync(CUES[131].start + 8.0)
        self.play(FadeOut(t1), FadeIn(prov), run_time=0.5)
        self.snap("cones")

        # cue 132: for Kerr the horizon is a spheroid
        self.at(132, *[FadeOut(m) for m in [axis, rl, l_exc, l_hor, tl_e, tl_h, cones, inn, out, prov]], run_time=0.5)
        sph = tex(r"\frac{x^2+y^2}{r_+^2+a^2} + \frac{z^2}{r_+^2} = 1", 40, RED)
        sph.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        sn = note("Kerr in Kerr–Schild coordinates: the horizon is the spheroid r = r₊\n→ no horizon finder needed", 22)
        sn.next_to(sph, DOWN, buff=0.4, aligned_edge=LEFT)
        sec = kerr_section(0.7, 1.15).move_to([4.2, -0.8, 0])
        self.play(Write(sph), run_time=1.4)
        self.play(FadeIn(sn), Create(sec), run_time=1.0)

        # cue 133: in general, solve for the apparent horizon
        theta = tex(r"\Theta = D_is^i - K + K_{ij}s^is^j = 0", 38, ORANGE)
        theta.next_to(sn, DOWN, buff=0.8, aligned_edge=LEFT)
        tn = note("in general: the apparent horizon is where the outgoing\nnull expansion Θ vanishes  (sⁱ: outward unit normal)", 20, GREY_B)
        tn.next_to(theta, DOWN, buff=0.3, aligned_edge=LEFT)
        self.at(133, Write(theta), run_time=1.6)
        self.play(FadeIn(tn), run_time=0.5)
        self.snap("theta")

        # cue 134: r_exc a few grid points inside the horizon
        self.at(134, *[FadeOut(m) for m in [sph, sn, sec, theta, tn]], run_time=0.5)
        dxr = 0.25
        rr = np.arange(1.0, 3.01, dxr)
        r_h, r_x = 2.0, 2.0 - 3.5 * dxr
        X = lambda r: -4.5 + (r - 0.8) * 3.6
        base2 = -0.6
        ax_r = Arrow([X(0.8) - 0.3, base2, 0], [X(3.3), base2, 0], buff=0, color=GREY_B, stroke_width=2)
        dots = VGroup(*[Dot([X(r), base2, 0], radius=0.07, color=(GREY_D if r < r_x else BLUE_C)) for r in rr])
        lh = DashedLine([X(r_h), base2 - 0.6, 0], [X(r_h), base2 + 1.3, 0], color=RED)
        lx = DashedLine([X(r_x), base2 - 0.6, 0], [X(r_x), base2 + 1.3, 0], color=ORANGE)
        th = tex(r"r_+", 28, RED).next_to(lh, UP, buff=0.05)
        tx = tex(r"r_{\rm exc}", 28, ORANGE).next_to(lx, UP, buff=0.05)
        br = BraceBetweenPoints([X(r_x), base2 - 0.75, 0], [X(r_h), base2 - 0.75, 0], direction=DOWN, color=YELLOW)
        brl = note("about 3–4 grid points", 20, YELLOW).next_to(br, DOWN, buff=0.1)
        top = note("r_exc sits slightly inside the horizon, by a few grid points", 22).next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(FadeIn(top), Create(ax_r), FadeIn(dots), run_time=0.8)
        self.play(Create(lh), FadeIn(th), run_time=0.5)
        self.sync(CUES[134].start + 5.0)
        self.play(Create(lx), FadeIn(tx), GrowFromCenter(br), FadeIn(brl), run_time=1.0)
        self.snap("rexc")

        # cue 135: use the r formula to build the mask
        self.at(135, *[FadeOut(m) for m in [top, ax_r, dots, lh, lx, th, tx, br, brl]], run_time=0.5)
        a = 0.7
        rpl = kerr_r_plus(a)
        r_cut = rpl - 3.5 * 0.12
        cen = np.array([3.5, -0.7, 0.0])
        pts = VGroup()
        for i in range(-12, 13):
            for j in range(-12, 13):
                x, z = i * 0.2, j * 0.2
                r, _ = ks_fields(a, x, z)
                pts.add(Dot(cen + [x * 0.75, z * 0.75, 0], radius=0.03, color=(GREY_D if r < r_cut else BLUE_C)))
        mk = tex(r"\text{mask: } r(x,z) < r_{\rm exc}", 36, YELLOW)
        mk.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        rf = tex(r"r^2 = \tfrac12\Big[(R^2-a^2)+\sqrt{(R^2-a^2)^2+4a^2z^2}\Big]", 26)
        rf.next_to(mk, DOWN, buff=0.4, aligned_edge=LEFT)
        clamp_width(rf, 6.2)
        mn = note("the same r as in the initial-data section;\nthe mask also swallows the ring singularity at r = 0", 20, GREY_B)
        mn.next_to(rf, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(Write(mk), run_time=1.2)
        self.play(Write(rf), FadeIn(pts), run_time=1.6)
        self.play(FadeIn(mn), run_time=0.5)
        self.snap("mask")
        self.finish(keep=[header])


# ==========================================================================
# S9_2 -- the excision boundary: leakage and the Jacobi refill  (cues 136-143)
# ==========================================================================
def bars(values, base_y, x0, width=0.42, gap=0.08, scale=1.4, active_from=None):
    g = VGroup()
    for i, v in enumerate(values):
        col = BLUE_C if (active_from is not None and i >= active_from) else ORANGE
        h = max(v * scale, 0.02)
        r = Rectangle(width=width, height=h, stroke_width=1, fill_color=col, fill_opacity=0.7, color=col)
        r.move_to([x0 + i * (width + gap), base_y + h / 2, 0])
        g.add(r)
    return g


class S9_2(SyncedScene):
    AUDIO_START, AUDIO_END = START["S9_2"], END["S9_2"]
    STARS = (60, 31)

    def construct(self):
        header = hdr("Excision")
        self.add(header)

        # cue 136: stencils reach into the excised points
        t1 = note("the subtlety: the boundary of the excised region", 24, YELLOW)
        t1.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.at(136, FadeIn(t1), run_time=0.5)
        n = 10
        base = -0.2
        cells = VGroup(*[Square(0.52, stroke_width=2, color=(GREY_B if i < 5 else BLUE_C),
                                fill_color=(GREY_D if i < 5 else BLUE_D), fill_opacity=(0.7 if i < 5 else 0.3))
                         for i in range(n)]).arrange(RIGHT, buff=0.0).move_to([-3.2, base, 0])
        ex_l = note("excised", 16, GREY_B).next_to(cells[:5], UP, buff=0.15)
        ac_l = note("evolved", 16, BLUE_C).next_to(cells[5:], UP, buff=0.15)
        self.sync(CUES[136].start + 1.5)
        self.play(FadeIn(cells), FadeIn(ex_l), FadeIn(ac_l), run_time=0.7)
        c0 = cells[5].get_center()
        arcs = VGroup(*[ArcBetweenPoints(c0 + [0, -0.3, 0], cells[i].get_center() + [0, -0.3, 0], angle=(1.5 if i < 5 else -1.5),
                                         color=ORANGE, stroke_width=4) for i in (3, 4, 6, 7)])
        arcs.add(*[cells[i].copy().set_fill(opacity=0).set_color(ORANGE).set_stroke(width=4) for i in (3, 4, 5, 6, 7)])
        stn = note("a 5-point stencil centred on the first\nevolved point reaches into excised points", 18, ORANGE)
        stn.next_to(cells, DOWN, buff=0.75)
        self.sync(CUES[136].start + 4.0)
        self.play(Create(arcs), FadeIn(stn), run_time=1.0)
        # numerical domain of dependence vs the light cone
        P = np.array([3.6, -1.9, 0.0])
        wide = Polygon(P, P + [-1.9, 2.4, 0], P + [1.9, 2.4, 0], color=ORANGE, fill_opacity=0.18, stroke_width=2)
        narrow = Polygon(P, P + [-0.6, 2.4, 0], P + [0.6, 2.4, 0], color=YELLOW, fill_opacity=0.35, stroke_width=2)
        wl = note("numerical domain\nof dependence", 16, ORANGE).move_to(P + [-2.1, 1.7, 0])
        nl = note("light cone", 16, YELLOW).move_to(P + [1.45, 2.65, 0])
        self.sync(CUES[136].start + 8.5)
        self.play(FadeIn(wide), FadeIn(narrow), FadeIn(wl), FadeIn(nl), run_time=0.8)
        leak = note("the stencil's reach is wider than the light cone, so errors near the\nsingularity can leak outward even though the physics is causal", 18, RED)
        leak.move_to([-1.6, -3.0, 0])
        self.sync(CUES[136].start + 11.5)
        self.play(FadeIn(leak), run_time=0.6)
        self.snap("leak")

        # cue 137: freezing the interior is not enough
        self.at(137, *[FadeOut(m) for m in [t1, cells, ex_l, ac_l, arcs, stn, wide, narrow, wl, nl, leak]], run_time=0.5)
        t2 = note("first attempt: simply freeze the values inside – not enough", 24, YELLOW)
        t2.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(FadeIn(t2), run_time=0.5)
        # cue 138: slows the growth, does not stop the exponential divergence
        ax = Axes(x_range=[0, 8, 1], y_range=[-8, 1, 2], x_length=6.4, y_length=3.0,
                  axis_config={"include_tip": False, "font_size": 16}).move_to([-1.6, -1.2, 0])
        axl = VGroup(note("time", 16).next_to(ax, DOWN, buff=0.05).align_to(ax, RIGHT),
                     note("log₁₀ (error)    – schematic", 16).next_to(ax, UP, buff=0.05).align_to(ax, LEFT))
        e_none = ax.plot(lambda t: -6 + 0.95 * t, x_range=[0, 7.2], color=RED)
        e_frz = ax.plot(lambda t: -6 + 0.45 * t, x_range=[0, 8], color=ORANGE)
        e_ref = ax.plot(lambda t: -6 + 0.02 * t, x_range=[0, 8], color=GREEN)
        l_none = note("interior left alone", 16, RED).next_to(ax.c2p(7.2, -6 + 0.95 * 7.2), LEFT, buff=0.1)
        l_frz = note("interior frozen:\nslower, still exponential", 16, ORANGE).next_to(ax.c2p(8, -6 + 0.45 * 8), RIGHT, buff=0.12)
        l_ref = note("refilled each step", 16, GREEN).next_to(ax.c2p(8, -6 + 0.02 * 8), RIGHT, buff=0.12)
        self.at(138, Create(ax), FadeIn(axl), run_time=0.5)
        self.play(Create(e_none), FadeIn(l_none), run_time=1.0)
        self.play(Create(e_frz), FadeIn(l_frz), run_time=1.2)
        self.snap("errors")

        # cue 139: refill every step by extrapolation, Jacobi-style
        self.at(139, Create(e_ref), FadeIn(l_ref), run_time=1.0)
        self.sync(CUES[139].start + 3.0)
        self.play(*[FadeOut(m) for m in [ax, axl, e_none, e_frz, e_ref, l_none, l_frz, l_ref, t2]], run_time=0.4)
        t3 = note("refill the excised points at every step: extrapolate from the outside\nusing a Jacobi-style iteration", 22, GREEN)
        t3.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(t3), run_time=0.4)
        N, NE = 14, 8
        rng = np.random.default_rng(3)
        u = np.zeros(N)
        u[NE:] = 1.0 + 0.15 * np.arange(N - NE)[::-1]
        u[:NE] = rng.uniform(0.0, 1.8, NE)
        frames = [u.copy()]
        iters = [0, 1, 3, 8, 40]
        cur = u.copy()
        done = 0
        for k in range(1, 41):
            nxt = cur.copy()
            for i in range(NE):
                left = cur[i - 1] if i > 0 else cur[0]
                nxt[i] = 0.5 * (left + cur[i + 1])
            cur = nxt
            if k in iters:
                frames.append(cur.copy())
        x0 = -3.0
        base_y = -2.5
        b = bars(frames[0], base_y, x0, active_from=NE)
        sep = DashedLine([x0 + NE * 0.5 - 0.29, base_y - 0.1, 0], [x0 + NE * 0.5 - 0.29, base_y + 2.7, 0], color=RED)
        ex_t = note("excised", 16, ORANGE).move_to([x0 + 2.0, base_y + 2.8, 0])
        ac_t = note("evolved (fixed)", 16, BLUE_C).move_to([x0 + 5.0, base_y + 2.8, 0])
        it_t = note("iteration 0", 18, YELLOW).move_to([x0 + 3.5, base_y - 0.45, 0])
        self.play(FadeIn(b), FadeIn(sep), FadeIn(ex_t), FadeIn(ac_t), FadeIn(it_t), run_time=0.5)
        for k, fr in zip(iters[1:], frames[1:]):
            nb = bars(fr, base_y, x0, active_from=NE)
            nt = note(f"iteration {k}", 18, YELLOW).move_to(it_t)
            self.play(Transform(b, nb), Transform(it_t, nt), run_time=0.55)
            self.wait(0.05)
        self.snap("jacobi")

        # cue 140: refilled values need to be smooth and non-growing, not accurate
        sm = note("the refilled values do not need to be accurate –\nonly smooth and non-growing", 22, YELLOW)
        sm.move_to([3.7, 1.0, 0])
        self.at(140, FadeIn(sm), run_time=0.5)

        # cues 141-142: low order, doesn't converge at the surface -- acceptable
        self.at(141, FadeOut(sm), run_time=0.4)
        lo = note("a low-order treatment:\nit does not converge at the excision surface", 22, ORANGE).move_to([3.7, 0.9, 0])
        self.play(FadeIn(lo), run_time=0.5)
        ok = note("that is acceptable – the region is causally\ndisconnected from the exterior", 22, GREEN).move_to([3.7, -0.5, 0])
        self.at(142, FadeIn(ok), run_time=0.5)

        # cue 143: convergence tests must exclude a thin shell
        self.at(143, *[FadeOut(m) for m in [b, sep, ex_t, ac_t, it_t, t3, lo, ok]], run_time=0.5)
        ax2 = Axes(x_range=[0.8, 3.0, 0.5], y_range=[-5, -1, 1], x_length=6.0, y_length=3.0,
                   axis_config={"include_tip": False, "font_size": 16}).move_to([-3.0, -1.0, 0])
        ax2.x_axis.shift(DOWN * 3.0)
        ss = lambda s: 0.0 if s <= 0 else (1.0 if s >= 1 else s * s * (3 - 2 * s))
        base_e = lambda r: -2.4 - 0.6 * (r - 1.0)
        coarse = ax2.plot(lambda r: base_e(r), x_range=[0.8, 3.0], color=ORANGE)
        fine = ax2.plot(lambda r: base_e(r) - 0.6 * ss((r - 1.15) / 0.35), x_range=[0.8, 3.0], color=GREEN)
        shell = Rectangle(width=ax2.c2p(1.5, 0)[0] - ax2.c2p(0.8, 0)[0], height=3.0, color=RED, fill_opacity=0.18, stroke_width=0)
        shell.move_to([(ax2.c2p(1.5, 0)[0] + ax2.c2p(0.8, 0)[0]) / 2, ax2.get_center()[1], 0])
        l_c = note("Δx", 16, ORANGE).next_to(ax2.c2p(3.0, base_e(3.0)), UP, buff=0.12)
        l_f = note("Δx / 2", 16, GREEN).next_to(ax2.c2p(3.0, base_e(3.0) - 0.6), DOWN, buff=0.12)
        axl2 = VGroup(note("r / M", 16).next_to(ax2, DOWN, buff=0.05).align_to(ax2, RIGHT),
                      note("log₁₀ (error)    – schematic", 16).next_to(ax2, UP, buff=0.05).align_to(ax2, LEFT))
        sn = note("convergence tests should exclude\na thin shell around the excision surface", 20, RED).next_to(ax2, RIGHT, buff=0.4)
        self.play(Create(ax2), FadeIn(axl2), run_time=0.6)
        self.play(Create(coarse), Create(fine), FadeIn(l_c), FadeIn(l_f), run_time=1.2)
        self.play(FadeIn(shell), FadeIn(sn), run_time=0.8)
        self.snap("shell")
        self.finish(keep=[header])


# ==========================================================================
# S10 -- method of lines, stencils, RK4, CFL  (cues 144-152)
# ==========================================================================
def stencil_row(offsets, weights, color=BLUE_C, width_unit=0.62, label_size=24, extent=3):
    """A number line with grid points i-extent..i+extent; the points in `offsets`
    are highlighted and carry the weight (a MathTex string) underneath."""
    g = VGroup()
    line = Line([-extent * width_unit - 0.3, 0, 0], [extent * width_unit + 0.3, 0, 0], color=GREY_B, stroke_width=2)
    g.add(line)
    for k in range(-extent, extent + 1):
        used = k in offsets
        d = Dot([k * width_unit, 0, 0], radius=0.1 if used else 0.05, color=color if used else GREY_B)
        g.add(d)
        idx = tex(("i" if k == 0 else (f"i{k:+d}")), 20, GREY_B if not used else WHITE).next_to(d, UP, buff=0.15)
        g.add(idx)
    for k, w in zip(offsets, weights):
        if w is None:
            continue
        g.add(tex(w, label_size, color).move_to([k * width_unit, -0.5, 0]))
    return g


class S10(SyncedScene):
    AUDIO_START, AUDIO_END = START["S10"], END["S10"]
    STARS = (50, 37)

    def construct(self):
        header = hdr("Method of Lines + RK4", 32)
        # cue 144: discretize space, leave time continuous
        self.at(144, Write(header), run_time=0.6)
        xs = np.linspace(-4.2, 4.2, 15)
        base = 0.2
        ax = Line([-4.8, base, 0], [4.8, base, 0], color=GREY_B, stroke_width=2)
        dots = VGroup(*[Dot([x, base, 0], radius=0.08, color=BLUE_C) for x in xs])
        tl = tex(r"x_i", 26, BLUE_C).next_to(dots[7], DOWN, buff=0.25)
        # each grid value oscillates in time -- time stays continuous
        tr = ValueTracker(0.0)
        arrows = always_redraw(lambda: VGroup(*[
            Arrow([x, base, 0], [x, base + 0.7 * math.sin(0.9 * x + 1.7 * tr.get_value()), 0], buff=0, color=ORANGE,
                  stroke_width=3, max_tip_length_to_length_ratio=0.3)
            if abs(0.7 * math.sin(0.9 * x + 1.7 * tr.get_value())) > 0.12 else VGroup()
            for x in xs]))
        t_cont = note("space: discrete grid points x_i", 22, BLUE_C).move_to([-2.6, -1.2, 0])
        t_time = note("time: left continuous – each u_i(t) is a function of t", 22, ORANGE).move_to([0, -1.8, 0])
        self.sync(CUES[144].start + 1.0)
        self.play(Create(ax), FadeIn(dots), FadeIn(tl), FadeIn(t_cont), run_time=0.8)
        self.add(arrows)
        self.play(tr.animate.set_value(3.0), FadeIn(t_time), run_time=3.0, rate_func=linear)

        # cue 145: one big system of ODEs
        self.at(145, *[FadeOut(m) for m in [ax, dots, tl, arrows, t_cont, t_time]], run_time=0.4)
        self.remove(arrows)
        ode = tex(r"\frac{d}{dt}\begin{pmatrix}u_1\\ \vdots\\ u_N\end{pmatrix} = \begin{pmatrix}F_1(u)\\ \vdots\\ F_N(u)\end{pmatrix}"
                  r"\quad\Longleftrightarrow\quad \partial_t u = F(u)", 40)
        clamp_width(ode, 10.5)
        ode.move_to([0, 0.5, 0])
        on = note("u: every evolved variable at every grid point  –  one big system of ODEs", 22, GREY_B)
        on.next_to(ode, DOWN, buff=0.5)
        self.play(Write(ode), run_time=1.8)
        self.play(FadeIn(on), run_time=0.5)

        # cue 146-147: centred finite differences, 2nd order -> 4th order
        self.at(146, FadeOut(ode), FadeOut(on), run_time=0.4)
        t_c = note("spatial derivatives: centred finite differences", 24, YELLOW).next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(t_c), run_time=0.4)
        s2 = stencil_row([-1, 0, 1], [r"-\tfrac12", None, r"+\tfrac12"], BLUE_C)
        s2.move_to([-3.4, -0.4, 0])
        l2 = tex(r"\partial_x u \approx \frac{u_{i+1}-u_{i-1}}{2\Delta x}", 26).next_to(s2, DOWN, buff=0.45)
        h2 = note("2nd order  (where we start)", 20, BLUE_C).next_to(s2, UP, buff=0.5)
        self.play(Create(s2), run_time=0.8)
        self.play(Write(l2), FadeIn(h2), run_time=0.8)
        s4 = stencil_row([-2, -1, 0, 1, 2], [r"\tfrac1{12}", r"-\tfrac23", None, r"\tfrac23", r"-\tfrac1{12}"], GREEN)
        s4.move_to([3.2, -0.4, 0])
        l4 = tex(r"\partial_x u \approx \frac{u_{i-2}-8u_{i-1}+8u_{i+1}-u_{i+2}}{12\Delta x}", 22).next_to(s4, DOWN, buff=0.45)
        h4 = note("4th order  (the natural upgrade)", 20, GREEN).next_to(s4, UP, buff=0.5)
        self.at(147, Create(s4), run_time=0.8)
        self.play(Write(l4), FadeIn(h4), run_time=0.8)
        self.snap("stencils")

        # cue 148: advection terms use one-sided stencils biased against the shift
        self.at(148, *[FadeOut(m) for m in [t_c, s2, l2, h2, s4, l4, h4]], run_time=0.4)
        t_a = note("exception: the advection terms βᵏ∂ₖ use one-sided stencils,\nbiased against the direction of the shift", 22, ORANGE)
        t_a.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        adv = tex(r"\beta^k\partial_k u", 40, ORANGE).move_to([-3.6, 0.5, 0])
        sa = stencil_row([-2, -1, 0], [r"\tfrac12", r"-2", r"\tfrac32"], ORANGE)
        sa.move_to([2.6, 0.3, 0])
        beta_ar = Arrow([1.2, 1.5, 0], [3.4, 1.5, 0], buff=0, color=PURPLE)
        beta_l = tex(r"\beta>0", 26, PURPLE).next_to(beta_ar, UP, buff=0.08)
        la = tex(r"\partial_x u \approx \frac{3u_i - 4u_{i-1} + u_{i-2}}{2\Delta x}", 26, ORANGE).next_to(sa, DOWN, buff=0.45)
        up_n = note("the stencil leans toward where the information comes from (upwind)", 20, GREY_B).move_to([0, -2.6, 0])
        self.play(FadeIn(t_a), run_time=0.5)
        self.sync(CUES[148].start + 3.0)
        self.play(Write(adv), run_time=0.8)
        self.sync(CUES[148].start + 5.0)
        self.play(Create(sa), GrowArrow(beta_ar), FadeIn(beta_l), run_time=1.0)
        self.play(Write(la), run_time=0.8)
        self.sync(CUES[148].start + 9.0)
        self.play(FadeIn(up_n), run_time=0.5)

        # cue 149: classical RK4
        self.at(149, *[FadeOut(m) for m in [t_a, adv, sa, beta_ar, beta_l, la, up_n]], run_time=0.4)
        rk = tex(r"""\begin{aligned}
            k_1 &= F(u^n), & k_2 &= F\big(u^n+\tfrac{\Delta t}{2}k_1\big),\\[3pt]
            k_3 &= F\big(u^n+\tfrac{\Delta t}{2}k_2\big), & k_4 &= F\big(u^n+\Delta t\,k_3\big),
            \end{aligned}""", 30)
        upd = tex(r"u^{n+1} = u^n + \frac{\Delta t}{6}\big(k_1 + 2k_2 + 2k_3 + k_4\big)", 34, YELLOW)
        rkg = VGroup(rk, upd).arrange(DOWN, buff=0.45, aligned_edge=LEFT)
        rkg.next_to(header, DOWN, buff=1.0, aligned_edge=LEFT)
        clamp_width(rkg, 8.6)
        rk_t = note("classical 4th-order Runge–Kutta", 22, GREY_B).next_to(rkg, UP, buff=0.2, aligned_edge=LEFT)
        self.play(Write(rk), FadeIn(rk_t), run_time=1.5)
        self.play(Write(upd), run_time=0.8)

        # cue 150-151: CFL condition
        cfl = tex(r"\Delta t \le \lambda\,\Delta x", 44, TEAL)
        cfl.next_to(rkg, DOWN, buff=0.6, aligned_edge=LEFT)
        cn = note("stability requires the CFL condition", 20, TEAL).next_to(cfl, RIGHT, buff=0.5)
        self.at(150, Write(cfl), FadeIn(cn), run_time=1.2)
        lam = tex(r"\lambda \approx 0.25", 40, YELLOW).next_to(cfl, DOWN, buff=0.45, aligned_edge=LEFT)
        ln = note("a safe choice that accounts for the fastest gauge speeds", 20, GREY_B).next_to(lam, RIGHT, buff=0.5)
        self.at(151, Write(lam), FadeIn(ln), run_time=1.2)
        self.snap("rk4")

        # cue 152: every RK stage must do three things first
        self.at(152, *[FadeOut(m) for m in [cfl, cn, lam, ln, rk_t]], rkg.animate.scale(0.7).to_corner(UL, buff=0.9).shift(DOWN * 0.7), run_time=0.6)
        stage = note("at every RK stage – not just every step – first:", 24, YELLOW).move_to([0, 1.2, 0])
        s1 = card("apply the boundary\nconditions", BLUE, 20, 3.0)
        s2c = card("fill the cartoon\nghost planes", TEAL, 20, 3.0)
        s3 = card("refill the\nexcised region", ORANGE, 20, 3.0)
        s4c = card("then evaluate\nF(u)", GREEN, 20, 3.0)
        chain = VGroup(s1, s2c, s3, s4c).arrange(RIGHT, buff=0.5).move_to([0, -0.8, 0])
        fit_width(chain, CONTENT_MAX_WIDTH)
        chain_ar = VGroup(*[Arrow(chain[i].get_right(), chain[i + 1].get_left(), buff=0.05) for i in range(3)])
        self.play(FadeIn(stage), run_time=0.4)
        self.sync(CUES[152].start + 1.8)
        self.play(FadeIn(s1), run_time=0.4)
        self.sync(CUES[152].start + 3.8)
        self.play(GrowArrow(chain_ar[0]), FadeIn(s2c), run_time=0.4)
        self.sync(CUES[152].start + 5.8)
        self.play(GrowArrow(chain_ar[1]), FadeIn(s3), run_time=0.4)
        self.sync(CUES[152].start + 7.6)
        self.play(GrowArrow(chain_ar[2]), FadeIn(s4c), run_time=0.4)
        self.snap("stage")
        self.finish(keep=[header])


# ==========================================================================
# S10_1 -- Kreiss-Oliger dissipation  (cues 153-155)
# ==========================================================================
def ko_filter(u, c):
    """u += c * (sixth difference of u), periodic.  c = eps * (dt/dx)/64 style gain."""
    d6 = (np.roll(u, 3) - 6 * np.roll(u, 2) + 15 * np.roll(u, 1) - 20 * u
          + 15 * np.roll(u, -1) - 6 * np.roll(u, -2) + np.roll(u, -3))
    return u + c * d6


class S10_1(SyncedScene):
    AUDIO_START, AUDIO_END = START["S10_1"], END["S10_1"]
    STARS = (50, 37)

    def construct(self):
        header = hdr("Method of Lines + RK4", 32)
        self.add(header)
        N = 72
        x = np.linspace(0, 1, N, endpoint=False)
        rng = np.random.default_rng(5)
        clean = np.sin(2 * PI * x)
        noise = 0.16 * ((-1.0) ** np.arange(N)) * rng.uniform(0.5, 1.0, N)
        u0 = clean + noise
        frames = [u0]
        cur = u0.copy()
        for k in range(1, 25):
            cur = ko_filter(cur, 0.014)  # stable: need gain <= 1/32 (the 6th difference is -64 at Nyquist)
            if k in (1, 3, 8, 24):
                frames.append(cur.copy())
        ax = Axes(x_range=[0, 1, 0.25], y_range=[-1.5, 1.5, 0.5], x_length=8.4, y_length=2.9,
                  axis_config={"include_tip": False, "font_size": 16}).move_to([0, -1.35, 0])

        def curve(u, col):
            m = VMobject(stroke_color=col, stroke_width=3)
            m.set_points_as_corners([ax.c2p(float(xx), float(uu)) for xx, uu in zip(list(x) + [1.0], list(u) + [u[0]])])
            return m
        c_clean = curve(clean, GREY_B)
        c_clean.set_stroke(width=2, opacity=0.6)
        c_now = curve(frames[0], RED)
        # cue 153: grid-scale noise can grow near the excision surface and outer boundary
        t1 = note("finite-difference noise at the grid scale can grow –\nespecially near the excision surface and the outer boundary", 22, RED)
        t1.next_to(header, DOWN, buff=0.45, aligned_edge=LEFT)
        self.at(153, Create(ax), run_time=0.5)
        self.play(Create(c_clean), Create(c_now), FadeIn(t1), run_time=1.2)

        # cue 154: add a small high-frequency filter to every right-hand side
        self.at(154, FadeOut(t1), run_time=0.4)
        ko = tex(r"\partial_t u \mathrel{+}= \frac{\epsilon}{64\,\Delta x}\big(u_{i+3}-6u_{i+2}+15u_{i+1}-20u_i+15u_{i-1}-6u_{i-2}+u_{i-3}\big)", 26, ORANGE)
        clamp_width(ko, CONTENT_MAX_WIDTH)
        ko.next_to(header, DOWN, buff=0.45, aligned_edge=LEFT)
        ko_n = note("added to every right-hand side  –  for a fourth-order scheme: the 6th-difference form,\n0 < ε < 1 (typically ∼ 0.1)", 18, GREY_B)
        ko_n.next_to(ko, DOWN, buff=0.2, aligned_edge=LEFT)
        self.play(Write(ko), run_time=3.0)
        self.play(FadeIn(ko_n), run_time=0.4)

        # cue 155: it removes only the noisiest modes
        self.at(155, run_time=0.01)
        lab = note("repeated application (schematic): only the grid-scale noise is damped", 18, GREEN)
        lab.move_to([0, -3.15, 0])
        self.play(FadeIn(lab), run_time=0.3)
        for fr in frames[1:]:
            self.play(Transform(c_now, curve(fr, RED)), run_time=0.9)
        self.play(c_now.animate.set_color(GREEN), run_time=0.4)
        keep = note("the smooth solution survives; the convergence order is unchanged", 20, GREEN)
        keep.move_to(lab)
        self.play(FadeOut(lab), FadeIn(keep), run_time=0.4)
        self.snap("ko")
        self.finish(keep=[header])


# ==========================================================================
# S11 -- outer boundary: Sommerfeld  (cues 156-166)
# ==========================================================================
def wave_frames(bc, N=201, L=10.0, tmax=8.0, snap_every=4):
    """1D wave equation u_tt = u_xx with a right-moving Gaussian pulse.
    bc = 'dirichlet' (freeze the boundary value) or 'sommerfeld' (u_t = -u_x
    at the right edge, u_t = +u_x at the left).  CFL = 1, so leapfrog is exact
    in the interior and the radiative edge is exactly absorbing here."""
    dx = L / (N - 1)
    dt = dx
    x = np.linspace(0, L, N)
    f = lambda s: np.exp(-((s - 6.0) ** 2) / 0.35)
    u_prev, u = f(x + dt), f(x)
    frames = [u.copy()]
    for n in range(int(tmax / dt)):
        un = np.zeros(N)
        un[1:-1] = 2 * u[1:-1] - u_prev[1:-1] + (u[2:] - 2 * u[1:-1] + u[:-2])
        if bc == "dirichlet":
            un[0], un[-1] = 0.0, 0.0
        else:
            un[-1] = u[-1] - (u[-1] - u[-2])
            un[0] = u[0] + (u[1] - u[0])
        u_prev, u = u, un
        if (n + 1) % snap_every == 0:
            frames.append(u.copy())
    return x, frames


class S11(SyncedScene):
    AUDIO_START, AUDIO_END = START["S11"], END["S11"]
    STARS = (50, 41)

    def construct(self):
        header = hdr("Boundary Conditions")
        # cue 156: the outer boundary is artificial -- let waves leave without reflecting
        self.at(156, Write(header), run_time=0.6)
        cen = np.array([0.0, -0.6, 0.0])
        domain = Circle(radius=2.1, color=BLUE_C, stroke_width=3).move_to(cen)
        bdry = DashedVMobject(Circle(radius=2.1, color=ORANGE, stroke_width=4).move_to(cen), num_dashes=40)
        bh = Circle(radius=0.35, color=WHITE, fill_color=BLACK, fill_opacity=1, stroke_width=2).move_to(cen)
        waves = VGroup(*[Circle(radius=r, color=TEAL, stroke_width=2, stroke_opacity=0.8 - 0.25 * k).move_to(cen)
                         for k, r in enumerate([0.8, 1.3, 1.8])])
        out_ar = VGroup(*[Arrow(cen + 1.2 * np.array([math.cos(t), math.sin(t), 0]),
                                cen + 2.7 * np.array([math.cos(t), math.sin(t), 0]),
                                buff=0, color=TEAL, stroke_width=3) for t in np.linspace(0.4, TAU + 0.4, 6, endpoint=False)])
        bl = note("outer boundary:\nartificial", 20, ORANGE).move_to(cen + [3.9, 1.5, 0])
        gl = note("waves and gauge pulses should\nleave without reflecting", 20, TEAL).move_to(cen + [-4.0, 1.3, 0])
        self.sync(CUES[156].start + 1.0)
        self.play(FadeIn(domain), FadeIn(bh), Create(bdry), FadeIn(bl), run_time=1.0)
        self.play(FadeIn(waves), LaggedStart(*[GrowArrow(a) for a in out_ar], lag_ratio=0.1), FadeIn(gl), run_time=1.5)

        # cues 157-159: the options (with a real 1D wave demo on the right)
        self.at(157, *[FadeOut(m) for m in [domain, bdry, bh, waves, out_ar, bl, gl]], run_time=0.5)
        xD, fD = wave_frames("dirichlet")
        xS, fS = wave_frames("sommerfeld")
        nfr = len(fD)
        idx = ValueTracker(0.0)

        def mk_ax(cy):
            return Axes(x_range=[0, 10, 2], y_range=[-1.2, 1.2, 1], x_length=4.8, y_length=1.45,
                        axis_config={"include_tip": False, "font_size": 14, "include_ticks": False}).move_to([3.9, cy, 0])
        axA, axB = mk_ax(1.15), mk_ax(-1.15)

        def trace(ax, xs, frames, col):
            def f():
                k = int(min(idx.get_value(), nfr - 1))
                u = frames[k]
                m = VMobject(stroke_color=col, stroke_width=3)
                m.set_points_as_corners([ax.c2p(float(a), float(b)) for a, b in zip(xs[::2], u[::2])])
                return m
            return always_redraw(f)
        trA, trB = trace(axA, xD, fD, ORANGE), trace(axB, xS, fS, GREEN)
        wall = lambda ax: DashedLine(ax.c2p(10, -1.2), ax.c2p(10, 1.2), color=RED, stroke_width=3)
        tA = note("frozen boundary value (Dirichlet): the pulse bounces back", 15, ORANGE).next_to(axA, UP, buff=0.05)
        tB = note("Sommerfeld (radiative): the pulse leaves", 15, GREEN).next_to(axB, UP, buff=0.05)
        idx.add_updater(lambda m, dt: m.set_value(min(m.get_value() + dt * 1.75, nfr - 1)))
        self.add(idx)
        self.play(Create(axA), Create(axB), Create(wall(axA)), Create(wall(axB)), FadeIn(tA), FadeIn(tB), run_time=0.8)
        self.add(trA, trB)

        def row(sym, col, text, sub=None):
            r = VGroup(Text(sym, font_size=26, color=col), Text(text, font_size=21))
            r.arrange(RIGHT, buff=0.2, aligned_edge=UP)
            if sub:
                s = Text(sub, font_size=16, color=GREY_B)
                s.next_to(r[1], DOWN, buff=0.08, aligned_edge=LEFT)
                r.add(s)
            return r
        rows = [row("✗", RED, "Dirichlet: freeze the fields", "reflects waves back in"),
                row("✗", RED, "Neumann / copy", "also reflects, and breaks the 1/r falloff of Kerr"),
                row("✗", RED, "periodic", "unphysical here"),
                row("✓", BLUE, "constraint-preserving, reflection-free", "e.g. Bayliss–Turkel, Buchman–Sarbach: best, but heavy"),
                row("✓", GREEN, "Sommerfeld (radiative)", "simple, and exactly preserves a static 1/r tail")]
        col = VGroup(*rows).arrange(DOWN, aligned_edge=LEFT, buff=0.38)
        col.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        col.shift(DOWN * 0.1)
        self.sync(CUES[157].start + 0.8)
        self.play(FadeIn(rows[0], shift=RIGHT * 0.2), run_time=0.4)
        self.sync(CUES[157].start + 3.2)
        self.play(FadeIn(rows[1], shift=RIGHT * 0.2), run_time=0.4)
        self.sync(CUES[157].start + 5.6)
        self.play(FadeIn(rows[2], shift=RIGHT * 0.2), run_time=0.4)
        self.at(158, FadeIn(rows[3], shift=RIGHT * 0.2), run_time=0.4)
        self.at(159, FadeIn(rows[4], shift=RIGHT * 0.2), run_time=0.4)
        box = SurroundingRectangle(rows[4], color=GREEN, buff=0.15)
        self.play(Create(box), run_time=0.5)
        self.snap("options")

        # cue 160: the Sommerfeld condition
        self.at(160, *[FadeOut(m) for m in [col, box, axA, axB, tA, tB, trA, trB]], run_time=0.5)
        self.remove(trA, trB)
        for mm in list(self.mobjects):
            if isinstance(mm, DashedLine) and mm not in self._persist:
                self.remove(mm)
        idx.clear_updaters()
        asm = note("assume each field approaches its flat-space value f₀,\nand the deviation is an outgoing spherical wave ∝ 1/r", 22, YELLOW)
        asm.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.sync(CUES[160].start + 0.5)
        self.play(FadeIn(asm), run_time=0.5)
        eqA = tex(r"(\partial_t + v\,\partial_r)\big(r\,(f-f_0)\big) = 0", 38)
        eqA.next_to(asm, DOWN, buff=0.5, aligned_edge=LEFT)
        self.sync(CUES[160].start + 3.5)
        self.play(Write(eqA), run_time=1.6)
        eqB = tex(r"\partial_t f = -v\Big[\frac{x^k}{r}\,\partial_k f + \frac{f-f_0}{r}\Big]", 42, YELLOW)
        eqB.next_to(eqA, DOWN, buff=0.5, aligned_edge=LEFT)
        self.sync(CUES[160].start + 8.0)
        self.play(Write(eqB), run_time=1.8)
        vn = tex(r"v = 1\ \text{for most fields},\qquad v = \sqrt2\ \text{for the 1+log lapse}", 28, TEAL)
        vn.next_to(eqB, DOWN, buff=0.5, aligned_edge=LEFT)
        self.sync(CUES[160].start + 12.5)
        self.play(Write(vn), run_time=1.5)
        self.snap("sommerfeld")

        # cues 161-162: the flat values
        self.at(161, FadeOut(asm), FadeOut(eqA), FadeOut(vn), eqB.animate.scale(0.75).next_to(header, DOWN, buff=0.45, aligned_edge=LEFT), run_time=0.5)
        flat_t = note("flat-space values f₀:", 22, YELLOW).next_to(eqB, DOWN, buff=0.45, aligned_edge=LEFT)
        f1 = tex(r"\alpha_0 = 1", 34)
        self.play(FadeIn(flat_t), run_time=0.4)
        f1.next_to(flat_t, DOWN, buff=0.3, aligned_edge=LEFT)
        self.sync(CUES[161].start + 0.6)
        self.play(Write(f1), run_time=0.8)
        f2 = tex(r"\tilde\gamma_{ij,0} = \delta_{ij}", 34)
        f3 = tex(r"\phi_0 = K_0 = \tilde A_{ij,0} = \tilde\Gamma^i_0 = \beta^i_0 = 0", 34)
        f2.next_to(f1, DOWN, buff=0.3, aligned_edge=LEFT)
        f3.next_to(f2, DOWN, buff=0.3, aligned_edge=LEFT)
        self.at(162, Write(f2), run_time=1.2)
        self.sync(CUES[162].start + 4.0)
        self.play(Write(f3), run_time=1.6)

        # cue 163: one-sided stencils at the boundary
        self.at(163, *[FadeOut(m) for m in [flat_t, f1, f2, f3]], run_time=0.4)
        os_t = note("derivatives at the boundary use one-sided stencils\n(only interior points are available)", 22, ORANGE)
        os_t.next_to(eqB, DOWN, buff=0.6, aligned_edge=LEFT)
        so = stencil_row([-2, -1, 0], [r"\tfrac12", r"-2", r"\tfrac32"], ORANGE)
        so.move_to([-1.5, -2.0, 0])
        wl = DashedLine([0.6, -2.6, 0], [0.6, -1.3, 0], color=RED, stroke_width=3)
        wln = note("outer boundary", 16, RED).next_to(wl, UP, buff=0.05)
        self.play(FadeIn(os_t), Create(so), Create(wl), FadeIn(wln), run_time=1.0)

        # cue 164: check -- a static 1/r tail gives df/dt = 0
        self.at(164, *[FadeOut(m) for m in [os_t, so, wl, wln]], run_time=0.4)
        chk = note("check: a static Kerr tail  f − f₀ = c / r", 22, YELLOW)
        chk.next_to(eqB, DOWN, buff=0.5, aligned_edge=LEFT)
        a1 = tex(r"\frac{x^k}{r}\,\partial_k\Big(\frac{c}{r}\Big) = -\frac{c}{r^2}", 34, ORANGE)
        a2 = tex(r"\frac{f-f_0}{r} = +\frac{c}{r^2}", 34, BLUE_C)
        pair = VGroup(a1, a2).arrange(RIGHT, buff=1.0).next_to(chk, DOWN, buff=0.45, aligned_edge=LEFT)
        res = tex(r"\partial_t f = -v\Big[-\frac{c}{r^2} + \frac{c}{r^2}\Big] = 0", 38, GREEN)
        res.next_to(pair, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(chk), run_time=0.4)
        self.sync(CUES[164].start + 2.5)
        self.play(Write(a1), run_time=1.4)
        self.sync(CUES[164].start + 5.5)
        self.play(Write(a2), run_time=1.2)
        self.sync(CUES[164].start + 8.0)
        self.play(Write(res), run_time=1.5)
        self.play(Circumscribe(res, color=GREEN), run_time=1.0)
        cancel = note("the two terms cancel exactly", 20, GREEN).next_to(res, RIGHT, buff=0.4)
        self.play(FadeIn(cancel), run_time=0.4)
        self.snap("check")

        # cue 165: but it is only an approximation
        self.at(165, *[FadeOut(m) for m in [chk, pair, cancel]], run_time=0.4)
        apx = note("it is an approximation – not a true characteristic condition", 24, RED)
        apx.next_to(res, DOWN, buff=0.7, aligned_edge=LEFT)
        self.play(FadeIn(apx), run_time=0.5)

        # cue 166: put the boundary far away and confirm independence
        self.at(166, *[FadeOut(m) for m in [res, apx, eqB]], run_time=0.4)
        far = note("so place the boundary very far away – 10M or more (tens of M if affordable)\nand confirm the results do not depend on where it is", 22, YELLOW)
        far.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        runs = VGroup(chip("R_out = 20M", BLUE_C, 22), chip("R_out = 40M", TEAL, 22), chip("R_out = 80M", GREEN, 22)).arrange(RIGHT, buff=0.9)
        runs.move_to([0, -0.6, 0])
        eqs = VGroup(tex(r"\approx", 44), tex(r"\approx", 44))
        eqs[0].move_to((runs[0].get_right() + runs[1].get_left()) / 2)
        eqs[1].move_to((runs[1].get_right() + runs[2].get_left()) / 2)
        same = note("same results  →  the boundary is not affecting the physics", 20, GREEN).next_to(runs, DOWN, buff=0.6)
        self.play(FadeIn(far), run_time=0.5)
        self.sync(CUES[166].start + 2.5)
        self.play(LaggedStart(*[FadeIn(r) for r in runs], lag_ratio=0.3), run_time=1.2)
        self.play(FadeIn(eqs), FadeIn(same), run_time=0.8)
        self.snap("far")
        self.finish(keep=[header])


# ==========================================================================
# S12 -- putting it together: code flow chart  (cues 167-178)
# ==========================================================================
def flow_box(text, color=BLUE, w=4.0, h=0.78, size=17):
    t = Text(text, font_size=size, line_spacing=0.9)
    box = RoundedRectangle(corner_radius=0.1, width=max(w, t.width + 0.4), height=max(h, t.height + 0.3), color=color)
    t.move_to(box.get_center())
    return VGroup(box, t)


class S12(SyncedScene):
    AUDIO_START, AUDIO_END = START["S12"], END["S12"]
    STARS = (50, 43)

    def construct(self):
        header = hdr("Code")
        # cues 167-170: putting it together is hard to find information on
        self.at(167, Write(header), run_time=0.6)
        t1 = Text("Now we actually have to put it all together.", font_size=32, color=YELLOW).move_to([0, 1.0, 0])
        self.sync(CUES[167].start + 0.4)
        self.play(FadeIn(t1), run_time=0.6)
        t2 = note("It sounds simple – but it is hard to find a clear write-up.", 24)
        t2.next_to(t1, DOWN, buff=0.5)
        self.at(168, FadeIn(t2), run_time=0.5)
        t3 = note("Most books and references assume you already know enough\nto fill in the details yourself –", 24, GREY_B)
        t3.next_to(t2, DOWN, buff=0.4)
        self.at(170, FadeIn(t3), run_time=0.5)
        t4 = note("so I will spell out the whole algorithm.", 26, GREEN)
        t4.next_to(t3, DOWN, buff=0.4)
        self.sync(CUES[170].start + 5.2)
        self.play(FadeIn(t4), run_time=0.5)

        # flow chart
        self.at(171, *[FadeOut(m) for m in [t1, t2, t3, t4]], run_time=0.4)
        xL, xR = -4.4, 2.6
        A = flow_box("1  initialize the parameters\nand set up the grid", BLUE).move_to([xL, 1.65, 0])
        B = flow_box("2  allocate the state arrays\non the grid", BLUE).move_to([xL, 0.55, 0])
        C = flow_box("3  set up the Kerr–Schild spacetime,\ncompute the conformal connections", TEAL).move_to([xL, -0.55, 0])
        D = flow_box("4  compute the Hamiltonian\nand momentum constraints", TEAL).move_to([xL, -1.65, 0])
        setup_arrows = VGroup(*[Arrow(a.get_bottom(), b.get_top(), buff=0.04, stroke_width=3, max_tip_length_to_length_ratio=0.25)
                                for a, b in [(A, B), (B, C), (C, D)]])
        self.play(FadeIn(A, shift=DOWN * 0.15), run_time=0.5)
        self.at(172, GrowArrow(setup_arrows[0]), FadeIn(B), run_time=0.5)
        self.sync(CUES[172].start + 4.2)
        self.play(GrowArrow(setup_arrows[1]), FadeIn(C), run_time=0.5)
        self.at(173, GrowArrow(setup_arrows[2]), FadeIn(D), run_time=0.5)

        # time loop
        loop_box = DashedVMobject(RoundedRectangle(corner_radius=0.2, width=6.6, height=5.4, color=YELLOW), num_dashes=70)
        loop_box.move_to([xR + 0.7, -0.2, 0])
        loop_lab = note("time loop", 18, YELLOW).move_to([xR + 0.7 + 2.4, 2.78, 0])
        E = flow_box("5  compute the CFL condition → Δt", ORANGE, w=5.6, h=0.6).move_to([xR + 0.7, 1.95, 0])
        F = flow_box("6  diagnostics", ORANGE, w=5.6, h=0.6).move_to([xR + 0.7, 1.05, 0])
        G = flow_box("7  RK4 step", GREEN, w=5.6, h=1.65, size=17).move_to([xR + 0.7, -0.45, 0])
        H = flow_box("8  diagnostics + output", ORANGE, w=5.6, h=0.6).move_to([xR + 0.7, -1.95, 0])
        sub = note("per stage:  Sommerfeld BC  →  cartoon ghost planes\n→  excision refill (extrapolation)\n→  right-hand side  +  dissipation  (+ safety net)", 14, GREY_B)
        sub.move_to(G).shift(DOWN * 0.22)
        G[1].move_to(G[0].get_top() + DOWN * 0.3)
        mid_ar = Arrow(D.get_right(), [xR - 2.9, D.get_center()[1], 0], buff=0.08, color=YELLOW, stroke_width=3)
        self.at(174, Create(loop_box), FadeIn(loop_lab), GrowArrow(mid_ar), FadeIn(E), run_time=0.8)
        loop_ar = VGroup(Arrow(E.get_bottom(), F.get_top(), buff=0.04, stroke_width=3, max_tip_length_to_length_ratio=0.3),
                         Arrow(F.get_bottom(), G.get_top(), buff=0.04, stroke_width=3, max_tip_length_to_length_ratio=0.3),
                         Arrow(G.get_bottom(), H.get_top(), buff=0.04, stroke_width=3, max_tip_length_to_length_ratio=0.3))
        self.at(175, GrowArrow(loop_ar[0]), FadeIn(F), run_time=0.5)

        # cue 176: the RK4 step with everything folded in
        self.at(176, GrowArrow(loop_ar[1]), FadeIn(G), run_time=0.5)
        self.sync(CUES[176].start + 3.0)
        self.play(FadeIn(sub), run_time=0.8)
        self.snap("rk")

        # cues 177-178: another diagnostic, repeat, finish with a final diagnostic
        self.at(177, GrowArrow(loop_ar[2]), FadeIn(H), run_time=0.5)
        back = CurvedArrow(H.get_right() + RIGHT * 0.05, E.get_right() + RIGHT * 0.05, angle=-TAU / 9, color=YELLOW, stroke_width=3)
        self.at(178, Create(back), run_time=1.0)
        rep = note("repeat until the end time", 16, YELLOW).next_to(back, RIGHT, buff=0.1).shift(UP * 0.0)
        rep.move_to([xR + 3.9, -0.2, 0]) if False else None
        fin = flow_box("9  final diagnostics", RED, w=4.0, h=0.6).move_to([xL, -2.75, 0])
        fin_ar = Arrow(D.get_bottom(), fin.get_top(), buff=0.04, stroke_width=3, color=RED, max_tip_length_to_length_ratio=0.25)
        self.sync(CUES[178].start + 5.5)
        self.play(GrowArrow(fin_ar), FadeIn(fin), run_time=0.8)
        self.snap("flow")
        self.finish()
