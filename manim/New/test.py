"""
WHCKL.py
================================================================================

Render one scene at a time, e.g.:

    manim -pqh WHCKL.py S1
    manim -pqh WHCKL.py S5

or render everything (see render_all.sh)

Scene map (this now reflects what each class actually shows on screen,
not the original -- and, in several cases, stale/mismatched -- outline
comments):
    S0            # Title card + outline
    S1            # Scope (4 bullets)
    S1_1          # "Scope: DG Method" bridge card
    S2            # General intro to DG (FD -> FV -> CG -> DG, weak form, ODE)
    S3            # Physical Model (Euler-Poisson system)
    S4            # L2 Projection
    S5            # HLLC Flux
    S6            # Standard LDG (auxiliary variable, Schur complement,
                  #   forward-Euler update loop)
    S9            # Well-balanced flux trick (hydrostatic star, polytropes/
                  #   Lane-Emden, the scheme's two failure modes, split,
                  #   modflux)
    S9_1          # Well-balanced momentum source & total energy conservation
    S9_2          # Time Discretization (SSP-RK)
    S10           # Oscillation Elimination
    S11           # Code (5-step diagram)
    S12           # Results (performance recap + placeholder results panel)
    S08_Questions # NOT part of this video -- see note below.

--------------------------------------------------------------------------
AUDIO SYNC
--------------------------------------------------------------------------
Each scene is rendered independently (per the CLI usage above), so Manim's
own clock, `self.time`, always restarts at 0 for every clip -- it has no
idea where that clip actually lands in the final, stitched-together video.

`AUDIO_START` (a class attribute on every `SyncedScene` below) records
that: the absolute timestamp, in seconds into the narration track
(DG_CSSN.srt), that corresponds to `self.time == 0` for that scene. Call
`self.sync(t)` anywhere in `construct()` and it will insert a `self.wait()`
just long enough to bring the clip's own clock even with narration time
`t` (a no-op if the clip is already past that point). Sync targets below
are written as `CUES[n].start` / `CUES[n].end`, i.e. "the moment cue n of
the SRT begins/ends", with the matched line quoted in a comment so you can
double check the pairing against the actual audio and nudge it if needed.

Each scene's `AUDIO_START` is simply the previous scene's last sync
target, so clips are meant to be laid end to end with no gaps or overlaps.

--------------------------------------------------------------------------
CONTENT THAT WAS ADDED, NOT JUST RETIMED
--------------------------------------------------------------------------
Matching every beat against DG_CSSN.srt turned up long stretches of
narration with no corresponding visual anywhere in the original file.
Rather than just holding on a static frame through all of them, new beats
were written for these spans, reconstructed from the *spoken description*
of the derivations (no equations for this material existed anywhere else
in this file to draw on):
    S6 (tail end)  forward-Euler update loop + "standard scheme" recap,
                   cues 117-124 (~920s-967s)
    S9 (middle)    polytropic relation, Lane-Emden equation, "star in a
                   box", a two-panel sketch of the scheme's failure mode
                   (mismatched flux/source discretizations -> leftover
                   truncation error sitting on the equilibrium), and a
                   bar-chart sketch of the energy-conservation failure
                   (large PE against large KE+internal, with a numerical
                   leak comparable in size), cues 128-157 (~967s-1204s)
    S9_1 (new)     well-balanced momentum-source algebra + total-energy
                   conservation derivation, cues 178-238 (~1364s-1827s)
    S9_2 (new)     SSP-RK time discretization, cues 239-248 (~1827s-1925s)
    S12 (new)      the closing "Results" section, cues 291-300 (~2312s-2392s)

These fill in real screen time and are synced the same way as everything
else, but -- unlike the rest of the file -- the exact equations in S9's
new panels, S9_1, and S9_2 are *this pass's* best reconstruction of math
that was only ever described out loud, not written down anywhere in the
original script. Check them against your derivation notes before treating
them as final; the surrounding staging/timing should still be fine even
if a formula needs a tweak. S12's closing panel is explicitly a labeled
placeholder (a toy collapsing-density sketch) standing in for the real
simulated-CCSN animation, which isn't available here -- swap in your
actual renders before using that scene as-is.

Also: `S08_Questions` ("If gravity moves at the speed of light...") does
not match anything in DG_CSSN.srt at all -- it reads like a leftover from
a different (cosmology-themed) project that shares this file's visual
theme. It is left as a plain `Scene` (no AUDIO_START) rather than given a
fabricated sync, and left out of `ALL_SCENES` below.
================================================================================
"""

import os
import re
from collections import namedtuple
from tkinter import RIGHT
from xml.dom.minidom import Text
from pathlib import Path
import numpy as np
from emacs_theme import *
from manim import *

# Consistent safe content area (stays clear of the top tab bar / bottom
# mode-line, which occupy roughly the outer 0.5 vertical units on each side).
TOP_Y = config.frame_height / 2 - 0.9
BOT_Y = -config.frame_height / 2 + 0.75
LEFT_X = -config.frame_width / 2 + 0.8
RIGHT_X = config.frame_width / 2 - 0.8

pack = Packages()# for LaTeX preamble (amsmath, amssymb, physics, siunitx)

MathTex.set_default(tex_template=pack.tex_template)
Tex.set_default(tex_template=pack.tex_template)


# =============================================================================
# Narration timing -- parse DG_CSSN.srt once, at import time, into a
# {cue_number: Cue(start, end, text)} lookup so the rest of this file can
# reference precise, traceable timestamps (`CUES[57].start`) instead of
# opaque hardcoded floats.
# =============================================================================
Cue = namedtuple("Cue", "start end text")
music_path = Path("~/Music").expanduser()
SRT_PATH = music_path / "DG_CSSN.srt"

def _load_cues(srt_path):
    cue_block = re.compile(
        r"(\d+)\s*\n"
        r"(\d\d):(\d\d):(\d\d),(\d\d\d)\s*-->\s*(\d\d):(\d\d):(\d\d),(\d\d\d)"
        r"\s*\n(.*?)(?=\n\s*\n|\Z)",
        re.S,
    )
    def to_seconds(h, m, s, ms):
        return int(h) * 3600 + int(m) * 60 + int(s) + int(ms) / 1000

    cues = {}
    with open(srt_path, encoding="utf-8") as f:
        text = f.read()
    for m in cue_block.finditer(text.strip() + "\n\n"):
        idx = int(m.group(1))
        start = to_seconds(*m.groups()[1:5])
        end = to_seconds(*m.groups()[5:9])
        cues[idx] = Cue(start, end, " ".join(m.group(10).split()))
    return cues

CUES = _load_cues(SRT_PATH)
TOTAL_DURATION = max(c.end for c in CUES.values())


def chrome_for(position, tab="DG CSNN.tex"):
    return BufferChrome(filename=tab, mode="LaTeX/Preview", position=position, tab=tab)


def audio_pct(audio_start):
    """Time-accurate replacement for hand-typed chrome_for("NN%") strings:
    what fraction of the full narration has elapsed once this scene's
    clip begins."""
    return f"{round(100 * audio_start / TOTAL_DURATION)}%"


def place_header(header, max_width=None):
    """Anchor a section-header mobject to a fixed top-left margin, shrinking
    it first if it would otherwise be wider than the safe content area.
    Every other element in the scene should be built relative to this fixed
    point (next_to(..., aligned_edge=LEFT)) so nothing drifts off-frame."""
    fit_width(header, max_width)
    header.to_edge(UP, buff=1.0)
    header.to_edge(LEFT, buff=0.8)
    return header


# Safe horizontal/vertical budget inside the chrome frame -- used below as
# a defensive backstop (not a substitute for actually looking at a render)
# on the newer, less battle-tested content added in this pass, since long
# MathTex lines are notoriously hard to size correctly by eye.
CONTENT_MAX_WIDTH = RIGHT_X - LEFT_X - 0.4
CONTENT_MAX_HEIGHT = TOP_Y - BOT_Y - 0.4


def lane_emden_profile(n, xi_max=12.0, h=0.01):
    """Numerically integrate the Lane-Emden equation
        (1/xi^2) d/dxi(xi^2 dtheta/dxi) = -theta^n,  theta(0)=1, theta'(0)=0
    with plain RK4 (no scipy dependency), starting just off the ξ=0
    singularity via the standard small-xi series expansion. Returns
    (xi_array, theta_array) from xi=0 up to the surface (theta == 0) or
    xi_max, whichever comes first -- used in S9 to plot an actual
    hydrostatic-equilibrium profile rather than just stating the equation.
    Checked against known values: surface at xi = pi for n=1 and
    xi ~= 6.897 for n=3 (n=5 has no finite surface, as expected).
    """
    xi = h
    theta = 1 - xi**2 / 6.0 + n * xi**4 / 120.0
    dtheta = -xi / 3.0 + n * xi**3 / 30.0
    xis, thetas = [0.0], [1.0]

    def deriv(xi, y1, y2):
        base = max(y1, 0.0)
        return y2, -(base ** n) - (2.0 / xi) * y2

    y1, y2 = theta, dtheta
    while xi < xi_max and y1 > 0:
        k1 = deriv(xi, y1, y2)
        k2 = deriv(xi + h / 2, y1 + h / 2 * k1[0], y2 + h / 2 * k1[1])
        k3 = deriv(xi + h / 2, y1 + h / 2 * k2[0], y2 + h / 2 * k2[1])
        k4 = deriv(xi + h, y1 + h * k3[0], y2 + h * k3[1])
        y1n = y1 + h / 6 * (k1[0] + 2 * k2[0] + 2 * k3[0] + k4[0])
        y2n = y2 + h / 6 * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1])
        xi += h
        xis.append(xi)
        thetas.append(max(y1n, 0.0))
        y1, y2 = y1n, y2n
    return np.array(xis), np.array(thetas)


def clamp_width(mobj, max_width=CONTENT_MAX_WIDTH):
    """Shrink `mobj` in place if it's wider than `max_width`; a no-op
    otherwise. Plain Manim (Mobject.set_width), independent of whatever
    emacs_theme.fit_width does under the hood, so it's safe to sprinkle
    on anything -- most usefully on long single-line MathTex, whose
    rendered width is hard to predict without actually rendering it."""
    if mobj.width > max_width:
        mobj.set_width(max_width)
    return mobj


class SyncedScene(Scene):
    """A Scene that can pace itself against absolute narration timestamps.

    Set AUDIO_START on a subclass to the narration time (seconds) at which
    that scene's rendered clip begins in the final, assembled video (i.e.
    the moment self.time == 0 corresponds to). Then call self.sync(t) at
    each beat that should land on narration timestamp t; it waits just
    long enough to catch up and never rewinds (a target already passed is
    a no-op), so it's safe to sprinkle sync() calls between the existing
    self.play()/self.wait() pacing without disturbing it.
    """
    AUDIO_START = 0.0

    def sync(self, target_abs_time):
        dt = (target_abs_time - self.AUDIO_START) - self.time
        if dt > 0:
            self.wait(dt)


# =============================================================================
# S0 -- Title card + outline
# Narration: cue 1, "Hello, and welcome to discontinuous Glerken Method, a
# focus on self-gravitating Euler equations." (0.312 - 7.526)
# =============================================================================
class S0(SyncedScene):
    AUDIO_START = 0.0

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(50, seed=2))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        doc_title = code_line([("* ", FG_DIM),
                                ("Discontininuous Galerkin Method: Self Gravitating Euler Equations", FG, )],
                               font_size=26)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        fit_width(doc_title)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        self.play(Write(doc_title), run_time=.5)
        self.wait(0.3)

        sections = [
            "Introduction",
            "Scope",
            "General Introduction to DG",
            "Physical Model",
            "Deriving Projection",
            "Structure preserving LDG",
            "Time Discretization",
            "Ossilation Elimination",
            "Structure Preserving Properties",
            "Code ",
            "Results",
        ]
        rows = VGroup(*[
            code_line([("** ", FG_DIM), (s, FG)], font_size=24) for s in sections
        ]).arrange(DOWN, buff=0.32, aligned_edge=LEFT)
        fit_width(rows)
        rows.next_to(doc_title, DOWN, buff=0.55, aligned_edge=LEFT)
        rows.shift(RIGHT * 0.4)

        for row in rows:
            self.play(FadeIn(row, shift=RIGHT * 0.15), run_time=0.12)
        self.wait(1)

        # Quick highlight sweep across the outline, like jumping through an
        # org-mode buffer with occur/imenu.
        for row in rows:
            self.play(Indicate(row, color=TEAL, scale_factor=1.04), run_time=0.18)

        # Hold on the outline until cue 1 ("...self-gravitating Euler
        # equations.") finishes, exactly when cue 2 ("Here, we address the
        # scope of this video.") begins -- the cue S1 picks up from.
        self.sync(CUES[1].end)

        self.play(FadeOut(doc_title), FadeOut(rows))


# =============================================================================
# S1 -- Scope (4 bullets)
# Narration: cues 2-11 (8.498 - 67.526), the video's spoken "scope"
# description -- matched bullet-by-bullet below.
# =============================================================================
class S1(SyncedScene):
    AUDIO_START = CUES[1].end  # 7.526, where S0 left off

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        # cue 2: "Here, we address the scope of this video."
        self.sync(CUES[2].start)
        header = code_line([("\\section", BLUE), ("{", FG_DIM),
                             ("Scope", PURPLE), ("}", FG_DIM)], font_size=38)
        place_header(header)
        self.play(Write(header), run_time=0.5)
        self.wait(0.2)
        scope_items = VGroup(
            Text("• Structure-preserving DG", font_size=30),
            Text("• Positivity (mass, energy, pressure > 0)", font_size=30),
            Text("• Well-balanced hydrostatic equilibrium", font_size=30),
            Text("• Total energy conservation", font_size=30),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.35)
        scope_items.next_to(header, DOWN, aligned_edge=LEFT, buff=0.8)

        # cue 5: "Taking a self-gravitating fluid and using a
        # structure-preserving capacity"
        self.sync(CUES[5].start)
        self.play(FadeIn(scope_items[0], shift=RIGHT * 0.3), run_time=0.6)
        # cue 6: "...of discontinuous glurkin to preserve positivity, i.e."
        self.sync(CUES[6].start)
        self.play(FadeIn(scope_items[1], shift=RIGHT * 0.3), run_time=0.6)
        # cue 8: "We also include well-balanced, i.e."
        self.sync(CUES[8].start)
        self.play(FadeIn(scope_items[2], shift=RIGHT * 0.3), run_time=0.6)
        # cue 10: "...I won't go far into the math for these more advanced
        # subjects, but they are there." -- wraps up the remaining scope
        # items, energy conservation included; no line names it directly.
        self.sync(CUES[10].start)
        self.play(FadeIn(scope_items[3], shift=RIGHT * 0.3), run_time=0.6)

        # cue 11: "...I will refer to discontinuous glurkin as DG." ends
        # here, right where S1_1 picks up.
        self.sync(CUES[11].end)

        self.play(FadeOut(header, scope_items), run_time=0.5)


# =============================================================================
# S1_1 -- "Scope: DG Method" bridge card
# A very short (~0.7s) transition card between S1 and S2; nothing in the
# narration names it specifically.
# =============================================================================
class S1_1(SyncedScene):
    AUDIO_START = CUES[11].end  # 67.526

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Scope: DG Method", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header), run_time=0.5)

        # cue 12: "Before we get into all the math, we're going to take a
        # more intuitive look." -- S2's opening line.
        self.sync(CUES[12].start)
        self.play(FadeOut(header), run_time=0.3)

# =============================================================================
# S2 -- General intro to DG
# Narration: cues 12-38 (68.247 - 229.417): the FD -> FV -> CG -> DG family
# tree, then the mesh / weak form / residual-ODE derivation.
# =============================================================================
class S2(SyncedScene):
    AUDIO_START = CUES[12].start  # 68.247

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Introduction to DG", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))

        labels = [
            "Finite\nDifference",
            "Finite\nVolume",
            "Continuous\nGalerkin (CG)",
            "Discontinuous\nGalerkin (DG)",
        ]
        captions = [
            "point values, stencils",
            "cell averages, Riemann fluxes",
            "one global piecewise\npolynomial, continuous",
            "polynomials allowed to\njump across elements",
        ]
 
        boxes = VGroup()
        for label in labels:
            box = RoundedRectangle(
                corner_radius=0.15, width=2.6, height=1.4, color=BLUE
            )
            text = Text(label, font_size=22).move_to(box.get_center())
            boxes.add(VGroup(box, text))
 
        boxes.arrange(RIGHT, buff=0.9).shift(DOWN * 0.3)
 
        arrows = VGroup(
            *[
                Arrow(boxes[i].get_right(), boxes[i + 1].get_left(), buff=0.1)
                for i in range(len(boxes) - 1)
            ]
        )

        # cue 14: "...had probably had an understanding of finite different
        # mod[el]."
        self.sync(CUES[14].start)
        self.play(FadeIn(boxes[0]))
        # cue 16: "Next we can get into finite volume..."
        self.sync(CUES[16].start)
        self.play(GrowArrow(arrows[0]), FadeIn(boxes[1]))
        # cue 17: "Then we come to continuous finite element or glurkin."
        self.sync(CUES[17].start)
        self.play(GrowArrow(arrows[1]), FadeIn(boxes[2]))
        # cue 20: "What separates DG from CG is that these polynomials are
        # discontinuous."
        self.sync(CUES[20].start)
        self.play(GrowArrow(arrows[2]), FadeIn(boxes[3]))
        self.wait(0.5)
 
        cap_group = VGroup()
        for i, cap in enumerate(captions):
            c = Text(cap, font_size=16, color=GREY_B).next_to(
                boxes[i], DOWN, buff=0.3
            )
            cap_group.add(c)
        # cue 21: "exactly as you would expect given the name."
        self.sync(CUES[21].start)
        self.play(*[FadeIn(c, shift=UP * 0.2) for c in cap_group], run_time=1.2)
        self.wait(1)
 
        # Highlight DG as the destination
        self.play(boxes[-1][0].animate.set_color(YELLOW))
        note = Text(
            "Discontinuities let the scheme represent\n"
            "genuinely discontinuous physics (shocks!)",
            font_size=22,
            color=YELLOW,
        ).next_to(boxes, DOWN, buff=1.6)
        # cue 23: "And a great example would be an explosion."
        self.sync(CUES[23].start)
        self.play(FadeIn(note))
        self.wait(1.5)
        self.play(FadeOut(boxes, arrows, cap_group, note))

        grid = VGroup(
            *[
                Square(side_length=1.0, color=BLUE_D)
                for _ in range(9)
            ]
        ).arrange_in_grid(rows=3, cols=3, buff=0.0)
        grid.scale(1.1).shift(LEFT * 3.2)
 
        omega_label = MathTex(r"\Omega = \bigcup_e \Omega_e", font_size=36)
        omega_label.next_to(grid, DOWN, buff=0.4)

        # cue 26: "Essentially, we divide the space into elements."
        self.sync(CUES[26].start)
        self.play(Create(grid))
        self.play(Write(omega_label))
 
        approx = MathTex(
            r"u_h(x,t) = \sum_{j=0}^N u_j(t)\, \phi_j(x)", font_size=32
        ).next_to(grid, RIGHT, buff=1.0).shift(UP * 1.5)
        # cue 27: "We approximate the solution to the differential equation
        # using polynomials."
        self.sync(CUES[27].start)
        self.play(Write(approx))
 
        strong_form = MathTex(
            r"\frac{\partial u}{\partial t} + \nabla \cdot \mathbf{F} = S(u)",
            font_size=34,
        ).move_to(approx.get_center() + DOWN * 1.2)
        # cue 28: "Next, we write the partial differential equation in its
        # weak form."
        self.sync(CUES[28].start)
        self.play(Write(strong_form))
 
        multiply_note = Text(
            "multiply by test function φ, integrate over Ω,\n"
            "integrate the flux term by parts",
            font_size=20,
            color=GREY_B,
        ).move_to(strong_form.get_center() + DOWN * 1.2 + RIGHT * 0.2)
        # cue 29: "multiply the test function phi, then integrate by parts."
        self.sync(CUES[29].start)
        self.play(FadeIn(multiply_note))
 
        weak_form = MathTex(
            r"\int_\Omega \phi\, \frac{\partial u}{\partial t}\, dV"
            r"-\int_\Omega \nabla\phi \cdot \mathbf{F}\, dV"
            r"+\int_{\partial\Omega} \phi\, \mathbf{F}\cdot\mathbf{n}\, ds"
            r"= \int_\Omega \phi\, S(u)\, dV",
            font_size=26,
        )
        weak_form.move_to(ORIGIN)
        clamp_width(weak_form)

        # cue 30: "They communicate between elements via flux..."
        self.sync(CUES[30].start)
        self.play(
            ReplacementTransform(VGroup(strong_form, multiply_note, approx, grid, omega_label), weak_form),
        )
        self.wait(1.5)
 
        box = SurroundingRectangle(weak_form, color=YELLOW, buff=0.15)
        self.play(Create(box))
        self.wait(1.5)
 
        self.play(
            FadeOut(weak_form), FadeOut(box), FadeOut(header), 
        )
        r_total = MathTex(
            r"\mathbf{R}(\mathbf{U}) = \mathbf{R}_{\text{vol}}"
            r"+ \mathbf{R}_{\text{surf}} + \mathbf{R}_{\text{src}}",
            font_size=36,
        ).shift(UP * 2.0)
        # cue 32: "We rearrange it all the way until we get this equation
        # here."
        self.sync(CUES[32].start)
        self.play(Write(r_total))
        self.wait(0.5)
 
        r_vol = MathTex(
            r"\mathbf{R}_{\text{vol}} = \int_K \nabla\boldsymbol{\phi}"
            r"\cdot \mathbf{F}(\mathbf{u}_h)\, dV",
            font_size=30, color=BLUE,
        )
        r_surf = MathTex(
            r"\mathbf{R}_{\text{surf}} = -\int_{\partial K} "
            r"\boldsymbol{\phi}\, \mathbf{F}^{*}\, dS",
            font_size=30, color=ORANGE,
        )
        r_src = MathTex(
            r"\mathbf{R}_{\text{src}} = \int_K \boldsymbol{\phi}\,"
            r"\mathbf{S}(\mathbf{u}_h)\, dV",
            font_size=30, color=GREEN,
        )
 
        terms = VGroup(r_vol, r_surf, r_src).arrange(DOWN, buff=0.5)
        terms.next_to(r_total, DOWN, buff=0.8)
 
        for term in terms:
            self.play(Write(term))
            self.wait(0.3)
 
        self.wait(0.5)
 
        ode = MathTex(
            r"M \frac{d\mathbf{U}}{dt} = \mathbf{R}(\mathbf{U})",
            font_size=40,
        ).to_edge(DOWN, buff=0.7)
        box = SurroundingRectangle(ode, color=YELLOW, buff=0.2)

        # cue 35: "...the derivative of the function as a function of time
        # can be derived via this equation right here that's extremely
        # simple."
        self.sync(CUES[35].start)
        self.play(Write(ode))
        self.play(Create(box))

        # Hold through cues 36-38 (209.46 - 229.417): "...it might seem
        # fairly confusing... I am allowing this brevity as I will take a
        # concrete example..." -- no new visual for this bridge into S3.
        self.sync(CUES[39].start)
 
        self.play(
            FadeOut(header), FadeOut(r_total), FadeOut(terms),
            FadeOut(ode), FadeOut(box),
        )

            
# =============================================================================
# S3 -- Physical Model
# Narration: cues 39-46 (230.378 - 285.166): the Euler-Poisson system.
# =============================================================================
class S3(SyncedScene):
    AUDIO_START = CUES[39].start  # 230.378

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(70, seed=3))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        # cue 39: "As mentioned earlier, we are going to take the Euler
        # Poisson"
        self.sync(CUES[39].start)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Physical Model", PURPLE), ("}", FG_DIM)],
                            font_size=28)
        place_header(header)
        self.play(Write(header))

        U = MathTex(
            r"\mathbf{U} = \begin{bmatrix}\rho \\ \rho\mathbf{u} \\ E\end{bmatrix}",
            font_size=34,
        )
        F = MathTex(
            r"\mathbf{F}(\mathbf{U}) = \begin{bmatrix}"
            r"\rho\mathbf{u} \\ \rho\mathbf{u}\otimes\mathbf{u} + p\mathbf{I} \\"
            r"(E+p)\mathbf{u}\end{bmatrix}",
            font_size=34,
        )
        S = MathTex(
            r"\mathbf{S}(\mathbf{U}, \nabla\phi) = \begin{bmatrix}"
            r"0 \\ -\rho\nabla\phi \\ -\rho\mathbf{u}\cdot\nabla\phi"
            r"\end{bmatrix}",
            font_size=34,
        )
 
        row = VGroup(U, F, S).arrange(RIGHT, buff=0.9).shift(UP * 0.6)
        for item in row:
            self.play(Write(item), run_time=0.9)
        self.wait(1)
 
        system = MathTex(
            r"\mathbf{U}_t + \nabla\cdot\mathbf{F}(\mathbf{U}) = "
            r"\mathbf{S}(\mathbf{U},\nabla\phi), \qquad "
            r"\Delta\phi = 4\pi G \rho",
            font_size=32,
        ).next_to(row, DOWN, buff=1.0)
        clamp_width(system)
        # cue 42: "Using the general Euler Poisson equation, we can express
        # it as such."
        self.sync(CUES[42].start)
        self.play(Write(system))

        # Hold through cues 43-46 (256.061 - 285.166): the polytropic /
        # Lane-Emden mention and "don't worry about the exact equations"
        # aside -- no dedicated visual for either here -- up to cue 47,
        # where S4 picks up ("assuming a uniform square cubic meshing").
        self.sync(CUES[47].start)
 
        self.play(
            FadeOut(header), FadeOut(row),
            FadeOut(system), 
        )


        # ============================================================================
# S4 -- L2 Projection
# =============================================================================
def _assumption_icons():
    """Six small glyphs, one per modeling assumption, sized to sit quietly
    to the right of each checklist row."""
    icons = []
    # 1. the observable universe, treated as a sphere
    icons.append(Circle(radius=0.16, color=BLUE, fill_color=BLUE, fill_opacity=0.35, stroke_width=1.5))
    # 2. flat spacetime -> stacked parallel lines
    lines = VGroup(*[Line(LEFT * 0.18, RIGHT * 0.18, color=TEAL, stroke_width=2) for _ in range(3)])
    lines.arrange(DOWN, buff=0.07)
    icons.append(lines)
    # 3. constant dark energy -> a flat dashed line
    icons.append(DashedLine(LEFT * 0.2, RIGHT * 0.2, color=ORANGE, stroke_width=2, dash_length=0.06))
    # 4. two competing dark-matter estimates -> two dots
    dots = VGroup(Dot(radius=0.05, color=BLUE), Dot(radius=0.05, color=RED)).arrange(RIGHT, buff=0.1)
    icons.append(dots)
    # 5. isotropic density -> an even grid
    grid = VGroup(*[Dot(radius=0.03, color=FG_DIM) for _ in range(9)])
    grid.arrange_in_grid(rows=3, cols=3, buff=0.075)
    icons.append(grid)
    # 6. "magic" outside energy -> a star
    icons.append(Star(n=5, outer_radius=0.17, color=PURPLE, fill_color=PURPLE,
                       fill_opacity=0.7, stroke_width=1))
    return icons


class S4(SyncedScene):
    AUDIO_START = CUES[47].start  # 285.567

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        # cue 47: "To create the mesh and just the projection, we start by
        # assuming a uniform square cubic meshing."
        self.sync(CUES[47].start)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("L2 Projections", PURPLE), ("}", FG_DIM)], font_size=30)
        place_header(header)
        self.play(Write(header))

        axes = Axes(
            x_range=[-1, 1, 0.5], y_range=[-0.2, 1.4, 0.5],
            x_length=6, y_length=3.2,
            axis_config={"include_tip": False},
        ).shift(LEFT * 3.0 + DOWN * 0.3)
 
        smooth_curve = axes.plot(lambda x: 1 - x**2, color=BLUE)
        smooth_label = Text("true u(x)", font_size=20, color=BLUE).next_to(
            axes, UP, buff=0.15
        ).shift(LEFT * 1.2)
 
        # crude piecewise-linear "DG projection" of the same curve
        poly_approx = axes.plot(
            lambda x: 0.95 - 0.85 * x**2, color=YELLOW, x_range=[-1, 1]
        )
        poly_label = Text(
            "P(u)  (DG polynomial)", font_size=20, color=YELLOW
        ).next_to(smooth_label, DOWN, buff=0.15)

        # cue 49: "Next we define the projection, but to do this we define
        # our test problem." (holds through 48's "n+/n-" aside, unshown)
        self.sync(CUES[49].start)
        self.play(Create(axes))
        # cue 50: "Simply a weighting function within the polynomial
        # space..."
        self.sync(CUES[50].start)
        self.play(Create(smooth_curve), FadeIn(smooth_label))
        # cue 51: "Then we can define the projection matrix P via the L
        # squared projection."
        self.sync(CUES[51].start)
        self.play(Create(poly_approx), FadeIn(poly_label))
 
        eqs = VGroup(
            MathTex(r"\int_K u\, v\, dx = \int_K \mathbf{P}(u)\, v\, dx", font_size=28),
            MathTex(r"P(u)(x) = \sum_j c_j\, v_j(x)", font_size=28),
            MathTex(r"M_{ij} = \int_K \phi_i\, v_j\, dx, \qquad b_j = \int_K u\, v_j\, dx", font_size=26),
            MathTex(r"P(u)(x) = \sum_j \big(M^{-1}b\big)_j\, v_j(x)", font_size=28, color=YELLOW),
        ).arrange(DOWN, buff=0.35)
        eqs.next_to(axes, RIGHT, buff=0.8).shift(UP * 0.1)

        # eqs[0]: the L2-projection definition itself -- same cue 51 moment.
        self.play(Write(eqs[0]), run_time=0.8)
        # cue 52: "Now we can start moving backwards by rearranging the
        # equation to get that matrix P. Then we can define the mass
        # matrix..."
        self.sync(CUES[52].start)
        self.play(Write(eqs[1]), run_time=0.8)
        self.play(Write(eqs[2]), run_time=0.8)
        # cue 55: "We can then rearrange our original projection function to
        # get this new, more easily computable projection function..."
        self.sync(CUES[55].start)
        self.play(Write(eqs[3]), run_time=0.8)

        # cue 56: "Next, we get to the flux..." -- S5's opening line.
        self.sync(CUES[56].start)
 
        self.play(
            FadeOut(header), FadeOut(axes), FadeOut(smooth_curve),
            FadeOut(poly_approx), FadeOut(smooth_label), FadeOut(poly_label),
            FadeOut(eqs),
        )

# =============================================================================
# S5 -- HLLC Flux
# Narration: cues 56-61 (376.576 - 440.061).
# =============================================================================
class S5(SyncedScene):
    AUDIO_START = CUES[56].start  # 376.576

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)

        # cue 56: "Next, we get to the flux, where we create a piecewise
        # function for the flux of the fluid."
        self.sync(CUES[56].start)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("HLLC Flux", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))

        origin = ORIGIN + DOWN * 1.5
 
        line_L = Line(origin, origin + UP * 3 + LEFT * 3.3, color=BLUE)
        line_star = Line(origin, origin + UP * 3 + LEFT * 0.4, color=YELLOW)
        line_R = Line(origin, origin + UP * 3 + RIGHT * 3.3, color=RED)
 
        label_L = Text("U_L", font_size=24, color=BLUE).move_to(origin + UP * 1.0 + LEFT * 2.6)
        label_starL = Text("U*_L", font_size=22, color=YELLOW).move_to(origin + UP * 1.6 + LEFT * 0.9)
        label_starR = Text("U*_R", font_size=22, color=YELLOW).move_to(origin + UP * 1.6 + RIGHT * 0.5)
        label_R = Text("U_R", font_size=24, color=RED).move_to(origin + UP * 1.0 + RIGHT * 2.6)
 
        speed_labels = VGroup(
            MathTex("S_L", font_size=26, color=BLUE).next_to(line_L.get_end(), UP),
            MathTex("S_\\star", font_size=26, color=YELLOW).next_to(line_star.get_end(), UP),
            MathTex("S_R", font_size=26, color=RED).next_to(line_R.get_end(), UP),
        )

        # cue 57: "Here we have the standard HLLC, or Harten-Lotz von
        # Leer-Contek flux solver."
        self.sync(CUES[57].start)
        self.play(Create(line_L), Create(line_star), Create(line_R))
        self.play(
            FadeIn(label_L), FadeIn(label_starL),
            FadeIn(label_starR), FadeIn(label_R),
            FadeIn(speed_labels),
        )
 
        formula = MathTex(
            r"""
            F^{hllc}(U_L,U_R;\mathbf{n}) =
            \begin{cases}
            \mathbf{F}(U_L), & 0 \leq S_L \\
            \mathbf{F}_{\star L}, & S_L \leq 0 \leq S_\star \\
            \mathbf{F}_{\star R}, & S_\star \leq 0 \leq S_R \\
            \mathbf{F}(U_R), & 0 \geq S_R
            \end{cases}
            """,
            font_size=28,
        ).to_edge(DOWN, buff=0.4)
        clamp_width(formula)
        # cue 58: "...it approximates the Riemannian problem to approximate
        # the flux of conserved quantities like energy, momentum, mass..."
        self.sync(CUES[58].start)
        self.play(Write(formula))
 
        note = Text(
            "at rest, equal pressure on both sides:\n"
            "collapses to just the pressure pushing on the face",
            font_size=20, color=GREY_B,
        ).next_to(formula, UP, buff=0.25)
        # cue 60: "...I want to build the plain scheme first so that I will
        # show you exactly how it fails near equilibrium later."
        self.sync(CUES[60].start)
        self.play(FadeIn(note))

        # Hold to cue 62 (440.722), "So we write out our conserved
        # variables, flux, and source terms the way we did generally
        # before." -- S6's opening line.
        self.sync(CUES[62].start)
 
        self.play(
            FadeOut(header), FadeOut(line_L), FadeOut(line_star),
            FadeOut(line_R), FadeOut(label_L), FadeOut(label_starL),
            FadeOut(label_starR), FadeOut(label_R), FadeOut(speed_labels),
            FadeOut(formula), FadeOut(note),
        )

        


# =============================================================================
# S6 -- Standard LDG
# Narration: cues 62-109 (440.722 - 864.258): re-stating the system, why
# local DG, the auxiliary variable, the block system, and the Schur
# complement / Cholesky factorization.
# =============================================================================
class S6(SyncedScene):
    AUDIO_START = CUES[62].start  # 440.722
 
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
 
        # cue 62: "So we write out our conserved variables, flux, and
        # source terms the way we did generally before."
        self.sync(CUES[62].start)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Standard LDG", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))
 
        # cue 63-65: "...specify for the self-gravitating Euler system in
        # d spatial dimensions... energy [is] the usual total
        # non-gravitational energy... compress it down to this much
        # simpler equation where phi is the gravitational potential and
        # g [G] is the gravitational constant."
        self.sync(CUES[63].start)
        euler_system = MathTex(
            r"""
            \mathbf{U} = \begin{bmatrix}\rho \\ \rho\mathbf{u} \\ E\end{bmatrix}, \quad
            \mathbf{F}(\mathbf{U}) = \begin{bmatrix}\rho\mathbf{u} \\ \rho\mathbf{u}\otimes\mathbf{u}+p\mathbf{I} \\ (E+p)\mathbf{u}\end{bmatrix}, \quad
            \mathbf{S} = \begin{bmatrix}0 \\ -\rho\nabla\phi \\ -\rho\mathbf{u}\cdot\nabla\phi\end{bmatrix},
            \quad \nabla^2\phi = 4\pi G \rho
            """,
            font_size=22,
        ).next_to(header, DOWN, buff=0.9, aligned_edge=LEFT)
        clamp_width(euler_system)
        self.play(Write(euler_system))
 
        # cue 66-69: "...Poisson's equation is the entire reason this
        # problem is harder than ordinary Euler[']s [solve]. It isn't
        # fixed, but rather generated by [the] fluid. It changes shape
        # every time the density field changes. [Every] time step we need
        # to re-derive a self-consistent gravitational field..."
        self.sync(CUES[66].start)
        self.play(euler_system.animate.scale(0.8).to_corner(UL, buff=2.0))
        loop_rho = MathTex(r"\rho", font_size=34, color=BLUE)
        loop_phi = MathTex(r"\phi", font_size=34, color=ORANGE)
        loop_grp = VGroup(loop_rho, loop_phi).arrange(RIGHT, buff=3.0).shift(DOWN * 0.3 + RIGHT * 0.5)
        fwd = Arrow(loop_rho.get_top(), loop_phi.get_top(), buff=0.3, path_arc=-1.4)
        fwd_label = MathTex(r"\nabla^2\phi = 4\pi G\rho", font_size=20).next_to(fwd, UP, buff=0.1)
        back = Arrow(loop_phi.get_bottom(), loop_rho.get_bottom(), buff=0.3, path_arc=-1.4)
        back_label = Text("re-solved every timestep", font_size=16, color=GREY_B).next_to(back, DOWN, buff=0.1)
        self.play(FadeIn(loop_rho), FadeIn(loop_phi))
        self.play(GrowArrow(fwd), FadeIn(fwd_label))
        self.play(GrowArrow(back), FadeIn(back_label))
        gen_note = Text(
            "\u03c6 isn't an external, fixed field -- it's generated\n"
            "by the fluid itself, and changes shape every\n"
            "time the density does",
            font_size=18, color=GREY_B,
        ).next_to(loop_grp, DOWN, buff=1.0)
        self.play(FadeIn(gen_note))
 
        # cue 70-72: "Now for why we look at local DG specifically. There
        # are two broad strategies existing for handling gravity terms
        # numerically. One is to treat the Poisson equation as a pseudo
        # hyperbolic problem[,] given an artificial wave speed and a
        # relaxing time so that the same Riemannian solver... can be
        # reused... in the gravity sector as well."
        self.sync(CUES[70].start)
        self.play(
            FadeOut(loop_rho), FadeOut(loop_phi), FadeOut(fwd), FadeOut(fwd_label),
            FadeOut(back), FadeOut(back_label), FadeOut(gen_note),
        )
        strat_L = VGroup(
            RoundedRectangle(corner_radius=0.15, width=4.6, height=1.7, color=BLUE),
            Text(
                "Pseudo-hyperbolic\n"
                "artificial wave speed + relaxation time;\n"
                "reuses the same Riemann-solver machinery\n"
                "built for the Euler equations",
                font_size=16, line_spacing=1.0,
            ),
        )
        strat_L[1].move_to(strat_L[0].get_center()) 
        strat_L.shift(RIGHT * 1.1)
        # cue 73-75: "The other, which is the one this scheme takes, is to
        # solve the elliptic Poisson equation directly. Directly solving
        # it used to be avoided... [meant] either an FFT-based Poisson
        # solver, which is fast but restricted to very particular domain
        # shapes, or a multipole expansion[:] flexible but extremely
        # expensive and awkward to make higher order."
        strat_R = VGroup(
            RoundedRectangle(corner_radius=0.15, width=4.6, height=1.7, color=YELLOW),
            Text(
                "Elliptic (this scheme)\n"
                "solve \u2207\u00b2\u03c6=4\u03c0G\u03c1 directly; historically:\n"
                "FFT (fast, but restricted domain shapes) or\n"
                "multipole (flexible, but expensive/awkward at\n"
                "higher order)",
                font_size=16, line_spacing=1.0,
            ),
        )
        strat_R[1].move_to(strat_R[0].get_center())
        strategies = VGroup(strat_L, strat_R).arrange(RIGHT, buff=0.6).next_to(
            header, DOWN, buff=0.9
        )
        self.play(FadeIn(strat_L))
        self.sync(CUES[73].start)
        self.play(FadeIn(strat_R))
 
        # cue 76-79: "Local DG sidesteps both of these complaints. It was
        # originally invented specifically to let DG methods handle
        # equations with second derivatives without ever forming a second
        # derivative explicitly. [It] inherits all of DG's machinery[,]
        # flexibility[,] and boundary connections[,] as well as geometry
        # for free."
        self.sync(CUES[76].start)
        self.play(strat_R[0].animate.set_color(GREEN))
        ldg_note = Text(
            "Local DG sidesteps both complaints: built to let DG\n"
            "handle second derivatives without ever forming one\n"
            "explicitly -- for free, it inherits DG's usual machinery,\n"
            "flexibility, and geometry",
            font_size=18, color=GREEN,
        ).next_to(strategies, DOWN, buff=0.5)
        self.play(FadeIn(ldg_note))
 
        # cue 80-82: "...how do we get phi out of [a] DG method where
        # Poisson's equation has a second derivative in it? DG's whole
        # weak-form machinery is built assuming one derivative per
        # equation. That is exactly the 'local' in local DG."
        self.sync(CUES[80].start)
        self.play(FadeOut(strat_L), FadeOut(strat_R), FadeOut(ldg_note), FadeOut(euler_system))
        one_deriv_note = Text(
            "DG's weak form assumes one derivative per equation --\n"
            "so split the 2nd-order Poisson equation into two\n"
            "coupled 1st-order equations. That's the \"local\" in\n"
            "local DG.",
            font_size=20, color=GREY_B,
        ).next_to(header, DOWN, buff=0.9, aligned_edge=LEFT)
        self.play(FadeIn(one_deriv_note))
 
        # cue 83: "Following Cockburn and Shu's original construction, we
        # introduce an auxiliary variable standing in for the
        # gravitational force itself."
        self.sync(CUES[83].start)
        self.play(FadeOut(one_deriv_note))
 
        aux = MathTex(r"\mathbf{g} = \nabla\phi", font_size=36, color=ORANGE)
        aux_note = Text(
            "an auxiliary variable so no equation\never needs a second derivative",
            font_size=20, color=GREY_B,
        ).next_to(aux, DOWN, buff=0.3)
        self.play(Write(aux), FadeIn(aux_note))
        self.wait(1)
 
        self.play(FadeOut(aux_note))
 
        system = MathTex(
            r"""
            \begin{cases}
            \mathbf{U}_t + \nabla\cdot\mathbf{F}(\mathbf{U}) = \mathbf{S}(\mathbf{U},\mathbf{g}) \\
            \mathbf{g} = \nabla\phi \\
            \nabla\cdot\mathbf{g} = 4\pi G \rho
            \end{cases}
            """,
            font_size=32,
        )
        clamp_width(system, max_width=CONTENT_MAX_WIDTH * 0.6)
        # cue 84: "...rewrite the whole system as a genuinely first order
        # system, no second derivatives anywhere."
        self.sync(CUES[84].start)
        self.play(ReplacementTransform(aux, system))
        self.wait(1.5)
 
        self.play(system.animate.to_edge(LEFT, buff=1.0).shift(UP * 0.3))
 
        block = MathTex(
            r"""
            \begin{bmatrix} K & B \\ -B^T & S \end{bmatrix}
            \begin{bmatrix} \mathbf{g}_h \\ \phi_h \end{bmatrix}
            =
            \begin{bmatrix} g \\ f \end{bmatrix}
            """,
            font_size=30,
        ).next_to(system, RIGHT, buff=1.0)
        clamp_width(block, max_width=CONTENT_MAX_WIDTH * 0.4)
        # Hold through cues 86-103 (629.765 - 807.28): mesh/face notation,
        # averages and jumps, the mixed LDG weak forms, penalty parameters
        # -- all unshown -- to cue 104: "...it has a block saddle point
        # structure by this equation below..."
        self.sync(CUES[104].start)
        self.play(Write(block))
 
        arrow = MathTex(r"\Longrightarrow \text{(Schur complement)}", font_size=26).next_to(
            block, DOWN, buff=0.4
        )
        schur = MathTex(
            r"A\phi_h = \big(S + B^T K^{-1} B\big)\phi_h = f + B^T K^{-1} g",
            font_size=26, color=YELLOW,
        ).next_to(arrow, DOWN, buff=0.3)
        clamp_width(schur)
 
        # cue 105: "The standard move is a sure [Schur] complement."
        self.sync(CUES[105].start)
        self.play(FadeIn(arrow))
        # cue 106: "Eliminate g algebraically to get a smaller system
        # purely in phi in this equation."
        self.sync(CUES[106].start)
        self.play(Write(schur))
 
        note = Text(
            "A is symmetric positive definite:\n"
            "factor once, reuse it every timestep",
            font_size=22, color=YELLOW,
        ).next_to(schur, DOWN, buff=0.5)
        # cue 107: "And now a is symmetric and positively definite." (runs
        # through cue 109's "...computed exactly once... and reused at
        # every single step afterwards.")
        self.sync(CUES[107].start)
        self.play(FadeIn(note))
 
        # Hold through cues 110-116 (865.239 - 919.436): sparsity pattern,
        # the periodic/LU variant, and the L2-projection initial-conditions
        # recap -- no dedicated visual -- to cue 117, where the scheme is
        # wrapped in forward Euler.
        self.sync(CUES[117].start)
        self.play(
            FadeOut(system), FadeOut(block), FadeOut(arrow),
            FadeOut(schur), FadeOut(note),
        )
 
        loop_boxes = VGroup(
            *[
                RoundedRectangle(corner_radius=0.12, width=3.0, height=0.9, color=BLUE)
                for _ in range(3)
            ]
        )
        loop_texts = [
            Text("ρ, ρu, E\nupdated", font_size=18),
            Text("g, φ re-derived\nfrom new ρ", font_size=18),
            Text("next\ntimestep", font_size=18),
        ]
        for box, txt in zip(loop_boxes, loop_texts):
            txt.move_to(box.get_center())
        loop_group = VGroup(*[VGroup(b, t) for b, t in zip(loop_boxes, loop_texts)])
        loop_group.arrange(RIGHT, buff=0.9)
        loop_arrows = VGroup(
            *[Arrow(loop_group[i].get_right(), loop_group[i + 1].get_left(), buff=0.08)
              for i in range(2)]
        )
        loop_back = CurvedArrow(
            loop_group[2].get_bottom() + UP * 1,
            loop_group[0].get_bottom() + UP * 1,
            angle=TAU / 3, color=YELLOW,
        )
 
        # cue 117: "Wrapping the whole thing in a forward Euler step in
        # time will get a complete working numerical scheme as stated
        # below."
        self.play(FadeIn(loop_group[0]))
        # cue 118-119: "Density, momentum, and energy evolve... gravity is
        # re-derived from the freshly updated density..."
        self.sync(CUES[119].start)
        self.play(GrowArrow(loop_arrows[0]), FadeIn(loop_group[1]))
        # cue 120: "The loop repeats."
        self.sync(CUES[120].start)
        self.play(GrowArrow(loop_arrows[1]), FadeIn(loop_group[2]))
        self.play(Create(loop_back))
 
        standard_note = Text(
            "\"the standard scheme\": real, high-order accurate --\n"
            "but nothing stops negative energy/pressure or an\n"
            "unphysical drift away from equilibrium",
            font_size=20, color=GREY_B,
        ).next_to(loop_group, DOWN, buff=0.8)
        # cue 124: "...running it smooth manufactures solutions to where
        # things don't simply work... which is why we are going to add
        # enforced positivity and well-balance."
        self.sync(CUES[124].start)
        self.play(FadeIn(standard_note))
 
        # cue 125: S9's opening line about hydrostatic equilibrium.
        self.sync(CUES[125].start)
 
        self.play(
            FadeOut(header), FadeOut(loop_group), FadeOut(loop_arrows),
            FadeOut(loop_back), FadeOut(standard_note),
        )
 
 


# =============================================================================
# S08_Questions -- GENERAL QUESTIONS
#
# NOTE: this scene's content ("If gravity moves at the speed of light...")
# does not appear anywhere in DG_CSSN.srt. It doesn't belong to this
# video's narration track -- left un-synced (plain Scene, no AUDIO_START)
# rather than given a fabricated timestamp. Confirm whether this should
# even be part of the DG/CCSN render before including it in render_all.sh.
# =============================================================================
class S08_Questions(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("92%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("General Questions", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))
        self.wait(4)

        q = code_line([("Q:  ", TEAL), ("If gravity moves at the speed of light,", FG)], font_size=24)
        q2 = Text("how does that change things?", font=FONT, font_size=24, color=FG)
        qgrp = VGroup(q, q2).arrange(DOWN, buff=0.15, aligned_edge=LEFT)
        fit_width(qgrp)
        qgrp.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.play(Write(qgrp), run_time=1.1)
        self.wait(4)

        a_lines = [
            "A:  Not by much.",
            "Redshift costs energy for waves -- not a",
            "constant field.",
            "The two bigger effects: the universe's",
            "radius shifts over time, and some of today's",
            "observable universe could cross the cosmic",
            "horizon, putting it forever out of reach.",
            "Those roughly cancel at these scales.",
            "(As always, I could be wrong.)",
        ]
        colors = [TEAL] + [FG] * 8 + [FG_DIM]
        a_group = VGroup(*[
            Text(l, font=FONT, font_size=21, color=c) for l, c in zip(a_lines, colors)
        ]).arrange(DOWN, buff=0.24, aligned_edge=LEFT)
        fit_width(a_group)
        a_group.next_to(qgrp, DOWN, buff=0.55, aligned_edge=LEFT)
        for l in a_group:
            self.play(FadeIn(l, shift=UP * 0.8), run_time= 0.45)
            self.wait(3)
        self.wait(14.5)

        self.play(FadeOut(header, qgrp, a_group))


# =============================================================================
# S9 -- Well-balanced flux trick (hydrostatic star, split, modified flux)
#
# Narration matched here spans cues 125-177 (967.403 - 1361.225), but with
# a large unshown gap in the middle -- see the KNOWN GAPS note at the top
# of this file: cues 128-157 (~988 - 1204, the polytropic/Lane-Emden
# equilibrium family, the standard scheme's failure mode, and the
# separate energy-conservation problem) have no visual here.
# =============================================================================
class S9(SyncedScene):
    AUDIO_START = CUES[125].start  # 967.403

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Structure LDG", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        # cue 125: "A huge fraction of astrophysically interesting
        # configurations spend most of their time sitting extremely close
        # to hydrostatic equilibrium."
        self.sync(CUES[125].start)
        self.play(Write(header))
        # simple equilibrium "star" picture
        star = Circle(radius=1.2, color=BLUE_D, fill_opacity=0.25).shift(LEFT * 3.3)
        out_arrows = VGroup(
            *[
                Arrow(
                    star.get_center(),
                    star.get_center() + 1.9 * np.array([np.cos(a), np.sin(a), 0]),
                    color=ORANGE, buff=1.2, stroke_width=3,
                )
                for a in np.linspace(0, 2 * PI, 8, endpoint=False)
            ]
        )
        pressure_label = Text("pressure ∇p", font_size=18, color=ORANGE).next_to(
            star, UP, buff=1.4
        )
        in_arrows = VGroup(
            *[
                Arrow(
                    star.get_center() + 1.9 * np.array([np.cos(a), np.sin(a), 0]),
                    star.get_center() + 1.25 * np.array([np.cos(a), np.sin(a), 0]),
                    color=PURPLE, buff=0.0, stroke_width=3,
                )
                for a in np.linspace(0, 2 * PI, 8, endpoint=False)
            ]
        )
        gravity_label = Text("gravity ρ∇φ", font_size=18, color=PURPLE).next_to(
            star, DOWN, buff=1.4
        )
 
        self.play(Create(star))
        # cue 126: "Gravity pulling material inward and pressure pushing
        # back outward."
        self.sync(CUES[126].start)
        self.play(*[GrowArrow(a) for a in out_arrows])
        self.play(*[GrowArrow(a) for a in in_arrows], FadeIn(gravity_label))
 
        balance_eq = MathTex(
            r"\nabla p^e = -\rho^e \nabla\phi^e", font_size=28
        ).next_to(star, DOWN, buff=2.0)
        # cue 127: "Velocity remaining essentially zero and the two forces
        # balance out to enormous precision."
        self.sync(CUES[127].start)
        self.play(Write(balance_eq))

        # cue 129: "This can be commonly and analytically traced to the
        # family of equilibria, which is the polytropic equilibrium."
        self.sync(CUES[129].start)
        polytrope = MathTex(r"p = K \rho^\gamma", font_size=30).to_edge(
            RIGHT, buff=1.2
        ).shift(UP * 2.4)
        clamp_width(polytrope, max_width=CONTENT_MAX_WIDTH * 0.4)
        self.play(Write(polytrope))

        # cue 131-133: "...substitute this and the hydrostatic balance
        # into the Poisson equation and we get the classic Lane-Emden
        # [equation]... in spherical symmetry, with xi the rescaled
        # radius, it reads as this very simple differential equation."
        self.sync(CUES[131].start)
        lane_emden = MathTex(
            r"\frac{1}{\xi^2}\frac{d}{d\xi}\!\left(\xi^2 \frac{d\theta}{d\xi}\right) = -\theta^n,"
            r"\qquad \rho = \lambda\theta^n,\ \ \gamma = \frac{n+1}{n}",
            font_size=20,
        ).next_to(polytrope, DOWN, buff=0.4)
        clamp_width(lane_emden, max_width=CONTENT_MAX_WIDTH * 0.42)
        self.play(Write(lane_emden))

        # cue 134-136: "...a closed form solution only for a few special
        # integer values... the full equilibrium profile is recovered
        # from these equations below... this is the analytic star in a
        # box that our scheme will be using to hold it perfectly still."
        # Rather than just asserting that, integrate the n=3 case (the
        # classic "Eddington standard model") and plot the actual profile.
        self.sync(CUES[134].start)
        xi_vals, theta_vals = lane_emden_profile(n=3, xi_max=8.0, h=0.01)
        le_axes = Axes(
            x_range=[0, 7, 7], y_range=[0, 1, 1],
            x_length=3.2, y_length=1.9,
            axis_config={"include_tip": False, "font_size": 14},
        ).next_to(lane_emden, DOWN, buff=0.4)
        le_curve = le_axes.plot_line_graph(
            x_values=xi_vals, y_values=theta_vals,
            line_color=YELLOW, add_vertex_dots=False, stroke_width=3,
        )
        le_caption = Text(
            "\u03b8(\u03be), n=3 -- solved once, up front", font_size=15, color=GREY_B
        ).next_to(le_axes, DOWN, buff=0.15)
        self.play(Create(le_axes))
        self.play(Create(le_curve["line_graph"]), FadeIn(le_caption))

        # cue 137: "The actual failure mode [stated] precisely."
        self.sync(CUES[137].start)
        self.play(
            FadeOut(polytrope), FadeOut(lane_emden), FadeOut(le_axes),
            FadeOut(le_curve), FadeOut(le_caption),
        )

        fail_L = Text("flux term\n(surface + volume\nDG integral)", font_size=18, color=BLUE)
        fail_R = Text("source term\n(volume DG\nintegral)", font_size=18, color=ORANGE)
        fail_group = VGroup(fail_L, fail_R).arrange(RIGHT, buff=1.0).to_edge(
            RIGHT, buff=0.5
        ).shift(UP * 1.6)
        fail_ne = MathTex(r"\neq", font_size=36).move_to(
            (fail_L.get_center() + fail_R.get_center()) / 2
        )
        # cue 138-141: "...discretized...using two completely independent
        # numerical procedures... there is no algebraic reason [they]
        # should cancel out exactly."
        self.sync(CUES[138].start)
        self.play(FadeIn(fail_L), FadeIn(fail_R))
        self.play(Write(fail_ne))

        fail_err = Text(
            "leftover O(h^{k+1}) error sits\nright on the equilibrium",
            font_size=20, color=RED,
        ).next_to(fail_group, DOWN, buff=0.5)
        # cue 143: "What's left is [an] O(h^{k+1}) discretization error,
        # setting right on the top of the equilibrium at every single
        # time step."
        self.sync(CUES[143].start)
        self.play(Write(fail_err))

        wb_label = Text(
            "-> the \"well-balanced\" problem", font_size=22, color=YELLOW
        ).next_to(fail_err, DOWN, buff=0.4)
        # cue 147: "This is the well-balanced problem, and a scheme that
        # avoids that is called well-balanced."
        self.sync(CUES[147].start)
        self.play(FadeIn(wb_label))

        # cue 149: "Total energy conservation." (the second failure)
        self.sync(CUES[149].start)
        self.play(
            FadeOut(fail_L), FadeOut(fail_R), FadeOut(fail_ne),
            FadeOut(fail_err), FadeOut(wb_label),
        )

        pe_bar = Rectangle(width=1.1, height=2.1, color=PURPLE, fill_opacity=0.6).to_edge(
            RIGHT, buff=5.0
        ).shift(UP * 0.3)
        ke_bar = Rectangle(width=1.1, height=1.9, color=BLUE, fill_opacity=0.6).next_to(
            pe_bar, RIGHT, buff=0.5
        ).align_to(pe_bar, DOWN)
        pe_label = Text("PE\n(large,\nnegative)", font_size=15).next_to(pe_bar, UP, buff=0.15)
        ke_label = Text("KE + internal\n(large,\npositive)", font_size=15).next_to(ke_bar, UP, buff=0.15)
        leak = Rectangle(width=0.3, height=0.3, color=RED, fill_opacity=0.9).next_to(
            ke_bar, RIGHT, buff=0.3
        ).align_to(ke_bar, DOWN)
        leak_label = Text("numerical\nleak", font_size=13, color=RED).next_to(leak, UP, buff=0.1)
        # cue 151-152: "...the gravitational potential energy...typically a
        # large negative number, nearly [cancelling] a large positive
        # kinetic plus internal energy."
        self.sync(CUES[151].start)
        self.play(FadeIn(pe_bar), FadeIn(ke_bar), FadeIn(pe_label), FadeIn(ke_label))
        # cue 153-155: "Any smaller systematic energy leak...can be
        # comparable to or larger than the actual dynamical energy of the
        # perturbation you are trying to resolve..."
        self.sync(CUES[153].start)
        self.play(FadeIn(leak), FadeIn(leak_label))

        both_note = Text(
            "want both properties at once --\ntwo largely independent fixes",
            font_size=20, color=GREY_B,
        ).next_to(VGroup(pe_bar, ke_bar, leak), DOWN, buff=0.6)
        # cue 156-157: "We want both properties simultaneously. And it
        # turns out there are two largely independent fixes."
        self.sync(CUES[156].start)
        self.play(FadeIn(both_note))

        # cue 158: "The core idea is to split both density and
        # gravitational potential into a known static equilibrium piece,
        # and evolving perturbations..."
        self.sync(CUES[158].start)
        self.play(
            FadeOut(pe_bar), FadeOut(ke_bar), FadeOut(pe_label),
            FadeOut(ke_label), FadeOut(leak), FadeOut(leak_label),
            FadeOut(both_note),
        )
 
        split = MathTex(
            r"\rho = \rho^e + \rho^\delta, \qquad \phi = \phi^e + \phi^\delta",
            font_size=30,
        ).to_edge(RIGHT, buff=0.7).shift(UP * 1.7)
        split_note = Text(
            "frozen equilibrium\n+ evolving perturbation",
            font_size=18, color=GREY_B,
        ).next_to(split, DOWN, buff=0.2)
        self.play(Write(split), FadeIn(split_note))
        # cue 159: "...computed once at initialization from an analytical
        # equilibrium, and then frozen and unchanged for the entire
        # simulation."
        self.sync(CUES[159].start)
 
        modflux = MathTex(
            r"\widehat{\mathbf{F}} = \mathbf{F}^{hllc}\!\left("
            r"\frac{p_h^{e,\star}}{p_h^{e,\text{int}}}\mathbf{U}_h^{\text{int}},\ "
            r"\frac{p_h^{e,\star}}{p_h^{e,\text{ext}}}\mathbf{U}_h^{\text{ext}}\right)",
            font_size=24,
        ).next_to(split, DOWN, buff=1.0)
        # cue 169: "The fix is instead of feeding HLLC the raw conserved
        # states, we rescale each state by the ratio of the face average
        # equilibrium pressure to that side's own cell interior
        # equilibrium pressure..."
        self.sync(CUES[169].start)
        self.play(Write(modflux))
 
        result = Text(
            "at equilibrium: this flux and the modified\n"
            "source term cancel exactly, term for term",
            font_size=20, color=YELLOW,
        ).next_to(modflux, DOWN, buff=0.4)
        # cue 177: "So the two cancel at equilibrium."
        self.sync(CUES[177].start)
        self.play(FadeIn(result))

        # cue 178: S9_1 picks up here with the momentum-source algebra.
        self.sync(CUES[178].start)
 
        self.play(
            FadeOut(header), FadeOut(star), FadeOut(out_arrows),
            FadeOut(in_arrows), FadeOut(pressure_label), FadeOut(gravity_label),
            FadeOut(balance_eq), FadeOut(split), FadeOut(split_note),
            FadeOut(modflux), FadeOut(result),
        )


# =============================================================================
# S9_1 -- Well-balanced momentum source & total energy conservation
# Narration: cues 178-238 (1363.539 - 1826.378). This is a dense,
# fast-moving derivation in the narration (cues 180-235 alone cover ~7
# minutes of algebra); the beats below hit the equations the narration
# calls out explicitly and hold through the connecting steps in between,
# rather than trying to stage every intermediate line. Reconstructed from
# the spoken description (no equations for this section exist elsewhere
# in this file) -- check the exact forms against your derivation notes
# before treating them as final.
# =============================================================================
class S9_1(SyncedScene):
    AUDIO_START = CUES[178].start  # 1363.539

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Well-Balanced Source & Energy", PURPLE), ("}", FG_DIM)],
                            font_size=26)
        place_header(header)
        # cue 178: "On the momentum equation, the full source S is equal
        # to these equations."
        self.sync(CUES[178].start)
        self.play(Write(header))

        source_split = MathTex(
            r"\mathbf{S}_{\text{mom}} = \mathbf{S}_{\text{mom}}^{e} + \mathbf{S}_{\text{mom}}^{\delta}",
            font_size=32,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        clamp_width(source_split)
        self.play(Write(source_split))

        # cue 181-183: "...the exact equilibrium relation is the gradient
        # of pressure equal to minus density times the gradient of phi...
        # trade the gradient of phi for the gradient of pressure divided
        # by density, evaluated directly from the known equilibrium
        # profile."
        self.sync(CUES[181].start)
        eq_trade = MathTex(
            r"\nabla\phi^e = -\frac{\nabla p^e}{\rho^e} \quad\text{(known from the equilibrium profile)}",
            font_size=26,
        ).next_to(source_split, DOWN, buff=0.6, aligned_edge=LEFT)
        clamp_width(eq_trade)
        self.play(Write(eq_trade))

        # cue 184-188: "Add and subtract the cell-average density inside
        # the integral... the new term vanishes identically the instant
        # density equals the equilibrium density -- constant across the
        # cell, exactly what happens at equilibrium." (decluttering here:
        # drop the first two lines rather than stacking a third below them)
        self.sync(CUES[184].start)
        self.play(FadeOut(source_split), FadeOut(eq_trade))
        s_e = MathTex(
            r"\mathbf{S}_{\text{mom}}^{e} \sim -\int_K \boldsymbol{\phi}\,"
            r"\big(\rho - \langle\rho^e\rangle_K\big)\, \nabla\phi^e\, dV"
            r"\;+\;\text{(boundary flux term)}",
            font_size=24, color=YELLOW,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        clamp_width(s_e)
        self.play(Write(s_e))
        vanish_note = Text(
            "vanishes exactly when \u03c1 = \u03c1^e (equilibrium): this is what\n"
            "makes the discrete source term well-balanced",
            font_size=18, color=GREY_B,
        ).next_to(s_e, DOWN, buff=0.3, aligned_edge=LEFT)
        # cue 189-195: "...integrated by parts using the same weak-form
        # move that built DG in the first place... built term for term to
        # exactly cancel out the modified HLLC flux."
        self.sync(CUES[189].start)
        self.play(FadeIn(vanish_note))

        # cue 197-200: "...a conservative form of the ordinary
        # non-gravitational energy... the total energy is in conservative
        # form... we can add a new piece, the gravitational energy flux."
        self.sync(CUES[197].start)
        self.play(FadeOut(s_e), FadeOut(vanish_note))
        energy_form = MathTex(
            r"\partial_t E_{\text{total}} + \nabla\cdot\big(\mathbf{F}_E + \mathbf{F}_g\big) = 0,"
            r"\qquad E_{\text{total}} = E_{\text{non-grav}} + \rho\phi",
            font_size=28,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        clamp_width(energy_form)
        self.play(Write(energy_form))

        # cue 201-209: "...the derivation is short... every surviving term
        # cancels out in pairs, and the whole expression collapses to
        # exactly zero... you land on the conservative form above."
        self.sync(CUES[203].start)
        deriv_note = Text(
            "d/dt(\u03c1\u03c6) + \u2207\u00b7F_g = 0  --  built from mass conservation and\n"
            "\u2207\u00b2\u03c6 = 4\u03c0G\u03c1 (plus its time derivative); every term cancels in pairs",
            font_size=20, color=GREY_B,
        ).next_to(energy_form, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(deriv_note))

        # cue 214-218: "...an obvious wrinkle: F_g contains a time
        # derivative of phi... the way around this is [that] the time
        # derivative of phi itself satisfies another Poisson equation...
        # one additional elliptic problem every stage, [via] the exact
        # same LDG mixed formulation as before."
        self.sync(CUES[214].start)
        self.play(FadeOut(deriv_note))
        wrinkle = MathTex(
            r"\nabla^2 \dot\phi = 4\pi G\, \dot\rho \quad\Longrightarrow\quad"
            r"\text{one more LDG (Schur-complement) solve, per stage}",
            font_size=24, color=YELLOW,
        ).next_to(energy_form, DOWN, buff=0.5, aligned_edge=LEFT)
        clamp_width(wrinkle)
        self.play(Write(wrinkle))

        # cue 219-227: "...a full order of accuracy [lost] if you
        # differentiate the DG polynomial pointwise... reach for the DG
        # weak form instead, [applying] summation by parts... restores the
        # full design order of accuracy."
        self.sync(CUES[219].start)
        self.play(FadeOut(wrinkle))
        sbp_note = Text(
            "pointwise d/dx of a degree-k polynomial only reaches\n"
            "degree k-1: differentiate F_g via the weak form + summation\n"
            "by parts instead, to keep the full design order of accuracy",
            font_size=19, color=GREY_B,
        ).next_to(energy_form, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(sbp_note))

        # cue 231-235: "...at every stage you need ordinary energy to get
        # pressure from the equation of state... [use] the projection
        # matrix P... compute the product pointwise and reproject and the
        # well-balanced cancellation breaks down -- an easy
        # misimplementation I caught myself making the first time around."
        self.sync(CUES[231].start)
        self.play(FadeOut(sbp_note))
        gotcha = Text(
            "pitfall: recover pressure from P(\u03c1\u03c6), the L2 projection --\n"
            "not from \u03c1\u03c6 evaluated pointwise and reprojected, or the\n"
            "well-balanced cancellation quietly breaks down",
            font_size=19, color=RED,
        ).next_to(energy_form, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(gotcha))

        # cue 236-238: "Now that everything is written semi-discretely...
        # this schematic form... where H is shorthand for the entire right
        # side... the standard, non-structure-preserving scheme has an
        # analogous operator, which I'll call L."
        self.sync(CUES[236].start)
        self.play(FadeOut(gotcha), FadeOut(energy_form))
        schematic = MathTex(
            r"\frac{d\mathbf{U}}{dt} = \mathbf{H}(\mathbf{U})"
            r"\qquad\big(\text{vs. } \mathbf{L}(\mathbf{U}) \text{ for the standard, non-structure-preserving scheme}\big)",
            font_size=26,
        ).move_to(ORIGIN)
        clamp_width(schematic)
        self.play(Write(schematic))

        # cue 239: S9_2's opening line about forward Euler being only
        # first order.
        self.sync(CUES[239].start)
        self.play(FadeOut(header), FadeOut(schematic))


# =============================================================================
# S9_2 -- Time Discretization (SSP-RK)
# Narration: cues 239-248 (1827.548 - 1922.978).
# =============================================================================
class S9_2(SyncedScene):
    AUDIO_START = CUES[239].start  # 1827.548

    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Time Discretization", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        # cue 239: "Forward Euler, as written, is only first order
        # accurate in time, which would badly undercut all the care we
        # took into adding higher spatial order."
        self.sync(CUES[239].start)
        self.play(Write(header))

        fe_note = Text(
            "1st-order-in-time forward Euler would throw away\n"
            "the higher spatial order built up so far",
            font_size=22, color=GREY_B,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        self.play(FadeIn(fe_note))

        # cue 243: "...strongly stability-preserving Runge-Kutta, or
        # SSP-RK, specifically [because it's] a convex combination of
        # forward-Euler steps..."
        self.sync(CUES[243].start)
        self.play(FadeOut(fe_note))
        ssp_rk2 = MathTex(
            r"""
            \begin{aligned}
            \mathbf{U}^{(1)} &= \mathbf{U}^n + \Delta t\, \mathbf{H}(\mathbf{U}^n) \\
            \mathbf{U}^{n+1} &= \tfrac{1}{2}\mathbf{U}^n + \tfrac{1}{2}\Big(\mathbf{U}^{(1)} + \Delta t\, \mathbf{H}(\mathbf{U}^{(1)})\Big)
            \end{aligned}
            """,
            font_size=28,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        clamp_width(ssp_rk2)
        ssp_note = Text(
            "SSP-RK2 (a convex combination of forward-Euler steps):\n"
            "any stability/positivity proof for forward Euler carries\n"
            "over automatically -- no extra work needed",
            font_size=18, color=GREY_B,
        ).next_to(ssp_rk2, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(Write(ssp_rk2))
        self.play(FadeIn(ssp_note))

        # cue 245-247: "Each stage is literally just another forward-Euler
        # style evaluation of H... this reusability is one of the more
        # pleasant features... raising the order is purely a matter of
        # adding stages, with zero changes to the spatial operator."
        # (decluttering here: drop the SSP-RK2 equation itself rather than
        # stacking a third block below it -- the "same H(U)" note stands
        # on its own without the equation still on screen.)
        self.sync(CUES[245].start)
        self.play(FadeOut(ssp_rk2), FadeOut(ssp_note))
        reuse_note = Text(
            "each stage reuses the exact same H(U): the elliptic solve,\n"
            "modified-HLLC flux, and gravity-energy-flux pipeline\n"
            "-- unchanged. Higher order = more stages, same H.",
            font_size=20, color=YELLOW,
        ).next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)
        self.play(FadeIn(reuse_note))

        # cue 248: "Practically, the timestep can [be chosen] using the
        # traditional CFL condition based on the fastest wave speed
        # present anywhere."
        self.sync(CUES[248].start)
        cfl = MathTex(
            r"\Delta t \leq \text{CFL} \cdot \frac{h}{S_{\max}}",
            font_size=32, color=BLUE,
        ).next_to(reuse_note, DOWN, buff=0.5, aligned_edge=LEFT)
        clamp_width(cfl)
        self.play(Write(cfl))

        # cue 249: S10's opening line.
        self.sync(CUES[249].start)
        self.play(FadeOut(header), FadeOut(reuse_note), FadeOut(cfl))


# =============================================================================
# S10 -- Oscillation Elimination
# Narration: cues 249-278 (1925.41 - 2183.451).
# =============================================================================
class S10(SyncedScene):
    AUDIO_START = CUES[249].start  # 1925.41

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        # cue 249: "Higher order polynomials are wonderful at representing
        # smooth [functions], and generally terrible at representing
        # shock[s]."
        self.sync(CUES[249].start)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Oscilation Elimination", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))

        axes_left = Axes(
            x_range=[-1, 1, 0.5], y_range=[-0.5, 1.5, 0.5],
            x_length=5.5, y_length=3.0, axis_config={"include_tip": False},
        ).shift(LEFT * 3.3 + DOWN * 0.3)
        axes_right = Axes(
            x_range=[-1, 1, 0.5], y_range=[-0.5, 1.5, 0.5],
            x_length=5.5, y_length=3.0, axis_config={"include_tip": False},
        ).shift(RIGHT * 3.3 + DOWN * 0.3)
 
        # a step-like target and a ringing high-order polynomial approximation
        def step(x):
            return 1.0 if x > 0 else 0.0
 
        def ringing(x):
            return step(x) + 0.35 * np.sin(10 * x) * np.exp(-3 * x**2)
 
        def damped(x):
            return step(x) + 0.03 * np.sin(10 * x) * np.exp(-3 * x**2)
 
        target_L = axes_left.plot(step, color=GREY_B, discontinuities=[0])
        ring_curve = axes_left.plot(ringing, color=RED)
 
        target_R = axes_right.plot(step, color=GREY_B, discontinuities=[0])
        damped_curve = axes_right.plot(damped, color=GREEN)
 
        label_left = Text("before OE", font_size=22, color=RED).next_to(axes_left, UP)
        label_right = Text("after OE", font_size=22, color=GREEN).next_to(axes_right, UP)

        # cue 250: "Near a real discontinuity, the blast wave from our
        # earlier explosion..."
        self.sync(CUES[250].start)
        self.play(Create(axes_left), Create(axes_right))
        self.play(Create(target_L), Create(target_R))
        # cue 251: "...will cause the high degree DG polynomial to ring."
        self.sync(CUES[251].start)
        self.play(Create(ring_curve), FadeIn(label_left))
        # cue 255: "...the scheme reaches for a more recent idea, the
        # oscillation elimination technique..."
        self.sync(CUES[255].start)
        self.play(Create(damped_curve), FadeIn(label_right))
 
        formula = MathTex(
            r"\frac{d\mathbf{U}_\sigma}{dt} = -\mathbf{\Sigma}(\mathbf{U})\,\mathbf{U}_\sigma",
            font_size=28,
        ).to_edge(DOWN, buff=0.5)
        note = Text(
            "damps high moments near a jump, leaves the cell average untouched;\n"
            "applied only to the perturbation part, never the frozen equilibrium",
            font_size=18, color=GREY_B,
        ).next_to(formula, UP, buff=0.2)

        # cue 257: "...a purely local per-cell dampening term, its
        # coefficient built from a smoothness indicator..." (runs through
        # cue 278's positivity-limiter aside, none of it shown separately.)
        self.sync(CUES[257].start)
        self.play(Write(formula), FadeIn(note))

        # cue 279: "We now have our structure-preserving properties..." --
        # S11's opening line.
        self.sync(CUES[279].start)
 
        self.play(
            FadeOut(header), FadeOut(axes_left), FadeOut(axes_right),
            FadeOut(target_L), FadeOut(target_R), FadeOut(ring_curve),
            FadeOut(damped_curve), FadeOut(label_left), FadeOut(label_right),
            FadeOut(formula), FadeOut(note),
        )

#==============================================================================
# S11 -- Code (5-step diagram)
# Narration: cues 279-290 (2185.08 - 2311.231).
#==============================================================================
class S11(SyncedScene):
    AUDIO_START = CUES[279].start  # 2185.08

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Code", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        # cue 279: "We now have our structure-preserving properties..."
        self.sync(CUES[279].start)
        self.play(Write(header))

        steps = [
            "Step 1\nGravity\n(solve the Poisson problems)",
            "Step 2\nFluid flux\n(modified HLLC)",
            "Step 3\nGravity energy flux",
            "Step 4\nAssemble & update\n(RK stage)",
            "Step 5\nOscillation control\n(OE + positivity limiter)",
        ]
 
        boxes = VGroup()
        for s in steps:
            box = RoundedRectangle(corner_radius=0.12, width=3.6, height=1.3, color=BLUE)
            text = Text(s, font_size=18, line_spacing=0.9).move_to(box.get_center())
            boxes.add(VGroup(box, text))
 
        boxes.arrange(DOWN, buff=0.35).scale(0.85).to_edge(RIGHT, buff=1.2)
 
        arrows = VGroup(
            *[
                Arrow(boxes[i].get_bottom(), boxes[i + 1].get_top(), buff=0.05)
                for i in range(len(boxes) - 1)
            ]
        )

        # cue 282: "Then we calculate gravity solving the [perturbation]
        # Poisson problem..." (holds through cue 281's "initializing by
        # projecting into polynomial space", not its own box below).
        self.sync(CUES[282].start)
        self.play(FadeIn(boxes[0]))
        # cue 285: "Next our state is the fluid flux at each interior
        # [face]..."
        self.sync(CUES[285].start)
        self.play(GrowArrow(arrows[0]), FadeIn(boxes[1]))
        # cue 286: "Then we take Gravity Energy Flux..."
        self.sync(CUES[286].start)
        self.play(GrowArrow(arrows[1]), FadeIn(boxes[2]))
        # cue 287: "Step 4, we assemble an update."
        self.sync(CUES[287].start)
        self.play(GrowArrow(arrows[2]), FadeIn(boxes[3]))
        # cue 289: "...then we add the oscillation control..."
        self.sync(CUES[289].start)
        self.play(GrowArrow(arrows[3]), FadeIn(boxes[4]))
 
        loop_arrow = CurvedArrow(
            boxes[-1].get_right() + LEFT * 3.0,
            boxes[0].get_right() + LEFT * 3.0,
            angle=-TAU / 4,
            color=YELLOW,
        )
        loop_label = Text(
            "next stage / next timestep", font_size=18, color=YELLOW
        ).next_to(loop_arrow, LEFT, buff=0.8)

        # cue 290: "...each move on the next time step, the freshly
        # computed CFL..."
        self.sync(CUES[290].start)
        self.play(Create(loop_arrow), FadeIn(loop_label))

        # cue 291: "In fact, when I had originally set up a small 3D toy
        # model..." -- S12's opening line.
        self.sync(CUES[291].start)
 
        self.play(
            FadeOut(header), FadeOut(boxes), FadeOut(arrows),
            FadeOut(loop_arrow), FadeOut(loop_label),
        )


# =============================================================================
# S12 -- Results
# Narration: cues 291-300 (2311.591 - 2392.381), the video's closing
# section. The performance recap (cues 291-295) is staged as text callouts
# taken directly from the numbers quoted in the narration. Cues 296-300
# describe showing off animations made from the user's actual simulated
# CCSN data -- since that data isn't available here, this scene ends with
# a clearly-labeled placeholder panel (a toy collapsing-density sketch)
# rather than fabricated "results"; swap in the real renders before using
# this scene as-is.
# =============================================================================
class S12(SyncedScene):
    AUDIO_START = CUES[291].start  # 2311.591

    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(60, seed=12))
        chrome = chrome_for(audio_pct(self.AUDIO_START))
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Results", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        # cue 291: "In fact, when I had originally set up a small 3D toy
        # model for my core collapse supernova..."
        self.sync(CUES[291].start)
        self.play(Write(header))

        perf_items = VGroup(
            Text("3D toy model: several days -> a fraction of a second of data", font_size=22),
            Text("Rebuilt in 2D + parallelized (14 cores):\n~3 days -> 3 seconds of data", font_size=22),
            Text("CFL kept shrinking: timesteps became\ntoo small to push much further", font_size=22),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.45)
        perf_items.next_to(header, DOWN, buff=0.8, aligned_edge=LEFT)

        # cue 292: "It took me several days just to get to a fraction of a
        # second."
        self.sync(CUES[292].start)
        self.play(FadeIn(perf_items[0], shift=RIGHT * 0.3))
        # cue 293: "...turned it into 2D, optimized...added
        # parallelization...14 cores...three days to get three seconds
        # worth of data."
        self.sync(CUES[293].start)
        self.play(FadeIn(perf_items[1], shift=RIGHT * 0.3))
        # cue 295: "...CFL grew so much that every single timestep became
        # far too small for me to get any further, but I do have that
        # data."
        self.sync(CUES[295].start)
        self.play(FadeIn(perf_items[2], shift=RIGHT * 0.3))

        # cue 296: "Using the earlier mentioned data from the simulated
        # pseudo core collapse supernova, we create these cool
        # animations."
        self.sync(CUES[296].start)
        self.play(FadeOut(perf_items))

        placeholder_box = RoundedRectangle(
            corner_radius=0.15, width=6.5, height=4.0, color=GREY_B
        ).shift(DOWN * 0.4)
        core = Dot(radius=0.12, color=YELLOW).move_to(placeholder_box.get_center())
        placeholder_label = Text(
            "PLACEHOLDER -- drop in the real\nsimulated-CCSN density animation here",
            font_size=20, color=YELLOW,
        ).next_to(placeholder_box, UP, buff=0.3)
        self.play(Create(placeholder_box), FadeIn(placeholder_label))
        self.play(GrowFromCenter(core))
        # a simple stand-in for "density piling up in the center" --
        # replace with the actual rendered simulation data.
        self.play(core.animate.scale(3.5).set_color(RED), run_time=2.0)

        # cue 298-300: "...for these values of, say, density, it doesn't
        # show how they move traditionally... in-falling matter with
        # constant density, you wouldn't see any change. You'd only see
        # the change in the center where that area's density blows up very
        # quickly."
        self.sync(CUES[298].start)
        density_note = Text(
            "reminder: this shows density, not motion --\n"
            "infalling matter at constant density is invisible here;\n"
            "only the central density spike shows up",
            font_size=18, color=GREY_B,
        ).next_to(placeholder_box, DOWN, buff=0.3)
        self.play(FadeIn(density_note))

        self.sync(CUES[300].end)
        self.play(
            FadeOut(header), FadeOut(placeholder_box), FadeOut(placeholder_label),
            FadeOut(core), FadeOut(density_note),
        )


# =============================================================================
# Convenience: full scene order, for external tooling (e.g. render_all.sh)
# =============================================================================
ALL_SCENES = [
    "S0", "S1", "S1_1", "S2", "S3", "S4", "S5", "S6",
    "S9", "S9_1", "S9_2", "S10", "S11", "S12",
    # "S08_Questions" intentionally left out -- see the note on that class:
    # its content doesn't match this video's narration (DG_CSSN.srt) at all.
]