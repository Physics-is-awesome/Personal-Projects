"""
WHCKL.py
================================================================================

Render one scene at a time, e.g.:

    manim -pqh WHCKL.py S1
    manim -pqh WHCKL.py S5

or render everything (see render_all.sh)

Scene map (mirrors the script's own \\section structure):
    S0 # introduction         
    S1 # Scope
    S2 # Modivating DG
    S3 # General Introduction to DG
    S4 # Physical Model
    S5 # L2 Projection
    S6 # Structure preserving LDG
    S7 # Time Discretization
    S8 # Ossilation Elimination
    S9 # Structure Preserving Properties
    S10 # Code 
    S11 # Results

================================================================================
"""

import os
from tkinter import RIGHT
from xml.dom.minidom import Text

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
def chrome_for(position, tab="DG CSNN.tex"):
    return BufferChrome(filename=tab, mode="LaTeX/Preview", position=position, tab=tab)


def place_header(header, max_width=None):
    """Anchor a section-header mobject to a fixed top-left margin, shrinking
    it first if it would otherwise be wider than the safe content area.
    Every other element in the scene should be built relative to this fixed
    point (next_to(..., aligned_edge=LEFT)) so nothing drifts off-frame."""
    fit_width(header, max_width)
    header.to_edge(UP, buff=1.0)
    header.to_edge(LEFT, buff=0.8)
    return header
min = 60

def wait_until_time(self, target):
    local_time = target - (60+7.5)
    dt = local_time - self.time
    if dt > 0:
        self.wait(dt)


# =============================================================================
# S0 -- Introduction
# =============================================================================
class S0(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(50, seed=2))
        chrome = chrome_for("0%")
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

        

        self.play(FadeOut(doc_title), FadeOut(rows))


# =============================================================================
# S1 -- Scope
# =============================================================================
class S1(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("4%")
        self.add(chrome)

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
 
        for item in scope_items:
            self.play(FadeIn(item, shift=RIGHT * 0.3), run_time=0.6)

        wait_until_time(self, 60+6.5)
        
        self.play(FadeOut(header, scope_items), run_time=0.5)



# =============================================================================
# S2 -- Motivating Operator Splitting
# =============================================================================
class S2(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("12%")
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
 
        self.play(FadeIn(boxes[0]))
        for i in range(len(boxes) - 1):
            self.play(GrowArrow(arrows[i]), FadeIn(boxes[i + 1]))
        self.wait(0.5)
 
        cap_group = VGroup()
        for i, cap in enumerate(captions):
            c = Text(cap, font_size=16, color=GREY_B).next_to(
                boxes[i], DOWN, buff=0.3
            )
            cap_group.add(c)
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
        self.play(FadeIn(note))
        target = 120 + 32
        wait_until_time(self, target)
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
 
        self.play(Create(grid))
        self.play(Write(omega_label))
        target = 2*60 + 36
        wait_until_time(self, target)
 
        approx = MathTex(
            r"u_h(x,t) = \sum_{j=0}^N u_j(t)\, \phi_j(x)", font_size=32
        ).next_to(grid, RIGHT, buff=1.0).shift(UP * 1.5)
        self.play(Write(approx))
        target = 2*60 + 42
        
 
        strong_form = MathTex(
            r"\frac{\partial u}{\partial t} + \nabla \cdot \mathbf{F} = S(u)",
            font_size=34,
        ).move_to(approx.get_center() + DOWN * 1.2)
        self.play(Write(strong_form))
        wait_until_time(self, target)
 
        multiply_note = Text(
            "multiply by test function φ, integrate over Ω,\n"
            "integrate the flux term by parts",
            font_size=20,
            color=GREY_B,
        ).move_to(strong_form.get_center() + DOWN * 1.2 + RIGHT * 0.2)
        self.play(FadeIn(multiply_note))
        wait_until_time(self, 2*60 + 57)
 
        weak_form = MathTex(
            r"\int_\Omega \phi\, \frac{\partial u}{\partial t}\, dV"
            r"-\int_\Omega \nabla\phi \cdot \mathbf{F}\, dV"
            r"+\int_{\partial\Omega} \phi\, \mathbf{F}\cdot\mathbf{n}\, ds"
            r"= \int_\Omega \phi\, S(u)\, dV",
            font_size=26,
        )
        weak_form.move_to(ORIGIN)
 
        self.play(
            ReplacementTransform(VGroup(strong_form, multiply_note, approx, grid, omega_label), weak_form),
        )
        wait_until_time(self, 3*60 + 8)

 
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
 
        self.play(Write(ode))
        self.play(Create(box))
        self.wait(1.5)
 
        self.play(
            FadeOut(header), FadeOut(r_total), FadeOut(terms),
            FadeOut(ode), FadeOut(box),
        )
        self.play(FadeOut(header))

            
# =============================================================================
# S03 -- Deriving Strang Splitting
# =============================================================================
class S3(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(70, seed=3))
        chrome = chrome_for("22%")
        self.add(chrome)

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
        self.play(Write(system))
        self.wait(1)
 
        
        self.wait(1.5)
 
        self.play(
            FadeOut(header), FadeOut(row),
            FadeOut(system), 
        )


        # ============================================================================
# S4 -- Adjoint Formulism
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


class S4(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("38%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("L2 Projections", PURPLE), ("}", FG_DIM)], font_size=30)
        place_header(header)
        self.play(Write(header))
        self.wait(6.0)

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
 
        self.play(Create(axes))
        self.play(Create(smooth_curve), FadeIn(smooth_label))
        self.wait(0.5)
        self.play(Create(poly_approx), FadeIn(poly_label))
        self.wait(1)
 
        eqs = VGroup(
            MathTex(r"\int_K u\, v\, dx = \int_K \mathbf{P}(u)\, v\, dx", font_size=28),
            MathTex(r"P(u)(x) = \sum_j c_j\, v_j(x)", font_size=28),
            MathTex(r"M_{ij} = \int_K \phi_i\, v_j\, dx, \qquad b_j = \int_K u\, v_j\, dx", font_size=26),
            MathTex(r"P(u)(x) = \sum_j \big(M^{-1}b\big)_j\, v_j(x)", font_size=28, color=YELLOW),
        ).arrange(DOWN, buff=0.35)
        eqs.next_to(axes, RIGHT, buff=0.8).shift(UP * 0.1)
 
        for eq in eqs:
            self.play(Write(eq), run_time=0.8)
        self.wait(1.5)
 
        self.play(
            FadeOut(header), FadeOut(axes), FadeOut(smooth_curve),
            FadeOut(poly_approx), FadeOut(smooth_label), FadeOut(poly_label),
            FadeOut(eqs),
        )

# =============================================================================
# S5 -- Higher Order Splitting and Composition
# =============================================================================
class S5(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("52%")
        self.add(chrome)

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
 
        self.play(Create(line_L), Create(line_star), Create(line_R))
        self.play(
            FadeIn(label_L), FadeIn(label_starL),
            FadeIn(label_starR), FadeIn(label_R),
            FadeIn(speed_labels),
        )
        self.wait(1)
 
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
        self.play(Write(formula))
        self.wait(1.5)
 
        note = Text(
            "at rest, equal pressure on both sides:\n"
            "collapses to just the pressure pushing on the face",
            font_size=20, color=GREY_B,
        ).next_to(formula, UP, buff=0.25)
        self.play(FadeIn(note))
        self.wait(1.5)
 
        self.play(
            FadeOut(header), FadeOut(line_L), FadeOut(line_star),
            FadeOut(line_R), FadeOut(label_L), FadeOut(label_starL),
            FadeOut(label_starR), FadeOut(label_R), FadeOut(speed_labels),
            FadeOut(formula), FadeOut(note),
        )

        


# =============================================================================
# S06 -- RESULTS
# =============================================================================
class S6(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("66%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Standard LDG", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))
        self.wait(0.3)

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
        self.play(Write(block))
        self.wait(1)
 
        arrow = MathTex(r"\Longrightarrow \text{(Schur complement)}", font_size=26).next_to(
            block, DOWN, buff=0.4
        )
        schur = MathTex(
            r"A\phi_h = \big(S + B^T K^{-1} B\big)\phi_h = f + B^T K^{-1} g",
            font_size=26, color=YELLOW,
        ).next_to(arrow, DOWN, buff=0.3)
 
        self.play(FadeIn(arrow))
        self.play(Write(schur))
        self.wait(1)
 
        note = Text(
            "A is symmetric positive definite:\n"
            "factor once, reuse it every timestep",
            font_size=22, color=YELLOW,
        ).next_to(schur, DOWN, buff=0.5)
        self.play(FadeIn(note))
        self.wait(1.5)
 
        self.play(
            FadeOut(heading), FadeOut(system), FadeOut(block),
            FadeOut(arrow), FadeOut(schur), FadeOut(note),
        )


# =============================================================================
# S08 -- GENERAL QUESTIONS
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
# S09 -- OUTRO
# =============================================================================
class S9(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for("100%")
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Structure LDG", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))
        self.wait(4)
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
        self.play(*[GrowArrow(a) for a in out_arrows], FadeIn(pressure_label))
        self.play(*[GrowArrow(a) for a in in_arrows], FadeIn(gravity_label))
 
        balance_eq = MathTex(
            r"\nabla p^e = -\rho^e \nabla\phi^e", font_size=28
        ).next_to(star, DOWN, buff=2.0)
        self.play(Write(balance_eq))
        self.wait(1)
 
        split = MathTex(
            r"\rho = \rho^e + \rho^\delta, \qquad \phi = \phi^e + \phi^\delta",
            font_size=30,
        ).to_edge(RIGHT, buff=0.7).shift(UP * 1.7)
        split_note = Text(
            "frozen equilibrium\n+ evolving perturbation",
            font_size=18, color=GREY_B,
        ).next_to(split, DOWN, buff=0.2)
        self.play(Write(split), FadeIn(split_note))
        self.wait(1)
 
        modflux = MathTex(
            r"\widehat{\mathbf{F}} = \mathbf{F}^{hllc}\!\left("
            r"\frac{p_h^{e,\star}}{p_h^{e,\text{int}}}\mathbf{U}_h^{\text{int}},\ "
            r"\frac{p_h^{e,\star}}{p_h^{e,\text{ext}}}\mathbf{U}_h^{\text{ext}}\right)",
            font_size=24,
        ).next_to(split, DOWN, buff=1.0)
        self.play(Write(modflux))
        self.wait(1)
 
        result = Text(
            "at equilibrium: this flux and the modified\n"
            "source term cancel exactly, term for term",
            font_size=20, color=YELLOW,
        ).next_to(modflux, DOWN, buff=0.4)
        self.play(FadeIn(result))
        self.wait(2)
 
        self.play(
            FadeOut(header), FadeOut(heading), FadeOut(star), FadeOut(out_arrows),
            FadeOut(in_arrows), FadeOut(pressure_label), FadeOut(gravity_label),
            FadeOut(balance_eq), FadeOut(split), FadeOut(split_note),
            FadeOut(modflux), FadeOut(result),
        )



#==============================================================================
#
#==============================================================================
# =============================================================================
# S09 -- OUTRO
# =============================================================================
class S10(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for("100%")
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Oscilation Elimination", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))
        self.wait(4)

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
 
        self.play(Create(axes_left), Create(axes_right))
        self.play(Create(target_L), Create(target_R))
        self.play(Create(ring_curve), FadeIn(label_left))
        self.play(Create(damped_curve), FadeIn(label_right))
        self.wait(1)
 
        formula = MathTex(
            r"\frac{d\mathbf{U}_\sigma}{dt} = -\mathbf{\Sigma}(\mathbf{U})\,\mathbf{U}_\sigma",
            font_size=28,
        ).to_edge(DOWN, buff=0.5)
        note = Text(
            "damps high moments near a jump, leaves the cell average untouched;\n"
            "applied only to the perturbation part, never the frozen equilibrium",
            font_size=18, color=GREY_B,
        ).next_to(formula, UP, buff=0.2)
 
        self.play(Write(formula), FadeIn(note))
        self.wait(2)
 
        self.play(
            FadeOut(header), FadeOut(axes_left), FadeOut(axes_right),
            FadeOut(target_L), FadeOut(target_R), FadeOut(ring_curve),
            FadeOut(damped_curve), FadeOut(label_left), FadeOut(label_right),
            FadeOut(formula), FadeOut(note),
        )

#==============================================================================
#
#==============================================================================
class S11(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for("100%")
        self.add(chrome)
        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                                     ("Code", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))

        self.wait(4)
        
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
 
        boxes.arrange(DOWN, buff=0.35).scale(0.85).to_edge(LEFT, buff=1.2)
 
        arrows = VGroup(
            *[
                Arrow(boxes[i].get_bottom(), boxes[i + 1].get_top(), buff=0.05)
                for i in range(len(boxes) - 1)
            ]
        )
 
        self.play(FadeIn(boxes[0]))
        for i in range(len(boxes) - 1):
            self.play(GrowArrow(arrows[i]), FadeIn(boxes[i + 1]))
        self.wait(0.5)
 
        loop_arrow = CurvedArrow(
            boxes[-1].get_right() + RIGHT * 0.1,
            boxes[0].get_right() + RIGHT * 0.1,
            angle=-TAU / 4,
            color=YELLOW,
        )
        loop_label = Text(
            "next stage / next timestep", font_size=18, color=YELLOW
        ).next_to(loop_arrow, RIGHT, buff=0.15)
 
        self.play(Create(loop_arrow), FadeIn(loop_label))
        self.wait(2)
 
        self.play(
            FadeOut(header), FadeOut(boxes), FadeOut(arrows),
            FadeOut(loop_arrow), FadeOut(loop_label),
        )

# =============================================================================
# Convenience: full scene order, for external tooling (e.g. render_all.sh)
# =============================================================================
ALL_SCENES = [
    "S00_Title", "S00b_Outline", "S01_Introduction", "S02_PlanckForce", "S03_StrikingEarth",
    "S04_Assumptions", "S05_Modeling", "S06_Results", "S07_Interpreting",
    "S08_Questions", "S09_Outro",
]
