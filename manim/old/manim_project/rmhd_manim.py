"""
Manim Community Edition animation script for the video on:
Kraus, Tassi & Grasso, "Variational Integrators for Reduced
Magnetohydrodynamics" (arXiv:1511.09314v2).

One Scene class per section of the narration script, so each can be
rendered and reviewed independently, then concatenated (see
render_all.sh in this same folder).

--------------------------------------------------------------------
IMPORTANT: this script was written and syntax-checked (`python -m
py_compile`) but could NOT be rendered in the environment it was
written in -- that sandbox has no network access, and while it has
texlive/cairo/pango system libraries installed, it does not have the
`manim` Python package or the `dvisvgm` binary that Manim's LaTeX
renderer needs, and neither could be installed offline. So: the
structure, API calls, and LaTeX are believed correct based on the
Manim Community Edition API, but you are the first one to actually
render this. If something breaks, it's most likely a small LaTeX
string issue or a version-specific API rename -- see the troubleshooting
notes at the bottom of this file.
--------------------------------------------------------------------

Install (on a machine with network access):
    pip install manim
    # system deps (Ubuntu/Debian): sudo apt install libcairo2-dev libpango1.0-dev
    #                               texlive texlive-latex-extra dvisvgm ffmpeg

Render one scene (fast preview):
    manim -pql rmhd_manim.py OpeningScene

Render everything and concatenate:
    bash render_all.sh l      # low quality preview
    bash render_all.sh h      # 1080p60 final
"""

import glob
import os

from manim import *

config.background_color = "#282C34"  #1A1B26: vim   #FDF6E3:Emacs

ASSET_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "assets")
VALIDATION_DIR = os.path.join(ASSET_DIR, "validation")
TURBULENCE_DIR = os.path.join(ASSET_DIR, "turbulence")

ACCENT = "#51AFEF"     # teal accent for headings / highlights #C0CAF5:vim  #268BD2:Emacs
GHOST = "#C678DD"      # purple for "ghost"/auxiliary variables   #BB9AF7:vim   #C678DD:Emacs
WARN = "#FF6C6B"       # orange for caveats / defects   #F7768E:vim #DC322F:Emacs
GOOD = "#ECBE7B"       # green for "this works" / conserved quantities  #7AA2F7:vim #B58900:Emacs

class CodeText(Text):
    def __init__(self, text, **kwargs):
        super().__init__(text, font="Times New Roman", **kwargs)



#config["tex_template"].add_to_preamble(r"\usepackage{}")  # example font

def make_title(text, sub=None):
    """Standard section title block used at the start of most scenes."""
    title = Text(text, font_size=52, color=ACCENT, weight=BOLD)
    group = VGroup(title)
    if sub:
        subtitle = Text(sub, font_size=28, color=WHITE)
        subtitle.next_to(title, DOWN, buff=0.3)
        group.add(subtitle)
    group.to_edge(UP, buff=0.6)
    return group


def clear_scene(scene):
    if scene.mobjects:
        scene.play(FadeOut(Group(*scene.mobjects)), run_time=0.6)


# ======================================================================
# 1. OPENING
# ======================================================================
class OpeningScene(Scene):
    def construct(self):
        title = Text(
            "Variational Integrators for\nReduced Magnetohydrodynamics",
            font_size=44, color=ACCENT, weight=BOLD, line_spacing=1.2,
        ).move_to(UP * 1.0)
        authors = Text(
            "Michael Kraus, Emanuele Tassi, Daniela Grasso",
            font_size=30, color=WHITE,
        ).next_to(title, DOWN, buff=0.6)

        self.play(FadeIn(title, shift=UP * 0.3), run_time=1.2)
        self.play(FadeIn(authors, shift=UP * 0.2), run_time=1.0)
        self.wait(19.0)



        #self.play(FadeIn(disclaimer), run_time=1.0)
        
        clear_scene(self)


# ======================================================================
# 2. INTRODUCTION
# ======================================================================
class IntroductionScene(Scene):
    def construct(self):
        title = make_title("Introduction")
        self.play(FadeIn(title))

        bullet1 = Text(
            "Goal: a numerical method for magnetized-plasma models\n"
            "that preserves conservation laws far more faithfully\n"
            "than standard methods.",
            font_size=30, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.8)

        bullet2 = Text(
            "Tool: a variational integrator.",
            font_size=32, color=ACCENT, weight=BOLD,
        ).next_to(bullet1, DOWN, buff=0.7)
        
        self.play(FadeIn(bullet1, shift=UP * 0.2))
        self.wait(8)
        self.play(FadeOut(bullet1, shift=UP * 0.2))
        self.play(FadeIn(bullet2, shift=UP * 2.0))
        self.wait(6.0)
        self.play(FadeOut(bullet2, shift=UP * 0.2))
        # visual: Action -> discrete scheme, bypassing the equations of motion
        action_box = Rectangle(width=3.2, height=1.0, color=ACCENT).set_fill(ACCENT, opacity=0.15)
        action_label = Text("Action  S[q]", font_size=26).move_to(action_box)
        action_group = VGroup(action_box, action_label).shift(LEFT * 4 + DOWN * 1.6)

        eom_box = Rectangle(width=3.2, height=1.0, color=GRAY_B).set_fill(GRAY_B, opacity=0.1)
        eom_label = Text("Equations of motion", font_size=22).move_to(eom_box)
        eom_group = VGroup(eom_box, eom_label).shift(UP * 0.6)

        scheme_box = Rectangle(width=3.2, height=1.0, color=GOOD).set_fill(GOOD, opacity=0.15)
        scheme_label = Text("Discrete scheme", font_size=26).move_to(scheme_box)
        scheme_group = VGroup(scheme_box, scheme_label).shift(RIGHT * 4 + DOWN * 1.6)

        arrow_traditional = Arrow(eom_group.get_bottom(), scheme_group.get_top(), color=GRAY_B)
        arrow_direct = Arrow(action_group.get_right(), scheme_group.get_left(), color=ACCENT)
        skip_label = Text("discretize the action directly", font_size=20, color=ACCENT)
        skip_label.next_to(arrow_direct, DOWN, buff=0.15)

        self.play(FadeIn(action_group), FadeIn(scheme_group))
        self.play(Create(arrow_direct), FadeIn(skip_label))
        self.wait(0.5)
        self.play(FadeIn(eom_group, shift=UP * 0.2))
        traditional_label = Text("(traditional route)", font_size=18, color=GRAY_B)
        traditional_label.next_to(arrow_traditional, LEFT, buff=0.15)
        self.play(Create(arrow_traditional), FadeIn(traditional_label))
        self.wait(2.0)
        clear_scene(self)


# ======================================================================
# 3. VARIATIONAL INTEGRATORS (traditional vs. variational, side by side)
# ======================================================================
class VariationalIntegratorsScene(Scene):
    def construct(self):
        title = make_title("Variational Integrators")
        self.play(FadeIn(title))

        left_header = Text("Traditional", font_size=30, color=WARN, weight=BOLD)
        right_header = Text("Variational", font_size=30, color=ACCENT, weight=BOLD)
        left_header.move_to(LEFT * 3.5 + UP * 1.7)
        right_header.move_to(RIGHT * 3.5 + UP * 1.7)
        self.play(FadeIn(left_header), FadeIn(right_header))

        left_steps = VGroup(
            MathTex("F = kx"),
            MathTex(r"\ddot{x} + \frac{k}{m}x = 0"),
            Text("discretize THIS equation", font_size=22, color=GRAY_B),
        ).arrange(DOWN, buff=0.45)
        left_steps.next_to(left_header, DOWN, buff=0.5)

        right_steps = VGroup(
            MathTex(r"S[q] = \int_{t_0}^{t_1} L(q,\dot q)\, dt"),
            MathTex(r"\delta S = 0"),
            Text("(Euler-Lagrange, Hamiltonian,\nconservation laws)",
                 font_size=20, color=GRAY_B, line_spacing=1.1),
        ).arrange(DOWN, buff=0.4)
        right_steps.next_to(right_header, DOWN, buff=0.5)

        self.play(LaggedStart(*[FadeIn(m, shift=UP * 0.15) for m in left_steps], lag_ratio=0.4))
        self.wait(8.0)
        self.play(LaggedStart(*[FadeIn(m, shift=UP * 0.15) for m in right_steps], lag_ratio=0.4))
        self.wait(12.0)
        self.play(FadeOut(left_steps), FadeOut(left_header))
        discretize_note = Text(
            "instead of discretizing the DE,\ndiscretize the Lagrangian itself:",
            font_size=22, color=ACCENT, line_spacing=1.2,
        ).move_to(LEFT * 3.5 + UP * 1.7)
        discrete_action = MathTex(
            r"S_d = \sum_{k=0}^{N-1} L_d(q_k, q_{k+1}),\quad "
            r"L_d \approx \int_{t_k}^{t_{k+1}} L\, dt"
        ).scale(0.8).next_to(discretize_note, DOWN, buff=0.3)

        self.play(FadeIn(discretize_note))
        self.play(Write(discrete_action))
        self.wait(10)

        punchline = Text(
            "Varying over the discrete sequence gives the Discrete\n"
            "Euler-Lagrange equations -- and that IS the algorithm.\n"
            "No extra discretization step needed.",
            font_size=20, color=GOOD, line_spacing=1.3,
        ).next_to(discrete_action, DOWN, buff=0.4)
        box = SurroundingRectangle(punchline, color=GOOD, buff=0.25)
        self.play(FadeIn(punchline), Create(box))
        self.wait(10.0)
        clear_scene(self)


# ======================================================================
# 4. RMHD
# ======================================================================
class RMHDScene(Scene):
    def construct(self):
        title = make_title("Reduced Magnetohydrodynamics (RMHD)")
        title.scale(0.8)
        self.play(FadeIn(title))

        mhd_text = Text(
            "MHD: plasma as a single conducting fluid\ncoupled to a magnetic field.",
            font_size=24, line_spacing=1.2,
        ).next_to(title, DOWN, buff=0.6)
        self.play(FadeIn(mhd_text))
        self.wait(28)

        rmhd_text = Text(
            "RMHD: strong background field (e.g. a tokamak) --\n"
            "fast dynamics along the field stop mattering,\n"
            "leaving an effectively 2D problem.",
            font_size=26, line_spacing=1.2,
        ).next_to(mhd_text, DOWN, buff=0.5)
        self.play(FadeIn(rmhd_text))
        self.wait(32.0)
        self.play(FadeOut(mhd_text), FadeOut(rmhd_text))

        fields = VGroup(
            MathTex(r"\phi", "-", r"\text{stream function (flow)}"),
            MathTex(r"\omega", "-", r"\text{vorticity (flow's swirl)}"),
            MathTex(r"\psi", "-", r"\text{magnetic potential}"),
            MathTex("j", "-", r"\text{current density (field's swirl)}"),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.35)
        for f in fields:
            f[0].set_color(ACCENT)
        fields.next_to(title, DOWN, buff=0.7)

        self.play(LaggedStart(*[FadeIn(f, shift=RIGHT * 0.2) for f in fields], lag_ratio=0.3))
        self.wait(6.30)
        self.play(fields.animate.scale(0.7).to_edge(LEFT, buff=0.6))

        eqs = VGroup(
            MathTex(r"\omega_t + \{\phi,\omega\} + \{j,\psi\} = 0,\quad -\Delta\phi=\omega"),
            MathTex(r"\psi_t + \{\phi,\psi\} = 0,\quad -\Delta\psi = j"),
            MathTex(r"\{\phi,\omega\} = \phi_x \omega_y - \phi_y \omega_x"),
        ).arrange(DOWN, buff=0.5).scale(0.85)
        eqs.next_to(fields, RIGHT, buff=0.8)

        self.play(Write(eqs[0]))
        self.wait(0.5)
        self.play(Write(eqs[1]))
        self.wait(0.5)
        self.play(Write(eqs[2]))
        self.wait(10)
        self.play(FadeOut(eqs), FadeOut(fields))

        casimir_title = Text("Casimir invariants:", font_size=28, color=ACCENT).next_to(title, DOWN, buff=0.7)
        casimirs = VGroup(
            Text("Magnetic helicity -- how tangled the field lines are", font_size=24),
            Text("Cross helicity -- how aligned flow is with field", font_size=24),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.3).next_to(casimir_title, DOWN, buff=0.4)

        self.play(FadeIn(casimir_title))
        self.play(LaggedStart(*[FadeIn(c) for c in casimirs], lag_ratio=0.5))
        self.wait(15.0)

        lie_note = Text(
            "Structure is Lie-Poisson, not canonical Hamiltonian --\n"
            "still conserves energy, but standard symplectic\n"
            "methods simply don't apply here.",
            font_size=24, color=WARN, line_spacing=1.3,
        ).next_to(casimirs, DOWN, buff=0.6)
        self.play(FadeIn(lie_note))
        self.wait(34.5)
        clear_scene(self)


# ======================================================================
# 5. FORMAL LAGRANGIAN
# ======================================================================
class FormalLagrangianScene(Scene):
    def construct(self):
        title = make_title("Formal Lagrangian")
        self.play(FadeIn(title))

        problem = Text(
            "Snag: no natural Eulerian Lagrangian exists for RMHD.",
            font_size=28, color=WARN,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(problem))
        self.wait(10.0)

        trick = Text(
            "The trick: multiply each equation by a made-up\n"
            '"ghost" (auxiliary) variable with no physical meaning,\n'
            "then add them all up.",
            font_size=26, line_spacing=1.3,
        ).next_to(problem, DOWN, buff=0.6)
        self.play(FadeIn(trick))
        self.wait(15)
        self.play(FadeOut(problem), FadeOut(trick))

        lagrangian = MathTex(
            r"L = ", r"\xi", r"(\omega_t + \{\phi,\omega\} + \{j,\psi\})",
            r"+\,", r"\mu", r"(\omega + \Delta\phi)",
            r"+\,", r"\chi", r"(\psi_t + \{\phi,\psi\})",
            r"+\,", r"\zeta", r"(j + \Delta\psi)",
        ).scale(0.62)
        lagrangian.next_to(title, DOWN, buff=0.9)
        for ghost in (1, 4, 7, 10):
            lagrangian[ghost].set_color(GHOST)

        self.play(Write(lagrangian), run_time=3.0)
        #self.wait(15.0)

        ghost_note = Text(
            r"ξ, μ, χ, ζ  --  four ghosts, existing purely so the arithmetic works.",
            font_size=24, color=GHOST,
        ).next_to(lagrangian, DOWN, buff=0.6)
        self.play(FadeIn(ghost_note))
        self.wait(15.0)
        self.play(FadeOut(ghost_note))

        el_eq = MathTex(
            r"\frac{\partial L}{\partial \varphi_a} - "
            r"\frac{\partial}{\partial x^\mu}\left(\frac{\partial L}{\partial(\partial_\mu \varphi_a)}\right) = 0"
            r"\quad \text{for all } a"
        ).scale(0.8).next_to(lagrangian, DOWN, buff=0.7)
        self.play(Write(el_eq))
        self.wait(5.0)
        self.play(FadeOut(el_eq))

        embedding = MathTex(
            r"\Phi : (\omega,\psi,\phi,j) \mapsto (\omega,\psi,\phi,j,\psi,\omega,0,0)"
        ).scale(0.75).next_to(lagrangian, DOWN, buff=0.7)
        embedding_note = Text(
            "the embedding: setting the ghosts equal to\n"
            "physical fields collapses everything consistently.",
            font_size=22, color=GRAY_B, line_spacing=1.2,
        ).next_to(embedding, DOWN, buff=0.4)
        self.play(Write(embedding))
        self.play(FadeIn(embedding_note))
        self.wait(15.0)
        clear_scene(self)


# ======================================================================
# 6. CONSERVATION LAWS
# ======================================================================
class ConservationLawsScene(Scene):
    def construct(self):
        title = make_title("Conservation Laws")
        self.play(FadeIn(title))

        purpose = Text(
            "The whole point of building that Lagrangian:\napplying Noether's theorem.",
            font_size=28, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(purpose))
        self.wait(7.0)
        self.play(FadeOut(purpose))

        vertical_note = Text(
            'We restrict to "vertical" transformations:\n'
            "symmetries that only shift FIELD VALUES,\n"
            "never the underlying space-time grid.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(vertical_note))

        # tiny diagram: a grid of dots (spacetime) with one vertical arrow
        dots = VGroup(*[Dot(radius=0.05, color=GRAY_B) for _ in range(12)])
        dots.arrange_in_grid(rows=3, cols=4, buff=0.5)
        dots.next_to(vertical_note, DOWN, buff=0.6)
        vert_arrow = Arrow(
            dots[5].get_center(), dots[5].get_center() + UP * 0.8,
            color=ACCENT, buff=0,
        )
        self.play(FadeIn(dots))
        self.play(Create(vert_arrow))
        self.wait(1.5)
        self.play(FadeOut(vertical_note), FadeOut(dots), FadeOut(vert_arrow))

        result_title = Text("Four conserved quantities fall out:", font_size=28, color=ACCENT)
        result_title.next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(result_title))

        items = VGroup(
            Text("Magnetic helicity", font_size=28),
            MathTex(r"L^2", r"\text{ norm of } \psi", font_size=36),
            Text("Cross helicity", font_size=28),
            Text("Energy", font_size=28),
        ).arrange(DOWN, buff=0.4).next_to(result_title, DOWN, buff=0.6)

        for item in items:
            check = Text("\u2713", color=GOOD, font_size=28).next_to(item, LEFT, buff=0.3)
            self.play(FadeIn(item, shift=RIGHT * 0.2), FadeIn(check), run_time=0.6)
        self.wait(32.0)
        clear_scene(self)


# ======================================================================
# 7. ELECTRON INERTIA
# ======================================================================
class ElectronInertiaScene(Scene):
    def construct(self):
        title = make_title("Electron Inertia")
        self.play(FadeIn(title))

        defect = Text(
            "Ideal RMHD has a defect: it FORBIDS magnetic\n"
            "reconnection -- field lines snapping and reconnecting,\n"
            "one of the biggest events in plasma physics.",
            font_size=26, color=WARN, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(defect))
        self.wait(4.0)

        # simple crossing-lines diagram suggesting reconnection
        line1 = Line(LEFT * 1.5, RIGHT * 1.5, color=BLUE).rotate(PI / 8)
        line2 = Line(LEFT * 1.5, RIGHT * 1.5, color=RED).rotate(-PI / 8)
        diagram = VGroup(line1, line2).next_to(defect, DOWN, buff=0.7)
        self.play(Create(diagram))
        self.wait(8.0)
        self.play(FadeOut(defect), FadeOut(diagram))

        fix = Text(
            "The fix: give the electron a small amount of mass\n"
            "(electron inertia). A new term appears in Ohm's law,\n"
            "and reconnection becomes possible again.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(fix))
        self.wait(12.0)
        self.play(FadeOut(fix))

        eq = MathTex(r"\bar\psi = \psi + d_e^2\, j").scale(1.3).next_to(title, DOWN, buff=1.0)
        eq_note = Text(
            "the electron canonical momentum;\nd_e = electron skin depth",
            font_size=24, color=GRAY_B, line_spacing=1.2,
        ).next_to(eq, DOWN, buff=0.5)
        self.play(Write(eq))
        self.play(FadeIn(eq_note))
        self.wait(24.0)

        survives = Text(
            "That's it -- the whole modification.\n"
            "Hamiltonian structure, Casimir invariants, and every\n"
            "conservation law survive, just written in terms of ",
            font_size=24, line_spacing=1.3,
        )
        psi_bar = MathTex(r"\bar\psi", font_size=32, color=ACCENT)
        survives_group = VGroup(survives, psi_bar).arrange(RIGHT, buff=0.15)
        survives_group.next_to(eq_note, DOWN, buff=0.6)
        self.play(FadeIn(survives_group))
        self.wait(16.0)
        clear_scene(self)


# ======================================================================
# 8. SYMMETRISATION
# ======================================================================
class SymmetrisationScene(Scene):
    def construct(self):
        title = make_title("Symmetrisation")
        self.play(FadeIn(title))

        intro = Text(
            "Tedious but essential housekeeping before\n"
            "anything touches a computer.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(1.0)
        self.play(FadeOut(intro))

        problem1 = Text(
            "Problem 1: second derivatives (those Laplacians)\n"
            "are painful to discretize directly.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(problem1))

        ibp = MathTex(r"\mu\,\Delta\phi \;\longrightarrow\; -\nabla\mu\cdot\nabla\phi").scale(0.9)
        ibp.next_to(problem1, DOWN, buff=0.5)
        ibp_note = Text("integration by parts -- first derivatives only", font_size=22, color=GRAY_B)
        ibp_note.next_to(ibp, DOWN, buff=0.3)
        self.play(Write(ibp))
        self.play(FadeIn(ibp_note))
        self.wait(24.0)
        self.play(FadeOut(problem1), FadeOut(ibp), FadeOut(ibp_note))

        problem2 = Text(
            "Problem 2: the Poisson bracket can be rewritten\n"
            "several equivalent ways (equal only after integrating\n"
            "over all space) -- pick carelessly and you break\n"
            "the antisymmetry your proofs depend on.",
            font_size=24, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(problem2))
        self.wait(16.0)
        self.play(FadeOut(problem2))

        combo = MathTex(
            r"\int \xi\{\phi,\omega\}\,dxdy = \int \Big["
            r"\alpha\,\xi\{\phi,\omega\} + \beta\,\phi\{\omega,\xi\} + \gamma\,\omega\{\xi,\phi\}"
            r"\Big]\, dxdy"
        ).scale(0.62).next_to(title, DOWN, buff=0.8)
        self.play(Write(combo))
        self.wait(6.0)

        alpha_note = MathTex(r"\alpha=\beta=\gamma=\tfrac{1}{3}", color=ACCENT).scale(1.1)
        alpha_note.next_to(combo, DOWN, buff=0.6)
        alpha_box = SurroundingRectangle(alpha_note, color=ACCENT)
        self.play(Write(alpha_note), Create(alpha_box))
        self.wait(2.0)
        self.play(FadeOut(combo), FadeOut(alpha_note), FadeOut(alpha_box))

        final_L = MathTex(
            r"L''' = \tfrac{1}{2}\varphi_t^T \Lambda \varphi",
            r"+ \tfrac{1}{3}\big(\xi\{\phi,\omega\}+\phi\{\omega,\xi\}+\omega\{\xi,\phi\}\big)",
            r"+ \tfrac{1}{3}\big(\xi\{j,\psi\}+j\{\psi,\xi\}+\psi\{\xi,j\}\big)",
            r"+ \tfrac{1}{3}\big(\chi\{\phi,\psi\}+\phi\{\psi,\chi\}+\psi\{\chi,\phi\}\big)",
            r"+ \mu\omega - \nabla\mu\cdot\nabla\phi + \zeta j - \nabla\zeta\cdot\nabla\psi",
        ).scale(0.55).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        final_L.next_to(title, DOWN, buff=0.7)

        self.play(LaggedStart(*[Write(line) for line in final_L], lag_ratio=0.6), run_time=4.0)
        self.wait(2.5)
        clear_scene(self)


# ======================================================================
# 9. DISCRETE ACTION PRINCIPLE
# ======================================================================
class DiscreteActionPrincipleScene(Scene):
    def construct(self):
        title = make_title("Discrete Action Principle")
        self.play(FadeIn(title))

        intro = Text(
            "Veselov-type finite differences: split space and time,\n"
            "work cell by cell on a rectangular grid.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(6.0)
        # grid-cell diagram with 4 corners labelled
        square = Square(side_length=2.2, color=GRAY_B).next_to(intro, DOWN, buff=0.6)
        corner_labels = VGroup(
            Text("(i,j)", font_size=20).move_to(square.get_corner(DL) + DL * 0.35),
            Text("(i+1,j)", font_size=20).move_to(square.get_corner(DR) + DR * 0.35),
            Text("(i+1,j+1)", font_size=20).move_to(square.get_corner(UR) + UR * 0.35),
            Text("(i,j+1)", font_size=20).move_to(square.get_corner(UL) + UL * 0.35),
        )
        self.play(Create(square))
        self.play(LaggedStart(*[FadeIn(c) for c in corner_labels], lag_ratio=0.3))
        self.wait(7.2)
        self.play(FadeOut(intro), FadeOut(square), FadeOut(corner_labels))

        hp = Text(
            "Hamilton-Pontryagin principle: treat velocity and\n"
            "momentum as independent, derive their relation\n"
            "afterwards, instead of assuming it up front.",
            font_size=24, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(hp))
        self.wait(21.0)
        self.play(FadeOut(hp))

        arakawa_title = Text("Arakawa's discretisation of the Poisson bracket:", font_size=26, color=ACCENT)
        arakawa_title.next_to(title, DOWN, buff=0.7)
        arakawa_eq = MathTex(
            r"A_{i,j}(\phi,\omega) = \tfrac{1}{3}\Big("
            r"A^{++}_{i,j}(\phi,\omega) + A^{+\times}_{i,j}(\phi,\omega) + A^{\times +}_{i,j}(\phi,\omega)"
            r"\Big)"
        ).scale(0.75).next_to(arakawa_title, DOWN, buff=0.5)
        self.play(FadeIn(arakawa_title))
        self.play(Write(arakawa_eq))
        self.wait(8.0)

        legend = VGroup(
            Text("A++   :  direct centred difference in x AND y", font_size=22),
            Text("A+x   :  diagonal ('divergence') form", font_size=22),
            Text("Ax+   :  diagonal ('rotation') form", font_size=22),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25).next_to(arakawa_eq, DOWN, buff=0.6)
        self.play(LaggedStart(*[FadeIn(l) for l in legend], lag_ratio=0.4))
        self.wait(8.0)
        self.play(FadeOut(arakawa_title), FadeOut(arakawa_eq), FadeOut(legend))

        final_title = Text("The resulting discrete scheme:", font_size=26, color=ACCENT)
        final_title.next_to(title, DOWN, buff=0.6)
        eqs = VGroup(
            MathTex(
                r"0=\frac{\omega^{n+1}-\omega^n}{h_t}+\tfrac14\big[A(\phi^{n+1},\omega^{n+1})+A(\phi^n,\omega^{n+1})"
                r"+A(\phi^{n+1},\omega^n)+A(\phi^n,\omega^n)\big]"
            ),
            MathTex(
                r"\qquad+\tfrac14\big[A(j^{n+1},\psi^{n+1})+A(j^n,\psi^{n+1})+A(j^{n+1},\psi^n)+A(j^n,\psi^n)\big]"
            ),
            MathTex(
                r"0=\frac{\psi^{n+1}-\psi^n}{h_t}+\tfrac14\big[A(\phi^{n+1},\psi^{n+1})+A(\phi^n,\psi^{n+1})"
                r"+A(\phi^{n+1},\psi^n)+A(\phi^n,\psi^n)\big]"
            ),
            MathTex(r"\omega^{n+1} = -\Delta_x\phi^{n+1} - \Delta_y\phi^{n+1}"),
            MathTex(r"j^{n+1} = -\Delta_x\psi^{n+1} - \Delta_y\psi^{n+1}"),
        ).scale(0.5).arrange(DOWN, buff=0.25)
        eqs.next_to(final_title, DOWN, buff=0.4)

        self.play(FadeIn(final_title))
        self.play(LaggedStart(*[Write(e) for e in eqs], lag_ratio=0.5), run_time=4.0)
        self.wait(8.0)

        note = Text(
            "This is already an algorithm -- no extra method needed\n"
            "(though we'll still need one to SOLVE it -- more later).",
            font_size=20, color=GRAY_B, line_spacing=1.2,
        ).next_to(eqs, DOWN, buff=0.4)
        self.play(FadeIn(note))
        self.wait(4.0)
        clear_scene(self)


# ======================================================================
# 10. DISCRETE CONSERVATION LAWS
# ======================================================================
class DiscreteConservationLawsScene(Scene):
    def construct(self):
        title = make_title("Discrete Conservation Laws")
        self.play(FadeIn(title))

        intro = Text(
            "The exact same argument, run on the discrete system,\n"
            "gives four EXACTLY conserved discrete quantities:",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(4.0)
        self.play(FadeOut(intro))

        laws = VGroup(
            VGroup(Text("Total energy", font_size=24, color=ACCENT),
                   MathTex(r"E=\tfrac{h_xh_y}{2}\sum_{i,j}(\phi_{i,j}\omega_{i,j}+\psi_{i,j}j_{i,j}) = \text{const.}").scale(0.6)),
            VGroup(Text("Magnetic helicity", font_size=24, color=ACCENT),
                   MathTex(r"C_{MH}=h_xh_y\sum_{i,j}\psi_{i,j} = \text{const.}").scale(0.6)),
            VGroup(Text("L\u00b2 norm of psi", font_size=24, color=ACCENT),
                   MathTex(r"C_{L^2}=h_xh_y\sum_{i,j}\psi_{i,j}^2 = \text{const.}").scale(0.6)),
            VGroup(Text("Cross helicity", font_size=24, color=ACCENT),
                   MathTex(r"C_{CH}=h_xh_y\sum_{i,j}\omega_{i,j}\psi_{i,j} = \text{const.}").scale(0.6)),
        )
        for row in laws:
            row.arrange(RIGHT, buff=0.5)
        laws.arrange(DOWN, aligned_edge=LEFT, buff=0.45).next_to(title, DOWN, buff=0.8)

        for row in laws:
            self.play(FadeIn(row, shift=UP * 0.15), run_time=0.8)
            box = SurroundingRectangle(row[1], color=GOOD, buff=0.12)
            self.play(Create(box), run_time=0.4)

        self.wait(5.0)


        #self.play(FadeIn(punch))
        
        clear_scene(self)


# ======================================================================
# 11. NUMERICAL METHODS
# ======================================================================
class NumericalMethodsScene(Scene):
    def construct(self):
        title = make_title("Numerical Methods")
        self.play(FadeIn(title))

        intro = Text(
            "The discrete equations are implicit and nonlinear --\n"
            "omega and psi at the new time step tangle with each other.",
            font_size=26, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(5.0)
        self.play(FadeOut(intro))

        chain = VGroup(
            Text("Newton's method", font_size=28, color=ACCENT),
            Text("linearize the residual", font_size=22, color=GRAY_B),
            Text("GMRES", font_size=28, color=ACCENT),
            Text("iterative Krylov solve of each linear system", font_size=22, color=GRAY_B),
            Text("physics-based preconditioner", font_size=28, color=ACCENT),
            Text("(Chacon et al.) to make GMRES converge fast", font_size=22, color=GRAY_B),
        ).arrange(DOWN, buff=0.28)
        chain.next_to(title, DOWN, buff=0.7)

        self.play(LaggedStart(*[FadeIn(m, shift=UP * 0.1) for m in chain], lag_ratio=0.35), run_time=3.0)
        self.wait(27.0)
        self.play(FadeOut(chain))

        elliptic = Text(
            "Elliptic solves (Laplacian inversions): PETSc in general,\n"
            "or -- for a doubly-periodic domain -- an exact FFT solve,\n"
            "since periodic BCs diagonalize the discrete Laplacian exactly.",
            font_size=24, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(elliptic))
        self.wait(32.0)
        self.play(FadeOut(elliptic))

        mine = Text("I implemented this myself, in Python:", font_size=28, color=ACCENT)
        mine.next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(mine))

        def file_box(name):
            rect = Rectangle(width=4.6, height=0.8, color=ACCENT)
            label = Text(name, font_size=22).move_to(rect.get_center())
            return VGroup(rect, label)

        files = VGroup(
            file_box("operators.py"),
            file_box("integrator.py"),
            file_box("initial_conditions.py"),
        ).arrange(DOWN, buff=0.35).next_to(mine, DOWN, buff=0.6)

        captions = VGroup(
            Text("Arakawa bracket, Laplacian, FFT solvers", font_size=18, color=GRAY_B),
            Text("Newton-GMRES time-stepper + diagnostics", font_size=18, color=GRAY_B),
            Text("Orszag-Tang & current-sheet test cases", font_size=18, color=GRAY_B),
        )
        for box, cap in zip(files, captions):
            cap.next_to(box, RIGHT, buff=0.4)

        for box, cap in zip(files, captions):
            self.play(FadeIn(box), FadeIn(cap), run_time=0.7)
        self.wait(6.0)
        clear_scene(self)


# ======================================================================
# 12. VALIDATION
# ======================================================================
class ValidationScene(Scene):
    def construct(self):
        title = make_title("Validation")
        self.play(FadeIn(title))

        intro = Text(
            "Does my implementation actually match the paper?",
            font_size=28,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(1.0)
        self.play(FadeOut(intro))

        def show_image(path, caption_text, scale=3.2):
            img = ImageMobject(path).scale_to_fit_height(scale)
            cap = Text(caption_text, font_size=24, color=ACCENT)
            group = Group(img, cap).arrange(DOWN, buff=0.3)
            group.next_to(title, DOWN, buff=0.5)
            return group

        ot_j = show_image(
            os.path.join(VALIDATION_DIR, "orszag_tang_current_density.png"),
            "Orszag-Tang vortex -- current density",
        )
        self.play(FadeIn(ot_j))
        self.wait(2.0)
        self.play(FadeOut(ot_j))

        ot_c = show_image(
            os.path.join(VALIDATION_DIR, "orszag_tang_conservation.png"),
            "Orszag-Tang vortex -- conservation errors",
        )
        self.play(FadeIn(ot_c))
        self.wait(2.0)
        self.play(FadeOut(ot_c))

        cs = show_image(
            os.path.join(VALIDATION_DIR, "current_sheet_psi_contours.png"),
            "Current sheet: ideal vs. electron inertia",
        )
        self.play(FadeIn(cs))
        self.wait(2.0)
        self.play(FadeOut(cs))

        cs_c = show_image(
            os.path.join(VALIDATION_DIR, "current_sheet_inertia_conservation.png"),
            "Current sheet + electron inertia -- conservation errors",
        )
        self.play(FadeIn(cs_c))
        self.wait(2.0)
        self.play(FadeOut(cs_c))

        conclusion = Text(
            "All four cases match the paper's own results,\n"
            "and match the conserved analytical quantities\n"
            "to within machine precision.",
            font_size=26, color=GOOD, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.8)
        self.play(FadeIn(conclusion))
        self.wait(16.0)
        clear_scene(self)


# ======================================================================
# 13. RANDOM TEST
# ======================================================================
class RandomTestScene(Scene):
    def construct(self):
        title = make_title("Random Test")
        self.play(FadeIn(title))

        intro = Text(
            "Now that we have a working model, let's use it\nfor something cool:",
            font_size=28, line_spacing=1.3,
        ).next_to(title, DOWN, buff=0.7)
        self.play(FadeIn(intro))
        self.wait(16.0)

        subtitle = Text(
            "Reduced-MHD turbulence, generated with our own solver",
            font_size=26, color=ACCENT,
        ).next_to(intro, DOWN, buff=0.4)
        self.play(FadeIn(subtitle))
        self.wait(16.0)


        self.play(FadeOut(intro), FadeOut(subtitle))

        frame_paths = sorted(glob.glob(os.path.join(TURBULENCE_DIR, "frame_*.png")))
        if not frame_paths:
            missing = Text("(turbulence frames not found)", color=WARN, font_size=28)
            self.play(FadeIn(missing))
            self.wait(16.0)
        else:
            img = ImageMobject(frame_paths[0]).scale_to_fit_height(5.0)
            img.next_to(title, DOWN, buff=0.6)
            self.play(FadeIn(img))
            for path in frame_paths[1:]:
                new_img = ImageMobject(path).scale_to_fit_height(5.0)
                new_img.move_to(img)
                self.remove(img)
                self.add(new_img)
                img = new_img
                self.wait(1 / 15)
            self.wait(0.5)
            self.play(FadeOut(img))

        outro = Text("Thanks for watching!", font_size=36, color=ACCENT, weight=BOLD)
        self.play(FadeIn(outro, shift=UP * 0.2))
        self.wait(8.0)
        clear_scene(self)


"""
TROUBLESHOOTING NOTES (things most likely to need a small fix once you
actually render this, since it was never tested against a live Manim
install):

1. `Text(..., "\u2713", ...)` (checkmark) in ConservationLawsScene needs
   a font with that glyph; if it renders as a box/blank, swap it for
   Text("OK") or a small Manim Checkmark-like shape.

2. `MathTex` strings use only standard LaTeX (no custom macros), so they
   should compile with a normal texlive install, but if any individual
   equation fails, Manim's error message will show you exactly which
   MathTex(...) string LaTeX choked on -- it's almost always a stray
   brace or an escaped character.

3. `ImageMobject(...).scale_to_fit_height(...)` is the Manim CE method
   name at the time of writing; if your installed version renamed it,
   the equivalent is `.height = <value>`.

4. The RandomTestScene flipbook (51 ImageMobjects swapped in a loop) is
   the most unusual bit of API use here -- it's a standard community
   workaround for "playing a video" since core Manim has no first-class
   video-embedding mobject, but it does mean that scene's render time
   scales with the number of frames.
"""
