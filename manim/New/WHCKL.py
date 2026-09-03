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
    S2 # deriving WH
    S3 # Symplectic Correctors
    S4 # Building the Code
    S5 # Results
    S6 # Watching the results

================================================================================
"""

import os
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
def chrome_for(position, tab="WHCKL.tex"):
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
                                ("Over Engineering Celestial Mechanics", FG, )],
                               font_size=26)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        fit_width(doc_title)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        self.play(Write(doc_title), run_time=1.0)
        self.wait(0.3)

        sections = [
            "Scope",
            "Deriving WH",
            "Symplectic Correctors",
            "Building the Code",
            "Results",
            "Watching the Results",
        ]
        rows = VGroup(*[
            code_line([("** ", FG_DIM), (s, FG)], font_size=24) for s in sections
        ]).arrange(DOWN, buff=0.32, aligned_edge=LEFT)
        fit_width(rows)
        rows.next_to(doc_title, DOWN, buff=0.55, aligned_edge=LEFT)
        rows.shift(RIGHT * 0.4)

        for row in rows:
            self.play(FadeIn(row, shift=RIGHT * 0.15), run_time=0.35)
        self.wait(1.2)

        # Quick highlight sweep across the outline, like jumping through an
        # org-mode buffer with occur/imenu.
        for row in rows:
            self.play(Indicate(row, color=TEAL, scale_factor=1.04), run_time=0.18)
        self.wait(0.8)

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
        self.play(Write(header))
        self.wait(1)

        lines = prose([
            "Goals:",
            "Challenge & Learning",
            "Long-Term Keplerian Orbits",
            "Accuracy and Efficiency",
        ], font_size=27)
        fit_width(lines)
        lines.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        i = 0
    
        for l in lines:
            i += 1
            self.play(Write(l), run_time=0.8)
            if i == 2:
                self.wait(10)
            else:
                self.wait(0.4)
        self.wait(20)

        Choice = Text("Wisdom-Holman: WHCKL",
                         font=FONT, font_size=32, color=TEAL, weight=BOLD)
        fit_width(Choice)
        Choice.next_to(lines, DOWN, buff=0.6, aligned_edge=LEFT)
        box = SurroundingRectangle(Choice, color=TEAL, buff=0.3, stroke_width=2)
        self.play(Write(Choice), run_time=1.2)
        self.play(Create(box))
        self.wait(26.5)
        self.play(FadeOut(header, lines, Choice, box))


# =============================================================================
# S2 -- Deriving WH
# =============================================================================
class S2(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("12%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Deriving WH", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))

        sub = Text("We start with N-Body Hamiltonian in Inertial Cartesian Coordinates", font_size=26, color=FG_DIM)
        sub.next_to(header, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(Write(sub))
        self.wait(3)

        # N-Body Hamiltonian in Inertial Cartesian Coordinates
        N_Ham = MathTex(r"H(q,p) = \sum_{i=0}^{N-1}\frac{P^2_i}{2m_i} - G \sum_{i<j} \frac{m_i m_j}{\norm{q_i - q_j}}", font_size=44, color=FG)
        N_Ham.move_to(ORIGIN)
        self.play(Write(N_Ham))
        self.wait(15)

        self.play(FadeOut(N_Ham, sub))

        self.wait(4)

        # Helicentric coordinates

        position_Hel = MathTex(r"Q_i = q_i - q_0", font_size=36, color=FG)
        Momenta_Hel = MathTex(r"P_i = p_i", font_size=36, color=FG)
        Momenta_Hel_sun = MathTex(r"P_0 = p_0 \sum_{i=1}^{N-1} p_i", font_size=36, color=FG)

        position_Hel.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        Momenta_Hel.next_to(position_Hel, DOWN, buff=0.3, aligned_edge=LEFT)
        Momenta_Hel_sun.next_to(Momenta_Hel, DOWN, buff=0.3, aligned_edge=LEFT)

        self.play(Write(position_Hel), run_time=1.0)
        self.play(Write(Momenta_Hel), run_time=1.0)
        self.play(Write(Momenta_Hel_sun), run_time=1.0)
        self.wait(38)
        self.play(FadeOut(position_Hel, Momenta_Hel, Momenta_Hel_sun))
    
        # full Helicentric Hamiltonian
        full_Hel = MathTex(r"H = \frac{P_0^2}{2M} + \sum_{i=1}^{N-1} \left[ \frac{P^2_i}{2m_i} - \frac{G m_0 M_i}{Q_i}  \right] + \sum^{N-1}_{i=1} \frac{P^2_i}{2m_0} - G \sum_{i<j, i,j > 0} \frac{m_i m_j}{\norm{Q_i-Q_j}}", font_size=44, color=FG)
        
        full_Hel.move_to(ORIGIN)
        self.play(Write(full_Hel), run_time=1.5)
        self.wait(12.0)
        self.play(FadeOut(full_Hel))
        
        #divide Hamiltonian
        Hel_divide = MathTex(r"H = H_{sun} + H_{kep} + H_{int}", font_size=44, color=FG)

        Hel_divide.move_to(ORIGIN)
        self.play(Write(Hel_divide), run_time=1.5)
        self.wait(11.0)
        self.play(FadeOut(Hel_divide))

        # Jacobi coordinates

        Capital_R = Text("Capital R = Center of Mass of all interior particles", font_size=26, color=FG_DIM)
        jacobi_pos = MathTex(r"r'_i = q_i - R_{i-1}", font_size=36, color=FG)
        jacobi_mom = MathTex(r"p' = m'_i \dot{r'}", font_size=36, color=FG)
        reduced_mass = MathTex(r"m'_i = \frac{m_i M_{i-1}}{M_i}", font_size=36, color=FG)
        Capital_R.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        jacobi_pos.next_to(Capital_R, DOWN, buff=0.3, aligned_edge=LEFT)
        jacobi_mom.next_to(jacobi_pos, DOWN, buff=0.3, aligned_edge=LEFT)
        reduced_mass.next_to(jacobi_mom, DOWN, buff=0.3, aligned_edge=LEFT)
        self.play(Write(Capital_R), run_time=1.0)
        self.play(Write(jacobi_pos), run_time=1.0)
        self.play(Write(jacobi_mom), run_time=1.0)
        self.play(Write(reduced_mass), run_time=1.0)
        self.wait(46)
        self.play(FadeOut(Capital_R, jacobi_pos, jacobi_mom, reduced_mass))

        #N-body Hamiltonian in Jacobi coordinates
        Jacobi_Ham = MathTex(r"H = \sum_{i=0}^{N-1}\frac{p'^2_i}{2m_i'} - \sum_{i=0}^{N-1} \sum_{j=i+1}^{N-1} \frac{G m_i m_j}{\norm{r_i - r_j}}", font_size=44, color=FG)
        Jacobi_Ham.move_to(ORIGIN)
        self.play(Write(Jacobi_Ham), run_time=1.5)
        self.wait(16.0)
        self.play(Jacobi_Ham.animate.shift(UP * 1.5))

        plus_minus = MathTex(r"H_{\pm} = \sum_{i=1}^{N-1} \frac{G m'_i M_i}{\norm{r'_i}}", font_size=44, color=FG)
        plus_minus.next_to(Jacobi_Ham, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(plus_minus), run_time=1.5)
        self.wait(7)
        self.play(plus_minus.animate.shift(UP * 1.5))

        grouping = MathTex(r"H = \underbrace{\frac{(p_0')^{2}}{2 m_0'}}_{H_0} + \underbrace{ \sum_{i=1}^{N-1} \left( \frac{(p_i')^{2}}{2 m_i'} - \frac{G\, m_i' M_i}{\lvert r_i' \rvert} \right) }_{H_{\text{Kepler}}} + \underbrace{ \sum_{i=1}^{N-1} \frac{G\, m_i' M_i}{\lvert r_i' \rvert} - \sum_{i=0}^{N-1} \sum_{j=i+1}^{N-1} \frac{G\, m_i m_j}{\lvert r_i - r_j \rvert} }_{H_{\text{Interaction}}}.", font_size=36, color=FG)

        grouping.move_to(Jacobi_Ham.get_center())

        self.play(ReplacementTransform(VGroup(Jacobi_Ham, plus_minus), grouping), run_time=1.5)
        self.wait(3)
        self.play(grouping.animate.shift(DOWN * 1.5))

        self.wait(6.0)
        self.play(FadeOut(grouping))
        # split keplerian and interaction Hamiltonians
        kep = MathTex(r"(H_{\text{Kepler}})_i = \frac{p^{'2}_i}{2m'_i} - \frac{Gm'_i M_i}{\norm{r'_i}}", font_size=36, color=FG)
        kep.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(Write(kep), run_time=1.5)
        self.wait(12.0)

        interaction = MathTex(r"H_{int} = \sum_{i=2}^{N-1} \frac{Gm'_i M_i}{\norm{r'_i}} - \sum_{i=0}^{N-1} \sum_{j=i+1}^{N-1} \frac{Gm_im_j}{\norm{r_i - r_j}}", font_size=36, color=FG)
        interaction.next_to(kep, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(interaction), run_time=1.5)
        self.wait(6.0)

        self.play(FadeOut(VGroup(kep, interaction)))

        self.play(Write(Hel_divide), run_time=1.5)
        self.wait(70.0)

        self.play(FadeOut(Hel_divide, header))
# =============================================================================
# S03 -- WH Mapping
# =============================================================================
class S3(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(70, seed=3))
        chrome = chrome_for("22%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("WH Maping", PURPLE), ("}", FG_DIM)],
                            font_size=28)
        place_header(header)
        self.play(Write(header))

        origional = MathTex(r"H(q,p) = \sum_{i=0}^{N-1}\frac{P^2_i}{2m_i} - G \sum_{i<j} \frac{m_i m_j}{\norm{q_i - q_j}}", font_size=36, color=FG)
        origional.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(Write(origional))
        self.wait(17.0)
        split = MathTex(r"H = A + B", font_size=36, color=FG)
        split.next_to(origional, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(split))
        self.wait(24.0)

        self.play(FadeOut(VGroup(origional, split)))

        # Liouville's Opertator
        Operator = MathTex(r"L_H f = \{f, H\}", font_size=36, color=FG)
        Operator = Operator.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(Write(Operator))
        self.wait(13.0)
        Operator_Expanded = MathTex(r"L_H f = \{f, H\} = -\frac{\partial f}{\partial t}", font_size=36, color=FG)
        Operator_Expanded.move_to(Operator.get_center())
        self.play(ReplacementTransform(Operator, Operator_Expanded), run_time=1.5)

        differential = MathTex(r"f(t+h) = e^{h L_H} f(t)", font_size=36, color=FG)
        differential.next_to(Operator_Expanded, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(differential), run_time=1.5)
        self.wait(20.0)
        self.play(FadeOut(VGroup(Operator_Expanded, differential)))
        # e

        # Define left and right anchors
        left_anchor = ORIGIN + LEFT * config.frame_width * 0.25
        right_anchor = ORIGIN + RIGHT * config.frame_width * 0.25

        # Left side content
        differential_operator = MathTex(r"e").move_to(ORIGIN + LEFT)

        middle = MathTex(r"\neq").move_to(ORIGIN)
        # Right side content
        euler = MathTex(r"e").move_to(ORIGIN + RIGHT)

        # Write both
        self.play(Write(differential_operator), Write(middle), Write(euler))
        self.wait(14)

        self.play(FadeOut(middle))
        # Animate left side
        self.play(differential_operator.animate.shift(UP * 1.5 + LEFT * config.frame_width * 0.25), euler.animate.shift(UP * 1.5 + RIGHT * config.frame_width * 0.25))

        # Animate right side
        #self.play(euler.animate.shift(UP * 1.5 + RIGHT * config.frame_width * 0.25))

        differential_Operator_word = Text("Differential Operator", font_size = 32).move_to(differential_operator.get_center())
        Euler_Word = Text("Euler's Number", font_size = 32).move_to(euler.get_center())

        self.play(ReplacementTransform(differential_operator, differential_Operator_word, run_time=1.5), ReplacementTransform(euler, Euler_Word, run_time=1.5))
        Characteristics_of_Euler = prose([
            "Number",
            "Powers commute",
        ], font_size=27)
        Characteristics_of_Euler.next_to(Euler_Word, DOWN, buff=0.7, aligned_edge=LEFT)
        
    
        

        Characteristics_of_Operator = prose([
                    "Operator",
                    "Expressions do not commute",
                ], font_size=27)
        Characteristics_of_Operator.next_to(differential_Operator_word, DOWN, buff=0.7, aligned_edge=LEFT)
                
            
        self.play(Write(Characteristics_of_Euler), Write(Characteristics_of_Operator))
        self.wait(12)
        self.play(FadeOut(Characteristics_of_Operator, Characteristics_of_Euler, differential_Operator_word, Euler_Word))

        # Poisson for

        L_Poisson = MathTex(r"L_H = L_0 + L_{Kepler} + L_{int}", font_size=36, color=FG)

        Poisson_C = MathTex(r"L_H = A + B + C", font_size=36, color=FG)

        Poisson_A = MathTex(r"L_H = A + B", font_size=36, color=FG)

        L_Poisson.move_to(ORIGIN)

        self.play(Write(L_Poisson))

        self.wait(8)
        self.play(ReplacementTransform(L_Poisson, Poisson_C), run_time=1.5)

        self.wait(8)
        self.play(ReplacementTransform(Poisson_C, Poisson_A), run_time=1.5)
        self.wait(6)
        # exact propergator
        exact_prop = MathTex(r"e^{h(A+B)} \neq e^{Ah + Bh}", font_size=36, color=FG)

        exact_prop.next_to(Poisson_A, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(exact_prop))

        self.wait(12)
        self.play(FadeOut(exact_prop, Poisson_A))
        # Strang
        strangs_expression = Text("Generic Strang Formula", font_size = 32)
        strang = MathTex(
            r"e^{\Delta t(A+B)} \approx "
            r"e^{\frac{\Delta t}{2}A}"
            r"e^{\Delta t B}"
            r"e^{\frac{\Delta t}{2}A}"
        )
        strangs_expression.next_to(strang, UP, buff=0.5, aligned_edge=LEFT)
        self.play(Write(strang), FadeIn(strangs_expression))

        self.wait(8)
        self.play(FadeOut(strang, strangs_expression))
        # ============================================================================
# S4 -- Symplectic correctors
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
                             ("Symplectic Correctors", PURPLE), ("}", FG_DIM)], font_size=30)
        place_header(header)
        self.play(Write(header))
        self.wait(3.0)

        # symplectic corrector equation
        strang_c = MathTex(r"e^{h(A+B)} \approx  e^{\frac{\Delta t}{2}A} e^{\Delta t B} e^{\frac{\Delta t}{2}A}",font_size=36, color=FG)

        self.play(Write(strang_c))
        self.wait(3)
        strang_c_counter = MathTex(r"e^{h(A+B)} \approx  \underbrace{e^C}_1 e^{\frac{\Delta t}{2}A} e^{\Delta t B} e^{\frac{\Delta t}{2}A} \underbrace{e^{-C}}_2",font_size=36, color=FG)

        self.play(ReplacementTransform(strang_c, strang_c_counter), run_time=1.5)
        self.wait(15)

        self.play(FadeOut(strang_c_counter))
        # Original coordinate system
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            axis_config={"include_numbers": True},
        ).scale(0.5)
        axes_label = axes.get_axis_labels("x", "y")
        self.play(Create(axes), Write(axes_label))

        # A vector in the original basis
        v = Arrow(axes.c2p(0, 0), axes.c2p(2, 1), buff=0, color=YELLOW)
        #v.scale(0.6)
        v_label = MathTex(r"\vec{v}").next_to(v.get_end(), UP)
        self.play(GrowArrow(v), Write(v_label))

        self.wait()

        # New basis matrix (columns are new basis vectors in old coordinates)
        # e1' = (1, 1), e2' = (-1, 1)
        basis_matrix = [[1, -1],
                        [1,  1]]

        basis_text = MathTex(
            r"B = \begin{bmatrix} 1 & -1 \\ 1 & 1 \end{bmatrix}"
        ).to_edge(UP)
        #self.play(Write(basis_text))

        # Animate the coordinate system changing to the new basis
        self.play(
            axes.animate.apply_matrix(basis_matrix),
            v.animate.apply_matrix(basis_matrix),
            v_label.animate.apply_matrix(basis_matrix),
            axes_label.animate.apply_matrix(basis_matrix),
            run_time=3,
        )

        
        self.wait(15)

        self.play(ReplacementTransform(VGroup(axes, v_label, v, axes_label), strang_c_counter), run_time=1.5)

        self.wait(79)
        self.play(FadeOut(strang_c_counter, header))

# =============================================================================
# S5 -- Building the code
# =============================================================================
class S5(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("52%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Building the Code", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))
        self.wait(10.0)
        Code_nature = prose([
            "- Code = FORTRAN",
            "- Optomize: Jacobi <-> Cartesian",
            "- Fiften most massive bodies in the solar system",
        ], font_size=24)
        Code_nature.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        for i in range(len(Code_nature)):
            self.play(Write(Code_nature[i]), run_time=0.5)
            self.wait(16)
        self.wait(12.0)
        self.play(FadeOut(Code_nature))

        self.wait(3)
        Code_nature = prose([
                    "- Eight Main Planets",
                    "- Ceres",
                    "- Pluto",
                    "- Haumea",
                    "- Makemake",
                    "- Gonggong",
                    "- Eris"
                ], font_size=24)
        Code_nature.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(Code_nature))
        self.wait(68)
        self.play(FadeOut(Code_nature))

        


# =============================================================================
# S06 -- RESULTS
# =============================================================================
class S06_Results(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("66%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Results", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))
        self.wait(0.3)

        run_lbl = code_line([("$ ", GREEN), ("run monte_carlo.sim --N 100000", FG)], font_size=24)
        run_lbl.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        self.play(Write(run_lbl), run_time=1.0)

        dots_txt = Text("...", font=FONT, font_size=24, color=FG_DIM)
        dots_txt.next_to(run_lbl, DOWN, buff=0.3, aligned_edge=LEFT)
        self.play(FadeIn(dots_txt))
        self.wait(0.6)
        self.play(FadeOut(dots_txt, header, run_lbl))

        img = ImageMobject("~/Downloads/mc_energy_distribution.png")
        img.scale(1)
        img.move_to(ORIGIN)
        self.add(img)

        self.wait(5.0)
        self.play(FadeOut(img))


# =============================================================================
# S07 -- INTERPRETING THE RESULTS
# =============================================================================
class S07_Interpreting(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(70, seed=7))
        chrome = chrome_for("78%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Interpreting the Results", PURPLE), ("}", FG_DIM)], font_size=28)
        place_header(header)
        self.play(Write(header))

        goal = Text("goal: energy -> force, relativistically", font=FONT, font_size=22, color=FG_DIM)
        goal.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(goal))
        self.wait(5)
        work = MathTex(r"W = F \cdot x", font_size=38, color=RED)
        work.next_to(goal, DOWN, buff=0.5)
        self.play(Write(work))
        self.wait(26)
        self.play(FadeOut(goal, work))


        cons = MathTex(r"\nabla_\mu T^{\mu\nu} = f^{\nu}", font_size=48, color=FG)
        cons.move_to(UP * 1.7)
        cons_note = comment_line("conservation of energy-momentum", font_size=20)
        cons_note.next_to(cons, DOWN, buff=0.3)
        self.play(Write(cons))
        self.play(FadeIn(cons_note))
        self.wait(17.5)

        expand = MathTex(
            r"f^\nu = \frac{\partial T^{0\nu}}{\partial x^0} + \frac{\partial T^{1\nu}}{\partial x^1}"
            r" + \frac{\partial T^{2\nu}}{\partial x^2} + \frac{\partial T^{3\nu}}{\partial x^3}",
            font_size=30, color=FG)
        expand.next_to(cons_note, DOWN, buff=0.5)
        self.play(Write(expand), run_time=1.3)
        self.wait(9)

        simplify = MathTex(
            r"f^{i} = \frac{1}{c}\frac{\partial T^{0i}}{\partial t} + \partial_j T^{ij}",
            font_size=34, color=TEAL)
        simplify.next_to(expand, DOWN, buff=0.5)
        simp_note = Text("change in momentum density + momentum flux through the boundary",
                          font=FONT, font_size=18, color=FG_DIM)
        simp_note.next_to(simplify, DOWN, buff=0.25)
        self.play(Write(simplify))
        self.play(FadeIn(simp_note))
        self.wait(6)

        self.play(FadeOut(cons, cons_note, expand, simplify, simp_note))

        focus = Text("keep only the energy-density term; spread over a 1 m volume:",
                      font=FONT, font_size=22, color=FG_DIM)
        focus.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)
        self.play(FadeIn(focus))
        self.wait(16)

        energy = MathTex(r"\Delta E = 3.6\times 10^{71}\ \text{J}", font_size=38, color=FG)
        energy.next_to(focus, DOWN, buff=0.5)
        self.play(Write(energy))
        self.wait(4)

        arrow = MathTex(r"\longrightarrow", font_size=38, color=FG_DIM)
        arrow.next_to(energy, RIGHT, buff=0.4)
        force = MathTex(r"F = 3.6\times 10^{71}\ \text{N}", font_size=44, color=ORANGE)
        force.next_to(arrow, RIGHT, buff=0.4)
        self.play(Write(arrow), Write(force))
        fbox = SurroundingRectangle(force, color=ORANGE, buff=0.25, stroke_width=2)
        self.play(Create(fbox))
        self.wait(22)
        self.play(FadeOut(focus, energy, arrow, force, fbox))

        headline = Text("36 followed by 70 zeros. Newtons.",
                         font=FONT, font_size=38, color=TEAL, weight=BOLD)
        headline.move_to(UP * 1.6)
        self.play(Write(headline), run_time=1.3)
        self.wait(5)
        self.play(headline.animate.to_edge(UP, buff=1.6))

        # --- comparison scale bar ---
        bar = scale_bar(0, 75, width=10.6)
        bar.move_to(DOWN * 0.6)
        left_lbl = MathTex(r"10^{0}", font_size=20, color=FG_DIM).next_to(bar.get_left(), DOWN, buff=0.25)
        right_lbl = MathTex(r"10^{75}", font_size=20, color=FG_DIM).next_to(bar.get_right(), DOWN, buff=0.25)
        self.play(Create(bar), FadeIn(left_lbl), FadeIn(right_lbl))

        markers = [
            (3.0, "avg. human punch\n(~1,000 N)", YELLOW, UP),
            (4.84, "hardest recorded punch\n(~69,000 N)", YELLOW, DOWN),
            (39.0, "Superman pulling planets\n(comic estimates)", PURPLE, UP),
            (46.56, "Big Bang energy release\n(est.)", RED, DOWN),
            (71.56, "this force", TEAL, UP),
        ]
        mgroup = VGroup()
        for exp, label, col, direc in markers:
            m = scale_marker(bar, exp, label, color=col, font_size=16, direction=direc, dot_radius=0.075)
            mgroup.add(m)
        self.play(LaggedStart(*[FadeIn(m, scale=0.7) for m in mgroup], lag_ratio=0.25), run_time=2.2)
        self.wait(1.5)

        closing = prose([
            "~67 orders of magnitude past the hardest human punch.",
            "~25 orders of magnitude past the Big Bang's energy release.",
            "Even Superman's comic-book feats fall ~31 orders short.",
        ], font_size=20, color=FG)
        closing.to_edge(DOWN, buff=1.0)
        self.play(FadeOut(headline, left_lbl, right_lbl))
        self.play(bar.animate.shift(UP * 1.0), mgroup.animate.shift(UP * 1.0))
        closing.next_to(bar, DOWN, buff=0.9)
        for l in closing:
            self.play(FadeIn(l), run_time=0.6)
            self.wait(0.3)
        self.wait(35)

        punch = Text("Humans are closer to Superman's feat than\nSuperman is to this one.",
                      font=FONT, font_size=22, color=TEAL, weight=BOLD)
        punch.next_to(closing, DOWN, buff=0.5)
        self.play(Write(punch), run_time=1.4)
        self.wait(14)

        self.play(FadeOut(VGroup(header, bar, mgroup, closing, punch)))


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
class S09_Outro(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=9))
        chrome = chrome_for("100%")
        self.add(chrome)

        thanks = Text("Thank you for watching.", font=FONT, font_size=44, color=FG, weight=BOLD)
        thanks.move_to(ORIGIN)
        cursor = Cursor().next_to(thanks, RIGHT, buff=0.15)
        self.play(Write(thanks), run_time=1.4)
        self.play(FadeIn(cursor))
        self.play(blink(cursor, cycles=2))
        self.wait(0.5)

        # "save" the buffer: mode-line flips from modified (**) to saved (--)
        saved_left = Text(f"--:---  universe-force.tex", font=FONT, font_size=16, color=MODELINE_FG)
        saved_left.move_to(chrome.left_txt)
        self.play(ReplacementTransform(chrome.left_txt, saved_left), run_time=0.6)
        save_note = comment_line("Wrote universe-force.tex", font_size=20)
        save_note.next_to(thanks, DOWN, buff=0.6)
        self.play(FadeIn(save_note))
        self.wait(1.5)

        self.play(FadeOut(thanks, cursor, save_note, chrome))
        self.wait(0.3)


# =============================================================================
# Convenience: full scene order, for external tooling (e.g. render_all.sh)
# =============================================================================
ALL_SCENES = [
    "S00_Title", "S00b_Outline", "S01_Introduction", "S02_PlanckForce", "S03_StrikingEarth",
    "S04_Assumptions", "S05_Modeling", "S06_Results", "S07_Interpreting",
    "S08_Questions", "S09_Outro",
]
