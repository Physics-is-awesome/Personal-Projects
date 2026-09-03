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
    S2 # Modivating operator splitting
    S3 # Derive Strang Splitting
    S4 # Adjoint formulism
    S5 # Higher order splitting and composition
    S6 # Splitting Methods for PDEs

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
def chrome_for(position, tab="Lie.tex"):
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
                                ("Lie Operator Splitting: One of the ten Pillars", FG, )],
                               font_size=26)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        fit_width(doc_title)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        self.play(Write(doc_title), run_time=1.0)
        self.wait(0.3)

        sections = [
            "Introduction",
            "Motivating Operator Splitting",
            "Derive Strang Splitting",
            "Adjoint Formulism",
            "Higher order splitting and composition",
            "Splitting Methods for PDEs",
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
        self.wait(2)

        split_ham = MathTex(r"H = A + B", font_size=44, color=FG)
        split_ham.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)

        self.play(Write(split_ham))
        self.wait(10)
        self.play(FadeOut(split_ham))

        circle_1 = Circle(radius=2.0, color=FG_DIM, fill_color=FG_DIM, fill_opacity=0.1, stroke_width=1.5)
        circle_1.move_to(ORIGIN)

        num_nodes = 10
        radius = 3.5
        outer_nodes = []
        lines = []

        for i in range(num_nodes):
            angle = TAU * i / num_nodes

            # Position of outer node
            point = radius * np.array([np.cos(angle), np.sin(angle), 0])
            node = Dot(point=point, radius=0.08, color=BLUE)
            outer_nodes.append(node)

            # Line endpoint on the circle boundary
            boundary_point = circle_1.point_at_angle(angle)

            # Line from boundary of circle to outer node
            line = Line(boundary_point, point, color=WHITE)
            lines.append(line)

        num_anlysis = Text("Numerical Analysis", font_size=32, color=FG)
        num_anlysis.move_to(ORIGIN)

        self.play(FadeIn(circle_1, num_anlysis, *outer_nodes, *lines))
        self.wait(5)

        target = outer_nodes[0]
        objects = VGroup(*outer_nodes, num_anlysis, *lines, circle_1)
        others = VGroup(*[obj for obj in objects if obj is not target])
        self.play(
            others.animate.set_opacity(0).scale(0.5),
            run_time=1.2
        )

        # 2. Move the target to the center
        self.play(target.animate.move_to(ORIGIN), run_time=1)

        # 3. Zoom in (scale up)
        self.play(target.animate.scale(2), run_time=1)

        # 4. Transform the target (example: turn square into circle)
        new_shape = Circle(radius=2.5, color=BLUE)
        self.play(Transform(target, new_shape), run_time=1.5)

        splitting_methods = Text("Splitting Methods", font_size=32, color=FG)
        splitting_methods.move_to(ORIGIN)
        self.play(Write(splitting_methods))
        self.wait(5)
        # finish this, I don't know what
        h = MathTex(r"H", font_size=36, color=FG)
        h.move_to(ORIGIN + DOWN * 0.5)
        self.play(Write(h))
        self.wait(5)
        self.play(Transform(h, MathTex(r"H = A + B", font_size=36, color=FG).move_to(h.get_center())))
        self.wait(35)

        self.play(FadeOut(header, splitting_methods, h, *outer_nodes, *lines, circle_1))



# =============================================================================
# S2 -- Motivating Operator Splitting
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

        ahar = MathTex(r"H(q,p) = \frac{p^2}{2m} + \frac{1}{2}m \omega^2q^2 + \lambda q^4", font_size=26, color=FG_DIM)
        ahar.next_to(header, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(Write(ahar))
        self.wait(3)

        # N-Body Hamiltonian in Inertial Cartesian Coordinates
        nonlinear = MathTex(r"\int \frac{dp}{dH} dt \qquad \int \frac{dq}{dH} dt", font_size=44, color=RED)
        nonlinear.move_to(ORIGIN)
        self.play(Write(nonlinear))
        self.wait(15)

        kinetic = MathTex(r"\frac{p^2}{2m}", font_size=44, color=FG).move_to(ORIGIN + LEFT * 2.5)
        potential = MathTex(r"\frac{1}{2}m \omega^2q^2 + \lambda q^4", font_size=44, color=FG).move_to(ORIGIN + RIGHT * 2.5)

        self.play(ReplacementTransform(VGroup(nonlinear, ahar), VGroup(kinetic, potential)), run_time=1.5)

        self.wait(4)



        vec_field_math = MathTex(r"X_H =  X_V + X_V", font_size=36, color=FG)
        self.play(ReplacementTransform(VGroup(kinetic, potential), vec_field_math), run_time=1.5)
        self.wait(2)
        
        m = 1
        omega = 1
        lam = 0.2

        def F(point):
            q, p = point[:2]

            dqdt = p / m
            dpdt = -m * omega**2 * q - 4 * lam * q**3

            return np.array([dqdt, dpdt, 0])

        field = ArrowVectorField(F, x_range=[-3, 3], y_range=[-3, 3]).scale(0.5)
        axes = Axes(x_range=[-3, 3], y_range=[-3, 3]).scale(0.5)
        
        plus = MathTex(r"+", font_size=45, color=FG).move_to(ORIGIN)
        self.play(ReplacementTransform(vec_field_math, VGroup(field, axes)), run_time=1.5)
        # --- KINETIC VECTOR FIELD ---
        def F_T(point):
            q, p = point[:2]
            dqdt = p / m
            dpdt = 0
            return np.array([dqdt, dpdt, 0])

        kinetic_field = ArrowVectorField(F_T, x_range=[-3, 3], y_range=[-3, 3]).move_to(LEFT * 4.5).scale(0.5)
        
        # --- POTENTIAL VECTOR FIELD ---
        def F_V(point):
            q, p = point[:2]
            dqdt = 0
            dpdt = -m * omega**2 * q - 4 * lam * q**3
            return np.array([dqdt, dpdt, 0])

        potential_field = ArrowVectorField(F_V, x_range=[-3, 3], y_range=[-3, 3]).move_to(RIGHT * 4.5).scale(0.5)

        self.wait(5)
        self.play(ReplacementTransform(VGroup(field, axes), VGroup(kinetic_field, potential_field, plus)), run_time=1.5)

        self.wait(10)
        self.play(FadeOut(VGroup(kinetic_field, potential_field, header, plus)))
        # Liouville's Opertator
        Operator = MathTex(r"L_H f = \{f, H\}", font_size=36, color=FG)
        Operator = Operator.move_to(ORIGIN)
        self.play(Write(Operator))
        self.wait(13.0)
        Operator_Expanded = MathTex(r"L_H  = L_T + L_V", font_size=36, color=FG)
        Operator_Expanded.move_to(Operator.get_center())
        self.play(ReplacementTransform(Operator, Operator_Expanded), run_time=1.5)

        self.play((Operator_Expanded.animate.shift(UP * 1.5)))

        z_matrix = MathTex(r"z = \begin{pmatrix} q \\ p \end{pmatrix}")
        z_matrix.next_to(Operator_Expanded, DOWN, buff=0.5)
        self.play(Write(z_matrix))
        self.wait(5)
        differential = MathTex(r"z = e^{h(L_T + L_V)} z_0", font_size=36, color=FG)

        self.play(ReplacementTransform(VGroup(z_matrix, Operator_Expanded), differential), run_time=1.5)

        self.wait(5)

        self.play((differential.animate.shift(UP * 1.5)))

        wrong_commute = MathTex(r"e^{h(L_T + L_V)} \neq e^{hL_T} e^{hL_V}", font_size=36, color=RED)
        wrong_commute.next_to(differential, DOWN, buff=0.5)
        self.play(Write(wrong_commute))
        self.wait(5)

        commute = MathTex(r"[L_T, L_V] \neq 0", font_size=36, color=TEAL)
        commute.next_to(wrong_commute, DOWN, buff=0.5)
        self.play(Write(commute))
        self.wait(5)

        solve_q = MathTex(r"[L_T, L_V]q = -\frac{V'(q)}{m} - 0", font_size=36, color=TEAL)
        solve_q.next_to(commute, DOWN, buff=0.5)
        self.play(Write(solve_q))
        self.wait(5)
        self.play(FadeOut(VGroup(differential, wrong_commute, commute, solve_q)))
        error = MathTex(r"e^{h(L_T + L_V)} = e^{hL_T} e^{hL_V} + \mathcal{O}(h^2)", font_size=36, color=RED)
        error.move_to(ORIGIN)
        self.play(Write(error))
        self.wait(5)    
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
                             ("Deriving Strang Splitting", PURPLE), ("}", FG_DIM)],
                            font_size=28)
        place_header(header)
        self.play(Write(header))

        origional = MathTex(r"H = T + V", font_size=36, color=FG)
        origional.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(Write(origional))
        self.wait(17.0)
        split = MathTex(r"H = A + B", font_size=36, color=FG)
        split.next_to(origional, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(ReplacementTransform(origional, split), run_time=1.5)
        self.wait(24.0)

        self.play(FadeOut(split))



        differential = MathTex(r"\dot{z} = (A+ B)z", font_size=36, color=FG)
        differential.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(differential), run_time=1.5)


        taylor = MathTex(r"z(t+h) = \underbrace{\left[I + h(A+B) + \frac{h^2}{2}(A^2 + AB + BA + B^2) \right]}z + O(h^3)", font_size=36, color=FG)
        taylor.next_to(differential, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(taylor), run_time=1.5)
        self.wait(10.0)

        three_comp = MathTex(r"S(h)= e^{ahA}e^{bhB}e^{ahA}", font_size=34).move_to(ORIGIN + UP * 2)
        self.play(ReplacementTransform(VGroup(taylor, differential), three_comp), font_size=36, color=FG, run_time=1.5)
        
        expand = MathTex(r"e^{ahA} = I + ahA + \frac{a^2h^2}{2}A^2+O(h^3) \qquad e^{bhB} = I bhB + \frac{b^2h^2}{2}B^2+ O(h^2)", font_size=30).move_to(ORIGIN + LEFT * 1.5)
    
        self.play(Write(expand), run_time=1.5)
        self.wait(10.0)

        multiple = MathTex(r"S(h) = I + h(2aA +bB) + O(h^3)", font_size=36)

        self.play(ReplacementTransform(expand, multiple), run_time=1.5)
        self.wait(10.0)

        solution = MathTex(r"I + h(A+B)+ O(h^3)", font_size=36) 
        self.play(multiple.animate.shift(LEFT * 3.0))
        self.wait(1.0)
        solution.next_to(multiple, RIGHT * 1.5, buff=2, aligned_edge=LEFT)
        self.play(ReplacementTransform(three_comp, solution), run_time=1.5)

        self.wait(2)
        a_b = MathTex(r"a = \frac{1}{2} \qquad b = 1").move_to(ORIGIN + UP * 1)

        self.play(ReplacementTransform(VGroup(multiple, solution), a_b), run_time=1.5)

        self.wait(10.0)

        strang = MathTex(r"S(h) = e^{\frac{h}{2}A}e^{hB}e^{\frac{h}{2}A}").move_to(ORIGIN)
        strang.next_to(a_b, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(strang), run_time=1.5)
        self.wait(10.0)
        self.play(FadeOut(VGroup(header, a_b, strang)))

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
                             ("Adjoint Formulism", PURPLE), ("}", FG_DIM)], font_size=30)
        place_header(header)
        self.play(Write(header))
        self.wait(3.0)

        self_adj = MathTex(r"\psi_h = \psi^{\star}_h = (\psi_{-h})^{-1}", font_size=36, color=FG)
        self.play(Write(self_adj))
        self.wait(3.0)

        defining_inverse = MaThTex(r"\psi = x^1 \circ x^2 \text{then} \psi^{\star} = x^2 \circ x^1")

        defining_inverse.next_to(self_adj, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(defining_inverse))
        self.wait(3.0)
        # haven't finished this section yet, but I will continue to work on it later
        ######################################################################
        self.play(FadeOut(header))

# =============================================================================
# S5 -- Higher Order Splitting and Composition
# =============================================================================
class S5(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("52%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Higher Order Splitting and Composition", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))

        strang_text = Tex('"it is a meta-theorem of numerical analysis that second order methods often achieve the right balance between accuracy and complexity." - Strang', font_size=30, color=FG_DIM)
        strang_text.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(strang_text))
        self.wait(10)

        self.play(FadeOut(strang_text))
        strang = MathTex(r"S^{[2]}_h = \varphi_{h/2} \circ \varphi_{h} \circ \varphi_{h/2}", font_size=36, color=FG)
        strang.move_to(ORIGIN + UP * 1.0)
        self.play(Write(strang))

        self.wait(10)

        comp = MathTex(r"\psi_h = S_{\gamma_s h} \circ S_{\gamma_{s-1} h} \circ ... \circ S_{\gamma_1 h}", font_size=36, color=FG)

        comp.next_to(strang, DOWN, buff=0.5, aligned_edge=LEFT)

        self.play(Write(comp))

        self.wait(5)

        self.play(FadeOut(strang))

        self.play(comp.animate.shift(UP * 1.0), FadeOut(strang))
    
        conditions = MathTex(r"\sum_{i=1}^s \gamma_i = 1 \qquad \sum^s_{j=1} \gamma_j^3 = 0", font_size=36, color=FG)
        conditions.next_to(comp, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(conditions))

        self.wait(10)

        sym = MathTex(r"\gamma_1 = \gamma_3")

        gamma = MathTex(r"\gamma_1 = \gamma_3 = \frac{1}{2 - 2^{1/3}} \qquad \gamma_2 = -\frac{2^{1/3}}{2 - 2^{1/3}}", font_size=36, color=FG)
        sym.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        gamma.next_to(sym, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(ReplacementTransform(VGroup(comp, conditions), VGroup(sym, gamma)), run_time=1.5)

        self.wait(10)

        general = Tex(
            r"\begin{align*}"
            r"S_{h}\!\left[2^{k}\right]= S_{\gamma_{1}\, h\!\left[2^{k-2}\right]} \\"
            r"\circ S_{\gamma_{1}\, h\!\left[2^{k-2}\right]} \circ S_{\left(1 - 4\gamma_{1}\right)\, h\!\left[2^{k-2}\right]} \\"
            r"\circ S_{\gamma_{1}\, h\!\left[2^{k-2}\right]} \circ S_{\gamma_{1}\, h\!\left[2^{k-2}\right]} \right]"
            r"\end{align*}", font_size=36, color=FG)

        self.play(ReplacementTransform(VGroup(sym, gamma), general), run_time=1.5)
        self.play(FadeOut(header))

        


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
