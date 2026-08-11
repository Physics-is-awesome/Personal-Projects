"""
universe_force.py
================================================================================
"Can You Destroy a Universe With a Force?" -- a Manim Community Edition video,
staged as a dark-Emacs-buffer walkthrough of the source script/derivation.

Render one scene at a time, e.g.:

    manim -pqh universe_force.py S00_Title
    manim -pqh universe_force.py S05_Modeling

or render everything (see render_all.sh). Each Scene is independent and
produces its own clip; stitch them together in your editor or with ffmpeg
concat once you've laid real voiceover over each one -- the self.wait(...)
calls are placeholders for narration pacing, not a locked timing track.

Scene map (mirrors the script's own \\section structure):
    S00_Title          cold open / title card
    S00b_Outline        org-mode style table of contents
    S01_Introduction    "how much force would it actually take?"
    S02_PlanckForce     c^4/G, the theoretical force ceiling
    S03_StrikingEarth   the Einstein Toolkit experiment + its limits
    S04_Assumptions     the six modeling assumptions (checklist)
    S05_Modeling        Friedmann -> Schwarzschild-de Sitter -> M_crit -> LTB
                         -> Monte Carlo over H0
    S06_Results         the (results) placeholder / energy readout
    S07_Interpreting    stress-energy tensor -> final force + scale comparison
    S08_Questions       "what if gravity isn't instantaneous?"
    S09_Outro           thank-you / buffer saved
================================================================================
"""

import os

import numpy as np
from emacs_theme import *
from manim import *

# Consistent safe content area (stays clear of the top tab bar / bottom
# mode-line, which occupy roughly the outer 0.5 vertical units on each side).
TOP_Y = config.frame_height / 2 - 0.9
BOT_Y = -config.frame_height / 2 + 0.75
LEFT_X = -config.frame_width / 2 + 0.8
RIGHT_X = config.frame_width / 2 - 0.8


def chrome_for(position, tab="universe-force.tex"):
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
# S00 -- TITLE
# =============================================================================
class S00_Title(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(90, seed=1))
        chrome = chrome_for("0%")
        self.add(chrome)

        # A near-empty buffer with a blinking cursor on line 1.
        cursor = Cursor().move_to(UP * 2.6 + LEFT_X * 0 + LEFT * 6.2)
        self.play(FadeIn(cursor))
        self.play(blink(cursor, cycles=2))

        prompt = comment_line("a video script, animated", font_size=24)
        prompt.next_to(cursor, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(prompt), run_time=0.8)
        self.wait(0.3)

        # Quick montage: things fictional characters destroy, scaled up.
        labels = ["a planet", "a star", "a galaxy", "a universe"]
        colors = [BLUE, ORANGE, PURPLE, TEAL]
        radii = [0.35, 0.55, 0.8, 1.05]
        prev = None
        for lbl, col, r in zip(labels, colors, radii):
            circ = Circle(radius=r, color=col, fill_color=col, fill_opacity=0.55)
            circ.move_to(ORIGIN + DOWN * 0.3)
            txt = Text(lbl, font=FONT, font_size=28, color=FG)
            txt.next_to(circ, DOWN, buff=0.35)
            grp = VGroup(circ, txt)
            if prev is None:
                self.play(FadeIn(circ, scale=0.5), Write(txt), run_time=0.5)
            else:
                self.play(ReplacementTransform(prev[0], circ),
                          ReplacementTransform(prev[1], txt), run_time=0.5)
            self.play(Flash(circ, color=RED, flash_radius=r + 0.4, line_length=0.25),
                      circ.animate.set_opacity(0.05), run_time=0.35)
            prev = grp
        self.play(FadeOut(prev, prompt, cursor))

        title1 = Text("Can You Destroy a Universe", font=FONT, font_size=46, color=FG, weight=BOLD)
        title2 = Text("With a Force?", font=FONT, font_size=46, color=TEAL, weight=BOLD)
        title_grp = VGroup(title1, title2).arrange(DOWN, buff=0.35).move_to(ORIGIN)
        self.play(Write(title1), run_time=1.2)
        self.play(Write(title2), run_time=1.0)
        self.wait(1.2)
        self.play(FadeOut(title_grp))


# =============================================================================
# S00b -- OUTLINE  (org-mode style table of contents; gives the viewer a map
# of the video and doubles as a nice demonstration of the buffer conceit)
# =============================================================================
class S00b_Outline(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(50, seed=2))
        chrome = chrome_for("2%")
        self.add(chrome)

        doc_title = code_line([("* ", FG_DIM),
                                ("Can You Destroy a Universe With a Force?", FG, )],
                               font_size=26)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        fit_width(doc_title)
        doc_title.to_edge(UP, buff=1.0).to_edge(LEFT, buff=0.8)
        self.play(Write(doc_title), run_time=1.0)
        self.wait(0.3)

        sections = [
            "Planck Force",
            "Striking the Earth with a Planck Force",
            "Assumptions about a Universe",
            "Modeling a Universe",
            "Results",
            "Interpreting the Results",
            "General Questions",
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
# S01 -- INTRODUCTION
# =============================================================================
class S01_Introduction(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("4%")
        self.add(chrome)

        header = code_line([("\\section", BLUE), ("{", FG_DIM),
                             ("Introduction", PURPLE), ("}", FG_DIM)], font_size=38)
        place_header(header)
        self.play(Write(header))
        self.wait(1)

        lines = prose([
            "Fictional characters blow up planets,",
            "stars, galaxies -- even universes.",
            "None of it is physically realistic.",
            "But it raises one fair question:",
        ], font_size=27)
        fit_width(lines)
        lines.next_to(header, DOWN, buff=0.7, aligned_edge=LEFT)
        for l in lines:
            self.play(Write(l), run_time=0.8)
            self.wait(1)
        self.wait(4.5)

        question = Text("How much force would it actually take?",
                         font=FONT, font_size=32, color=TEAL, weight=BOLD)
        fit_width(question)
        question.next_to(lines, DOWN, buff=0.6, aligned_edge=LEFT)
        box = SurroundingRectangle(question, color=TEAL, buff=0.3, stroke_width=2)
        self.play(Write(question), run_time=1.2)
        self.play(Create(box))
        self.wait(8)
        self.play(FadeOut(header, lines, question, box))


# =============================================================================
# S02 -- PLANCK FORCE
# =============================================================================
class S02_PlanckForce(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("12%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Planck Force", PURPLE), ("}", FG_DIM)], font_size=36)
        place_header(header)
        self.play(Write(header))

        sub = prose([
            "The theoretical highest force that still works",
            "with modern physics.",
        ], font_size=26, color=FG_DIM)
        sub.next_to(header, DOWN, buff=0.4, aligned_edge=LEFT)
        self.play(FadeIn(sub))
        self.wait(3)

        # Build the constants -> Planck units -> Planck force chain.
        consts = MathTex(r"c,\quad G,\quad \hbar", font_size=44, color=FG)
        consts.move_to(UP * 1.1)
        consts_lbl = Text("speed of light  \u00b7  gravitational constant  \u00b7  Planck's constant",
                           font=FONT, font_size=20, color=FG_DIM)
        consts_lbl.next_to(consts, DOWN, buff=0.3)
        self.play(Write(consts))
        self.play(FadeIn(consts_lbl))
        self.wait(4)

        arrow = Arrow(consts.get_bottom() + DOWN * 0.6, DOWN * 1.0, color=FG_DIM, buff=0.1)
        planck_lbl = Text("normalize into Planck units", font=FONT, font_size=22, color=FG_DIM)
        planck_lbl.next_to(arrow, RIGHT, buff=0.3)
        self.play(GrowArrow(arrow), FadeIn(planck_lbl))
        self.wait(46)

        formula = MathTex(r"F_P = \frac{c^4}{G}", font_size=64, color=TEAL)
        formula.move_to(DOWN * 1.2)
        fbox = SurroundingRectangle(formula, color=TEAL, buff=0.35, stroke_width=2)
        self.play(FadeOut(consts, consts_lbl, arrow, planck_lbl))
        self.play(Write(formula))
        self.play(Create(fbox))
        self.wait(4)

        value = MathTex(r"F_P \approx 2\times 10^{44}\ \text{N}", font_size=44, color=ORANGE)
        value.next_to(fbox, DOWN, buff=0.55)
        self.play(Write(value))
        self.wait(5.5)

        note = comment_line("disputed: some derive F_P / 4 from General Relativity", font_size=22)
        note.next_to(value, DOWN, buff=0.45)
        #self.play(FadeIn(note))
        self.wait(2.0)

        unknown = Text("Until quantum gravity is solved, nobody truly knows.",
                        font=FONT, font_size=24, color=FG_DIM)
        unknown.next_to(note, DOWN, buff=0.4)
        #self.play(FadeIn(unknown))
        self.wait(32)

        self.play(FadeOut(VGroup(header, sub, formula, fbox, value)))


# =============================================================================
# S03 -- STRIKING THE EARTH
# =============================================================================
class S03_StrikingEarth(Scene):
    def construct(self):
        self.camera.background_color = BG
        self.add(starfield(70, seed=3))
        chrome = chrome_for("22%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Striking the Earth with a Planck Force", PURPLE), ("}", FG_DIM)],
                            font_size=28)
        place_header(header)
        self.play(Write(header))
        self.wait(15)

        setup = prose([
            "Simulated in the Einstein Toolkit -- an",
            "open-source numerical-relativity codebase --",
            "with a custom \"thorn\" that strikes a point",
            "on an Earth-like neutron-star model.",
        ], font_size=22)
        fit_width(setup)
        setup.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(setup))
        self.wait(1.0)
        self.play(FadeOut(setup))

        # -- Earth + force strike --
        earth = Circle(radius=1.1, color=BLUE, fill_color=BLUE, fill_opacity=0.5)
        earth.move_to(LEFT * 2 + DOWN * 0.3)
        earth_lbl = Text("Earth", font=FONT, font_size=20, color=FG_DIM)
        earth_lbl.next_to(earth, DOWN, buff=0.2)
        strike_arrow = Arrow(earth.get_left() + LEFT * 2.6, earth.get_left(),
                              color=RED, buff=0.05, stroke_width=6)
        strike_lbl = MathTex(r"F_P", font_size=36, color=RED)
        strike_lbl.next_to(strike_arrow, UP, buff=0.15)

        self.play(FadeIn(earth), FadeIn(earth_lbl))
        self.play(GrowArrow(strike_arrow), Write(strike_lbl))
        self.wait(2)
        self.play(FadeOut(strike_arrow, strike_lbl, earth, earth_lbl))
        # Placeholder "movie" panel, echoing the script's own "(Insert movie)".
        panel = Rectangle(width=4.6, height=2.6, color=FG_DIM, stroke_width=2)
        panel.move_to(RIGHT * 3.1 + DOWN * 0.3)
        frame_dir = "/home/ajcason/Videos/frames_Planck_Force"
        frames = sorted(os.listdir(frame_dir))
        for f in frames:
            img = ImageMobject(os.path.join(frame_dir, f))
            img.scale(0.75)
            self.add(img)
            self.wait(1/30) # 60 FPS
            self.remove(img)
        self.play(Create(panel))
        
        self.wait(0.5)

        # Strike -> relativistic explosion -> gravitational collapse.
        self.play(Flash(earth, color=RED, flash_radius=1.6, num_lines=20), run_time=0.4)
        shock = Circle(radius=1.1, color=RED, stroke_width=3).move_to(earth.get_center())
        #self.play(earth.animate.set_fill(opacity=0.15),
                   #shock.animate.scale(3.2).set_stroke(opacity=0), run_time=0.9)
       #self.remove(shock)
       #self.play(FadeOut(strike_arrow, strike_lbl), run_time=0.35)

        collapse_lbl = Text("gravity wins -> collapses to a point", font=FONT, font_size=20, color=FG_DIM)
        collapse_lbl.next_to(earth, UP, buff=0.6)
        #self.play(FadeIn(collapse_lbl))
        #self.play(earth.animate.scale(0.02).set_fill(opacity=1, color=YELLOW), run_time=1.3,
        #           rate_func=rush_into)
        #self.wait(0.5)
       #self.play(FadeOut(collapse_lbl))

        unknown_q = Text("black hole?  neutron matter?  fused particles?",
                          font=FONT, font_size=20, color=FG_DIM)
        unknown_q.move_to(earth.get_center() + UP * 0.7)
       #self.play(FadeIn(unknown_q))
        
        self.play(FadeOut(panel))
       #self.play(FadeOut(VGroup(panel)))

        # -- The superluminal-velocity numerical error --
        err_header = Text("the problem:", font=FONT, font_size=26, color=FG_DIM)
        err_header.to_edge(UP, buff=1.1)
        err_header.to_edge(LEFT, buff=0.8)
        self.play(FadeIn(err_header))

        err_box = Rectangle(width=10.6, height=1.35, color=RED, stroke_width=2,
                             fill_color="#3B2C2E", fill_opacity=1)
        err_box.next_to(err_header, DOWN, buff=0.35, aligned_edge=LEFT)
        err_txt1 = Text("*compilation error*", font=FONT, font_size=20, color=RED, weight=BOLD)
        err_txt2 = Text("particle speed exceeds c -> mass becomes imaginary",
                         font=FONT, font_size=18, color=RED)
        err_grp = VGroup(err_txt1, err_txt2).arrange(DOWN, buff=0.14)
        fit_width(err_grp, err_box.width - 0.6)
        err_grp.move_to(err_box.get_center())
        self.play(Create(err_box), Write(err_grp))
        self.wait(1.0)

        fix_note = prose([
            "Simplified: 1/8th of the Earth (symmetry), 2D, finer mesh.",
        ], font_size=20, color=FG_DIM)
        fit_width(fix_note)
        fix_note.next_to(err_box, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(fix_note))
        self.wait(5)

        # Shrinking time-step countdown.
        dt_lbl = Text("time step \u0394t", font=FONT, font_size=22, color=FG_DIM)
        dt_val = MathTex(r"1\times 10^{-6}\ \text{s}", font_size=34, color=ORANGE)
        dt_grp = VGroup(dt_lbl, dt_val).arrange(RIGHT, buff=0.4)
        dt_grp.next_to(fix_note, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(dt_grp))
        for exp in [-8, -10, -13]:
            new_val = MathTex(rf"1\times 10^{{{exp}}}\ \text{{s}}", font_size=34, color=ORANGE)
            new_val.move_to(dt_val)
            self.play(ReplacementTransform(dt_val, new_val), run_time=0.5)
            dt_val = new_val
            self.wait(0.15)
        still_err = Text("still: velocity > c", font=FONT, font_size=20, color=RED)
        still_err.next_to(dt_grp, DOWN, buff=0.35, aligned_edge=LEFT)
        self.play(FadeIn(still_err))
        self.wait(4.0)

        self.play(FadeOut(VGroup(err_header, err_box, err_grp, fix_note, dt_grp, still_err)))

        # Conclusion: uncertain, "mini-star" guess.
        guess = prose([
            "Best guess: it collapses, but probably not",
            "into a black hole -- more likely a short-lived",
            "\"mini-star\" that eventually explodes back out.",
            "Can't confirm it without a bigger simulation.",
        ], font_size=24, aligned_edge=ORIGIN)
        fit_width(guess)
        guess.move_to(ORIGIN)
        self.play(Write(guess), run_time=2.0)
        self.wait(3)
        self.play(FadeOut(guess))


# =============================================================================
# S04 -- ASSUMPTIONS ABOUT A UNIVERSE
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


class S04_Assumptions(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("38%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Assumptions about a Universe", PURPLE), ("}", FG_DIM)], font_size=30)
        place_header(header)
        self.play(Write(header))
        self.wait(15.0)

        display_items = [
            "Universe = the observable universe, ~93 Gly across.",
            "Spacetime is treated as flat (curvature is tiny).",
            "Dark energy is constant (vacuum-energy model).",
            "Dark matter uses both Hubble's and SH0ES's estimates.",
            "Density is isotropic -- roughly constant everywhere.",
            "The force is \"magic\" energy from outside the universe.",
        ]
        icons = _assumption_icons()

        rows = VGroup()
        for i, text in enumerate(display_items):
            row = checklist_item(text, checked=False, font_size=22)
            rows.add(row)
        rows.arrange(DOWN, buff=0.42, aligned_edge=LEFT)
        max_text_width = (RIGHT_X - LEFT_X) - 1.3   # reserve room for icons
        if rows.width > max_text_width:
            rows.scale_to_fit_width(max_text_width)
        rows.next_to(header, DOWN, buff=0.6, aligned_edge=LEFT)

        for icon, row in zip(icons, rows):
            icon.next_to(row, RIGHT, buff=0.4)
            icon.move_to([icon.get_center()[0], row.get_center()[1], 0])

        for i, (row, icon) in enumerate(zip(rows, icons)):
            self.play(FadeIn(row, shift=RIGHT * 0.2), FadeIn(icon, scale=0.4), run_time=0.5)
            checked = checklist_item(display_items[i], checked=True, font_size=22)
            checked.move_to(row, aligned_edge=LEFT)
            self.play(ReplacementTransform(row, checked), run_time=0.35)
            rows.submobjects[i] = checked
            self.wait(13)

        self.wait(6)
        self.play(FadeOut(header), FadeOut(VGroup(*rows)), FadeOut(VGroup(*icons)))


# =============================================================================
# S05 -- MODELING A UNIVERSE
# =============================================================================
class S05_Modeling(Scene):
    def construct(self):
        self.camera.background_color = BG
        chrome = chrome_for("52%")
        self.add(chrome)

        header = code_line([("\\subsection", BLUE), ("{", FG_DIM),
                             ("Modeling a Universe", PURPLE), ("}", FG_DIM)], font_size=32)
        place_header(header)
        self.play(Write(header))
        self.wait(4.0)
        why = prose([
            "The universe is mostly empty space -- you can't just",
            "\"punch\" it. So instead: work backward from energy.",
        ], font_size=24)
        why.next_to(header, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(FadeIn(why))
        self.wait(46.0)
        self.play(FadeOut(why))

        # --- Friedmann equation ---
        step_lbl = Text("start: the Friedmann equation", font=FONT, font_size=22, color=FG_DIM)
        step_lbl.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        friedmann = MathTex(
            r"H(a)^2 = H_0^2\left(\Omega_{r0}a^{-4} + \Omega_{m0}a^{-3} + \Omega_{\Lambda 0}\right)",
            font_size=34, color=FG)
        friedmann.next_to(step_lbl, DOWN, buff=0.4)
        self.play(FadeIn(step_lbl))
        self.play(Write(friedmann), run_time=1.5)
        self.wait(4)

        compress = MathTex(r"H(a)^2 = H_0^2\, E(a)", font_size=34, color=TEAL)
        compress.move_to(friedmann)
        compress_note = comment_line("compress the parenthetical into E(a)", font_size=18)
        compress_note.next_to(compress, DOWN, buff=0.3)
        self.play(ReplacementTransform(friedmann, compress))
        self.play(FadeIn(compress_note))
        self.wait(5)
        self.play(FadeOut(step_lbl, compress, compress_note))

        # --- Schwarzschild-de Sitter ---
        step2 = Text("Schwarzschild-de Sitter metric function (flat, expanding space)",
                      font=FONT, font_size=20, color=FG_DIM)
        step2.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        fr = MathTex(r"f(r) = 1 - \frac{2GM}{c^2 r} - \frac{\Lambda r^2}{3}", font_size=36, color=FG)
        fr.next_to(step2, DOWN, buff=0.4)
        self.play(FadeIn(step2))
        self.play(Write(fr), run_time=1.2)
        self.wait(5)

        deriv_note = Text("set f'(r) = 0  ->  the radius where the universe is static",
                           font=FONT, font_size=20, color=FG_DIM)
        deriv_note.next_to(fr, DOWN, buff=0.45)
        self.play(FadeIn(deriv_note))
        self.wait(5)

        rc = MathTex(r"r_c = \left(\frac{3GM}{\Lambda c^2}\right)^{1/3}", font_size=40, color=ORANGE)
        rc.next_to(deriv_note, DOWN, buff=0.45)
        rc_box = SurroundingRectangle(rc, color=ORANGE, buff=0.25, stroke_width=2)
        self.play(Write(rc))
        self.play(Create(rc_box))
        self.wait(5)
        self.play(FadeOut(step2, fr, deriv_note, rc, rc_box))

        # --- Critical mass ---
        step3 = Text("solve for M at r = R (today's observable radius)",
                      font=FONT, font_size=20, color=FG_DIM)
        step3.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        mcrit = MathTex(
            r"M_{crit} = \frac{\Lambda c^2}{3G}R_{obs}^3 = \frac{\Omega_{\Lambda 0}H_0^2}{G}R_{obs}^3",
            font_size=36, color=TEAL)
        mcrit.next_to(step3, DOWN, buff=0.45)
        mcrit_box = SurroundingRectangle(mcrit, color=TEAL, buff=0.25, stroke_width=2)
        self.play(FadeIn(step3))
        self.play(Write(mcrit), run_time=1.3)
        self.play(Create(mcrit_box))
        self.wait(5)
        self.play(FadeOut(step3, mcrit, mcrit_box))

        # --- LTB metric ---
        step4 = Text("exact solution: Lemaitre-Tolman-Bondi metric (no symmetry assumed)",
                      font=FONT, font_size=19, color=FG_DIM)
        step4.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        ltb = MathTex(
            r"ds^2 = -c^2dt^2 + \frac{R'(r,t)^2}{1 + 2E(r)/c^2}dr^2 + R(r,t)^2\,d\Omega^2",
            font_size=32, color=FG)
        ltb.next_to(step4, DOWN, buff=0.4)
        self.play(FadeIn(step4))
        self.play(Write(ltb), run_time=1.5)
        self.wait(5)

        algebra_note = comment_line("(algebraic manipulation)", font_size=20)
        algebra_note.next_to(ltb, DOWN, buff=0.4)
        self.play(FadeIn(algebra_note))
        self.wait(5)

        punchline = Text("result: only the total mass of the shell matters -- not its shape.",
                          font=FONT, font_size=22, color=GREEN)
        punchline.next_to(algebra_note, DOWN, buff=0.4)
        self.play(Write(punchline), run_time=1.0)
        self.wait(5)

        self.play(FadeOut(step4, ltb, algebra_note, punchline))

        # --- Monte Carlo over H0 (Hubble tension) ---
        step5 = Text("plug in real numbers: Monte Carlo over H\u2080 uncertainty",
                      font=FONT, font_size=22, color=FG_DIM)
        step5.next_to(header, DOWN, buff=0.55, aligned_edge=LEFT)
        self.play(FadeIn(step5))

        axes = Axes(
            x_range=[64, 76, 4], y_range=[0, 1.1, 0.5],
            x_length=8.6, y_length=3.1,
            axis_config={"color": FG_DIM, "include_tip": False, "font_size": 18},
        )
        axes.next_to(step5, DOWN, buff=0.35)
        xlabel = Text("H\u2080  (km/s/Mpc)", font=FONT, font_size=18, color=FG_DIM)
        xlabel.next_to(axes.x_axis, DOWN, buff=0.15)
        self.play(Create(axes), FadeIn(xlabel))

        def gauss(mu, sigma):
            return lambda x: np.exp(-((x - mu) ** 2) / (2 * sigma ** 2))

        curve_h = axes.plot(gauss(67.4, 0.7), x_range=[64, 76], color=BLUE)
        curve_s = axes.plot(gauss(73.0, 1.2), x_range=[64, 76], color=RED)
        lbl_h = Text("Hubble", font=FONT, font_size=18, color=BLUE).next_to(curve_h, UP, buff=0.15).shift(LEFT * 1.6)
        lbl_s = Text("SH0ES", font=FONT, font_size=18, color=RED).next_to(curve_s, UP, buff=0.15).shift(RIGHT * 1.4)
        self.play(Create(curve_h), Create(curve_s))
        self.play(FadeIn(lbl_h), FadeIn(lbl_s))
        self.wait(5)

        rng = np.random.default_rng(7)
        samples_h = rng.normal(67.4, 0.7, 14)
        samples_s = rng.normal(73.0, 1.2, 14)
        dots = VGroup()
        for x in samples_h:
            d = Dot(axes.c2p(x, 0), radius=0.035, color=BLUE, fill_opacity=0.85)
            dots.add(d)
        for x in samples_s:
            d = Dot(axes.c2p(x, 0), radius=0.035, color=RED, fill_opacity=0.85)
            dots.add(d)
        self.play(LaggedStart(*[FadeIn(d, shift=UP * 0.15) for d in dots], lag_ratio=0.05), run_time=1.4)
        mc_note = Text("Bayesian resampling -> a distribution of possible answers",
                        font=FONT, font_size=18, color=FG_DIM)
        mc_note.next_to(axes, DOWN, buff=0.55)
        self.play(FadeIn(mc_note))
        self.wait(13)

        self.play(FadeOut(VGroup(header, step5, axes, xlabel, curve_h, curve_s,
                                  lbl_h, lbl_s, dots, mc_note)))


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
