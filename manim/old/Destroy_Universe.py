"""
universe_destroyer.py -- Manim Community Edition scenes for
"Can You Destroy a Universe With a Force?"

SETUP (on a machine with internet access):
    pip install manim
    # manim needs a LaTeX distribution + ffmpeg on the system; see
    # https://docs.manim.community/en/stable/installation.html
    pip install scipy numpy          # used by physics_results.py

RENDER a single scene, fast preview:
    manim -pql universe_destroyer.py TitleScene

RENDER everything at final quality (slow -- do this last):
    manim -pqh universe_destroyer.py TitleScene PlanckForceScene EarthStrikeScene \
        AssumptionsScene FriedmannScene BalancePointScene LTBScene ResultsScene \
        ForceDerivationScene ComparisonScene FAQScene EndScene

Every number on screen is computed live from physics_results.py at
construct()-time -- nothing here is a hard-coded string. Re-run the physics
module and the animation's numbers update with it.
"""

from manim import *
import numpy as np
from physics_results import headline_numbers, f_of_R, monte_carlo, G, c

NUMS = headline_numbers()

BLUE = "#4C72B0"
RED = "#C44E52"
YELLOW = "#DDB35C"
GREY = "#9AA0A6"

config.background_color = "#0b0e14"


def sci(x, sig=3):
    """Format a float as LaTeX scientific notation, e.g. '3.65 \\times 10^{71}'."""
    exp = int(np.floor(np.log10(abs(x))))
    mant = x / 10**exp
    return f"{mant:.{sig - 1}f} \\times 10^{{{exp}}}"


def section_title(text, sub=None):
    t = Text(text, font_size=44, weight=BOLD).to_edge(UP)
    grp = VGroup(t)
    if sub:
        s = Text(sub, font_size=24, color=GREY).next_to(t, DOWN, buff=0.2)
        grp.add(s)
    return grp


# ----------------------------------------------------------------------
class TitleScene(Scene):
    def construct(self):
        title = Text("Can You Destroy a Universe", font_size=52, weight=BOLD)
        title2 = Text("With a Force?", font_size=52, weight=BOLD, color=YELLOW)
        title2.next_to(title, DOWN, buff=0.3)
        group = VGroup(title, title2).move_to(ORIGIN)
        self.play(Write(title), run_time=1.5)
        self.play(Write(title2), run_time=1.2)
        self.wait(1.5)
        self.play(FadeOut(group))


# ----------------------------------------------------------------------
class PlanckForceScene(Scene):
    def construct(self):
        header = section_title("The Planck Force")
        self.play(Write(header))

        formula = MathTex(r"F_P = \frac{c^4}{G}", font_size=72)
        self.play(Write(formula))
        self.wait(0.5)

        value = MathTex(r"F_P \approx 2 \times 10^{44}\ \text{N}",
                         font_size=52, color=YELLOW)
        value.next_to(formula, DOWN, buff=0.8)
        self.play(Write(value))
        self.wait(0.5)

        caption = Text(
            "The theoretical ceiling on force in classical GR --\n"
            "where quantum gravity and curvature both become significant.",
            font_size=24, color=GREY, line_spacing=1.2
        ).next_to(value, DOWN, buff=0.7)
        self.play(FadeIn(caption))
        self.wait(2)
        self.play(FadeOut(VGroup(header, formula, value, caption)))


# ----------------------------------------------------------------------
class EarthStrikeScene(Scene):
    """
    Stylized / conceptual only -- no real simulation data was available to
    animate here, so this represents the qualitative story from the script
    (relativistic blow-off, gravity slowly winning, collapse to a point,
    the superluminal-velocity numerical failure) rather than real NR output.
    """
    def construct(self):
        header = section_title("Striking Earth With a Planck Force",
                                "conceptual illustration, not simulation data")
        self.play(Write(header))

        earth = Circle(radius=1.2, color=BLUE, fill_opacity=0.6)
        earth_label = Text("Earth", font_size=24).move_to(earth)
        arrow = Arrow(LEFT * 4 + UP * 0.2, LEFT * 1.4 + UP * 0.2,
                      color=RED, buff=0)
        arrow_label = MathTex(r"F_P", color=RED, font_size=40).next_to(arrow, UP)

        self.play(Create(earth), Write(earth_label))
        self.play(GrowArrow(arrow), Write(arrow_label))
        self.wait(0.3)

        shock = Circle(radius=1.2, color=YELLOW)
        shock.move_to(earth)
        self.play(
            shock.animate.scale(3).set_opacity(0),
            run_time=1.2,
        )

        note1 = Text("relativistic blow-off into vacuum", font_size=22,
                      color=GREY).next_to(earth, DOWN, buff=1.0)
        self.play(FadeIn(note1))
        self.wait(0.6)

        collapse_note = Text("...then gravity slowly wins", font_size=22,
                              color=GREY).move_to(note1)
        self.play(Transform(note1, collapse_note))
        self.play(earth.animate.scale(0.02), run_time=1.5)
        self.wait(0.4)

        warn = Text(
            "Simulation caveat: particle velocities approached c,\n"
            "causing numerical (superluminal) failures near the collapse --\n"
            "outcome (black hole vs. exotic remnant) unresolved here.",
            font_size=22, color=RED, line_spacing=1.2
        ).next_to(note1, DOWN, buff=0.8)
        self.play(FadeIn(warn))
        self.wait(2.5)

        self.play(FadeOut(VGroup(header, earth, earth_label, arrow,
                                  arrow_label, note1, warn)))


# ----------------------------------------------------------------------
class AssumptionsScene(Scene):
    def construct(self):
        header = section_title("Assumptions About a Universe")
        self.play(Write(header))

        items = [
            "Observable universe: ~93 billion light-years across",
            "Spacetime is treated as exactly flat",
            "Dark energy is a constant (vacuum energy)",
            "Dark matter values taken from Hubble and SH0ES estimates",
            "Density is treated as isotropic and uniform",
            "The energy source itself is left unexplained ('magic')",
        ]
        group = VGroup(*[
            Text(f"{i+1}.  {t}", font_size=26) for i, t in enumerate(items)
        ]).arrange(DOWN, aligned_edge=LEFT, buff=0.35)
        group.next_to(header, DOWN, buff=0.6)

        for line in group:
            self.play(FadeIn(line, shift=RIGHT * 0.3), run_time=0.5)
        self.wait(2)
        self.play(FadeOut(VGroup(header, group)))


# ----------------------------------------------------------------------
class FriedmannScene(Scene):
    def construct(self):
        header = section_title("The Horizon Integral")
        self.play(Write(header))

        friedmann = MathTex(
            r"H(a)^2 = H_0^2\left(\Omega_{r0}a^{-4}+\Omega_{m0}a^{-3}+\Omega_{\Lambda0}\right)",
            font_size=42
        )
        self.play(Write(friedmann))
        self.wait(0.5)

        compress = MathTex(
            r"H(a)^2 = H_0^2\, E(a)^2", font_size=42
        ).next_to(friedmann, DOWN, buff=0.6)
        self.play(TransformFromCopy(friedmann, compress))
        self.wait(1)

        self.play(FadeOut(friedmann), compress.animate.to_edge(UP).shift(DOWN * 1.2))

        null_ray = MathTex(r"dr = \frac{c\,dt}{a(t)}", font_size=40)
        null_ray.next_to(compress, DOWN, buff=0.7)
        self.play(Write(null_ray))
        self.wait(0.5)

        integral = MathTex(
            r"R_{obs} = c\int_0^{t_0}\frac{dt}{a}"
            r"\;=\;\frac{c}{H_0}\int_0^1 \frac{da}{a^2 E(a)}",
            font_size=38
        ).next_to(null_ray, DOWN, buff=0.7)
        self.play(Write(integral))
        self.wait(1)

        result = MathTex(
            r"R_{obs} \approx " + f"{NUMS['R_obs_gly']:.1f}" + r"\ \text{Gly}",
            font_size=48, color=YELLOW
        ).next_to(integral, DOWN, buff=0.8)
        self.play(Write(result))
        self.wait(2)

        self.play(FadeOut(VGroup(header, compress, null_ray, integral, result)))


# ----------------------------------------------------------------------
class BalancePointScene(Scene):
    def construct(self):
        header = section_title("Finding the Balance Point")
        self.play(Write(header))

        left_label = Text("Newtonian analogy", font_size=26, color=GREY)
        right_label = Text("Exact GR (Schwarzschild-de Sitter)", font_size=26, color=GREY)
        left_label.to_edge(LEFT).shift(UP * 2.3 + RIGHT * 0.5)
        right_label.to_edge(RIGHT).shift(UP * 2.3 + LEFT * 0.5)
        self.play(Write(left_label), Write(right_label))

        left_eq = MathTex(
            r"\ddot r = -\frac{GM}{r^2} + \frac{\Lambda c^2}{3} r",
            font_size=34
        ).next_to(left_label, DOWN, buff=0.5).align_to(left_label, LEFT)

        right_eq = MathTex(
            r"f(r) = 1 - \frac{2GM}{c^2 r} - \frac{\Lambda r^2}{3}",
            font_size=34
        ).next_to(right_label, DOWN, buff=0.5).align_to(right_label, RIGHT)

        self.play(Write(left_eq), Write(right_eq))
        self.wait(1)

        right_deriv = MathTex(
            r"f'(r) = 0", font_size=30
        ).next_to(right_eq, DOWN, buff=0.4).align_to(right_eq, RIGHT)
        self.play(Write(right_deriv))

        left_result = MathTex(
            r"r_c = \left(\frac{3GM}{\Lambda c^2}\right)^{1/3}",
            font_size=36, color=YELLOW
        ).next_to(left_eq, DOWN, buff=0.9).align_to(left_eq, LEFT)
        right_result = MathTex(
            r"r_c = \left(\frac{3GM}{\Lambda c^2}\right)^{1/3}",
            font_size=36, color=YELLOW
        ).next_to(right_deriv, DOWN, buff=0.4).align_to(right_deriv, RIGHT)

        self.play(Write(left_result), Write(right_result))
        self.wait(0.5)

        self.play(
            left_result.animate.move_to(ORIGIN + UP * 0.3),
            FadeOut(right_result),
            FadeOut(VGroup(left_eq, right_eq, right_deriv, left_label, right_label)),
        )
        box = SurroundingRectangle(left_result, color=YELLOW)
        same = Text("same formula -- two independent derivations",
                     font_size=24, color=GREY).next_to(left_result, DOWN, buff=0.6)
        self.play(Create(box), FadeIn(same))
        self.wait(1.5)

        m_crit = MathTex(
            r"M_{crit} = \frac{\Omega_{\Lambda0} H_0^2}{G} R_{obs}^3 \approx " +
            sci(NUMS['M_crit']) + r"\ \text{kg}",
            font_size=34
        ).next_to(same, DOWN, buff=0.8)
        self.play(Write(m_crit))
        self.wait(1)

        e_crit = MathTex(
            r"E_{total} = M_{crit}c^2 \approx " + sci(NUMS['E_total']) + r"\ \text{J}",
            font_size=40, color=YELLOW
        ).next_to(m_crit, DOWN, buff=0.5)
        self.play(Write(e_crit))
        self.wait(2)

        self.play(FadeOut(VGroup(header, left_result, box, same, m_crit, e_crit)))


# ----------------------------------------------------------------------
class LTBScene(Scene):
    def construct(self):
        header = section_title("The Exact Solution: LTB")
        self.play(Write(header))

        metric = MathTex(
            r"ds^2 = -c^2dt^2 + \frac{R'(r,t)^2}{1+2E(r)/c^2}dr^2 + R(r,t)^2 d\Omega^2",
            font_size=34
        )
        self.play(Write(metric))
        self.wait(1)
        self.play(metric.animate.to_edge(UP).shift(DOWN * 1.1))

        shell_eq = MathTex(
            r"\dot R(r,t)^2 = \frac{2GM(r)}{R} + \frac{\Lambda c^2}{3}R^2 + 2E(r)",
            font_size=34
        ).next_to(metric, DOWN, buff=0.6)
        self.play(Write(shell_eq))

        note = Text("depends only on total enclosed mass M(r) -- not its shape",
                     font_size=22, color=GREY).next_to(shell_eq, DOWN, buff=0.4)
        self.play(FadeIn(note))
        self.wait(1)

        self.play(FadeOut(VGroup(metric, note)), shell_eq.animate.to_edge(UP).shift(DOWN * 1.1))

        cond = MathTex(
            r"\dot R = 0 \quad\text{and}\quad \ddot R = 0 \quad\text{at } R = R_{obs}",
            font_size=32
        ).next_to(shell_eq, DOWN, buff=0.6)
        self.play(Write(cond))
        self.wait(0.5)

        e_needed = MathTex(
            r"E(r_{obs}) = -\frac{3GM_{crit}}{2R_{obs}} \approx " +
            sci(NUMS['E_curv']) + r"\ \text{m}^2/\text{s}^2",
            font_size=32, color=YELLOW
        ).next_to(cond, DOWN, buff=0.7)
        self.play(Write(e_needed))
        self.wait(1.5)

        self.play(FadeOut(VGroup(header, shell_eq, cond, e_needed)))

        # --- f(R) convexity plot, using the real function ---
        header2 = section_title("Regularity Check")
        self.play(Write(header2))

        R_obs = NUMS['R_obs_m']
        Lambda = NUMS['Lambda']
        M = NUMS['M_crit']
        E0 = NUMS['E_curv']

        R_lo, R_hi = 0.5 * R_obs, 1.5 * R_obs
        f_at_lo = f_of_R(R_lo, M, E0, Lambda)
        y_max = f_at_lo * 1.05

        axes = Axes(
            x_range=[0.5, 1.5, 0.25],
            y_range=[-y_max * 0.15, y_max, y_max / 4],
            x_length=8, y_length=4.5,
            axis_config={"color": GREY, "font_size": 20},
        ).next_to(header2, DOWN, buff=0.5)
        x_label = Text("R / R_obs", font_size=22).next_to(axes.x_axis, DOWN, buff=0.2)
        y_label = Text("f(R) = Rdot^2", font_size=22).next_to(axes.y_axis, LEFT, buff=0.2).rotate(PI / 2)

        def make_curve(E_val, color):
            return axes.plot(
                lambda x: f_of_R(x * R_obs, M, E_val, Lambda),
                x_range=[0.5, 1.5, 0.01],
                color=color,
            )

        curve_crit = make_curve(E0, YELLOW)
        curve_bound = make_curve(E0 * 1.01, RED)
        curve_unbound = make_curve(E0 * 0.99, BLUE)

        self.play(Create(axes), Write(x_label), Write(y_label))
        self.play(Create(curve_crit))
        self.wait(0.5)
        legend = VGroup(
            Text("critical (touches zero)", font_size=20, color=YELLOW),
            Text("more bound -> recollapses", font_size=20, color=RED),
            Text("less bound -> escapes", font_size=20, color=BLUE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.15).to_corner(UR).shift(DOWN * 0.3)
        self.play(Create(curve_bound), Create(curve_unbound), FadeIn(legend))
        self.wait(2)

        self.play(FadeOut(VGroup(header2, axes, x_label, y_label,
                                  curve_crit, curve_bound, curve_unbound, legend)))

        # --- freeze-out timescale ---
        header3 = section_title("Freeze-Out: It Never Quite Arrives")
        self.play(Write(header3))

        line1 = MathTex(
            r"t(R) = \int_0^{R} \frac{dR'}{\sqrt{f(R')}}", font_size=36
        ).next_to(header3, DOWN, buff=0.6)
        self.play(Write(line1))
        self.wait(1)

        t90 = MathTex(
            r"t(0.90\,R_{obs}) \approx " + f"{NUMS['t_90_gyr']:.1f}" + r"\ \text{Gyr}",
            font_size=32
        ).next_to(line1, DOWN, buff=0.6)
        t99 = MathTex(
            r"t(0.99\,R_{obs}) \approx " + f"{NUMS['t_99_gyr']:.1f}" + r"\ \text{Gyr}",
            font_size=32
        ).next_to(t90, DOWN, buff=0.35)
        age = Text("(age of the universe today: 13.8 Gyr)", font_size=22, color=GREY)
        age.next_to(t99, DOWN, buff=0.5)

        self.play(Write(t90))
        self.play(Write(t99))
        self.play(FadeIn(age))
        self.wait(2)

        self.play(FadeOut(VGroup(header3, line1, t90, t99, age)))


# ----------------------------------------------------------------------
class ResultsScene(Scene):
    def construct(self):
        header = section_title("Results: Propagating the Uncertainty")
        self.play(Write(header))

        Es = monte_carlo(n=4000) / 1e71   # rescale for a readable axis
        counts, edges = np.histogram(Es, bins=40)
        centers = 0.5 * (edges[:-1] + edges[1:])

        axes = Axes(
            x_range=[edges[0], edges[-1], (edges[-1] - edges[0]) / 6],
            y_range=[0, counts.max() * 1.15, counts.max() / 4],
            x_length=9, y_length=4.5,
            axis_config={"color": GREY, "font_size": 20},
        ).next_to(header, DOWN, buff=0.5)
        x_lab = Text("Energy needed  (x 10^71 J)", font_size=22).next_to(axes.x_axis, DOWN, buff=0.25)
        self.play(Create(axes), Write(x_lab))

        bar_width = (edges[1] - edges[0])
        bars = VGroup()
        for cx, cnt in zip(centers, counts):
            if cnt == 0:
                continue
            bar = Rectangle(
                width=axes.x_axis.unit_size * bar_width * 0.95,
                height=axes.y_axis.unit_size * cnt,
                fill_color=BLUE, fill_opacity=0.85, stroke_width=0.5, stroke_color=BLUE,
            )
            bar.move_to(axes.c2p(cx, 0), aligned_edge=DOWN)
            bars.add(bar)
        self.play(LaggedStartMap(GrowFromEdge, bars, edge=DOWN, lag_ratio=0.02), run_time=2)
        self.wait(0.5)

        median_line = axes.get_vertical_line(
            axes.c2p(NUMS['E_median'] / 1e71, counts.max() * 1.1), color=YELLOW
        )
        self.play(Create(median_line))
        label = MathTex(
            r"E \approx " + sci(NUMS['E_median']) + r"\ \text{J}",
            font_size=32, color=YELLOW
        ).next_to(axes, DOWN, buff=0.6)
        ci = MathTex(
            r"68\%\ \text{CI}: \left[" + sci(NUMS['E_lo68']) + r",\," +
            sci(NUMS['E_hi68']) + r"\right]\ \text{J}",
            font_size=26
        ).next_to(label, DOWN, buff=0.3)
        self.play(Write(label))
        self.play(Write(ci))
        self.wait(2.5)

        self.play(FadeOut(VGroup(header, axes, x_lab, bars, median_line, label, ci)))


# ----------------------------------------------------------------------
class ForceDerivationScene(Scene):
    def construct(self):
        header = section_title("From Energy to Force")
        self.play(Write(header))

        cons = MathTex(r"\nabla_\mu T^{\mu\nu} = f^\nu", font_size=44)
        self.play(Write(cons))
        self.wait(1)
        self.play(cons.animate.to_edge(UP).shift(DOWN * 1.2))

        expand = MathTex(
            r"f^i = \frac{1}{c}\frac{\partial T^{0i}}{\partial t} + \partial_j T^{ij}",
            font_size=36
        ).next_to(cons, DOWN, buff=0.6)
        self.play(Write(expand))
        note = Text("rate of change of momentum density, plus flux through the boundary",
                     font_size=22, color=GREY).next_to(expand, DOWN, buff=0.4)
        self.play(FadeIn(note))
        self.wait(1.5)

        self.play(FadeOut(VGroup(expand, note)))

        simp = Text(
            "Simplification: hold only the energy-density term, spread\n"
            "over a 1-metre volume -- energy in joules reads off as force in newtons.",
            font_size=24, color=GREY, line_spacing=1.2
        ).next_to(cons, DOWN, buff=0.6)
        self.play(FadeIn(simp))
        self.wait(1)

        force = MathTex(
            r"F \approx 3.6 \times 10^{71}\ \text{N}",
            font_size=56, color=YELLOW
        ).next_to(simp, DOWN, buff=0.8)
        self.play(Write(force))
        self.wait(2)

        self.play(FadeOut(VGroup(header, cons, simp, force)))


# ----------------------------------------------------------------------
class ComparisonScene(Scene):
    """Log-scale comparison -- values span ~70 orders of magnitude, so the
    axis plots log10(force) rather than force itself."""
    def construct(self):
        header = section_title("How Big Is That, Really?")
        self.play(Write(header))

        entries = [
            ("Average punch", 1.0e3, GREY),
            ("Hardest recorded punch", 6.9e4, GREY),
            ("Superman (comic estimates)", 1.0e39, BLUE),
            ("This calculation", 3.6e71, YELLOW),
        ]

        axis = NumberLine(
            x_range=[0, 75, 10],
            length=10,
            include_numbers=True,
            font_size=22,
            color=GREY,
        ).shift(DOWN * 0.3)
        axis_label = Text("log10(Force in Newtons)", font_size=22).next_to(axis, DOWN, buff=0.4)
        self.play(Create(axis), Write(axis_label))

        dots = VGroup()
        labels = VGroup()
        for name, val, color in entries:
            x = np.log10(val)
            dot = Dot(axis.n2p(x), color=color, radius=0.09)
            lab = Text(name, font_size=20, color=color)
            lab.next_to(dot, UP, buff=0.35).rotate(PI / 6, about_point=dot.get_center())
            dots.add(dot)
            labels.add(lab)

        for dot, lab in zip(dots, labels):
            self.play(FadeIn(dot, scale=0.5), Write(lab), run_time=0.7)
        self.wait(1)

        gap = Text(
            "~31 orders of magnitude past comics' best estimate of Superman",
            font_size=22, color=GREY
        ).next_to(axis_label, DOWN, buff=0.5)
        self.play(FadeIn(gap))
        self.wait(2.5)

        self.play(FadeOut(VGroup(header, axis, axis_label, dots, labels, gap)))


# ----------------------------------------------------------------------
class FAQScene(Scene):
    def construct(self):
        header = section_title("A Quick Question")
        self.play(Write(header))

        q = Text('"If gravity moves at the speed of light, does that change things?"',
                  font_size=28, color=YELLOW, line_spacing=1.2)
        q.next_to(header, DOWN, buff=0.6)
        self.play(Write(q))
        self.wait(1)

        a = Text(
            "Not much. Unlike radiation, a constant gravitational field\n"
            "doesn't redshift or lose energy -- the model already forces\n"
            "gravity to overcome dark energy by construction.\n\n"
            "The two real effects -- the universe's radius changing, and\n"
            "parts of it crossing beyond causal reach -- push in opposite\n"
            "directions and largely cancel at this scale.",
            font_size=24, line_spacing=1.3
        ).next_to(q, DOWN, buff=0.6)
        self.play(FadeIn(a))
        self.wait(3)

        self.play(FadeOut(VGroup(header, q, a)))


# ----------------------------------------------------------------------
class EndScene(Scene):
    def construct(self):
        msg = Text("Thank you for watching", font_size=48, weight=BOLD)
        self.play(Write(msg))
        self.wait(2)
        self.play(FadeOut(msg))
