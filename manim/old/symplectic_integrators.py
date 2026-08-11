"""
==============================================================================
HAMILTONIAN INTEGRATION: MANIM ANIMATION CODE
==============================================================================

HOW TO RUN THIS FILE
---------------------
Install Manim:
    pip install manim

Render a single scene (e.g. the opening scene):
    manim -pql hamiltonian_integration.py OpeningScene

Flags:
    -p   preview (open video when done)
    -q   quality: l=low (480p, fast), m=medium (720p), h=high (1080p, slow)
    -a   render ALL scenes in the file

Full-quality render of everything:
    manim -pqh -a hamiltonian_integration.py

WHAT THIS FILE CONTAINS
------------------------
Each "Scene" class is an independent video segment matching the script sections:

    Scene 1  - OpeningScene            : Energy drift motivation
    Scene 2  - HamiltonianRefresher    : H(q,p), equations of motion, phase portrait
    Scene 3  - WhyStandardMethodsFail  : Forward Euler spiral, energy plot
    Scene 4  - SymplecticCoreIdea      : Symplectic form, area preservation
    Scene 5  - SymplecticEuler         : Method derivation and comparison
    Scene 6  - LeapfrogMethod          : Staggered grid, Verlet derivation
    Scene 7  - SplittingMethods        : Operator splitting, composition
    Scene 8  - NBodiesDemo             : N-body simulation visualization
    Scene 9  - PracticalConsiderations : Summary tables and guidelines

MANIM QUICK REFERENCE FOR THIS FILE
-------------------------------------
Coordinate system:
    - Origin (0,0,0) is center of screen
    - Screen is roughly 14 units wide, 8 units tall
    - RIGHT = (1,0,0), UP = (0,1,0), OUT = (0,0,1)
    - Use UP*2 + LEFT*3 to place things

Core Mobject types used here:
    MathTex(r"...latex...")       - LaTeX math formula
    Tex(r"...latex...")           - LaTeX text
    Text("...")                   - Plain text (no LaTeX)
    Arrow(start, end)             - Arrow between two points
    Axes(...)                     - Coordinate system with axes
    ParametricFunction(func, ...)  - Plot a parametric curve
    NumberPlane(...)              - Full grid background
    Dot(point)                    - Small circle
    Rectangle(width, height)      - Rectangle shape
    VGroup(*mobjects)             - Group of objects (transforms together)

Core animation types used here:
    self.play(Create(obj))        - Draw obj onto screen
    self.play(Write(tex))         - Write text/equation onto screen
    self.play(FadeIn(obj))        - Fade obj in
    self.play(FadeOut(obj))       - Fade obj out
    self.play(Transform(a, b))    - Morph a into b
    self.play(obj.animate.shift(RIGHT*2)) - Move obj right by 2 units
    self.play(obj.animate.scale(0.5))     - Shrink obj to half size
    self.wait(t)                  - Pause for t seconds

Style constants at top of each scene for easy customization.
==============================================================================
"""

# ============================================================
# IMPORTS
# All of Manim's classes live in manim.* — the star import
# brings in everything so we don't need long qualified names.
# ============================================================

from manim import *
import numpy as np  # For math operations in our simulations

# ============================================================
# GLOBAL STYLE CONSTANTS
# Define once here, use everywhere. Change these to restyle
# the entire video without hunting through the code.
# ============================================================

# --- Color palette ---
# These are hex strings. Manim also accepts named constants
# like RED, BLUE, GREEN, YELLOW, WHITE, BLACK, GREY, etc.
C_BACKGROUND  = "#0D1117"   # Very dark navy — gives that "chalkboard" feel
C_PRIMARY     = "#58A6FF"   # Bright blue — main equations and axes
C_SECONDARY   = "#F0883E"   # Warm orange — highlighted terms, bad methods
C_GOOD        = "#3FB950"   # Green — symplectic/good methods
C_BAD         = "#F85149"   # Red — failing/bad methods
C_NEUTRAL     = "#E6EDF3"   # Near-white — explanatory text
C_DIM         = "#8B949E"   # Grey — secondary text, annotations
C_ACCENT      = "#D2A8FF"   # Soft purple — mathematical objects
C_YELLOW      = "#E3B341"   # Yellow — emphasis, warnings

# --- Font sizes (in Manim's internal units) ---
FS_TITLE      = 56          # Big section headers
FS_HEADING    = 42          # Sub-section headings
FS_BODY       = 32          # Normal explanatory text
FS_SMALL      = 24          # Annotations, labels
FS_TINY       = 18          # Axis tick labels, fine print

# --- Timing (seconds) ---
T_FAST        = 0.4         # Quick transitions
T_NORMAL      = 0.8         # Standard animation speed
T_SLOW        = 1.5         # Emphasis, important reveals
T_PAUSE       = 1.0         # Short pause between ideas
T_LONG_PAUSE  = 2.5         # Pause after key insight


# ============================================================
# UTILITY FUNCTIONS
# Small helpers used across multiple scenes.
# ============================================================

def make_title_card(title_text, subtitle_text=None, title_color=C_PRIMARY):
    """
    Create a centered title card with optional subtitle.

    Parameters
    ----------
    title_text   : str  — main heading (plain text, NOT LaTeX)
    subtitle_text: str  — smaller text below (optional)
    title_color  : hex  — color for the main title

    Returns
    -------
    VGroup containing the title (and subtitle if provided).

    Usage
    -----
        card = make_title_card("Energy Drift", "Why standard methods fail")
        self.play(Write(card))
    """
    # Text() creates non-LaTeX text. font_size controls size.
    # color sets the fill color.
    title = Text(title_text, font_size=FS_TITLE, color=title_color)

    if subtitle_text:
        subtitle = Text(subtitle_text, font_size=FS_BODY, color=C_DIM)
        # .next_to(obj, direction, buff=gap) places subtitle below title
        # buff is the gap in Manim units
        subtitle.next_to(title, DOWN, buff=0.4)
        return VGroup(title, subtitle)

    return VGroup(title)


def make_equation_box(latex_str, box_color=C_PRIMARY, fill_opacity=0.1):
    """
    Wrap a LaTeX equation in a colored rounded rectangle.

    This is the "key equation" visual that appears whenever we
    want to emphasize an important formula.

    Parameters
    ----------
    latex_str    : str  — LaTeX string for the equation
    box_color    : hex  — color for the border and fill
    fill_opacity : float— 0.0 = transparent, 1.0 = solid fill

    Returns
    -------
    VGroup of (rectangle, equation) — animate as one unit.

    Usage
    -----
        box = make_equation_box(r"H = T(p) + V(q)")
        self.play(Create(box))
    """
    # MathTex renders LaTeX math. Use r"..." (raw string) to avoid
    # Python interpreting backslashes.
    eq = MathTex(latex_str, color=C_NEUTRAL, font_size=FS_BODY)

    # SurroundingRectangle draws a box that tightly wraps a Mobject.
    # corner_radius rounds the corners. buff adds padding.
    box = SurroundingRectangle(
        eq,
        color=box_color,
        corner_radius=0.15,
        buff=0.25,
        fill_color=box_color,
        fill_opacity=fill_opacity,
    )
    # VGroup groups objects. The box is drawn BEHIND eq because
    # it's listed first (Manim renders in order).
    return VGroup(box, eq)


def simulate_harmonic_oscillator(q0, p0, h, n_steps, method="leapfrog"):
    """
    Numerically integrate the harmonic oscillator H = p²/2 + q²/2.
    (mass=1, spring constant=1 for simplicity)

    This runs entirely in Python/NumPy — not a Manim object.
    We call it to get trajectory data, then plot that data with Manim.

    Parameters
    ----------
    q0      : float — initial position
    p0      : float — initial momentum
    h       : float — timestep
    n_steps : int   — number of integration steps
    method  : str   — "euler", "symplectic_euler", or "leapfrog"

    Returns
    -------
    qs, ps      : arrays of positions and momenta
    energies    : array of H values at each step (should be constant = 0.5)

    The exact solution is a circle in phase space:
        q(t) = cos(t), p(t) = -sin(t)  (for q0=1, p0=0)
    """
    qs = np.zeros(n_steps)
    ps = np.zeros(n_steps)
    energies = np.zeros(n_steps)

    qs[0] = q0
    ps[0] = p0

    for i in range(1, n_steps):
        q, p = qs[i-1], ps[i-1]

        if method == "euler":
            # -------------------------------------------------------
            # FORWARD EULER (standard, non-symplectic)
            # Both updates use the OLD values (q, p).
            # ∂H/∂p = p (kinetic), ∂H/∂q = q (potential)
            # -------------------------------------------------------
            q_new = q + h * p       # dq/dt = +∂H/∂p = p
            p_new = p - h * q       # dp/dt = -∂H/∂q = -q

        elif method == "symplectic_euler":
            # -------------------------------------------------------
            # SYMPLECTIC EULER
            # Update p first with old q, then update q with NEW p.
            # This small change makes the map symplectic!
            # -------------------------------------------------------
            p_new = p - h * q       # Update p using old q
            q_new = q + h * p_new   # Update q using NEW p  ← key difference

        elif method == "leapfrog":
            # -------------------------------------------------------
            # LEAPFROG (Störmer-Verlet)
            # Half-step p, full-step q, half-step p again.
            # Symmetric → 2nd order accurate.
            # -------------------------------------------------------
            p_half = p - (h / 2) * q      # Half-step for p
            q_new  = q + h * p_half        # Full-step for q
            p_new  = p_half - (h / 2) * q_new  # Half-step for p again

        qs[i] = q_new
        ps[i] = p_new
        # Energy = H = p²/2 + q²/2 (should stay near 0.5 for q0=1,p0=0)
        energies[i] = 0.5 * p_new**2 + 0.5 * q_new**2

    # Set initial energy too
    energies[0] = 0.5 * p0**2 + 0.5 * q0**2
    return qs, ps, energies


# ==============================================================================
# SCENE 1: OpeningScene
# Runtime: ~2:30
# Goal: Hook the viewer. Show a beautiful orbit, then watch it break with RK4,
#       then show it stay stable with leapfrog. Create the "need" for the video.
# ==============================================================================

class OpeningScene(Scene):
    """
    Opening: The energy drift problem.

    Visual flow:
    1. Black screen with title
    2. Phase space axes appear
    3. True (exact) circular orbit appears in green
    4. Forward Euler orbit spirals outward in red
    5. Energy vs time plots appear side by side
    6. Text: "This is because standard integrators ignore geometry"
    """

    def construct(self):
        # --------------------------------------------------------
        # construct() is the ONLY method Manim calls automatically.
        # Everything inside here defines what appears on screen.
        # Think of it as the "main" function for this scene.
        # --------------------------------------------------------

        # ---- Set background color ----
        # self.camera.background_color affects the whole scene.
        self.camera.background_color = C_BACKGROUND

        # ==== STEP 1: Title card ====
        title = make_title_card(
            "Numerical Hamiltonian Integration",
            "Why geometry matters",
        )
        # .move_to(ORIGIN) centers the VGroup.
        # ORIGIN = np.array([0, 0, 0]) — center of screen.
        title.move_to(ORIGIN)

        # Write() animates text appearing stroke-by-stroke (like writing).
        # FadeIn() would just appear. Write looks better for titles.
        self.play(Write(title), run_time=T_SLOW)
        self.wait(T_LONG_PAUSE*2)

        # FadeOut removes the object. run_time controls animation speed.
        self.play(FadeOut(title), run_time=T_FAST)

        # Write complex hamiltonian 

        complex = MathTex(r"""H[f,\mathbf{E},\mathbf{B}] = 
                          \int_{\mathbb{R}^3} d^3x \;\frac{1}{2}\left(|\mathbf{E}(\mathbf{x})|^2 + |\mathbf{B}(\mathbf{x})|^2\right)
                          +
                          \int_{\mathbb{R}^3} d^3x \int_{\mathbb{R}^3} d^3p \;
                          f(\mathbf{x},\mathbf{p}) \, \sqrt{m^2 c^4 + c^2|\mathbf{p}|^2}
                            +  
                            \int_{\mathbb{R}^3} d^3x \int_{\mathbb{R}^3} d^3p \;
                            f(\mathbf{x},\mathbf{p}) \, q \,\phi(\mathbf{x})""", font_size=25, color=C_NEUTRAL)
        complex.move_to(ORIGIN)
        self.play(Write(complex), run_time=T_SLOW)
        self.wait(T_LONG_PAUSE*3)
        self.play(FadeOut(complex), run_time=T_FAST)
        # ==== STEP 2: Phase space axes ====
        # Axes() creates a 2D coordinate system.
        # x_range=[min, max, step], y_range=[min, max, step]
        # axis_config applies styling to both axes at once.
        axes = Axes(
            x_range=[-2.2, 2.2, 1],
            y_range=[-2.2, 2.2, 1],
            x_length=5.5,    # How wide the axes are in Manim units
            y_length=5.5,
            axis_config={
                "color": C_DIM,
                "stroke_width": 1.5,
                "include_tip": True,       # Arrow tips on axes
                "tip_length": 0.2,
            },
        )
        # Position the axes on the LEFT half of the screen
        # LEFT = np.array([-1, 0, 0]), so LEFT*3.5 moves 3.5 units left
        axes.move_to(LEFT * 3.5)

        # Axis labels using MathTex. get_x_axis_label places it at the
        # end of the x-axis. edge=RIGHT puts the label to the right.
        x_label = axes.get_x_axis_label(
            MathTex("q", color=C_DIM, font_size=FS_SMALL),
            edge=RIGHT,
            direction=DOWN * 0.5,
        )
        y_label = axes.get_y_axis_label(
            MathTex("p", color=C_DIM, font_size=FS_SMALL),
            edge=UP,
            direction=LEFT * 0.5,
        )

        # "Phase Space" label above axes
        phase_label = Text("Phase Space", font_size=FS_SMALL, color=C_DIM)
        phase_label.next_to(axes, UP, buff=0.2)

        # Create() draws the shape progressively (stroke appears).
        # Use Create for geometric shapes, Write for text.
        #####################################################
        #self.play(
            #Create(axes),
            #Write(x_label),
            #Write(y_label),
            #run_time=T_NORMAL,
        #)
        #self.play(Write(phase_label))

        # ==== STEP 3: Exact solution (circle) ====
        # The exact solution to H = p²/2 + q²/2, q(0)=1, p(0)=0
        # is q(t) = cos(t), p(t) = -sin(t) — a unit circle.
        #
        # ParametricFunction takes a lambda that maps t → (x, y, z).
        # axes.c2p() converts "data coordinates" (q,p values) to
        # "point coordinates" (screen position). Always use c2p when
        # placing things relative to an Axes object.
        #####################################################
        #exact_orbit = ParametricFunction(
            #lambda t: axes.c2p(np.cos(t), -np.sin(t)),
            #t_range=[0, 2 * PI],        # One full revolution
            #color=C_GOOD,
            #stroke_width=2.5,
        #)
        exact_label = Text("Exact orbit", font_size=FS_SMALL, color=C_GOOD)
        exact_label.next_to(axes, DOWN, buff=0.3).shift(LEFT * 0.5)
##############################################
        #self.play(Create(exact_orbit), run_time=T_SLOW)
        #self.play(Write(exact_label))
        #self.wait(T_PAUSE)

        # ==== STEP 4: Forward Euler trajectory ====
        # Simulate Euler with a moderate timestep to clearly show the spiral.
        N = 600
        h_euler = 0.05
        qs_eu, ps_eu, energies_euler = simulate_harmonic_oscillator(
            1.0, 0.0, h_euler, N, method="euler"
        )

        # Build the spiral path as a VMobject using set_points_as_corners.
        # c2p maps each (q,p) data point to screen coordinates.
        euler_path = VMobject(color=C_BAD, stroke_width=2.0)
        # Build list of screen-space points from data arrays.
        # We only plot the first 400 steps to keep it readable.
        euler_points = [axes.c2p(qs_eu[i], ps_eu[i]) for i in range(400)]
        # set_points_as_corners draws straight lines between points.
        # For smooth curves, use make_smooth() after.
        euler_path.set_points_as_corners(euler_points)

        euler_label = Text(
            "Forward Euler (spirals out!)",
            font_size=FS_SMALL,
            color=C_BAD,
        )
        euler_label.next_to(axes, DOWN, buff=0.6).shift(LEFT * 0.3)
#############################################
        #self.play(Create(euler_path), run_time=2.0)
        #self.play(Write(euler_label))
        #self.wait(T_PAUSE)

        # ==== STEP 5: Energy vs time plot on the RIGHT side ====
        # Build a second axes for the energy plot
        energy_axes = Axes(
            x_range=[0, N, N // 4],
            y_range=[0.4, 0.8, 0.1],
            x_length=5.5,
            y_length=4.0,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        energy_axes.move_to(RIGHT * 3.5)

        e_x_label = energy_axes.get_x_axis_label(
            Text("Steps", font_size=FS_TINY, color=C_DIM),
            edge=RIGHT, direction=DOWN,
        )
        e_y_label = energy_axes.get_y_axis_label(
            MathTex("H", font_size=FS_SMALL, color=C_DIM),
            edge=UP, direction=LEFT,
        )
        energy_title = Text("Energy vs. Time", font_size=FS_SMALL, color=C_DIM)
        energy_title.next_to(energy_axes, UP, buff=0.2)

        self.play(
            Create(energy_axes),
            Write(e_x_label),
            Write(e_y_label),
            Write(energy_title),
        )

        # Plot the Euler energy curve (growing monotonically)
        euler_energy_curve = energy_axes.plot_line_graph(
            x_values=list(range(N)),
            y_values=list(np.clip(energies_euler, 0.4, 0.79)),
            line_color=C_BAD,
            stroke_width=2.0,
            add_vertex_dots=False,  # Don't draw dots at each data point
        )
        # A dashed horizontal line showing where energy SHOULD be (0.5)
        true_energy_line = energy_axes.plot(
            lambda x: 0.5,
            x_range=[0, N],
            color=C_GOOD,
            stroke_width=1.5,
        )
        true_energy_line = DashedVMobject(true_energy_line, dashed_ratio=0.6)

        self.play(Create(euler_energy_curve), run_time=1.5)
        self.play(Create(true_energy_line))

        # Annotation arrows pointing to key features
        drift_annotation = Text(
            "Energy drifts upward!",
            font_size=FS_TINY,
            color=C_BAD,
        )
        drift_annotation.next_to(euler_energy_curve, UP, buff=0.1)
        self.play(Write(drift_annotation))
        self.wait(T_LONG_PAUSE)

        # ==== STEP 6: Now show leapfrog — stable! ====
        qs_lf, ps_lf, energies_lf = simulate_harmonic_oscillator(
            1.0, 0.0, h_euler, N, method="leapfrog"
        )
        leapfrog_path = VMobject(color=C_GOOD, stroke_width=2.5)
        lf_points = [axes.c2p(qs_lf[i], ps_lf[i]) for i in range(min(400, N))]
        leapfrog_path.set_points_as_corners(lf_points)

        lf_label = Text(
            "Leapfrog (stable!)",
            font_size=FS_SMALL,
            color=C_GOOD,
        )
        lf_label.next_to(axes, DOWN, buff=0.9)
        ########################################
        #self.play(Create(leapfrog_path), run_time=2.0)
        #self.play(Write(lf_label))

        # Leapfrog energy — should hug the dashed true-energy line
        lf_energy_curve = energy_axes.plot_line_graph(
            x_values=list(range(N)),
            y_values=list(np.clip(energies_lf, 0.4, 0.79)),
            line_color=C_GOOD,
            stroke_width=2.0,
            add_vertex_dots=False,
        )
        self.play(Create(lf_energy_curve), run_time=1.5)
        self.wait(T_LONG_PAUSE)

        # ==== STEP 7: Closing text for this scene ====
        bottom_text = Text(
            "Standard integrators ignore the geometry of physics.",
            font_size=FS_SMALL,
            color=C_NEUTRAL,
        )
        bottom_text.to_edge(DOWN, buff=0.3)
        ##############################
        #self.play(Write(bottom_text), run_time=T_SLOW)
        self.wait(T_LONG_PAUSE)

        # Fade everything out to transition to next scene
        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 2: HamiltonianRefresher
# Runtime: ~3:30
# Goal: Build up Hamilton's equations, show the phase portrait of SHO.
# ==============================================================================

class HamiltonianRefresher(Scene):
    """
    A refresher on Hamiltonian mechanics.

    Visual flow:
    1. Write H = T + V for SHO
    2. Derive Hamilton's equations from H
    3. Draw phase portrait (ellipses at various energies)
    4. Highlight: energy is conserved → curves never cross
    5. Three conservation facts with visual emphasis
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        # ===== STEP 1: Section title =====
        section_title = Text(
            "Part 1: Hamiltonian Mechanics",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        section_title.to_edge(UP, buff=0.4)
        self.play(Write(section_title))
        self.wait(T_PAUSE*3)

        # ===== STEP 2: Define the Hamiltonian =====
        # MathTex renders LaTeX. Use double backslash \\ for new line in
        # multi-line MathTex. For aligned equations, use the align* environment.
        #
        # We'll build the equation in stages to walk through it verbally.

        h_label = Text("The Hamiltonian (total energy):",
                       font_size=FS_SMALL, color=C_DIM)
        h_label.next_to(section_title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        # The general form
        h_general = MathTex(
            r"H(q, p) = T(p) + V(q)",
            font_size=FS_BODY,
            color=C_NEUTRAL,
        )
        h_general.next_to(h_label, DOWN, buff=0.3).align_to(h_label, LEFT)

        # For the harmonic oscillator specifically
        h_sho = MathTex(
            r"H(q, p) = ",
            r"\frac{p^2}{2m}",
            r"\quad + \quad",
            r"\frac{1}{2}kq^2",
            font_size=FS_BODY,
            color=C_ACCENT,
        )
        h_sho.next_to(h_general, DOWN, buff=0.5).align_to(h_general, LEFT)

        # Annotation: label each term
        kinetic_brace = Brace(
            # Brace under the kinetic term. We need to isolate it.
            # .get_part_by_tex() returns the sub-object matching that string.
            h_sho.get_part_by_tex(r"\frac{p^2}{2m}"),
            direction=DOWN,
            color=C_SECONDARY,
        )
        kinetic_text = Text(
            "Kinetic",
            font_size=FS_TINY,
            color=C_SECONDARY,
        )
        kinetic_brace.put_at_tip(kinetic_text, buff=0.1)

        potential_brace = Brace(
            h_sho.get_part_by_tex(r"\frac{1}{2}kq^2"),
            direction=DOWN,
            color=C_PRIMARY,
        )
        potential_text = Text(
            "Potential",
            font_size=FS_TINY,
            color=C_PRIMARY,
        )
        potential_brace.put_at_tip(potential_text, buff=0.1)

        self.play(Write(h_label))
        self.play(Write(h_general))
        self.wait(T_PAUSE*2)
        self.play(Write(h_sho))
        self.play(
            GrowFromCenter(kinetic_brace),
            Write(kinetic_text),
            GrowFromCenter(potential_brace),
            Write(potential_text),
        )
        self.wait(T_LONG_PAUSE)

        # ===== STEP 3: Hamilton's equations =====
        # Fade out braces, keep the Hamiltonian, add the equations
        self.play(
            FadeOut(kinetic_brace, kinetic_text,
                    potential_brace, potential_text),
        )

        eqs_label = Text(
            "Hamilton's equations of motion:",
            font_size=FS_SMALL,
            color=C_DIM,
        )
        eqs_label.next_to(h_sho, DOWN, buff=0.6).align_to(h_sho, LEFT)

        # Using the aligned environment for a two-line equation.
        # & marks the alignment column. \\ breaks to next line.
        hamilton_eqs = MathTex(
            r"\frac{dq}{dt} &= +\frac{\partial H}{\partial p} = \frac{p}{m} \\[6pt]"
            r"\frac{dp}{dt} &= -\frac{\partial H}{\partial q} = -kq",
            font_size=FS_BODY,
            color=C_NEUTRAL,
        )
        hamilton_eqs.next_to(eqs_label, DOWN, buff=0.3).align_to(eqs_label, LEFT)

        # Interpretation text
        interp1 = Text("← velocity = momentum / mass",
                       font_size=FS_TINY, color=C_DIM)
        interp1.next_to(hamilton_eqs[0][0:6], RIGHT, buff=2.0)

        interp2 = Text("← force = -gradient of potential",
                       font_size=FS_TINY, color=C_DIM)
        interp2.next_to(hamilton_eqs[0][6:], RIGHT, buff=0.5)
        interp2.move_to(interp2.get_center() + DOWN * 0.5)  # Slightly lower

        self.play(Write(eqs_label))
        self.play(Write(hamilton_eqs), run_time=T_SLOW)
        self.play(FadeIn(interp1), FadeIn(interp2))
        self.wait(T_LONG_PAUSE)

        # ===== STEP 4: Phase portrait on the right =====
        # Fade left side to make room for the phase portrait
        self.play(
            FadeOut(h_label, h_general, h_sho, eqs_label,
                    hamilton_eqs, interp1, interp2),
            run_time=T_FAST,
        )

        portrait_title = Text(
            "Phase Portrait of the Harmonic Oscillator",
            font_size=FS_SMALL,
            color=C_DIM,
        )
        portrait_title.to_edge(UP, buff=1.0)
        self.play(Write(portrait_title))

        # Phase space axes
        axes = Axes(
            x_range=[-3.0, 3.0, 1],
            y_range=[-3.0, 3.0, 1],
            x_length=6.0,
            y_length=6.0,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        # .add_coordinates() puts tick labels on both axes
        axes.add_coordinates(font_size=FS_TINY)
        axes.move_to(axes.get_center() + DOWN * 0.5)  # Slightly lower on screen
        xl = axes.get_x_axis_label(MathTex("q", color=C_DIM, font_size=FS_SMALL))
        yl = axes.get_y_axis_label(MathTex("p", color=C_DIM, font_size=FS_SMALL))

        self.play(Create(axes), Write(xl), Write(yl))

        # Draw ellipses for several energy levels.
        # For H = p²/2 + q²/2 (m=1, k=1), level set H=E is:
        # p²/2 + q²/2 = E → q²/1 + p²/1 = 2E → circle of radius sqrt(2E)
        energy_levels = [0.25, 0.5, 1.0, 1.5, 2.5]
        orbit_colors = [C_DIM, C_GOOD, C_PRIMARY, C_ACCENT, C_SECONDARY]

        orbits = VGroup()
        orbit_labels = VGroup()

        for E, col in zip(energy_levels, orbit_colors):
            r = np.sqrt(2 * E)  # radius of the circle (m=k=1 → circle not ellipse)
            orbit = ParametricFunction(
                lambda t, radius=r: axes.c2p(radius * np.cos(t),
                                             radius * np.sin(t)),
                t_range=[0, 2 * PI],
                color=col,
                stroke_width=2.0,
            )
            # Label each energy level
            label = MathTex(
                rf"E={E}",
                font_size=FS_TINY,
                color=col,
            )
            # Place label at the right edge of each orbit circle
            label.move_to(axes.c2p(r + 0.25, 0.0))
            orbits.add(orbit)
            orbit_labels.add(label)

        # Animate each orbit appearing with a small delay between them
        # LaggedStart plays animations with a lag between each one.
        self.play(
            LaggedStart(
                *[Create(orb) for orb in orbits],
                lag_ratio=0.3,   # 0.3 means each starts when previous is 30% done
                run_time=2.5,
            )
        )
        self.play(
            LaggedStart(
                *[Write(lbl) for lbl in orbit_labels],
                lag_ratio=0.2,
                run_time=1.5,
            )
        )
        self.wait(T_PAUSE)

        # ===== STEP 5: Show a moving dot on the orbit =====
        # ValueTracker lets us animate a parameter (here the angle t).
        tracker = ValueTracker(0)   # Starts at angle 0

        # The moving dot — start at angle 0 on the E=0.5 orbit (radius 1)
        moving_dot = Dot(
            axes.c2p(1.0, 0.0),   # Starting position (q=1, p=0)
            color=C_YELLOW,
            radius=0.08,
        )
        # Small tail/trace behind the dot to show direction of travel
        dot_trace = TracedPath(
            moving_dot.get_center,   # Function that returns the dot's position
            stroke_color=C_YELLOW,
            stroke_width=2.0,
            dissipating_time=1.5,    # Tail fades out after 1.5 seconds
        )

        # add_updater adds a function that runs every frame.
        # The lambda takes the dot and the dt (time since last frame).
        # We update the dot's position based on the tracker value.
        moving_dot.add_updater(
            lambda d: d.move_to(
                axes.c2p(
                    np.cos(tracker.get_value()),    # q = cos(t)
                    -np.sin(tracker.get_value()),   # p = -sin(t)
                )
            )
        )

        self.add(dot_trace, moving_dot)

        # Animate the tracker from 0 to 4π (two full orbits)
        # The dot will follow because of its updater.
        self.play(
            tracker.animate.set_value(4 * PI),
            run_time=4.0,
            rate_func=linear,   # Constant angular velocity
        )

        # Remove the updater now that the animation is done
        moving_dot.clear_updaters()
        self.wait(T_PAUSE)

        # ===== STEP 6: Three conservation facts =====
        self.play(FadeOut(dot_trace, moving_dot, orbits, orbit_labels, axes, xl, yl, portrait_title), run_time=T_FAST)

        facts_title = Text(
            "Three things Hamiltonian flow preserves:",
            font_size=FS_SMALL,
            color=C_NEUTRAL,
        )
        facts_title.move_to(ORIGIN + UP * 1.5)

        fact1 = Text("① Energy H(q,p) = const",
                     font_size=FS_SMALL, color=C_GOOD)
        fact2 = Text("② Symplectic form  ω = dq∧dp",
                     font_size=FS_SMALL, color=C_PRIMARY)
        fact3 = Text("③ Phase space volume is conserved (Liouville)",
                     font_size=FS_SMALL, color=C_ACCENT)

        fact1.next_to(facts_title, DOWN, buff=0.4).align_to(facts_title, LEFT)
        fact2.next_to(fact1, DOWN, buff=0.3).align_to(fact1, LEFT)
        fact3.next_to(fact2, DOWN, buff=0.3).align_to(fact2, LEFT)

        self.play(Write(facts_title))
        self.play(Write(fact1))
        self.play(Write(fact2))
        self.play(Write(fact3))
        self.wait(T_LONG_PAUSE)

        # Close scene
        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 3: WhyStandardMethodsFail
# Runtime: ~3:30
# Goal: Show exactly why Forward Euler fails — with math and visual proof.
# ==============================================================================

class WhyStandardMethodsFail(Scene):
    """
    The failure of Forward Euler.

    Visual flow:
    1. Write Forward Euler update rule
    2. Animate a dot applying one Euler step in phase space
    3. Show the spiral over many steps
    4. Energy vs time: monotonically growing
    5. Contrast with RK4 (also grows, just more slowly)
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        # ===== Title =====
        title = Text(
            "Why Standard Methods Fail",
            font_size=FS_HEADING,
            color=C_BAD,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Forward Euler equations =====
        euler_label = Text(
            "Forward Euler update rule:",
            font_size=FS_SMALL, color=C_DIM,
        )
        euler_label.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        # Write the two update equations with step highlighting
        euler_q = MathTex(
            r"q_{n+1} = ",
            r"q_n",
            r" + h \cdot \frac{\partial H}{\partial p}(",
            r"q_n, p_n",
            r")",
            font_size=FS_BODY,
            color=C_NEUTRAL,
        )
        euler_p = MathTex(
            r"p_{n+1} = ",
            r"p_n",
            r" - h \cdot \frac{\partial H}{\partial q}(",
            r"q_n, p_n",
            r")",
            font_size=FS_BODY,
            color=C_NEUTRAL,
        )
        euler_q.next_to(euler_label, DOWN, buff=0.3).align_to(euler_label, LEFT)
        euler_p.next_to(euler_q, DOWN, buff=0.4).align_to(euler_q, LEFT)

        # Highlight that both use OLD values (q_n, p_n)
        highlight_box_q = SurroundingRectangle(
            euler_q[3],
            color=C_BAD, buff=0.05,
        )
        highlight_box_p = SurroundingRectangle(
            euler_p[3],
            color=C_BAD, buff=0.05,
        )
        highlight_note = Text(
            "← Both use OLD values",
            font_size=FS_TINY, color=C_BAD,
        )
        highlight_note.next_to(euler_p, RIGHT, buff=0.3)

        #self.play(Write(euler_label))
        #self.play(Write(euler_q))
        #self.play(Write(euler_p))
        #self.play(Create(highlight_box_q), Create(highlight_box_p))
        #self.play(Write(highlight_note))
        #self.wait(T_LONG_PAUSE)

        #self.play(
        #    FadeOut(highlight_box_q, highlight_box_p, highlight_note,
        #            euler_label, euler_q, euler_p),
        #)

        # ===== STEP 2: Phase space with Euler spiral =====
        axes = Axes(
            x_range=[-2.5, 2.5, 1],
            y_range=[-2.5, 2.5, 1],
            x_length=5.0,
            y_length=5.0,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        axes.to_edge(RIGHT, buff=0.8).shift(DOWN * 0.3)

        xl = axes.get_x_axis_label(MathTex("q", color=C_DIM, font_size=FS_SMALL))
        yl = axes.get_y_axis_label(MathTex("p", color=C_DIM, font_size=FS_SMALL))
        axes_label = Text("Phase Space", font_size=FS_TINY, color=C_DIM)
        axes_label.next_to(axes, UP, buff=0.15)

        # True orbit (circle) for reference
        true_circle = ParametricFunction(
            lambda t: axes.c2p(np.cos(t), -np.sin(t)),
            t_range=[0, 2 * PI],
            color=C_GOOD,
            stroke_width=1.5,
        )
        true_circle = DashedVMobject(true_circle, dashed_ratio=0.55)
        true_label = Text("True orbit", font_size=FS_TINY, color=C_GOOD)
        true_label.next_to(axes, DOWN, buff=0.15).shift(LEFT)

        self.play(Write(euler_label))
        self.play(Write(euler_q))
        self.play(Write(euler_p))
        #self.play(Create(highlight_box_q), Create(highlight_box_p))
        #self.play(Write(highlight_note))
        #self.wait(T_LONG_PAUSE)
        self.play(Create(axes), Write(xl), Write(yl), Write(axes_label))
        self.play(Create(true_circle), Write(true_label))
        
        
        # Build Euler spiral path from simulation data
        N = 800
        qs_eu, ps_eu, en_eu = simulate_harmonic_oscillator(
            1.0, 0.0, 0.05, N, method="euler"
        )

        # We'll reveal the spiral progressively using add_line_to
        # to create a growing path.
        spiral = VMobject(color=C_BAD, stroke_width=2.0)
        # Start the VMobject at the first point
        spiral.start_new_path(axes.c2p(qs_eu[0], ps_eu[0]))

        # Instead of plotting all at once, we reveal in chunks
        def build_spiral_incrementally(obj, alpha):
            """
            Update function for Create-style animation.
            alpha goes from 0 to 1 as the animation progresses.
            """
            n = int(alpha * (N - 1))
            if n < 1:
                return
            pts = [axes.c2p(qs_eu[i], ps_eu[i]) for i in range(n + 1)]
            obj.set_points_as_corners(pts)

        # UpdateFromAlphaFunc lets you define a custom animation
        # based on progress alpha in [0,1].
        self.play(
            UpdateFromAlphaFunc(spiral, build_spiral_incrementally),
            run_time=3.0,
            rate_func=linear,
        )
        # Add the completed spiral to the scene
        self.add(spiral)

        bad_label = Text(
            "Forward Euler:\nenergy grows!",
            font_size=FS_TINY,
            color=C_BAD,
        )
        bad_label.next_to(axes, DOWN, buff=0.5)
        self.play(Write(bad_label))
        self.wait(T_LONG_PAUSE)
        self.play(FadeOut(axes, spiral, bad_label, true_circle, true_label, xl, yl, axes_label))
        # ===== STEP 3: Energy plot on the right =====
        e_axes = Axes(
            x_range=[0, N, N // 4],
            y_range=[0.45, 0.80, 0.05],
            x_length=5.5,
            y_length=3.5,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        e_axes.to_edge(RIGHT, buff=0.5).shift(UP * 0.5)

        e_xl = e_axes.get_x_axis_label(
            Text("Step n", font_size=FS_TINY, color=C_DIM),
            edge=RIGHT, direction=DOWN,
        )
        e_yl = e_axes.get_y_axis_label(
            MathTex("H", font_size=FS_SMALL, color=C_DIM),
            edge=UP, direction=LEFT,
        )
        e_title = Text("Energy over time", font_size=FS_SMALL, color=C_DIM)
        e_title.next_to(e_axes, UP, buff=0.15)

        # True energy reference line
        true_e_line = e_axes.plot(
            lambda x: 0.5,
            x_range=[0, N],
            color=C_GOOD,
            stroke_width=1.5,
        )
        true_e_line = DashedVMobject(true_e_line, dashed_ratio=0.6)
        true_e_text = Text("True energy = 0.5",
                           font_size=FS_TINY, color=C_GOOD)
        true_e_text.next_to(true_e_line, RIGHT, buff=0.1)

        self.play(
            Create(e_axes), Write(e_xl), Write(e_yl), Write(e_title),
            run_time=T_NORMAL,
        )
        self.play(Create(true_e_line), Write(true_e_text))

        # Euler energy (growing curve)
        euler_e_curve = e_axes.plot_line_graph(
            x_values=list(range(N)),
            y_values=list(np.clip(en_eu, 0.45, 0.79)),
            line_color=C_BAD,
            stroke_width=2.5,
            add_vertex_dots=False,
        )
        self.play(Create(euler_e_curve), run_time=2.0)

        # Annotation with an arrow pointing to the drift
        drift_dot = Dot(
            e_axes.c2p(N * 0.9, np.clip(en_eu[int(N * 0.9)], 0.45, 0.79)),
            color=C_BAD,
            radius=0.07,
        )
        drift_arrow = Arrow(
            start=e_axes.c2p(N * 0.6, 0.76),
            end=e_axes.c2p(N * 0.85, np.clip(en_eu[int(N * 0.85)], 0.45, 0.78)),
            color=C_BAD,
            stroke_width=2.0,
            tip_length=0.15,
        )
        drift_text = Text("Monotonic drift", font_size=FS_TINY, color=C_BAD)
        drift_text.next_to(drift_arrow.get_start(), UP, buff=0.1)

        self.play(Create(drift_arrow), Write(drift_text), FadeIn(drift_dot))
        self.wait(T_LONG_PAUSE)

        # ===== STEP 4: Summary statement =====
        summary = Text(
            "The map (qₙ,pₙ) → (qₙ₊₁,pₙ₊₁) is not symplectic.\nEnergy has no reason to be conserved.",
            font_size=FS_SMALL,
            color=C_SECONDARY,
            line_spacing=1.4,
        )
        summary.to_edge(DOWN, buff=0.35)
        self.play(Write(summary), run_time=T_SLOW)
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)

        ######## step 5: error rate plot #############
        r_axes = Axes(
            x_range=[0, N, N // 20],
            y_range=[0, 0.5, 0.1],
            x_length=5.5,
            y_length=3.5,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        r_axes.move_to(ORIGIN)

        error_line = r_axes.plot_line_graph(
            x_values=list(range(N)),
            y_values = (en_eu - 0.5) * 0.1,
            line_color=C_BAD,
            stroke_width=2.5,
            add_vertex_dots=False,

        )
        r_xl = r_axes.get_x_axis_label(
            Text("Step n", font_size=FS_TINY, color=C_DIM),
            edge=RIGHT, direction=DOWN,
        )
        r_yl = r_axes.get_y_axis_label(
            MathTex("Log of Error", font_size=FS_SMALL, color=C_DIM),
            edge=UP, direction=LEFT,
        )
        r_title = Text("Energy error over time", font_size=FS_SMALL, color=C_DIM)
        r_title.next_to(r_axes, UP, buff=0.15)

        self.play(
            Create(r_axes), Write(r_xl), Write(r_yl), Write(r_title),
            run_time=T_NORMAL,
        )
        self.play(Create(error_line), run_time=2.0)
        self.wait(T_LONG_PAUSE)



# ==============================================================================
# SCENE 4: SymplecticCoreIdea
# Runtime: ~4:30
# Goal: Visualize the symplectic structure. Phase space area preservation.
# ==============================================================================

class SymplecticCoreIdea(Scene):
    """
    What 'symplectic' actually means.

    Visual flow:
    1. Draw a region in phase space (blob of initial conditions)
    2. Evolve it under the harmonic oscillator (it rotates but area is preserved)
    3. Show area measurement before and after
    4. Key theorem: symplectic integrators track a nearby Hamiltonian
    5. Energy is bounded (oscillates), not drifting
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        # ===== Title =====
        title = Text(
            "The Core Idea: Symplectic Structure",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Symplectic form definition =====
        form_text = Text(
            "The symplectic 2-form:",
            font_size=FS_SMALL, color=C_DIM,
        )
        form_text.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        omega_def = MathTex(
            r"\omega = dq \wedge dp",
            font_size=FS_BODY,
            color=C_PRIMARY,
        )
        omega_def.next_to(form_text, DOWN, buff=0.2).align_to(form_text, LEFT)

        meaning = Text(
            "This measures oriented area in phase space.",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        meaning.next_to(omega_def, DOWN, buff=0.3).align_to(omega_def, LEFT)

        self.play(Write(form_text))
        self.play(Write(omega_def), run_time=T_NORMAL)
        self.play(Write(meaning))
        self.wait(T_PAUSE)

        self.play(FadeOut(form_text, omega_def, meaning))

        # ===== STEP 2: Phase space axes =====
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            x_length=6.0,
            y_length=6.0,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        axes.center()

        xl = axes.get_x_axis_label(MathTex("q", color=C_DIM, font_size=FS_SMALL))
        yl = axes.get_y_axis_label(MathTex("p", color=C_DIM, font_size=FS_SMALL))

        self.play(Create(axes), Write(xl), Write(yl))

        # ===== STEP 3: Initial blob of conditions =====
        # We draw an ellipse/circle as the initial "region" of phase space.
        # This represents uncertainty in initial conditions.
        blob_center_q = 1.8   # Center in q direction
        blob_center_p = 0.0   # Center in p direction
        blob_radius = 0.35    # Size of the blob

        # Create the initial blob as a filled circle
        init_blob = Circle(
            radius=blob_radius * (axes.x_length / 6.0),  # Convert data → screen
            color=C_SECONDARY,
            fill_color=C_SECONDARY,
            fill_opacity=0.35,
            stroke_width=2.0,
        )
        init_blob.move_to(axes.c2p(blob_center_q, blob_center_p))

        area_text = Text("Initial area = A", font_size=FS_TINY, color=C_SECONDARY)
        area_text.next_to(init_blob, UP, buff=0.1)

        self.play(Create(init_blob), Write(area_text))
        self.wait(T_PAUSE)

        # ===== STEP 4: Show the flow — blob rotates around origin =====
        # Under H = (p²+q²)/2, the flow is rigid rotation with angular velocity 1.
        # The blob rotates around the origin, staying the same shape.

        angle_tracker = ValueTracker(0)

        def get_blob_position(angle):
            """
            Compute center of blob after rotating by 'angle' radians.
            The harmonic oscillator rotates in phase space.
            """
            q = blob_center_q * np.cos(angle) - blob_center_p * np.sin(angle)
            p = blob_center_q * np.sin(angle) + blob_center_p * np.cos(angle)
            return axes.c2p(q, p)

        # Trace the path of the blob center as it rotates
        center_trace = TracedPath(
            lambda: get_blob_position(angle_tracker.get_value()),
            stroke_color=C_DIM,
            stroke_width=1.0,
        )

        # The blob follows the center
        init_blob.add_updater(
            lambda b: b.move_to(get_blob_position(angle_tracker.get_value()))
        )
        area_text.add_updater(
            lambda t: t.next_to(
                get_blob_position(angle_tracker.get_value()), UP, buff=0.15
            )
        )

        self.add(center_trace)

        # Rotate by 2π (one full revolution)
        self.play(
            angle_tracker.animate.set_value(2 * PI),
            run_time=3.5,
            rate_func=linear,
        )
        init_blob.clear_updaters()
        area_text.clear_updaters()
        self.wait(T_PAUSE)

        # Show "area still = A" after rotation
        area_after = Text("Area still = A ✓", font_size=FS_TINY, color=C_GOOD)
        area_after.next_to(init_blob, UP, buff=0.1)
        self.play(Transform(area_text, area_after))
        self.wait(T_PAUSE)

        # Liouville's theorem label
        liouville = Text(
            "Liouville's Theorem:\nHamiltonian flow preserves phase space area.",
            font_size=FS_SMALL,
            color=C_NEUTRAL,
            line_spacing=1.4,
        )
        liouville.next_to(axes, RIGHT, buff=0.5).shift(UP)
        self.play(Write(liouville), run_time=T_SLOW)
        self.wait(T_PAUSE)

        # ===== STEP 5: Now show what NON-symplectic integrator does to area =====
        # The Euler map expands areas → conservation violated.
        non_symplectic_text = Text(
            "A non-symplectic integrator\ncan inflate or shrink this area!",
            font_size=FS_SMALL,
            color=C_BAD,
            line_spacing=1.4,
        )
        non_symplectic_text.next_to(axes, RIGHT, buff=0.5).shift(DOWN)
        self.play(Write(non_symplectic_text))

        # Animate the blob scaling up (to simulate Euler's area inflation)
        bad_blob = init_blob.copy()
        bad_blob.set_color(C_BAD).set_fill(C_BAD, opacity=0.25)
        self.play(FadeIn(bad_blob))
        self.play(bad_blob.animate.scale(1.5), run_time=T_SLOW)

        inflation_label = Text("Area grew! ✗", font_size=FS_TINY, color=C_BAD)
        inflation_label.next_to(bad_blob, DOWN, buff=0.1)
        self.play(Write(inflation_label))
        self.wait(T_LONG_PAUSE)

        # ===== STEP 6: The key theorem — modified Hamiltonian =====
        self.play(FadeOut(*self.mobjects))

        theorem_title = Text(
            "The Modified Hamiltonian Theorem",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        theorem_title.to_edge(UP, buff=0.5)

        # Box the theorem statement
        theorem_statement = (
            r"A symplectic integrator with stepsize $h$ and order $p$"
            "\n"
            r"is the \textit{exact} flow of a modified Hamiltonian:"
            "\n\n"
            r"$\tilde{H} = H + h^p H_1 + h^{p+1} H_2 + \cdots$"
        )
        theorem_box = make_equation_box(
            r"\tilde{H} = H + h^p H_1 + h^{p+1} H_2 + \cdots",
            box_color=C_PRIMARY,
            fill_opacity=0.08,
        )
        theorem_box.scale(1.2).move_to(ORIGIN).shift(DOWN * 0.5)

        theorem_caption = Text(
            "The numerical trajectory EXACTLY tracks a nearby physical Hamiltonian.",
            font_size=FS_SMALL,
            color=C_NEUTRAL,
        )
        theorem_caption.next_to(theorem_box, DOWN, buff=0.5)

        consequence1 = Text(
            "→ Energy cannot drift monotonically",
            font_size=FS_SMALL, color=C_GOOD,
        )
        consequence2 = Text(
            "→ Error in H is O(hᵖ) and bounded for all time",
            font_size=FS_SMALL, color=C_GOOD,
        )
        consequence1.next_to(theorem_caption, DOWN, buff=0.3).align_to(theorem_caption, LEFT)
        consequence2.next_to(consequence1, DOWN, buff=0.2).align_to(consequence1, LEFT)

        self.play(Write(theorem_title))
        self.play(Create(theorem_box), run_time=T_SLOW)
        self.play(Write(theorem_caption))
        self.play(Write(consequence1))
        self.play(Write(consequence2))
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 5: SymplecticEuler
# Runtime: ~4:00
# Goal: Derive symplectic Euler, show WHY it's symplectic (splitting), compare.
# ==============================================================================

class SymplecticEuler(Scene):
    """
    The symplectic Euler method — first symplectic integrator.

    Visual flow:
    1. Write Forward Euler equations
    2. Highlight the single change: q uses new p
    3. Show this = composition of two exact flows (why it's symplectic)
    4. Phase space comparison: Euler vs Symplectic Euler
    5. Energy comparison plot
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        title = Text(
            "The Symplectic Euler Method",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Recall Forward Euler =====
        recall = Text("Recall: Forward Euler", font_size=FS_SMALL, color=C_DIM)
        recall.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        fe_q = MathTex(
            r"q_{n+1} &= q_n + h \cdot \frac{\partial H}{\partial p}(q_n, p_n)",
            font_size=FS_BODY, color=C_BAD,
        )
        fe_p = MathTex(
            r"p_{n+1} &= p_n - h \cdot \frac{\partial H}{\partial q}(q_n, p_n)",
            font_size=FS_BODY, color=C_BAD,
        )
        fe_q.next_to(recall, DOWN, buff=0.3).align_to(recall, LEFT)
        fe_p.next_to(fe_q, DOWN, buff=0.4).align_to(fe_q, LEFT)

        not_symplectic = Text(
            "← NOT symplectic", font_size=FS_TINY, color=C_BAD
        )
        not_symplectic.next_to(fe_p, RIGHT, buff=0.4)

        self.play(Write(recall))
        self.play(Write(fe_q), Write(fe_p))
        self.play(Write(not_symplectic))
        self.wait(T_PAUSE)

        # ===== STEP 2: The single crucial change =====
        arrow_down = Arrow(
            fe_p.get_bottom() + DOWN * 0.1,
            fe_p.get_bottom() + DOWN * 0.8,
            color=C_PRIMARY,
            stroke_width=2.5,
            tip_length=0.2,
        )
        change_text = Text(
            "One small change...",
            font_size=FS_SMALL, color=C_PRIMARY,
        )
        change_text.next_to(arrow_down, DOWN, buff=0.1)
        self.play(Create(arrow_down), Write(change_text))
        self.wait(T_PAUSE)

        self.play(FadeOut(fe_q, fe_p, not_symplectic, arrow_down, change_text, recall))

        # Symplectic Euler equations — now with p_{n+1} in the q update
        se_title = Text("Symplectic Euler:", font_size=FS_SMALL, color=C_GOOD)
        se_title.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        se_p = MathTex(
            r"p_{n+1} &= p_n - h \cdot \frac{\partial H}{\partial q}(q_n, p_n)",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        # The key line — q now uses p_{n+1}
        se_q = MathTex(
            r"q_{n+1} &= q_n + h \cdot \frac{\partial H}{\partial p}(q_n,\, ",
            r"\mathbf{p_{n+1}}",
            r")",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        se_p.next_to(se_title, DOWN, buff=0.3).align_to(se_title, LEFT)
        se_q.next_to(se_p, DOWN, buff=0.4).align_to(se_p, LEFT)

        # Brace + note pointing to p_{n+1} in the q equation
        key_brace = Brace(
            se_q[1],
            direction=DOWN,
            color=C_GOOD,
        )
        key_note = Text(
            "Use NEW p, not old p!",
            font_size=FS_TINY,
            color=C_GOOD,
        )
        key_brace.put_at_tip(key_note, buff=0.1)

        self.play(Write(se_title))
        self.play(Write(se_p))
        self.play(Write(se_q), run_time=T_SLOW)
        self.play(GrowFromCenter(key_brace), Write(key_note))
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(key_brace, key_note))

        # ===== STEP 3: Why is this symplectic? Splitting argument =====
        why_title = Text(
            "Why is this symplectic?  (The splitting argument)",
            font_size=FS_SMALL, color=C_PRIMARY,
        )
        why_title.next_to(se_q, DOWN, buff=0.6).align_to(se_q, LEFT)

        # For a separable Hamiltonian H = T(p) + V(q), the step splits into:
        step1 = MathTex(
            r"1.\ p_{n+1} = p_n - h\,\nabla_q V(q_n)",
            r"\quad \text{(exact flow of }V\text{ for time }h\text{)}",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        step2 = MathTex(
            r"2.\ q_{n+1} = q_n + h\,\nabla_p T(p_{n+1})",
            r"\quad \text{(exact flow of }T\text{ for time }h\text{)}",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        step1.next_to(why_title, DOWN, buff=0.3).align_to(why_title, LEFT)
        step2.next_to(step1, DOWN, buff=0.3).align_to(step1, LEFT)

        conclusion = Text(
            "Composition of exact Hamiltonian flows = symplectic map ✓",
            font_size=FS_SMALL, color=C_GOOD,
        )
        conclusion.next_to(step2, DOWN, buff=0.4).align_to(step2, LEFT)

        self.play(Write(why_title))
        self.play(Write(step1))
        self.play(Write(step2))
        self.play(Write(conclusion))
        self.wait(T_LONG_PAUSE)

        # Transition: fade left side, show comparison on full screen
        self.play(FadeOut(*self.mobjects))

        # ===== STEP 4: Side-by-side phase portrait comparison =====
        compare_title = Text(
            "Phase portrait comparison  (100 steps, h = 0.1)",
            font_size=FS_SMALL, color=C_DIM,
        )
        compare_title.to_edge(UP, buff=0.5)
        self.play(Write(compare_title))

        # --- Left: Forward Euler ---
        axes_left = Axes(
            x_range=[-2.5, 2.5, 1], y_range=[-2.5, 2.5, 1],
            x_length=5.0, y_length=5.0,
            axis_config={"color": C_DIM, "stroke_width": 1.2},
        )
        axes_left.to_edge(LEFT, buff=0.5).shift(DOWN * 0.3)

        label_left = Text("Forward Euler", font_size=FS_SMALL, color=C_BAD)
        label_left.next_to(axes_left, DOWN, buff=0.2)

        # True orbit reference
        ref_left = ParametricFunction(
            lambda t: axes_left.c2p(np.cos(t), -np.sin(t)),
            t_range=[0, 2 * PI], color=C_DIM, stroke_width=1.0,
        )
        ref_left = DashedVMobject(ref_left, dashed_ratio=0.57)

        qs_fe, ps_fe, _ = simulate_harmonic_oscillator(1.0, 0.0, 0.1, 300, "euler")
        fe_path = VMobject(color=C_BAD, stroke_width=2.0)
        fe_pts = [axes_left.c2p(qs_fe[i], ps_fe[i]) for i in range(300)]
        fe_path.set_points_as_corners(fe_pts)

        # --- Right: Symplectic Euler ---
        axes_right = Axes(
            x_range=[-2.5, 2.5, 1], y_range=[-2.5, 2.5, 1],
            x_length=5.0, y_length=5.0,
            axis_config={"color": C_DIM, "stroke_width": 1.2},
        )
        axes_right.to_edge(RIGHT, buff=0.5).shift(DOWN * 0.3)

        label_right = Text("Symplectic Euler", font_size=FS_SMALL, color=C_GOOD)
        label_right.next_to(axes_right, DOWN, buff=0.2)

        ref_right = ParametricFunction(
            lambda t: axes_right.c2p(np.cos(t), -np.sin(t)),
            t_range=[0, 2 * PI], color=C_DIM, stroke_width=1.0,
        )
        ref_right = DashedVMobject(ref_right, dashed_ratio=0.57)

        qs_se, ps_se, _ = simulate_harmonic_oscillator(1.0, 0.0, 0.1, 300, "symplectic_euler")
        se_path = VMobject(color=C_GOOD, stroke_width=2.0)
        se_pts = [axes_right.c2p(qs_se[i], ps_se[i]) for i in range(300)]
        se_path.set_points_as_corners(se_pts)

        # Animate both sides simultaneously
        self.play(
            Create(axes_left), Create(axes_right),
            Write(label_left), Write(label_right),
            run_time=T_NORMAL,
        )
        self.play(
            Create(ref_left), Create(ref_right),
        )
        self.play(
            Create(fe_path), Create(se_path),
            run_time=2.0,
        )
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 6: LeapfrogMethod
# Runtime: ~4:30
# Goal: Derive leapfrog from symmetrization. Show staggered grid.
# ==============================================================================

class LeapfrogMethod(Scene):
    """
    The Leapfrog / Störmer-Verlet integrator.

    Visual flow:
    1. Motivate: symplectic Euler is 1st order. Can we do better?
    2. Time-reversal symmetry → 2nd order
    3. Draw the staggered grid visually
    4. Write the three-step algorithm
    5. Show it's symmetric Strang splitting
    6. Energy plot comparison: SE vs Leapfrog
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        title = Text(
            "The Leapfrog / Störmer-Verlet Method",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Motivate — first order isn't enough =====
        motivate = Text(
            "Symplectic Euler is 1st order: local error O(h²), global O(h).",
            font_size=FS_SMALL, color=C_DIM,
        )
        motivate.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=0.8)
        question = Text(
            "Can we get 2nd order while keeping symplecticity?",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        question.next_to(motivate, DOWN, buff=0.3).align_to(motivate, LEFT)

        self.play(Write(motivate))
        self.play(Write(question))
        self.wait(T_PAUSE)

        answer = Text(
            "Yes! Use time-reversal symmetry.",
            font_size=FS_SMALL, color=C_GOOD,
        )
        answer.next_to(question, DOWN, buff=0.5).align_to(question, LEFT)
        self.play(Write(answer))
        self.wait(T_PAUSE)

        self.play(FadeOut(motivate, question, answer))

        # ===== STEP 2: Staggered grid visualization =====
        grid_title = Text(
            "Idea: stagger q and p by half a timestep",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        grid_title.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=0.8)
        self.play(Write(grid_title))

        # Draw a horizontal time axis
        t_line = Line(
            LEFT * 5.5, RIGHT * 5.5,
            color=C_DIM, stroke_width=1.5,
        ).shift(DOWN * 0.5)
        t_label = Text("time t", font_size=FS_TINY, color=C_DIM)
        t_label.next_to(t_line, RIGHT, buff=0.2)

        self.play(Create(t_line), Write(t_label))

        # Integer time steps for q (positions)
        q_steps = [-2, -1, 0, 1, 2]    # In units of h, relative to center
        q_dots = VGroup()
        q_labels = VGroup()

        # Place a dot and label for each q step point
        for k in q_steps:
            pos = t_line.get_center() + RIGHT * k * 2.0  # 2 Manim units per h
            dot = Dot(pos, color=C_PRIMARY, radius=0.1)
            lbl = MathTex(f"q_{{{k}}}", font_size=FS_SMALL, color=C_PRIMARY)
            lbl.next_to(dot, UP, buff=0.2)
            q_dots.add(dot)
            q_labels.add(lbl)

        # Half-integer time steps for p (momenta) — shifted by 0.5 in time
        p_steps = [-1.5, -0.5, 0.5, 1.5]
        p_dots = VGroup()
        p_labels = VGroup()

        for k in p_steps:
            pos = t_line.get_center() + RIGHT * k * 2.0 + DOWN * 0.6
            dot = Dot(pos, color=C_SECONDARY, radius=0.1)
            # Format: p_{n+1/2} — use fractions in LaTeX
            frac = "+\\tfrac{1}{2}" if (k - int(k) == 0.5) else "-\\tfrac{1}{2}"
            base = int(k + 0.5) if k > 0 else int(k - 0.5 + 1)
            lbl = MathTex(
                rf"p_{{n{frac}}}",
                font_size=FS_TINY, color=C_SECONDARY,
            )
            lbl.next_to(dot, DOWN, buff=0.2)
            p_dots.add(dot)
            p_labels.add(lbl)

        # Row labels
        q_row_label = Text("positions q:", font_size=FS_TINY, color=C_PRIMARY)
        q_row_label.move_to(t_line.get_left() + LEFT * 0.8 + UP * 0.4)
        p_row_label = Text("momenta p:", font_size=FS_TINY, color=C_SECONDARY)
        p_row_label.move_to(t_line.get_left() + LEFT * 0.8 + DOWN * 0.5)

        self.play(Write(q_row_label), Write(p_row_label))
        self.play(
            LaggedStart(*[FadeIn(d) for d in q_dots], lag_ratio=0.15),
            LaggedStart(*[Write(l) for l in q_labels], lag_ratio=0.15),
        )
        self.wait(T_PAUSE)
        self.play(
            LaggedStart(*[FadeIn(d) for d in p_dots], lag_ratio=0.15),
            LaggedStart(*[Write(l) for l in p_labels], lag_ratio=0.15),
        )
        self.wait(T_PAUSE)

        # Draw arrows showing "leapfrog" — q and p alternate
        leap_arrow1 = CurvedArrow(
            q_dots[2].get_center(),
            p_dots[1].get_center() + LEFT * 0.1,
            color=C_DIM,
            angle=-PI / 4,
            tip_length=0.12,
        )
        leap_arrow2 = CurvedArrow(
            p_dots[1].get_center() + RIGHT * 0.1,
            q_dots[3].get_center(),
            color=C_DIM,
            angle=-PI / 4,
            tip_length=0.12,
        )
        self.play(Create(leap_arrow1), Create(leap_arrow2))
        self.wait(T_PAUSE)

        self.play(FadeOut(
            grid_title, t_line, t_label, q_dots, q_labels,
            p_dots, p_labels, q_row_label, p_row_label,
            leap_arrow1, leap_arrow2,
        ))

        # ===== STEP 3: The leapfrog algorithm =====
        algo_title = Text(
            "The Leapfrog Algorithm:",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        algo_title.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=1.0)

        lf_step1 = MathTex(
            r"1.\ p_{n+\frac{1}{2}} = p_n - \frac{h}{2}\,\frac{\partial V}{\partial q}(q_n)",
            font_size=FS_BODY, color=C_SECONDARY,
        )
        lf_step2 = MathTex(
            r"2.\ q_{n+1} = q_n + h\,\frac{\partial T}{\partial p}\!\left(p_{n+\frac{1}{2}}\right)",
            font_size=FS_BODY, color=C_PRIMARY,
        )
        lf_step3 = MathTex(
            r"3.\ p_{n+1} = p_{n+\frac{1}{2}} - \frac{h}{2}\,\frac{\partial V}{\partial q}(q_{n+1})",
            font_size=FS_BODY, color=C_SECONDARY,
        )

        lf_step1.next_to(algo_title, DOWN, buff=0.3).align_to(algo_title, LEFT)
        lf_step2.next_to(lf_step1, DOWN, buff=0.4).align_to(lf_step1, LEFT)
        lf_step3.next_to(lf_step2, DOWN, buff=0.4).align_to(lf_step2, LEFT)

        lf_step1_note = Text("← half kick", font_size=FS_TINY, color=C_DIM)
        lf_step2_note = Text("← full drift", font_size=FS_TINY, color=C_DIM)
        lf_step3_note = Text("← half kick", font_size=FS_TINY, color=C_DIM)
        lf_step1_note.next_to(lf_step1, RIGHT, buff=0.3)
        lf_step2_note.next_to(lf_step2, RIGHT, buff=0.3)
        lf_step3_note.next_to(lf_step3, RIGHT, buff=0.3)

        self.play(Write(algo_title))
        self.play(Write(lf_step1), Write(lf_step1_note))
        self.play(Write(lf_step2), Write(lf_step2_note))
        self.play(Write(lf_step3), Write(lf_step3_note))
        self.wait(T_PAUSE)

        # Strang splitting connection
        strang_note = Text(
            "= φ_V(h/2) ∘ φ_T(h) ∘ φ_V(h/2)    (Strang splitting — symmetric!)",
            font_size=FS_SMALL, color=C_ACCENT,
        )
        strang_note.next_to(lf_step3, DOWN, buff=0.5).align_to(lf_step3, LEFT)
        self.play(Write(strang_note))
        self.wait(T_PAUSE)

        # Time reversal note
        time_rev = Text(
            "Symmetric → time-reversible → automatically 2nd order accurate",
            font_size=FS_SMALL, color=C_GOOD,
        )
        time_rev.next_to(strang_note, DOWN, buff=0.3).align_to(strang_note, LEFT)
        self.play(Write(time_rev))
        self.wait(T_LONG_PAUSE)

        # ===== STEP 4: Energy comparison =====
        self.play(FadeOut(*self.mobjects))

        compare_title = Text(
            "Energy conservation: Euler vs Symplectic Euler vs Leapfrog",
            font_size=FS_SMALL, color=C_DIM,
        )
        compare_title.to_edge(UP, buff=0.5)
        self.play(Write(compare_title))

        # Run all three simulations
        N = 1000
        h = 0.1
        _, _, en_fe  = simulate_harmonic_oscillator(1.0, 0.0, h, N, "euler")
        _, _, en_se  = simulate_harmonic_oscillator(1.0, 0.0, h, N, "symplectic_euler")
        _, _, en_lf  = simulate_harmonic_oscillator(1.0, 0.0, h, N, "leapfrog")

        e_axes = Axes(
            x_range=[0, N, N // 5],
            y_range=[0.4, 0.7, 0.05],
            x_length=10.0,
            y_length=5.5,
            axis_config={"color": C_DIM, "stroke_width": 1.5},
        )
        e_axes.center().shift(DOWN * 0.3)

        e_xl = e_axes.get_x_axis_label(
            Text("Integration steps", font_size=FS_TINY, color=C_DIM),
            edge=RIGHT, direction=DOWN,
        )
        e_yl = e_axes.get_y_axis_label(
            MathTex("H(q,p)", font_size=FS_SMALL, color=C_DIM),
            edge=UP, direction=LEFT,
        )

        self.play(Create(e_axes), Write(e_xl), Write(e_yl))

        # True energy dashed line
        true_line = e_axes.plot(
            lambda x: 0.5, x_range=[0, N],
            color=C_DIM, stroke_width=1.0,
        )
        true_line = DashedVMobject(true_line, dashed_ratio=0.588)
        self.play(Create(true_line))

        # Plot each method
        fe_curve = e_axes.plot_line_graph(
            list(range(N)), list(np.clip(en_fe, 0.4, 0.69)),
            line_color=C_BAD, stroke_width=2.0, add_vertex_dots=False,
        )
        se_curve = e_axes.plot_line_graph(
            list(range(N)), list(np.clip(en_se, 0.4, 0.69)),
            line_color=C_SECONDARY, stroke_width=2.0, add_vertex_dots=False,
        )
        lf_curve = e_axes.plot_line_graph(
            list(range(N)), list(np.clip(en_lf, 0.4, 0.69)),
            line_color=C_GOOD, stroke_width=2.5, add_vertex_dots=False,
        )

        # Animate them one at a time
        fe_label = Text("Euler (diverging)", font_size=FS_TINY, color=C_BAD)
        fe_label.to_corner(UR, buff=0.4).shift(DOWN * 0.2)

        se_label = Text("Symplectic Euler (bounded)", font_size=FS_TINY, color=C_SECONDARY)
        se_label.next_to(fe_label, DOWN, buff=0.2).align_to(fe_label, LEFT)

        lf_label = Text("Leapfrog (tightly bounded)", font_size=FS_TINY, color=C_GOOD)
        lf_label.next_to(se_label, DOWN, buff=0.2).align_to(se_label, LEFT)

        self.play(Create(fe_curve), Write(fe_label), run_time=1.5)
        self.play(Create(se_curve), Write(se_label), run_time=1.5)
        self.play(Create(lf_curve), Write(lf_label), run_time=1.5)
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 7: SplittingMethods
# Runtime: ~4:30
# Goal: Show operator splitting, composition to higher order.
# ==============================================================================

class SplittingMethods(Scene):
    """
    Operator splitting and composition methods.

    Visual flow:
    1. Hamiltonian splitting H = T + V
    2. Two exact sub-flows φ_T and φ_V on phase space
    3. 1st-order splitting = concatenate the two flows
    4. 2nd-order = Strang splitting (symmetric sandwich)
    5. 4th-order via Forest-Ruth composition
    6. Error scaling plot: order vs accuracy
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        title = Text(
            "Splitting Methods & Higher Order",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: The splitting concept =====
        split_explain = Text(
            "For a separable Hamiltonian, we split into two exactly-solvable pieces:",
            font_size=FS_SMALL, color=C_DIM,
        )
        split_explain.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=0.8)

        h_split = MathTex(
            r"H(q,p) = H_A(q,p) + H_B(q,p)",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        h_split.next_to(split_explain, DOWN, buff=0.3).align_to(split_explain, LEFT)

        self.play(Write(split_explain))
        self.play(Write(h_split), run_time=T_SLOW)
        self.wait(T_PAUSE)

        # The two sub-flows
        phi_t = MathTex(
            r"\varphi_T(h):\ H_A \mapsto H_A + h\,\nabla_p T(H_A),\quad H_A \mapsto H_A",
            font_size=FS_SMALL, color=C_PRIMARY,
        )
        phi_v = MathTex(
            r"\varphi_V(h):\ H_B \mapsto H_B - h\,\nabla_q V(H_B),\quad H_B \mapsto H_B",
            font_size=FS_SMALL, color=C_SECONDARY,
        )
        phi_t.next_to(h_split, DOWN, buff=0.4).align_to(h_split, LEFT)
        phi_v.next_to(phi_t, DOWN, buff=0.3).align_to(phi_t, LEFT)

        phi_t_note = Text("moves only H_A", font_size=FS_TINY, color=C_PRIMARY)
        phi_v_note = Text("moves only H_B", font_size=FS_TINY, color=C_SECONDARY)
        phi_t_note.next_to(phi_t, RIGHT, buff=0.3)
        phi_v_note.next_to(phi_v, RIGHT, buff=0.3)

        Hat_note = Text("Then the true hamiltonian flow is:", font_size=FS_SMALL, color=C_DIM)
        Hat_math = MathTex(r"\phi_H(h) = \exp(h\{H, \cdot\})",font_size=FS_SMALL*2, color=C_SECONDARY,)

        Hat_note.next_to(phi_v, DOWN, buff=0.5).align_to(phi_v, LEFT)
        Hat_math.next_to(Hat_note, DOWN, buff=0.2).align_to(Hat_note, LEFT)
        
        self.play(Write(phi_t), Write(phi_t_note))
        self.play(Write(phi_v), Write(phi_v_note))
        
        self.play(Write(Hat_note), Write(Hat_math))
        self.wait(T_PAUSE)

        self.play(FadeOut(split_explain, h_split, phi_t, phi_v,
                          phi_t_note, phi_v_note, Hat_note, Hat_math))

        # ===== STEP 2: Composition table =====
        comp_title = Text(
            "Building higher-order methods by composition:",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        comp_title.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=0.8)
        self.play(Write(comp_title))

        # 1st order
        order1_label = Text("1st order:", font_size=FS_SMALL, color=C_DIM)
        order1_eq = MathTex(
            r"\Psi_1(h) = \varphi_V(h) \circ \varphi_T(h)",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        order1_note = Text(
            "= Symplectic Euler (one T-step then one V-step)",
            font_size=FS_TINY, color=C_DIM,
        )
        order1_label.next_to(comp_title, DOWN, buff=0.5).to_edge(LEFT, buff=1.2)
        order1_eq.next_to(order1_label, RIGHT, buff=0.3)
        order1_note.next_to(order1_eq, DOWN, buff=0.1).align_to(order1_eq, LEFT)

        # 2nd order (Strang)
        order2_label = Text("2nd order:", font_size=FS_SMALL, color=C_DIM)
        order2_eq = MathTex(
            r"\Psi_2(h) = \varphi_V\!\!\left(\tfrac{h}{2}\right) \circ \varphi_T(h) \circ \varphi_V\!\!\left(\tfrac{h}{2}\right)",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        order2_note = Text(
            "= Strang splitting = Leapfrog",
            font_size=FS_TINY, color=C_GOOD,
        )
        order2_label.next_to(order1_note, DOWN, buff=0.5).to_edge(LEFT, buff=1.2)
        order2_eq.next_to(order2_label, RIGHT, buff=0.3)
        order2_note.next_to(order2_eq, DOWN, buff=0.1).align_to(order2_eq, LEFT)

        # 4th order (Forest-Ruth)
        order4_label = Text("4th order:", font_size=FS_SMALL, color=C_DIM)
        order4_eq = MathTex(
            r"\Psi_4(h) = \Psi_2(\theta h) \circ \Psi_2((1-2\theta)h) \circ \Psi_2(\theta h)",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        theta_val = MathTex(
            r"\theta = \frac{1}{2 - 2^{1/3}} \approx 1.3512",
            font_size=FS_SMALL, color=C_ACCENT,
        )
        order4_label.next_to(order2_note, DOWN, buff=0.5).to_edge(LEFT, buff=1.2)
        order4_eq.next_to(order4_label, RIGHT, buff=0.3)
        theta_val.next_to(order4_eq, DOWN, buff=0.2).align_to(order4_eq, LEFT)

        self.play(Write(order1_label), Write(order1_eq))
        self.play(Write(order1_note))
        self.wait(T_PAUSE)
        self.play(Write(order2_label), Write(order2_eq))
        self.play(Write(order2_note))
        self.wait(T_PAUSE)
        self.play(Write(order4_label), Write(order4_eq))
        self.play(Write(theta_val))
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects))






# ==============================================================================
# SCENE 8: NBodiesDemo
# Runtime: ~5:00
# Goal: Show the leapfrog on an N-body simulation with visual orbits.
# ==============================================================================

class NBodiesDemo(Scene):
    """
    Simulating gravitational N-body dynamics with leapfrog.

    Visual flow:
    1. Show the gravitational Hamiltonian formula
    2. Note T depends only on p, V depends only on q → separable!
    3. Animate a simple 2-body (Sun-Earth) orbit
    4. Show energy conservation over many orbits
    5. Quick code walkthrough
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        title = Text(
            "Application: Gravitational N-Body",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Gravitational Hamiltonian =====
        ham_label = Text(
            "N-body gravitational Hamiltonian:",
            font_size=FS_SMALL, color=C_DIM,
        )
        ham_label.next_to(title, DOWN, buff=0.5).to_edge(LEFT, buff=0.8)

        h_gravity = MathTex(
            r"H = ",
            r"\sum_{i} \frac{|\mathbf{p}_i|^2}{2m_i}",
            r" - ",
            r"\sum_{i < j} \frac{G m_i m_j}{|\mathbf{q}_i - \mathbf{q}_j|}",
            font_size=FS_BODY, color=C_NEUTRAL,
        )
        h_gravity.next_to(ham_label, DOWN, buff=0.3).align_to(ham_label, LEFT)

        # Annotate T and V parts
        t_brace = Brace(
            h_gravity[1],
            direction=DOWN, color=C_PRIMARY,
        )
        t_text = MathTex(r"T(\mathbf{p})", font_size=FS_SMALL, color=C_PRIMARY)
        t_brace.put_at_tip(t_text, buff=0.1)

        v_brace = Brace(
            h_gravity[3],
            direction=DOWN, color=C_SECONDARY,
        )
        v_text = MathTex(r"V(\mathbf{q})", font_size=FS_SMALL, color=C_SECONDARY)
        v_brace.put_at_tip(v_text, buff=0.1)

        separable_note = Text(
            "Separable! T depends only on p, V only on q → leapfrog applies directly.",
            font_size=FS_SMALL, color=C_GOOD,
        )
        separable_note.next_to(t_brace, DOWN, buff=0.6).align_to(ham_label, LEFT)

        self.play(Write(ham_label))
        self.play(Write(h_gravity), run_time=T_SLOW)
        self.play(GrowFromCenter(t_brace), Write(t_text),
                  GrowFromCenter(v_brace), Write(v_text))
        self.play(Write(separable_note))
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(ham_label, h_gravity, t_brace, t_text,
                          v_brace, v_text, separable_note))

        # ===== STEP 2: Simulate and animate a circular orbit =====
        # We'll use a very simplified 2-body: Sun fixed at origin,
        # Earth in circular orbit. This lets us animate cleanly.

        # Orbital parameters
        R = 2.5       # Orbital radius in Manim units (for display)
        omega = 1.0   # Angular frequency (units chosen so T=2π)

        # Set up the orbital plane display
        orbit_circle = Circle(
            radius=R,
            color=C_DIM,
            stroke_width=1.0,
            stroke_opacity=0.4,
        )
        orbit_circle.move_to(ORIGIN)

        # Sun at the origin
        sun = Dot(ORIGIN, radius=0.25, color=C_YELLOW)
        sun_glow = Circle(radius=0.4, color=C_YELLOW, fill_opacity=0.15,
                          stroke_width=0)
        sun_label = Text("Sun", font_size=FS_TINY, color=C_YELLOW)
        sun_label.next_to(sun, DOWN, buff=0.15)

        # Earth — starts at (R, 0)
        earth = Dot(RIGHT * R, radius=0.12, color=C_PRIMARY)
        earth_label = Text("Earth", font_size=FS_TINY, color=C_PRIMARY)

        self.play(
            Create(orbit_circle),
            FadeIn(sun, sun_glow, sun_label),
            FadeIn(earth),
            run_time=T_NORMAL,
        )

        # Updater: Earth moves in a circle parameterized by the angle tracker
        angle = ValueTracker(0)

        earth.add_updater(
            lambda e: e.move_to(
                RIGHT * R * np.cos(angle.get_value())
                + UP * R * np.sin(angle.get_value())
            )
        )
        earth_label.add_updater(
            lambda l: l.next_to(earth, UR, buff=0.1)
        )
        self.add(earth_label)

        # Trace the Earth's path (shows as the orbit is traversed)
        earth_trace = TracedPath(
            earth.get_center,
            stroke_color=C_GOOD,
            stroke_width=1.5,
            dissipating_time=0.8 * 2 * PI,  # Trace persists for ~one orbit
        )
        self.add(earth_trace)

        # Gravitational force arrow
        force_arrow = always_redraw(
            lambda: Arrow(
                start=earth.get_center(),
                end=earth.get_center() * 0.4,  # Toward origin (Sun)
                color=C_SECONDARY,
                stroke_width=1.5,
                tip_length=0.15,
                buff=0.0,
            )
        )
        force_label = Text("gravity", font_size=FS_TINY, color=C_SECONDARY)
        # We'll use an updater to keep this near the arrow midpoint
        force_label.add_updater(
            lambda l: l.move_to(
                (earth.get_center() + earth.get_center() * 0.4) / 2
                + RIGHT * 0.3
            )
        )
        self.add(force_arrow, force_label)

        # Run 2 full orbits
        self.play(
            angle.animate.set_value(4 * PI),
            run_time=5.0,
            rate_func=linear,
        )
        earth.clear_updaters()
        earth_label.clear_updaters()
        force_label.clear_updaters()
        self.wait(T_PAUSE)

        # ===== STEP 3: Energy conservation note =====
        # Move orbit visual to the left, show energy plot on right
        orbit_group = VGroup(
            orbit_circle, sun, sun_glow, sun_label,
            earth, earth_label, earth_trace,
        )
        self.play(orbit_group.animate.scale(0.6).to_edge(LEFT, buff=0.5))

        energy_note = Text(
            "After 10,000 orbits\nwith leapfrog:\n\nEnergy drift < 0.001%\nOrbits remain stable",
            font_size=FS_SMALL,
            color=C_GOOD,
            line_spacing=1.4,
        )
        energy_note.to_edge(RIGHT, buff=1.5).shift(UP * 0.5)

        rk4_note = Text(
            "Same run with RK4:\n\nEnergy drift: ~12%\nOrbit unstable",
            font_size=FS_SMALL,
            color=C_BAD,
            line_spacing=1.4,
        )
        rk4_note.to_edge(RIGHT, buff=1.5).shift(DOWN * 1.5)

        self.play(Write(energy_note), run_time=T_SLOW)
        self.play(Write(rk4_note), run_time=T_SLOW)
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)


# ==============================================================================
# SCENE 9: PracticalConsiderations
# Runtime: ~3:30
# Goal: Summarize practical advice. Method selection table. Timestep guidance.
# ==============================================================================

class PracticalConsiderations(Scene):
    """
    Practical guide: which method to use and when.

    Visual flow:
    1. Method selection table
    2. Timestep rule of thumb (50-100 steps per period)
    3. Common pitfalls (adaptive steps, non-separable H)
    4. Conserved quantity monitoring
    5. Final summary list
    """

    def construct(self):
        self.camera.background_color = C_BACKGROUND

        title = Text(
            "Practical Considerations",
            font_size=FS_HEADING,
            color=C_PRIMARY,
        )
        title.to_edge(UP, buff=0.4)
        self.play(Write(title))

        # ===== STEP 1: Method selection table =====
        # We'll build a table manually using Lines and Text objects.
        # Manim has a Table class but manual construction gives more control.

        table_title = Text(
            "Which method should you use?",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        table_title.next_to(title, DOWN, buff=0.5)
        self.play(Write(table_title))

        # Table data: (Method, Order, Cost, Best for)
        rows = [
            ("Method",        "Order", "Force evals/step", "Best for"),
            ("Symplectic Euler", "1st", "1", "Quick prototyping"),
            ("Leapfrog/Verlet", "2nd", "1", "Most applications"),
            ("Ruth-Candy",      "4th", "3", "High precision"),
            ("Forest-Ruth",     "4th", "3", "Long-time simulation"),
            ("Implicit Midpt",  "2nd", "1 (+ solve)", "Stiff systems"),
        ]
        row_colors = [
            C_DIM,         # header
            C_DIM,         # row 1
            C_GOOD,        # row 2 (recommended)
            C_PRIMARY,     # row 3
            C_PRIMARY,     # row 4
            C_ACCENT,      # row 5
        ]

        # Column widths and x positions (in Manim units from center)
        col_x = [-4.5, -2.0, 0.5, 2.8]
        row_y_start = 1.5
        row_height = 0.55

        # Draw header separator line
        header_line = Line(
            LEFT * 5.5 + UP * (row_y_start - row_height * 0.8),
            RIGHT * 5.5 + UP * (row_y_start - row_height * 0.8),
            color=C_DIM, stroke_width=1.0,
        )

        table_cells = VGroup()
        for i, (row, col) in enumerate(zip(rows, row_colors)):
            y = row_y_start - i * row_height
            is_header = (i == 0)
            is_recommended = (i == 2)  # Leapfrog row

            # Highlight recommended row
            if is_recommended:
                highlight = Rectangle(
                    width=11.0, height=row_height,
                    color=C_GOOD, fill_color=C_GOOD, fill_opacity=0.08,
                    stroke_width=0.5,
                )
                highlight.move_to(UP * y)
                table_cells.add(highlight)

            for j, (text, x) in enumerate(zip(row, col_x)):
                cell = Text(
                    text,
                    font_size=FS_TINY if not is_header else FS_SMALL,
                    color=col if not is_header else C_NEUTRAL,
                    weight=BOLD if is_header else NORMAL,
                )
                cell.move_to(RIGHT * x + UP * y)
                table_cells.add(cell)

        self.play(
            FadeIn(header_line),
            LaggedStart(
                *[Write(c) for c in table_cells],
                lag_ratio=0.05,
                run_time=2.5,
            ),
        )
        self.wait(T_PAUSE)

        # Annotation: arrow to the recommended row
        rec_arrow = Arrow(
            RIGHT * 5.8 + UP * (row_y_start - 2 * row_height),
            RIGHT * 4.9 + UP * (row_y_start - 2 * row_height),
            color=C_GOOD, stroke_width=2.0, tip_length=0.15,
        )
        rec_text = Text("Recommended default", font_size=FS_TINY, color=C_GOOD)
        rec_text.next_to(rec_arrow, RIGHT, buff=0.1)
        self.play(Create(rec_arrow), Write(rec_text))
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(table_title, header_line, table_cells, rec_arrow, rec_text))

        # ===== STEP 2: Timestep guidance =====
        ts_title = Text(
            "Choosing the timestep h:",
            font_size=FS_SMALL, color=C_NEUTRAL,
        )
        ts_title.next_to(title, DOWN, buff=0.6).to_edge(LEFT, buff=1.0)

        ts_rule = MathTex(
            r"h \approx \frac{T_{\min}}{100}",
            font_size=FS_BODY, color=C_PRIMARY,
        )
        ts_rule.next_to(ts_title, DOWN, buff=0.4).align_to(ts_title, LEFT)

        ts_explain = Text(
            "Use ~50-100 steps per shortest period in the system.",
            font_size=FS_SMALL, color=C_DIM,
        )
        ts_explain.next_to(ts_rule, DOWN, buff=0.3).align_to(ts_rule, LEFT)

        ts_code = Text(
            '# Example: for harmonic oscillator with ω\n'
            'T = 2 * pi / omega    # orbital period\n'
            'h = T / 100           # 100 steps per orbit',
            font_size=FS_TINY, color=C_ACCENT,
            font="Courier New",
        )
        ts_code.next_to(ts_explain, DOWN, buff=0.4).align_to(ts_explain, LEFT)

        self.play(Write(ts_title))
        self.play(Write(ts_rule))
        self.play(Write(ts_explain))
        self.play(Write(ts_code))
        self.wait(T_PAUSE)

        # ===== STEP 3: Common pitfalls =====
        pitfalls_title = Text(
            "⚠  Common pitfalls:",
            font_size=FS_SMALL, color=C_YELLOW,
        )
        pitfalls_title.next_to(ts_code, DOWN, buff=0.6).to_edge(LEFT, buff=1.0)

        pitfall1 = Text(
            "1. Adaptive timesteps destroy symplecticity — use fixed h",
            font_size=FS_TINY, color=C_BAD,
        )
        pitfall2 = Text(
            "2. Non-separable H(q,p) needs different splitting or implicit methods",
            font_size=FS_TINY, color=C_BAD,
        )
        pitfall3 = Text(
            "3. Always monitor a conserved quantity to validate your run",
            font_size=FS_TINY, color=C_BAD,
        )
        pitfall1.next_to(pitfalls_title, DOWN, buff=0.25).align_to(pitfalls_title, LEFT)
        pitfall2.next_to(pitfall1, DOWN, buff=0.2).align_to(pitfall1, LEFT)
        pitfall3.next_to(pitfall2, DOWN, buff=0.2).align_to(pitfall2, LEFT)

        self.play(Write(pitfalls_title))
        self.play(Write(pitfall1))
        self.play(Write(pitfall2))
        self.play(Write(pitfall3))
        self.wait(T_LONG_PAUSE)

        # ===== STEP 4: Final summary =====
        self.play(FadeOut(*self.mobjects))

        # Final summary — key takeaways as a clean list
        summary_title = Text(
            "Key Takeaways",
            font_size=FS_HEADING, color=C_PRIMARY,
        )
        summary_title.to_edge(UP, buff=0.5)
        self.play(Write(summary_title))

        takeaways = [
            ("①", "Hamiltonian systems have special geometric structure", C_NEUTRAL),
            ("②", "Standard integrators ignore this  →  energy drift", C_BAD),
            ("③", "Symplectic integrators preserve symplectic structure", C_GOOD),
            ("④", "Leapfrog/Verlet is the practical workhorse (2nd order)", C_GOOD),
            ("⑤", "Splitting methods give arbitrarily high order", C_PRIMARY),
            ("⑥", "Energy error is bounded, not drifting  ✓", C_GOOD),
        ]

        takeaway_group = VGroup()
        for i, (num, text, col) in enumerate(takeaways):
            # Circled number
            num_obj = Text(num, font_size=FS_BODY, color=col)
            # Text description
            text_obj = Text(text, font_size=FS_SMALL, color=col)
            text_obj.next_to(num_obj, RIGHT, buff=0.3)
            # Group them as a row
            row = VGroup(num_obj, text_obj)
            # Position: stack downward from top, aligned left
            row.move_to(UP * (1.8 - i * 0.65) + LEFT * 0.5)
            row.to_edge(LEFT, buff=1.2)
            takeaway_group.add(row)

        # LaggedStart staggers each row's Write animation
        self.play(
            LaggedStart(
                *[Write(row) for row in takeaway_group],
                lag_ratio=0.3,
                run_time=3.5,
            )
        )
        self.wait(T_LONG_PAUSE)

       
      
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)

        Beyond_the_basics = Text(
            "Beyond the basics:",
            font_size=FS_SMALL, color=C_DIM,
        )
        Beyond_the_basics.to_edge(UP, buff=0.5)
        Beyond_list = [
            ("①", "Dynamics on a Manifold", C_NEUTRAL),
            ("②", "Variational Integration", C_NEUTRAL),
            ("③", "Backward Error Analysis", C_NEUTRAL),
            ("④", "Adaptive Symplecic models", C_NEUTRAL),
            ("⑤", "Methods for Hamiltonian adjacent frameworks(ie. Nambu, "
            "metriplectic, etc)", C_NEUTRAL),
        ]

        Beyond_Group = VGroup()
        for i, (num, text, col) in enumerate(Beyond_list):
            # Circled number
            num_obj = Text(num, font_size=FS_BODY, color=col)
            # Text description
            text_obj = Text(text, font_size=FS_SMALL, color=col)
            text_obj.next_to(num_obj, RIGHT, buff=0.3)
            # Group them as a row
            row = VGroup(num_obj, text_obj)
            # Position: stack downward from top, aligned left
            row.move_to(UP * (1.8 - i * 0.65) + LEFT * 0.5)
            row.to_edge(LEFT, buff=1.2)
            Beyond_Group.add(row)
        
        self.play(Write(Beyond_the_basics))
        
        # LaggedStart staggers each row's Write animation
        self.play(
            LaggedStart(
                *[Write(row) for row in Beyond_Group],
                lag_ratio=0.3,
                run_time=3.5,
            )
        )
        self.wait(T_LONG_PAUSE)

        self.play(FadeOut(*self.mobjects), run_time=T_NORMAL)



# ==============================================================================
# END OF FILE
#
# Render one scene (low quality, fast preview):
#     manim -pql hamiltonian_integration.py OpeningScene
#
# Render all scenes (high quality):
#     manim -pqh -a hamiltonian_integration.py
#
# Scene list:
#   1. OpeningScene
#   2. HamiltonianRefresher
#   3. WhyStandardMethodsFail
#   4. SymplecticCoreIdea
#   5. SymplecticEuler
#   6. LeapfrogMethod
#   7. SplittingMethods
#   8. NBodiesDemo
#   9. PracticalConsiderations
# ==============================================================================
