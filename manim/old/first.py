from manim import *
import numpy as np

class ManifoldIntroduction(Scene):
    def construct(self):
        # Title
        title = Text("What is a Manifold?", font_size=48)
        self.play(Write(title))
        self.wait()
        self.play(title.animate.scale(0.6).to_edge(UP))
        
        # Definition
        definition = Text(
            "A space that locally looks like Euclidean space,\n"
            "but may have a different global structure",
            font_size=28,
            line_spacing=1.2
        )
        self.play(Write(definition))
        self.wait(2)
        self.play(FadeOut(definition))
        
        # Show dimension concept
        dim_text = Text("Key idea: Dimension", font_size=36)
        self.play(Write(dim_text))
        self.wait()
        self.play(dim_text.animate.to_edge(UP, buff=1.5))
        
        # 1D manifold examples
        line = Line(LEFT * 3, RIGHT * 3, color=BLUE)
        line_label = Text("1D: Line", font_size=24).next_to(line, DOWN)
        
        circle = Circle(radius=1.5, color=BLUE)
        circle_label = Text("1D: Circle", font_size=24).next_to(circle, DOWN)
        
        group_1d = VGroup(line, line_label).shift(UP)
        
        self.play(Create(line), Write(line_label))
        self.wait()
        self.play(
            Transform(line, circle),
            Transform(line_label, circle_label)
        )
        self.wait(2)
        self.play(FadeOut(line), FadeOut(line_label), FadeOut(dim_text))


class LocalFlatness(ThreeDScene):
    def construct(self):
        # Title
        title = Text("Local Flatness", font_size=48)
        title.to_edge(UP)
        self.add_fixed_in_frame_mobjects(title)
        self.play(Write(title))
        self.wait()
        
        # Create a sphere
        sphere = Surface(
            lambda u, v: np.array([
                1.5 * np.cos(u) * np.cos(v),
                1.5 * np.cos(u) * np.sin(v),
                1.5 * np.sin(u)
            ]),
            u_range=[-PI/2, PI/2],
            v_range=[0, TAU],
            resolution=(30, 30),
            fill_opacity=0.8,
            checkerboard_colors=[BLUE_D, BLUE_E]
        )
        
        # Set up 3D view
        self.set_camera_orientation(phi=75 * DEGREES, theta=-45 * DEGREES)
        self.play(Create(sphere))
        self.wait()
        
        # Add a point on the sphere
        point = Dot3D(point=[1.5, 0, 0], color=YELLOW, radius=0.1)
        self.play(FadeIn(point))
        
        # Create a small tangent plane patch
        patch = Surface(
            lambda u, v: np.array([
                1.5 + u * 0.01,
                v * 0.5,
                u * 0.5
            ]),
            u_range=[-1, 1],
            v_range=[-1, 1],
            resolution=(10, 10),
            fill_opacity=0.7,
            fill_color=YELLOW
        )
        
        self.play(Create(patch))
        self.wait()
        
        # Add text explaining local flatness
        explanation = Text(
            "Locally, the sphere looks flat!",
            font_size=28
        ).to_edge(DOWN)
        self.add_fixed_in_frame_mobjects(explanation)
        self.play(Write(explanation))
        
        # Rotate to show different angles
        self.begin_ambient_camera_rotation(rate=0.3)
        self.wait(4)
        self.stop_ambient_camera_rotation()
        self.wait()


class ChartsAndAtlas(Scene):
    def construct(self):
        # Title
        title = Text("Charts and Atlases", font_size=48)
        self.play(Write(title))
        self.wait()
        self.play(title.animate.scale(0.6).to_edge(UP))
        
        # Draw a circle (1D manifold)
        circle = Circle(radius=2, color=BLUE)
        circle_label = Text("Circle (S¹)", font_size=24).next_to(circle, UP)
        
        self.play(Create(circle), Write(circle_label))
        self.wait()
        
        # Highlight different regions on the circle
        colors = [RED, GREEN, YELLOW, PURPLE]
        arcs = []
        
        for i in range(4):
            start_angle = i * PI / 2
            arc = Arc(
                radius=2,
                start_angle=start_angle,
                angle=PI/2 + 0.2,  # Slight overlap
                color=colors[i],
                stroke_width=8
            )
            arcs.append(arc)
        
        self.play(*[Create(arc) for arc in arcs])
        self.wait()
        
        # Show mapping to intervals
        intervals = VGroup()
        for i, color in enumerate(colors):
            interval = Line(LEFT * 0.8, RIGHT * 0.8, color=color, stroke_width=6)
            interval.shift(DOWN * 2 + RIGHT * (i - 1.5) * 1.2)
            intervals.add(interval)
            
            # Add label
            label = MathTex(f"\\phi_{i+1}", font_size=24)
            label.next_to(interval, DOWN, buff=0.2)
            intervals.add(label)
        
        chart_text = Text("Charts (local coordinates)", font_size=24)
        chart_text.next_to(intervals, DOWN, buff=0.5)
        
        self.play(Create(intervals), Write(chart_text))
        self.wait()
        
        # Show arrows from arcs to intervals
        arrows = VGroup()
        for i in range(4):
            arrow = Arrow(
                arcs[i].get_bottom(),
                intervals[2*i].get_top(),
                color=colors[i],
                buff=0.1
            )
            arrows.add(arrow)
        
        self.play(Create(arrows))
        self.wait()
        
        # Atlas definition
        atlas_text = Text(
            "Atlas = Collection of compatible charts\n"
            "covering the entire manifold",
            font_size=24,
            line_spacing=1.2
        )
        atlas_text.to_corner(DR)
        
        self.play(Write(atlas_text))
        self.wait(2)


class SphereManifold(ThreeDScene):
    def construct(self):
        # Title
        title = Text("The Sphere: A 2D Manifold in 3D Space", font_size=40)
        title.to_edge(UP)
        self.add_fixed_in_frame_mobjects(title)
        self.play(Write(title))
        self.wait()
        
        # Create sphere
        sphere = Surface(
            lambda u, v: np.array([
                2 * np.cos(u) * np.cos(v),
                2 * np.cos(u) * np.sin(v),
                2 * np.sin(u)
            ]),
            u_range=[-PI/2, PI/2],
            v_range=[0, TAU],
            resolution=(40, 40),
            fill_opacity=0.8,
            checkerboard_colors=[BLUE_D, BLUE_E]
        )
        
        self.set_camera_orientation(phi=75 * DEGREES, theta=-45 * DEGREES)
        self.play(Create(sphere))
        self.wait()
        
        # Add coordinate lines (latitude and longitude)
        curves = VGroup()
        
        # Latitude lines
        for lat in np.linspace(-PI/2 + 0.3, PI/2 - 0.3, 5):
            curve = ParametricFunction(
                lambda t: np.array([
                    2 * np.cos(lat) * np.cos(t),
                    2 * np.cos(lat) * np.sin(t),
                    2 * np.sin(lat)
                ]),
                t_range=[0, TAU],
                color=YELLOW,
                stroke_width=2
            )
            curves.add(curve)
        
        # Longitude lines
        for lon in np.linspace(0, TAU - 0.5, 8):
            curve = ParametricFunction(
                lambda t: np.array([
                    2 * np.cos(t) * np.cos(lon),
                    2 * np.cos(t) * np.sin(lon),
                    2 * np.sin(t)
                ]),
                t_range=[-PI/2, PI/2],
                color=YELLOW,
                stroke_width=2
            )
            curves.add(curve)
        
        self.play(Create(curves))
        self.wait()
        
        # Rotate sphere
        self.begin_ambient_camera_rotation(rate=0.2)
        self.wait(4)
        self.stop_ambient_camera_rotation()
        
        # Add dimension callout
        dim_text = Text("2-dimensional manifold", font_size=28)
        dim_text.to_corner(DL).shift(UP)
        self.add_fixed_in_frame_mobjects(dim_text)
        self.play(Write(dim_text))
        self.wait()
        
        embedded_text = Text("(embedded in 3D space)", font_size=24)
        embedded_text.next_to(dim_text, DOWN)
        self.add_fixed_in_frame_mobjects(embedded_text)
        self.play(Write(embedded_text))
        self.wait(2)


class ManifoldProperties(Scene):
    def construct(self):
        # Title
        title = Text("Key Properties of Manifolds", font_size=48)
        self.play(Write(title))
        self.wait()
        self.play(title.animate.scale(0.6).to_edge(UP))
        
        # Properties list
        properties = VGroup(
            Text("1. Locally Euclidean", font_size=30),
            Text("   Each point has a neighborhood ≅ ℝⁿ", font_size=24),
            Text("2. Hausdorff", font_size=30),
            Text("   Points can be separated by neighborhoods", font_size=24),
            Text("3. Second-countable", font_size=30),
            Text("   Has a countable basis", font_size=24),
            Text("4. Dimension", font_size=30),
            Text("   Well-defined intrinsic dimension n", font_size=24),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        
        properties.shift(UP * 0.5)
        
        # Color the main points
        for i in [0, 2, 4, 6]:
            properties[i].set_color(YELLOW)
        
        self.play(Write(properties), run_time=4)
        self.wait(2)
        
        self.play(FadeOut(properties))
        
        # Examples
        examples_title = Text("Examples of Manifolds", font_size=36)
        examples_title.to_edge(UP, buff=1)
        self.play(Write(examples_title))
        
        examples = VGroup(
            MathTex(r"\mathbb{R}^n", " - Euclidean space"),
            MathTex(r"S^n", " - n-sphere"),
            MathTex(r"T^n", " - n-torus"),
            MathTex(r"\text{SO}(3)", " - rotation group"),
            MathTex(r"\text{Grassmannian}", " - subspace manifolds"),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.5)
        
        for example in examples:
            example[0].set_color(BLUE)
        
        self.play(Write(examples), run_time=3)
        self.wait(3)


class CompleteSummary(Scene):
    def construct(self):
        # Title
        title = Text("Manifolds: Summary", font_size=48, color=YELLOW)
        self.play(Write(title))
        self.wait()
        self.play(title.animate.scale(0.7).to_edge(UP))
        
        # Create visual summary
        summary_items = VGroup(
            Text("→ Locally flat, globally curved", font_size=28),
            Text("→ Described by charts & atlases", font_size=28),
            Text("→ Intrinsic dimension", font_size=28),
            Text("→ Foundation of differential geometry", font_size=28),
            Text("→ Essential for physics (GR, gauge theory)", font_size=28),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.4)
        
        summary_items.shift(LEFT * 0.5)
        
        for item in summary_items:
            self.play(FadeIn(item, shift=RIGHT * 0.5))
            self.wait(0.5)
        
        self.wait(2)
        
        # Final message
        final = Text(
            "Manifolds let us do calculus on curved spaces!",
            font_size=32,
            color=GREEN
        )
        final.to_edge(DOWN, buff=1)
        
        self.play(Write(final))
        self.wait(3)


# Render all scenes
class FullPresentation(Scene):
    def construct(self):
        scenes = [
            ManifoldIntroduction,
            LocalFlatness,
            ChartsAndAtlas,
            SphereManifold,
            ManifoldProperties,
            CompleteSummary
        ]
        
        for scene_class in scenes:
            scene = scene_class()
            scene.construct()
            self.wait(2)
