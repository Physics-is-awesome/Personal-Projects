"""Phase 2 composition preview: proves the emacs_ui components genuinely
compose together (not just construct in isolation) by building three
realistic mockups entirely from ordinary Manim positioning — no special
"compose with X" glue code, because these are all just VGroups.

Run with e.g.::

    manim -ql examples/phase2_emacs_ui_preview.py Phase2Preview
"""
from __future__ import annotations

from manim import DOWN, LEFT, ORIGIN, RIGHT, UP, FadeIn, FadeOut, Scene, VGroup, config

from manimacs.emacs_ui import (
    CodeBuffer,
    CompileBuffer,
    EmacsCursor,
    EmacsWindow,
    FileTree,
    FileTreeEntry,
    OrgDocument,
    OrgHeading,
    PopupWindow,
    StatusIndicator,
)
from manimacs.theme import DOOM


def _fit(group: VGroup, *, max_width=None, max_height=None) -> VGroup:
    max_width = max_width or (config.frame_width - 1.0)
    max_height = max_height or (config.frame_height - 1.0)
    if group.width > max_width:
        group.scale_to_fit_width(max_width)
    if group.height > max_height:
        group.scale_to_fit_height(max_height)
    group.move_to(ORIGIN)
    return group


class Phase2Preview(Scene):
    theme = DOOM

    def construct(self) -> None:
        theme = self.theme
        self.camera.background_color = theme.colors.surface.base

        stage_a = self._stage_file_tree_and_org_doc(theme)
        self.play(FadeIn(stage_a))
        self.wait(theme.motion.pauses.long)
        self.play(FadeOut(stage_a))

        stage_b = self._stage_code_window_with_chrome(theme)
        self.play(FadeIn(stage_b))
        self.wait(theme.motion.pauses.long)
        self.play(FadeOut(stage_b))

        stage_c = self._stage_split_window(theme)
        self.play(FadeIn(stage_c))
        self.wait(theme.motion.pauses.long)

    # -- Stage A: file tree + EmacsWindow hosting an OrgDocument --------
    def _stage_file_tree_and_org_doc(self, theme) -> VGroup:
        entries = [
            FileTreeEntry("videos", is_dir=True, depth=0),
            FileTreeEntry("friedmann.org", is_dir=False, depth=1),
            FileTreeEntry("scenes.py", is_dir=False, depth=1),
            FileTreeEntry("assets", is_dir=True, depth=0),
        ]
        tree = FileTree(theme, entries, width=3.0, height=5.2)
        tree.select(1)

        headings = [
            OrgHeading(
                text="Friedmann equation", level=1, todo="TODO", tags=("cosmology", "physics"),
                body="Governs the expansion rate of a homogeneous universe.",
                code="H2 = (8*pi*G/3) * rho - k*c**2/a(t)**2",
                children=(
                    OrgHeading(text="Flat universe (k=0)", level=2, todo="DONE",
                               body="Setting curvature to zero simplifies the derivation."),
                    OrgHeading(text="Open questions", level=2, folded=True,
                               body="Dark energy contribution.", tags=("todo",)),
                ),
            )
        ]
        org = OrgDocument(theme, headings)

        window = EmacsWindow(theme, width=8.6, height=5.6, filename="friedmann.org", mode="Org")
        window.set_buffer(org)

        tree.next_to(window, LEFT, buff=theme.spacing.md)
        group = VGroup(tree, window)
        return _fit(group)

    # -- Stage B: code window + status row + compile buffer + popup -----
    def _stage_code_window_with_chrome(self, theme) -> VGroup:
        code = (
            "def hubble(t):\n"
            "    # Friedmann equation, flat universe\n"
            "    H2 = (8*pi*G/3) * rho\n"
            "    return H2**0.5\n"
        )
        buf = CodeBuffer(theme, code, language="python")
        buf.highlight_line(3)
        window = EmacsWindow(theme, width=9.5, height=3.8, filename="friedmann.py", mode="Python")
        window.set_buffer(buf)

        cursor = EmacsCursor(theme, row_height=buf._row_h)
        end_of_line3 = buf.get_line(3).get_right()
        cursor.place_at(end_of_line3 + RIGHT * theme.spacing.xxs)

        status_row = VGroup(
            StatusIndicator(theme, "friedmann.py", status="info"),
            StatusIndicator(theme, "no lint errors", status="success"),
            StatusIndicator(theme, "1 TODO", status="warning"),
        ).arrange(RIGHT, buff=theme.spacing.lg)

        compile_buf = CompileBuffer(theme, width=9.5, height=1.6, title="*compilation*")
        compile_buf.add_line("Compiling friedmann.py...", "default")
        compile_buf.add_line("Rendered HubbleParameterScene", "success")
        compile_buf.add_line("unused import: numpy", "warning")

        popup = PopupWindow(
            theme,
            ["SPC f f  find-file", "SPC f s  save-buffer", "SPC b b  switch-buffer"],
            title="SPC f",
        )

        stack = VGroup(status_row, window, compile_buf).arrange(DOWN, buff=theme.spacing.md)
        popup.next_to(stack, RIGHT, buff=theme.spacing.lg).align_to(stack, UP)
        group = VGroup(stack, popup)
        group.add(cursor)  # cursor rides along with window's absolute position pre-fit
        fitted = _fit(group)
        return fitted

    # -- Stage C: EmacsWindow split into two panes -----------------------
    def _stage_split_window(self, theme) -> VGroup:
        window = EmacsWindow(theme, width=11.5, height=6.0, filename="", show_title_bar=False, mode="")
        pane_a, pane_b = window.split(direction="horizontal")

        buf_a = CodeBuffer(theme, "def f(x):\n    return x**2\n", language="python", width=pane_a.width - 2 * theme.panel.padding)
        buf_b = CodeBuffer(theme, "class Particle:\n    def __init__(self, m):\n        self.m = m\n",
                            language="python", width=pane_b.width - 2 * theme.panel.padding)
        pane_a.set_buffer(buf_a)
        pane_b.set_buffer(buf_b)
        pane_a.modeline.set_buffer_name("physics.py")
        pane_a.modeline.set_mode("Python")
        pane_b.modeline.set_buffer_name("particle.py")
        pane_b.modeline.set_mode("Python")

        return _fit(VGroup(window))
