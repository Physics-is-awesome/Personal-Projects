"""Phase 1 style-guide preview: renders every semantic color/type role for
a theme, plus a mocked-up window chrome, using nothing but theme tokens.

Run it with, e.g.::

    manim -ql examples/phase1_theme_preview.py DoomThemePreview
    manim -ql examples/phase1_theme_preview.py PaperThemePreview
    manim -ql examples/phase1_theme_preview.py CosmologyThemePreview
    manim -ql examples/phase1_theme_preview.py PresentationThemePreview

The four Scene subclasses at the bottom differ in exactly one line each
(which theme they set) — everything else, including all layout and
animation code, is identical. That's the whole point of Phase 1: swapping
DOOM for PAPER changes the entire visual identity without touching a
single line of scene logic.

Deliberately uses only ``Text`` (Pango), not ``MathTex``/``Tex`` — real
LaTeX-typeset equations are Phase 4's job. This scene only proves the
theme/design-system layer.
"""
from __future__ import annotations

from dataclasses import fields

from manim import (
    DOWN,
    LEFT,
    ORIGIN,
    RIGHT,
    UP,
    FadeIn,
    GrowFromCenter,
    LaggedStartMap,
    Line,
    Rectangle,
    RoundedRectangle,
    Scene,
    Text,
    VGroup,
    config,
)

from manimacs.theme import COSMOLOGY, DOOM, PAPER, PRESENTATION, Theme, resolve_font


class ThemePreviewScene(Scene):
    """Set the ``theme`` class attribute in a subclass to preview it."""

    theme: Theme = DOOM

    def construct(self) -> None:
        theme = self.theme
        self.camera.background_color = theme.colors.surface.primary
        # Resolve each font stack to whatever's actually installed on this
        # machine — this is exactly what resolve_font() is for, and it
        # quietly avoids Pango's noisy "font not found" fallback warnings.
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._code_font = resolve_font(theme.typography.code_font)

        # Build every piece and lay the whole composition out top-to-bottom
        # *before* animating anything.
        title = self._build_title(theme)
        syntax_row = self._swatch_row(theme.colors.syntax, theme)
        math_row = self._swatch_row(theme.colors.math, theme)
        rows = VGroup(syntax_row, math_row).arrange(DOWN, buff=theme.spacing.lg)
        window = self._fake_window(theme)

        rows.next_to(title, DOWN, buff=theme.spacing.xl)
        window.next_to(rows, DOWN, buff=theme.spacing.xl)

        # However many roles a palette ends up with, the whole composition
        # must still fit in frame — so scale everything down together
        # (preserving relative proportions) rather than letting any one
        # piece clip off-screen.
        composition = VGroup(title, rows, window)
        max_height = config.frame_height - 2 * theme.spacing.md
        if composition.height > max_height:
            composition.scale_to_fit_height(max_height)
        composition.move_to(ORIGIN)

        self.play(FadeIn(title[0], shift=DOWN * 0.2), GrowFromCenter(title[1]))
        self.play(LaggedStartMap(FadeIn, syntax_row, shift=UP * 0.1, lag_ratio=0.05))
        self.wait(theme.motion.pauses.short)
        self.play(LaggedStartMap(FadeIn, math_row, shift=UP * 0.1, lag_ratio=0.05))
        self.wait(theme.motion.pauses.medium)
        self.play(FadeIn(window, scale=0.97))
        self.wait(theme.motion.pauses.long)

    def _build_title(self, theme: Theme) -> VGroup:
        title = Text(
            f"{theme.name} theme",
            font=self._ui_font,
            font_size=theme.typography.sizes.title,
            color=theme.colors.foreground.primary,
            weight=theme.typography.weights.bold,
        )
        title.to_edge(UP, buff=theme.spacing.lg)
        bar = Line(LEFT, RIGHT, color=theme.colors.accent.primary, stroke_width=theme.borders.thick)
        bar.width = title.width * 0.6
        bar.next_to(title, DOWN, buff=theme.spacing.xs)
        return VGroup(title, bar)

    def _swatch_row(self, palette, theme: Theme) -> VGroup:
        swatches = VGroup()
        for field in fields(palette):
            value = getattr(palette, field.name)
            color = value[0] if isinstance(value, tuple) else value  # PlotColors.series is a tuple
            chip = RoundedRectangle(
                width=0.32, height=0.32, corner_radius=theme.radii.sm,
                fill_color=color, fill_opacity=1,
                stroke_width=theme.borders.thin, stroke_color=theme.colors.ui.border_default,
            )
            label = Text(
                field.name, font=self._ui_font,
                font_size=theme.typography.sizes.modeline,
                color=theme.colors.foreground.secondary,
            )
            label.next_to(chip, DOWN, buff=theme.spacing.xxs)
            swatches.add(VGroup(chip, label))
        # Wrap into a grid rather than one long row, since a palette can
        # have more roles than fit across the frame at a legible size.
        max_width = config.frame_width - 2 * theme.spacing.xl
        cols = max(1, min(len(swatches), 9))
        swatches.arrange_in_grid(cols=cols, buff=(theme.spacing.sm, theme.spacing.md))
        if swatches.width > max_width:
            swatches.scale_to_fit_width(max_width)
        return swatches

    def _fake_window(self, theme: Theme) -> VGroup:
        w, h = 9.5, 2.3
        body = RoundedRectangle(
            width=w, height=h, corner_radius=theme.window.corner_radius,
            fill_color=theme.colors.surface.elevated, fill_opacity=1,
            stroke_color=theme.colors.ui.border_default, stroke_width=theme.borders.default,
        )
        title_bar = Rectangle(
            width=w - theme.borders.default * 0.02, height=theme.window.title_bar_height,
            fill_color=theme.colors.surface.sunken, fill_opacity=1, stroke_width=0,
        )
        title_bar.align_to(body, UP).shift(DOWN * theme.borders.default * 0.01)
        title_text = Text(
            "friedmann.org", font=self._code_font,
            font_size=theme.typography.sizes.code_small, color=theme.colors.foreground.muted,
        )
        title_text.move_to(title_bar)

        modeline = Rectangle(
            width=w - theme.borders.default * 0.02, height=theme.window.modeline_height,
            fill_color=theme.colors.ui.modeline_active_bg, fill_opacity=1, stroke_width=0,
        )
        modeline.align_to(body, DOWN).shift(UP * theme.borders.default * 0.01)
        mode_text = Text(
            "org   friedmann.org   12,4   (Org)",
            font=self._ui_font,
            font_size=theme.typography.sizes.modeline,
            color=theme.colors.ui.modeline_active_fg,
        )
        mode_text.move_to(modeline).align_to(modeline, LEFT).shift(RIGHT * theme.spacing.sm)

        code_line = Text(
            "H(t)^2 = (8*pi*G/3) * rho - k*c^2/a(t)^2",
            font=self._code_font,
            font_size=theme.typography.sizes.code,
            color=theme.colors.syntax.function,
        )
        code_line.move_to(body).shift(UP * theme.spacing.xs)

        return VGroup(body, title_bar, title_text, modeline, mode_text, code_line)


class DoomThemePreview(ThemePreviewScene):
    theme = DOOM


class PaperThemePreview(ThemePreviewScene):
    theme = PAPER


class CosmologyThemePreview(ThemePreviewScene):
    theme = COSMOLOGY


class PresentationThemePreview(ThemePreviewScene):
    theme = PRESENTATION
