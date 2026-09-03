"""``CompileBuffer`` — a build/command-output log (``*compilation*``-style),
with per-line status coloring (default/success/warning/error)."""
from __future__ import annotations

from manim import DOWN, LEFT, RIGHT, UP, Rectangle, Text, VGroup

from ..theme import Theme, resolve_font
from ._layout import reference_row_height, stack_fixed_rows

_STATUS_ROLE = {"success": "success", "warning": "warning", "error": "error"}


class CompileBuffer(VGroup):
    def __init__(self, theme: Theme, *, width: float, height: float, title: str = "*compilation*", **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self._code_font = resolve_font(theme.typography.code_font)
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._width = width
        self._height = height
        self._lines: list[tuple[str, str]] = []  # (text, status)

        self.panel = Rectangle(width=width, height=height,
                                fill_color=theme.colors.surface.sunken, fill_opacity=1,
                                stroke_color=theme.colors.ui.border_default, stroke_width=theme.borders.thin)
        title_bar_h = theme.window.title_bar_height * 0.7
        self.title_bar = Rectangle(width=width, height=title_bar_h,
                                    fill_color=theme.colors.surface.elevated, fill_opacity=1, stroke_width=0)
        self.title_bar.align_to(self.panel, UP)
        self.title_text = Text(title, font=self._ui_font, font_size=theme.typography.sizes.code_small,
                                color=theme.colors.foreground.muted)
        self.title_text.move_to(self.title_bar)
        self.add(self.panel, self.title_bar, self.title_text)

        self._row_h = reference_row_height(theme, self._code_font, theme.typography.sizes.code_small)
        self._rows_group = VGroup()
        self.add(self._rows_group)
        self._max_visible = max(1, int((height - title_bar_h - 2 * theme.spacing.xs) // (self._row_h + theme.spacing.xxs)))

    def add_line(self, text: str, status: str = "default") -> "CompileBuffer":
        self._lines.append((text, status))
        self._relayout()
        return self

    def clear(self) -> "CompileBuffer":
        self._lines.clear()
        self._relayout()
        return self

    def _relayout(self) -> None:
        theme = self.theme
        self.remove(self._rows_group)
        self._rows_group = VGroup()
        visible = self._lines[-self._max_visible:]
        mobjects = []
        for text, status in visible:
            color = (
                getattr(theme.colors.status, _STATUS_ROLE[status])
                if status in _STATUS_ROLE
                else theme.colors.foreground.secondary
            )
            mobjects.append(Text(text, font=self._code_font, font_size=theme.typography.sizes.code_small, color=color))
        top_left = self.title_bar.get_corner(DOWN + LEFT) + DOWN * theme.spacing.xs + RIGHT * theme.spacing.xs
        stack_fixed_rows(mobjects, row_pitch=self._row_h + theme.spacing.xxs, start=top_left, aligned_edge=LEFT)
        self._rows_group.add(*mobjects)
        self.add(self._rows_group)
