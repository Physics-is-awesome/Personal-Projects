"""``PopupWindow`` — a small floating overlay: which-key-style keybinding
hints, completion lists, tooltips."""
from __future__ import annotations

from manim import DOWN, LEFT, UP, Text, VGroup

from ..theme import Theme, resolve_font
from ._layout import reference_row_height, stack_fixed_rows, themed_panel


class PopupWindow(VGroup):
    def __init__(self, theme: Theme, items: list[str], *, title: str | None = None, width: float | None = None, **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self._ui_font = resolve_font(theme.typography.ui_font)

        row_h = reference_row_height(theme, self._ui_font, theme.typography.sizes.label)
        pad = theme.panel.padding
        rows = [Text(item, font=self._ui_font, font_size=theme.typography.sizes.label,
                      color=theme.colors.foreground.primary) for item in items]
        title_mob = None
        if title:
            title_mob = Text(title, font=self._ui_font, font_size=theme.typography.sizes.label,
                              color=theme.colors.accent.primary, weight=theme.typography.weights.bold)

        content_width = width or max(
            [m.width for m in rows] + ([title_mob.width] if title_mob else [0.0]) + [1.0]
        ) + 2 * pad
        n_rows = len(rows) + (1 if title_mob else 0)
        content_height = n_rows * row_h + (n_rows + 1) * theme.spacing.xxs + 2 * pad

        self.panel = themed_panel(
            theme, content_width, content_height,
            fill=theme.colors.surface.overlay,
            corner_radius=theme.panel.corner_radius,
            border_width=theme.panel.border_width,
        )
        self.add(self.panel)

        top_left = self.panel.get_corner(UP + LEFT) + (pad * (DOWN + LEFT))
        all_rows = ([title_mob] if title_mob else []) + rows
        stack_fixed_rows(all_rows, row_pitch=row_h + theme.spacing.xxs, start=top_left, aligned_edge=LEFT)
        for m in all_rows:
            self.add(m)
        self.items = rows
        self.title_mobject = title_mob
