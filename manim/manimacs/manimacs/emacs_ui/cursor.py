"""``EmacsCursor`` — the cursor's static appearance and instant positioning.

Blinking and animated movement are explicitly Phase 3 ("cursor blinking",
"cursor movement" are listed under the typing/editing *animation* system,
not the UI system). This class only knows how to look like a cursor and
sit still wherever it's put.
"""
from __future__ import annotations

from manim import DOWN, LEFT, Rectangle, VGroup

from ..theme import Theme


class EmacsCursor(VGroup):
    def __init__(self, theme: Theme, *, row_height: float = 0.4, **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self.row_height = row_height
        shape = theme.cursor.shape
        color = theme.colors.ui.cursor
        if shape == "block":
            glyph = Rectangle(width=row_height * 0.55, height=row_height,
                               fill_color=color, fill_opacity=0.85, stroke_width=0)
        elif shape == "underline":
            glyph = Rectangle(width=row_height * 0.55, height=theme.cursor.width,
                               fill_color=color, fill_opacity=1, stroke_width=0)
            glyph.shift(DOWN * row_height * 0.45)
        else:  # "bar"
            glyph = Rectangle(width=theme.cursor.width, height=row_height,
                               fill_color=color, fill_opacity=1, stroke_width=0)
        self.glyph = glyph
        self.add(glyph)

    def place_at(self, point) -> "EmacsCursor":
        """Instantly move the cursor so its left edge sits at *point*."""
        self.move_to(point, aligned_edge=LEFT)
        return self
