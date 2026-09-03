"""Shared, low-level helpers used by every emacs_ui component.

Kept as plain functions rather than a base class — composition over
inheritance: a ``CodeBuffer`` and a ``PopupWindow`` both need a themed
panel rectangle, but they have nothing else in common, so a shared
function is a better fit than forcing them into one inheritance tree.
"""
from __future__ import annotations

from manim import DOWN, LEFT, UP, MarkupText, RoundedRectangle, Text, VGroup

from ..theme import Theme


def themed_panel(
    theme: Theme,
    width: float,
    height: float,
    *,
    fill: str | None = None,
    corner_radius: float | None = None,
    border_width: float | None = None,
) -> RoundedRectangle:
    """A themed rounded-rectangle backdrop shared by windows, popups, the
    file tree, compile buffers, and the minibuffer."""
    return RoundedRectangle(
        width=width,
        height=height,
        corner_radius=theme.panel.corner_radius if corner_radius is None else corner_radius,
        fill_color=theme.colors.surface.elevated if fill is None else fill,
        fill_opacity=1,
        stroke_color=theme.colors.ui.border_default,
        stroke_width=theme.panel.border_width if border_width is None else border_width,
    )


def reference_row_height(theme: Theme, font: str, font_size: float) -> float:
    """Height of one fixed text row at *font_size*, for consistent
    line-to-line spacing regardless of which glyphs a particular line
    happens to contain (ascenders/descenders otherwise make naive
    ``arrange(DOWN)`` line spacing visibly uneven for code)."""
    probe = Text("Ag", font=font, font_size=font_size)
    return probe.height


def stack_fixed_rows(
    mobjects: list,
    *,
    row_pitch: float,
    start: "object",
    aligned_edge=LEFT,
) -> VGroup:
    """Position *mobjects* as top-to-bottom rows of fixed height
    *row_pitch*, each aligned to *aligned_edge* at *start*'s x (or y, for
    non-LEFT/RIGHT edges), rather than each mobject's own bounding box.
    """
    group = VGroup()
    for i, mobj in enumerate(mobjects):
        mobj.move_to(start + i * row_pitch * DOWN, aligned_edge=aligned_edge)
        group.add(mobj)
    return group


def markup_line_or_blank(markup: str, *, font: str, font_size: float, fallback_color: str) -> "Text | MarkupText":
    """``MarkupText`` for one code line. A blank line gets an invisible
    (``fill_opacity=0``) plain ``Text`` placeholder instead — a zero-width
    space collapses to a zero-size bounding box in Manim/Pango, which
    breaks ``aligned_edge=LEFT`` positioning for that row; plain invisible
    glyphs keep real geometry so every row still lines up.
    """
    if not markup:
        return Text("Ag", font=font, font_size=font_size, fill_opacity=0)
    return MarkupText(markup, font=font, font_size=font_size)
