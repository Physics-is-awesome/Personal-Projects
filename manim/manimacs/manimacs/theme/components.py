"""Component-level style bundles.

These hold the non-color styling parameters specific to a family of
components (code buffers, equations, plots, titles, panels). Colors for
these components are deliberately *not* duplicated here — they're pulled
from ``ColorPalette`` (``theme.colors.math``, ``theme.colors.plot``, etc.)
so every color in the framework has exactly one source of truth. This
mirrors a standard three-tier design-token architecture: global tokens
(``Spacing``, ``Radii``, ...) feed semantic/alias tokens (``ColorPalette``
roles) feed these component tokens.
"""
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class CodeStyle:
    tab_width: int
    show_line_numbers: bool
    show_indent_guides: bool
    active_line_highlight: bool
    gutter_width: float          # scene units reserved for line numbers
    line_spacing: float           # vertical gap between lines of code


@dataclass(frozen=True, slots=True)
class EquationStyle:
    """Non-color sizing/geometry for equations and derivations (Phase 4).
    Highlight-box and arrow *colors* come from ``theme.colors.math``."""
    highlight_box_corner_radius: float
    highlight_box_stroke_width: float
    highlight_box_fill_opacity: float
    term_spacing: float            # horizontal gap between aligned terms
    row_spacing: float              # vertical gap between chained equations
    arrow_stroke_width: float


@dataclass(frozen=True, slots=True)
class PlotStyle:
    """Non-color sizing for plots (Phase 6). Colors come from
    ``theme.colors.plot``."""
    axis_stroke_width: float
    gridline_stroke_width: float
    gridline_opacity: float
    line_width: float
    point_radius: float
    error_band_opacity: float
    error_bar_cap_size: float


@dataclass(frozen=True, slots=True)
class TitleStyle:
    """Non-color sizing for scene titles. Colors come from
    ``theme.colors.foreground`` / ``theme.colors.accent``."""
    show_accent_bar: bool
    accent_bar_height: float
    accent_bar_buffer: float    # gap between title text and its accent bar


@dataclass(frozen=True, slots=True)
class PanelStyle:
    """Non-color sizing for secondary panels/popups (distinct from the
    main EmacsWindow, which uses ``WindowGeometry``). Colors come from
    ``theme.colors.surface`` / ``theme.colors.ui``."""
    padding: float
    corner_radius: float
    border_width: float
    show_shadow: bool
