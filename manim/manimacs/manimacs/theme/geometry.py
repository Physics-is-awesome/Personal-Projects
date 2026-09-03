"""Geometry tokens: spacing, radii, borders, shadows, window geometry, opacity.

Two different unit spaces show up here and it matters which is which:

- ``Spacing``, ``Radii``, window/panel sizes, and offsets are in **Manim
  scene units** (the same units as ``width=``/``height=``/``.shift()`` —
  roughly 14.2 x 8.0 units across a default 16:9 frame).
- ``Borders`` (and any ``stroke_width``-like value) are in **Manim's
  stroke-width unit**, which behaves like SVG stroke-width / points: it
  does *not* scale with scene coordinates, so a ``stroke_width=2`` line
  looks like a 2-unit-wide line regardless of camera framing.

Mixing these up is a classic Manim footgun, so it's called out here once
rather than left for every component to rediscover.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

CursorShape = Literal["bar", "block", "underline"]


@dataclass(frozen=True, slots=True)
class Spacing:
    """Scene-unit spacing scale for margins, padding, and gaps."""
    xxs: float
    xs: float
    sm: float
    md: float
    lg: float
    xl: float
    xxl: float


@dataclass(frozen=True, slots=True)
class Radii:
    """Scene-unit corner-radius scale for rounded rectangles/panels."""
    none: float
    sm: float
    md: float
    lg: float
    xl: float


@dataclass(frozen=True, slots=True)
class Borders:
    """Stroke-width scale (Manim stroke-width units, not scene units)."""
    thin: float
    default: float
    thick: float
    heavy: float


@dataclass(frozen=True, slots=True)
class Shadow:
    """Drop-shadow parameters.

    Manim's cairo renderer has no native blur filter, so a "shadow" is
    approximated by Phase 2 components as a small stack of offset,
    increasingly-transparent copies behind the shadowed mobject. ``layers``
    is how many copies to stack; ``blur`` is a conceptual softness value
    those copies should spread across.
    """
    opacity: float
    offset_x: float
    offset_y: float
    blur: float
    layers: int


@dataclass(frozen=True, slots=True)
class WindowGeometry:
    """Default sizing for a top-level EmacsWindow (Phase 2)."""
    default_width: float
    default_height: float
    title_bar_height: float
    modeline_height: float
    padding: float
    corner_radius: float


@dataclass(frozen=True, slots=True)
class Opacity:
    """Semantic opacity scale (0..1) for fades, dimming, and overlays."""
    disabled: float
    dim: float      # de-emphasized / "unchanged" objects
    subtle: float
    hover: float
    scrim: float    # modal/backdrop dimming behind a popup
    full: float


@dataclass(frozen=True, slots=True)
class CursorStyle:
    """Behavioral/geometric cursor properties. Color lives in
    ``ColorPalette.ui.cursor`` — kept there rather than duplicated here so
    there's a single source of truth for every color in the theme."""
    shape: CursorShape
    width: float          # only meaningful for "bar" shape (scene units)
    blink: bool
    blink_period: float    # seconds per full on/off cycle

    def __post_init__(self) -> None:
        valid = {"bar", "block", "underline"}
        if self.shape not in valid:
            raise ValueError(f"CursorStyle.shape must be one of {valid}, got {self.shape!r}")
