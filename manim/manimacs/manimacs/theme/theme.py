"""The top-level ``Theme`` object: one immutable bundle of every visual
property a manimacs scene might need, composed from the token modules.

A ``Theme`` is never mutated in place — it's frozen. To make a variant,
call ``.derive(...)`` (or ``dataclasses.replace`` for a shallow, top-level
change) to get a new ``Theme`` back. This is what makes "changing the
theme" safe: nothing can accidentally share and corrupt state between two
themes that both started from ``DOOM``.
"""
from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Literal, Optional

from .colors import ColorPalette
from .components import CodeStyle, EquationStyle, PanelStyle, PlotStyle, TitleStyle
from .geometry import Borders, CursorStyle, Opacity, Radii, Shadow, Spacing, WindowGeometry
from .motion import Motion
from .overrides import deep_replace
from .typography import Typography

ThemeMode = Literal["dark", "light"]


@dataclass(frozen=True, slots=True)
class Theme:
    name: str
    mode: ThemeMode
    colors: ColorPalette
    typography: Typography
    spacing: Spacing
    radii: Radii
    borders: Borders
    shadow: Shadow
    opacity: Opacity
    window: WindowGeometry
    motion: Motion
    cursor: CursorStyle
    code: CodeStyle
    equation: EquationStyle
    plot: PlotStyle
    title: TitleStyle
    panel: PanelStyle

    def derive(self, overrides: dict, *, name: Optional[str] = None) -> "Theme":
        """Return a new ``Theme`` with a deep-merged set of overrides applied.

        ``overrides`` may nest plain dicts to reach into sub-objects, e.g.::

            MY_THEME = DOOM.derive({
                "colors": {"syntax": {"keyword": "#ff8800"}},
                "motion": {"typing_chars_per_second": 30},
            }, name="my-doom-variant")

        Every leaf value is still validated (hex colors, cursor shape,
        etc.) because it flows back through the normal dataclass
        constructors. The original ``Theme`` is never modified.
        """
        derived = deep_replace(self, overrides)
        if name is not None:
            derived = replace(derived, name=name)
        return derived
