"""manimacs.theme — the data-driven design-system layer.

Public API::

    from manimacs.theme import DOOM, PAPER, COSMOLOGY, PRESENTATION
    from manimacs.theme import Theme, get_current_theme, use_theme

Everything under this package except ``manim_adapters.py`` is pure data —
importable and testable without Manim installed. See the package
docstrings in ``colors.py``, ``theme.py``, and ``manim_adapters.py`` for
the reasoning behind that split.
"""
from __future__ import annotations

from .color_utils import blend, darken, hex_to_rgb, lighten, relative_luminance, rgb_to_hex, validate_hex
from .colors import (
    AccentColors,
    ColorPalette,
    ForegroundColors,
    MathColors,
    PlotColors,
    StatusColors,
    SurfaceColors,
    SyntaxColors,
    UIColors,
)
from .components import CodeStyle, EquationStyle, PanelStyle, PlotStyle, TitleStyle
from .geometry import (
    Borders,
    CursorShape,
    CursorStyle,
    Opacity,
    Radii,
    Shadow,
    Spacing,
    WindowGeometry,
)
from .manim_adapters import resolve_font, resolve_rate_func, to_manim_color
from .motion import Motion, PauseDurations, RateFuncs, TransitionDurations
from .overrides import deep_replace
from .presets import COSMOLOGY, DOOM, PAPER, PRESENTATION
from .registry import get_current_theme, get_theme, list_themes, register_theme, set_current_theme, use_theme
from .theme import Theme, ThemeMode
from .typography import FontSizes, FontStack, FontWeights, Typography

for _preset in (DOOM, PAPER, COSMOLOGY, PRESENTATION):
    register_theme(_preset)
del _preset

__all__ = [
    # presets
    "DOOM", "PAPER", "COSMOLOGY", "PRESENTATION",
    # core types
    "Theme", "ThemeMode",
    "ColorPalette", "SurfaceColors", "ForegroundColors", "AccentColors",
    "SyntaxColors", "MathColors", "StatusColors", "UIColors", "PlotColors",
    "Typography", "FontStack", "FontSizes", "FontWeights",
    "Spacing", "Radii", "Borders", "Shadow", "Opacity", "WindowGeometry",
    "CursorStyle", "CursorShape",
    "Motion", "PauseDurations", "TransitionDurations", "RateFuncs",
    "CodeStyle", "EquationStyle", "PlotStyle", "TitleStyle", "PanelStyle",
    # registry / current-theme
    "register_theme", "get_theme", "list_themes",
    "get_current_theme", "set_current_theme", "use_theme",
    # overrides
    "deep_replace",
    # color utils
    "validate_hex", "hex_to_rgb", "rgb_to_hex", "blend", "lighten", "darken", "relative_luminance",
    # manim adapters
    "to_manim_color", "resolve_rate_func", "resolve_font",
]
