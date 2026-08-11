"""
themes/base.py

Core theme definitions for the Physics Animation Framework.

Every theme (Doom, Paper, Spacetime, Light, etc.) should inherit
from these dataclasses.

Author: Alex Cason
"""

from __future__ import annotations

from dataclasses import dataclass, field, replace
from typing import Dict, Optional


# ============================================================
# Color Groups
# ============================================================

@dataclass(frozen=True)
class SyntaxColors:
    """Syntax highlighting colors."""

    keyword: str
    function: str
    string: str
    number: str
    comment: str
    constant: str
    operator: str
    type_name: str
    variable: str


@dataclass(frozen=True)
class MathColors:
    """Semantic colors for mathematics."""

    equation: str
    operator: str
    variable: str
    tensor: str
    vector: str
    scalar: str
    function: str
    constant: str
    highlight: str
    warning: str
    success: str


@dataclass(frozen=True)
class UIColors:
    """Editor UI colors."""

    background: str
    foreground: str

    border: str
    shadow: str

    cursor: str

    selection: str
    line_highlight: str

    modeline_bg: str
    modeline_fg: str

    minibuffer_bg: str
    minibuffer_fg: str

    popup_bg: str
    popup_fg: str


@dataclass(frozen=True)
class AccentColors:
    """Reusable accent palette."""

    blue: str
    cyan: str
    green: str
    yellow: str
    orange: str
    red: str
    magenta: str
    violet: str

    gray: str
    white: str


# ============================================================
# Typography
# ============================================================

@dataclass(frozen=True)
class Fonts:

    code: str
    text: str
    title: str
    math: str


@dataclass(frozen=True)
class FontSizes:

    title: int = 54
    subtitle: int = 40
    section: int = 34

    body: int = 30
    caption: int = 24

    equation: int = 40

    code: int = 28

    tiny: int = 18


# ============================================================
# Layout
# ============================================================

@dataclass(frozen=True)
class Layout:

    page_margin: float = 0.50

    panel_padding: float = 0.35

    window_radius: float = 0.18

    border_width: float = 2.0

    shadow_offset: float = 0.08

    line_spacing: float = 0.42

    paragraph_spacing: float = 0.70

    object_spacing: float = 0.40

    cursor_width: float = 0.05


# ============================================================
# Animation Timing
# ============================================================

@dataclass(frozen=True)
class Timing:

    instant: float = 0.0

    very_fast: float = 0.15

    fast: float = 0.30

    normal: float = 0.50

    slow: float = 0.90

    very_slow: float = 1.50

    typing_delay: float = 0.035

    cursor_blink: float = 0.60

    scroll_duration: float = 0.75

    zoom_duration: float = 1.25

    camera_pan: float = 1.20

    fade_duration: float = 0.45


# ============================================================
# Shadows
# ============================================================

@dataclass(frozen=True)
class Shadow:

    enabled: bool = True

    opacity: float = 0.25

    blur_radius: float = 8.0

    offset_x: float = 0.08

    offset_y: float = -0.08


# ============================================================
# Window Decorations
# ============================================================

@dataclass(frozen=True)
class WindowStyle:

    rounded: bool = True

    title_bar: bool = True

    modeline: bool = True

    shadow: bool = True

    transparency: float = 1.0

    border_radius: float = 0.18


# ============================================================
# Theme
# ============================================================

@dataclass(frozen=True)
class Theme:
    """
    Complete visual description of a theme.

    All components should depend ONLY on Theme.
    """

    name: str

    syntax: SyntaxColors

    math: MathColors

    ui: UIColors

    accents: AccentColors

    fonts: Fonts

    sizes: FontSizes = field(default_factory=FontSizes)

    layout: Layout = field(default_factory=Layout)

    timing: Timing = field(default_factory=Timing)

    shadow: Shadow = field(default_factory=Shadow)

    windows: WindowStyle = field(default_factory=WindowStyle)

    metadata: Dict[str, str] = field(default_factory=dict)

    # --------------------------------------------------------

    def copy(self) -> "Theme":
        return replace(self)

    # --------------------------------------------------------

    def with_overrides(self, **kwargs) -> "Theme":
        """
        Create a modified copy.

        Example

        theme = DOOM.with_overrides(
            name="Large Text",
            sizes=FontSizes(code=36)
        )
        """
        return replace(self, **kwargs)

    # --------------------------------------------------------

    def validate(self) -> None:
        """
        Basic validation.
        """

        if self.sizes.body <= 0:
            raise ValueError("Body font size must be positive.")

        if self.sizes.code <= 0:
            raise ValueError("Code font size must be positive.")

        if self.layout.window_radius < 0:
            raise ValueError("Negative window radius.")

        if self.layout.border_width < 0:
            raise ValueError("Negative border width.")

    # --------------------------------------------------------

    @property
    def background(self) -> str:
        return self.ui.background

    @property
    def foreground(self) -> str:
        return self.ui.foreground

    @property
    def cursor(self) -> str:
        return self.ui.cursor


# ============================================================
# Utility Functions
# ============================================================

def merge_metadata(
    base: Optional[Dict[str, str]],
    extra: Optional[Dict[str, str]],
) -> Dict[str, str]:
    """
    Merge metadata dictionaries.
    """

    merged = {}

    if base:
        merged.update(base)

    if extra:
        merged.update(extra)

    return merged
