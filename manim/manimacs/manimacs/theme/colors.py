"""Semantic color roles for the manimacs theme system.

Every color used anywhere in a manimacs scene should come from one of the
role-based palettes defined here rather than being hard-coded inline in a
scene. Colors are stored as plain ``"#rrggbb"`` hex strings so that this
module has **zero dependency on Manim** (or any other rendering library):
it can be imported, inspected, unit-tested, diffed, or hand-edited without
Manim installed at all. The only place Manim types are constructed is
``manim_adapters.py``, which is the sole bridge between this pure data
layer and the rendering engine.

Opacity/transparency is handled separately (see
``manimacs.theme.geometry.Opacity``) because Manim treats fill/stroke
opacity as parameters distinct from color, so baking alpha into a hex
string here would just have to be unpacked again later.

Color roles are grouped by *what kind of thing they color*, not by which
component draws them, so the same role means the same thing everywhere:
a "keyword" is always the same color whether it appears in an EmacsWindow
code buffer, a popup, or a scratch buffer.
"""
from __future__ import annotations

import re
from dataclasses import dataclass, fields

_HEX_RE = re.compile(r"^#[0-9a-fA-F]{6}$")


def _validate_hex_fields(instance: object) -> None:
    """Raise ValueError if any string field on *instance* isn't ``#rrggbb``.

    Called from every color dataclass's ``__post_init__`` below so that a
    typo'd color (e.g. ``"#bbc2cff"`` or ``"cyan"``) fails immediately at
    theme-construction time with a message that names the exact field,
    rather than surfacing later as a cryptic error deep inside Manim.
    """
    for f in fields(instance):
        value = getattr(instance, f.name)
        if isinstance(value, str) and not _HEX_RE.match(value):
            raise ValueError(
                f"{type(instance).__name__}.{f.name} = {value!r} is not a "
                "valid '#rrggbb' hex color."
            )


@dataclass(frozen=True, slots=True)
class SurfaceColors:
    """Background / elevation roles, darkest ``base`` to topmost ``overlay``.

    Modeled as elevation layers (as in Material Design or Doom Emacs's use
    of progressively lighter/darker panels) rather than a single
    "background" color, so panels, popups, and the main buffer can all be
    visually distinct without any component inventing its own shade.
    """
    base: str               # outermost canvas, behind everything
    primary: str            # main buffer / editor content area
    elevated: str           # panels, sidebars, modeline, file tree
    overlay: str            # popups, tooltips, which-key-style overlays
    sunken: str             # recessed chrome: title bars, minibuffer
    selection: str          # selected-text background
    line_highlight: str     # current-line background (subtle)

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class ForegroundColors:
    """Text-color roles, from full-emphasis to barely-there."""
    primary: str    # default body/code text
    secondary: str  # slightly de-emphasized text
    muted: str      # comments, disabled text
    faint: str      # line numbers, watermark-level text
    inverse: str    # text drawn on top of a bright accent fill

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class AccentColors:
    """The theme's brand colors — used sparingly, for emphasis and identity."""
    primary: str    # cyan/blue in DOOM; the theme's signature color
    secondary: str  # purple/magenta in DOOM; secondary emphasis
    tertiary: str   # used rarely, for a third level of distinction

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class SyntaxColors:
    """Roles for source-code syntax highlighting (EmacsWindow / buffers)."""
    keyword: str
    string: str
    comment: str
    function: str
    type: str
    number: str
    constant: str
    operator: str
    variable: str
    parameter: str
    builtin: str
    decorator: str
    punctuation: str
    tag: str
    error: str
    docstring: str

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class MathColors:
    """Roles for equation and derivation semantic highlighting.

    These are the colors that give a quantity a *consistent visual
    meaning* across an entire project: once "tensor" is magenta, it stays
    magenta whether it appears in this video's derivation or next year's.
    """
    scalar: str
    vector: str
    tensor: str
    matrix: str
    constant: str        # physical constants: c, G, hbar, ...
    variable: str
    index: str            # mu, nu, i, j, ...
    operator: str          # nabla, partial, ...
    differential: str      # d, dt, dx, ...
    integral: str
    function: str
    highlight: str        # the term currently being emphasized
    substitution: str      # a term being substituted in this step
    new_term: str          # a term newly introduced in this step
    unchanged: str         # de-emphasized / carried-over term
    warning: str
    error: str
    annotation: str        # side notes, labels, callouts

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class StatusColors:
    """Generic status/feedback roles, shared across UI components."""
    success: str
    warning: str
    error: str
    info: str

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class UIColors:
    """Editor/window chrome: borders, cursor, gutters, modeline, popups..."""
    border_default: str
    border_subtle: str
    border_focus: str
    cursor: str
    line_number: str
    line_number_current: str
    modeline_active_bg: str
    modeline_active_fg: str
    modeline_inactive_bg: str
    modeline_inactive_fg: str
    minibuffer_bg: str
    minibuffer_fg: str
    popup_bg: str
    popup_border: str
    file_tree_bg: str
    file_tree_fg: str
    file_tree_selected_bg: str
    file_tree_selected_fg: str
    file_tree_icon_dir: str
    file_tree_icon_file: str
    shadow: str

    def __post_init__(self) -> None:
        _validate_hex_fields(self)


@dataclass(frozen=True, slots=True)
class PlotColors:
    """Colors for scientific plots (Phase 6), kept alongside the rest of
    the palette so a plot never has to invent its own colors either."""
    axis: str
    gridline: str
    series: tuple[str, ...]   # ordered palette for multiple data series
    error_band: str            # confidence-interval / uncertainty fill
    reference_line: str        # e.g. a theoretical curve overlaid on data

    def __post_init__(self) -> None:
        _validate_hex_fields(self)
        for color in self.series:
            if not _HEX_RE.match(color):
                raise ValueError(f"PlotColors.series contains invalid color {color!r}")


@dataclass(frozen=True, slots=True)
class ColorPalette:
    """The complete set of semantic color roles for a theme."""
    surface: SurfaceColors
    foreground: ForegroundColors
    accent: AccentColors
    syntax: SyntaxColors
    math: MathColors
    status: StatusColors
    ui: UIColors
    plot: PlotColors
