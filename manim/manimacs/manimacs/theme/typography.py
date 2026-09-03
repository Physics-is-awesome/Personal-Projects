"""Typography tokens: font stacks, semantic font sizes, and font weights.

Font *stacks* are ordered tuples of family names, most-preferred first —
similar in spirit to a CSS font-family fallback list. Manim/Pango doesn't
resolve fallback stacks natively, so ``manim_adapters.resolve_font`` walks
a stack and returns the first family that's actually installed, falling
back to the first entry (letting Pango substitute) if none are found.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

FontStack = tuple[str, ...]


@dataclass(frozen=True, slots=True)
class FontSizes:
    """Semantic font-size scale, in Manim's ``font_size`` units.

    Named by role rather than t-shirt size (sm/md/lg) because the same
    numeric size can mean different things in different contexts — an
    "equation" and a "heading" might coincidentally share a size in one
    theme and diverge in another.
    """
    modeline: float           # tiny UI chrome text
    code_small: float          # line numbers, inline annotations
    caption: float
    label: float                # small UI labels
    code: float                  # buffer / code text
    body: float                   # general body text
    equation: float
    equation_emphasis: float      # a zoomed-in / emphasized equation
    subtitle: float
    heading: float                 # in-scene section headers
    title: float                    # big scene title


@dataclass(frozen=True, slots=True)
class FontWeights:
    """Semantic weight names, mapped to Pango weight strings Manim's
    ``Text(weight=...)`` accepts (e.g. "NORMAL", "BOLD")."""
    light: str
    regular: str
    medium: str
    bold: str


@dataclass(frozen=True, slots=True)
class Typography:
    code_font: FontStack
    ui_font: FontStack
    math_font_package: Optional[str]  # LaTeX math font package for Phase 4's
                                       # TexTemplate; None = Manim/LaTeX default
    sizes: FontSizes
    weights: FontWeights
