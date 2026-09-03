"""Pure hex-string color math. No Manim dependency — see ``colors.py`` for
why that separation matters.

Manim keeps color and opacity as separate concerns (``fill_color`` vs.
``fill_opacity``), so these helpers do too: nothing here bakes an alpha
channel into a hex string.
"""
from __future__ import annotations

import re

_HEX_RE = re.compile(r"^#[0-9a-fA-F]{6}$")


def validate_hex(value: str) -> None:
    if not _HEX_RE.match(value):
        raise ValueError(f"{value!r} is not a valid '#rrggbb' hex color.")


def hex_to_rgb(value: str) -> tuple[int, int, int]:
    validate_hex(value)
    return (int(value[1:3], 16), int(value[3:5], 16), int(value[5:7], 16))


def rgb_to_hex(r: int, g: int, b: int) -> str:
    for name, component in (("r", r), ("g", g), ("b", b)):
        if not 0 <= component <= 255:
            raise ValueError(f"rgb_to_hex(): {name}={component} is out of range 0..255")
    return f"#{r:02x}{g:02x}{b:02x}"


def blend(hex_a: str, hex_b: str, t: float) -> str:
    """Linearly interpolate between two colors. ``t=0`` -> hex_a, ``t=1`` -> hex_b."""
    if not 0.0 <= t <= 1.0:
        raise ValueError(f"blend(): t={t} must be in [0, 1]")
    ar, ag, ab = hex_to_rgb(hex_a)
    br, bg, bb = hex_to_rgb(hex_b)
    return rgb_to_hex(
        round(ar + (br - ar) * t),
        round(ag + (bg - ag) * t),
        round(ab + (bb - ab) * t),
    )


def lighten(value: str, amount: float) -> str:
    """Blend *value* toward white by *amount* (0..1)."""
    return blend(value, "#ffffff", amount)


def darken(value: str, amount: float) -> str:
    """Blend *value* toward black by *amount* (0..1)."""
    return blend(value, "#000000", amount)


def relative_luminance(value: str) -> float:
    """Approximate sRGB relative luminance (0=black, 1=white); handy for
    deciding e.g. whether foreground.inverse should be dark or light."""
    r, g, b = (c / 255.0 for c in hex_to_rgb(value))
    return 0.2126 * r + 0.7152 * g + 0.0722 * b
