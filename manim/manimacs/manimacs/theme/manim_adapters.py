"""The one module in the theme package allowed to import Manim.

Everything else in ``manimacs.theme`` is pure data so it can be loaded,
tested, and reasoned about without Manim installed. These three functions
are the bridge components actually use at draw time to turn theme tokens
into real Manim objects/callables.
"""
from __future__ import annotations

from typing import Callable, Sequence


def to_manim_color(value: str):
    """Convert a theme hex string into a ``manim.ManimColor``."""
    from manim import ManimColor

    return ManimColor(value)


def resolve_rate_func(name: str) -> Callable[[float], float]:
    """Look up a rate-function name (e.g. ``"smooth"``) in
    ``manim.utils.rate_functions``, raising a clear error on a typo
    instead of a bare ``AttributeError`` deep inside an animation call."""
    from manim.utils import rate_functions

    func = getattr(rate_functions, name, None)
    if func is None or not callable(func):
        available = sorted(
            n for n in dir(rate_functions) if not n.startswith("_") and callable(getattr(rate_functions, n))
        )
        raise ValueError(
            f"{name!r} is not a rate function in manim.utils.rate_functions. "
            f"Available: {', '.join(available)}"
        )
    return func


def resolve_font(stack: Sequence[str]) -> str:
    """Return the first font family in *stack* that's actually installed,
    falling back to ``stack[0]`` (letting Pango substitute) if font
    discovery fails or nothing in the stack is found."""
    if not stack:
        raise ValueError("resolve_font() requires a non-empty font stack")
    try:
        import manimpango

        installed = set(manimpango.list_fonts())
    except Exception:
        return stack[0]
    for family in stack:
        if family in installed:
            return family
    return stack[0]
