"""Theme registry (lookup by name) and current/active-theme tracking.

Two independent conveniences live here:

1. A name -> Theme registry, so a video's render script can pick a theme
   by string (handy for a ``--theme=doom`` CLI flag) without importing
   every preset by hand.
2. A single "current theme" global with a context-manager override, for
   scenes that would rather not thread ``theme=...`` through every single
   component call. Passing ``theme=`` explicitly is still the recommended
   default — this is an escape hatch, not the primary API.
"""
from __future__ import annotations

import contextlib
from typing import Iterator

from .presets import DOOM
from .theme import Theme

_registry: dict[str, Theme] = {}
_current: Theme = DOOM


def register_theme(theme: Theme, *, overwrite: bool = False) -> None:
    if not overwrite and theme.name in _registry:
        raise ValueError(
            f"A theme named {theme.name!r} is already registered. "
            "Pass overwrite=True to replace it."
        )
    _registry[theme.name] = theme


def get_theme(name: str) -> Theme:
    try:
        return _registry[name]
    except KeyError as exc:
        available = ", ".join(sorted(_registry)) or "(none registered)"
        raise KeyError(f"No theme named {name!r} is registered. Available: {available}") from exc


def list_themes() -> list[str]:
    return sorted(_registry)


def get_current_theme() -> Theme:
    return _current


def set_current_theme(theme: Theme) -> None:
    global _current
    _current = theme


@contextlib.contextmanager
def use_theme(theme: Theme) -> Iterator[Theme]:
    """Temporarily override the current theme within a ``with`` block."""
    global _current
    previous = _current
    _current = theme
    try:
        yield theme
    finally:
        _current = previous
