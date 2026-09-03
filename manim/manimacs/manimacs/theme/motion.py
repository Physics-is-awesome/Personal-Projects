"""Animation-timing tokens: typing speed, pauses, transitions, easing.

Rate functions are stored as **strings** (e.g. ``"smooth"``), not as the
actual Manim callables, so that this module — like the rest of the theme
package — never has to import Manim. ``manim_adapters.resolve_rate_func``
looks the name up in ``manim.utils.rate_functions`` on demand and raises a
clear error listing valid options if a theme author typos a name.
"""
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class PauseDurations:
    """Seconds to hold, from a tiny narration "beat" to a long dwell."""
    beat: float
    short: float
    medium: float
    long: float


@dataclass(frozen=True, slots=True)
class TransitionDurations:
    """Seconds for ``self.play(...)`` calls of different weights."""
    fast: float
    base: float
    slow: float
    camera: float   # deliberately slower — see the camera-motion principle


@dataclass(frozen=True, slots=True)
class RateFuncs:
    """Semantic names, resolved to ``manim.utils.rate_functions`` callables
    by ``manim_adapters.resolve_rate_func``. Only names Manim actually
    ships are used as defaults: linear, smooth, there_and_back, rush_into,
    rush_from, wiggle, exponential_decay."""
    default: str
    entrance: str
    exit: str
    emphasis: str          # a flash/attention beat, e.g. on a new term
    camera: str
    warning_shake: str


@dataclass(frozen=True, slots=True)
class Motion:
    typing_chars_per_second: float
    typing_jitter: float    # 0..1 proportion of randomness in per-char timing
    pauses: PauseDurations
    transitions: TransitionDurations
    rate_funcs: RateFuncs
