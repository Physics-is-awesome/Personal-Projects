"""``StatusIndicator`` — a small colored dot + label showing a status role
(success/warning/error/info) from ``theme.colors.status``. Used for things
like build status, LSP/git status, or any other small at-a-glance signal.
"""
from __future__ import annotations

from manim import RIGHT, Dot, Text, VGroup

from ..theme import Theme, resolve_font

_VALID = ("success", "warning", "error", "info")


class StatusIndicator(VGroup):
    def __init__(self, theme: Theme, label: str, *, status: str = "info", **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._label_text = label
        self.dot = Dot(radius=theme.spacing.xxs * 1.4, fill_opacity=1, stroke_width=0)
        self.label = Text(label, font=self._ui_font,
                           font_size=theme.typography.sizes.label,
                           color=theme.colors.foreground.secondary)
        self.label.next_to(self.dot, RIGHT, buff=theme.spacing.xs)
        self.add(self.dot, self.label)
        self.set_status(status)

    def set_status(self, status: str) -> "StatusIndicator":
        if status not in _VALID:
            raise ValueError(f"StatusIndicator status must be one of {_VALID}, got {status!r}")
        self.status = status
        self.dot.set_fill(getattr(self.theme.colors.status, status))
        return self

    def set_label(self, text: str) -> "StatusIndicator":
        new_label = Text(text, font=self._ui_font,
                          font_size=self.theme.typography.sizes.label,
                          color=self.theme.colors.foreground.secondary)
        new_label.next_to(self.dot, RIGHT, buff=self.theme.spacing.xs)
        self.remove(self.label)
        self.label = new_label
        self.add(self.label)
        self._label_text = text
        return self
