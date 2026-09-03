"""``ModeLine`` — the status bar Emacs shows at the bottom of each window,
reporting buffer name, major mode, and cursor position."""
from __future__ import annotations

from manim import LEFT, RIGHT, Rectangle, Text, VGroup

from ..theme import Theme, resolve_font


class ModeLine(VGroup):
    def __init__(
        self,
        theme: Theme,
        width: float,
        *,
        buffer_name: str = "*scratch*",
        mode: str = "Fundamental",
        position: tuple[int, int] = (1, 1),
        active: bool = True,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.theme = theme
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._width = width
        self._buffer_name = buffer_name
        self._mode = mode
        self._position = position
        self._active = active

        self.bar = Rectangle(width=width, height=theme.window.modeline_height,
                              fill_opacity=1, stroke_width=0)
        self.text = Text("", font=self._ui_font,
                          font_size=theme.typography.sizes.modeline)
        self.add(self.bar, self.text)
        self._refresh()

    def _refresh(self) -> None:
        theme = self.theme
        bg = theme.colors.ui.modeline_active_bg if self._active else theme.colors.ui.modeline_inactive_bg
        fg = theme.colors.ui.modeline_active_fg if self._active else theme.colors.ui.modeline_inactive_fg
        self.bar.set_fill(bg)
        line, col = self._position
        content = f"  {self._buffer_name}   {line},{col}   ({self._mode})"
        new_text = Text(content, font=self._ui_font,
                         font_size=theme.typography.sizes.modeline, color=fg)
        new_text.move_to(self.bar, aligned_edge=LEFT).shift(RIGHT * self.theme.spacing.xs)
        self.remove(self.text)
        self.text = new_text
        self.add(self.text)

    def set_buffer_name(self, name: str) -> "ModeLine":
        self._buffer_name = name
        self._refresh()
        return self

    def set_mode(self, mode: str) -> "ModeLine":
        self._mode = mode
        self._refresh()
        return self

    def set_position(self, line: int, col: int) -> "ModeLine":
        self._position = (line, col)
        self._refresh()
        return self

    def set_active(self, active: bool) -> "ModeLine":
        self._active = active
        self._refresh()
        return self
