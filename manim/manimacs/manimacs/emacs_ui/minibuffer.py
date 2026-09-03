"""``MiniBuffer`` — Emacs's prompt/command line (M-x, search prompts,
"Find file:", y-or-n-p questions, ...)."""
from __future__ import annotations

from manim import LEFT, RIGHT, Rectangle, Text, VGroup

from ..theme import Theme, resolve_font


class MiniBuffer(VGroup):
    def __init__(self, theme: Theme, width: float, *, prompt: str = "", text: str = "", **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self._code_font = resolve_font(theme.typography.code_font)
        self._width = width
        self._prompt = prompt
        self._text = text

        self.bar = Rectangle(width=width, height=theme.window.modeline_height,
                              fill_color=theme.colors.surface.sunken, fill_opacity=1, stroke_width=0)
        self.content = Text("", font=self._code_font, font_size=theme.typography.sizes.modeline)
        self.add(self.bar, self.content)
        self._refresh()

    def _refresh(self) -> None:
        theme = self.theme
        full = f"{self._prompt}{self._text}" or " "
        t2c = {self._prompt: theme.colors.foreground.muted} if self._prompt else {}
        new_content = Text(
            full, font=self._code_font,
            font_size=theme.typography.sizes.modeline,
            color=theme.colors.foreground.primary, t2c=t2c,
        )
        new_content.move_to(self.bar, aligned_edge=LEFT).shift(RIGHT * theme.spacing.xs)
        self.remove(self.content)
        self.content = new_content
        self.add(self.content)

    def set_prompt(self, prompt: str) -> "MiniBuffer":
        self._prompt = prompt
        self._refresh()
        return self

    def set_text(self, text: str) -> "MiniBuffer":
        self._text = text
        self._refresh()
        return self

    def clear(self) -> "MiniBuffer":
        self._prompt = ""
        self._text = ""
        self._refresh()
        return self
