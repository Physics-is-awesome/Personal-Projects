"""``CodeBuffer`` — syntax-highlighted code with optional line numbers and
per-line highlighting. The main content area hosted inside an
``EmacsWindow``, but usable standalone too.

Deliberately has no ``type()``/``scroll_to_line()`` — those are Phase 3's
"typing, cursor, scrolling, and editing animations", built as a layer on
top of what this class exposes (``get_line()``, ``set_code()``). This
class answers "what does the buffer look like right now", not "how did it
get typed out".
"""
from __future__ import annotations

from manim import DOWN, LEFT, RIGHT, UP, Rectangle, Text, VGroup

from ..theme import Theme, resolve_font
from ._layout import markup_line_or_blank, reference_row_height, stack_fixed_rows
from .syntax import highlight_code_lines


class CodeBuffer(VGroup):
    def __init__(
        self,
        theme: Theme,
        code: str = "",
        *,
        language: str = "python",
        width: float | None = None,
        show_line_numbers: bool | None = None,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.theme = theme
        self._code_font = resolve_font(theme.typography.code_font)
        self._language = language
        self._code = code
        self._show_line_numbers = (
            theme.code.show_line_numbers if show_line_numbers is None else show_line_numbers
        )
        self._width = width
        self._row_h = reference_row_height(theme, self._code_font, theme.typography.sizes.code)
        self._row_pitch = self._row_h + theme.code.line_spacing

        self._highlight_layer = VGroup()
        self._gutter_layer = VGroup()
        self._text_layer = VGroup()
        # Highlights behind gutter behind text, in that z-order.
        self.add(self._highlight_layer, self._gutter_layer, self._text_layer)

        self._line_highlights: dict[int, Rectangle] = {}
        self._lines: list[Text] = []
        self.set_code(code, language)

    # -- content -----------------------------------------------------
    def set_code(self, code: str, language: str | None = None) -> "CodeBuffer":
        theme = self.theme
        self._code = code
        if language is not None:
            self._language = language
        markups = highlight_code_lines(code, self._language, theme)

        self._text_layer.remove(*self._text_layer.submobjects)
        self._gutter_layer.remove(*self._gutter_layer.submobjects)
        self._highlight_layer.remove(*self._highlight_layer.submobjects)
        self._line_highlights = {}

        self._lines = [
            markup_line_or_blank(m, font=self._code_font, font_size=theme.typography.sizes.code,
                                  fallback_color=theme.colors.foreground.primary)
            for m in markups
        ]
        gutter_w = theme.code.gutter_width if self._show_line_numbers else 0.0
        origin = UP * 0 + RIGHT * gutter_w
        stack_fixed_rows(self._lines, row_pitch=self._row_pitch, start=origin, aligned_edge=LEFT)
        self._text_layer.add(*self._lines)

        if self._show_line_numbers:
            numbers = []
            for i in range(len(self._lines)):
                n = Text(str(i + 1), font=self._code_font, font_size=theme.typography.sizes.code_small,
                          color=theme.colors.ui.line_number)
                numbers.append(n)
            stack_fixed_rows(numbers, row_pitch=self._row_pitch, start=UP * 0, aligned_edge=LEFT)
            for n in numbers:
                n.align_to(origin + LEFT * theme.spacing.xs, RIGHT)
            self._gutter_layer.add(*numbers)
            self._number_mobjects = numbers
        else:
            self._number_mobjects = []
        return self

    # -- access --------------------------------------------------------
    def get_line(self, n: int) -> Text:
        """1-indexed: get_line(1) is the first line."""
        if not 1 <= n <= len(self._lines):
            raise IndexError(f"CodeBuffer.get_line({n}): buffer has {len(self._lines)} lines")
        return self._lines[n - 1]

    @property
    def line_count(self) -> int:
        return len(self._lines)

    # -- state changes (instant; Phase 3 will add animated variants) --
    def highlight_line(self, n: int) -> "CodeBuffer":
        theme = self.theme
        if n in self._line_highlights:
            return self
        line = self.get_line(n)
        rect_width = (self._width or (line.width + theme.code.gutter_width + 2 * theme.spacing.sm))
        rect = Rectangle(
            width=rect_width, height=self._row_pitch,
            fill_color=theme.colors.surface.line_highlight, fill_opacity=1, stroke_width=0,
        )
        target = line.get_left().copy()
        target[0] -= theme.code.gutter_width if self._show_line_numbers else theme.spacing.xs
        rect.move_to(target, aligned_edge=LEFT)
        self._highlight_layer.add(rect)
        self._line_highlights[n] = rect
        return self

    def unhighlight_line(self, n: int) -> "CodeBuffer":
        rect = self._line_highlights.pop(n, None)
        if rect is not None:
            self._highlight_layer.remove(rect)
        return self

    def clear_highlights(self) -> "CodeBuffer":
        for n in list(self._line_highlights):
            self.unhighlight_line(n)
        return self
