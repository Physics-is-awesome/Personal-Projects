"""``EmacsWindow`` — the top-level frame: title bar + content area (hosts
a buffer: ``CodeBuffer``/``OrgDocument``/scratch/``CompileBuffer``) +
modeline.

``open_file``/``type``/``scroll``/``fold_heading`` appear together in the
brief's illustrative example, but only the structural half belongs here —
``open_file`` (attach a buffer, instant) is Phase 2; the typing/scrolling
*process* is Phase 3, built as a layer that manipulates the buffers this
class hosts (via ``get_line()`` etc.), not something this class does
itself.

``split()`` supports one level of side-by-side or stacked panes — real
Emacs allows arbitrary recursive splits, but one level covers the common
case for a video without a general recursive window-manager.
"""
from __future__ import annotations

from manim import DOWN, LEFT, RIGHT, UP, Dot, Line, Rectangle, Text, VGroup

from ..theme import Theme, resolve_font
from ._layout import themed_panel
from .modeline import ModeLine


class EmacsWindow(VGroup):
    def __init__(
        self,
        theme: Theme,
        *,
        width: float | None = None,
        height: float | None = None,
        buffer=None,
        filename: str = "*scratch*",
        mode: str = "Fundamental",
        show_title_bar: bool = True,
        show_modeline: bool = True,
        show_border: bool = True,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.theme = theme
        self._code_font = resolve_font(theme.typography.code_font)
        self.width_setting = width or theme.window.default_width
        self.height_setting = height or theme.window.default_height
        self._show_title_bar = show_title_bar
        self._show_modeline = show_modeline
        self.filename = filename

        w, h = self.width_setting, self.height_setting
        if show_border:
            self.body = themed_panel(theme, w, h, fill=theme.colors.surface.primary,
                                      corner_radius=theme.window.corner_radius,
                                      border_width=theme.borders.default)
        else:
            self.body = Rectangle(width=w, height=h, fill_color=theme.colors.surface.primary,
                                   fill_opacity=1, stroke_width=0)
        self.add(self.body)

        top = self.body.get_top()
        self.title_bar = None
        if show_title_bar:
            self.title_bar = Rectangle(width=w, height=theme.window.title_bar_height,
                                        fill_color=theme.colors.surface.sunken, fill_opacity=1, stroke_width=0)
            self.title_bar.move_to(top, aligned_edge=UP)
            self.title_text = Text(filename, font=self._code_font, font_size=theme.typography.sizes.code_small,
                                    color=theme.colors.foreground.muted)
            self.title_text.move_to(self.title_bar)
            self.add(self.title_bar, self.title_text)

        self.modeline = None
        if show_modeline:
            self.modeline = ModeLine(theme, w, buffer_name=filename, mode=mode)
            self.modeline.move_to(self.body.get_bottom(), aligned_edge=DOWN)
            self.add(self.modeline)

        self.content_area = VGroup()
        # An empty VGroup silently anchors its bounding box at the origin
        # (0,0,0) in Manim — a real, reproducible bug otherwise, since a
        # freshly-split pane with no buffer set yet would report a
        # distorted height that includes coordinate (0,0,0) even though
        # nothing is drawn there. A zero-size invisible anchor at the
        # body's own center keeps the empty-state bounding box correct;
        # it's cleared out the moment a real buffer is attached.
        self.content_area.add(Dot(radius=0, fill_opacity=0).move_to(self.body.get_center()))
        self.add(self.content_area)
        self._content_top = (self.title_bar.get_bottom() if self.title_bar is not None else top)
        self._content_bottom = (self.modeline.get_top() if self.modeline is not None else self.body.get_bottom())
        self.buffer = None
        if buffer is not None:
            self.set_buffer(buffer)

    # -- content -------------------------------------------------------
    def set_buffer(self, buffer) -> "EmacsWindow":
        self.content_area.remove(*self.content_area.submobjects)
        pad = self.theme.panel.padding
        target = self._content_top.copy()
        target[1] -= pad
        target[0] = self.body.get_left()[0] + pad
        buffer.move_to(target, aligned_edge=UP + LEFT)
        self.content_area.add(buffer)
        self.buffer = buffer
        return self

    def open_file(self, filename: str, buffer, *, mode: str | None = None) -> "EmacsWindow":
        self.filename = filename
        self.set_buffer(buffer)
        if self.title_bar is not None:
            theme = self.theme
            new_title = Text(filename, font=self._code_font, font_size=theme.typography.sizes.code_small,
                              color=theme.colors.foreground.muted)
            new_title.move_to(self.title_bar)
            self.remove(self.title_text)
            self.title_text = new_title
            self.add(self.title_text)
        if self.modeline is not None:
            self.modeline.set_buffer_name(filename)
            if mode is not None:
                self.modeline.set_mode(mode)
        return self

    # -- splitting -------------------------------------------------------
    def split(self, direction: str = "horizontal", ratio: float = 0.5) -> tuple["EmacsWindow", "EmacsWindow"]:
        """Split the content area into two panes (each with its own
        modeline, sharing this window's outer frame/title bar).

        ``direction="horizontal"``: panes side by side (like Emacs's
        ``split-window-right`` / ``C-x 3``).
        ``direction="vertical"``: panes stacked (like ``split-window-below``
        / ``C-x 2``).
        """
        if direction not in ("horizontal", "vertical"):
            raise ValueError('EmacsWindow.split(direction=...) must be "horizontal" or "vertical"')
        theme = self.theme
        self.content_area.remove(*self.content_area.submobjects)
        self.buffer = None

        # The outer window's own modeline no longer corresponds to any
        # single buffer once panes take over that role, so it's hidden
        # (not removed — .unsplit() could restore it) rather than left
        # showing stale/empty buffer info. Panes then get the full body
        # height rather than leaving a gap where it used to be.
        top = self._content_top
        bottom = self.body.get_bottom()
        if self.modeline is not None:
            self.modeline.set_opacity(0)
        left_x = self.body.get_left()[0]
        right_x = self.body.get_right()[0]
        content_h = top[1] - bottom[1]
        content_w = right_x - left_x

        if direction == "horizontal":
            split_x = left_x + content_w * ratio
            pane_a = EmacsWindow(theme, width=split_x - left_x, height=content_h,
                                  show_title_bar=False, show_border=False)
            pane_b = EmacsWindow(theme, width=right_x - split_x, height=content_h,
                                  show_title_bar=False, show_border=False)
            pane_a.move_to((left_x + (split_x - left_x) / 2, (top[1] + bottom[1]) / 2, 0))
            pane_b.move_to((split_x + (right_x - split_x) / 2, (top[1] + bottom[1]) / 2, 0))
            divider = Line(UP * content_h / 2, DOWN * content_h / 2, color=theme.colors.ui.border_default,
                            stroke_width=theme.borders.thin)
            divider.move_to((split_x, (top[1] + bottom[1]) / 2, 0))
        else:
            split_y = top[1] - content_h * ratio
            pane_a = EmacsWindow(theme, width=content_w, height=top[1] - split_y,
                                  show_title_bar=False, show_border=False)
            pane_b = EmacsWindow(theme, width=content_w, height=split_y - bottom[1],
                                  show_title_bar=False, show_border=False)
            pane_a.move_to(((left_x + right_x) / 2, (top[1] + split_y) / 2, 0))
            pane_b.move_to(((left_x + right_x) / 2, (split_y + bottom[1]) / 2, 0))
            divider = Line(LEFT * content_w / 2, RIGHT * content_w / 2, color=theme.colors.ui.border_default,
                            stroke_width=theme.borders.thin)
            divider.move_to(((left_x + right_x) / 2, split_y, 0))

        self.content_area.add(pane_a, pane_b, divider)
        self.panes = (pane_a, pane_b)
        return pane_a, pane_b
