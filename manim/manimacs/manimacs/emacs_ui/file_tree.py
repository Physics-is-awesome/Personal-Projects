"""``FileTree`` — a sidebar directory/file listing (treemacs/neotree-style).

Kept intentionally simple: a flat list of ``FileTreeEntry`` (name,
is_dir, depth) rather than a real recursive filesystem model — this is an
animation prop, not a file manager.
"""
from __future__ import annotations

from dataclasses import dataclass
from xml.sax.saxutils import escape

from manim import DOWN, LEFT, RIGHT, UP, MarkupText, Rectangle, VGroup

from ..theme import Theme, resolve_font
from ._layout import reference_row_height, stack_fixed_rows


@dataclass(frozen=True)
class FileTreeEntry:
    name: str
    is_dir: bool = False
    depth: int = 0


class FileTree(VGroup):
    def __init__(self, theme: Theme, entries: list[FileTreeEntry], *, width: float, height: float, **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self.entries = entries
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._width = width
        self._height = height
        self._selected_index: int | None = None

        self.panel = Rectangle(width=width, height=height,
                                fill_color=theme.colors.ui.file_tree_bg, fill_opacity=1,
                                stroke_color=theme.colors.ui.border_subtle, stroke_width=theme.borders.thin)
        self.add(self.panel)

        self._row_h = reference_row_height(theme, self._ui_font, theme.typography.sizes.label)
        self.selection_rect = Rectangle(
            width=width, height=self._row_h + theme.spacing.xxs,
            fill_color=theme.colors.ui.file_tree_selected_bg, fill_opacity=1, stroke_width=0,
        )
        self.selection_rect.set_opacity(0)
        self.add(self.selection_rect)

        self.rows = self._build_rows()
        top_left = self.panel.get_corner(UP + LEFT) + DOWN * theme.spacing.xs
        stack_fixed_rows(self.rows, row_pitch=self._row_h + theme.spacing.xxs, start=top_left, aligned_edge=LEFT)
        for row in self.rows:
            self.add(row)

    def _build_rows(self) -> list[VGroup]:
        theme = self.theme
        rows = []
        self._base_colors: list[str] = []
        for entry in self.entries:
            icon_color = theme.colors.ui.file_tree_icon_dir if entry.is_dir else theme.colors.ui.file_tree_icon_file
            marker = "\u25b8 " if entry.is_dir else "  "  # small triangle for directories
            base_color = theme.colors.foreground.primary if entry.is_dir else theme.colors.ui.file_tree_fg
            indent = "  " * entry.depth
            markup = (
                escape(indent)
                + f'<span foreground="{icon_color}">{escape(marker)}</span>'
                + f'<span foreground="{base_color}">{escape(entry.name)}</span>'
            )
            label = MarkupText(markup, font=self._ui_font, font_size=theme.typography.sizes.label)
            self._base_colors.append(base_color)
            rows.append(VGroup(label))
        return rows

    def select(self, index: int) -> "FileTree":
        if not 0 <= index < len(self.rows):
            raise IndexError(f"FileTree.select({index}): only {len(self.rows)} entries")
        if self._selected_index is not None:
            self.rows[self._selected_index][0].set_color(self._base_colors[self._selected_index])
        self._selected_index = index
        target = self.panel.get_left().copy()
        target[1] = self.rows[index].get_center()[1]
        self.selection_rect.move_to(target, aligned_edge=LEFT)
        self.selection_rect.set_opacity(1)
        self.rows[index][0].set_color(self.theme.colors.ui.file_tree_selected_fg)
        return self

    def clear_selection(self) -> "FileTree":
        if self._selected_index is not None:
            self.rows[self._selected_index][0].set_color(self._base_colors[self._selected_index])
        self._selected_index = None
        self.selection_rect.set_opacity(0)
        return self
