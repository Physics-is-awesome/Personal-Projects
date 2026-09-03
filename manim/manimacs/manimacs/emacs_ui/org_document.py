"""``OrgDocument`` — a simplified org-mode document: headings with
TODO/DONE state and tags, body prose, embedded code blocks, and a
per-heading ``folded`` flag.

Folding here is *state*, not animation — toggling ``folded`` and calling
``rebuild()`` instantly shows/hides a subtree. The animated fold/unfold
*transition* (per the brief, "code folding" is explicitly part of Phase
3's animation system) is Phase 3's job to build on top of this.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from xml.sax.saxutils import escape

from manim import DOWN, LEFT, RIGHT, MarkupText, Text, VGroup

from ..theme import Theme, resolve_font
from .syntax import highlight_code_lines

_LEVEL_COLOR_TIERS = 2  # levels beyond this use plain foreground


@dataclass
class OrgHeading:
    text: str
    level: int = 1
    todo: str | None = None
    tags: tuple[str, ...] = ()
    body: str = ""
    code: str | None = None
    code_language: str = "python"
    children: tuple["OrgHeading", ...] = field(default_factory=tuple)
    folded: bool = False


class OrgDocument(VGroup):
    def __init__(self, theme: Theme, headings: list[OrgHeading], *, width: float | None = None, **kwargs):
        super().__init__(**kwargs)
        self.theme = theme
        self.headings = headings
        self._width = width
        self._ui_font = resolve_font(theme.typography.ui_font)
        self._code_font = resolve_font(theme.typography.code_font)
        self.rebuild()

    def rebuild(self) -> "OrgDocument":
        """Re-render from current heading/fold state."""
        theme = self.theme
        self.remove(*self.submobjects)
        blocks: list[tuple[object, int]] = []  # (mobject, indent_level)
        for heading in self.headings:
            self._render_heading(heading, blocks)
        if not blocks:
            placeholder = Text("empty buffer", font=self._ui_font, font_size=theme.typography.sizes.body, fill_opacity=0)
            blocks = [(placeholder, 0)]
        mobjects = [m for m, _ in blocks]
        group = VGroup(*mobjects).arrange(DOWN, buff=theme.spacing.xs, aligned_edge=LEFT)
        # Apply indentation *after* arranging: arrange(aligned_edge=LEFT)
        # aligns every block to one common edge, so an indent shift has to
        # come afterward or arrange() would just cancel it back out.
        for mob, indent in blocks:
            if indent:
                mob.shift(RIGHT * indent * theme.spacing.md)
        self.add(group)
        self.content = group
        return self

    def _heading_color(self, level: int) -> str:
        theme = self.theme
        if level == 1:
            return theme.colors.accent.primary
        if level == 2:
            return theme.colors.accent.secondary
        return theme.colors.foreground.primary

    def _render_heading(self, heading: OrgHeading, blocks: list[tuple[object, int]], indent: int = 0) -> None:
        theme = self.theme
        stars = "*" * heading.level
        level_color = self._heading_color(heading.level)
        markup = f'<span foreground="{level_color}">{escape(stars)}</span> '
        if heading.todo:
            todo_color = theme.colors.status.success if heading.todo == "DONE" else theme.colors.status.warning
            markup += f'<span foreground="{todo_color}">{escape(heading.todo)}</span> '
        markup += f'<span foreground="{theme.colors.foreground.primary}">{escape(heading.text)}</span>'
        has_hidden_content = bool(heading.body or heading.code or heading.children)
        if heading.folded and has_hidden_content:
            markup += f'<span foreground="{theme.colors.foreground.muted}"> \u2026</span>'
        if heading.tags:
            tag_str = ":" + ":".join(heading.tags) + ":"
            markup += f'  <span foreground="{theme.colors.foreground.muted}">{escape(tag_str)}</span>'
        heading_mob = MarkupText(markup, font=self._ui_font, font_size=theme.typography.sizes.body)
        # Headings stay flush-left, org-mode style — only body/code indent.
        blocks.append((heading_mob, 0))

        if heading.folded:
            return

        if heading.body:
            for line in heading.body.splitlines() or [heading.body]:
                body_mob = Text(line, font=self._ui_font, font_size=theme.typography.sizes.label,
                                 color=theme.colors.foreground.secondary)
                blocks.append((body_mob, indent + 1))

        if heading.code:
            for markup_line in highlight_code_lines(heading.code, heading.code_language, theme):
                code_mob = MarkupText(markup_line or " ", font=self._code_font,
                                       font_size=theme.typography.sizes.code_small)
                blocks.append((code_mob, indent + 1))

        for child in heading.children:
            self._render_heading(child, blocks, indent=indent + 1)
