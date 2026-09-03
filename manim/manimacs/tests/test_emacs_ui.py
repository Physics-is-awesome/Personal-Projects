"""Tests for manimacs.emacs_ui.

These construct real Manim mobjects (unlike the pure-data theme tests),
so they need Manim installed — checking structural properties (sizes,
submobject counts, colors, exceptions) rather than rendering to video.
Rendering itself is covered separately by the Phase 2 example scene.
"""
from __future__ import annotations

import logging

import pytest

logging.getLogger("manim").setLevel(logging.ERROR)  # silence font-fallback noise in test output

from manimacs.emacs_ui import (
    CodeBuffer,
    CompileBuffer,
    EmacsCursor,
    EmacsWindow,
    FileTree,
    FileTreeEntry,
    MiniBuffer,
    ModeLine,
    OrgDocument,
    OrgHeading,
    PopupWindow,
    StatusIndicator,
    highlight_code_lines,
    scratch_buffer,
)
from manimacs.emacs_ui._layout import markup_line_or_blank, reference_row_height, stack_fixed_rows
from manimacs.theme import COSMOLOGY, DOOM, PAPER

ALL_THEMES = [DOOM, PAPER, COSMOLOGY]


# ---------------------------------------------------------------------------
# syntax.py
# ---------------------------------------------------------------------------

def test_highlight_code_lines_matches_source_line_count() -> None:
    code = "a = 1\n\nb = 2\n"
    lines = highlight_code_lines(code, "python", DOOM)
    assert len(lines) == 3


def test_highlight_code_lines_uses_theme_syntax_colors() -> None:
    lines = highlight_code_lines("def f():\n    pass\n", "python", DOOM)
    assert DOOM.colors.syntax.keyword in lines[0]  # "def"


def test_highlight_code_lines_unknown_language_falls_back() -> None:
    lines = highlight_code_lines("some ~~weird~~ content", "not-a-real-language", DOOM)
    assert len(lines) == 1


def test_highlight_code_lines_empty_code() -> None:
    assert highlight_code_lines("", "python", DOOM) == [""]


# ---------------------------------------------------------------------------
# _layout.py
# ---------------------------------------------------------------------------

def test_reference_row_height_positive() -> None:
    assert reference_row_height(DOOM, "DejaVu Sans Mono", 28) > 0


def test_markup_line_or_blank_blank_has_same_height_as_reference() -> None:
    ref = reference_row_height(DOOM, "DejaVu Sans Mono", 28)
    blank = markup_line_or_blank("", font="DejaVu Sans Mono", font_size=28, fallback_color="#ffffff")
    assert abs(blank.height - ref) < 1e-9


def test_stack_fixed_rows_evenly_spaced() -> None:
    from manim import ORIGIN, LEFT

    blanks = [markup_line_or_blank("", font="DejaVu Sans Mono", font_size=28, fallback_color="#fff") for _ in range(3)]
    group = stack_fixed_rows(blanks, row_pitch=0.5, start=ORIGIN, aligned_edge=LEFT)
    ys = [m.get_center()[1] for m in group]
    assert ys[0] == pytest.approx(0.0)
    assert ys[1] == pytest.approx(-0.5)
    assert ys[2] == pytest.approx(-1.0)


# ---------------------------------------------------------------------------
# EmacsCursor
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("theme", ALL_THEMES, ids=lambda t: t.name)
def test_cursor_constructs_for_every_preset_shape(theme) -> None:
    cursor = EmacsCursor(theme)
    assert cursor.width > 0 and cursor.height > 0


def test_cursor_place_at() -> None:
    from manim import RIGHT

    cursor = EmacsCursor(DOOM)
    cursor.place_at(RIGHT * 2)
    assert cursor.get_left()[0] == pytest.approx(2.0)


# ---------------------------------------------------------------------------
# StatusIndicator
# ---------------------------------------------------------------------------

def test_status_indicator_valid_statuses() -> None:
    si = StatusIndicator(DOOM, "build", status="success")
    assert si.status == "success"
    si.set_status("error")
    assert si.status == "error"


def test_status_indicator_invalid_status_raises() -> None:
    si = StatusIndicator(DOOM, "build")
    with pytest.raises(ValueError):
        si.set_status("not-a-status")


# ---------------------------------------------------------------------------
# ModeLine / MiniBuffer
# ---------------------------------------------------------------------------

def test_modeline_updates() -> None:
    ml = ModeLine(DOOM, 10, buffer_name="a.py")
    ml.set_buffer_name("b.py")
    ml.set_position(5, 2)
    ml.set_active(False)
    assert ml._buffer_name == "b.py"
    assert ml._position == (5, 2)
    assert ml._active is False


def test_minibuffer_prompt_and_clear() -> None:
    mb = MiniBuffer(DOOM, 10, prompt="M-x ", text="save-buffer")
    assert mb._prompt == "M-x "
    mb.clear()
    assert mb._prompt == "" and mb._text == ""


# ---------------------------------------------------------------------------
# PopupWindow
# ---------------------------------------------------------------------------

def test_popup_window_with_and_without_title() -> None:
    p1 = PopupWindow(DOOM, ["a", "b", "c"], title="Menu")
    p2 = PopupWindow(DOOM, ["a", "b", "c"])
    assert p1.height > p2.height  # title adds a row


# ---------------------------------------------------------------------------
# FileTree
# ---------------------------------------------------------------------------

def test_file_tree_select_and_clear() -> None:
    entries = [FileTreeEntry("videos", is_dir=True), FileTreeEntry("a.py", depth=1), FileTreeEntry("b.py", depth=1)]
    ft = FileTree(DOOM, entries, width=3, height=2)
    ft.select(1)
    assert ft.selection_rect.fill_opacity == 1
    ft.select(2)  # switching selection shouldn't error
    ft.clear_selection()
    assert ft.selection_rect.fill_opacity == 0


def test_file_tree_select_out_of_range_raises() -> None:
    ft = FileTree(DOOM, [FileTreeEntry("a.py")], width=3, height=2)
    with pytest.raises(IndexError):
        ft.select(5)


# ---------------------------------------------------------------------------
# CompileBuffer
# ---------------------------------------------------------------------------

def test_compile_buffer_add_and_clip_to_max_visible() -> None:
    cb = CompileBuffer(DOOM, width=6, height=1.5)
    for i in range(10):
        cb.add_line(f"line {i}", "default")
    assert len(cb._rows_group.submobjects) == cb._max_visible
    assert len(cb._lines) == 10  # full history retained even if not all shown


def test_compile_buffer_clear() -> None:
    cb = CompileBuffer(DOOM, width=6, height=1.5)
    cb.add_line("x", "error")
    cb.clear()
    assert cb._lines == []
    assert len(cb._rows_group.submobjects) == 0


# ---------------------------------------------------------------------------
# CodeBuffer
# ---------------------------------------------------------------------------

def test_code_buffer_line_count_and_get_line() -> None:
    buf = CodeBuffer(DOOM, "a = 1\nb = 2\nc = 3\n", language="python")
    assert buf.line_count == 3
    assert buf.get_line(1) is buf.get_line(1)  # stable reference


def test_code_buffer_get_line_out_of_range_raises() -> None:
    buf = CodeBuffer(DOOM, "a = 1\n", language="python")
    with pytest.raises(IndexError):
        buf.get_line(99)
    with pytest.raises(IndexError):
        buf.get_line(0)


def test_code_buffer_highlight_unhighlight() -> None:
    buf = CodeBuffer(DOOM, "a = 1\nb = 2\n", language="python")
    buf.highlight_line(1)
    assert 1 in buf._line_highlights
    buf.highlight_line(1)  # idempotent, no duplicate
    assert len(buf._highlight_layer.submobjects) == 1
    buf.unhighlight_line(1)
    assert 1 not in buf._line_highlights
    assert len(buf._highlight_layer.submobjects) == 0


def test_code_buffer_set_code_rebuilds() -> None:
    buf = CodeBuffer(DOOM, "a = 1\n", language="python")
    buf.highlight_line(1)
    buf.set_code("x = 1\ny = 2\nz = 3\n")
    assert buf.line_count == 3
    assert buf._line_highlights == {}  # highlights don't carry over stale line refs


@pytest.mark.parametrize("theme", ALL_THEMES, ids=lambda t: t.name)
def test_code_buffer_constructs_for_every_preset(theme) -> None:
    buf = CodeBuffer(theme, "def f(x):\n    return x\n", language="python")
    assert buf.line_count == 2


# ---------------------------------------------------------------------------
# scratch_buffer
# ---------------------------------------------------------------------------

def test_scratch_buffer_has_default_comment() -> None:
    buf = scratch_buffer(DOOM, "x = 1\n")
    assert buf.line_count >= 3  # comment lines + blank + content


# ---------------------------------------------------------------------------
# OrgDocument
# ---------------------------------------------------------------------------

def _sample_org():
    return [
        OrgHeading(
            text="Friedmann equation", level=1, todo="TODO", tags=("cosmology",),
            body="Governs cosmic expansion.",
            code="H2 = rho - k",
            children=(OrgHeading(text="Derivation", level=2, body="From Einstein's equations."),),
        )
    ]


def test_org_document_folding_changes_block_count() -> None:
    headings = _sample_org()
    doc = OrgDocument(DOOM, headings)
    unfolded_count = len(doc.content.submobjects)
    headings[0].folded = True
    doc.rebuild()
    folded_count = len(doc.content.submobjects)
    assert folded_count == 1
    assert folded_count < unfolded_count


def test_org_document_body_is_indented_relative_to_heading() -> None:
    doc = OrgDocument(DOOM, _sample_org())
    heading_x = doc.content.submobjects[0].get_left()[0]
    body_x = doc.content.submobjects[1].get_left()[0]
    assert body_x > heading_x


def test_org_document_empty_headings_list_does_not_crash() -> None:
    doc = OrgDocument(DOOM, [])
    assert doc.height > 0


# ---------------------------------------------------------------------------
# EmacsWindow
# ---------------------------------------------------------------------------

def test_emacs_window_open_file_and_set_buffer() -> None:
    win = EmacsWindow(DOOM, width=10, height=6)
    buf = CodeBuffer(DOOM, "x = 1\n", language="python")
    win.open_file("friedmann.org", buf, mode="Org")
    assert win.filename == "friedmann.org"
    assert win.modeline._buffer_name == "friedmann.org"
    assert win.modeline._mode == "Org"
    assert win.buffer is buf


def test_emacs_window_split_horizontal() -> None:
    win = EmacsWindow(DOOM, width=10, height=6)
    pane_a, pane_b = win.split(direction="horizontal")
    assert pane_a.width == pytest.approx(pane_b.width, rel=0.05)
    assert pane_a.title_bar is None  # panes don't get their own title bar


def test_emacs_window_split_vertical() -> None:
    win = EmacsWindow(DOOM, width=10, height=6)
    pane_a, pane_b = win.split(direction="vertical")
    assert pane_a.height == pytest.approx(pane_b.height, rel=0.05)


def test_emacs_window_split_invalid_direction_raises() -> None:
    win = EmacsWindow(DOOM, width=10, height=6)
    with pytest.raises(ValueError):
        win.split(direction="diagonal")


def test_emacs_window_no_title_bar_or_modeline() -> None:
    win = EmacsWindow(DOOM, width=6, height=4, show_title_bar=False, show_modeline=False)
    assert win.title_bar is None
    assert win.modeline is None
